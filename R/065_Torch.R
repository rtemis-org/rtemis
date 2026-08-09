# 065_Torch.R
# ::rtemis::
# 2026- EDG rtemis.org

# Torch plumbing shared by the torch-backed algorithms. Nothing here knows which
# module it is training: the algorithm builds the module and hands it in, and the
# loop requires only that calling it on a batch's input tensors returns something
# the loss accepts. `train_MLP.R` is the first consumer.
#
# Numbered rather than named `utils_torch.R` because the vocabulary constants
# below are read at class-definition time by `070_Hyperparameters.R`, which is
# top-level code evaluated in collation order.
#
# `torch` is a Suggests-gated backend reached by `::`, so every torch object is
# built inside a function. A module generator or a device at the top level of
# this file would make the package unloadable without the backend installed.
#
# A method on a torch object is reached with `[[` and then called --
# `module[["to"]](device = dev)`, not `module$to(device = dev)`. A torch object
# is an environment, and `$` on one reads to static analysis as a call to a
# free function of the method's name, which `object_usage_linter` reports as an
# unbound global. Keep the convention: a `$` call here fails `just lint`.

# %% TORCH_DEVICES ----
# Compute devices, in the order `resolve_torch_device()` prefers them when no
# device is named. `mps` is deliberately absent: a caller may ask for it, but
# nothing here picks it, because it does not honor `torch_manual_seed()` -- two
# runs of one fit under one seed differ -- and silently losing reproducibility
# is worse than losing the acceleration.
TORCH_DEVICES <- c("cpu", "cuda", "mps")
TORCH_DEVICE_PREFERENCE <- c("cuda", "cpu")

# %% TORCH_ACTIVATIONS ----
TORCH_ACTIVATIONS <- c(
  "relu",
  "gelu",
  "silu",
  "elu",
  "selu",
  "leaky_relu",
  "tanh"
)

# %% TORCH_NORMS ----
TORCH_NORMS <- c("batch_norm", "layer_norm")

# %% TORCH_OPTIMIZERS ----
TORCH_OPTIMIZERS <- c("adamw", "adam", "sgd", "rmsprop")

# %% TORCH_SCHEDULERS ----
TORCH_SCHEDULERS <- c(
  "step",
  "cosine_annealing",
  "one_cycle",
  "reduce_on_plateau"
)

# %% TORCH_LOSSES ----
# Regression losses first, then the classification one. `torch_loss_module()`
# maps each to the `nn_*_loss` module of the same name.
TORCH_REGRESSION_LOSSES <- c("mse", "l1", "smooth_l1")
TORCH_CLASSIFICATION_LOSSES <- "cross_entropy"
TORCH_LOSSES <- c(TORCH_REGRESSION_LOSSES, TORCH_CLASSIFICATION_LOSSES)


# %% resolve_torch_device ----
#' Resolve the compute device to train on
#'
#' @param device Character or NULL: Device to use. NULL picks the first
#' available of `TORCH_DEVICE_PREFERENCE`.
#' @param seed Integer or NULL: The seed the caller asked for, used only to warn
#' that `mps` will not honor it.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Character: The resolved device name.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_torch_device <- function(device = NULL, seed = NULL, verbosity = 1L) {
  available <- function(name) {
    switch(
      name,
      cpu = TRUE,
      cuda = torch::cuda_is_available(),
      mps = torch::backends_mps_is_available()
    )
  }
  if (is.null(device)) {
    device <- TORCH_DEVICE_PREFERENCE[
      vapply(TORCH_DEVICE_PREFERENCE, available, logical(1L))
    ][[1L]]
    msg0("Using ", device, " device...", verbosity = verbosity - 1L)
    return(device)
  }
  if (!available(device)) {
    rtemis.core::abort(
      "Device '",
      device,
      "' is not available on this system.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (identical(device, "mps") && !is.null(seed)) {
    warn(
      "The mps device does not honor `seed`: this fit is not reproducible. Set device = \"cpu\" to reproduce it."
    )
  }
  device
} # /rtemis::resolve_torch_device


# %% torch_activation_module ----
#' Build an activation module by name
#'
#' @param name Character: One of `TORCH_ACTIVATIONS`.
#'
#' @return `nn_module` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_activation_module <- function(name) {
  switch(
    name,
    relu = torch::nn_relu(),
    gelu = torch::nn_gelu(),
    silu = torch::nn_silu(),
    elu = torch::nn_elu(),
    selu = torch::nn_selu(),
    leaky_relu = torch::nn_leaky_relu(),
    tanh = torch::nn_tanh()
  )
} # /rtemis::torch_activation_module


# %% torch_norm_module ----
#' Build a normalization module by name
#'
#' @param name Character or NULL: One of `TORCH_NORMS`, or NULL for none.
#' @param num_features Integer: Width of the layer being normalized.
#'
#' @return `nn_module` object; identity when `name` is NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_norm_module <- function(name, num_features) {
  if (is.null(name)) {
    return(torch::nn_identity())
  }
  switch(
    name,
    batch_norm = torch::nn_batch_norm1d(num_features),
    layer_norm = torch::nn_layer_norm(num_features)
  )
} # /rtemis::torch_norm_module


# %% torch_loss_module ----
#' Build a loss module by name
#'
#' Always `reduction = "none"`: case weights are applied to the per-case losses
#' and reduced by `torch_weighted_loss()`, so the loop has one reduction path
#' whether or not weights were supplied.
#'
#' @param name Character: One of `TORCH_LOSSES`.
#'
#' @return `nn_module` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_loss_module <- function(name) {
  switch(
    name,
    mse = torch::nn_mse_loss(reduction = "none"),
    l1 = torch::nn_l1_loss(reduction = "none"),
    smooth_l1 = torch::nn_smooth_l1_loss(reduction = "none"),
    cross_entropy = torch::nn_cross_entropy_loss(reduction = "none")
  )
} # /rtemis::torch_loss_module


# %% torch_weighted_loss ----
#' Reduce a per-case loss tensor to one weighted mean
#'
#' Regression losses come back with the target's shape (cases x outputs) and
#' classification losses with one value per case, so the extra dimensions are
#' averaged out before weighting. The weights sum in the denominator, so unit
#' weights give the plain mean and the scale of the loss does not move with the
#' weights' scale.
#'
#' @param per_case `torch_tensor`: Unreduced loss.
#' @param weights `torch_tensor`: One weight per case.
#'
#' @return `torch_tensor` scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_weighted_loss <- function(per_case, weights) {
  if (per_case[["dim"]]() > 1L) {
    per_case <- per_case[["mean"]](dim = 2L)
  }
  weighted <- per_case[["view"]](c(-1L)) * weights[["view"]](c(-1L))
  weighted[["sum"]]() / weights[["sum"]]()
} # /rtemis::torch_weighted_loss


# %% torch_optimizer ----
#' Build an optimizer over a module's parameters
#'
#' The three conditional arguments are NULL where they do not apply to the
#' chosen optimizer and where the caller left torch's own default in place, so
#' each is passed only when set.
#'
#' @param name Character: One of `TORCH_OPTIMIZERS`.
#' @param params List of `torch_tensor`: Parameters to optimize.
#' @param lr Numeric: Learning rate.
#' @param weight_decay Numeric: L2 penalty, decoupled under `adamw`.
#' @param betas Numeric or NULL: Exponential decay rates of the moment
#' estimates, for the Adam family.
#' @param eps Numeric or NULL: Denominator term for numerical stability.
#' @param momentum Numeric or NULL: Momentum factor, for `sgd` and `rmsprop`.
#'
#' @return `torch_Optimizer` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_optimizer <- function(
  name,
  params,
  lr,
  weight_decay,
  betas = NULL,
  eps = NULL,
  momentum = NULL
) {
  args <- list(params = params, lr = lr, weight_decay = weight_decay)
  if (name %in% c("adamw", "adam")) {
    if (!is.null(betas)) {
      args[["betas"]] <- betas
    }
    if (!is.null(eps)) {
      args[["eps"]] <- eps
    }
  } else if (identical(name, "rmsprop")) {
    if (!is.null(eps)) {
      args[["eps"]] <- eps
    }
    if (!is.null(momentum)) {
      args[["momentum"]] <- momentum
    }
  } else if (identical(name, "sgd") && !is.null(momentum)) {
    args[["momentum"]] <- momentum
  }
  do.call(
    switch(
      name,
      adamw = torch::optim_adamw,
      adam = torch::optim_adam,
      sgd = torch::optim_sgd,
      rmsprop = torch::optim_rmsprop
    ),
    args
  )
} # /rtemis::torch_optimizer


# %% torch_scheduler ----
#' Build a learning-rate scheduler
#'
#' Each schedule configures itself from the run's own budget rather than from
#' hyperparameters of its own: a scheduler-specific argument set would be five
#' to eight properties, each gated on one enum value, for knobs nobody reaches
#' for before `lr` and `max_epochs`. The derivations are documented on
#' [setup_MLP].
#'
#' @param name Character or NULL: One of `TORCH_SCHEDULERS`, or NULL for none.
#' @param optimizer `torch_Optimizer` object.
#' @param lr Numeric: Learning rate the optimizer was built with.
#' @param max_epochs Integer: Epoch budget.
#' @param steps_per_epoch Integer: Batches per epoch.
#' @param patience Integer: Early-stopping patience, which `reduce_on_plateau`
#' halves so that it reacts before training stops.
#'
#' @return List with `scheduler` (or NULL) and `per_batch` (Logical: whether the
#' schedule steps once per batch rather than once per epoch).
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_scheduler <- function(
  name,
  optimizer,
  lr,
  max_epochs,
  steps_per_epoch,
  patience
) {
  if (is.null(name)) {
    return(list(scheduler = NULL, per_batch = FALSE))
  }
  if (identical(name, "step")) {
    return(list(
      scheduler = torch::lr_step(
        optimizer,
        step_size = max(1L, max_epochs %/% 3L),
        gamma = 0.1
      ),
      per_batch = FALSE
    ))
  }
  if (identical(name, "cosine_annealing")) {
    return(list(
      scheduler = torch::lr_cosine_annealing(optimizer, T_max = max_epochs),
      per_batch = FALSE
    ))
  }
  if (identical(name, "one_cycle")) {
    return(list(
      scheduler = torch::lr_one_cycle(
        optimizer,
        max_lr = lr,
        epochs = max_epochs,
        steps_per_epoch = steps_per_epoch
      ),
      per_batch = TRUE
    ))
  }
  list(
    scheduler = torch::lr_reduce_on_plateau(
      optimizer,
      factor = 0.1,
      patience = max(1L, patience %/% 2L)
    ),
    per_batch = FALSE
  )
} # /rtemis::torch_scheduler


# %% torch_target ----
#' Build the target tensor for a supervised outcome
#'
#' Classification targets are 1-based class indices, which is what R torch's
#' `nn_cross_entropy_loss` expects; regression targets are a single column, so
#' that the loss's shape follows the module's output.
#'
#' @param y Numeric or factor: Outcome.
#' @param type Character: "Regression" or "Classification".
#'
#' @return `torch_tensor` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_target <- function(y, type) {
  if (identical(type, "Classification")) {
    torch::torch_tensor(as.integer(y), dtype = torch::torch_long())
  } else {
    torch::torch_tensor(
      matrix(as.numeric(y), ncol = 1L),
      dtype = torch::torch_float()
    )
  }
} # /rtemis::torch_target


# %% torch_batch_iterator ----
#' Iterate a dataloader without `coro`
#'
#' `dataloader_next(iter, completed = NULL)` returns NULL once the epoch is
#' exhausted, so a plain `repeat` reads the whole epoch. `coro::loop()` would do
#' the same and pull in a package this one does not otherwise need.
#'
#' @param dl `dataloader` object.
#' @param fn Function of one argument: Called with each batch.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_batch_iterator <- function(dl, fn) {
  iter <- torch::dataloader_make_iter(dl)
  repeat {
    batch <- torch::dataloader_next(iter, completed = NULL)
    if (is.null(batch)) {
      break
    }
    fn(batch)
  }
  invisible(NULL)
} # /rtemis::torch_batch_iterator


# %% torch_dataloader ----
#' Build a dataloader over inputs, target and case weights
#'
#' A batch is the module's input tensors in order, then the target, then the
#' weights -- so the loop splits it by counting the inputs rather than by
#' knowing what they mean.
#'
#' @param inputs List of `torch_tensor`: The module's inputs.
#' @param target `torch_tensor`: Outcome.
#' @param weights `torch_tensor`: One weight per case.
#' @param batch_size Integer: Cases per batch.
#' @param shuffle Logical: If TRUE, reshuffle every epoch.
#' @param drop_last Logical: If TRUE, drop the last incomplete batch.
#' @param num_workers Integer: Subprocesses for data loading.
#'
#' @return `dataloader` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_dataloader <- function(
  inputs,
  target,
  weights,
  batch_size,
  shuffle,
  drop_last = FALSE,
  num_workers = 0L
) {
  dataset <- do.call(
    torch::tensor_dataset,
    c(inputs, list(target, weights))
  )
  torch::dataloader(
    dataset,
    batch_size = batch_size,
    shuffle = shuffle,
    drop_last = drop_last,
    num_workers = num_workers
  )
} # /rtemis::torch_dataloader


# %% torch_l1_norm ----
#' Sum of the absolute values of the penalized parameters
#'
#' An L1 penalty has no torch optimizer argument -- `weight_decay` is L2 and,
#' under AdamW, decoupled -- so it is accumulated here and added to the loss.
#'
#' @param parameters Named list of `torch_tensor`: `module$parameters`.
#' @param names Character vector: Which of them to penalize.
#'
#' @return `torch_tensor` scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_l1_norm <- function(parameters, names) {
  total <- parameters[[names[[1L]]]][["abs"]]()[["sum"]]()
  for (nm in names[-1L]) {
    total <- total + parameters[[nm]][["abs"]]()[["sum"]]()
  }
  total
} # /rtemis::torch_l1_norm


# %% torch_fit ----
#' Train a torch module
#'
#' The training loop, generic over the module: it moves batches to the device,
#' calls the module on the batch's input tensors, reduces the weighted loss,
#' steps the optimizer and scheduler, and stops early on a validation set. It
#' never inspects the module, so a different architecture needs no change here.
#'
#' Early stopping requires `inputs_validation`: without it the loop runs the
#' full `max_epochs` and `patience` has no effect. The weights returned are
#' those of the best validation epoch, not of the last one -- a run that
#' overfits past its optimum would otherwise return the overfitted model and
#' report the optimum's loss.
#'
#' @param module `nn_module` object: Built by the algorithm.
#' @param inputs List of `torch_tensor`: Training inputs.
#' @param target `torch_tensor`: Training outcome.
#' @param weights `torch_tensor`: One weight per training case.
#' @param inputs_validation Optional list of `torch_tensor`: Validation inputs.
#' @param target_validation Optional `torch_tensor`: Validation outcome.
#' @param weights_validation Optional `torch_tensor`: Validation case weights.
#' @param loss Character: One of `TORCH_LOSSES`.
#' @param optimizer Character: One of `TORCH_OPTIMIZERS`.
#' @param lr Numeric: Learning rate.
#' @param weight_decay Numeric: L2 penalty.
#' @param betas,eps,momentum Numeric or NULL: Optimizer-specific settings.
#' @param lr_scheduler Character or NULL: One of `TORCH_SCHEDULERS`.
#' @param batch_size Integer: Cases per batch.
#' @param max_epochs Integer: Epoch budget.
#' @param patience Integer: Epochs without validation improvement before
#' stopping.
#' @param max_grad_norm Numeric or NULL: Gradient-norm clipping threshold.
#' @param l1_penalty Numeric: L1 coefficient, added to the loss by hand.
#' @param l1_parameters Character vector: Names of the parameters `l1_penalty`
#' applies to.
#' @param drop_last Logical: If TRUE, drop the last incomplete training batch.
#' @param num_workers Integer: Subprocesses for data loading.
#' @param device Character: Resolved device name.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with `module` (trained, holding the best weights),
#' `epochs_trained`, `best_epoch`, `best_loss` and `history` (a data.frame of
#' per-epoch training and validation loss). The training loss is the objective
#' that was minimized, so it includes `l1_penalty`; the validation loss is the
#' loss alone, since a penalty on the weights says nothing about held-out fit.
#' The two are therefore not comparable to each other when `l1_penalty` is set.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_fit <- function(
  module,
  inputs,
  target,
  weights,
  inputs_validation = NULL,
  target_validation = NULL,
  weights_validation = NULL,
  loss = "mse",
  optimizer = "adamw",
  lr = 1e-3,
  weight_decay = 0,
  betas = NULL,
  eps = NULL,
  momentum = NULL,
  lr_scheduler = NULL,
  batch_size = 256L,
  max_epochs = 100L,
  patience = 10L,
  max_grad_norm = NULL,
  l1_penalty = 0,
  l1_parameters = character(),
  drop_last = FALSE,
  num_workers = 0L,
  device = "cpu",
  verbosity = 1L
) {
  dev <- torch::torch_device(device)
  module[["to"]](device = dev)
  n_inputs <- length(inputs)
  loss_fn <- torch_loss_module(loss)
  penalize <- l1_penalty > 0 && length(l1_parameters) > 0L
  train_loader <- torch_dataloader(
    inputs,
    target,
    weights,
    batch_size = batch_size,
    shuffle = TRUE,
    drop_last = drop_last,
    num_workers = num_workers
  )
  validate <- !is.null(inputs_validation)
  validation_loader <- if (validate) {
    torch_dataloader(
      inputs_validation,
      target_validation,
      weights_validation,
      batch_size = batch_size,
      shuffle = FALSE,
      num_workers = num_workers
    )
  }
  # `drop_last` can empty the loader when there are fewer cases than one batch,
  # which would train on nothing and report a loss of NaN.
  steps_per_epoch <- length(train_loader)
  if (steps_per_epoch == 0L) {
    rtemis.core::abort(
      "No complete batch of ",
      batch_size,
      " cases can be formed from ",
      target[["size"]](1L),
      " training cases with `drop_last = TRUE`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  opt <- torch_optimizer(
    optimizer,
    params = module[["parameters"]],
    lr = lr,
    weight_decay = weight_decay,
    betas = betas,
    eps = eps,
    momentum = momentum
  )
  sched <- torch_scheduler(
    lr_scheduler,
    optimizer = opt,
    lr = lr,
    max_epochs = max_epochs,
    steps_per_epoch = steps_per_epoch,
    patience = patience
  )
  epoch_loss <- function(batch, train) {
    inputs_batch <- lapply(batch[seq_len(n_inputs)], function(t) {
      t[["to"]](device = dev)
    })
    target_batch <- batch[[n_inputs + 1L]][["to"]](device = dev)
    weights_batch <- batch[[n_inputs + 2L]][["to"]](device = dev)
    output <- do.call(module, inputs_batch)
    value <- torch_weighted_loss(
      loss_fn(output, target_batch),
      weights_batch
    )
    if (train && penalize) {
      value <- value +
        l1_penalty * torch_l1_norm(module[["parameters"]], l1_parameters)
    }
    value
  }
  best_loss <- Inf
  best_epoch <- 0L
  best_state <- NULL
  since_best <- 0L
  epochs_trained <- 0L
  history <- data.frame(
    epoch = integer(),
    loss_training = numeric(),
    loss_validation = numeric()
  )
  for (epoch in seq_len(max_epochs)) {
    module[["train"]]()
    running <- 0
    torch_batch_iterator(train_loader, function(batch) {
      opt[["zero_grad"]]()
      value <- epoch_loss(batch, train = TRUE)
      value[["backward"]]()
      if (!is.null(max_grad_norm)) {
        torch::nn_utils_clip_grad_norm_(module[["parameters"]], max_grad_norm)
      }
      opt[["step"]]()
      if (sched[["per_batch"]]) {
        sched[["scheduler"]][["step"]]()
      }
      running <<- running + as.numeric(value[["item"]]())
    })
    loss_training <- running / steps_per_epoch
    epochs_trained <- epoch
    loss_validation <- NA_real_
    if (validate) {
      module[["eval"]]()
      running_validation <- 0
      n_batches <- 0L
      torch::with_no_grad({
        torch_batch_iterator(validation_loader, function(batch) {
          running_validation <<- running_validation +
            as.numeric(epoch_loss(batch, train = FALSE)[["item"]]())
          n_batches <<- n_batches + 1L
        })
      })
      loss_validation <- running_validation / n_batches
    }
    history <- rbind(
      history,
      data.frame(
        epoch = epoch,
        loss_training = loss_training,
        loss_validation = loss_validation
      )
    )
    dbg(
      "Epoch ",
      epoch,
      ": training loss ",
      format(loss_training, digits = 5L),
      if (validate) {
        paste0(", validation loss ", format(loss_validation, digits = 5L))
      } else {
        ""
      },
      verbosity = verbosity
    )
    if (!sched[["per_batch"]] && !is.null(sched[["scheduler"]])) {
      if (identical(lr_scheduler, "reduce_on_plateau")) {
        sched[["scheduler"]][["step"]](
          if (validate) loss_validation else loss_training
        )
      } else {
        sched[["scheduler"]][["step"]]()
      }
    }
    if (!validate) {
      next
    }
    if (loss_validation < best_loss) {
      best_loss <- loss_validation
      best_epoch <- epoch
      # Cloned: a state dict holds references to the live parameters, which the
      # next optimizer step would move.
      best_state <- lapply(
        module[["state_dict"]](),
        function(t) t[["detach"]]()[["clone"]]()
      )
      since_best <- 0L
    } else {
      since_best <- since_best + 1L
      if (since_best >= patience) {
        msg0(
          "Early stopping at epoch ",
          epoch,
          "; best validation loss ",
          format(best_loss, digits = 5L),
          " at epoch ",
          best_epoch,
          "...",
          verbosity = verbosity
        )
        break
      }
    }
  }
  if (!is.null(best_state)) {
    module[["load_state_dict"]](best_state)
  }
  module[["eval"]]()
  list(
    module = module,
    epochs_trained = epochs_trained,
    best_epoch = if (validate) best_epoch else epochs_trained,
    best_loss = if (validate) best_loss else NA_real_,
    history = history
  )
} # /rtemis::torch_fit


# %% torch_forward ----
#' Run a trained module over inputs in batches
#'
#' @param module `nn_module` object.
#' @param inputs List of `torch_tensor`: Inputs, in the module's order.
#' @param batch_size Integer: Cases per batch.
#' @param device Character: Resolved device name.
#'
#' @return Matrix of the module's raw outputs, one row per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_forward <- function(module, inputs, batch_size = 1024L, device = "cpu") {
  dev <- torch::torch_device(device)
  module[["to"]](device = dev)
  module[["eval"]]()
  n_cases <- inputs[[1L]][["size"]](1L)
  starts <- seq(1L, n_cases, by = batch_size)
  out <- vector("list", length(starts))
  torch::with_no_grad({
    for (i in seq_along(starts)) {
      index <- seq(starts[[i]], min(starts[[i]] + batch_size - 1L, n_cases))
      slice <- lapply(inputs, function(t) {
        t[index, , drop = FALSE][["to"]](device = dev)
      })
      out[[i]] <- as.matrix(do.call(module, slice)[["to"]](device = "cpu"))
    }
  })
  do.call(rbind, out)
} # /rtemis::torch_forward


# %% torch_state ----
#' Serialize a module's parameters to a raw vector
#'
#' A fitted module holds external pointers, so `saveRDS()` writes an object that
#' reloads as "external pointer is not valid" and fails at first prediction
#' rather than at read. A raw vector is an ordinary R value, so a model carrying
#' one saves and reloads with no special path; the architecture is rebuilt from
#' the model's own recorded settings and the parameters loaded back into it.
#'
#' @param module `nn_module` object.
#'
#' @return Raw vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_state <- function(module) {
  torch::torch_serialize(module[["state_dict"]]())
} # /rtemis::torch_state


# %% torch_restore ----
#' Load serialized parameters back into a freshly built module
#'
#' @param module `nn_module` object: Same architecture the state came from.
#' @param state Raw vector: Output of `torch_state()`.
#'
#' @return `module`, with the stored parameters loaded.
#'
#' @author EDG
#' @keywords internal
#' @noRd
torch_restore <- function(module, state) {
  module[["load_state_dict"]](torch::torch_load(state))
  module[["eval"]]()
  module
} # /rtemis::torch_restore
