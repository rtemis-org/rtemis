# decomp_Autoencoder.R
# ::rtemis::
# 2026- EDG rtemis.org

# Torch autoencoders as decompositions. The training loop, the vocabularies and
# the serialization helpers live in `065_Torch.R`; what is here is the
# autoencoder itself: how the hidden widths are decided, how the inputs are
# standardized, the module, and the fit object that survives `saveRDS()`.
#
# The methods are defined on `AutoencoderBaseConfig`, so every autoencoder leaf
# inherits them through S7 dispatch.
#
# Inside the module, a field is **read** with `[[` and **written** with `$`, as
# in `train_MLP.R`: `$<-` registers a submodule with torch, and `$` on the read
# side reads to static analysis as a call to an unbound function.

# %% AutoencoderFit ----
#' @title AutoencoderFit
#'
#' @description
#' A fitted autoencoder as plain R data. A torch module holds external pointers
#' and fails at first use after `readRDS()`, so the fit stores the serialized
#' parameters and everything needed to rebuild the module around them, and
#' `apply_decomp_()` and `reconstruct_()` rebuild it. Every architectural
#' setting is stored here, so the fit is readable on its own.
#'
#' @field state Raw: Serialized module parameters, from `torch_state()`.
#' @field features Character: Input features, in the order the fit used.
#' @field k Integer: Latent width.
#' @field hidden_units Integer: Encoder hidden widths, input side first.
#' @field activation,norm Character: Layer settings; `norm` NULL for none.
#' @field dropout,input_dropout,input_noise Numeric: Regularization settings.
#' @field variational Logical: Whether the module is a variational autoencoder.
#' @field beta Optional Numeric: KL weight a variational fit was trained with;
#'   NULL for a plain one.
#' @field center,scale Numeric: Training statistics the inputs are standardized
#'   with, one per feature.
#' @field device Character: Device the fit was trained on.
#' @field n_validation Integer: Cases held out for early stopping.
#' @field epochs_trained,best_epoch Integer: Epochs run, and the epoch whose
#'   weights were kept.
#' @field history data.frame: Per-epoch training and validation loss.
#'
#' @author EDG
#' @keywords internal
#' @noRd
AutoencoderFit <- new_class(
  name = "AutoencoderFit",
  package = "rtemis",
  properties = list(
    state = class_raw,
    features = class_character,
    k = class_integer,
    hidden_units = class_integer,
    activation = class_character,
    norm = NULL | class_character,
    dropout = class_numeric,
    input_dropout = class_numeric,
    input_noise = class_numeric,
    variational = class_logical,
    beta = NULL | class_numeric,
    center = class_numeric,
    scale = class_numeric,
    device = class_character,
    n_validation = class_integer,
    epochs_trained = class_integer,
    best_epoch = class_integer,
    history = class_data.frame
  ),
  validator = function(self) {
    p <- length(self@features)
    if (length(self@center) != p || length(self@scale) != p) {
      return("center and scale need one value per feature.")
    }
    if (any(self@scale <= 0)) {
      return("scale must be positive.")
    }
    if (self@variational == is.null(self@beta)) {
      return("beta is set exactly when the fit is variational.")
    }
    NULL
  }
) # /rtemis::AutoencoderFit


# %% repr.AutoencoderFit ----
method(repr, AutoencoderFit) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name("AutoencoderFit", pad = pad, output_type = output_type),
    repr_ls(
      list(
        features = length(x@features),
        hidden_units = x@hidden_units,
        k = x@k,
        variational = x@variational,
        beta = x@beta,
        device = x@device,
        epochs_trained = x@epochs_trained,
        best_epoch = x@best_epoch
      ),
      pad = pad,
      output_type = output_type
    )
  )
} # /rtemis::repr.AutoencoderFit


# %% print.AutoencoderFit ----
method(print, AutoencoderFit) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.AutoencoderFit


# %% autoencoder_beta ----
#' The KL weight of a variational autoencoder config, or NULL
#'
#' The one thing the fitting method needs to know about the leaf: NULL trains a
#' plain autoencoder, a number a variational one with that weight. Dispatched,
#' so the shared `decomp_()` method never tests for a class.
#'
#' @param config `AutoencoderBaseConfig` object.
#'
#' @return Numeric scalar, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_beta <- new_generic("autoencoder_beta", "config")


# %% autoencoder_beta.AutoencoderBaseConfig ----
method(autoencoder_beta, AutoencoderBaseConfig) <- function(config) {
  NULL
} # /rtemis::autoencoder_beta.AutoencoderBaseConfig


# %% autoencoder_beta.VariationalAutoencoderConfig ----
method(autoencoder_beta, VariationalAutoencoderConfig) <- function(config) {
  config[["beta"]]
} # /rtemis::autoencoder_beta.VariationalAutoencoderConfig


# %% vae_objective ----
#' The per-case objective of a variational autoencoder
#'
#' For each case, the reconstruction loss summed over features plus `beta`
#' times the KL divergence of the case's latent normal from the standard
#' normal, in closed form `-0.5 * sum(1 + logvar - mu^2 - exp(logvar))` over
#' the latent dimensions. Summed over features, not averaged, so `beta` weighs
#' the prior against the whole reconstruction, as in the beta-VAE literature.
#' `torch_fit()` takes the weighted mean over cases.
#'
#' @param loss Character: One of `TORCH_REGRESSION_LOSSES`.
#' @param beta Numeric: KL weight.
#'
#' @return Function of the module's output, `list(reconstruction, mu, logvar)`,
#' and the target, returning a tensor with one loss per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
vae_objective <- function(loss, beta) {
  reconstruction_loss <- torch_loss_module(loss)
  function(output, target) {
    mu <- output[[2L]]
    logvar <- output[[3L]]
    reconstruction <- reconstruction_loss(output[[1L]], target)[["sum"]](
      dim = 2L
    )
    kl <- -0.5 *
      (1 + logvar - mu[["pow"]](2) - logvar[["exp"]]())[["sum"]](dim = 2L)
    reconstruction + beta * kl
  }
} # /rtemis::vae_objective


# %% autoencoder_hidden_units ----
#' Resolve the encoder's hidden widths
#'
#' Given widths are used as they are. Unset gives one hidden layer whose width
#' is the geometric mean of the input and latent widths -- halfway, on the log
#' scale, between the data and the bottleneck -- held between 32 and 512 and
#' never below `k`. The floor is measured: on iris (`k = 2`), a 3-unit layer
#' (the unfloored mean) reconstructed nothing and an 8-unit one trailed PCA,
#' while 32 units matched it (2026-09-29).
#'
#' @param hidden_units Integer vector or NULL: Widths given in the config.
#' @param n_features Integer: Input width.
#' @param k Integer: Latent width.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Integer vector: One width per encoder hidden layer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_hidden_units <- function(
  hidden_units,
  n_features,
  k,
  verbosity = 1L
) {
  if (!is.null(hidden_units)) {
    msg0(
      "Encoder hidden layers, as given: ",
      paste(hidden_units, collapse = ", "),
      "...",
      verbosity = verbosity
    )
    return(as.integer(hidden_units))
  }
  width <- max(
    as.integer(k),
    min(512L, max(32L, as.integer(round(sqrt(n_features * k)))))
  )
  msg0(
    "Encoder hidden layer, derived from ",
    n_features,
    " features and k = ",
    k,
    ": ",
    width,
    "...",
    verbosity = verbosity
  )
  width
} # /rtemis::autoencoder_hidden_units


# %% autoencoder_batch_size ----
#' Resolve the batch size
#'
#' Unset gives a tenth of the training cases, held between 16 and 256, so every
#' epoch takes about ten optimization steps until the data are large. A fixed
#' 256 gives 150 cases one step per epoch, and a 100-epoch fit on iris then
#' reconstructs nothing; a fixed 32 gave 10,000 cases seven times the training
#' time of 256 for the same reconstruction (2026-09-29).
#'
#' @param batch_size Integer or NULL: Batch size given in the config.
#' @param n_training Integer: Training cases.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_batch_size <- function(batch_size, n_training) {
  if (!is.null(batch_size)) {
    return(as.integer(batch_size))
  }
  as.integer(min(256L, max(16L, n_training %/% 10L)))
} # /rtemis::autoencoder_batch_size


# %% autoencoder_standardize ----
#' Standardize a data matrix with a fit's training statistics
#'
#' @param xm Numeric matrix: Cases by features, in the fit's feature order.
#' @param center,scale Numeric: One value per feature.
#'
#' @return Numeric matrix.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_standardize <- function(xm, center, scale) {
  sweep(sweep(xm, 2L, center, FUN = "-"), 2L, scale, FUN = "/")
} # /rtemis::autoencoder_standardize


# %% autoencoder_matrix ----
#' Take the features a fit was trained on as a numeric matrix
#'
#' The only point on the apply and reconstruct paths that sees the data before
#' torch does, so a missing feature or a missing value is rejected here, naming
#' it: a missing value would otherwise propagate through the network as NaN.
#'
#' @param x Tabular data.
#' @param fit `AutoencoderFit` object.
#'
#' @return Numeric matrix, cases by `fit@features`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_matrix <- function(x, fit) {
  x <- as.data.frame(x)
  absent <- setdiff(fit@features, names(x))
  if (length(absent) > 0L) {
    rtemis.core::abort(
      "Data is missing ",
      length(absent),
      " feature(s) the autoencoder was fit on: ",
      paste0("'", absent, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  xm <- as.matrix(x[, fit@features, drop = FALSE])
  incomplete <- fit@features[colSums(is.na(xm)) > 0L]
  if (length(incomplete) > 0L) {
    rtemis.core::abort(
      "The autoencoder cannot encode missing values; ",
      length(incomplete),
      " feature(s) have them: ",
      paste0("'", incomplete, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  xm
} # /rtemis::autoencoder_matrix


# %% autoencoder_module ----
#' Build the autoencoder module
#'
#' An `encoder` and a `decoder` submodule, each callable on its own, so encoding
#' and decoding are `torch_forward()` over one of them. Each hidden layer is a
#' linear map, the activation, the normalization and dropout, in that order (as
#' `mlp_module()` with `norm_first = FALSE`); the latent layer and the output
#' layer are linear.
#'
#' A variational module's encoder holds the hidden layers and two linear heads,
#' for the means and the log variances of the latent normal; called on its own
#' it returns the means, which are the components. The full module returns
#' `list(reconstruction, mu, logvar)`, decoding a reparameterized draw
#' `mu + exp(logvar / 2) * noise` in training mode and `mu` in eval mode, so
#' the validation loss and every output after training are deterministic.
#' Every noise draw goes through `torch_seeded_randn()`, so a seeded fit
#' reproduces on mps too.
#'
#' The corruption that makes the model denoising is applied by the full module
#' to its input, and only in training mode: Gaussian noise of standard
#' deviation `input_noise`, then dropout at `input_dropout`. Training passes the
#' clean data as the target, so the model learns to undo the corruption; the
#' validation loss, `apply_decomp_()` and `reconstruct_()` run in eval mode and
#' see clean inputs.
#'
#' The generator is created inside the function because `torch` is a
#' Suggests-gated backend.
#'
#' @param n_features Integer: Input width.
#' @param hidden_units Integer vector: Encoder hidden widths, input side first.
#' @param k Integer: Latent width.
#' @param activation Character: One of `TORCH_ACTIVATIONS`.
#' @param norm Character or NULL: One of `TORCH_NORMS`, or NULL for none.
#' @param dropout Numeric: Dropout after every hidden layer.
#' @param input_dropout Numeric: Masking probability on the training input.
#' @param input_noise Numeric: Gaussian noise level on the training input.
#' @param variational Logical: Whether to build a variational autoencoder.
#'
#' @return `nn_module` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_module <- function(
  n_features,
  hidden_units,
  k,
  activation,
  norm,
  dropout,
  input_dropout,
  input_noise,
  variational = FALSE
) {
  layers <- function(widths) {
    blocks <- lapply(seq_len(length(widths) - 1L), function(i) {
      list(
        torch::nn_linear(widths[[i]], widths[[i + 1L]]),
        torch_activation_module(activation),
        torch_norm_module(norm, widths[[i + 1L]]),
        torch::nn_dropout(dropout)
      )
    })
    unlist(blocks, recursive = FALSE)
  }
  variational_encoder <- torch::nn_module(
    classname = "VariationalEncoder",
    initialize = function(widths, k) {
      self$trunk <- do.call(torch::nn_sequential, layers(widths))
      self$mu <- torch::nn_linear(widths[[length(widths)]], k)
      self$logvar <- torch::nn_linear(widths[[length(widths)]], k)
    },
    forward = function(x) {
      self[["mu"]](self[["trunk"]](x))
    }
  )
  generator <- torch::nn_module(
    classname = "Autoencoder",
    initialize = function(
      n_features,
      hidden_units,
      k,
      input_dropout,
      input_noise,
      variational
    ) {
      self$input_noise <- input_noise
      self$variational <- variational
      self$corrupt <- torch::nn_dropout(input_dropout)
      encoder_widths <- c(n_features, hidden_units)
      decoder_widths <- c(k, rev(hidden_units))
      self$encoder <- if (variational) {
        variational_encoder(encoder_widths, k)
      } else {
        do.call(
          torch::nn_sequential,
          c(
            layers(encoder_widths),
            list(torch::nn_linear(encoder_widths[[length(encoder_widths)]], k))
          )
        )
      }
      self$decoder <- do.call(
        torch::nn_sequential,
        c(
          layers(decoder_widths),
          list(torch::nn_linear(
            decoder_widths[[length(decoder_widths)]],
            n_features
          ))
        )
      )
    },
    forward = function(x) {
      if (self[["training"]]) {
        if (self[["input_noise"]] > 0) {
          x <- x + self[["input_noise"]] * torch_seeded_randn(x)
        }
        x <- self[["corrupt"]](x)
      }
      if (!self[["variational"]]) {
        return(self[["decoder"]](self[["encoder"]](x)))
      }
      encoder <- self[["encoder"]]
      h <- encoder[["trunk"]](x)
      mu <- encoder[["mu"]](h)
      logvar <- encoder[["logvar"]](h)
      z <- if (self[["training"]]) {
        mu + (0.5 * logvar)[["exp"]]() * torch_seeded_randn(mu)
      } else {
        mu
      }
      list(self[["decoder"]](z), mu, logvar)
    }
  )
  generator(
    n_features = n_features,
    hidden_units = hidden_units,
    k = k,
    input_dropout = input_dropout,
    input_noise = input_noise,
    variational = variational
  )
} # /rtemis::autoencoder_module


# %% autoencoder_fit_module ----
#' Rebuild a fit's module and load its parameters
#'
#' @param fit `AutoencoderFit` object.
#'
#' @return `nn_module` object in eval mode.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_fit_module <- function(fit) {
  torch_restore(
    autoencoder_module(
      n_features = length(fit@features),
      hidden_units = fit@hidden_units,
      k = fit@k,
      activation = fit@activation,
      norm = fit@norm,
      dropout = fit@dropout,
      input_dropout = fit@input_dropout,
      input_noise = fit@input_noise,
      variational = fit@variational
    ),
    fit@state
  )
} # /rtemis::autoencoder_fit_module


# %% autoencoder_tensor ----
#' A numeric matrix as a float tensor
#'
#' @param xm Numeric matrix.
#'
#' @return `torch_tensor` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_tensor <- function(xm) {
  torch::torch_tensor(unname(xm), dtype = torch::torch_float())
} # /rtemis::autoencoder_tensor


# %% autoencoder_scores ----
#' Encode data with a fitted module
#'
#' The one encoding path: `decomp_()` computes its components through it after
#' the fit, and `apply_decomp_()` through it on new data, so applying a fit to
#' its own training data reproduces the fitted components exactly.
#'
#' @param module `nn_module` object: The fitted autoencoder.
#' @param fit `AutoencoderFit` object.
#' @param x Tabular data.
#' @param algorithm Character: Algorithm name, which prefixes the column names.
#' @param device Character: Torch device name.
#'
#' @return Numeric matrix, cases by `k`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_scores <- function(module, fit, x, algorithm, device) {
  standardized <- autoencoder_standardize(
    autoencoder_matrix(x, fit),
    fit@center,
    fit@scale
  )
  transformed <- torch_forward(
    module[["encoder"]],
    list(autoencoder_tensor(standardized)),
    device = device
  )
  colnames(transformed) <- paste0(algorithm, "_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::autoencoder_scores


# %% autoencoder_device ----
#' The torch device name for an execution config's requested device
#'
#' @param execution_config Optional `ExecutionConfig` object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_device <- function(execution_config = NULL) {
  requested <- execution_device(execution_config)
  torch_device_name(torch_training_device(requested), requested)
} # /rtemis::autoencoder_device


# %% autoencoder_holdout ----
#' Draw the cases held out for early stopping
#'
#' Drawn from R's random stream, which `decomp()` seeds from the execution
#' config. Too few cases to hold out one while keeping one for training
#' disables early stopping.
#'
#' @param n Integer: Number of cases.
#' @param fraction Numeric \[0, 1): Fraction to hold out.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Integer vector of held-out row indices, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
autoencoder_holdout <- function(n, fraction, verbosity = 1L) {
  n_validation <- as.integer(round(fraction * n))
  if (fraction > 0 && (n_validation == 0L || n_validation >= n)) {
    msg0(
      "Too few cases to hold out ",
      format(fraction),
      " of them; training for the full epoch budget without early stopping...",
      verbosity = verbosity
    )
    return(integer())
  }
  if (n_validation == 0L) {
    return(integer())
  }
  sort(sample.int(n, n_validation))
} # /rtemis::autoencoder_holdout


# %% decomp_.AutoencoderBaseConfig ----
#' Autoencoder decomposition
#'
#' @param config `AutoencoderBaseConfig` object.
#' @param x Tabular data: Numeric features.
#' @param execution_config Optional `ExecutionConfig` object: Threads, device
#' and, through `decomp()`, the seed.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `decom` (`AutoencoderFit`), `transformed` and `config`,
#' the config with the hidden widths the fit resolved.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decomp_, AutoencoderBaseConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_dependencies("torch")
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Standardize ----
  xm <- as.matrix(x)
  features <- colnames(xm)
  center <- colMeans(xm)
  scale <- apply(xm, 2L, stats::sd)
  # A constant feature carries nothing to scale; it is centered to zero.
  scale[!is.finite(scale) | scale == 0] <- 1
  standardized <- autoencoder_standardize(xm, center, scale)

  # Architecture ----
  k <- config[["k"]]
  hidden_units <- autoencoder_hidden_units(
    config[["hidden_units"]],
    n_features = length(features),
    k = k,
    verbosity = verbosity
  )

  # Resources ----
  set_torch_threads(algorithm_threads(execution_config), verbosity = verbosity)
  device <- autoencoder_device(execution_config)
  check_mps_reproducible(
    device,
    seed = if (is.null(execution_config)) NULL else execution_config@seed,
    dropout = c(config[["dropout"]], config[["input_dropout"]])
  )

  # Randomness ----
  # Both from R's stream, which `decomp()` seeds from the execution config:
  # the held-out cases, then torch's own generator, which `set.seed()` does not
  # reach. Seeded before the module is built, since that draws the weights.
  validation_index <- autoencoder_holdout(
    NROW(xm),
    config[["validation_fraction"]],
    verbosity = verbosity
  )
  torch::torch_manual_seed(sample.int(.Machine[["integer.max"]], 1L))

  # Train ----
  msg("Training", config@algorithm, "on", device, "...", verbosity = verbosity)
  beta <- autoencoder_beta(config)
  variational <- !is.null(beta)
  module <- autoencoder_module(
    n_features = length(features),
    hidden_units = hidden_units,
    k = k,
    activation = config[["activation"]],
    norm = config[["norm"]],
    dropout = config[["dropout"]],
    input_dropout = config[["input_dropout"]],
    input_noise = config[["input_noise"]],
    variational = variational
  )
  training <- standardized[
    setdiff(seq_len(NROW(xm)), validation_index),
    ,
    drop = FALSE
  ]
  validate <- length(validation_index) > 0L
  batch_size <- autoencoder_batch_size(config[["batch_size"]], NROW(training))
  # Batch normalization needs more than one case per batch to compute a
  # variance, so a trailing batch of one aborts the fit partway through.
  drop_last <- identical(config[["norm"]], "batch_norm") &&
    NROW(training) %% batch_size == 1L
  if (drop_last) {
    msg(
      "Dropping the last batch: batch normalization cannot use a batch of one case.",
      verbosity = verbosity
    )
  }
  training_tensor <- autoencoder_tensor(training)
  validation_tensor <- if (validate) {
    autoencoder_tensor(standardized[validation_index, , drop = FALSE])
  }
  fitted <- torch_fit(
    module = module,
    inputs = list(training_tensor),
    target = training_tensor,
    weights = torch::torch_ones(NROW(training), 1L),
    inputs_validation = if (validate) list(validation_tensor),
    target_validation = validation_tensor,
    weights_validation = if (validate) {
      torch::torch_ones(length(validation_index), 1L)
    },
    loss = config[["loss"]],
    objective = if (variational) vae_objective(config[["loss"]], beta),
    optimizer = config[["optimizer"]],
    lr = config[["lr"]],
    weight_decay = config[["weight_decay"]],
    betas = torch_betas(config[["beta1"]], config[["beta2"]]),
    eps = config[["eps"]],
    momentum = config[["momentum"]],
    lr_scheduler = config[["lr_scheduler"]],
    batch_size = batch_size,
    max_epochs = config[["max_epochs"]],
    patience = config[["patience"]],
    max_grad_norm = config[["max_grad_norm"]],
    drop_last = drop_last,
    device = device,
    verbosity = verbosity
  )

  # Components ----
  # Encoded after the fit, in eval mode, on the training device and through the
  # helper `apply_decomp_()` uses, so applying the fit to these cases
  # reproduces them.
  decom <- AutoencoderFit(
    state = torch_state(fitted[["module"]]),
    features = features,
    k = k,
    hidden_units = hidden_units,
    activation = config[["activation"]],
    norm = config[["norm"]],
    dropout = config[["dropout"]],
    input_dropout = config[["input_dropout"]],
    input_noise = config[["input_noise"]],
    variational = variational,
    beta = beta,
    center = unname(center),
    scale = unname(scale),
    device = device,
    n_validation = length(validation_index),
    epochs_trained = as.integer(fitted[["epochs_trained"]]),
    best_epoch = as.integer(fitted[["best_epoch"]]),
    history = fitted[["history"]]
  )
  transformed <- autoencoder_scores(
    fitted[["module"]],
    decom,
    x,
    algorithm = config@algorithm,
    device = device
  )
  # Resolved here, from the data, so the record would otherwise report unset
  # values the fit demonstrably used.
  config@hidden_units <- hidden_units
  config@batch_size <- batch_size
  list(decom = decom, transformed = transformed, config = config)
} # /rtemis::decomp_.AutoencoderBaseConfig


# %% apply_decomp_.AutoencoderBaseConfig ----
#' Apply a fitted autoencoder to new data
#'
#' @param config `AutoencoderBaseConfig` object.
#' @param decom `AutoencoderFit` object.
#' @param new_data Tabular data: The fit's features.
#' @param execution_config Optional `ExecutionConfig`: Where the fit is applied.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix of components.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, AutoencoderBaseConfig) <- function(
  config,
  decom,
  new_data,
  execution_config = NULL,
  verbosity = 1L
) {
  check_dependencies("torch")
  check_is_S7(decom, AutoencoderFit)
  set_torch_threads(algorithm_threads(execution_config), verbosity = verbosity)
  autoencoder_scores(
    autoencoder_fit_module(decom),
    decom,
    new_data,
    algorithm = config@algorithm,
    device = autoencoder_device(execution_config)
  )
} # /rtemis::apply_decomp_.AutoencoderBaseConfig


# %% reconstruct_.AutoencoderBaseConfig ----
#' Decode autoencoder components back to input space
#'
#' @details
#' The decoder's output is in standardized units; the training statistics
#' stored on the fit map it back to the units of the data. Standardization is
#' per feature, so `x` is not needed.
#'
#' @param config `AutoencoderBaseConfig` object.
#' @param decom `AutoencoderFit` object.
#' @param transformed Numeric matrix: Components, cases by `k`.
#' @param x Tabular data: The data being reconstructed.
#' @param execution_config Optional `ExecutionConfig`: Where the work runs.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix: Reconstruction in input units, cases by features.
#'
#' @keywords internal
#' @noRd
method(reconstruct_, AutoencoderBaseConfig) <- function(
  config,
  decom,
  transformed,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  check_dependencies("torch")
  check_is_S7(decom, AutoencoderFit)
  set_torch_threads(algorithm_threads(execution_config), verbosity = verbosity)
  decoded <- torch_forward(
    autoencoder_fit_module(decom)[["decoder"]],
    list(autoencoder_tensor(as.matrix(transformed))),
    device = autoencoder_device(execution_config)
  )
  reconstructed <- sweep(
    sweep(decoded, 2L, decom@scale, FUN = "*"),
    2L,
    decom@center,
    FUN = "+"
  )
  colnames(reconstructed) <- decom@features
  reconstructed
} # /rtemis::reconstruct_.AutoencoderBaseConfig


# %% training_device.AutoencoderBaseConfig ----
#' The device an autoencoder fit will run on
#'
#' Resolved once here for `decomp()`'s resources line and again in `decomp_()`
#' for the fit; the resolution is deterministic, so the two agree. NULL when
#' libtorch is absent, since `decomp_()` is about to report that.
#'
#' @param x `AutoencoderBaseConfig` object.
#' @param requested Optional `DeviceConfig` object.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(training_device, AutoencoderBaseConfig) <- function(
  x,
  requested = NULL
) {
  torch_training_device(requested)
} # /rtemis::training_device.AutoencoderBaseConfig
