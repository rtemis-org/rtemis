# train_MLP.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Shape vocabulary: Talos <https://mikkokotila.github.io/slate/#shapes>, by
#   way of AutoPyTorch's `get_shaped_neuron_counts()`.
# - Embedding sizing: fastai's `emb_sz_rule`.
# - torch for R <https://torch.mlverse.org/docs/>
#
# The generic training loop, the device, optimizer, scheduler and loss
# vocabularies, and the serialization helpers all live in `065_Torch.R` and know
# nothing about this algorithm. What is here is the MLP itself: how the hidden
# widths are decided, how the design frame becomes tensors, and the module.
#
# Inside the module, a field is **read** with `[[` and **written** with `$`.
# Both halves matter: `$<-` on an `nn_module` is what registers a submodule or a
# parameter with torch, and `[[<-` would bypass that; `$` on the read side reads
# to static analysis as a call to a free function of the field's name, which
# `object_usage_linter` reports as an unbound global.

# %% mlp_ramp ----
#' Interpolate a layer width linearly over `n` layers
#'
#' Linear, not geometric: the obvious guess (halving each layer) does not match
#' the reference vocabulary this implements.
#'
#' @param from,to Numeric: Widths at the two ends, both inclusive.
#' @param n Integer: Number of widths to produce.
#'
#' @return Integer vector of length `n`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_ramp <- function(from, to, n) {
  if (n <= 0L) {
    return(integer())
  }
  if (n == 1L) {
    return(as.integer(round(from)))
  }
  as.integer(round(seq(from, to, length.out = n)))
} # /rtemis::mlp_ramp


# %% mlp_shape_units ----
#' Generate hidden layer widths from a shape
#'
#' Every shape returns **exactly** `layers` widths. The reference implementation
#' does not: it composes its segments and then warns that "layer count does not
#' match" for `long_funnel`, `diamond`, `hexagon` and `stairs`. A resolver that
#' silently returns the wrong depth under a tuner is a bad failure mode, so the
#' segments here are sized to sum to `layers` by construction.
#'
#' The narrowest generated layer is `max_units / layers`, not the network's
#' output width. Tapering to the output width is what the reference does, and it
#' puts a one-unit layer at the bottom of every regression funnel and a
#' `n_classes`-unit layer at the bottom of every classification funnel -- a
#' bottleneck rather than a taper. The chosen floor reproduces the common
#' hand-written pattern instead: two layers from 200 give `200, 100`.
#'
#' @param shape Character: One of `MLP_SHAPES`.
#' @param layers Integer: Number of hidden layers.
#' @param max_units Integer: Width of the widest layer.
#' @param in_feat Integer: Encoded input width, where a shape rises from it.
#'
#' @return Integer vector of length `layers`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_shape_units <- function(shape, layers, max_units, in_feat) {
  layers <- as.integer(layers)
  max_units <- as.integer(max_units)
  if (layers == 1L) {
    return(max_units)
  }
  narrowest <- max(1L, max_units %/% layers)
  # A shape that rises does so from the input width, which `shape_max_units` may
  # sit below when the user set it explicitly.
  start <- min(as.integer(in_feat), max_units)
  # A descending segment continues from `max_units` rather than restarting at
  # it, so the layer already emitted is not repeated.
  descend <- function(n) mlp_ramp(max_units, narrowest, n + 1L)[-1L]
  units <- switch(
    shape,
    constant = rep(max_units, layers),
    funnel = mlp_ramp(max_units, narrowest, layers),
    triangle = mlp_ramp(start, max_units, layers),
    long_funnel = {
      flat <- as.integer(ceiling(layers / 2))
      c(rep(max_units, flat), descend(layers - flat))
    },
    diamond = {
      up <- as.integer(ceiling(layers / 2))
      c(mlp_ramp(start, max_units, up), descend(layers - up))
    },
    hexagon = {
      up <- max(1L, layers %/% 3L)
      down <- up
      if (up + down > layers) {
        up <- layers %/% 2L
        down <- layers - up
      }
      c(
        mlp_ramp(start, max_units, up),
        rep(max_units, layers - up - down),
        descend(down)
      )
    },
    stairs = {
      treads <- mlp_ramp(max_units, narrowest, as.integer(ceiling(layers / 2)))
      rep(treads, each = 2L)[seq_len(layers)]
    }
  )
  pmax(1L, units)
} # /rtemis::mlp_shape_units


# %% mlp_hidden_units ----
#' Resolve the hidden architecture
#'
#' The single construction path for the hidden widths, called once at the top of
#' training. Not in `setup_MLP()`: a derived width needs the *encoded* input
#' dimension, and setup never sees data. Everything downstream -- the module, the
#' model object, the run record -- consumes only the vector this returns.
#'
#' @param hidden_units Integer vector or NULL: Widths given directly.
#' @param shape Character or NULL: Profile to generate. NULL uses "funnel".
#' @param shape_layers Integer or NULL: Layers to generate. NULL uses 3.
#' @param shape_max_units Integer or NULL: Widest layer. NULL derives it from
#' `in_feat`.
#' @param in_feat Integer: Encoded input width, after embeddings or one-hot.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Integer vector: One width per hidden layer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_hidden_units <- function(
  hidden_units,
  shape,
  shape_layers,
  shape_max_units,
  in_feat,
  verbosity = 1L
) {
  if (!is.null(hidden_units)) {
    msg0(
      "Hidden layers, as given: ",
      paste(hidden_units, collapse = ", "),
      "...",
      verbosity = verbosity
    )
    return(as.integer(hidden_units))
  }
  shape <- shape %||% "funnel"
  layers <- as.integer(shape_layers %||% 3L)
  # Four times the input width, held between 64 and 512, and never below the
  # input width itself -- the floor the reference implementation also applies.
  # The multiplier and the bounds are a judgment call, not a result.
  max_units <- as.integer(
    shape_max_units %||%
      max(as.integer(in_feat), min(512L, max(64L, 4L * in_feat)))
  )
  units <- mlp_shape_units(shape, layers, max_units, in_feat)
  msg0(
    "Hidden layers, generated from shape '",
    shape,
    "' (",
    layers,
    " layers, at most ",
    max_units,
    " units): ",
    paste(units, collapse = ", "),
    "...",
    verbosity = verbosity
  )
  units
} # /rtemis::mlp_hidden_units


# %% mlp_embedding_dim ----
#' Size an embedding from its feature's cardinality
#'
#' fastai's rule, which is the one tabular practice converged on: wide enough
#' for a high-cardinality feature, small for a binary one.
#'
#' @param cardinality Integer vector: Categories per feature, including the
#' out-of-vocabulary slot.
#'
#' @return Integer vector of embedding widths.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_embedding_dim <- function(cardinality) {
  pmax(1L, as.integer(pmin(600, round(1.6 * cardinality^0.56))))
} # /rtemis::mlp_embedding_dim


# %% mlp_matrix ----
#' Take named columns of a design frame as a typed matrix
#'
#' `as.matrix()` of a zero-column selection loses the row count, which would
#' make an all-categorical or all-numeric dataset build a tensor of the wrong
#' shape, so the empty case is constructed explicitly.
#'
#' @param dat data.frame: Design frame.
#' @param columns Character vector: Columns to take, in order.
#' @param mode Character: Storage mode to coerce to.
#'
#' @return Matrix with `NROW(dat)` rows.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_matrix <- function(dat, columns, mode) {
  if (length(columns) == 0L) {
    return(matrix(vector(mode, 0L), nrow = NROW(dat), ncol = 0L))
  }
  out <- as.matrix(dat[, columns, drop = FALSE])
  storage.mode(out) <- mode
  out
} # /rtemis::mlp_matrix


# %% mlp_inputs ----
#' Build the module's input tensors from a design frame
#'
#' The categorical tensor is omitted rather than passed empty when there are no
#' categorical features, so the module's `forward` is called with one argument
#' and the dataloader carries one tensor fewer.
#'
#' Missing values are rejected here, naming the features that carry them,
#' because this is the only point on the predict path that sees the design
#' frame before torch does.
#'
#' @param dat data.frame: Design frame, already preprocessed.
#' @param numeric_features,categorical_features Character vectors: Column names,
#' in the order the fit used.
#'
#' @return List of `torch_tensor` objects.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_inputs <- function(dat, numeric_features, categorical_features) {
  dat <- as.data.frame(dat)
  missing_columns <- setdiff(
    c(numeric_features, categorical_features),
    names(dat)
  )
  if (length(missing_columns) > 0L) {
    rtemis.core::abort(
      "Data is missing ",
      length(missing_columns),
      " feature(s) the model was fit on: ",
      paste0("'", missing_columns, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  x_numeric <- mlp_matrix(dat, numeric_features, "double")
  x_categorical <- mlp_matrix(dat, categorical_features, "integer")
  # Both matrices, and before either becomes a tensor. A missing category code
  # is the worse of the two: `factor2integer` codes NA as NA by design, and an
  # NA index reaches `nn_embedding` as an out-of-range lookup that libtorch
  # reports with a sixty-frame C++ trace naming no column. Training rejects
  # missing values outright, so this is the predict-time path.
  incomplete <- c(
    numeric_features[colSums(is.na(x_numeric)) > 0L],
    categorical_features[colSums(is.na(x_categorical)) > 0L]
  )
  if (length(incomplete) > 0L) {
    rtemis.core::abort(
      "MLP cannot predict from missing values; ",
      length(incomplete),
      " feature(s) have them: ",
      paste0("'", incomplete, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  inputs <- list(torch::torch_tensor(x_numeric, dtype = torch::torch_float()))
  if (length(categorical_features) > 0L) {
    inputs <- c(
      inputs,
      list(torch::torch_tensor(x_categorical, dtype = torch::torch_long()))
    )
  }
  inputs
} # /rtemis::mlp_inputs


# %% mlp_module ----
#' Build the MLP module
#'
#' Embedding block (or a passthrough when the features are already numeric),
#' then one hidden layer per element of `hidden_units`, then the task head.
#'
#' The generator is created inside the function because `torch` is a
#' Suggests-gated backend: an `nn_module` at the top level of this file would
#' make the package unloadable without it. Everything the module needs arrives
#' through `initialize`, so nothing depends on what the enclosing frame happens
#' to hold.
#'
#' @param n_numeric Integer: Numeric features.
#' @param embedding_sizes Integer vector: Categories per categorical feature,
#' including the out-of-vocabulary slot.
#' @param embedding_dims Integer vector: Width of each embedding.
#' @param hidden_units Integer vector: One width per hidden layer.
#' @param out_features Integer: Output units: 1 for regression, one per class
#' for classification.
#' @param activation Character: One of `TORCH_ACTIVATIONS`.
#' @param norm Character or NULL: One of `TORCH_NORMS`, or NULL for none.
#' @param norm_first Logical: If TRUE, normalize before the activation.
#' @param bias Logical: If TRUE, give the linear layers a bias term.
#' @param residual Logical: If TRUE, add a shortcut around every hidden layer,
#' projected where the layer changes width.
#' @param dropout,input_dropout,embedding_dropout Numeric: Dropout
#' probabilities after each hidden layer, on the encoded input, and on the
#' concatenated embeddings.
#'
#' @return `nn_module` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_module <- function(
  n_numeric,
  embedding_sizes,
  embedding_dims,
  hidden_units,
  out_features,
  activation,
  norm,
  norm_first,
  bias,
  residual,
  dropout,
  input_dropout,
  embedding_dropout
) {
  generator <- torch::nn_module(
    classname = "MLPNet",
    initialize = function(
      n_numeric,
      embedding_sizes,
      embedding_dims,
      hidden_units,
      out_features,
      activation,
      norm,
      norm_first,
      bias,
      residual,
      dropout,
      input_dropout,
      embedding_dropout
    ) {
      self$n_numeric <- n_numeric
      self$n_categorical <- length(embedding_sizes)
      self$n_layers <- length(hidden_units)
      self$norm_first <- norm_first
      self$residual <- residual
      if (self[["n_categorical"]] > 0L) {
        self$embeddings <- torch::nn_module_list(lapply(
          seq_along(embedding_sizes),
          function(i) {
            torch::nn_embedding(embedding_sizes[[i]], embedding_dims[[i]])
          }
        ))
      }
      self$embedding_dropout <- torch::nn_dropout(embedding_dropout)
      self$input_dropout <- torch::nn_dropout(input_dropout)
      widths <- c(n_numeric + sum(embedding_dims), hidden_units)
      self$linears <- torch::nn_module_list(lapply(
        seq_len(self[["n_layers"]]),
        function(i) {
          torch::nn_linear(widths[[i]], widths[[i + 1L]], bias = bias)
        }
      ))
      self$norms <- torch::nn_module_list(lapply(
        hidden_units,
        function(width) torch_norm_module(norm, width)
      ))
      self$activations <- torch::nn_module_list(lapply(
        hidden_units,
        function(width) torch_activation_module(activation)
      ))
      self$dropouts <- torch::nn_module_list(lapply(
        hidden_units,
        function(width) torch::nn_dropout(dropout)
      ))
      # A residual connection needs its two ends to have the same width, so a
      # tapering shape gets a bias-free projection on every layer that changes
      # width. Without it, every shape but `constant` would be a run-time shape
      # error whenever `residual` is set.
      self$shortcuts <- torch::nn_module_list(lapply(
        seq_len(self[["n_layers"]]),
        function(i) {
          if (!residual || widths[[i]] == widths[[i + 1L]]) {
            torch::nn_identity()
          } else {
            torch::nn_linear(widths[[i]], widths[[i + 1L]], bias = FALSE)
          }
        }
      ))
      self$head <- torch::nn_linear(
        widths[[length(widths)]],
        out_features,
        bias = bias
      )
    },
    forward = function(x_num, x_cat = NULL) {
      parts <- list()
      if (self[["n_numeric"]] > 0L) {
        parts[[length(parts) + 1L]] <- x_num
      }
      if (self[["n_categorical"]] > 0L) {
        embedded <- lapply(
          seq_len(self[["n_categorical"]]),
          function(i) self[["embeddings"]][[i]](x_cat[, i])
        )
        parts[[length(parts) + 1L]] <- self[["embedding_dropout"]](
          torch::torch_cat(embedded, dim = 2L)
        )
      }
      h <- if (length(parts) == 1L) {
        parts[[1L]]
      } else {
        torch::torch_cat(parts, dim = 2L)
      }
      h <- self[["input_dropout"]](h)
      for (i in seq_len(self[["n_layers"]])) {
        shortcut <- self[["shortcuts"]][[i]](h)
        h <- self[["linears"]][[i]](h)
        h <- if (self[["norm_first"]]) {
          self[["activations"]][[i]](self[["norms"]][[i]](h))
        } else {
          self[["norms"]][[i]](self[["activations"]][[i]](h))
        }
        h <- self[["dropouts"]][[i]](h)
        if (self[["residual"]]) {
          h <- h + shortcut
        }
      }
      self[["head"]](h)
    }
  )
  generator(
    n_numeric = n_numeric,
    embedding_sizes = embedding_sizes,
    embedding_dims = embedding_dims,
    hidden_units = hidden_units,
    out_features = out_features,
    activation = activation,
    norm = norm,
    norm_first = norm_first,
    bias = bias,
    residual = residual,
    dropout = dropout,
    input_dropout = input_dropout,
    embedding_dropout = embedding_dropout
  )
} # /rtemis::mlp_module


# %% mlp_model_module ----
#' Rebuild a fitted model's module and load its parameters
#'
#' The architecture is read off the model rather than off the hyperparameters,
#' so a model loaded from disk on its own predicts without them.
#'
#' @param model `MLPModel` object.
#'
#' @return `nn_module` object in eval mode.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_model_module <- function(model) {
  torch_restore(
    mlp_module(
      n_numeric = length(model@numeric_features),
      embedding_sizes = model@embedding_sizes,
      embedding_dims = model@embedding_dims,
      hidden_units = model@hidden_units,
      out_features = model@out_features,
      activation = model@activation,
      norm = model@norm,
      norm_first = model@norm_first,
      bias = model@bias,
      residual = model@residual,
      dropout = model@dropout,
      input_dropout = model@input_dropout,
      embedding_dropout = model@embedding_dropout
    ),
    model@state
  )
} # /rtemis::mlp_model_module


# %% mlp_softmax ----
#' Row-wise softmax of a matrix of logits
#'
#' Shifted by each row's maximum before exponentiating, so a large logit cannot
#' overflow to `Inf` and yield `NaN` probabilities.
#'
#' @param x Matrix: Logits, one row per case.
#'
#' @return Matrix of probabilities whose rows sum to 1.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_softmax <- function(x) {
  shifted <- exp(x - apply(x, 1L, max))
  shifted / rowSums(shifted)
} # /rtemis::mlp_softmax


# %% mlp_preprocessor_config ----
#' The preprocessor an MLP fit needs
#'
#' All data transformation for this algorithm is expressed here, as
#' `setup_Preprocessor()` options, and nowhere else -- the transformer that
#' follows needs the same categorical encoding, and a one-off written in this
#' file would be written twice and drift.
#'
#' Centering and scaling is not a user switch: unlike the tree models, an
#' unscaled network fails quietly. Scale and center skip the integer category
#' codes, so the embedding indices survive the same call.
#'
#' @param embeddings Logical: If TRUE, integer-code the factors for embedding
#' lookup; if FALSE, one-hot encode them.
#'
#' @return `PreprocessorConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mlp_preprocessor_config <- function(embeddings) {
  if (embeddings) {
    # 1-based codes: R torch's `nn_embedding` indexes from 1, so the levels map
    # to 1..K and an unseen level to K + 1, which is exactly the table's size.
    setup_Preprocessor(
      factor2integer = TRUE,
      factor2integer_startat0 = FALSE,
      scale = TRUE,
      center = TRUE
    )
  } else {
    setup_Preprocessor(one_hot = TRUE, scale = TRUE, center = TRUE)
  }
} # /rtemis::mlp_preprocessor_config


# %% train_.MLPHyperparameters ----
#' Train a Multilayer Perceptron
#'
#' Train an MLP using `torch`, for regression, binary and multiclass
#' classification.
#'
#' MLP does not work in the presence of missing values.
#'
#' @param hyperparameters `MLPHyperparameters` object: make using [setup_MLP].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Validation set for early
#' stopping.
#' @param execution_config `ExecutionConfig` object: Not used for MLP.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Named list with `model` (`MLPModel`), `preprocessor` (the encoder,
#' re-applied at predict time) and `hyperparameters` (carrying the hidden widths
#' the fit resolved).
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, MLPHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("torch")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    allow_missing = FALSE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  y <- outcome(x)
  y_levels <- if (identical(type, "Classification")) levels(y) else NULL
  out_features <- if (identical(type, "Classification")) {
    nlevels(y)
  } else {
    1L
  }
  loss <- hyperparameters[["loss"]] %||%
    if (identical(type, "Classification")) "cross_entropy" else "mse"
  if (
    identical(type, "Classification") != (loss %in% TORCH_CLASSIFICATION_LOSSES)
  ) {
    rtemis.core::abort(
      "Loss '",
      loss,
      "' does not fit a ",
      type,
      " outcome. Use one of ",
      paste0(
        "'",
        if (identical(type, "Classification")) {
          TORCH_CLASSIFICATION_LOSSES
        } else {
          TORCH_REGRESSION_LOSSES
        },
        "'",
        collapse = ", "
      ),
      ", or leave `loss` NULL to set it from the outcome type.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Preprocess ----
  embeddings <- hyperparameters[["embeddings"]]
  prp <- preprocess(
    features(x),
    config = mlp_preprocessor_config(embeddings),
    verbosity = verbosity - 1L
  )
  dat <- as.data.frame(preprocessed(prp))
  categorical_features <- names(prp@values[["factor2integer_levels"]]) %||%
    character()
  numeric_features <- setdiff(names(dat), categorical_features)
  not_numeric <- numeric_features[
    !vapply(dat[numeric_features], is.numeric, logical(1L))
  ]
  if (length(not_numeric) > 0L) {
    rtemis.core::abort(
      "MLP needs numeric features; ",
      paste0("'", not_numeric, "'", collapse = ", "),
      " survived preprocessing as ",
      paste(
        unique(vapply(
          dat[not_numeric],
          function(v) class(v)[[1L]],
          character(1L)
        )),
        collapse = ", "
      ),
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  embedding_sizes <- as.integer(
    lengths(prp@values[["factor2integer_levels"]]) + 1L
  )
  embedding_dims <- if (length(embedding_sizes) == 0L) {
    integer()
  } else if (is.null(hyperparameters[["embedding_dim"]])) {
    mlp_embedding_dim(embedding_sizes)
  } else {
    rep(as.integer(hyperparameters[["embedding_dim"]]), length(embedding_sizes))
  }

  # Architecture ----
  hidden_units <- mlp_hidden_units(
    hidden_units = hyperparameters[["hidden_units"]],
    shape = hyperparameters[["shape"]],
    shape_layers = hyperparameters[["shape_layers"]],
    shape_max_units = hyperparameters[["shape_max_units"]],
    in_feat = length(numeric_features) + sum(embedding_dims),
    verbosity = verbosity
  )

  # Train ----
  device <- resolve_torch_device(
    hyperparameters[["device"]],
    verbosity = verbosity
  )
  check_mps_reproducible(
    device,
    seed = hyperparameters[["seed"]],
    dropout = c(
      hyperparameters[["dropout"]],
      hyperparameters[["input_dropout"]],
      hyperparameters[["embedding_dropout"]]
    )
  )
  if (!is.null(hyperparameters[["seed"]])) {
    torch::torch_manual_seed(hyperparameters[["seed"]])
  }
  module <- mlp_module(
    n_numeric = length(numeric_features),
    embedding_sizes = embedding_sizes,
    embedding_dims = embedding_dims,
    hidden_units = hidden_units,
    out_features = out_features,
    activation = hyperparameters[["activation"]],
    norm = hyperparameters[["norm"]],
    norm_first = hyperparameters[["norm_first"]],
    bias = hyperparameters[["bias"]],
    residual = hyperparameters[["residual"]],
    dropout = hyperparameters[["dropout"]],
    input_dropout = hyperparameters[["input_dropout"]],
    embedding_dropout = hyperparameters[["embedding_dropout"]]
  )
  # The L1 penalty is on the linear weights: every 2-D parameter outside the
  # embedding tables, which are a lookup rather than a projection.
  parameters <- module[["parameters"]]
  l1_parameters <- names(parameters)[
    vapply(parameters, function(p) p[["dim"]]() == 2L, logical(1L)) &
      !startsWith(names(parameters), "embeddings")
  ]
  batch_size <- hyperparameters[["batch_size"]]
  drop_last <- hyperparameters[["drop_last"]]
  # Batch normalization needs more than one case per batch to compute a
  # variance, so a trailing batch of one aborts the fit partway through. The
  # only fix at that point is to drop it.
  if (
    identical(hyperparameters[["norm"]], "batch_norm") &&
      !drop_last &&
      NROW(dat) %% batch_size == 1L
  ) {
    drop_last <- TRUE
    msg(
      "Dropping the last batch: batch normalization cannot use a batch of one case.",
      verbosity = verbosity
    )
  }
  weights <- if (is.null(weights)) rep(1, NROW(dat)) else weights
  fitted <- torch_fit(
    module = module,
    inputs = mlp_inputs(dat, numeric_features, categorical_features),
    target = torch_target(y, type),
    weights = torch::torch_tensor(
      matrix(as.numeric(weights), ncol = 1L),
      dtype = torch::torch_float()
    ),
    inputs_validation = if (!is.null(dat_validation)) {
      mlp_inputs(
        apply_preprocessor(
          prp,
          features(dat_validation),
          verbosity = verbosity - 1L
        ),
        numeric_features,
        categorical_features
      )
    },
    target_validation = if (!is.null(dat_validation)) {
      torch_target(outcome(dat_validation), type)
    },
    weights_validation = if (!is.null(dat_validation)) {
      # Weighted the same way the training loss is. Early stopping selects on
      # this number, so leaving it unweighted under `ifw = TRUE` would pick the
      # epoch that is best for the majority class while training optimized a
      # balanced objective -- which is the thing `ifw` was set to avoid.
      torch::torch_tensor(
        matrix(
          if (identical(type, "Classification") && hyperparameters[["ifw"]]) {
            ifw(
              outcome(dat_validation),
              type = "case_weights",
              verbosity = verbosity - 1L
            )
          } else {
            rep(1, NROW(dat_validation))
          },
          ncol = 1L
        ),
        dtype = torch::torch_float()
      )
    },
    loss = loss,
    optimizer = hyperparameters[["optimizer"]],
    lr = hyperparameters[["lr"]],
    weight_decay = hyperparameters[["weight_decay"]],
    betas = if (
      is.null(hyperparameters[["beta1"]]) && is.null(hyperparameters[["beta2"]])
    ) {
      NULL
    } else {
      c(
        hyperparameters[["beta1"]] %||% 0.9,
        hyperparameters[["beta2"]] %||% 0.999
      )
    },
    eps = hyperparameters[["eps"]],
    momentum = hyperparameters[["momentum"]],
    lr_scheduler = hyperparameters[["lr_scheduler"]],
    batch_size = batch_size,
    max_epochs = hyperparameters[["max_epochs"]],
    patience = hyperparameters[["patience"]],
    max_grad_norm = hyperparameters[["max_grad_norm"]],
    l1_penalty = hyperparameters[["l1_penalty"]],
    l1_parameters = l1_parameters,
    drop_last = drop_last,
    num_workers = hyperparameters[["num_workers"]],
    device = device,
    verbosity = verbosity
  )
  model <- MLPModel(
    state = torch_state(fitted[["module"]]),
    hidden_units = hidden_units,
    activation = hyperparameters[["activation"]],
    norm = hyperparameters[["norm"]],
    norm_first = hyperparameters[["norm_first"]],
    bias = hyperparameters[["bias"]],
    residual = hyperparameters[["residual"]],
    dropout = hyperparameters[["dropout"]],
    input_dropout = hyperparameters[["input_dropout"]],
    embedding_dropout = hyperparameters[["embedding_dropout"]],
    numeric_features = numeric_features,
    categorical_features = categorical_features,
    embedding_sizes = embedding_sizes,
    embedding_dims = embedding_dims,
    out_features = out_features,
    type = type,
    y_levels = y_levels,
    device = device,
    epochs_trained = as.integer(fitted[["epochs_trained"]]),
    best_epoch = as.integer(fitted[["best_epoch"]]),
    history = fitted[["history"]]
  )
  check_inherits(model, MLPModel)
  # The widths are resolved here, from the encoded input width, so `train()`
  # would otherwise report NULL for an architecture the fit demonstrably used.
  # They go into `hidden_units` itself: the run record compares the input config
  # against this one field by field, so a NULL that became a vector reads as
  # `origin: "derived"` with no second property to carry it. The `shape_*`
  # settings stay beside it and say where the widths came from.
  hyperparameters@hidden_units <- hidden_units
  list(model = model, preprocessor = prp, hyperparameters = hyperparameters)
} # /rtemis::train_.MLPHyperparameters


# %% predict_super.MLPModel ----
#' Predict from an MLP model
#'
#' Rebuilds the module from the model's recorded architecture and loads its
#' stored parameters: a `torch` module cannot be saved, so the model carries
#' the parameters serialized rather than the live object.
#'
#' @param model `MLPModel` object.
#' @param newdata tabular data: Data to predict on, already through the
#' algorithm's own preprocessor.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @keywords internal
#' @noRd
method(predict_super, MLPModel) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_dependencies("torch")
  output <- torch_forward(
    mlp_model_module(model),
    mlp_inputs(newdata, model@numeric_features, model@categorical_features),
    device = model@device
  )
  if (identical(model@type, "Regression")) {
    return(output[, 1L])
  }
  # The head emits one logit per class, so the probabilities come from a
  # softmax. rtemis expects the second level's probability in the binary case
  # and the full matrix otherwise.
  predicted_prob <- mlp_softmax(output)
  colnames(predicted_prob) <- model@y_levels
  if (NCOL(predicted_prob) == 2L) {
    return(predicted_prob[, 2L])
  }
  predicted_prob
} # /rtemis::predict_super.MLPModel


# %% varimp_super.MLPModel ----
#' Get variable importance from an MLP model
#'
#' A torch MLP has no native measure of variable importance. Permutation
#' importance would be the real answer and belongs across algorithms rather than
#' in one of them.
#'
#' @param model `MLPModel` object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, MLPModel) <- function(model) {
  NULL
} # /rtemis::varimp_super.MLPModel


# %% training_device.MLPHyperparameters ----
#' The device an MLP fit will run on
#'
#' Resolved twice: once here so `train()` can name it in the line it prints
#' before training starts, and once in `train_()` for real. Resolution is
#' deterministic and free of side effects, so the two agree.
#'
#' NULL when `torch` is absent -- `train_()` is about to abort on the missing
#' dependency, and a message has no business raising a different error first.
#'
#' @param x `MLPHyperparameters` object.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(training_device, MLPHyperparameters) <- function(x) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    return(NULL)
  }
  resolve_torch_device(x[["device"]], verbosity = 0L)
} # /rtemis::training_device.MLPHyperparameters
