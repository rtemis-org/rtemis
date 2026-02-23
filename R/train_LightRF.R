# train_LightRF.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

# %% train_super.LightRFHyperparameters ----
#' Random Forest using LightGBM
#'
#' @param hyperparameters `LightRFHyperparameters` object: make using [setup_LightRF].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data or NULL: Validation set for early stopping.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_super, LightRFHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightRFHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  if (type == "Classification") {
    nclasses <- nlevels(outcome(x))
  } else {
    nclasses <- 1L
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@hyperparameters[["objective"]] <- if (
      type == "Regression"
    ) {
      "regression"
    } else {
      if (nclasses == 2L) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }

  ## Preprocess ----
  factor_index <- names(x)[which(sapply(x, is.factor))]
  if (length(factor_index) > 0L) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      dat_validation = dat_validation,
      verbosity = verbosity
    )
    if (is.null(dat_validation)) {
      x <- prp@preprocessed
    } else {
      x <- prp@preprocessed[["training"]]
      dat_validation <- prp@preprocessed[["validation"]]
    }
  } else {
    factor_index <- prp <- NULL
  }
  if (type == "Classification") {
    # remove outcomes from factor_index
    # will be character(0) if only outcome was factor, but that works
    factor_index <- factor_index[seq_len(length(factor_index) - 1)]
  }

  x <- lightgbm::lgb.Dataset(
    data = as.matrix(features(x)),
    categorical_feature = factor_index,
    label = outcome(x),
    weight = weights
  )

  if (!is.null(dat_validation)) {
    dat_validation <- lightgbm::lgb.Dataset(
      data = as.matrix(features(dat_validation)),
      categorical_feature = factor_index,
      label = outcome(dat_validation)
    )
  }

  # Train ----
  params <- hyperparameters@hyperparameters
  # Remove params that are not used by LightGBM
  params[["ifw"]] <- NULL
  params[["early_stopping_rounds"]] <- NULL
  # num_class is required for multiclass classification only, must be 1 or unset for regression & binary classification
  if (nclasses > 2L) {
    params[["num_class"]] <- nclasses
  }
  # Set n threads
  params[["num_threads"]] <- prop(hyperparameters, "n_workers")

  model <- lightgbm::lgb.train(
    params = params,
    data = x,
    nrounds = hyperparameters[["nrounds"]],
    valids = if (!is.null(dat_validation)) {
      list(training = x, validation = dat_validation)
    } else {
      list(training = x)
    },
    early_stopping_rounds = hyperparameters[["early_stopping_rounds"]],
    verbose = verbosity - 2L
  )
  check_inherits(model, "lgb.Booster")
  list(model = model, preprocessor = prp)
} # /rtemis::train_super.LightRFHyperparameters
