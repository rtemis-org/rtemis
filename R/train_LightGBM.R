# train_LightGBM.R
# ::rtemis::
# 2025 EDG rtemis.org

# LightGBM parameters
# https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Gradient Boosting with LightGBM
#'
#' @param x data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GLMNETHyperparameters` object: make using [setup_GLMNET].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_LightGBM <- function(
  x,
  dat_validation = NULL,
  weights = NULL,
  hyperparameters = setup_LightGBM(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightGBMHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Convert "null" nrounds to max_nrounds
  if (hyperparameters[["nrounds"]] == "null") {
    hyperparameters@hyperparameters[["nrounds"]] <- hyperparameters[[
      "max_nrounds"
    ]]
  }

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  ## Objective ----
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
  if (length(factor_index) > 0) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      dat_validation = dat_validation,
      verbosity = verbosity - 1L
    )
    if (is.null(dat_validation)) {
      x <- prp@preprocessed
    } else {
      x <- prp@preprocessed[["training"]]
      dat_validation <- prp@preprocessed[["validation"]]
    }
  } else {
    factor_index <- NULL
  }
  if (type == "Classification") {
    # remove outcomes from factor_index
    # will be character(0) if only outcome was factor, but that works
    factor_index <- factor_index[seq_len(length(factor_index) - 1)]
  }

  x <- lightgbm::lgb.Dataset(
    data = as.matrix(exc(x, ncol(x))),
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
  params[["max_nrounds"]] <- NULL
  params[["force_nrounds"]] <- NULL
  params[["ifw"]] <- NULL
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
    verbose = verbosity - 1L
  )
  check_inherits(model, "lgb.Booster")
  model
} # /rtemis::train_LightGBM

#' Predict from LightGBM LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightGBM`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_LightGBM <- function(model, newdata, type, verbosity = 0L) {
  check_inherits(model, "lgb.Booster")
  check_inherits(newdata, "data.frame")

  # Preprocess ----
  newdata <- as.matrix(
    preprocess(
      newdata,
      config = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      verbosity = 0L
    )@preprocessed
  )

  # Predict ----
  predict(model, newdata = newdata)
} # /rtemis::predict_LightGBM

#' Get variable importance from LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightGBM`.
#'
#' @keywords internal
#' @noRd
varimp_LightGBM <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightGBM
