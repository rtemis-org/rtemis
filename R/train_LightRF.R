# train_LightRF.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Random Forest using LightGBM
#'
#' @inheritParams train_GLMNET
#'
#' @return `lgb.Booster` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_LightRF <- function(
  x,
  dat_validation = NULL,
  weights = NULL,
  hyperparameters = setup_LightRF(),
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
  model
} # /rtemis::train_LightRF

#' Predict from LightRF LightGBM model
#'
#' @param model lgb.Booster object trained using `train_LightRF`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_LightRF <- function(model, newdata, type, verbosity = 0L) {
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
} # /rtemis::predict_LightRF

#' Get variable importance from LightRF model
#'
#' @param model `lgb.Booster`` object trained using `train_LightRF`.
#'
#' @keywords internal
#' @noRd
varimp_LightRF <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_LightRF
