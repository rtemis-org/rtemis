# train_LightGBM.R
# ::rtemis::
# 2025 EDG rtemis.org

# LightGBM parameters
# https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Gradient Boosting with LightGBM
#'
#' @param hyperparameters `LightGBMHyperparameters` object: make using [setup_LightGBM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data or NULL: Validation set for early stopping.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd

method(train_super, LightGBMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
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

  ## Preprocess for LightGBM ----
  # Preprocess features and outcome.
  # Outcome will be removed from factor_index if it is a factor
  factor_index <- names(x)[which(sapply(x, is.factor))]
  if (length(factor_index) > 0) {
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
  list(model = model, preprocessor = prp)
} # /rtemis::train_super.LightGBMHyperparameters


#' Predict from LightGBM model
#'
#' @param model lgb.Booster object.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_lgb.Booster) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(model, "lgb.Booster")
  check_inherits(newdata, "data.frame")

  # Algorithm-specific preprocessing (factor2integer) is applied by
  # predict.Supervised before calling this method. See R/train.R:420-504
  # and R/07_S7_Supervised.R:127-135

  # Predict ----
  predict(model, newdata = as.matrix(newdata))
} # /rtemis::predict_super.lgb.Booster


#' Get variable importance from LightGBM model
#'
#' @param model lgb.Booster object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_lgb.Booster) <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE)
  out <- data.frame(t(vi[["Gain"]]))
  names(out) <- vi[["Feature"]]
  out
} # /rtemis::varimp_super.lgb.Booster
