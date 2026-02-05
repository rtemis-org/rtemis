# train_TabNet.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a TabNet model
#'
#' Train a TabNet model using `TabNet`.
#'
#' TabNet does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `TabNetHyperparameters` object: make using [setup_TabNet].
#' @param tuner_config `TunerConfig` object created using [setup_GridSearch].
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `TabNet`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_TabNet <- function(
  x,
  weights = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("torch", "tabnet")

  # Checks ----
  check_is_S7(hyperparameters, TabNetHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  # n_classes <- if (type == "Classification") {
  #   nlevels(outcome(x))
  # } else {
  #   NA
  # }

  # Scale data ----
  y <- outcome(x)
  prp <- preprocess(
    features(x),
    config = setup_Preprocessor(scale = TRUE, center = TRUE)
  )
  x <- prp@preprocessed

  # ?Any params that may be NULL by setup_ but aren't allowed to be NULL by training f

  # Train ----
  # The predictor data should be standardized (e.g. centered or scaled). The model treats
  # categorical predictors internally thus, you don't need to make any treatment.
  config <- get_tabnet_config(hyperparameters)
  config[["verbose"]] <- verbosity > 0L
  model <- tabnet::tabnet_fit(
    x = x,
    y = y,
    config = config,
    weights = weights
  )
  check_inherits(model, "tabnet_fit")
  model
} # /rtemis::train_TabNet

#' Predict from TabNet model
#'
#' @param model TabNet model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_TabNet <- function(model, newdata, type) {
  if (type == "Regression") {
    predict(model, new_data = newdata)[[1]]
  } else if (type == "Classification") {
    predicted <- predict(model, new_data = newdata, type = "prob")
    if (NCOL(predicted) == 2) {
      predicted[[2]]
    } else {
      predicted
    }
  }
} # /rtemis::predict_TabNet

#' Get coefficients from TabNet model
#'
#' @param model TabNet model.
#'
#' @keywords internal
#' @noRd
varimp_TabNet <- function(model) {
  NULL
} # /rtemis::varimp_TabNet
