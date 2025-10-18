# train_SVM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a SVM model
#'
#' Train a SVM model using `SVM`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param x data.frame or similar: Training set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `SVMHyperparameters` object: make using [setup_RadialSVM].
#' @param tuner_config `TunerConfig` object: make using [setup_GridSearch].
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_SVM <- function(
  x,
  weights = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("e1071")

  # Checks ----
  if (
    !(S7_inherits(hyperparameters, LinearSVMHyperparameters) ||
      S7_inherits(hyperparameters, RadialSVMHyperparameters))
  ) {
    cli::cli_abort(
      "hyperparameters must be LinearSVMHyperparameters or RadialSVMHyperparameters"
    )
  }

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
  n_classes <- if (type == "Classification") {
    nlevels(outcome(x))
  } else {
    NA
  }

  # One-hot encode ----
  y <- outcome(x)
  x <- preprocess(
    features(x),
    config = setup_Preprocessor(one_hot = TRUE),
    verbosity = verbosity
  )@preprocessed

  # Can use class_weights or set class.weights = "inverse" in svm()
  # if (is.null(weights)) {
  #   weights <- rep(1, NROW(x))
  # }

  # Train ----
  class_weights <-
    if (
      type == "Classification" && n_classes == 2 && hyperparameters[["ifw"]]
    ) {
      "inverse"
    } else {
      NULL
    }
  # gamma can't be NULL even if not used
  gamma <- hyperparameters[["gamma"]]
  if (is.null(gamma)) {
    gamma <- 1
  }
  model <- e1071::svm(
    x = x,
    y = y, # factor or numeric
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    gamma = gamma,
    class.weights = class_weights,
    probability = TRUE
  )
  check_inherits(model, "svm")
  model
} # /rtemis::train_SVM

train_LinearSVM <- train_RadialSVM <- train_SVM

#' Predict from SVM model
#'
#' @param model SVM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_SVM <- function(model, newdata, type, verbosity = 0L) {
  newdata <- preprocess(
    newdata,
    config = setup_Preprocessor(one_hot = TRUE),
    verbosity = verbosity - 1L
  )@preprocessed
  if (type == "Classification") {
    predicted_prob <- attr(
      predict(model, newdata = newdata, probability = TRUE),
      "probabilities"
    )
    if (length(model$levels) == 2) {
      predicted_prob[, 2]
    } else {
      predicted_prob
    }
  } else {
    predict(model, newdata = newdata)
  }
} # /rtemis::predict_SVM

predict_LinearSVM <- predict_RadialSVM <- predict_SVM

#' Get coefficients from SVM model
#'
#' @param model SVM model.
#'
#' @keywords internal
#' @noRd
varimp_RadialSVM <- function(model) {
  NULL
} # /rtemis::varimp_RadialSVM

varimp_LinearSVM <- function(model) {
  # Only for binary classification
  if (model[["nclasses"]] == 2) {
    coef(model)
  } else {
    NULL
  }
} # /rtemis::varimp_LinearSVM
