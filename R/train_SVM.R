# train_SVM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a Linear SVM model
#'
#' Train a Linear SVM model using `e1071::svm`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param hyperparameters `LinearSVMHyperparameters` object: make using [setup_LinearSVM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data or NULL: Not used for Linear SVM.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_super, LinearSVMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("e1071")

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
} # /rtemis::train_super.LinearSVMHyperparameters


#' Train a Radial SVM model
#'
#' Train a Radial SVM model using `e1071::svm`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param hyperparameters `RadialSVMHyperparameters` object: make using [setup_RadialSVM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data or NULL: Not used for Radial SVM.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_super, RadialSVMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("e1071")

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
} # /rtemis::train_super.RadialSVMHyperparameters

#' Predict from SVM model
#'
#' @param model SVM model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#'
#' @keywords internal
#' @noRd
method(predict_super, class_svm) <- function(
  model,
  newdata,
  type = NULL
) {
  newdata <- preprocess(
    newdata,
    config = setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
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
} # /rtemis::predict_super.svm


#' Get coefficients from SVM model
#'
#' @param model SVM model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_svm) <- function(model) {
  # Only for linear kernel with binary classification
  if (model[["kernel"]] == 0L && model[["nclasses"]] == 2) {
    coef(model)
  } else {
    NULL
  }
} # /rtemis::varimp_super.svm
