# train_SVM.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_.LinearSVMHyperparameters ----
#' Train a Linear SVM model
#'
#' Train a Linear SVM model using `e1071::svm`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param hyperparameters `LinearSVMHyperparameters` object: make using [setup_LinearSVM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used for Linear SVM.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LinearSVMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_FutureExecution(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("e1071")

  # Checks ----
  if (!is.null(weights)) {
    rtemis.core::abort(
      "Case weights are not supported by e1071::svm. You can enable `ifw` in the hyperparameters to use inverse frequency weighting instead.",
      class = "rtemis_unsupported_error"
    )
  }

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
    allow_missing = FALSE,
    verbosity = verbosity
  )

  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    nlevels(outcome(x))
  } else {
    NA
  }

  # Preprocess ----
  # One-hot encode
  y <- outcome(x)
  x <- features(x)
  factor_index <- names(x)[which(sapply(x, is.factor))]
  if (length(factor_index) > 0L) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(one_hot = TRUE),
      verbosity = verbosity
    )
    x <- preprocessed(prp)
  } else {
    prp <- NULL
  }

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
  list(model = model, preprocessor = prp)
} # /rtemis::train_.LinearSVMHyperparameters


# %% train_.RadialSVMHyperparameters ----
#' Train a Radial SVM model
#'
#' Train a Radial SVM model using `e1071::svm`.
#'
#' SVM does not work in the presence of missing values.
#'
#' @param hyperparameters `RadialSVMHyperparameters` object: make using [setup_RadialSVM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used for Radial SVM.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `svm`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, RadialSVMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_FutureExecution(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("e1071")

  # Checks ----
  if (!is.null(weights)) {
    rtemis.core::abort(
      "Case weights are not supported by e1071::svm. You can enable `ifw` in the hyperparameters to use inverse frequency weighting instead.",
      class = "rtemis_unsupported_error"
    )
  }

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
    allow_missing = FALSE,
    verbosity = verbosity
  )

  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    nlevels(outcome(x))
  } else {
    NA
  }

  # Preprocess ----
  # One-hot encode
  y <- outcome(x)
  x <- features(x)
  factor_index <- names(x)[which(sapply(x, is.factor))]
  if (length(factor_index) > 0L) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(one_hot = TRUE),
      verbosity = verbosity
    )
    x <- preprocessed(prp)
  } else {
    prp <- NULL
  }

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
  list(model = model, preprocessor = prp)
} # /rtemis::train_.RadialSVMHyperparameters


# %% predict_super.svm ----
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
  type = NULL,
  execution_config = NULL,
  verbosity = 0L
) {
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


# %% svm_margin ----
#' Decision values of a binary SVM, oriented to the positive class
#'
#' `e1071` names the decision-value column `"A/B"`, where a positive value
#' favors `A` -- and which class that is follows from the order the classes were
#' encountered in the training data, **not** from the level order. So it is read
#' from the name rather than assumed: a fitted model whose column reads
#' `"neg/pos"` has the opposite sign to one reading `"pos/neg"`, and taking
#' either as given would invert every contribution for roughly half of all
#' fitted models, silently.
#'
#' @param model `svm` object.
#' @param newdata tabular data.
#'
#' @return Numeric matrix, one column, positive toward the second level.
#'
#' @keywords internal
#' @noRd
svm_margin <- function(model, newdata) {
  predicted <- predict(model, newdata = newdata, decision.values = TRUE)
  values <- attr(predicted, "decision.values")
  favored <- strsplit(colnames(values)[[1L]], "/", fixed = TRUE)[[1L]][[1L]]
  # rtemis reports the second level as the positive class, which is what
  # `predict_super.class_svm` returns for a binary outcome.
  sign <- if (identical(favored, model[["levels"]][[2L]])) 1 else -1
  matrix(sign * as.numeric(values), ncol = 1L)
} # /rtemis::svm_margin


# %% explain_super.class_svm ----
#' LinearSHAP contributions from a linear-kernel SVM
#'
#' The decision function of a linear-kernel SVM is affine in the features, so
#' `phi_j = beta_j * (x_j - E[x_j])` is exact. The map is recovered by probing
#' rather than from `coef()`, which reports it in the space `svm()` scaled
#' internally.
#'
#' Only a binary or regression fit has one such function. Multiclass `e1071` is
#' one-vs-one voting, which is not a linear decision function per class, and a
#' non-linear kernel has none at all -- both are refused.
#'
#' This method is never reached for a RadialSVM, even though it shares the
#' backend class: the kernel estimator is model-agnostic and is handled in
#' `explain()` before dispatch, so only an exact estimator arrives here.
#'
#' @param model `svm` object.
#' @param newdata tabular data: Cases to explain.
#' @param background tabular data: Reference cases.
#' @param estimator Character: Resolved estimator.
#' @param perturbation Character: Resolved value function.
#' @param scale Character: Scale the contributions are additive on.
#' @param type Character: "Regression" or "Classification".
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @keywords internal
#' @noRd
method(explain_super, class_svm) <- function(
  model,
  newdata,
  background,
  estimator,
  perturbation,
  scale,
  type,
  verbosity = 0L
) {
  check_shap_linear(estimator, perturbation, "LinearSVM")
  # `kernel` is 0 for linear; anything else has no linear decision function.
  if (!identical(as.integer(model[["kernel"]]), 0L)) {
    rtemis.core::abort(
      "LinearSHAP applies only to a linear-kernel SVM.\n",
      "Use `setup_SHAP(estimator = \"kernel\")`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  check_shap_binary(type, model[["nclasses"]], "LinearSVM")
  shap_require_background(background, "LinearSHAP")
  probed_linear_shap(
    design = newdata,
    background = background,
    margin_fn = function(x) {
      if (identical(type, "Classification")) {
        svm_margin(model, x)
      } else {
        matrix(predict(model, newdata = x), ncol = 1L)
      }
    },
    label = "LinearSHAP"
  )
} # /rtemis::explain_super.class_svm


# %% varimp_super.class_svm ----
#' Get coefficients from SVM model
#'
#' @param model SVM model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_svm) <- function(model) {
  # Only a linear kernel has one weight per predictor, and only a regression or
  # binary classification has a single decision function.
  if (model[["kernel"]] != 0L || model[["nclasses"]] > 2L) {
    return(NULL)
  }
  .coefs <- coef(model)
  weights <- .coefs[names(.coefs) != "(Intercept)"]
  regression <- model[["type"]] %in% c(3L, 4L)
  scaled <- rep_len(as.logical(model[["scaled"]]), length(weights))
  per <- if (all(scaled)) {
    "per standard deviation of the predictor"
  } else if (!any(scaled)) {
    "per unit of the predictor"
  } else {
    paste0(
      "per standard deviation of the predictors e1071 scaled and per unit of ",
      "the others (",
      paste(names(weights)[!scaled], collapse = ", "),
      ")"
    )
  }
  if (regression) {
    outcome <- if (!is.null(model[["y.scale"]])) {
      "in standard deviations of the outcome"
    } else {
      "in outcome units"
    }
    description <- paste0(
      "Slope of each predictor in the linear regression function (e1071), ",
      outcome,
      " ",
      per,
      "; the intercept is not included."
    )
  } else {
    # The decision function is positive toward the first class in e1071's
    # label order, which follows the training data; it is oriented here to
    # the second outcome level, the positive class.
    favored <- model[["levels"]][[model[["labels"]][[1L]]]]
    if (!identical(favored, model[["levels"]][[2L]])) {
      weights <- -weights
    }
    description <- paste0(
      "Weight of each predictor in the linear decision function (e1071), ",
      per,
      ", with positive values toward class ",
      model[["levels"]][[2L]],
      "; the intercept is not included."
    )
  }
  VariableImportance(
    measures = list(
      Coefficient = importance_measure(
        names(weights),
        unname(weights),
        kind = "coefficient",
        signed = TRUE,
        scale_dependent = !all(scaled),
        direction = "absolute",
        description = description
      )
    )
  )
} # /rtemis::varimp_super.svm
