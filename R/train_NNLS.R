# train_NNLS.R
# ::rtemis::
# 2026- EDG rtemis.org

# Backend parameter documentation:
# https://cran.r-project.org/package=nnls

# %% train_.NNLSHyperparameters ----
#' Train a non-negative least squares model
#'
#' `nnls::nnls()` takes a design matrix and a response and nothing else: no
#' intercept, no weights, and it discards the design matrix's column names. The
#' fitted object is therefore an rtemis `NNLS`, which carries the named
#' coefficients, whether they were normalized, and what the outcome was.
#'
#' Case weights are applied the way the SuperLearner literature's `method.NNLS`
#' does, by scaling both sides of the system by `sqrt(w)`: minimizing
#' `||sqrt(w) * (y - Xb)||^2` is weighted least squares, and the sign constraint
#' is unaffected.
#'
#' @param hyperparameters `NNLSHyperparameters` object: make using [setup_NNLS].
#' @param x tabular data: Training set. All predictors must be numeric.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Not used.
#' @param execution_config `ExecutionConfig` object: Not used.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `NNLS` model and a NULL preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, NNLSHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("nnls")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(x = x, allow_missing = FALSE, verbosity = verbosity)
  type <- supervised_type(x)
  feat <- as.data.frame(features(x))
  non_numeric <- names(feat)[!vapply(feat, is.numeric, logical(1L))]
  if (length(non_numeric) > 0L) {
    rtemis.core::abort(
      "NNLS requires numeric predictors; these are not: ",
      paste(non_numeric, collapse = ", "),
      ".",
      class = c("rtemis_type_error", "rtemis_data_error")
    )
  }

  y_levels <- NULL
  if (type == "Classification") {
    if (nlevels(outcome(x)) > 2L) {
      rtemis.core::abort(
        "NNLS does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    y_levels <- levels(outcome(x))
    # 0/1 on the second level, matching rtemis' positive-class convention.
    y <- as.numeric(outcome(x)) - 1
  } else {
    y <- as.numeric(outcome(x))
  }

  # Train ----
  design <- as.matrix(feat)
  if (!is.null(weights)) {
    root_w <- sqrt(weights)
    design <- design * root_w
    y <- y * root_w
  }
  fit <- nnls::nnls(design, y)
  check_inherits(fit, "nnls")

  coefficients <- stats::coef(fit)
  # `nnls` returns NA for a column it could not resolve (a zero or duplicated
  # predictor); such a column contributes nothing, which is what 0 says.
  coefficients[is.na(coefficients)] <- 0
  if (hyperparameters[["normalize"]]) {
    total <- sum(coefficients)
    # An all-zero solution has nothing to normalize, and dividing would give NaN.
    if (total > 0) {
      coefficients <- coefficients / total
    }
  }
  names(coefficients) <- names(feat)

  model <- NNLS(
    coefficients = coefficients,
    xnames = names(feat),
    normalize = hyperparameters[["normalize"]],
    y_levels = y_levels,
    type = type
  )
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.NNLSHyperparameters


# %% predict_super.NNLS ----
#' Predict from an NNLS model
#'
#' @param model `NNLS` object trained using `train_NNLS`.
#' @param newdata tabular data: Data to predict on.
#' @param type Not used; the model carries its own.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Numeric vector: fitted values, or probabilities of the second outcome
#' level for classification.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, NNLS) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(newdata, "data.frame")
  design <- as.matrix(as.data.frame(newdata)[, model@xnames, drop = FALSE])
  predicted <- as.numeric(design %*% model@coefficients)
  if (model@type == "Classification") {
    # A convex combination of probabilities is itself a probability, but an
    # unnormalized fit carries no such guarantee.
    predicted <- pmin(pmax(predicted, 0), 1)
  }
  predicted
} # /rtemis::predict_super.NNLS


# %% varimp_super.NNLS ----
#' Get coefficients from an NNLS model
#'
#' The coefficients are the model, and they are non-negative and (when
#' `normalize` is TRUE) sum to 1, so they read directly as each predictor's
#' share of the fit.
#'
#' @param model `NNLS` object trained using `train_NNLS`.
#'
#' @return `VariableImportance` object with one measure, `coefficient`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(varimp_super, NNLS) <- function(model) {
  VariableImportance(
    data.table(
      variable = model@xnames,
      coefficient = unname(model@coefficients)
    )
  )
} # /rtemis::varimp_super.NNLS


# %% explain_super.NNLS ----
#' LinearSHAP contributions from an NNLS model
#'
#' The coefficients are the model, so `phi_j = beta_j * (x_j - E[x_j])` is exact
#' with nothing to extract. There is no intercept, which makes the baseline the
#' fit at the background's mean.
#'
#' For classification this is a linear probability model: its own scale *is* the
#' probability scale, unlike every other classifier here, and `predict_super()`
#' clamps the result to \[0, 1\]. The contributions decompose the unclamped fit,
#' as the logit does for a GLM -- the clamp is a transform applied after the
#' additive part, and is where a clamped case's contributions will not sum to
#' the reported probability.
#'
#' @param model `NNLS` object.
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
method(explain_super, NNLS) <- function(
  model,
  newdata,
  background,
  estimator,
  perturbation,
  scale,
  type,
  verbosity = 0L
) {
  check_shap_linear(estimator, perturbation, "NNLS")
  shap_require_background(background, "LinearSHAP")
  design <- function(x) {
    as.matrix(as.data.frame(x)[, model@xnames, drop = FALSE])
  }
  linear_shap(
    design = design(newdata),
    background = design(background),
    coefficients = matrix(model@coefficients, ncol = 1L),
    intercept = 0,
    # Checked against the model's own predictions only where they are the
    # unclamped fit; for classification the clamp legitimately makes them
    # differ, and there is no second source to check against.
    margin = if (identical(type, "Regression")) {
      matrix(
        predict_super(model = model, newdata = newdata, type = type),
        ncol = 1L
      )
    } else {
      NULL
    },
    label = "LinearSHAP"
  )
} # /rtemis::explain_super.NNLS
