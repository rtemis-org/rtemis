# train_GLM.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a GLM model
#'
#' Train a GLM model using `stats::glm`.
#'
#' @details
#' `stats::glm` does not work in the presence of missing values.
#' This function uses the formula interface to `glm` to train a GLM model.
#' No preprocessing is needed.
#'
#' @param x data.frame or similar: Training set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GLMHyperparameters` object: make using [setup_GLM].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return GLM model.
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_GLM <- function(
  x,
  weights = NULL,
  hyperparameters,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(hyperparameters, GLMHyperparameters)

  # Data ----
  check_supervised(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )

  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }

  type <- supervised_type(x)
  if (type == "Classification") {
    n_classes <- nlevels(outcome(x))
    if (n_classes > 2L) {
      cli::cli_abort("GLM does not support multiclass classification")
    }
  } else {
    n_classes <- NA_integer_
  }

  # Formula ----
  formula <- as.formula(
    paste(
      names(x)[ncol(x)],
      "~ ."
    )
  )

  # Train ----
  family <- if (type == "Regression") {
    gaussian()
  } else if (type == "Classification") {
    binomial()
  }
  model <- glm(
    formula = formula,
    family = family,
    data = x,
    weights = weights
  )
  check_inherits(model, "glm")
  model
} # /rtemis::train_GLM

#' Predict from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
predict_GLM <- function(model, newdata, type) {
  predict(model, newdata = newdata, type = "response")
} # /rtemis::predict_GLM

#' Get coefficients from GLM model
#'
#' @param model GLM model.
#'
#' @keywords internal
#' @noRd
varimp_GLM <- function(model) {
  coef(model)
} # /rtemis::varimp_GLM

#' Get Standard Errors from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
se_GLM <- function(model, newdata) {
  predict(model, newdata = newdata, se.fit = TRUE)[["se.fit"]]
}
