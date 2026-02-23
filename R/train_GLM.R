# train_GLM.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% train_.GLMHyperparameters ----
#' Train a GLM model
#'
#' Train a GLM model using `stats::glm`.
#'
#' @details
#' `stats::glm` does not work in the presence of missing values.
#' This function uses the formula interface to `glm` to train a GLM model.
#' No preprocessing is needed.
#'
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param hyperparameters `GLMHyperparameters` object: make using [setup_GLM].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return GLM model.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, GLMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
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
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.GLMHyperparameters


# %% predict_super.class_glm ----
#' Predict from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_glm) <- function(
  model,
  newdata,
  type = NULL
) {
  predict(model, newdata = newdata, type = "response")
} # /rtemis::predict_super.glm


# %% varimp_super.class_glm ----
#' Get coefficients from GLM model
#'
#' @param model GLM model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_glm) <- function(
  model,
  type = c("coefficients", "p-value")
) {
  type <- match.arg(type)
  if (type == "coefficients") {
    coef(model)
  } else if (type == "p-value") {
    summary(model)[["coefficients"]][, 4]
  }
} # /rtemis::varimp_super.glm


# %% se_super.class_glm ----
#' Get Standard Errors from GLM model
#'
#' @param model GLM model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(se_super, class_glm) <- function(model, newdata) {
  predict(model, newdata = newdata, se.fit = TRUE)[["se.fit"]]
} # /rtemis::se_super.glm
