# train_GLMNET.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% glmnet_design_matrix ----
#' Design matrix passed to glmnet
#'
#' glmnet takes a numeric matrix, so factors are expanded to dummy variables.
#' `penalty_factor` needs one entry per *expanded* column, which is why its
#' length cannot be declared as `data_bound = "n_features"` - the expanded
#' width exceeds the feature count whenever a factor is present.
#'
#' @param x tabular data: Training set (outcome in the last column).
#'
#' @return Numeric matrix.
#'
#' @author EDG
#' @keywords internal
#' @noRd
glmnet_design_matrix <- function(x) {
  as.matrix(model.matrix(~., exc(x, NCOL(x)))[, -1])
} # /rtemis::glmnet_design_matrix


# %% validate_hyperparameters.GLMNETHyperparameters ----
#' Validate GLMNET Hyperparameters
#'
#' `penalty_factor` is bound to the design-matrix width rather than the feature
#' count, which the `data_bound` vocabulary cannot express; everything else is
#' declarative and handled by `check_data_bounds()`.
#'
#' @param hyperparameters `GLMNETHyperparameters`: Hyperparameters to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(validate_hyperparameters, GLMNETHyperparameters) <- function(
  hyperparameters,
  x
) {
  check_data_bounds(hyperparameters, x)
  penalty_factor <- hyperparameters[["penalty_factor"]]
  # Only build the design matrix when there is something to check against it.
  if (!is.null(penalty_factor)) {
    n_design <- NCOL(glmnet_design_matrix(x))
    if (length(penalty_factor) != n_design) {
      rtemis.core::abort(
        "`penalty_factor` must have one value per design-matrix column: expected length ",
        n_design,
        ", got ",
        length(penalty_factor),
        ". Factors are expanded to dummy variables, so this can exceed the number of features (",
        NCOL(features(x)),
        ").",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.GLMNETHyperparameters


# %% train_.GLMNETHyperparameters ----
#' Train a GLMNET model
#'
#' Train a GLMNET model using `glmnet`.
#'
#' GLMNET does not work in the presence of missing values.
#'
#' @param hyperparameters `GLMNETHyperparameters` object: make using [setup_GLMNET].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data: Validation set (unused).
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, GLMNETHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("glmnet")

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

  # weights can't be NULL.
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }
  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    nlevels(outcome(x))
  } else {
    NA_integer_
  }
  family <- if (is.null(hyperparameters[["family"]])) {
    if (type == "Regression") {
      "gaussian"
    } else if (type == "Classification") {
      if (n_classes == 2L) {
        "binomial"
      } else {
        "multinomial"
      }
    }
  }

  # Train ----
  xm <- glmnet_design_matrix(x)
  # penalty_factor defaults to no differential penalty. A user-supplied value
  # has its length checked by validate_hyperparameters() before tuning begins.
  if (is.null(hyperparameters[["penalty_factor"]])) {
    hyperparameters@penalty_factor <- rep(1, NCOL(xm))
    if (verbosity > 1L) {
      info("NCOL(xm): ", NCOL(xm))
      info('Updated hyperparameters[["penalty_factor"]] to all 1s.')
    }
  }
  # if lambda is NULL, use cv.glmnet to find optimal lambda
  if (is.null(hyperparameters[["lambda"]])) {
    model <- glmnet::cv.glmnet(
      x = xm,
      y = outcome(x),
      family = family,
      weights = weights,
      offset = hyperparameters[["offset"]],
      alpha = hyperparameters[["alpha"]],
      nlambda = hyperparameters[["nlambda"]],
      standardize = hyperparameters[["standardize"]],
      intercept = hyperparameters[["intercept"]], # can't be NULL
      penalty.factor = hyperparameters[["penalty_factor"]]
    )
    check_inherits(model, "cv.glmnet")
  } else {
    model <- glmnet::glmnet(
      x = xm,
      y = outcome(x),
      family = family,
      weights = weights,
      offset = hyperparameters[["offset"]],
      alpha = hyperparameters[["alpha"]],
      nlambda = hyperparameters[["nlambda"]],
      lambda = hyperparameters[["lambda"]],
      standardize = hyperparameters[["standardize"]],
      intercept = hyperparameters[["intercept"]], # can't be NULL
      penalty.factor = hyperparameters[["penalty_factor"]]
    )
    check_inherits(model, "glmnet")
  }
  # `hyperparameters` is returned because this method resolved values into
  # it (R copied the caller's object, so the caller cannot see them).
  # `train()` adopts them, and the fitted model reports what it used.
  list(model = model, preprocessor = NULL, hyperparameters = hyperparameters)
} # /rtemis::train_.GLMNETHyperparameters

#' Predict from GLMNET model
#'
#' @param model glmnet model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Optional character: "Regression" or "Classification". Auto-detected if NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, class_glmnet) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  # Determine type
  # if model@classnames exists, type is Classification
  if (is.null(type)) {
    type <- if (!is.null(model[["classnames"]])) {
      "Classification"
    } else {
      "Regression"
    }
  }
  newdata <- as.matrix(
    model.matrix(~., newdata)[, -1, drop = FALSE]
  )
  if (type == "Regression") {
    predict(model, newx = newdata, type = "response")[, 1]
  } else if (type == "Classification") {
    predicted_prob <- predict(model, newx = newdata, type = "response")
    if (NCOL(predicted_prob) == 1) {
      # In binary classification, glmnet returns matrix with 1 column
      # with probabilities of second level.
      predicted_prob <- as.numeric(predicted_prob)
    }
    predicted_prob
  }
} # /rtemis::predict_super.class_glmnet

#' @keywords internal
#' @noRd
method(predict_super, class_cv.glmnet) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  # Determine type
  # if model@classnames exists, type is Classification
  if (is.null(type)) {
    type <- if (!is.null(model[["classnames"]])) {
      "Classification"
    } else {
      "Regression"
    }
  }
  newdata <- as.matrix(
    model.matrix(~., newdata)[, -1, drop = FALSE]
  )
  if (type == "Regression") {
    predict(model, newx = newdata, type = "response")[, 1]
  } else if (type == "Classification") {
    predicted_prob <- predict(model, newx = newdata, type = "response")
    if (NCOL(predicted_prob) == 1) {
      # In binary classification, glmnet returns matrix with 1 column
      # with probabilities of second level.
      predicted_prob <- as.numeric(predicted_prob)
    }
    predicted_prob
  }
} # /rtemis::predict_super.class_cv.glmnet


# %% varimp_super.class_glmnet ----
#' Get coefficients from GLMNET model
#'
#' @param model glmnet model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_glmnet) <- function(model) {
  coefs <- coef(model)

  # In multiclass, coef(model) returns a list of coefficient matrices, one per class.
  # Not yet supported as VariableImportance.
  if (is.list(coefs)) {
    return(NULL)
  }

  if (NCOL(coefs) > 1) {
    msg("GLMNET with multiple sets of coefficients - returning first column.")
  }

  # Exclude intercept
  coefs <- coefs[, 1][-1]
  VariableImportance(
    data.table(
      variable = names(coefs),
      Coefficient = unname(coefs)
    )
  )
} # /rtemis::varimp_super.class_glmnet


# %% varimp_super.class_cv.glmnet ----
#' @keywords internal
#' @noRd
method(varimp_super, class_cv.glmnet) <- function(model) {
  coefs <- coef(model)

  # In multiclass, coef(model) returns a list of coefficient matrices, one per class.
  # Not yet supported as VariableImportance.
  if (is.list(coefs)) {
    return(NULL)
  }

  # Exclude intercept
  coefs <- coefs[, 1][-1]
  VariableImportance(
    data.table(
      variable = names(coefs),
      Coefficient = unname(coefs)
    )
  )
} # /rtemis::varimp_super.class_cv.glmnet
