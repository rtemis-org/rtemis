# train_GLMNET.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Prepare data for GLMNET
#'
#' @param x tabular data: Features.
#'
#' @return Matrix with features.
#'
#' @keywords internal
#' @noRd
preproc_GLMNET <- function(x) {
  as.matrix(
    model.matrix(~., x)[, -1]
  )
}

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

method(train_super, GLMNETHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("glmnet")

  # Checks ----
  check_is_S7(hyperparameters, GLMNETHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Convert "null" lambda to NULL
  if (hyperparameters[["lambda"]] == "null") {
    hyperparameters@hyperparameters[["lambda"]] <- NULL
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
  # Create xm so that the correct NCOL is used for penalty_factor,
  # since factors are converted to dummy variables.
  xm <- as.matrix(
    model.matrix(~., exc(x, NCOL(x)))[, -1]
  )
  # Check data-specific hyperparameter values
  # penalty_factor must be of length = N features.
  if (is.null(hyperparameters[["penalty_factor"]])) {
    hyperparameters@hyperparameters[["penalty_factor"]] <- rep(1, NCOL(xm))
    if (verbosity > 1L) {
      msg_info("NCOL(xm): ", NCOL(xm))
      msg_info('Updated hyperparameters[["penalty_factor"]] to all 1s.')
    }
  } else {
    if (length(hyperparameters[["penalty_factor"]]) != NCOL(xm)) {
      cli::cli_abort(
        "Length of penalty_factor must be equal to the number of predictors."
      )
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
  list(model = model, preprocessor = NULL)
} # /rtemis::train_super.GLMNETHyperparameters

#' Predict from GLMNET model
#'
#' @param model glmnet model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Character: "Regression" or "Classification" (auto-detected if NULL).
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, class_glmnet) <- function(model, newdata, type = NULL) {
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
  type = NULL
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


#' Get coefficients from GLMNET model
#'
#' @param model glmnet model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_glmnet) <- function(model) {
  coef(model)
} # /rtemis::varimp_super.class_glmnet


#' @keywords internal
#' @noRd
method(varimp_super, class_cv.glmnet) <- function(model) {
  coef(model)
} # /rtemis::varimp_super.class_cv.glmnet
