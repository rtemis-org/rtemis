# train_CART.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_super.CARTHyperparameters ----
#' Train a CART decision tree
#'
#' Train a CART decision tree using `rpart`.
#'
#' CART does not need any special preprocessing.
#' It works with numeric and factor variables and handles missing values.
#' The "train_*" functions train a single model.
#' Use [train] for tuning and test using nested cross-validation.
#'
#' @param hyperparameters `CARTHyperparameters` object: make using [setup_CART].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data or NULL: Not used for CART.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_super, CARTHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("rpart")

  # Arguments ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }

  # Train ----
  # weights can't be NULL.
  # !If formula is character, the input to weights must be the unquoted column name in the data.frame
  # that contains weights, e.g. by doing cbind(x, weights = weights)
  model <- rpart::rpart(
    as.formula(make_formula(x)),
    data = x,
    weights = weights,
    control = rpart::rpart.control(
      minsplit = hyperparameters[["minsplit"]],
      minbucket = hyperparameters[["minbucket"]],
      cp = hyperparameters[["cp"]],
      maxcompete = hyperparameters[["maxcompete"]],
      maxsurrogate = hyperparameters[["maxsurrogate"]],
      usesurrogate = hyperparameters[["usesurrogate"]],
      surrogatestyle = hyperparameters[["surrogatestyle"]],
      maxdepth = hyperparameters[["maxdepth"]],
      xval = hyperparameters[["xval"]]
    )
  )

  # Cost-Complexity Pruning ----
  if (!is.null(hyperparameters[["prune_cp"]])) {
    model <- rpart::prune(model, cp = hyperparameters[["prune_cp"]])
  }
  check_inherits(model, "rpart")
  list(model = model, preprocessor = NULL)
} # /rtemis::train_super.CARTHyperparameters


# %% predict_super.class_rpart ----
#' Predict from rpart model
#'
#' @param model rpart model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#'
#' @keywords internal
#' @noRd
method(predict_super, class_rpart) <- function(
  model,
  newdata,
  type = NULL
) {
  if (type == "Classification") {
    # Classification
    # predict.rpart returns a matrix n_cases x n_classes,
    # with classes are ordered the same as factor levels
    predicted_prob <- predict(model, newdata = newdata, type = "prob") # binclasspos = 2L
    if (NCOL(predicted_prob) == 2L) {
      # In binary classification, rpart returns matrix with 2 columns
      predicted_prob <- predicted_prob[, 2L]
    }
    predicted_prob
  } else {
    predict(model, newdata = newdata, type = "vector")
  }
} # /rtemis::predict_super.rpart


# %% varimp_super.class_rpart ----
#' Get variable importance from rpart model
#'
#' @param model rpart model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_rpart) <- function(model) {
  model[["variable.importance"]]
} # /rtemis::varimp_super.rpart
