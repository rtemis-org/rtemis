# train_TabNet.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_.TabNetHyperparameters ----
#' Train a TabNet model
#'
#' Train a TabNet model using `TabNet`.
#'
#' TabNet does not work in the presence of missing values.
#'
#' @param hyperparameters `TabNetHyperparameters` object: make using [setup_TabNet].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data: Validation set for early stopping.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `TabNet`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, TabNetHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("torch", "tabnet")

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

  # Scale data ----
  y <- outcome(x)
  prp <- preprocess(
    features(x),
    config = setup_Preprocessor(scale = TRUE, center = TRUE)
  )
  x <- prp@preprocessed

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
  list(model = model, preprocessor = prp)
} # /rtemis::train_.TabNetHyperparameters


# %% predict_super.class_tabnet_fit ----
#' Predict from TabNet model
#'
#' @param model TabNet model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Character: "Regression" or "Classification".
#'
#' @keywords internal
#' @noRd
method(predict_super, class_tabnet_fit) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
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
} # /rtemis::predict_super.class_tabnet_fit


# %% varimp_super.class_tabnet_fit ----
#' Get variable importance from TabNet model
#'
#' @param model TabNet model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_tabnet_fit) <- function(model) {
  NULL
} # /rtemis::varimp_super.class_tabnet_fit
