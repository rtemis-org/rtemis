# train_LightCART.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_.LightCARTHyperparameters ----
#' Decision Tree using LightGBM
#'
#' @param hyperparameters `LightCARTHyperparameters` object: make using [setup_LightCART].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation data.frame or similar: Validation set (not used for LightCART).
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LightCARTHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Checks ----
  check_is_S7(hyperparameters, LightCARTHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  if (type == "Classification") {
    nclasses <- nlevels(outcome(x))
  } else {
    nclasses <- 1L
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@hyperparameters[["objective"]] <- if (
      type == "Regression"
    ) {
      "regression"
    } else {
      if (nclasses == 2L) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }
  ## Preprocess & create lgb.Dataset ----
  lgb_data <- prepare_lgb_data(
    x = x,
    type = type,
    weights = weights,
    verbosity = verbosity
  )
  x <- lgb_data[["train_data"]]
  prp <- lgb_data[["preprocessor"]]

  # Train ----
  params <- hyperparameters@hyperparameters
  params[["ifw"]] <- NULL
  # num_class is required for multiclass classification only, must be 1 or unset for regression & binary classification
  if (nclasses > 2L) {
    params[["num_class"]] <- nclasses
  }
  # Set n threads
  params[["num_threads"]] <- 1L

  model <- lightgbm::lgb.train(
    params = params,
    data = x,
    nrounds = 1L,
    valids = list(training = x),
    early_stopping_rounds = NULL,
    verbose = verbosity - 2L
  )
  check_inherits(model, "lgb.Booster")
  list(model = model, preprocessor = prp)
} # /rtemis::train_.LightCARTHyperparameters
