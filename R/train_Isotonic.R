# train_Isotonic.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train an Isotonic model
#'
#' @details
#' This is primarily used for calibration of classification models.
#' Binary classification will not work if x and y are not monotonic, i.e. higher values in `x` must
#' correspond to `1`, i.e. positive class in y.
#' outcome `1`.
#'
#' @param x data.frame or similar: Training set. Only a single predictor is allowed.
#' @param dat_validation data.frame or similar: Validation set.
#' @param dat_test data.frame or similar: Test set.
#' @param weights Not used.
#' @param hyperparameters `IsotonicHyperparameters` object: make using [setup_Isotonic].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `stepfun`.
#'
#' @author EDG
#' @keywords internal
#' @noRd

train_Isotonic <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL,
  weights = NULL,
  hyperparameters = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(hyperparameters, IsotonicHyperparameters)

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    dat_test = dat_test,
    allow_missing = FALSE,
    verbosity = verbosity
  )
  if (NCOL(x) > 2) {
    cli::cli_abort("Isotonic requires a single predictor.")
  }

  if (!is.null(weights)) {
    cli::cli_abort("Isotonic does not support weights.")
  }

  type <- supervised_type(x)
  if (type == "Classification") {
    n_classes <- nlevels(outcome(x))
    if (n_classes > 2L) {
      cli::cli_abort("Isotonic does not support multiclass classification")
    }
    # Assuming binclasspos = 2L
    y <- as.numeric(x[[2]]) - 1
  } else {
    y <- x[[2]]
    n_classes <- NA_integer_
  }

  # Model ----
  ir <- isoreg(cbind(x[[1]], y))
  model <- as.stepfun(ir)
  check_inherits(model, "stepfun")
  model
} # /rtemis::train_Isotonic

#' Predict from Isotonic model
#'
#' @param model Isotonic model.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
predict_Isotonic <- function(model, newdata, type, verbosity = 0L) {
  model(newdata[[1]])
} # /rtemis::predict_Isotonic

#' Get coefficients from Isotonic model
#'
#' @param model Isotonic model.
#'
#' @keywords internal
#' @noRd
varimp_Isotonic <- function(model) {
  NA
} # /rtemis::varimp_Isotonic
