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
#' @param hyperparameters `IsotonicHyperparameters` object: make using [setup_Isotonic].
#' @param x tabular data: Training set. Only a single predictor is allowed.
#' @param weights Not used.
#' @param dat_validation Not used.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `stepfun`.
#'
#' @author EDG
#' @keywords internal
#' @noRd

method(train_super, IsotonicHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(hyperparameters, IsotonicHyperparameters)

  # Data ----
  check_supervised(
    x = x,
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
  list(model = model, preprocessor = NULL)
} # /rtemis::train_super.IsotonicHyperparameters


#' Predict from Isotonic model
#'
#' @param model Isotonic model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Not used.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, class_stepfun) <- function(model, newdata, type = NULL) {
  model(newdata[[1]])
} # /rtemis::predict_super.class_stepfun


#' Get coefficients from Isotonic model
#'
#' @param model Isotonic model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_stepfun) <- function(model) {
  NA
} # /rtemis::varimp_super.class_stepfun
