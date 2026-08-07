# train_Isotonic.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% isotonic_bound_probabilities ----
#' Keep isotonic block means away from 0 and 1
#'
#' Isotonic regression on a 0/1 outcome returns the mean of each level block,
#' so a block whose cases share a label is fitted at exactly 0 or 1. As a
#' calibrated probability that is an assertion of certainty, and it makes log
#' loss infinite for a single case there whose label disagrees.
#'
#' The bound is `1 / (2 * n)`: a calibration set of `n` cases cannot resolve a
#' probability finer than one case in `n`, so half a case is the smallest
#' defensible distance from the boundary. It shrinks as the calibration set
#' grows, so the bound never dominates a genuinely confident block.
#'
#' Only the fitted values move, and they move by less than the spacing between
#' adjacent block means, so the fit stays non-decreasing and the ordering --
#' and therefore AUC -- is untouched.
#'
#' @param yf Numeric vector: Fitted block means from `isoreg`.
#' @param n_cases Integer: Number of calibration cases.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
isotonic_bound_probabilities <- function(yf, n_cases) {
  eps <- 1 / (2 * n_cases)
  pmin(pmax(yf, eps), 1 - eps)
} # /rtemis::isotonic_bound_probabilities


# %% train_.IsotonicHyperparameters ----
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
method(train_, IsotonicHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Data ----
  check_supervised(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )
  if (NCOL(x) > 2) {
    rtemis.core::abort(
      "Isotonic requires a single predictor.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }

  if (!is.null(weights)) {
    rtemis.core::abort(
      "Isotonic does not support weights.",
      class = "rtemis_unsupported_error"
    )
  }

  type <- supervised_type(x)
  if (type == "Classification") {
    n_classes <- nlevels(outcome(x))
    if (n_classes > 2L) {
      rtemis.core::abort(
        "Isotonic does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    # Assuming binclasspos = 2L
    y <- as.numeric(x[[2]]) - 1
  } else {
    y <- x[[2]]
    n_classes <- NA_integer_
  }

  # Model ----
  ir <- isoreg(cbind(x[[1]], y))
  # A regression outcome is on its own scale, so only a classification fit --
  # whose fitted values are probabilities -- is bounded.
  if (type == "Classification") {
    ir[["yf"]] <- isotonic_bound_probabilities(ir[["yf"]], NROW(x))
  }
  model <- as.stepfun(ir)
  check_inherits(model, "stepfun")
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.IsotonicHyperparameters


# %% predict_super.class_stepfun ----
#' Predict from Isotonic model
#'
#' @param model Isotonic model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Not used.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, class_stepfun) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  model(newdata[[1]])
} # /rtemis::predict_super.class_stepfun


# %% varimp_super.class_stepfun ----
#' Get coefficients from Isotonic model
#'
#' @param model Isotonic model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_stepfun) <- function(model) {
  NULL
} # /rtemis::varimp_super.class_stepfun
