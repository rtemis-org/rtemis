# calibrate.R
# ::rtemis::
# 2025 EDG rtemis.org

# calibrate <- new_generic("calibrate", "x")

# calibrate <- new_generic(
#   "calibrate", ("x"),
#   function(x, predicted_probabilities, true_labels, algorithm = "isotonic", hyperparameters = NULL) {
#     S7_dispatch()
#   }
# )

calibrate <- new_generic(
  "calibrate",
  ("x"),
  function(
    x,
    algorithm = "isotonic",
    hyperparameters = NULL,
    verbosity = 1L,
    ...
  ) {
    S7_dispatch()
  }
)

#' @name calibrate.Classification
#' @title
#' Calibrate Binary Classification Models
#'
#' @description
#' The goal of calibration is to adjust the predicted probabilities of a binary classification
#' model so that they better reflect the true probabilities (i.e. empirical risk) of the positive
#' class.
#'
#' @details
#' Important: The calibration model's training data should be different from the classification
#' model's training data.
#'
#' @param x Classification object.
#' @param predicted_probabilities Numeric vector: Predicted probabilities.
#' @param true_labels Factor: True class labels.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters Hyperparameters object: Setup using one of `setup_*` functions.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @return CalibratedClassification object.
#'
#' @author EDG
#' @export

calibrate.Classification <- function(
  x,
  predicted_probabilities,
  true_labels,
  algorithm = "isotonic",
  hyperparameters = NULL,
  verbosity = 1L,
  ...
) {
  # Check inputs
  check_is_S7(x, Classification)
  check_float01inc(predicted_probabilities)
  check_inherits(true_labels, "factor")

  # Training data is whatever is passed by user
  dat <- data.table(predicted_probabilities, true_labels)
  # Test data is taken from mod, if available
  if (!is.null(x@y_test) && !is.null(x@predicted_prob_test)) {
    dat_test <- data.table(
      predicted_probabilities = x@predicted_prob_test,
      true_labels = x@y_test
    )
  } else {
    dat_test <- NULL
  }
  # Calibration model
  cal_model <- train(
    dat,
    dat_test = dat_test,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    verbosity = verbosity
  )

  CalibratedClassification(x, cal_model)
} # /rtemis::calibrate


#' @name calibrate.ClassificationRes
#' @title
#' Calibrate Cross-validated Binary Classification Models
#'
#' @description
#' The goal of calibration is to adjust the predicted probabilities of a binary classification
#' model so that they better reflect the true probabilities (i.e. empirical risk) of the positive
#' class.
#'
#' @param x ClassificationRes object.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters Hyperparameters object: Setup using one of `setup_*` functions.
#' @param resampler_config ResamplerConfig
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @author EDG
#' @export

method(calibrate, Classification) <- function(
  x,
  algorithm = "isotonic",
  hyperparameters = NULL,
  verbosity = 1L,
  ...
) {
  calibrate.Classification(
    x,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    verbosity = verbosity,
    ...
  )
}

calibrate.ClassificationRes <- function(
  x,
  algorithm = "isotonic",
  hyperparameters = NULL,
  resampler_config = setup_Resampler(
    n_resamples = 5L,
    type = "KFold"
  ),
  verbosity = 1L,
  ...
) {
  # Check inputs
  check_inherits(algorithm, "character")
  check_is_S7(resampler_config, ResamplerConfig)
  verbosity <- clean_int(verbosity)

  # Check IFW is FALSE
  if (!is.null(hyperparameters) && hyperparameters[["ifw"]]) {
    cli::cli_abort("IFW must be FALSE for proper calibration.")
  }

  # Calibration models
  calmods <- lapply(
    x@models,
    function(mod) {
      dat <- data.table(
        predicted_probabilities = mod@predicted_prob_test,
        true_labels = mod@y_test
      )
      train(
        dat,
        algorithm = algorithm,
        hyperparameters = hyperparameters,
        outer_resampling = resampler_config
      )
    }
  )
  names(calmods) <- names(x@models)

  # calcv <- CalibrationRes(
  #   models = calmods,
  #   resampler_config = resampler_config
  # )

  # CalibratedClassificationRes
  CalibratedClassificationRes(x, calmods)
} # /rtemis::calibrate.ClassificationRes


method(calibrate, ClassificationRes) <- function(
  x,
  algorithm = "isotonic",
  hyperparameters = NULL,
  verbosity = 1L,
  ...
) {
  calibrate.ClassificationRes(
    x,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    verbosity = verbosity,
    ...
  )
}
