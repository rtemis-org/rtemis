# calibrate.R
# ::rtemis::
# 2025 EDG rtemis.org

# Calibrate Generic ----

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
#' @param x `Classification` object.
#' @param predicted_probabilities Numeric vector: Predicted probabilities.
#' @param true_labels Factor: True class labels.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @return `CalibratedClassification` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' datc2 <- data.frame(
#'   gn = factor(sample(c("alpha", "beta", "gamma"), 100, replace = TRUE)),
#'   iris[51:150, ]
#' )
#' res <- resample(datc2)
#' datc2$Species <- factor(datc2$Species)
#' datc2_train <- datc2[res[[1]], ]
#' datc2_test <- datc2[-res[[1]], ]
#' mod_c_lightrf <- train(
#'   x = datc2_train,
#'   dat_test = datc2_test,
#'   hyperparameters = setup_LightRF(nrounds = 20L)
#' )
#' mod_c_lightrf_cal <- calibrate(
#'   mod_c_lightrf,
#'   predicted_probabilities = mod_c_lightrf$predicted_prob_training,
#'   true_labels = mod_c_lightrf$y_training
#')
#' mod_c_lightrf_cal
#' # Note: In this trivial example, there may be no change in predicted probabilities.
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
#' @param x `ClassificationRes` object.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param resampler_config ResamplerConfig
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @author EDG
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
} # /rtemis::calibrate.Classification


#' Calibrate Resampled Classification Models
#'
#' @param x `ClassificationRes` object.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param resampler_config `ResamplerConfig` object: Configuration for resampling during calibration model training.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @return `CalibratedClassificationRes` object.
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' datc2 <- data.frame(
#'  gn = factor(sample(c("alpha", "beta", "gamma"), 100, replace = TRUE)),
#'  iris[51:150, ]
#' )
#' datc2$Species <- factor(datc2$Species)
#' resmod_c_lightrf <- train(
#'  x = datc2,
#'  hyperparameters = setup_LightRF(nrounds = 20L),
#'  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
#' )
#' resmod_c_lightrf_cal <- calibrate(resmod_c_lightrf)
#' resmod_c_lightrf_cal
#' }
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
        outer_resampling_config = resampler_config
      )
    }
  )
  names(calmods) <- names(x@models)

  # CalibratedClassificationRes
  CalibratedClassificationRes(x, calmods)
} # /rtemis::calibrate.ClassificationRes


#' Calibrate ClassificationRes
#'
#' @param x `ClassificationRes` object.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
} # /rtemis::calibrate.ClassificationRes
