# calibrate.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% calibrate.Classification ----
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
#' @keywords internal
#' @noRd
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
#' mod_c_glm <- train(
#'   x = datc2_train,
#'   dat_test = datc2_test,
#'   algorithm = "glm"
#' )
#' mod_c_glm_cal <- calibrate(
#'   mod_c_glm,
#'   predicted_probabilities = mod_c_glm$predicted_prob_training,
#'   true_labels = mod_c_glm$y_training
#' )
#' mod_c_glm_cal
method(calibrate, Classification) <- function(
  x,
  predicted_probabilities,
  true_labels,
  algorithm = "isotonic",
  hyperparameters = NULL,
  verbosity = 1L,
  ...
) {
  # Check inputs
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
  if (verbosity > 0L) {
    msg(
      fmt("<>", col = col_calibrator, bold = TRUE),
      "Calibrating",
      x@algorithm,
      "classification..."
    )
  }
  cal_model <- train(
    dat,
    dat_test = dat_test,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    verbosity = verbosity
  )

  mod_cal <- CalibratedClassification(x, cal_model)
  if (verbosity > 0L) {
    message()
    print(mod_cal)
    message()
  }
  if (verbosity > 0L) {
    msg(fmt("</>", col = col_calibrator, bold = TRUE), "Calibration done.")
  }
  mod_cal
} # /rtemis::calibrate


# %% calibrate.ClassificationRes ----
#' Calibrate Resampled Classification Models
#'
#' @param x `ClassificationRes` object.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param resampler_config `ResamplerConfig` object: Configuration for resampling during calibration model training.
#' @param train_verbosity Integer: Verbosity level for training calibration models.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used
#'
#' @return `CalibratedClassificationRes` object.
#' @author EDG
#' @keywords internal
#' @noRd
method(calibrate, ClassificationRes) <- function(
  x,
  algorithm = "isotonic",
  hyperparameters = NULL,
  resampler_config = setup_Resampler(
    n_resamples = 5L,
    type = "KFold"
  ),
  train_verbosity = 0L,
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
  if (verbosity > 0L) {
    msg(
      fmt("<>", col = col_calibrator, bold = TRUE),
      "Calibrating",
      x@algorithm,
      "resampled classification..."
    )
  }
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
        outer_resampling_config = resampler_config,
        verbosity = train_verbosity
      )
    }
  )
  names(calmods) <- names(x@models)

  # CalibratedClassificationRes
  modres_cal <- CalibratedClassificationRes(x, calmods)

  # Outro ----
  if (verbosity > 0L) {
    message()
    print(modres_cal)
    message()
  }
  if (verbosity > 0L) {
    msg(fmt("</>", col = col_calibrator, bold = TRUE), "Calibration done.")
  }
  modres_cal
} # /rtemis::calibrate.ClassificationRes
