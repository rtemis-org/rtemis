# S7_SuperConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# %% SuperConfig Class ----
#' SuperConfig Class
#'
#' @description
#' Supervised Learning Configuration Class.
#'
#' @author EDG
#' @noRd
SuperConfig <- new_class(
  name = "SuperConfig",
  properties = list(
    dat_training_path = class_character,
    dat_validation_path = class_character | NULL,
    dat_test_path = class_character | NULL,
    weights = class_character | NULL, # column name in dat_training
    preprocessor_config = PreprocessorConfig | NULL,
    algorithm = class_character | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_config = TunerConfig | NULL,
    outer_resampling_config = ResamplerConfig | NULL,
    execution_config = ExecutionConfig,
    question = class_character | NULL,
    outdir = class_character,
    verbosity = class_integer
  )
) # /rtemis::SuperConfig


# %% repr SuperConfig ----
#' Repr SuperConfig
#'
#' @param x `SuperConfig` object.
#' @param pad Integer: Number of spaces to pad the message with.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted string that can be printed with cat()
#'
#' @author EDG
#' @noRd
method(repr, SuperConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("SuperConfig", pad = pad, output_type = output_type)
  out <- paste0(
    out,
    repr_ls(props(x), pad = pad + 2L, limit = 20L, output_type = output_type)
  )
  out
} # /rtemis::repr.SuperConfig


# %% print `SuperConfig` ----
#' Print `SuperConfig`
#'
#' Print `SuperConfig` object
#'
#' @param x `SuperConfig` object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, SuperConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.SuperConfig


# %% setup SuperConfig ----
#' Setup SuperConfig
#'
#' Setup `SuperConfig` object.
#'
#' @param dat_training_path Character: Path to training data file.
#' @param dat_validation_path Character: Path to validation data file.
#' @param dat_test_path Character: Path to test data file.
#' @param weights Optional Character: Column name in training data to use as observation weights. If NULL, no weights are used.
#' @param preprocessor_config `PreprocessorConfig` object: Configuration for data preprocessing.
#' @param algorithm Character: Algorithm to use for training.
#' @param hyperparameters `Hyperparameters` object: Configuration for model hyperparameters.
#' @param tuner_config `TunerConfig` object: Configuration for hyperparameter tuning.
#' @param outer_resampling_config `ResamplerConfig` object: Configuration for outer res
#' resampling during model training.
#' @param question Character: Question to answer with the supervised learning analysis.
#' @param outdir Character: Output directory for results.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `SuperConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' sc <- setup_superconfig(
#'   dat_training_path = "train.csv",
#'   preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
#'   algorithm = "LightRF",
#'   hyperparameters = setup_LightRF(),
#'   tuner_config = setup_GridSearch(),
#'   outer_resampling_config = setup_Resampler(),
#'   execution_config = setup_ExecutionConfig(),
#'   question = "Can we tell iris species apart given their measurements?",
#'   outdir = "models/"
#' )
setup_superconfig <- function(
  dat_training_path,
  dat_validation_path = NULL,
  dat_test_path = NULL,
  weights = NULL,
  preprocessor_config = NULL,
  algorithm = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  execution_config,
  question = NULL,
  outdir = "results/",
  verbosity = 1L
) {
  SuperConfig(
    dat_training_path = dat_training_path,
    dat_validation_path = dat_validation_path,
    dat_test_path = dat_test_path,
    weights = weights,
    preprocessor_config = preprocessor_config,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    tuner_config = tuner_config,
    outer_resampling_config = outer_resampling_config,
    execution_config = execution_config,
    question = question,
    outdir = outdir,
    verbosity = verbosity
  )
} # /setup_superconfig
