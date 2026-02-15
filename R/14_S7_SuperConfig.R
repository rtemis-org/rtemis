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
    repr_ls(props(x), pad = pad, limit = 20L, output_type = output_type)
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
#' @param execution_config `ExecutionConfig` object: Configuration for execution settings. Setup
#' with [setup_ExecutionConfig].
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
#' sc <- setup_SuperConfig(
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
setup_SuperConfig <- function(
  dat_training_path,
  dat_validation_path = NULL,
  dat_test_path = NULL,
  weights = NULL,
  preprocessor_config = NULL,
  algorithm = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  execution_config = setup_ExecutionConfig(),
  question = NULL,
  outdir = "results/",
  verbosity = 1L
) {
  # Sanitize paths for security
  dat_training_path <- sanitize_path(dat_training_path, must_exist = FALSE)

  if (!is.null(dat_validation_path)) {
    dat_validation_path <- sanitize_path(
      dat_validation_path,
      must_exist = FALSE
    )
  }

  if (!is.null(dat_test_path)) {
    dat_test_path <- sanitize_path(dat_test_path, must_exist = FALSE)
  }

  outdir <- sanitize_path(outdir, must_exist = FALSE, type = "any")

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
} # /setup_SuperConfig


# %% to_toml SuperConfig ----
#' Convert `SuperConfig` to TOML
#'
#' Convert `SuperConfig` object to TOML format for saving to file that can be read back in with
#' `read_config()`.
#'
#' @param x `SuperConfig` object.
#'
#' @return Character: TOML string representation of the `SuperConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_toml, SuperConfig) <- function(x) {
  check_dependencies("toml")
  xl <- S7_to_list(props(x))
  toml_with_meta(x, xl)
} # /rtemis::to_toml.SuperConfig


# %% write_toml SuperConfig ----
#' @name
#' write_toml
#'
#' @param x `SuperConfig` object.
#' @param file Character: Path to output TOML file.
#' @param overwrite Logical: If TRUE, overwrite existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `SuperConfig` object, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- setup_SuperConfig(
#'   dat_training_path = "~/Data/iris.csv",
#'   dat_validation_path = NULL,
#'   dat_test_path = NULL,
#'   weights = NULL,
#'   preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
#'   algorithm = "LightRF",
#'   hyperparameters = setup_LightRF(),
#'   tuner_config = setup_GridSearch(),
#'   outer_resampling_config = setup_Resampler(),
#'   execution_config = setup_ExecutionConfig(),
#'   question = "Can we tell iris species apart given their measurements?",
#'   outdir = "models/",
#'   verbosity = 1L
#' )
#' tmpdir <- tempdir()
#' write_toml(x, file.path(tmpdir, "rtemis.toml"))
write_toml.SuperConfig <- method(write_toml, SuperConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  toml_str <- to_toml(x)
  write_lines(
    toml_str,
    file = file,
    overwrite = overwrite,
    verbosity = verbosity
  )
  invisible(x)
} # /rtemis::write_toml.SuperConfig


# %% read_config ----
#' Read `SuperConfig` from TOML file
#'
#' Read `SuperConfig` object from TOML file that was written with `write_toml()`.
#'
#' @param file Character: Path to input TOML file.
#'
#' @return `SuperConfig` object.
#'
#' @author EDG
#' @export
read_config <- function(file) {
  check_dependencies("toml")
  file <- sanitize_path(file, must_exist = TRUE, type = "file")
  xl <- toml::read_toml(file)
  xl <- toml_empty_to_null(xl)
  # Convert list to SuperConfig object

  setup_SuperConfig(
    dat_training_path = xl[["dat_training_path"]],
    dat_validation_path = xl[["dat_validation_path"]],
    dat_test_path = xl[["dat_test_path"]],
    weights = xl[["weights"]],
    preprocessor_config = if (is.null(xl[["preprocessor_config"]])) {
      NULL
    } else {
      do.call(setup_Preprocessor, xl[["preprocessor_config"]])
    },
    algorithm = xl[["algorithm"]],
    hyperparameters = if (is.null(xl[["hyperparameters"]])) {
      NULL
    } else {
      list_to_Hyperparameters(xl[["hyperparameters"]])
    },
    tuner_config = if (is.null(xl[["tuner_config"]])) {
      NULL
    } else {
      list_to_TunerConfig(xl[["tuner_config"]])
    },
    outer_resampling_config = if (is.null(xl[["outer_resampling_config"]])) {
      NULL
    } else {
      list_to_ResamplerConfig(xl[["outer_resampling_config"]])
    },
    execution_config = if (is.null(xl[["execution_config"]])) {
      setup_ExecutionConfig()
    } else {
      do.call(setup_ExecutionConfig, xl[["execution_config"]])
    },
    question = iflengthy(xl[["question"]]),
    outdir = iflengthy(xl[["outdir"]]),
    verbosity = iflengthy(xl[["verbosity"]])
  )
} # /rtemis::read_config


# %% to_yaml SuperConfig ----
#' Convert `SuperConfig` to YAML
#'
#' Convert `SuperConfig` object to YAML format for saving to file that can be read back in with
#' `read_config()`.
#'
#' @param x `SuperConfig` object.
#'
#' @return Character: YAML string representation of the `SuperConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_yaml, SuperConfig) <- function(x) {
  xl <- S7_to_list(props(x))
  yaml::as.yaml(xl)
} # /rtemis::to_yaml.SuperConfig
