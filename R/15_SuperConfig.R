# S7_SuperConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# %% SuperConfig ----
#' SuperConfig Class
#'
#' @description
#' Supervised Learning Configuration Class.
#'
#' @author EDG
#' @noRd
SuperConfig <- new_class(
  name = "SuperConfig",
  package = "rtemis",
  properties = list(
    dat_training_path = class_character | NULL,
    dat_validation_path = class_character | NULL,
    dat_test_path = class_character | NULL,
    weights = class_character | NULL, # column name in dat_training
    positive_class = class_character | NULL, # binary-classification positive level
    preprocessor_config = PreprocessorConfig | NULL,
    decomposition_config = DecompositionConfig | NULL,
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


# %% repr.SuperConfig ----
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


# %% print.SuperConfig ----
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


# %% setup_SuperConfig ----
#' Setup SuperConfig
#'
#' Setup `SuperConfig` object. `SuperConfig` is a portable, data-agnostic recipe:
#' `dat_training_path` is optional, so the same config can be validated, shared,
#' or described without data and have a path bound later (e.g. by the `rtemis`
#' CLI) before [train].
#'
#' @param dat_training_path Character or NULL: Path to training data file. NULL
#' leaves the recipe unbound; set it (or supply data) before [train].
#' @param dat_validation_path Character: Path to validation data file.
#' @param dat_test_path Character: Path to test data file.
#' @param weights Optional Character: Column name in training data to use as observation weights.
#' If NULL, no weights are used.
#' @param positive_class Character or NULL: For binary classification, the
#' outcome level to treat as positive. NULL keeps the existing factor level order.
#' @param preprocessor_config `PreprocessorConfig` object: Configuration for data preprocessing.
#' @param decomposition_config `DecompositionConfig` object: Configuration for data decomposition.
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
  dat_training_path = NULL,
  dat_validation_path = NULL,
  dat_test_path = NULL,
  weights = NULL,
  positive_class = NULL,
  preprocessor_config = NULL,
  decomposition_config = NULL,
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
  if (!is.null(dat_training_path)) {
    dat_training_path <- sanitize_path(dat_training_path, must_exist = FALSE)
  }

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
    positive_class = positive_class,
    preprocessor_config = preprocessor_config,
    decomposition_config = decomposition_config,
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


# %% .detect_config_kind ----
#' Detect the config family of a parsed config list
#'
#' Maps a parsed config's `$schema` URL to its rtemis family by exact match
#' against the supported schemas (.RTEMIS_SUPPORTED_CONFIGS). A missing,
#' malformed, or unrecognized `$schema` is an error: every config must declare a
#' known schema.
#'
#' @param x Named list from a parsed JSON config.
#'
#' @return Character: a family name from .RTEMIS_SUPPORTED_CONFIGS.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.detect_config_kind <- function(x) {
  schema <- x[["$schema"]]
  supported <- .RTEMIS_SUPPORTED_CONFIGS
  if (is.null(schema) || !is.character(schema) || length(schema) != 1L) {
    rtemis.core::abort(
      "Config is missing a valid `$schema`.\n",
      "Every config must declare one of the supported schemas:\n",
      paste0("  - ", supported, collapse = "\n"),
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  kind <- names(supported)[match(schema, supported)]
  if (is.na(kind)) {
    rtemis.core::abort(
      "Unsupported `$schema`: ",
      schema,
      ".\nSupported schemas:\n",
      paste0("  - ", supported, collapse = "\n"),
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  kind
} # /rtemis::.detect_config_kind


# %% .list_to_SuperConfig ----
#' Convert a list to a `SuperConfig` object
#'
#' Internal function used by [read_config] to reconstruct a `SuperConfig` from a
#' named list, such as the result of parsing a JSON config. Nested config
#' objects are rebuilt via their respective `.list_to_*` / `setup_*` functions.
#'
#' @param x Named list carrying `SuperConfig` fields (e.g. `algorithm`,
#'   `hyperparameters`, `decomposition_config`, `outer_resampling_config`).
#'
#' @return `SuperConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_SuperConfig <- function(x) {
  args <- list(
    dat_training_path = x[["dat_training_path"]],
    dat_validation_path = x[["dat_validation_path"]],
    dat_test_path = x[["dat_test_path"]],
    weights = x[["weights"]],
    positive_class = iflengthy(x[["positive_class"]]),
    preprocessor_config = if (is.null(x[["preprocessor_config"]])) {
      NULL
    } else {
      do.call(setup_Preprocessor, x[["preprocessor_config"]])
    },
    decomposition_config = if (is.null(x[["decomposition_config"]])) {
      NULL
    } else {
      .list_to_DecompositionConfig(x[["decomposition_config"]])
    },
    algorithm = x[["algorithm"]],
    # `.list_to_Hyperparameters` wants `{algorithm, hyperparameters}`. The JSON
    # schema keeps `algorithm` as a top-level sibling with a flat
    # `hyperparameters` map (as `write_config()` emits), so bundle it. A
    # pre-nested `{algorithm, hyperparameters}` shape is also accepted.
    hyperparameters = if (is.null(x[["hyperparameters"]])) {
      NULL
    } else if (!is.null(x[["hyperparameters"]][["hyperparameters"]])) {
      .list_to_Hyperparameters(x[["hyperparameters"]])
    } else {
      .list_to_Hyperparameters(list(
        algorithm = x[["algorithm"]],
        hyperparameters = x[["hyperparameters"]]
      ))
    },
    tuner_config = if (is.null(x[["tuner_config"]])) {
      NULL
    } else {
      .list_to_TunerConfig(x[["tuner_config"]])
    },
    outer_resampling_config = if (is.null(x[["outer_resampling_config"]])) {
      NULL
    } else {
      .list_to_ResamplerConfig(x[["outer_resampling_config"]])
    },
    question = iflengthy(x[["question"]])
  )
  # `execution_config`, `outdir`, and `verbosity` carry non-NULL defaults in
  # `setup_SuperConfig`; only override them when the config actually supplies a
  # value, so a portable recipe that omits them keeps the defaults.
  if (!is.null(x[["execution_config"]])) {
    args[["execution_config"]] <- do.call(
      setup_ExecutionConfig,
      x[["execution_config"]]
    )
  }
  if (!is.null(x[["outdir"]])) {
    args[["outdir"]] <- x[["outdir"]]
  }
  if (!is.null(x[["verbosity"]])) {
    args[["verbosity"]] <- x[["verbosity"]]
  }
  do.call(setup_SuperConfig, args)
} # /rtemis::.list_to_SuperConfig


# %% SuperConfigLive ----
#' SuperConfigLive
#'
#' @details
#' Like `SuperConfig`, but carries in-memory training/validation/test data
#' instead of file paths. Used by `rtemislive` (uploads arrive over a WS
#' frame, not as a file) and by future HPC submission paths that hand the
#' data directly to a worker.
#' Not serializable to a config file — in-memory data does not round-trip
#' cleanly. Use `SuperConfig` when you need on-disk reproducibility.
#'
#' @author EDG
#' @noRd
SuperConfigLive <- new_class(
  name = "SuperConfigLive",
  package = "rtemis",
  properties = list(
    dat_training = class_tabular,
    dat_validation = class_tabular | NULL,
    dat_test = class_tabular | NULL,
    weights = class_character | NULL, # column name in dat_training
    positive_class = class_character | NULL, # binary-classification positive level
    preprocessor_config = PreprocessorConfig | NULL,
    decomposition_config = DecompositionConfig | NULL,
    algorithm = class_character | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_config = TunerConfig | NULL,
    outer_resampling_config = ResamplerConfig | NULL,
    execution_config = ExecutionConfig,
    question = class_character | NULL,
    outdir = class_character | NULL,
    verbosity = class_integer
  )
) # /rtemis::SuperConfigLive


# %% repr.SuperConfigLive ----
#' @author EDG
#' @noRd
method(repr, SuperConfigLive) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("SuperConfigLive", pad = pad, output_type = output_type)
  # Replace heavy data slots with a {rows, cols} summary so the printout
  # stays readable.
  pl <- props(x)
  fmt_dim <- function(d) {
    if (is.null(d)) {
      return(NULL)
    }
    paste0("<", NROW(d), " x ", NCOL(d), ">")
  }
  pl[["dat_training"]] <- fmt_dim(pl[["dat_training"]])
  pl[["dat_validation"]] <- fmt_dim(pl[["dat_validation"]])
  pl[["dat_test"]] <- fmt_dim(pl[["dat_test"]])
  out <- paste0(
    out,
    repr_ls(pl, pad = pad, limit = 20L, output_type = output_type)
  )
  out
} # /rtemis::repr.SuperConfigLive


# %% print.SuperConfigLive ----
#' @author EDG
#' @noRd
method(print, SuperConfigLive) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.SuperConfigLive


# %% setup_SuperConfigLive ----
#' Setup SuperConfigLive
#'
#' Build a `SuperConfigLive` — same shape as [setup_SuperConfig] but with
#' in-memory tabular data instead of file paths.
#'
#' @param dat_training data.frame or data.table. Training data.
#' @param dat_validation data.frame, data.table, or `NULL`.
#' @param dat_test data.frame, data.table, or `NULL`.
#' @param weights Character or `NULL`. Column name in `dat_training` used
#'   as observation weights.
#' @param positive_class Character or `NULL`. For binary classification, the
#'   outcome level to treat as positive; forwarded to [train] which reorders
#'   the outcome factor via [set_positive_class]. `NULL` keeps the existing
#'   level order.
#' @param preprocessor_config,algorithm,hyperparameters,tuner_config,outer_resampling_config,execution_config,question,verbosity
#'   See [setup_SuperConfig].
#' @param decomposition_config `DecompositionConfig` object: Configuration for data decomposition.
#' @param outdir Character or `NULL`. Output directory; `NULL` (the
#'   default) means "do not write to disk" (the rtemislive case).
#'
#' @return `SuperConfigLive` object.
#'
#' @author EDG
#' @export
setup_SuperConfigLive <- function(
  dat_training,
  dat_validation = NULL,
  dat_test = NULL,
  weights = NULL,
  positive_class = NULL,
  preprocessor_config = NULL,
  decomposition_config = NULL,
  algorithm = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  execution_config = setup_ExecutionConfig(),
  question = NULL,
  outdir = NULL,
  verbosity = 1L
) {
  if (!is.null(outdir)) {
    outdir <- sanitize_path(outdir, must_exist = FALSE, type = "any")
  }
  SuperConfigLive(
    dat_training = dat_training,
    dat_validation = dat_validation,
    dat_test = dat_test,
    weights = weights,
    positive_class = positive_class,
    preprocessor_config = preprocessor_config,
    decomposition_config = decomposition_config,
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    tuner_config = tuner_config,
    outer_resampling_config = outer_resampling_config,
    execution_config = execution_config,
    question = question,
    outdir = outdir,
    verbosity = as.integer(verbosity)
  )
} # /rtemis::setup_SuperConfigLive
