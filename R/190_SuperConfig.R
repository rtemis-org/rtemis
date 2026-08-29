# 190_SuperConfig.R
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
    dat_training_path = prop_string(
      NULL,
      nullable = TRUE,
      description = "Path to the training data."
    ),
    dat_validation_path = prop_string(
      NULL,
      nullable = TRUE,
      description = "Path to the validation data."
    ),
    dat_test_path = prop_string(
      NULL,
      nullable = TRUE,
      description = "Path to the held-out test data."
    ),
    # How the paths above are read. A delimited file carries no type
    # information, so something has to decide that a column of labels is a
    # factor rather than a string, and that decision belongs to the config: it
    # changes what the run trains on, it is what `validate_config()` has to know
    # to answer whether the run can work, and the record has to be able to
    # report it. Applied to all three datasets, so their columns cannot end up
    # typed differently from each other.
    character2factor = prop_boolean(
      TRUE,
      description = paste0(
        "Read character columns as factors. Supervised learning needs ",
        "categorical predictors as factors, and a delimited file cannot say ",
        "which columns those are."
      )
    ),
    # Column name in the training data.
    weights = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the column to use as case weights."
    ),
    # Binary-classification positive level.
    positive_class = prop_string(
      NULL,
      nullable = TRUE,
      description = "Outcome level to treat as positive (binary classification)."
    ),
    preprocessor_config = NULL | SupervisedPreprocessorConfig,
    decomposition_config = NULL | DecompositionConfig,
    # A set is a union of search spaces over one algorithm, and a recipe has to
    # say which was asked for. NULL stays first so the prototype is NULL.
    hyperparameters = NULL | Hyperparameters | HyperparametersSet,
    tuner_config = NULL | TunerConfig,
    outer_resampling_config = NULL | ResamplerConfig,
    execution_config = ExecutionConfig,
    question = prop_string(
      NULL,
      nullable = TRUE,
      description = "User-provided label / question for the run."
    ),
    # Nullable, like `SuperConfigLive`: a run that writes nothing to disk must
    # be able to say so. A non-nullable field would have every record claim the
    # default directory whatever the run did, and a record that names a
    # directory nothing was written to is worse than one that names none.
    # `setup_SuperConfig()` still defaults to "results/", so a portable recipe
    # that omits the field keeps writing where it always did.
    outdir = prop_string(
      NULL,
      nullable = TRUE,
      description = "Output directory for results. NULL = do not write to disk."
    ),
    verbosity = prop_integer(
      1L,
      min = 0L,
      description = "Verbosity level."
    )
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
#' @param dat_validation_path Optional Character: Path to validation data file.
#' @param dat_test_path Optional Character: Path to test data file.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' A delimited file carries no type information, so this is what decides whether
#' a column of labels is a categorical predictor or an unusable string one.
#' Applied to all three datasets.
#' @param weights Optional Character: Column name in training data to use as observation weights.
#' If NULL, no weights are used.
#' @param positive_class Character or NULL: For binary classification, the
#' outcome level to treat as positive. NULL keeps the existing factor level order.
#' @param preprocessor_config `SupervisedPreprocessorConfig` object: Configuration for data preprocessing.
#' @param decomposition_config `DecompositionConfig` object: Configuration for data decomposition.
#' @param hyperparameters `Hyperparameters` object: Configuration for model hyperparameters.
#' @param tuner_config `TunerConfig` object: Configuration for hyperparameter tuning.
#' @param outer_resampling_config `ResamplerConfig` object: Configuration for outer res
#' resampling during model training.
#' @param execution_config `ExecutionConfig` object: Configuration for execution settings. Setup
#' with [setup_ExecutionConfig].
#' @param question Optional Character: Question to answer with the supervised learning analysis.
#' @param outdir Optional Character: Output directory for results; `NULL` to
#' write nothing to disk.
#' @param verbosity Integer [0, Inf): Verbosity level.
#'
#' @return `SuperConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' sc <- setup_SuperConfig(
#'   dat_training_path = "train.csv",
#'   preprocessor_config = setup_SupervisedPreprocessor(scale = TRUE),
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
  character2factor = TRUE,
  weights = NULL,
  positive_class = NULL,
  preprocessor_config = NULL,
  decomposition_config = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  execution_config = setup_ExecutionConfig(),
  question = NULL,
  outdir = "results/",
  verbosity = 1L
) {
  # Validated, not resolved: a config is a portable recipe, so it stores the
  # path its author wrote rather than that path resolved against this machine's
  # working directory.
  if (!is.null(dat_training_path)) {
    dat_training_path <- sanitize_path(
      dat_training_path,
      must_exist = FALSE,
      normalize = FALSE
    )
  }

  if (!is.null(dat_validation_path)) {
    dat_validation_path <- sanitize_path(
      dat_validation_path,
      must_exist = FALSE,
      normalize = FALSE
    )
  }

  if (!is.null(dat_test_path)) {
    dat_test_path <- sanitize_path(
      dat_test_path,
      must_exist = FALSE,
      normalize = FALSE
    )
  }

  # Nullable like the property it fills, and like `setup_SuperConfigLive()`: a
  # run that writes nothing to disk has no path to sanitize.
  if (!is.null(outdir)) {
    outdir <- sanitize_path(
      outdir,
      must_exist = FALSE,
      type = "any",
      normalize = FALSE
    )
  }

  SuperConfig(
    dat_training_path = dat_training_path,
    dat_validation_path = dat_validation_path,
    dat_test_path = dat_test_path,
    character2factor = character2factor,
    weights = weights,
    positive_class = positive_class,
    preprocessor_config = preprocessor_config,
    decomposition_config = decomposition_config,
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
    # A record is the same field vocabulary with every value resolved, so it
    # would *read* as a config and quietly replace the defaults the caller
    # expected to be live -- including values a run derived from data this call
    # has never seen. Named rather than lumped in with an unknown URL.
    record_kind <- names(.RTEMIS_RECORD_SCHEMAS)[
      match(schema, .RTEMIS_RECORD_SCHEMAS)
    ]
    if (!is.na(record_kind)) {
      rtemis.core::abort(
        "This is a ",
        record_kind,
        " run *record*, not a config.\n",
        "A record states what one run resolved -- including values derived ",
        "from its data -- so using it as an input would silently pin settings ",
        "this call should decide for itself.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
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
#' @param x Named list carrying `SuperConfig` fields (e.g. `hyperparameters`,
#'   `decomposition_config`, `outer_resampling_config`).
#'
#' @return `SuperConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_SuperConfig <- function(x) {
  check_wire_keys(x, names(SuperConfig@properties), "supervised config")
  args <- list(
    dat_training_path = x[["dat_training_path"]],
    dat_validation_path = x[["dat_validation_path"]],
    dat_test_path = x[["dat_test_path"]],
    weights = x[["weights"]],
    positive_class = iflengthy(x[["positive_class"]]),
    preprocessor_config = if (is.null(x[["preprocessor_config"]])) {
      NULL
    } else {
      .list_to_SupervisedPreprocessorConfig(x[["preprocessor_config"]])
    },
    decomposition_config = if (is.null(x[["decomposition_config"]])) {
      NULL
    } else {
      .list_to_DecompositionConfig(x[["decomposition_config"]])
    },
    hyperparameters = if (is.null(x[["hyperparameters"]])) {
      NULL
    } else if (is_wire_hyperparameters_set(x[["hyperparameters"]])) {
      .list_to_HyperparametersSet(x[["hyperparameters"]])
    } else {
      .list_to_Hyperparameters(x[["hyperparameters"]])
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
  # `execution_config` and `verbosity` carry non-NULL defaults in
  # `setup_SuperConfig`; only override them when the config actually supplies a
  # value, so a portable recipe that omits them keeps the defaults.
  if (!is.null(x[["execution_config"]])) {
    args[["execution_config"]] <- do.call(
      setup_ExecutionConfig,
      .drop_meta_keys(x[["execution_config"]])
    )
  }
  # `outdir` is nullable *and* carries a non-NULL `setup_SuperConfig` default,
  # so an absent key and an explicit `null` mean different things: absent keeps
  # "results/", `null` means write nothing. Key presence is the only thing that
  # separates them. `args["outdir"] <- list(NULL)` stores a NULL element;
  # `args[["outdir"]] <- NULL` would delete it and restore the default.
  if ("outdir" %in% names(x)) {
    args["outdir"] <- list(x[["outdir"]])
  }
  if (!is.null(x[["verbosity"]])) {
    args[["verbosity"]] <- x[["verbosity"]]
  }
  # Non-nullable with a TRUE default, so an absent key keeps the default and
  # only an explicit `false` turns the conversion off.
  if (!is.null(x[["character2factor"]])) {
    args[["character2factor"]] <- x[["character2factor"]]
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
#' Not serializable to a config file -- in-memory data does not round-trip
#' cleanly. Use `SuperConfig` when you need on-disk reproducibility.
#'
#' @author EDG
#' @noRd
SuperConfigLive <- new_class(
  name = "SuperConfigLive",
  package = "rtemis",
  properties = list(
    dat_training = class_tabular,
    dat_validation = NULL | class_tabular,
    dat_test = NULL | class_tabular,
    # Column name in dat_training.
    weights = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the column to use as case weights."
    ),
    # Binary-classification positive level.
    positive_class = prop_string(
      NULL,
      nullable = TRUE,
      description = "Outcome level to treat as positive (binary classification)."
    ),
    preprocessor_config = NULL | SupervisedPreprocessorConfig,
    decomposition_config = NULL | DecompositionConfig,
    # A set is a union of search spaces over one algorithm, and a recipe has to
    # say which was asked for. NULL stays first so the prototype is NULL.
    hyperparameters = NULL | Hyperparameters | HyperparametersSet,
    tuner_config = NULL | TunerConfig,
    outer_resampling_config = NULL | ResamplerConfig,
    execution_config = ExecutionConfig,
    question = prop_string(
      NULL,
      nullable = TRUE,
      description = "User-provided label / question for the run."
    ),
    outdir = prop_string(
      NULL,
      nullable = TRUE,
      description = "Output directory for results. NULL = do not write to disk."
    ),
    verbosity = prop_integer(
      1L,
      min = 0L,
      description = "Verbosity level."
    )
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
#' Build a `SuperConfigLive` -- same shape as [setup_SuperConfig] but with
#' in-memory tabular data instead of file paths.
#'
#' @param dat_training data.frame or data.table. Training data.
#' @param dat_validation data.frame, data.table, or `NULL`.
#' @param dat_test data.frame, data.table, or `NULL`.
#' @param weights Optional Character: Column name in `dat_training` used
#'   as observation weights.
#' @param positive_class Optional Character: For binary classification, the
#'   outcome level to treat as positive; forwarded to [train] which reorders
#'   the outcome factor via [set_positive_class]. `NULL` keeps the existing
#'   level order.
#' @param preprocessor_config,hyperparameters,tuner_config,outer_resampling_config,execution_config,question,verbosity
#'   See [setup_SuperConfig].
#' @param decomposition_config `DecompositionConfig` object: Configuration for data decomposition.
#' @param outdir Optional Character: Output directory; `NULL`
#'   means "do not write to disk" (the rtemislive case).
#'
#' @return `SuperConfigLive` object.
#'
#' @author EDG
#' @export
#' @examples
#' scl <- setup_SuperConfigLive(
#'   dat_training = iris,
#'   hyperparameters = setup_LightGBM(),
#'   outer_resampling_config = setup_Resampler(),
#'   question = "Can we tell iris species apart given their measurements?"
#' )
setup_SuperConfigLive <- function(
  dat_training,
  dat_validation = NULL,
  dat_test = NULL,
  weights = NULL,
  positive_class = NULL,
  preprocessor_config = NULL,
  decomposition_config = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  execution_config = setup_ExecutionConfig(),
  question = NULL,
  outdir = NULL,
  verbosity = 1L
) {
  if (!is.null(outdir)) {
    outdir <- sanitize_path(
      outdir,
      must_exist = FALSE,
      type = "any",
      normalize = FALSE
    )
  }
  SuperConfigLive(
    dat_training = dat_training,
    dat_validation = dat_validation,
    dat_test = dat_test,
    weights = weights,
    positive_class = positive_class,
    preprocessor_config = preprocessor_config,
    decomposition_config = decomposition_config,
    hyperparameters = hyperparameters,
    tuner_config = tuner_config,
    outer_resampling_config = outer_resampling_config,
    execution_config = execution_config,
    question = question,
    outdir = outdir,
    verbosity = as.integer(verbosity)
  )
} # /rtemis::setup_SuperConfigLive
