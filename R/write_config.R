# write_config.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% write_config ----
#' Write an rtemis config to a JSON file
#'
#' Write a config object to a schema.rtemis.org JSON file that can be read back
#' with [read_config] and consumed by rtemislive and the `rtemis` CLI. The
#' emitted JSON carries a `$schema` field identifying the config family, so the
#' reader can dispatch to the right object:
#'
#' - `SuperConfig` (from [setup_SuperConfig]) -> supervised schema.
#' - `DecomposeConfig` (from [setup_DecomposeConfig]) -> decompose schema.
#' - `ClusterConfig` (from [setup_ClusterConfig]) -> cluster schema.
#' - `DecompositionConfig` (from `setup_PCA()`, `setup_ICA()`, ...) ->
#'   decomposition schema.
#' - `ClusteringConfig` (from `setup_KMeans()`, `setup_DBSCAN()`, ...) ->
#'   clustering schema.
#' - `PreprocessorConfig` (from [setup_Preprocessor]) -> preprocessor schema.
#'
#' A config is an **input**: what a run is being asked to do. Fields it omits
#' fall back to their `setup_*` defaults when read, so a config stays valid as
#' those defaults improve. That is the opposite of a **record**, which states
#' what one run actually did, with every value resolved -- see [write_record].
#' The two are separate functions rather than one with a flag, because a caller
#' should have to say which artifact they want.
#'
#' @param x A `SuperConfig`, `DecomposeConfig`, `ClusterConfig`,
#'   `DecompositionConfig`, `ClusteringConfig`, or `PreprocessorConfig` object.
#' @param file Character: Path to output JSON file.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' x <- setup_SuperConfig(
#'   dat_training_path = "~/Data/iris.csv",
#'   preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
#'   hyperparameters = setup_LightRF(),
#'   outer_resampling_config = setup_Resampler(),
#'   question = "Can we tell iris species apart given their measurements?",
#'   outdir = "models/"
#' )
#' tmpfile <- file.path(tempdir(), "rtemis.json")
#' write_config(x, tmpfile, overwrite = TRUE)
write_config <- new_generic(
  "write_config",
  "x",
  function(x, file, overwrite = FALSE, verbosity = 1L) {
    # See the generics note in `030_init.R`.
    if (!missing(file)) {
      force(file)
    }
    if (!missing(overwrite)) {
      force(overwrite)
    }
    if (!missing(verbosity)) {
      force(verbosity)
    }
    S7_dispatch()
  }
) # /rtemis::write_config


# %% .RTEMIS_SUPPORTED_CONFIGS ----
#' Supported rtemis config schemas
#'
#' Single source of truth for the config families that [write_config] emits and
#' [read_config] accepts: a named vector mapping each family to its complete
#' schema.rtemis.org schema URL (matching the URLs the `rtemis` CLI embeds). The
#' family name routes to the matching `.list_to_*` reconstructor; the URL is
#' written as a config's `$schema` and matched on read. The pipeline-recipe
#' families (`"decompose"`, `"cluster"`) bundle data path, algorithm config, and
#' output dir; the bare algorithm-config families (`"decomposition"`,
#' `"clustering"`) are the configs they wrap.
#'
#' @author EDG
#' @keywords internal
#' @noRd
# Record counterparts, keyed the same way. A record is the same field
# vocabulary with every value resolved, so it is a sibling document rather than
# a different family.
.RTEMIS_RECORD_SCHEMAS <- c(
  supervised = "https://schema.rtemis.org/supervised/v1/record.json",
  decompose = "https://schema.rtemis.org/decompose/v1/record.json",
  cluster = "https://schema.rtemis.org/cluster/v1/record.json"
) # /rtemis::.RTEMIS_RECORD_SCHEMAS


.RTEMIS_SUPPORTED_CONFIGS <- c(
  supervised = "https://schema.rtemis.org/supervised/v1/schema.json",
  decompose = "https://schema.rtemis.org/decompose/v1/schema.json",
  cluster = "https://schema.rtemis.org/cluster/v1/schema.json",
  decomposition = "https://schema.rtemis.org/decomposition/v1/schema.json",
  clustering = "https://schema.rtemis.org/clustering/v1/schema.json",
  preprocessor = "https://schema.rtemis.org/preprocessor/v1/schema.json"
) # /rtemis::.RTEMIS_SUPPORTED_CONFIGS


# %% .compact_config ----
#' Recursively drop zero-length elements from a config payload
#'
#' Removes `NULL` and empty-vector (e.g. `character(0)`) elements so the emitted
#' JSON carries only values that were actually set; everything omitted falls back
#' to its `setup_*` default on read. This also avoids the `[] -> list()`
#' round-trip mismatch when the JSON reader parses with `simplifyVector = FALSE`.
#'
#' @param x List or leaf value.
#'
#' @return `x` with zero-length elements removed recursively.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.compact_config <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, .compact_config)
  # Only drop empty elements from named lists (JSON objects); compacting an
  # unnamed list (JSON array) would renumber and shorten it, corrupting data.
  if (!is.null(names(x))) {
    x <- x[lengths(x) > 0L]
  }
  x
} # /rtemis::.compact_config


# %% .write_config_json ----
#' Serialize a config payload list to a JSON file
#'
#' @param payload Named list with a leading `$schema` element.
#' @param file Character: Path to output JSON file.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.write_config_json <- function(payload, file, overwrite, verbosity) {
  check_dependencies("jsonlite")
  json_str <- as.character(jsonlite::toJSON(
    .compact_config(payload),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  ))
  write_lines(
    json_str,
    file = file,
    overwrite = overwrite,
    verbosity = verbosity
  )
} # /rtemis::.write_config_json


# %% write_config.SuperConfig ----
#' @author EDG
#' @noRd
method(write_config, SuperConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  # Write the complete object: every nested config (preprocessor, decomposition,
  # resampler, tuner, hyperparameters) is serialized in full by `S7_to_list()`,
  # which recurses through props.
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["supervised"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.SuperConfig


# %% write_config.DecomposeConfig ----
#' @author EDG
#' @noRd
method(write_config, DecomposeConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["decompose"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.DecomposeConfig


# %% write_config.ClusterConfig ----
#' @author EDG
#' @noRd
method(write_config, ClusterConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["cluster"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.ClusterConfig


# %% write_config.DecompositionConfig ----
#' @author EDG
#' @noRd
method(write_config, DecompositionConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["decomposition"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.DecompositionConfig


# %% write_config.PreprocessorConfig ----
#' @author EDG
#' @noRd
method(write_config, PreprocessorConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["preprocessor"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.PreprocessorConfig


# %% write_config.ClusteringConfig ----
#' @author EDG
#' @noRd
method(write_config, ClusteringConfig) <- function(
  x,
  file,
  overwrite = FALSE,
  verbosity = 1L
) {
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["clustering"]]),
    S7_to_list(serializable_props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.ClusteringConfig


# %% write_record ----
#' Write a run record to a JSON file
#'
#' A record states what a run actually did: every value resolved, where each one
#' came from, and what produced it. Deliberately a separate function rather than
#' an argument to [write_config] -- the two artifacts answer different
#' questions, and a caller should have to say which it wants.
#'
#' Three things distinguish a record from the config it came from:
#'
#' - **Every field is present and resolved.** A config omits what the user did
#'   not set, so a reader applies defaults; a record leaves nothing to them, and
#'   writes an unset field as an explicit `null`.
#' - **Each value says where it came from**, in a parallel `origin` map:
#'   `user`, `default`, `derived` (computed from the data), `tuned`, or `unset`
#'   for a field a failed run never reached.
#' - **`provenance`** records the rtemis and R versions, platform, timing,
#'   outcome, and a fingerprint of the data.
#'
#' For a supervised run the top level is what was *asked for* and `folds` is
#' what *ran*, one entry per model fitted -- outer resampling resolves different
#' values in each fold, so a single resolved value at the top level would state
#' something no fold did.
#'
#' A supervised record also states **what the run scored**. `metrics` holds each
#' sample's headline row as a flat metric-to-value map, meaned across outer
#' resamples, with `metrics_sd` beside it for the spread (`null` for a single
#' fit, which has none). The full metrics -- the confusion matrix, the per-class
#' rows -- are in each fold's own `metrics`. The flat block exists so that "was
#' this model any good?" is one lookup in one file, with no averaging and no R,
#' which is what makes a directory of records rankable.
#'
#' [train], [decomp] and [cluster] call this automatically when given an
#' `outdir`. A record is not a config: feeding one to [read_config] is an error,
#' since its resolved values would silently pin settings the new call should
#' decide for itself.
#'
#' Unlike a config, a record is **not compacted**: an unset field is written as
#' an explicit `null` rather than omitted, so nothing in it falls back to a
#' reader's defaults. That is the whole claim a record makes.
#'
#' @param x Fitted model object, or a record list from [record].
#' @param file Character: Path to write to.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' mod <- train(iris, hyperparameters = setup_CART())
#' write_record(mod, "train_CART.record.json")
#' }
write_record <- function(x, file, overwrite = FALSE, verbosity = 1L) {
  check_dependencies("jsonlite")
  payload <- if (is.list(x) && !S7_inherits(x)) x else record(x)
  json_str <- as.character(jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null"
  ))
  write_lines(
    json_str,
    file = file,
    overwrite = overwrite,
    verbosity = verbosity
  )
  # The same opportunistic check `write_config()` runs, and it matters more
  # here: a record and its schema are generated from one set of declarations, so
  # the two disagreeing means the generator and the writer have drifted.
  .validate_config_cli(file)
  invisible(x)
} # /rtemis::write_record
