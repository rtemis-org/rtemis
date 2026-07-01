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
#'
#' @param x A `SuperConfig`, `DecomposeConfig`, `ClusterConfig`,
#'   `DecompositionConfig`, or `ClusteringConfig` object.
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
#'   algorithm = "LightRF",
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
.RTEMIS_SUPPORTED_CONFIGS <- c(
  supervised = "https://schema.rtemis.org/supervised/v1/schema.json",
  decompose = "https://schema.rtemis.org/decompose/v1/schema.json",
  cluster = "https://schema.rtemis.org/cluster/v1/schema.json",
  decomposition = "https://schema.rtemis.org/decomposition/v1/schema.json",
  clustering = "https://schema.rtemis.org/clustering/v1/schema.json"
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
  # which recurses through props. The Hyperparameters object carries its own
  # `algorithm`, so a SuperConfig built from `hyperparameters` alone (top-level
  # `algorithm` left NULL) still round-trips: `read_config()` rebuilds it with
  # the matching `setup_<algorithm>()`.
  payload <- c(
    list(`$schema` = .RTEMIS_SUPPORTED_CONFIGS[["supervised"]]),
    S7_to_list(props(x))
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
    S7_to_list(props(x))
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
    S7_to_list(props(x))
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
    S7_to_list(props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.DecompositionConfig


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
    S7_to_list(props(x))
  )
  .write_config_json(payload, file, overwrite, verbosity)
  invisible(x)
} # /rtemis::write_config.ClusteringConfig
