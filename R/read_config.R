# read_config.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% read_config ----
#' Read an rtemis config from file
#'
#' Read a schema.rtemis.org JSON config and return the appropriate rtemis object.
#' Two families of config are supported: pipeline recipes that bundle a data
#' path, algorithm config, and output directory (`SuperConfig`,
#' `DecomposeConfig`, `ClusterConfig`), and the bare algorithm configs they wrap
#' (`DecompositionConfig`, `ClusteringConfig`). The file is JSON, as written by
#' [write_config] and consumed by rtemislive and the `rtemis` CLI.
#'
#' The config family is taken from the instance's `$schema`; pass `kind` to
#' override (a config without a `$schema` defaults to `supervised`).
#'
#' @param file Character: Path to input JSON config file.
#' @param kind Character or NULL: Force the config family (`"supervised"`,
#'   `"decompose"`, `"cluster"`, `"decomposition"`, or `"clustering"`) instead of
#'   detecting it from `$schema`.
#'
#' @return A `SuperConfig`, `DecomposeConfig`, `ClusterConfig`,
#'   `DecompositionConfig`, or `ClusteringConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Create a SuperConfig object
#' x <- setup_SuperConfig(
#'   dat_training_path = "~/Data/iris.csv",
#'   algorithm = "LightRF",
#'   hyperparameters = setup_LightRF()
#' )
#' # Write JSON config file
#' tmpdir <- tempdir()
#' tmpfile <- file.path(tmpdir, "rtemis_test.json")
#' write_config(x, tmpfile, overwrite = TRUE)
#' # Read config back from JSON file
#' x_read <- read_config(tmpfile)
read_config <- function(file, kind = NULL) {
  file <- sanitize_path(file, must_exist = TRUE, type = "file")
  check_dependencies("jsonlite")
  # Simplify leaf scalar-arrays to atomic vectors (e.g. a multi-value
  # `date_features`) so they satisfy the `setup_*` type checks, but keep object
  # arrays and nested objects as named lists for the `.list_to_*` reconstructors.
  xl <- jsonlite::fromJSON(
    file,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  if (is.null(kind)) {
    kind <- .detect_config_kind(xl)
  }
  switch(
    kind,
    supervised = .list_to_SuperConfig(xl),
    decompose = .list_to_DecomposeConfig(xl),
    cluster = .list_to_ClusterConfig(xl),
    decomposition = .list_to_DecompositionConfig(xl),
    clustering = .list_to_ClusteringConfig(xl),
    rtemis.core::abort(
      "Unknown config kind: ",
      kind,
      ". Expected 'supervised', 'decompose', 'cluster', 'decomposition', or ",
      "'clustering'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
} # /rtemis::read_config
