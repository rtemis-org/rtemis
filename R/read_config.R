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
#' The config family is taken from the instance's `$schema`, which must be one of
#' the supported schemas; a missing or unrecognized `$schema` is an error.
#'
#' When the `rtemis` CLI is on the `PATH`, the file is additionally validated
#' against its vendored schema.rtemis.org JSON Schema before reconstruction, so
#' structural problems (unknown fields, wrong nesting, missing required values)
#' are caught up front with schema-path errors. Without the CLI, the `setup_*`
#' functions remain the backstop, validating field values during reconstruction.
#' Control this with `options(rtemis.validate = )`: `"auto"` (default) uses the
#' CLI when present and skips it otherwise, `"always"` errors if the CLI is
#' missing, `"never"` skips the gate. The CLI binary can be overridden with
#' `options(rtemis.cli = )`.
#'
#' @param file Character: Path to input JSON config file.
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
read_config <- function(file) {
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
  kind <- .detect_config_kind(xl)
  # Full-document schema validation via the `rtemis` CLI, when available.
  .validate_config_cli(file)
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


# %% .validate_config_cli ----
#' Validate a config file against its schema using the `rtemis` CLI
#'
#' Opportunistic full-document JSON Schema validation. Policy is read from
#' `getOption("rtemis.validate", "auto")`:
#' - `"auto"`: validate if the CLI is found, skip silently otherwise.
#' - `"always"`: validate, erroring if the CLI is missing.
#' - `"never"`: skip entirely.
#'
#' The CLI binary is resolved from `getOption("rtemis.cli", "rtemis")`. On a
#' validation failure the schema errors are surfaced via [rtemis.core::abort];
#' a CLI that is present but misbehaves degrades to a warning under `"auto"` so a
#' flaky binary never blocks a legitimate read.
#'
#' @param file Character: Path to the JSON config file (already sanitized).
#'
#' @return `invisible(NULL)`, called for its side effect (error on invalid).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.validate_config_cli <- function(file) {
  policy <- match.arg(
    getOption("rtemis.validate", "auto"),
    c("auto", "always", "never")
  )
  if (policy == "never") {
    return(invisible(NULL))
  }
  cli <- Sys.which(getOption("rtemis.cli", "rtemis"))[[1L]]
  if (!nzchar(cli)) {
    if (policy == "always") {
      rtemis.core::abort(
        "`rtemis` CLI not found but `options(rtemis.validate = \"always\")` ",
        "requires it for schema validation.",
        class = c("rtemis_dependency_error", "rtemis_runtime_error")
      )
    }
    return(invisible(NULL))
  }
  out <- suppressWarnings(
    system2(
      cli,
      c("validate", "--json", shQuote(file)),
      stdout = TRUE,
      stderr = FALSE
    )
  )
  report <- tryCatch(
    jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  result <- report[["results"]][[1L]]
  if (is.null(result)) {
    msg <- paste0("Could not parse `rtemis validate` output for ", file, ".")
    if (policy == "always") {
      rtemis.core::abort(
        msg,
        class = c("rtemis_runtime_error")
      )
    }
    rtemis.core::warn(msg, " Falling back to `setup_*` validation.")
    return(invisible(NULL))
  }
  if (!isTRUE(result[["valid"]])) {
    rtemis.core::abort(
      "Config failed schema validation against ",
      result[["schema"]],
      ":\n",
      paste0("  - ", unlist(result[["errors"]]), collapse = "\n"),
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis::.validate_config_cli
