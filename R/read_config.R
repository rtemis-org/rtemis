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
#' A **run record** is rejected, and named as such. A record declares the same
#' fields as the config it came from, so it would read as one -- but every value
#' in it is resolved, including values a run derived from data this call has
#' never seen. Accepting it would silently pin settings the new call should
#' decide for itself. Records are written by [write_record] and identify
#' themselves with a `record.json` schema.
#'
#' Validation is entirely rtemis's own: `check_wire_keys()` rejects an unknown
#' key at every level of the document, and each `setup_*` then constructs an S7
#' object whose properties enforce their declared types, bounds, enums and
#' arities. Those properties are the same `PropertySpec`s the published JSON
#' Schemas are generated from, so reading a config in R applies the published
#' contract by construction -- there is nothing an external validator could add.
#'
#' @param file Character: Path to input JSON config file.
#'
#' @return A `SuperConfig`, `DecomposeConfig`, `ClusterConfig`,
#'   `DecompositionConfig`, or `ClusteringConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' # Create a SuperConfig object
#' x <- setup_SuperConfig(
#'   dat_training_path = "~/Data/iris.csv",
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
  switch(
    kind,
    supervised = .list_to_SuperConfig(xl),
    decompose = .list_to_DecomposeConfig(xl),
    cluster = .list_to_ClusterConfig(xl),
    decomposition = .list_to_DecompositionConfig(xl),
    clustering = .list_to_ClusteringConfig(xl),
    preprocessor = .list_to_PreprocessorConfig(xl),
    rtemis.core::abort(
      "Unknown config kind: ",
      kind,
      ". Expected 'supervised', 'decompose', 'cluster', 'decomposition', ",
      "'clustering', or 'preprocessor'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
} # /rtemis::read_config


# %% .drop_meta_keys ----
#' Drop JSON document metadata keys from a parsed config list
#'
#' Every schema.rtemis.org config declares a `$schema`, and so may a nested one:
#' the family schemas (preprocessor, execution, decomposition, clustering, ...)
#' all permit `$schema`, so a `preprocessor_config` lifted verbatim out of a
#' standalone preprocessor config file carries its own. Those `$`-prefixed keys
#' are document metadata, not `setup_*` arguments, so they are stripped before a
#' parsed list is forwarded via `do.call()`. Anything else unknown still reaches
#' the `setup_*` function and fails there, as it should.
#'
#' Exported for `rtemis.server`, which applies the same rule to the config
#' blocks it receives over the wire.
#'
#' @param x Named list parsed from JSON, or `NULL`.
#'
#' @return `x` without its `$`-prefixed elements.
#'
#' @author EDG
#' @keywords internal
#' @export
#' @noRd
#' @examples
#' .drop_meta_keys(list(`$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
#'                      remove_duplicates = TRUE))
.drop_meta_keys <- function(x) {
  if (is.null(names(x))) {
    return(x)
  }
  x[!startsWith(names(x), "$")]
} # /rtemis::.drop_meta_keys


# %% check_wire_keys ----
#' Reject wire keys a config does not declare
#'
#' Every `.list_to_*()` reconstructor calls this before building its object, so
#' a mistyped or stale key is named rather than dropped. Silence here is the
#' worst outcome: a config that looks accepted but trains something else.
#'
#' Some reconstructors already errored, because `do.call(setup_*, x)` rejects an
#' unused argument -- but with R's "unused argument (bogus = 1)", which names no
#' config and suggests nothing. Routing every family through one check makes the
#' message uniform and lets it point at the intended key.
#'
#' `$`-prefixed metadata is always allowed: it identifies the document, not a
#' field, and `.drop_meta_keys()` removes it downstream.
#'
#' @param x Named list parsed from the wire.
#' @param valid Character: Keys this config declares.
#' @param label Character: The config's name, for the error message.
#'
#' @return `x`, invisibly. Throws if any key is unknown.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_wire_keys <- function(x, valid, label) {
  nms <- names(x)
  if (is.null(nms)) {
    return(invisible(x))
  }
  unknown <- setdiff(nms[nzchar(nms)], c(valid, nms[startsWith(nms, "$")]))
  if (length(unknown) == 0L) {
    return(invisible(x))
  }
  rtemis.core::abort(
    "Unknown ",
    label,
    if (length(unknown) == 1L) " key: " else " keys: ",
    paste0("`", unknown, "`", nearest_hint(unknown, valid), collapse = ", "),
    ".",
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /rtemis::check_wire_keys


# %% nearest_hint ----
#' " (did you mean `x`?)" for each unknown key with a near-miss among `valid`
#'
#' A typo and a renamed field look identical to a reader of the error, so the
#' suggestion is what turns "unknown key" into a fix. Two things qualify as a
#' near miss: a small edit distance (`maxdept` -> `maxdepth`), and a prefix
#' relation, which is what an abbreviation or a rename-by-extension looks like
#' (`n` -> `n_resamples`, the historical resampler rename). Edit distance alone
#' misses that one, being 10 apart on a one-character key.
#'
#' A wrong-but-plausible name (`train_p` on a KFold) matches neither and gets no
#' misleading suggestion.
#'
#' @param unknown Character: The offending keys.
#' @param valid Character: Keys this config declares.
#'
#' @return Character, same length as `unknown`; empty strings where nothing is
#'   close enough.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nearest_hint <- function(unknown, valid) {
  if (length(valid) == 0L) {
    return(rep("", length(unknown)))
  }
  vapply(
    unknown,
    function(key) {
      prefix <- startsWith(valid, key) | startsWith(key, valid)
      if (any(prefix)) {
        # Shortest match: `n` should reach `n_resamples`, not a longer name that
        # merely shares the prefix.
        return(paste0(
          " (did you mean `",
          valid[prefix][[which.min(nchar(valid[prefix]))]],
          "`?)"
        ))
      }
      d <- utils::adist(key, valid, ignore.case = TRUE)[1L, ]
      # A third of the key's length, so short names need a closer match than
      # long ones and an unrelated name never suggests anything.
      if (min(d) > max(1L, floor(nchar(key) / 3L))) {
        return("")
      }
      paste0(" (did you mean `", valid[[which.min(d)]], "`?)")
    },
    character(1L),
    USE.NAMES = FALSE
  )
} # /rtemis::nearest_hint
