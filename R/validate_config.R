# validate_config.R
# ::rtemis::
# 2026- EDG rtemis.org

# Validating a config is two questions, and they are asked in that order:
#
#   1. Is this a config at all? -- answered by reconstructing it. `read_config()`
#      already validates by construction: `check_wire_keys()` rejects an unknown
#      key at every level and each `setup_*` builds an S7 object whose
#      properties enforce their declared types, bounds, enums and arities. So
#      the schema half of validation is that reconstruction wrapped, not a
#      second validator beside it. Anything else would be a copy of a contract
#      rtemis already publishes, free to drift.
#
#   2. Is it the right config for *this* data? -- answered by the checks in
#      `validate_data.R`, which the schema question gates: a config that will
#      not reconstruct has no fields to check anything against, so a
#      `SCHEMA_INVALID` finding is returned alone rather than followed by seven
#      findings derived from a half-read document.
#
# The output is a `Diagnostics`, empty when there is nothing to say. It reports
# rather than throws, because the caller is usually assembling a plan and wants
# every problem at once, not the first one.

# %% validate_config ----
#' Validate an rtemis config, against the schema and optionally against data
#'
#' Checks a config document -- as parsed from JSON, or already resolved -- and
#' reports what is wrong with it. With no `data`, only the schema is checked:
#' whether the document reconstructs into a valid rtemis config object. With
#' `data`, the config is additionally checked against the dataset it would run
#' on, catching the errors a schema cannot see: an outcome column that is not
#' there, more folds than the rarer class has cases, predictors that never vary.
#'
#' Nothing is thrown. Every finding is a `Diagnostic` carrying a stable `code`,
#' a `severity`, a technical `message`, a plain-language `plain` text, the
#' `evidence` behind it, and -- where a deterministic one exists -- a `fix`, an
#' RFC 6902 JSON Patch that resolves it. An empty result means the config is
#' clean.
#'
#' Each data check runs only when the config carries the part it is about: the
#' resampling checks need a resampler, the outcome checks need an outcome. A
#' config carrying neither is checked for what it does declare and reports
#' nothing about what it does not, so a preprocessing step and a training step
#' each get the checks that apply to them.
#'
#' @param config Named list parsed from a config document (carrying its
#'   `$schema`), or a resolved rtemis config object. A resolved object is valid
#'   by construction, so the schema check is skipped for it.
#' @param data Optional tabular data: The dataset the config would run on. NULL
#'   checks the schema only.
#' @param outcome Optional Character: Name of the outcome column in `data`.
#'   NULL takes rtemis's convention, the last column -- but only for a config
#'   that designates an outcome at all; a decomposition or clustering config has
#'   no outcome and every column is a feature.
#' @param step Optional Integer [1, Inf): Position of this config in the plan it
#'   came from, recorded on every finding. NULL when the config stands alone.
#'
#' @return `Diagnostics` object. Empty when nothing is wrong.
#'
#' @author EDG
#' @export
#' @examples
#' # A config that does not reconstruct: one finding, and no data checks.
#' bad <- list(
#'   `$schema` = "https://schema.rtemis.org/supervised/v1/schema.json",
#'   n_foldz = 10L
#' )
#' validate_config(bad)
#'
#' # A resolved config with nothing wrong: an empty Diagnostics.
#' validate_config(setup_SuperConfig(hyperparameters = setup_LightRF()))
validate_config <- function(
  config,
  data = NULL,
  outcome = NULL,
  step = NULL
) {
  check_character(outcome, allow_null = TRUE)
  if (!is.null(step)) {
    step <- clean_int(step)
  }

  # Schema ----
  resolved <- if (S7_inherits(config)) {
    # Already an rtemis config object: its properties were validated when it was
    # built, so there is no document left to check.
    config
  } else {
    if (!is.list(config)) {
      rtemis.core::abort(
        "`config` must be a named list parsed from a config document, or a resolved rtemis config object.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    # `abort()` echoes the error to the console before throwing it. This call
    # reports rather than throws, so the echo is muffled: a finding returned in
    # a `Diagnostics` must not also arrive as console output.
    schema_result <- withCallingHandlers(
      tryCatch(.config_from_list(config), error = function(e) e),
      message = function(m) invokeRestart("muffleMessage")
    )
    if (inherits(schema_result, "condition")) {
      return(Diagnostics(list(new_diagnostic(
        code = "SCHEMA_INVALID",
        severity = "error",
        message = conditionMessage(schema_result),
        evidence = list(
          `$schema` = config[["$schema"]] %||% NA_character_,
          condition_class = class(schema_result)[[1L]]
        ),
        step = step
      ))))
    }
    schema_result
  }

  # Data ----
  if (is.null(data)) {
    return(Diagnostics())
  }
  check_tabular(data)
  Diagnostics(validate_data(
    config = resolved,
    data = data,
    outcome = outcome,
    step = step
  ))
} # /rtemis::validate_config
