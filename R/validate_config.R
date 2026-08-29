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
  # A config that names its outcome answers the question, so the caller need
  # not. `outcome` stays an override -- checking a config against a frame whose
  # outcome differs is a real question -- but the default now comes from the
  # document rather than from the last-column convention. Without this,
  # `SuperConfig@outcome` would be decorative here while the Rust evaluator
  # reads it, and the two would disagree on the same input.
  if (is.null(outcome) && prop_exists(resolved, "outcome")) {
    outcome <- resolved@outcome
  }
  Diagnostics(validate_data(
    config = resolved,
    data = data,
    outcome = outcome,
    step = step
  ))
} # /rtemis::validate_config


# %% preflight_config ----
#' Check a training call's configuration against its data before it runs
#'
#' The pre-flight [train] performs when `preflight = TRUE`. The user who most
#' needs these checks is the one who does not know to ask for them: without it a
#' config that cannot work is discovered by the run failing, or -- worse -- by a
#' run that completes and reports a number computed on folds a class never
#' reached.
#'
#' Errors stop the run, because every one of them means the run either fails or
#' answers a different question than the caller asked. Warnings and notes are
#' reported and the run proceeds: a warning says the result is likely not what
#' was wanted, which is the caller's judgment to make, not this function's.
#'
#' The config is reassembled here because the inner [train] holds its
#' configuration as separate arguments rather than as one object. Every property
#' `config_parts()` reads has to be carried across, or this reports something
#' different from what `validate_config()` reports on the same run --
#' `positive_class` is one such property, being what `declared_task()` reads to
#' decide whether a config states a task.
#'
#' @param x tabular data: The training set.
#' @param preprocessor_config,decomposition_config,hyperparameters,tuner_config,outer_resampling_config
#'   The configuration objects [train] was given.
#' @param positive_class Optional character: The outcome level [train] was given
#'   as the positive class.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `NULL`, invisibly. Throws on any finding of severity "error".
#'
#' @author EDG
#' @keywords internal
#' @noRd
preflight_config <- function(
  x,
  preprocessor_config = NULL,
  decomposition_config = NULL,
  hyperparameters = NULL,
  tuner_config = NULL,
  outer_resampling_config = NULL,
  positive_class = NULL,
  verbosity = 1L
) {
  diagnostics <- validate_config(
    setup_SuperConfig(
      preprocessor_config = preprocessor_config,
      decomposition_config = decomposition_config,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      outer_resampling_config = outer_resampling_config,
      positive_class = positive_class,
      outdir = NULL,
      verbosity = 0L
    ),
    data = x
  )
  if (length(diagnostics) == 0L) {
    return(invisible(NULL))
  }
  found <- diagnostics@diagnostics
  errors <- Filter(function(d) d@severity == "error", found)
  if (length(errors) > 0L) {
    rtemis.core::abort(
      "Configuration cannot run on this data:\n",
      paste0(
        "  - ",
        vapply(
          errors,
          function(d) paste0(d@code, ": ", d@message),
          character(1L)
        ),
        collapse = "\n"
      ),
      "\nCall `validate_config()` for the full findings, or `preflight = FALSE` to skip this check.",
      class = c("rtemis_preflight_error", "rtemis_data_error")
    )
  }
  warnings <- Filter(function(d) d@severity == "warning", found)
  if (length(warnings) > 0L && verbosity > 0L) {
    for (d in warnings) {
      msg0(d@code, ": ", d@message)
    }
  }
  invisible(NULL)
} # /rtemis::preflight_config
