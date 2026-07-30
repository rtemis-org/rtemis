# 060_Provenance.R
# ::rtemis::
# 2026- EDG rtemis.org

# The provenance block of a run record: what produced a result, when, and on
# which data. A record states the values a run used; this states the run itself,
# so a result found later is interpretable without the session that made it.
#
# Declared with the `prop_*` factories like every other config class, so its
# schema is generated rather than hand-written and a port reads it from the
# published contract. It is `$ref`d by each `<family>/v1/record.json` rather
# than restated in every one.
#
# Environment detail is *drawn from* `Supervised@session_info` and `@session`,
# not duplicated: those hold the full `sessionInfo()` and the run timeline, and
# stay on the saved object. Promoted here is only what a record needs to be
# read on its own -- the versions that decide whether a result is reproducible,
# and the timing that says what was run.

# %% RUN_OUTCOMES ----
# How a run ended. A canceled or failed run still gets a record: "all runs are
# observable" does not hold if only the successful ones are written down.
RUN_OUTCOMES <- c("completed", "failed", "canceled")


# %% Provenance ----
#' Provenance
#'
#' @description
#' What produced a run record: package and language versions, platform, timing,
#' how the run ended, and a `DataFingerprint` per dataset it used.
#'
#' @field rtemis_version Character: rtemis version that produced the record.
#' @field r_version Character: R version, as `R.version.string`.
#' @field platform Character: Platform the run executed on.
#' @field started,finished Character: ISO 8601 timestamps.
#' @field elapsed_seconds Numeric [0, Inf): Wall-clock duration.
#' @field outcome Character \{"completed", "failed", "canceled"\}: How the run
#'   ended.
#' @field data_training,data_validation,data_test `DataFingerprint`: Identity of
#'   each dataset used.
#'
#' @author EDG
#' @noRd
Provenance <- new_class(
  name = "Provenance",
  package = "rtemis",
  properties = list(
    # Versions decide reproducibility: the same config on a different rtemis
    # can resolve a default differently, and only the record can say which one
    # ran.
    rtemis_version = prop_string(
      "",
      description = "rtemis version that produced this record."
    ),
    r_version = prop_string(
      "",
      description = "R version, as reported by `R.version.string`."
    ),
    platform = prop_string(
      "",
      description = "Platform the run executed on."
    ),
    started = prop_string(
      "",
      description = "When the run started, ISO 8601."
    ),
    finished = prop_string(
      "",
      description = "When the run finished, ISO 8601."
    ),
    elapsed_seconds = prop_float(
      0,
      min = 0,
      description = "Wall-clock duration of the run, in seconds."
    ),
    outcome = prop_string(
      "completed",
      enum = RUN_OUTCOMES,
      description = "How the run ended: completed, failed, or canceled."
    ),
    # One fingerprint per dataset the run saw. A path is not identity -- the
    # file at a path can change -- so the record carries the hash, and
    # `present()` can tell a rerun on the same data from a rerun on different
    # data.
    data_training = NULL | DataFingerprint,
    data_validation = NULL | DataFingerprint,
    data_test = NULL | DataFingerprint
  )
) # /rtemis::Provenance
