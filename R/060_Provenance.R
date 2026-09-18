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


# %% Implementation ----
#' Implementation identity
#'
#' @description Package and language versions of the implementation that ran.
#' @field name Character: Implementation package name.
#' @field version Character: Implementation package version.
#' @field language Character: Lowercase language identifier.
#' @field language_version Character: Language runtime version.
#' @author EDG
#' @keywords internal
#' @noRd
Implementation <- schema_class(
  name = "Implementation",
  package = "rtemis",
  properties = list(
    name = prop_string(description = "Implementation package name."),
    version = prop_string(description = "Implementation package version."),
    language = prop_string(
      description = "Lowercase language identifier, such as r or python."
    ),
    language_version = prop_string(description = "Language runtime version.")
  ),
  publication = SchemaPublication(
    role = "document",
    slug = "implementation",
    title = "rtemis Implementation",
    description = "Package and language identity of the implementation that executed a run.",
    kind = "report",
    scope = "shared"
  )
)


# %% repr.Implementation ----
#' @keywords internal
#' @noRd
method(repr, Implementation) <- function(x, output_type = NULL, ...) {
  fmt(
    paste0(
      x@name,
      " ",
      x@version,
      " (",
      x@language,
      " ",
      x@language_version,
      ")"
    ),
    output_type = output_type
  )
}


# %% Provenance ----
#' Provenance
#'
#' @description
#' What produced a run record: package and language versions, platform, timing,
#' how the run ended, and a `DataFingerprint` per dataset it used.
#'
#' @field implementation `Implementation`: Package and language that executed the run.
#' @field platform Character: Platform the run executed on.
#' @field started,finished Character: ISO 8601 timestamps.
#' @field elapsed_seconds Numeric [0, Inf): Wall-clock duration.
#' @field outcome Character \{"completed", "failed", "canceled"\}: How the run
#'   ended.
#' @field data_training,data_validation,data_test Optional `DataFingerprint`: Identity of
#'   each dataset used.
#'
#' @author EDG
#' @noRd
Provenance <- schema_class(
  name = "Provenance",
  package = "rtemis",
  properties = list(
    # Versions decide reproducibility: the same config on a different rtemis
    # can resolve a default differently, and only the record can say which one
    # ran.
    implementation = prop_object(
      Implementation,
      description = "Package and language identity of the implementation that executed the run."
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
    data_training = prop_object(
      DataFingerprint,
      nullable = TRUE,
      description = "Identity of the training dataset."
    ),
    data_validation = prop_object(
      DataFingerprint,
      nullable = TRUE,
      description = "Identity of the validation dataset."
    ),
    data_test = prop_object(
      DataFingerprint,
      nullable = TRUE,
      description = "Identity of the test dataset."
    )
  ),
  publication = SchemaPublication(
    role = "document",
    slug = "provenance",
    title = "rtemis Provenance",
    description = "What produced a run record: package and language versions, platform, timing, how the run ended, and a fingerprint of each dataset used. Referenced by every `<family>/v1/record.json`.",
    order = 1L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::Provenance
