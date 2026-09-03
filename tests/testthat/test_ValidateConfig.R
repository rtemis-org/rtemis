# test_ValidateConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# The schema half of `validate_config()`, and the two classes it reports
# through. The data half's fixture corpus lives in
# `test_ValidateConfigFixtures.R`.

.supervised_schema <- "https://schema.rtemis.org/supervised/v1/schema.json"

.min_config <- function(...) {
  c(
    list(
      `$schema` = .supervised_schema,
      hyperparameters = list(algorithm = "LightRF")
    ),
    list(...)
  )
}


# %% Diagnostic / Diagnostics ----
test_that("every code carries an authored plain text", {
  expect_setequal(names(DIAGNOSTIC_PLAIN), DIAGNOSTIC_CODES)
  expect_true(all(nzchar(DIAGNOSTIC_PLAIN)))
  # Plain text is for a reader with no statistics background, so it must not
  # reach for the vocabulary the technical `message` uses.
  jargon <- c(
    "n_resamples",
    "outer_resampling_config",
    "preprocessor_config",
    "hyperparameters",
    "NULL",
    "NA"
  )
  for (term in jargon) {
    expect_false(
      any(grepl(term, DIAGNOSTIC_PLAIN, fixed = TRUE)),
      info = paste0("`", term, "` appears in a plain-language text")
    )
  }
})


test_that("Diagnostic rejects an unknown code and an unknown severity", {
  expect_error(Diagnostic(code = "NOT_A_CODE"), "must be one of")
  expect_error(
    Diagnostic(code = "DIM_P_GT_N", severity = "fatal"),
    "must be one of"
  )
})


test_that("new_diagnostic() takes plain text from the code", {
  d <- new_diagnostic(
    code = "FEATURE_CONSTANT",
    severity = "warning",
    message = "technical",
    evidence = list(features = "a")
  )
  expect_s7_class(d, Diagnostic)
  expect_identical(d@plain, unname(DIAGNOSTIC_PLAIN[["FEATURE_CONSTANT"]]))
  expect_identical(d@message, "technical")
  expect_null(d@step)
  expect_null(d@fix)
})


test_that("Diagnostic@fix accepts an RFC 6902 patch and defaults to NULL", {
  d <- new_diagnostic(
    code = "RESAMPLE_MIN_CLASS",
    severity = "error",
    message = "m",
    fix = list(list(op = "replace", path = "/a/n_resamples", value = 3L))
  )
  expect_length(d@fix, 1L)
  expect_identical(d@fix[[1L]][["op"]], "replace")
  expect_null(Diagnostic()@fix)
})


test_that("Diagnostics holds only Diagnostic objects", {
  expect_length(Diagnostics(), 0L)
  expect_error(Diagnostics(list("not a diagnostic")))
  d <- new_diagnostic("DIM_P_GT_N", "warning", "m")
  ds <- Diagnostics(list(d))
  expect_length(ds, 1L)
  expect_identical(ds[[1L]]@code, "DIM_P_GT_N")
  expect_identical(diagnostic_codes(ds), "DIM_P_GT_N")
  expect_false(has_errors(ds))
  expect_true(has_errors(Diagnostics(list(
    new_diagnostic("OUTCOME_MISSING", "error", "m")
  ))))
})


test_that("Diagnostics prints clean and dirty states", {
  expect_match(repr(Diagnostics()), "No problems found", fixed = TRUE)
  ds <- Diagnostics(list(new_diagnostic("DIM_P_GT_N", "warning", "wide")))
  expect_match(repr(ds), "DIM_P_GT_N", fixed = TRUE)
  expect_match(repr(ds), "wide", fixed = TRUE)
})


test_that("to_json() emits a Diagnostics the wire can carry", {
  ds <- Diagnostics(list(new_diagnostic(
    "FEATURE_CONSTANT",
    "warning",
    "m",
    evidence = list(features = "a"),
    fix = list(list(op = "add", path = "/x", value = list("a")))
  )))
  j <- to_json(ds)
  # Exactly what `diagnostics/v1` declares, and nothing else: the schema is
  # `additionalProperties: false`, so an extra key makes the wire document
  # invalid against the contract the server publishes it under.
  expect_identical(names(j), "diagnostics")
  expect_length(j[["diagnostics"]], 1L)
  expect_identical(j[["diagnostics"]][[1L]][["code"]], "FEATURE_CONSTANT")
  expect_identical(j[["diagnostics"]][[1L]][["evidence"]][["features"]], "a")
  # An empty Diagnostics must serialize as an empty array, not as an object.
  expect_identical(
    as.character(jsonlite::toJSON(to_json(Diagnostics())[["diagnostics"]])),
    "[]"
  )
})


# %% validate_config(): schema half ----
test_that("a clean config document validates to an empty Diagnostics", {
  out <- validate_config(.min_config())
  expect_s7_class(out, Diagnostics)
  expect_length(out, 0L)
})


test_that("an unknown key is reported as SCHEMA_INVALID", {
  out <- validate_config(.min_config(n_foldz = 10L))
  expect_length(out, 1L)
  expect_identical(out[[1L]]@code, "SCHEMA_INVALID")
  expect_identical(out[[1L]]@severity, "error")
  # The condition's own message is carried through rather than replaced, so the
  # near-miss hint `check_wire_keys()` produces survives.
  expect_match(out[[1L]]@message, "n_foldz", fixed = TRUE)
  expect_identical(out[[1L]]@evidence[["$schema"]], .supervised_schema)
})


test_that("a missing or unrecognized $schema is reported as SCHEMA_INVALID", {
  expect_identical(
    validate_config(list(verbosity = 1L))[[1L]]@code,
    "SCHEMA_INVALID"
  )
  expect_identical(
    validate_config(list(`$schema` = "https://example.com/x.json"))[[1L]]@code,
    "SCHEMA_INVALID"
  )
})


test_that("a run record is reported as SCHEMA_INVALID, and named as a record", {
  out <- validate_config(list(
    `$schema` = "https://schema.rtemis.org/supervised/v1/record.json"
  ))
  expect_identical(out[[1L]]@code, "SCHEMA_INVALID")
  expect_match(out[[1L]]@message, "record", fixed = TRUE)
})


test_that("an out-of-bounds value is reported as SCHEMA_INVALID", {
  out <- validate_config(.min_config(
    outer_resampling_config = list(type = "KFold", n_resamples = 0L)
  ))
  expect_length(out, 1L)
  expect_identical(out[[1L]]@code, "SCHEMA_INVALID")
})


test_that("a resolved config object skips the schema check", {
  cfg <- setup_SuperConfig(hyperparameters = setup_LightRF())
  expect_length(validate_config(cfg), 0L)
})


test_that("validate_config() records the plan step on every finding", {
  out <- validate_config(.min_config(n_foldz = 1L), step = 3L)
  expect_identical(out[[1L]]@step, 3L)
})


test_that("validate_config() rejects a config that is neither list nor object", {
  expect_error(validate_config("a string"), class = "rtemis_type_error")
})


# %% Operations a supervised config cannot express ----
# The type carries the rule, so a supervised document naming one of these does
# not reconstruct: the answer is the schema half, which is why these live here
# rather than with the data-check fixtures. No `data` is needed -- a config that
# will not reconstruct has no fields for a data check to read.

test_that("each excluded operation is an unknown key in a supervised config", {
  values <- list(
    complete_cases = TRUE,
    remove_duplicates = TRUE,
    remove_cases_thres = 0.5,
    remove_features_thres = 0.9
  )
  for (op in names(values)) {
    out <- validate_config(.min_config(preprocessor_config = values[op]))
    expect_length(out, 1L)
    expect_identical(out[[1L]]@code, "SCHEMA_INVALID", info = op)
    expect_match(out[[1L]]@message, op, fixed = TRUE, info = op)
  }
})


test_that("the excluded operations remain valid for a standalone preprocessor", {
  # `preprocess()` supports every one of them; only a run that fits one cannot.
  expect_length(
    validate_config(list(
      `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      remove_duplicates = TRUE,
      remove_cases_thres = 0.5
    )),
    0L
  )
})
