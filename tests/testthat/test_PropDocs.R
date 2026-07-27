# test_PropDocs.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# Tests for the property documentation audit (R/utils_audit_props.R). These
# exercise the audit machinery on seeded inputs; they deliberately do NOT gate
# the package on a clean audit of rtemis itself, which is a triage list rather
# than an invariant. See data-raw/audit_props.R for the runner.

# %% parse_doc_type ----
test_that("parse_doc_type() reads the documented grammar", {
  p <- parse_doc_type("Integer [1, Inf): Maximum depth.")
  expect_true(p[["parsed"]])
  expect_identical(p[["type"]], "Integer")
  expect_identical(p[["json_type"]], "integer")
  expect_identical(p[["minimum"]], 1)
  # Inf is not a JSON Schema bound; it means "no upper bound".
  expect_null(p[["maximum"]])
  expect_false(p[["nullable"]])
  expect_false(p[["tunable"]])
  expect_false(p[["vector"]])
})


test_that("parse_doc_type() reads the (Tunable) and Optional prefixes", {
  p <- parse_doc_type("(Tunable) Optional Integer [1, Inf): Features.")
  expect_true(p[["tunable"]])
  expect_true(p[["nullable"]])
  expect_identical(p[["json_type"]], "integer")

  # "Integer or NULL" is a non-standard spelling of "Optional Integer".
  q <- parse_doc_type("Integer or NULL: Seed.")
  expect_true(q[["nullable"]])
  expect_identical(q[["type"]], "Integer")
})


test_that("parse_doc_type() reads the trailing vector marker", {
  p <- parse_doc_type("Optional Numeric [0, Inf) vector: Weights.")
  expect_true(p[["vector"]])
  expect_true(p[["nullable"]])
  expect_identical(p[["json_type"]], "number")
  expect_identical(p[["minimum"]], 0)
})


test_that("parse_doc_type() returns unparsed for text with no declaration", {
  p <- parse_doc_type("Just a description with no type.")
  expect_false(p[["parsed"]])
  expect_true(is.na(p[["type"]]))
})


# %% Constraint parsing ----
test_that("parse_doc_type() distinguishes closed, open and half-open bounds", {
  closed <- parse_doc_type("Numeric [0, 1]: Rate.")
  expect_identical(closed[["minimum"]], 0)
  expect_identical(closed[["maximum"]], 1)
  expect_null(closed[["exclusive_minimum"]])
  expect_null(closed[["exclusive_maximum"]])

  open <- parse_doc_type("Numeric (0, 1): Rate.")
  expect_identical(open[["exclusive_minimum"]], 0)
  expect_identical(open[["exclusive_maximum"]], 1)
  expect_null(open[["minimum"]])
  expect_null(open[["maximum"]])

  half <- parse_doc_type("Numeric (0, 1]: Rate.")
  expect_identical(half[["exclusive_minimum"]], 0)
  expect_identical(half[["maximum"]], 1)
})


test_that("parse_doc_type() accepts roxygen-escaped brackets and braces", {
  p <- parse_doc_type("(Tunable) Numeric \\[0, 1\\]: Decay.")
  expect_identical(p[["minimum"]], 0)
  expect_identical(p[["maximum"]], 1)

  e <- parse_doc_type("Character \\{\"sparsemax\", \"entmax\"\\}: Mask type.")
  expect_identical(e[["enum"]], c("sparsemax", "entmax"))
  expect_identical(e[["json_type"]], "string")
})


# %% parse_roxygen_params ----
test_that("parse_roxygen_params() extracts params and splits shared entries", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#' Setup Demo",
      "#'",
      "#' @param n Integer [1, Inf): Count.",
      "#' @param min,max Numeric or NULL: Inclusive bounds.",
      "#' @param long Character: A description that",
      "#'   continues on the next line.",
      "#' @return Nothing.",
      "setup_Demo <- function(n, min, max, long) NULL"
    ),
    file.path(dir, "demo.R")
  )
  docs <- parse_roxygen_params(dir)
  expect_true("setup_Demo" %in% names(docs))
  params <- docs[["setup_Demo"]]
  expect_setequal(names(params), c("n", "min", "max", "long"))
  # A shared `@param min,max` entry applies to both names.
  expect_identical(params[["min"]], params[["max"]])
  # Continuation lines are appended.
  expect_match(params[["long"]], "continues on the next line")
  # `@return` ends the parameter block rather than continuing `long`.
  expect_false(grepl("Nothing", params[["long"]]))
})


test_that("parse_roxygen_params() reads class `@field` blocks too", {
  # Classes document their properties with `@field`, functions with `@param`;
  # both are the human-stated contract, so both are audited.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#' DemoConfig",
      "#' @field n Integer [1, Inf): Count.",
      "#' @field mode Character: Mode.",
      "DemoConfig <- new_class(",
      "  name = 'DemoConfig'",
      ")"
    ),
    file.path(dir, "DemoConfig.R")
  )
  docs <- parse_roxygen_params(dir)
  expect_true("DemoConfig" %in% names(docs))
  expect_setequal(names(docs[["DemoConfig"]]), c("n", "mode"))
  # The class's own name is a doc-source candidate, ranked last.
  sources <- doc_source_for_class("DemoConfig")
  expect_identical(sources[[length(sources)]], "DemoConfig")
})


test_that("parse_roxygen_params() ignores blocks not followed by a function", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#' Orphan block",
      "#' @param x Integer: Ignored.",
      "",
      "#' Real function",
      "#' @param y Integer: Kept.",
      "setup_Real <- function(y) NULL"
    ),
    file.path(dir, "orphan.R")
  )
  docs <- parse_roxygen_params(dir)
  expect_identical(names(docs), "setup_Real")
  expect_setequal(names(docs[["setup_Real"]]), "y")
})


# %% doc_source_for_class ----
test_that("doc_source_for_class() offers stripped and unstripped candidates", {
  expect_true(
    "setup_LightRF" %in% doc_source_for_class("LightRFHyperparameters")
  )
  # setup_ExecutionConfig keeps the suffix.
  expect_true(
    "setup_ExecutionConfig" %in% doc_source_for_class("ExecutionConfig")
  )
  # Resampler subclasses share one setup function.
  expect_true("setup_Resampler" %in% doc_source_for_class("KFoldConfig"))
  # No NA leaks in for a class with no family fallback.
  expect_false(anyNA(doc_source_for_class("PCAConfig")))
})


# %% audit_prop_docs ----
# A seeded class whose documentation agrees with its specs on every property
# except the ones each test targets.
AuditDemoConfig <- S7::new_class(
  name = "AuditDemoConfig",
  package = NULL,
  properties = list(
    n = prop_integer(1L, min = 1L, description = "Count."),
    mode = prop_string(
      "a",
      enum = c("a", "b"),
      description = "Mode."
    ),
    rate = prop_float(
      0.5,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Rate."
    )
  )
)


.audit_demo_dir <- function(param_lines) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(
    c(
      "#' Setup AuditDemo",
      "#'",
      param_lines,
      "setup_AuditDemo <- function() NULL"
    ),
    file.path(dir, "setup_AuditDemo.R")
  )
  dir
}


test_that("audit_prop_docs() reports nothing when docs match the specs", {
  dir <- .audit_demo_dir(c(
    "#' @param n Integer [1, Inf): Count.",
    "#' @param mode Character \\{\"a\", \"b\"\\}: Mode.",
    "#' @param rate (Tunable) Numeric \\[0, 1\\]: Rate."
  ))
  findings <- audit_prop_docs(
    dir,
    classes = list(AuditDemoConfig = AuditDemoConfig)
  )
  expect_identical(nrow(findings), 0L)
})


test_that("audit_prop_docs() detects each kind of disagreement", {
  dir <- .audit_demo_dir(c(
    # Wrong type, missing bound.
    "#' @param n Numeric: Count.",
    # Missing enum, wrongly marked Optional.
    "#' @param mode Optional Character: Mode.",
    # Missing (Tunable), wrong upper bound.
    "#' @param rate Numeric \\[0, 2\\]: Rate."
  ))
  findings <- audit_prop_docs(
    dir,
    classes = list(AuditDemoConfig = AuditDemoConfig)
  )
  checks <- split(findings[["check"]], findings[["property"]])
  expect_true("type" %in% checks[["n"]])
  expect_true("bounds" %in% checks[["n"]])
  expect_true("enum" %in% checks[["mode"]])
  expect_true("nullable" %in% checks[["mode"]])
  expect_true("tunable" %in% checks[["rate"]])
  expect_true("bounds" %in% checks[["rate"]])
  # Ordered by severity: type outranks bounds.
  expect_lt(
    min(findings[["severity"]][findings[["check"]] == "type"]),
    min(findings[["severity"]][findings[["check"]] == "bounds"])
  )
})


test_that("audit_prop_docs() reports an undocumented property", {
  dir <- .audit_demo_dir(c(
    "#' @param n Integer [1, Inf): Count.",
    "#' @param mode Character \\{\"a\", \"b\"\\}: Mode."
  ))
  findings <- audit_prop_docs(
    dir,
    classes = list(AuditDemoConfig = AuditDemoConfig)
  )
  rate <- findings[findings[["property"]] == "rate", , drop = FALSE]
  expect_identical(rate[["check"]], "undocumented")
})


test_that("audit_prop_docs() reports a class with no documentation source once", {
  dir <- withr::local_tempdir()
  writeLines("# no roxygen here", file.path(dir, "empty.R"))
  findings <- audit_prop_docs(
    dir,
    classes = list(AuditDemoConfig = AuditDemoConfig)
  )
  expect_identical(nrow(findings), 1L)
  expect_identical(findings[["check"]], "no_doc_source")
  expect_identical(findings[["property"]], "(class)")
})


test_that("audit_prop_docs() flags a missing description", {
  Bare <- S7::new_class(
    name = "BareConfig",
    package = NULL,
    properties = list(n = prop_integer(1L, min = 1L))
  )
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#' Setup Bare",
      "#' @param n Integer [1, Inf): Count.",
      "setup_Bare <- function() NULL"
    ),
    file.path(dir, "setup_Bare.R")
  )
  findings <- audit_prop_docs(dir, classes = list(BareConfig = Bare))
  expect_true("description" %in% findings[["check"]])
})


test_that("audit_prop_docs() errors on a missing directory", {
  expect_error(
    audit_prop_docs(file.path(tempdir(), "does-not-exist-rtemis")),
    class = "rtemis_value_error"
  )
})


# %% resolve_doc_delegation ----
test_that("resolve_doc_delegation() follows a See [setup_X] cross-reference", {
  docs <- list(
    setup_Target = c(n = "Integer [1, Inf): Count."),
    setup_Wrapper = c(n = "See [setup_Target].")
  )
  expect_identical(
    resolve_doc_delegation("See [setup_Target].", "n", docs),
    "Integer [1, Inf): Count."
  )
  # Not a delegation: returned unchanged.
  expect_identical(
    resolve_doc_delegation("Integer: Count.", "n", docs),
    "Integer: Count."
  )
  # Target exists but does not document this parameter: unchanged.
  expect_identical(
    resolve_doc_delegation("See [setup_Target].", "other", docs),
    "See [setup_Target]."
  )
  # Target does not exist: unchanged.
  expect_identical(
    resolve_doc_delegation("See [setup_Missing].", "n", docs),
    "See [setup_Missing]."
  )
})


# %% Property/argument name aliases ----
test_that("audit_prop_docs() resolves a property documented under an alias", {
  # Empty by design: the only entry this existed for was retired by renaming
  # `ResamplerConfig@n` to `n_resamples` (2026-07-26). The mechanism is kept
  # and tested so a future genuine divergence has a home.
  expect_length(PROP_DOC_ALIASES, 0L)

  Aliased <- S7::new_class(
    name = "AliasedConfig",
    package = NULL,
    properties = list(
      n = prop_integer(1L, min = 1L, nullable = TRUE, description = "Count.")
    )
  )
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#' Setup Aliased",
      "#' @param n_resamples Optional Integer [1, Inf): Count.",
      "setup_Aliased <- function() NULL"
    ),
    file.path(dir, "setup_Aliased.R")
  )
  findings <- audit_prop_docs(
    dir,
    classes = list(AliasedConfig = Aliased),
    aliases = c(n = "n_resamples")
  )
  expect_identical(nrow(findings), 0L)

  # Without the alias the property reads as undocumented.
  bare <- audit_prop_docs(dir, classes = list(AliasedConfig = Aliased))
  expect_identical(bare[["check"]], "undocumented")
})


# %% Package gate ----
test_that("no rtemis property documentation drifts from its PropertySpec", {
  # The audit's own acceptance test: every prop_*-declared property's roxygen
  # `@param` declaration must agree with its PropertySpec. Skipped when the
  # package sources are unavailable (R CMD check runs tests from a directory
  # that has no R/).
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package R/ sources not available")
  findings <- audit_prop_docs(r_dir)
  if (nrow(findings) > 0L) {
    detail <- paste0(
      findings[["class"]],
      "@",
      findings[["property"]],
      " [",
      findings[["check"]],
      "] documented: ",
      findings[["documented"]],
      " | declared: ",
      findings[["declared"]],
      collapse = "\n"
    )
    fail(paste0(
      "Property documentation drift (run `Rscript data-raw/audit_props.R`):\n",
      detail
    ))
  } else {
    succeed()
  }
})


# %% spec_classes ----
test_that("spec_classes() finds the migrated config classes", {
  classes <- spec_classes()
  expect_true(length(classes) > 20L)
  for (nm in c(
    "LightRFHyperparameters",
    "PreprocessorConfig",
    "PCAConfig",
    "KFoldConfig"
  )) {
    expect_true(nm %in% names(classes), info = nm)
  }
})
