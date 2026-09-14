# test_SchemaRules.R
# ::rtemis::
# 2026- EDG rtemis.org

testthat::skip_if_not_installed("jsonvalidate")


# %% .rule_schema_validator ----
.rule_schema_validator <- function(schema) {
  jsonvalidate::json_validator(
    jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
    engine = "ajv"
  )
}


# %% .rule_accepts ----
.rule_accepts <- function(validator, value) {
  isTRUE(validator(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )))
}


test_that("parallel dispatch rules agree across R, JSON Schema, and reconstruction", {
  schema <- S7_to_JSONSchema(
    FutureExecutionConfig,
    base = ExecutionConfig,
    id = "https://example.org/future/schema.json"
  )
  validator <- .rule_schema_validator(schema)
  restored <- JSONSchema_to_S7(
    schema,
    defaults = list(n_workers = 1L, future_plan = "mirai_multisession")
  )
  values <- list(NULL, 1L, 2L, 4L)
  for (outer in values) {
    for (tuning in values) {
      args <- list(n_workers_outer = outer, n_workers_tuning = tuning)
      expected <- !(isTRUE(outer > 1L) && isTRUE(tuning > 1L))
      original <- tryCatch(
        do.call(FutureExecutionConfig, args),
        error = identity
      )
      rebuilt <- tryCatch(do.call(restored, args), error = identity)
      expect_identical(!inherits(original, "error"), expected)
      expect_identical(!inherits(rebuilt, "error"), expected)
      expect_identical(.rule_accepts(validator, args), expected)
    }
  }
  expect_true(.rule_accepts(validator, list(n_workers_outer = 2L)))
  without <- schema
  without[["allOf"]] <- Filter(
    function(clause) {
      !identical(clause[["$comment"]], "execution.parallel-dispatch")
    },
    schema[["allOf"]]
  )
  if (!length(without[["allOf"]])) {
    without[["allOf"]] <- NULL
  }
  expect_true(.rule_accepts(
    .rule_schema_validator(without),
    list(n_workers_outer = 2L, n_workers_tuning = 2L)
  ))
  expect_identical(
    schema_rules(FutureExecutionConfig),
    schema_rules(MiraiExecutionConfig)
  )
})


test_that("the current Spectral restriction derives from its class declaration", {
  schema <- S7_to_JSONSchema(
    SpectralConfig,
    base = ClusteringConfig,
    id = "https://example.org/spectral/schema.json"
  )
  validator <- .rule_schema_validator(schema)
  for (kernel in c("rbf", "laplace", "rbf_local")) {
    for (nystrom in c(FALSE, TRUE)) {
      args <- list(kernel = kernel, nystrom = nystrom)
      expected <- !(kernel == "rbf_local" && nystrom)
      object <- tryCatch(do.call(SpectralConfig, args), error = identity)
      expect_identical(!inherits(object, "error"), expected)
      expect_identical(.rule_accepts(validator, args), expected)
    }
  }
  without <- schema
  without[["allOf"]] <- Filter(
    function(clause) {
      !identical(clause[["$comment"]], "spectral.local-kernel-nystrom")
    },
    schema[["allOf"]]
  )
  expect_true(.rule_accepts(
    .rule_schema_validator(without),
    list(kernel = "rbf_local", nystrom = TRUE)
  ))
  expect_true(.rule_accepts(validator, list(nystrom = TRUE)))
})


test_that("status and value pairing agrees at every measure and status boundary", {
  schema <- S7_to_JSONSchema(
    ClusteringMetrics,
    id = "https://example.org/metrics/schema.json",
    asserted = TRUE
  )
  validator <- .rule_schema_validator(schema)
  restored <- JSONSchema_to_S7(schema)
  base <- ClusteringMetrics()
  for (nm in names(CLUSTERING_MEASURES)) {
    for (status in CLUSTERING_MEASURE_STATUS) {
      for (present in c(FALSE, TRUE)) {
        values <- base@metrics
        statuses <- base@status
        statuses[[nm]] <- status
        if (present) {
          values[[nm]] <- if (is.integer(values[[nm]])) 1L else 0.5
        }
        expected <- identical(status == "computed", present)
        original <- tryCatch(
          {
            object <- base
            S7::props(object) <- list(metrics = values, status = statuses)
            object
          },
          error = identity
        )
        rebuilt <- tryCatch(
          restored(sample = base@sample, metrics = values, status = statuses),
          error = identity
        )
        wire <- record_object(base)
        wire[["metrics"]] <- values
        wire[["status"]] <- statuses
        label <- paste(nm, status, present)
        expect_identical(!inherits(original, "error"), expected, info = label)
        expect_identical(!inherits(rebuilt, "error"), expected, info = label)
        expect_identical(.rule_accepts(validator, wire), expected, info = label)
      }
    }
  }
  wire <- record_object(base)
  for (nm in c("metrics", "status")) {
    empty <- wire
    empty[[nm]] <- empty[[nm]][FALSE, , drop = FALSE]
    expect_false(.rule_accepts(validator, empty))
    duplicate <- wire
    duplicate[[nm]] <- rbind(duplicate[[nm]], duplicate[[nm]])
    expect_false(.rule_accepts(validator, duplicate))
    unset <- wire
    unset[nm] <- list(NULL)
    expect_true(.rule_accepts(validator, unset))
  }
  mutant <- wire
  mutant[["status"]][[1L]] <- "computed"
  expect_false(.rule_accepts(validator, mutant))
  without <- schema
  without[["allOf"]] <- Filter(
    function(clause) {
      !identical(clause[["$comment"]], "clustering-metrics.status-value")
    },
    schema[["allOf"]]
  )
  if (!length(without[["allOf"]])) {
    without[["allOf"]] <- NULL
  }
  expect_true(.rule_accepts(.rule_schema_validator(without), mutant))
})


test_that("invalid class-rule declarations fail before any instance exists", {
  expect_error(SchemaPredicate(property = "x"), "exactly one")
  expect_error(SchemaPredicate(property = "x", equals = NA), "scalar")
  expect_error(
    SchemaPredicate(property = "x", equals = TRUE, minimum = 2),
    "exactly one"
  )
  rule <- ForbidTogether(
    id = "test.forbid",
    message = "Choose one.",
    conditions = list(
      SchemaPredicate(property = "x", minimum = 2),
      SchemaPredicate(property = "missing", equals = TRUE)
    )
  )
  expect_error(
    schema_class(
      "BadRule",
      properties = list(x = prop_integer(1L)),
      rules = list(rule)
    ),
    "scalar properties"
  )
  rule <- StatusValueRule(
    id = "test.status",
    message = "Match status.",
    values = "values",
    statuses = "statuses"
  )
  expect_error(
    schema_class(
      "BadTables",
      properties = list(
        values = prop_table(
          list(x = prop_float(NULL, nullable = TRUE)),
          nullable = TRUE
        ),
        statuses = prop_table(
          list(
            x = prop_string("computed", enum = c("computed", "unsupported"))
          ),
          nullable = TRUE
        )
      ),
      rules = list(rule)
    ),
    "exactly one row"
  )
  expect_error(
    schema_rule_from_fields(list(kind = "arbitrary-code")),
    "Unsupported"
  )
})
