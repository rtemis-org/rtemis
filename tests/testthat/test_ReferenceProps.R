# test_ReferenceProps.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("inline alternatives retain the caller's publication URL context", {
  catalog <- schema_catalog()
  for (record in c(FALSE, TRUE)) {
    urls <- schema_reference_urls(
      catalog,
      "https://example.org/contracts",
      record
    )
    schema <- S7_to_JSONSchema(
      SuperConfigPaths,
      id = "https://example.org/contracts/supervised/v1/schema.json",
      record = record,
      reference_urls = urls
    )
    variants <- schema[["properties"]][["hyperparameters"]][["allOf"]][[
      1L
    ]][["then"]][["properties"]][["variants"]]
    expect_identical(
      variants[["additionalProperties"]][["$ref"]],
      unname(urls["rtemis::Hyperparameters"])
    )
    expect_identical(
      schema[["properties"]][["hyperparameters"]][["allOf"]][[2L]][["then"]][[
        "$ref"
      ]],
      unname(urls["rtemis::Hyperparameters"])
    )
  }
})


test_that("reference declarations validate target identity and collection bounds", {
  Map <- S7::new_class(
    "ReferenceMap",
    properties = list(
      value = prop_collection(
        Hyperparameters,
        container = "map",
        min_items = 1L,
        max_items = 2L
      )
    )
  )
  x <- Map(value = list(first = setup_GLM()))
  expect_error(Map(value = list()), "at least 1")
  expect_error(Map(value = list(setup_GLM())), "names")
  expect_error(
    Map(value = list(first = setup_GLM(), first = setup_CART())),
    "names"
  )
  expect_error(Map(value = list(first = setup_KMeans())), "Hyperparameters")
  expect_error(
    Map(value = list(a = setup_GLM(), b = setup_GLM(), c = setup_GLM())),
    "at most 2"
  )
  expect_error(x@value <- list(first = setup_KMeans()), "Hyperparameters")
  expect_error(prop_collection(Hyperparameters, min_items = -1L), "min_items")
  expect_error(
    prop_collection(Hyperparameters, min_items = 2L, max_items = 1L),
    "max_items"
  )
  expect_error(
    prop_collection(Hyperparameters, container = "matrix"),
    "container"
  )
  expect_error(prop_object(S7::class_list), "package identity")
})


test_that("null, empty maps and empty arrays retain their declared wire types", {
  Collections <- S7::new_class(
    "ReferenceCollections",
    properties = list(
      mapping = prop_collection(Diagnostic, container = "map"),
      sequence = prop_collection(Diagnostic),
      optional = prop_object(Diagnostic, nullable = TRUE)
    )
  )
  x <- Collections(mapping = list(), sequence = list())
  json <- jsonlite::toJSON(S7_to_list(x), auto_unbox = TRUE, null = "null")
  expect_identical(
    as.character(json),
    '{"mapping":{},"sequence":[],"optional":null}'
  )
  record <- config_record(x, x)
  expect_false("origin" %in% names(record))
  expect_true("optional" %in% names(record))
  expect_null(record[["optional"]])
  expect_identical(names(record[["mapping"]]), character())
  expect_null(names(record[["sequence"]]))
})


test_that("reference constraints survive reconstruction from serialized schema", {
  Declared <- S7::new_class(
    "ReferenceRoundtrip",
    properties = list(
      value = prop_collection(
        Hyperparameters,
        container = "map",
        nullable = TRUE,
        min_items = 1L,
        max_items = 2L,
        description = "Named alternatives."
      )
    )
  )
  schema <- S7_to_JSONSchema(
    Declared,
    id = "https://example.org/reference/schema.json"
  )
  schema <- jsonlite::fromJSON(
    jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  Restored <- JSONSchema_to_S7(schema)
  expect_identical(
    spec_fields(get_spec(Declared@properties[["value"]])),
    spec_fields(get_spec(Restored@properties[["value"]]))
  )
  expect_null(Restored()@value)
  expect_s7_class(Restored(value = list(a = setup_GLM())), Restored)
  expect_error(Restored(value = list()), "at least 1")
  expect_error(Restored(value = list(a = setup_KMeans())), "Hyperparameters")
})


test_that("reference discovery rejects unpublished targets without publishing them", {
  Unpublished <- S7::new_class("UnpublishedTarget", package = "rtemis")
  Holder <- S7::new_class(
    "ReferenceHolder",
    properties = list(
      target = prop_object(Unpublished, nullable = TRUE)
    )
  )
  expect_error(
    S7_to_JSONSchema(Holder, id = "https://example.org/holder/schema.json"),
    "unpublished class rtemis::UnpublishedTarget",
    class = "rtemis_schema_error"
  )
})


test_that("report collections serialize and validate their actual element types", {
  finding <- new_diagnostic(
    code = "SCHEMA_INVALID",
    severity = "error",
    message = "Invalid input."
  )
  report <- Diagnostics(list(finding))
  wire <- record_object(report)
  expect_identical(wire[["diagnostics"]][[1L]][["code"]], finding@code)
  expect_error(Diagnostics(list(setup_GLM())), "Diagnostic")
  expect_error(report@diagnostics <- list(setup_GLM()), "Diagnostic")
  expect_identical(
    get_spec(RegressionMetricsRes@properties[["res_metrics"]])@target_class,
    "rtemis::RegressionMetrics"
  )
  expect_identical(
    get_spec(ClassificationMetricsRes@properties[["res_metrics"]])@target_class,
    "rtemis::ClassificationMetrics"
  )
})
# %% Portable library identities ----
test_that("library names use the same portable grammar in R and JSON Schema", {
  spec <- get_spec(MetaLearnerHyperparameters@properties[["base_learners"]])
  schema <- spec_to_schema(spec, reference = "https://example.org/learner.json")
  # Isolate the key predicate; full member documents are covered by schema-graph.
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(
      list(type = "object", propertyNames = schema[["propertyNames"]]),
      auto_unbox = TRUE
    ),
    engine = "ajv"
  )
  valid <- c(
    "glm_main",
    "glmnet_sparse",
    "ranger_500",
    "lightgbm.depth4",
    ".model",
    "Model1",
    "if_model"
  )
  invalid <- c(
    "random forest",
    "1st_model",
    "r\u00e9gression",
    "if",
    "for",
    "TRUE",
    "NA_real_",
    ".5model",
    "model\n",
    "model/model"
  )
  for (name in c(valid, invalid)) {
    expected <- name %in% valid
    library <- stats::setNames(
      list(setup_GLM(), setup_CART()),
      c(name, "other")
    )
    expect_identical(
      is.null(validate_reference_value(library, spec_fields(spec))),
      expected,
      info = name
    )
    document <- stats::setNames(list(NULL, NULL), c(name, "other"))
    expect_identical(
      isTRUE(validate(jsonlite::toJSON(
        document,
        auto_unbox = TRUE,
        null = "null"
      ))),
      expected,
      info = name
    )
  }
  restored <- schema_to_spec(jsonlite::fromJSON(
    jsonlite::toJSON(schema, auto_unbox = TRUE),
    simplifyVector = FALSE
  ))
  expect_identical(restored@key_pattern, spec@key_pattern)
  expect_identical(restored@key_not_pattern, spec@key_not_pattern)
})
