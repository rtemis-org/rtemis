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
      id = "https://example.org/contracts/supervised/r/v1/schema.json",
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
  Restored <- JSONSchema_to_S7(schema, defaults = list(value = NULL))
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


test_that("schema-selected references preserve types and explicit wire identity", {
  property <- Supervised@properties[["hyperparameters"]]
  spec <- get_spec(property)
  schema <- prop_to_schema(property)
  declaration <- default_declarations(spec, schema, "/properties/value")
  restored <- schema_to_spec(
    schema,
    declarations = declaration,
    path = "/properties/value"
  )
  expect_identical(spec_fields(restored), spec_fields(spec))
  Holder <- new_class(
    "ImplementationHolder",
    properties = list(value = property)
  )
  config <- setup_LightGBM(num_leaves = 7L)
  holder <- Holder(value = config)
  wire <- S7_to_list(holder)
  identity <- "https://schema.rtemis.org/hyperparameters/r/v1/schema.json"
  expect_identical(wire[["value"]][["$schema"]], identity)
  expect_identical(wire[["value"]][["num_leaves"]], 7L)
  expect_identical(from_wire(wire, Holder)[["value"]]@num_leaves, 7L)
  expect_null(Holder()@value)
  expect_error(Holder(value = setup_KMeans()), "Hyperparameters")
  for (tag in list(NULL, "https://example.org/unknown/v1/schema.json", 1L)) {
    invalid <- wire
    invalid[["value"]]["$schema"] <- list(tag)
    expect_error(from_wire(invalid, Holder), "declared contract")
  }
  expect_error(
    prop_schema_choice(prop_string(), list(`test::Config` = identity)),
    "scalar reference"
  )
  expect_error(
    prop_schema_choice(
      prop_collection(Hyperparameters),
      list(`test::Config` = identity)
    ),
    "scalar reference"
  )
  expect_error(
    prop_schema_choice(
      prop_object(Hyperparameters),
      list(`test::Config` = "unknown")
    ),
    "schema_urls|schema URLs"
  )
  expect_error(
    prop_schema_choice(
      prop_object(Hyperparameters),
      list(`test::Config` = identity, `test::Other` = identity)
    ),
    "unique"
  )
  for (branch in schema[["oneOf"]][[2L]][["oneOf"]]) {
    expect_identical(unname(branch[["required"]]), I("$schema"))
  }
})


test_that("schema-selected resampled settings retain their inline variant set", {
  property <- SupervisedRes@properties[["hyperparameters"]]
  Holder <- new_class(
    "ImplementationSetHolder",
    properties = list(value = property)
  )
  variants <- HyperparametersSet(
    variants = list(small = setup_KNN(k = 3L), large = setup_KNN(k = 5L))
  )
  wire <- S7_to_list(Holder(value = variants))
  expect_named(wire[["value"]], "variants")
  restored <- from_wire(wire, Holder)[["value"]]
  expect_s7_class(restored, HyperparametersSet)
  expect_identical(names(restored@variants), c("small", "large"))
})


test_that("artifact decoding preserves omission and tracks explicit later assignments", {
  Config <- schema_class(
    "PresenceFixture",
    package = "rtemis",
    properties = list(
      count = prop_integer(3L, min = 1L),
      label = prop_string(NULL, nullable = TRUE)
    ),
    publication = SchemaPublication(description = "Presence fixture.")
  )
  id <- "https://schema.rtemis.org/presencefixture/r/v1/schema.json"
  schema <- S7_to_JSONSchema(Config, id = id)
  declarations <- unlist(
    lapply(names(Config@properties), function(nm) {
      default_declarations(
        get_spec(Config@properties[[nm]]),
        schema[["properties"]][[nm]],
        paste0("/properties/", nm)
      )
    }),
    recursive = FALSE
  )
  graph <- default_artifact_graph(
    stats::setNames(list(schema), id),
    list(
      format_version = 1L,
      declarations = stats::setNames(list(declarations), id)
    )
  )
  config <- graph[["decode"]](list(label = NULL), "rtemis::PresenceFixture")
  expect_identical(S7_to_list(config), list(label = NULL))
  expect_identical(default_wire_value(config), list(label = NULL))
  materialized <- result_walk(
    config,
    function(value, fields) value,
    native = TRUE
  )
  expect_identical(S7_to_list(materialized), list(label = NULL))
  config@count <- 3L
  expect_identical(S7_to_list(config), list(count = 3L, label = NULL))
  expect_error(config@count <- 0L, "at least|>=|minimum|must")
  expect_error(config@count <- "3", "integer")
  expect_error(
    default_artifact_graph(list(), list(list(format_version = 2L))),
    "Unsupported"
  )
  expect_error(
    default_artifact_graph(
      list(),
      list(
        list(format_version = 1L, declarations = list(same = list())),
        list(format_version = 1L, declarations = list(same = list()))
      )
    ),
    "overlapping"
  )
})
