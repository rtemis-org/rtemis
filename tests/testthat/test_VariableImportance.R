# test_VariableImportance.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("variable importance declares typed named measures on its source class", {
  data <- data.table::data.table(
    variable = c("age", "weight"),
    Gain = c(0.75, NA_real_),
    Coefficient = c(-2, 0),
    `another measure` = c(Inf, 1)
  )
  result <- VariableImportance(data)
  expect_identical(result@data, data)
  expect_identical(
    schema_publication_annotation(VariableImportance)[["scope"]],
    "shared"
  )
  schema <- S7_to_JSONSchema(
    VariableImportance,
    id = "https://example.test/importance",
    asserted = TRUE
  )
  row <- schema[["properties"]][["data"]][["items"]]
  expect_identical(row[["minProperties"]], 2L)
  expect_identical(
    row[["additionalProperties"]][["type"]],
    I(c("number", "null"))
  )
  expect_null(get_spec(VariableImportance@properties[["data"]])@default)
  expect_false(
    get_spec(VariableImportance@properties[["data"]])@default_present
  )
  expect_error(VariableImportance(), "requires an explicit value")
  expect_error(
    VariableImportance(data.table::data.table(variable = "x")),
    "at least 2"
  )
  expect_error(
    VariableImportance(data.table::data.table(
      variable = character(),
      Gain = numeric()
    )),
    "at least 1"
  )
  expect_error(
    VariableImportance(data.table::data.table(
      variable = NA_character_,
      Gain = 1
    )),
    "missing"
  )
  expect_error(
    VariableImportance(data.table::data.table(variable = "x", Gain = "1")),
    "number"
  )
  expect_error(
    VariableImportance(data.table::data.table(variable = "x", Gain = TRUE)),
    "number"
  )
})


test_that("additional column declarations roundtrip without source class defaults", {
  schema <- S7_to_JSONSchema(
    VariableImportance,
    id = "https://example.test/importance",
    asserted = TRUE
  )
  spec <- get_spec(VariableImportance@properties[["data"]])
  declarations <- default_declarations(
    spec,
    schema[["properties"]][["data"]],
    "/properties/data"
  )
  expect_true(
    "/properties/data/items/additionalProperties" %in% names(declarations)
  )
  artifacts <- list(
    format_version = 1L,
    declarations = stats::setNames(list(declarations), schema[["$id"]])
  )
  graph <- default_artifact_graph(
    stats::setNames(list(schema), schema[["$id"]]),
    artifacts
  )
  Restored <- graph[["class"]](schema[["$id"]])
  expect_identical(
    spec_fields(get_spec(Restored@properties[["data"]])),
    spec_fields(spec)
  )
  expect_error(Restored(), "requires an explicit value")
  expect_error(
    Restored(data = data.frame(variable = "x", Gain = "invalid")),
    "number"
  )
  wire <- list(
    data = list(
      list(
        variable = "age",
        Gain = 0.75,
        Coefficient = -2,
        `another measure` = NULL
      ),
      list(
        variable = "weight",
        Gain = NULL,
        Coefficient = 0,
        `another measure` = 1
      )
    )
  )
  decoded <- do.call(Restored, from_wire(wire, Restored))
  expect_identical(
    names(decoded@data),
    c("variable", "Gain", "Coefficient", "another measure")
  )
  expect_identical(decoded@data[["Gain"]], c(0.75, NA_real_))
  expect_identical(decoded@data[["Coefficient"]], c(-2, 0))
  expect_identical(decoded@data[["another measure"]], c(NA_real_, 1))
  serialized <- jsonlite::fromJSON(
    jsonlite::toJSON(
      S7_to_list(decoded),
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ),
    simplifyVector = FALSE
  )
  expect_equal(serialized, wire)
})


test_that("typed additional members retain validation and defaults recursively", {
  property <- prop_struct(
    list(label = prop_string("value")),
    nullable = TRUE,
    additional = prop_array(prop_integer(2L, min = 1L), nullable = TRUE),
    min_members = 2L
  )
  spec <- get_spec(property)
  expect_identical(
    spec_fields(spec_object(spec_fields(spec))),
    spec_fields(spec)
  )
  Test <- S7::new_class(
    "AdditionalMembers",
    properties = list(payload = property)
  )
  expect_no_error(Test(payload = list(label = "value", counts = c(1L, 2L))))
  expect_error(Test(payload = list(label = "value")), "at least 2")
  expect_error(
    Test(payload = list(label = "value", counts = c(1, 2))),
    "integer"
  )
  expect_error(
    Test(payload = list(label = "value", counts = c(0L, 2L))),
    ">= 1"
  )
  expect_error(
    prop_table(
      list(x = prop_string()),
      additional = prop_array(prop_integer(1L), nullable = TRUE)
    ),
    "scalar"
  )
  expect_error(
    prop_struct(list(x = prop_string()), nullable = TRUE, min_members = 2L),
    "closed shape"
  )
  expect_error(
    prop_struct(list(x = prop_string()), nullable = TRUE, min_members = -1L),
    "non-negative"
  )
})


test_that("a LightGBM model without splits has unset variable importance", {
  skip_if_not_installed("lightgbm")
  dataset <- lightgbm::lgb.Dataset(
    matrix(1, nrow = 10L, ncol = 2L),
    label = seq_len(10L)
  )
  model <- lightgbm::lgb.train(
    params = list(objective = "regression", verbosity = -1L, num_threads = 1L),
    data = dataset,
    nrounds = 1L
  )
  expect_null(varimp_super(model))
})
