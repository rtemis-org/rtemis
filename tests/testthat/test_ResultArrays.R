# test_ResultArrays.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("matrix declarations preserve cell constraints and absent defaults", {
  property <- prop_matrix(
    items = prop_float(NULL, min = 0, max = 1, nullable = TRUE)
  )
  spec <- get_spec(property)
  expect_false(spec@default_present)
  schema <- prop_to_schema(property)
  declarations <- default_declarations(spec, schema, "/properties/value")
  expect_identical(declarations[["/properties/value"]][["kind"]], "none")
  expect_identical(
    declarations[["/properties/value/items/items"]][["kind"]],
    "literal"
  )
  restored <- schema_to_spec(
    schema,
    declarations = declarations,
    path = "/properties/value"
  )
  expect_identical(spec_fields(restored), spec_fields(spec))
  expect_equal(schema[["items"]][["items"]][["minimum"]], 0)
  expect_equal(schema[["items"]][["items"]][["maximum"]], 1)
  expect_setequal(
    as.character(schema[["items"]][["items"]][["type"]]),
    c("number", "null")
  )
  expect_error(prop_matrix(items = prop_string("")), "numeric scalar")
  expect_error(
    prop_matrix(items = prop_float(0, vector = TRUE)),
    "numeric scalar"
  )
  expect_error(
    prop_matrix(items = prop_float(0, tunable = TRUE)),
    "numeric scalar"
  )
  expect_error(prop_matrix(items = "number"), "prop_\\*")
})


test_that("numeric matrices enforce shape, bounds and independent cell nullability", {
  Demo <- new_class(
    "ResultMatrix",
    properties = list(
      probabilities = Classification@properties[["predicted_prob_training"]],
      finite = prop_matrix(nullable = TRUE)
    )
  )
  value <- matrix(c(0.25, NA_real_, 0.75, 1), nrow = 2L)
  expect_identical(Demo(probabilities = value)@probabilities, value)
  expect_error(Demo(finite = value), "missing")
  expect_error(Demo(finite = matrix(Inf)), "finite")
  expect_error(Demo(probabilities = matrix(Inf)), "finite")
  expect_error(Demo(probabilities = matrix("0.5")), "numeric matrix")
  expect_error(Demo(probabilities = matrix(TRUE)), "numeric matrix")
  expect_error(Demo(probabilities = matrix(-0.01)), ">= 0")
  expect_error(Demo(probabilities = matrix(1.01)), "<= 1")
  expect_error(
    Demo(probabilities = matrix(numeric(), nrow = 0L, ncol = 2L)),
    "row and one column"
  )
  Required <- new_class(
    "RequiredResultMatrix",
    properties = list(
      value = CalibratedClassification@properties[[
        "predicted_prob_training_calibrated"
      ]]
    )
  )
  expect_error(Required(), "explicit value")
  expect_error(Required(value = NULL))
  expect_identical(Required(value = matrix(0.5))@value, matrix(0.5))
})


test_that("wire arrays retain null positions and reject scalar type coercions", {
  Demo <- new_class(
    "ResultArray",
    properties = list(
      numbers = prop_array(prop_float(NULL, nullable = TRUE)),
      counts = prop_array(prop_integer(NULL, nullable = TRUE), nullable = TRUE),
      labels = prop_array(prop_string(NULL, nullable = TRUE), nullable = TRUE),
      flags = prop_array(prop_boolean(NULL, nullable = TRUE), nullable = TRUE),
      groups = prop_map(prop_float(NULL, nullable = TRUE), nullable = TRUE),
      matrix = prop_matrix(
        nullable = TRUE,
        items = prop_float(NULL, nullable = TRUE)
      )
    )
  )
  values <- list(
    numbers = list(NULL, 2.5, NULL),
    counts = list(1L, NULL, 3L),
    labels = list(NULL, "a", NULL),
    flags = list(TRUE, NULL, FALSE),
    groups = list(a = 1.5, b = NULL),
    matrix = list(list(0.2, NULL), list(NULL, 0.8))
  )
  decoded <- from_wire(values, Demo)
  expect_identical(decoded[["numbers"]], c(NA_real_, 2.5, NA_real_))
  expect_identical(decoded[["counts"]], c(1L, NA_integer_, 3L))
  expect_identical(decoded[["labels"]], c(NA_character_, "a", NA_character_))
  expect_identical(decoded[["flags"]], c(TRUE, NA, FALSE))
  expect_identical(decoded[["groups"]], c(a = 1.5, b = NA_real_))
  expect_identical(decoded[["matrix"]], matrix(c(0.2, NA, NA, 0.8), nrow = 2L))
  object <- do.call(Demo, decoded)
  wire <- jsonlite::toJSON(
    S7_to_list(object),
    auto_unbox = TRUE,
    na = "null",
    null = "null"
  )
  parsed <- jsonlite::fromJSON(wire, simplifyVector = FALSE)
  expect_identical(parsed, values)
  expect_no_error(do.call(Demo, from_wire(parsed, Demo)))
  for (bad in list(list(1, TRUE), list(1, "2"), list(list(1), 2))) {
    expect_error(from_wire(list(numbers = bad), Demo), "JSON type")
  }
  expect_error(from_wire(list(counts = list(1.5)), Demo), "JSON type")
  expect_error(
    from_wire(list(counts = list(2^31)), Demo),
    "native integer range"
  )
  expect_error(from_wire(list(numbers = list(a = 1)), Demo), "JSON object")
  expect_error(
    from_wire(list(matrix = list(a = list(1))), Demo),
    "array of rows"
  )
  expect_error(
    from_wire(list(matrix = list(list(a = 1))), Demo),
    "rows must be arrays"
  )
  expect_error(
    from_wire(list(matrix = list(list(1, 2), list(3))), Demo),
    "equal positive lengths"
  )
  expect_error(
    from_wire(list(matrix = list(1, 2)), Demo),
    "rows must be arrays"
  )
  expect_error(
    from_wire(list(matrix = list(list())), Demo),
    "equal positive lengths"
  )
  Single <- new_class(
    "SingleResultArray",
    properties = list(value = prop_array(prop_float(NULL, nullable = TRUE)))
  )
  single <- jsonlite::toJSON(
    S7_to_list(Single(value = NA_real_)),
    auto_unbox = TRUE,
    na = "null"
  )
  expect_identical(
    jsonlite::fromJSON(single, simplifyVector = FALSE)[["value"]],
    list(NULL)
  )
})
