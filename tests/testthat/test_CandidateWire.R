# test_CandidateWire.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("non-tunable maps preserve a sole candidates key", {
  property <- PreprocessorConfig@properties[["scale_centers"]]
  schema <- prop_to_schema(property)
  for (number in c(-2, 0, 1.5)) {
    value <- list(candidates = number)
    expect_true(jsonvalidate::json_validate(
      jsonlite::toJSON(value, auto_unbox = TRUE),
      jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
      engine = "ajv"
    ))
    restored <- .list_to_PreprocessorConfig(list(
      scale = TRUE,
      scale_centers = value
    ))
    expect_identical(restored@scale_centers, c(candidates = number))
    expect_identical(default_from_wire(value, schema), c(candidates = number))
  }
  expect_error(.list_to_PreprocessorConfig(list(
    scale = TRUE,
    scale_centers = list(candidates = "invalid")
  )))
  bag <- prop_bag()
  value <- list(candidates = list(1, 2))
  expect_identical(default_from_wire(value, prop_to_schema(bag)), value)
})


test_that("candidate-named map defaults reconstruct and retain record origins", {
  spec <- get_spec(prop_map(prop_float(0), nullable = TRUE))
  spec@default <- c(candidates = 1.5)
  property <- make_prop(spec)
  schema <- prop_to_schema(property)
  declarations <- default_declarations(spec, schema, "/properties/weights")
  declarations <- jsonlite::fromJSON(
    jsonlite::toJSON(declarations, auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  restored <- schema_to_spec(
    schema,
    declarations = declarations,
    path = "/properties/weights"
  )
  expect_identical(restored@default, c(candidates = 1.5))
  input <- list(candidates = 1.5)
  resolved <- list(candidates = 2.5)
  expect_identical(value_origin(input, resolved, spec), "derived")
  cfg <- setup_Preprocessor(scale = TRUE, scale_centers = c(candidates = 1.5))
  result <- cfg
  result@scale_centers <- c(candidates = 2.5)
  expect_identical(
    config_record(cfg, result)[["origin"]][["scale_centers"]],
    "derived"
  )
})


test_that("declared tunable scalar and vector domains still decode", {
  cls <- schema_class(
    "CandidateWireConfig",
    properties = list(
      scalar = prop_float(1, tunable = TRUE),
      vector = prop_integer(c(1L, 2L), vector = TRUE, tunable = TRUE)
    )
  )
  values <- list(
    scalar = tune_over(1, 2),
    vector = tune_over(c(1L, 2L), c(3L, 4L))
  )
  for (nm in names(values)) {
    property <- cls@properties[[nm]]
    wire <- jsonlite::fromJSON(
      jsonlite::toJSON(wire_value(values[[nm]], property), auto_unbox = TRUE),
      simplifyVector = FALSE
    )
    decoded <- from_wire(stats::setNames(list(wire), nm), cls)[[nm]]
    expect_s7_class(decoded, HyperparameterCandidates)
    expect_identical(decoded@candidates, values[[nm]]@candidates)
    expect_no_error(do.call(cls, stats::setNames(list(decoded), nm)))
    restored <- default_from_wire(wire, prop_to_schema(property))
    expect_identical(restored@candidates, values[[nm]]@candidates)
    expect_identical(value_origin(wire, 1, get_spec(property)), "tuned")
  }
  fixed <- schema_class("FixedWireConfig", properties = list(x = prop_float(1)))
  input <- from_wire(list(x = list(candidates = list(1, 2))), fixed)
  expect_error(do.call(fixed, input))
  expect_error(from_wire(list(scalar = list(candidates = list(1, "bad"))), cls))
  expect_error(from_wire(
    list(vector = list(candidates = list(c(1L, 2L), c(3.2, 4)))),
    cls
  ))
})


test_that("map defaults retain scalar storage types and nested values", {
  for (entry in list(
    list(property = prop_integer(0L), value = c(candidates = 2L)),
    list(property = prop_boolean(FALSE), value = c(candidates = TRUE)),
    list(property = prop_string(""), value = c(candidates = "value")),
    list(
      property = prop_integer(c(1L, 2L), vector = TRUE),
      value = list(candidates = c(3L, 4L))
    )
  )) {
    spec <- get_spec(prop_map(entry[["property"]], nullable = TRUE))
    spec@default <- entry[["value"]]
    schema <- spec_to_schema(spec)
    wire <- jsonlite::fromJSON(
      jsonlite::toJSON(
        default_wire_value(spec@default, spec_fields(spec)),
        auto_unbox = TRUE
      ),
      simplifyVector = FALSE
    )
    expect_identical(default_from_wire(wire, schema), entry[["value"]])
  }
})
