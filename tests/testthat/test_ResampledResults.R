# test_ResampledResults.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("nested result containers preserve types, names and missing positions", {
  Demo <- new_class(
    "NestedResultValues",
    properties = list(
      outcomes = prop_array(prop_factor(allow_missing = TRUE, nullable = TRUE)),
      indices = prop_map(prop_integer(min = 1L, vector = TRUE)),
      importance = prop_array(prop_object(VariableImportance, nullable = TRUE)),
      probabilities = prop_array(prop_matrix(
        nullable = TRUE,
        items = prop_float(NULL, min = 0, max = 1, nullable = TRUE)
      ))
    )
  )
  importance <- VariableImportance(data.frame(variable = "x", score = 0.5))
  obj <- Demo(
    outcomes = list(
      first = factor(c("b", NA), levels = c("b", "a")),
      missing = NULL
    ),
    indices = list(bootstrap = c(2L, 2L, 1L), singleton = 3L),
    importance = list(first = importance, missing = NULL),
    probabilities = list(
      first = matrix(c(0.5, NA_real_), ncol = 1L),
      missing = NULL
    )
  )
  document <- jsonlite::fromJSON(
    jsonlite::toJSON(
      S7_to_list(obj),
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ),
    simplifyVector = FALSE
  )
  expect_null(names(document[["outcomes"]]))
  expect_identical(document[["outcomes"]][[1L]][["levels"]], list("b", "a"))
  expect_identical(document[["outcomes"]][[1L]][["codes"]], list(1L, NULL))
  expect_identical(document[["outcomes"]][2L], list(NULL))
  expect_identical(
    document[["indices"]],
    list(bootstrap = list(2L, 2L, 1L), singleton = list(3L))
  )
  expect_identical(document[["importance"]][2L], list(NULL))
  expect_null(names(document[["importance"]]))
  expect_identical(
    document[["probabilities"]],
    list(list(list(0.5), list(NULL)), NULL)
  )
  for (values in list(list(TRUE), list(iris), list(list(data = 1)))) {
    expect_error({
      obj@importance <- values
    })
  }
  expect_error({
    obj@outcomes <- list(c("a", "b"))
  })
  expect_error({
    obj@probabilities <- list(matrix(1.1, nrow = 1L))
  })
  expect_error(
    {
      obj@indices <- list(c(1L, 2L))
    },
    "names"
  )
  expect_error(
    {
      obj@indices <- setNames(list(1L, 2L), c("same", "same"))
    },
    "names"
  )
  expect_error({
    obj@indices <- list(a = c(1L, NA_integer_))
  })
  expect_error({
    obj@indices <- list(a = c(0L, 1L))
  })
  # Wire reconstruction is checked separately from native assignment.
  restored <- from_wire(
    list(
      outcomes = document[["outcomes"]],
      indices = document[["indices"]],
      probabilities = document[["probabilities"]]
    ),
    Demo
  )
  expect_identical(restored[["outcomes"]], unname(obj@outcomes))
  expect_identical(restored[["indices"]], obj@indices)
  expect_identical(restored[["probabilities"]], unname(obj@probabilities))
})


test_that("required integer arrays and maps retain absent defaults", {
  for (property in list(
    prop_integer(min = 1L, vector = TRUE),
    prop_map(prop_integer(min = 1L, vector = TRUE))
  )) {
    spec <- get_spec(property)
    expect_false(spec@default_present)
    schema <- prop_to_schema(property)
    defaults <- default_declarations(spec, schema, "/properties/value")
    restored <- schema_to_spec(
      schema,
      declarations = defaults,
      path = "/properties/value"
    )
    expect_identical(spec_fields(restored), spec_fields(spec))
  }
})


test_that("typed resampling reports retain draw identity and bootstrap repeats", {
  config <- setup_Custom(
    resamples = list(first = c(1L, 1L, 2L), second = c(2L, 3L))
  )
  draw <- resample(1:4, config, verbosity = 0L)
  expect_s7_class(draw, Resampler)
  expect_identical(
    draw@resamples,
    list(first = c(1L, 1L, 2L), second = c(2L, 3L))
  )
  wire <- jsonlite::fromJSON(
    jsonlite::toJSON(S7_to_list(draw), auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  expect_identical(wire[["resamples"]][["first"]], list(1L, 1L, 2L))
  expect_identical(schema_publication(Resampler)@scope, "shared")
  expect_error(Resampler(
    type = "Custom",
    config = config,
    resamples = list(bad = c(1L, 0L))
  ))
})
