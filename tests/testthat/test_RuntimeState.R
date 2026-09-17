# test_RuntimeState.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("runtime declarations preserve native types without publishing them", {
  fields <- c("model", "preprocessor", "preprocessor_internal", "decomposition")
  State <- schema_class(
    "RuntimeState",
    package = "rtemis",
    properties = Supervised@properties[fields],
    publication = SchemaPublication(
      description = "Runtime state fixture.",
      kind = "report",
      scope = "shared"
    )
  )
  state <- State()
  for (nm in fields) {
    expect_null(prop(state, nm))
  }
  for (nm in setdiff(fields, "model")) {
    expect_error(prop(state, nm) <- "invalid native state")
    expect_error(prop(state, nm) <- setup_PCA())
  }
  schema <- S7_to_JSONSchema(
    State,
    id = "https://example.test/runtime-state",
    asserted = TRUE
  )
  descriptors <- schema[["x-rtemis"]][["runtime_properties"]]
  expect_setequal(names(descriptors), fields)
  expect_false(any(fields %in% names(schema[["properties"]])))
  for (descriptor in descriptors) {
    expect_setequal(names(descriptor), c("kind", "description"))
    expect_identical(descriptor[["kind"]], "opaque")
  }
  for (cls in list(Regression, Classification, CalibratedClassification)) {
    expect_true(all(
      vapply(cls@properties[fields], prop_role, character(1L)) == "runtime"
    ))
  }
})


test_that("fitted transformations remain usable while records retain settings", {
  x <- as.data.frame(iris[, 1:3])
  dat <- data.frame(x, outcome = iris[[4L]])
  fitted <- train(
    dat,
    preprocessor_config = setup_SupervisedPreprocessor(
      center = TRUE,
      scale = TRUE
    ),
    decomposition_config = setup_PCA(k = 2L),
    hyperparameters = setup_GLM(),
    execution_config = setup_SerialExecution(seed = 21L),
    verbosity = 0L
  )
  expect_s7_class(fitted@preprocessor, Preprocessor)
  expect_s7_class(fitted@decomposition, Decomposition)
  expect_null(fitted@preprocessor_internal)
  predictions <- predict(fitted, x, verbosity = 0L)
  expect_equal(as.numeric(predictions), as.numeric(fitted@predicted_training))
  fields <- c("model", "preprocessor", "preprocessor_internal", "decomposition")
  for (serialize in list(S7_to_list, record_object, to_json)) {
    expect_false(any(fields %in% names(serialize(fitted))))
  }
  run <- record(fitted)
  for (settings in list(run, run[["folds"]][[1L]])) {
    expect_true(settings[["preprocessor_config"]][["center"]])
    expect_true(settings[["preprocessor_config"]][["scale"]])
    expect_identical(settings[["decomposition_config"]][["algorithm"]], "PCA")
    expect_identical(settings[["decomposition_config"]][["k"]], 2L)
  }
  expect_true(jsonlite::validate(jsonlite::toJSON(
    run,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )))
  expect_equal(predict(fitted, x, verbosity = 0L), predictions)
})


test_that("algorithm-internal preprocessing remains attached for prediction", {
  skip_if_not_installed("kknn")
  x <- data.frame(length = iris[[1L]], species = iris[[5L]])
  fitted <- train(
    data.frame(x, outcome = iris[[4L]]),
    hyperparameters = setup_KNN(k = 3L),
    execution_config = setup_SerialExecution(seed = 21L),
    verbosity = 0L
  )
  expect_s7_class(fitted@preprocessor_internal, Preprocessor)
  transformed <- apply_preprocessor(
    fitted@preprocessor_internal,
    x,
    verbosity = 0L
  )
  predictions <- predict(fitted, x, verbosity = 0L)
  expect_equal(
    predictions,
    predict_super(
      fitted@model,
      transformed,
      type = "Regression",
      verbosity = 0L
    )
  )
  expect_false("preprocessor_internal" %in% names(to_json(fitted)))
  expect_false("preprocessor_internal" %in% names(S7_to_list(fitted)))
  expect_equal(predict(fitted, x, verbosity = 0L), predictions)
})
