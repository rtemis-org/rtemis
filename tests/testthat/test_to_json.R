# test_to_json.R
# ::rtemis::
# 2026- EDG rtemis.org

skip_if_not_installed("jsonlite")

library(data.table)


# Data ----
n <- 100L
x <- rnormmat(n, 3L, seed = 2026L)
y <- x[, 1L] + x[, 2L] + rnorm(n)
datr <- data.table(x, y)


# Generic ----
test_that("to_json() is a registered S7 generic", {
  expect_true(inherits(to_json, "S7_generic"))
})


# Supervised (Regression) ----
mod_r_glm <- train(x = datr, hyperparameters = setup_GLM())

test_that("to_json(Regression) returns a list with the core fields", {
  j <- to_json(mod_r_glm)
  expect_type(j, "list")
  expect_equal(j[["type"]], "Regression")
  expect_true(is.character(j[["xnames"]]))
  expect_equal(j[["n_features"]], length(mod_r_glm@xnames))
})

test_that("to_json(Regression) recurses into nested S7 props", {
  j <- to_json(mod_r_glm)
  expect_true(is.list(j[["metrics_training"]]))
  expect_true(is.list(j[["execution_config"]]))
  # A nested object carries the nested class's own published properties, not a
  # marker naming it: the property it sits on is what says which class it is.
  expect_setequal(
    names(j[["execution_config"]]),
    published_prop_names(ExecutionConfig)
  )
})

test_that("to_json(Regression) is JSON-serializable and round-trips", {
  j <- to_json(mod_r_glm)
  txt <- jsonlite::toJSON(j, auto_unbox = TRUE, na = "null", null = "null")
  expect_true(jsonlite::validate(txt))
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_equal(parsed[["type"]], "Regression")
})

test_that("to_json(Regression) excludes model, raw vectors, session_info", {
  j <- to_json(mod_r_glm)
  expect_false("model" %in% names(j))
  expect_false("y_training" %in% names(j))
  expect_false("predicted_training" %in% names(j))
  expect_false("session_info" %in% names(j))
})

test_that("to_json(Regression) drops NULL fields cleanly", {
  j <- to_json(mod_r_glm)
  expect_true(all(vapply(j, function(v) !is.null(v), logical(1L))))
})


# Supervised (Classification) ----
datc <- data.frame(iris[51:150, ])
datc$Species <- factor(datc$Species)
mod_c_glm <- train(x = datc, hyperparameters = setup_GLM())

test_that("to_json(Classification) includes binclasspos", {
  j <- to_json(mod_c_glm)
  expect_equal(j[["type"]], "Classification")
  expect_true("binclasspos" %in% names(j))
  expect_true(is.integer(j[["binclasspos"]]))
})


# SupervisedRes ----
resmod <- train(
  x = datr,
  hyperparameters = setup_GLM(),
  outer_resampling_config = setup_KFold(n_resamples = 3L)
)

test_that("to_json(RegressionRes) returns a list with the resample summary", {
  j <- to_json(resmod)
  expect_type(j, "list")
  expect_equal(j[["n_resamples"]], 3L)
  expect_true(is.list(j[["outer_resampler"]]))
  expect_true(is.list(j[["metrics_training"]]))
  expect_true(is.list(j[["metrics_test"]]))
})

test_that("to_json(RegressionRes) is JSON-serializable", {
  j <- to_json(resmod)
  txt <- jsonlite::toJSON(j, auto_unbox = TRUE, na = "null", null = "null")
  expect_true(jsonlite::validate(txt))
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_equal(parsed[["n_resamples"]], 3L)
})

test_that("to_json(RegressionRes) excludes models list (only summary count)", {
  j <- to_json(resmod)
  expect_false("models" %in% names(j))
})


# Default method ----
test_that("default to_json emits exactly the published properties", {
  exec <- setup_ExecutionConfig()
  j <- to_json(exec)
  expect_type(j, "list")
  # Exactly, in order: a results schema is `additionalProperties: false`, so an
  # extra key makes the document invalid against the contract it is published
  # under, and a missing one makes it incomplete.
  expect_identical(names(j), published_prop_names(ExecutionConfig))
})
