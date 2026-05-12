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
mod_r_glm <- train(x = datr, algorithm = "glm")

test_that("to_json(Regression) returns a list with .class and core fields", {
  j <- to_json(mod_r_glm)
  expect_type(j, "list")
  expect_equal(j[[".class"]], "Regression")
  expect_equal(j[["type"]], "Regression")
  expect_true(is.character(j[["xnames"]]))
  expect_equal(j[["n_features"]], length(mod_r_glm@xnames))
  expect_equal(j[["n_training"]], length(mod_r_glm@y_training))
})

test_that("to_json(Regression) recurses into nested S7 props with .class tags", {
  j <- to_json(mod_r_glm)
  expect_true(is.list(j[["metrics_training"]]))
  expect_true(".class" %in% names(j[["metrics_training"]]))
  expect_true(is.list(j[["execution_config"]]))
  expect_equal(j[["execution_config"]][[".class"]], "ExecutionConfig")
})

test_that("to_json(Regression) is JSON-serializable and round-trips", {
  j <- to_json(mod_r_glm)
  txt <- jsonlite::toJSON(j, auto_unbox = TRUE, na = "null", null = "null")
  expect_true(jsonlite::validate(txt))
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_equal(parsed[[".class"]], "Regression")
  expect_equal(parsed[["type"]], "Regression")
})

test_that("to_json(Regression) excludes model, raw vectors, session_info", {
  j <- to_json(mod_r_glm)
  expect_false("model" %in% names(j))
  expect_false("y_training" %in% names(j))
  expect_false("predicted_training" %in% names(j))
  expect_false("session_info" %in% names(j))
  expect_false("extra" %in% names(j))
})

test_that("to_json(Regression) drops NULL fields cleanly", {
  j <- to_json(mod_r_glm)
  expect_true(all(vapply(j, function(v) !is.null(v), logical(1L))))
})


# Supervised (Classification) ----
datc <- data.frame(iris[51:150, ])
datc$Species <- factor(datc$Species)
mod_c_glm <- train(x = datc, algorithm = "glm")

test_that("to_json(Classification) tags class and includes binclasspos", {
  j <- to_json(mod_c_glm)
  expect_equal(j[[".class"]], "Classification")
  expect_true("binclasspos" %in% names(j))
  expect_true(is.integer(j[["binclasspos"]]))
})


# SupervisedRes ----
resmod <- train(
  x = datr,
  algorithm = "glm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)

test_that("to_json(RegressionRes) returns a list with .class and resample summary", {
  j <- to_json(resmod)
  expect_type(j, "list")
  expect_equal(j[[".class"]], "RegressionRes")
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
  expect_equal(parsed[[".class"]], "RegressionRes")
  expect_equal(parsed[["n_resamples"]], 3L)
})

test_that("to_json(RegressionRes) excludes models list (only summary count)", {
  j <- to_json(resmod)
  expect_false("models" %in% names(j))
})


# Default method ----
test_that("default to_json walks props and tags .class for arbitrary S7 objects", {
  exec <- setup_ExecutionConfig()
  j <- to_json(exec)
  expect_type(j, "list")
  expect_equal(j[[".class"]], "ExecutionConfig")
})
