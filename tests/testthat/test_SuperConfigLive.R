# test_SuperConfigLive.R
# ::rtemis::
# 2026- EDG rtemis.org
#
# Tests for `SuperConfigLive` and its `train()` dispatch arm. The wire-
# level integration (rtemislive train handler → SuperConfigLive) is
# covered in test_rtemislive_dispatch_data_jobs.R / test_rtemislive_serial.R.

library(data.table)

# Helpers --------------------------------------------------------------------

small_regression_dt <- function(seed = 2030L, n = 60L) {
  set.seed(seed)
  dt <- data.table(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n),
    y = NA_real_
  )
  dt[, y := a + 0.5 * b + rnorm(n)]
  dt
}


# Constructor / props -------------------------------------------------------

test_that("setup_SuperConfigLive returns a SuperConfigLive with expected props", {
  dt <- small_regression_dt()
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    algorithm = "glm"
  )
  expect_true(S7_inherits(cfg, SuperConfigLive))
  expect_identical(cfg@dat_training, dt)
  expect_null(cfg@dat_validation)
  expect_null(cfg@dat_test)
  expect_equal(cfg@algorithm, "glm")
  expect_null(cfg@outdir)
})

test_that("setup_SuperConfigLive enforces tabular type on dat_training", {
  expect_error(
    setup_SuperConfigLive(dat_training = "not a data frame"),
    regexp = "(class_tabular|tabular|data.frame|data.table)"
  )
})

test_that("setup_SuperConfigLive accepts a data.frame (not just data.table)", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  cfg <- setup_SuperConfigLive(dat_training = df, algorithm = "glm")
  expect_s3_class(cfg@dat_training, "data.frame")
})


# train() SuperConfigLive dispatch ------------------------------------------

test_that("train(SuperConfigLive) runs end-to-end for a simple GLM regression", {
  dt <- small_regression_dt(seed = 2031L)
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    algorithm = "glm",
    verbosity = 0L
  )
  mod <- train(cfg)
  expect_true(S7_inherits(mod, Supervised))
  expect_equal(mod@algorithm, "GLM")
  expect_length(mod@predicted_training, nrow(dt))
})

test_that("train(SuperConfigLive) accepts an in-memory validation split", {
  dt <- small_regression_dt(seed = 2032L)
  val <- small_regression_dt(seed = 2033L, n = 20L)
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    dat_validation = val,
    algorithm = "glm",
    verbosity = 0L
  )
  mod <- train(cfg)
  expect_true(S7_inherits(mod, Supervised))
  expect_length(mod@predicted_validation, nrow(val))
})
