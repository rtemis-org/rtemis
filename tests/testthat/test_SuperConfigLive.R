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
    hyperparameters = setup_GLM()
  )
  expect_true(S7_inherits(cfg, SuperConfigLive))
  expect_identical(cfg@dat_training, dt)
  expect_null(cfg@dat_validation)
  expect_null(cfg@dat_test)
  expect_equal(cfg@hyperparameters@algorithm, "GLM")
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
  cfg <- setup_SuperConfigLive(dat_training = df, hyperparameters = setup_GLM())
  expect_s3_class(cfg@dat_training, "data.frame")
})


# train() SuperConfigLive dispatch ------------------------------------------

test_that("train(SuperConfigLive) runs end-to-end for a simple GLM regression", {
  dt <- small_regression_dt(seed = 2031L)
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    hyperparameters = setup_GLM(),
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
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  mod <- train(cfg)
  expect_true(S7_inherits(mod, Supervised))
  expect_length(mod@predicted_validation, nrow(val))
})


# Weights column resolution --------------------------------------------------

# The datasets a config carries are `data.table`s whenever they come off disk
# via `read()`, so the weights column has to be dropped in a way that respects
# the frame's class and leaves the caller's copy alone.

weighted_regression_dt <- function(seed = 2040L, n = 60L) {
  dt <- small_regression_dt(seed = seed, n = n)
  dt[, w := runif(n, 0.5, 1.5)]
  setcolorder(dt, c("a", "b", "c", "w", "y"))
  dt[]
}

test_that("train(SuperConfigLive) resolves a named weights column on data.table", {
  dt <- weighted_regression_dt()
  val <- weighted_regression_dt(seed = 2041L, n = 20L)
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    dat_validation = val,
    weights = "w",
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  mod <- train(cfg)
  expect_true(S7_inherits(mod, Supervised))
  # The weights column trains the model, it is not one of its features.
  expect_identical(mod@xnames, c("a", "b", "c"))
  expect_length(mod@predicted_validation, nrow(val))
  # The model's input config reports the column the user asked for.
  expect_identical(mod@config@weights, "w")
  # Dropping the column must not touch the caller's tables.
  expect_identical(names(dt), c("a", "b", "c", "w", "y"))
  expect_identical(names(val), c("a", "b", "c", "w", "y"))
})

test_that("train(SuperConfigLive) resolves a named weights column on data.frame", {
  dt <- as.data.frame(weighted_regression_dt(seed = 2042L))
  cfg <- setup_SuperConfigLive(
    dat_training = dt,
    weights = "w",
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  mod <- train(cfg)
  expect_identical(mod@xnames, c("a", "b", "c"))
  expect_identical(names(dt), c("a", "b", "c", "w", "y"))
})
