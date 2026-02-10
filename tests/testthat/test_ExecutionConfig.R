# test_ExecutionConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# %% ExecutionConfig ----
ec <- ExecutionConfig(
  backend = "future",
  n_workers = 4L,
  future_plan = "multisession"
)
ec
testthat::test_that("ExecutionConfig() works", {
  expect_s7_class(
    ec,
    ExecutionConfig
  )
})

# %% setup_ExecutionConfig() ----
ec <- setup_ExecutionConfig(
  backend = "future",
  n_workers = 4L,
  future_plan = "multisession"
)
testthat::test_that("setup_ExecutionConfig() works", {
  expect_s7_class(
    ec,
    ExecutionConfig
  )
})

# %% setup_ExecutionConfig() fails when backend is future and plan is NULL ----
testthat::test_that("setup_ExecutionConfig() fails when backend is future and plan is NULL", {
  expect_error(
    setup_ExecutionConfig(
      backend = "future",
      n_workers = 4L,
      future_plan = NULL
    ),
    "must be set when backend is 'future'"
  )
})
