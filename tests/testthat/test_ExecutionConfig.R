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


# %% seed ----
testthat::test_that("setup_ExecutionConfig() keeps an explicit seed", {
  expect_identical(
    setup_ExecutionConfig(backend = "none", seed = 2026L)@seed,
    2026L
  )
})

testthat::test_that("setup_ExecutionConfig() resolves a seed when none is given", {
  # An unseeded run must still be reproducible, so a seed is drawn and recorded rather
  # than left NULL for the run to improvise.
  ec_unseeded <- setup_ExecutionConfig(backend = "none")
  expect_type(ec_unseeded@seed, "integer")
  expect_false(is.null(ec_unseeded@seed))
})

testthat::test_that("the drawn seed comes from the caller's RNG stream", {
  # Which is what keeps `set.seed(x); train(...)` deterministic without an explicit seed.
  set.seed(2026L)
  first <- setup_ExecutionConfig(backend = "none")@seed
  set.seed(2026L)
  expect_identical(setup_ExecutionConfig(backend = "none")@seed, first)
})
