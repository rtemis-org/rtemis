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


# %% per-level workers ----
# Three levels can absorb workers. Left alone one is chosen by priority; named, the
# caller's choice replaces that entirely.

testthat::test_that("unset levels leave the ladder in charge", {
  ec <- setup_ExecutionConfig(backend = "mirai", n_workers = 4L)
  expect_null(ec@n_workers_outer)
  expect_null(ec@n_workers_tuning)
  expect_null(ec@n_workers_algorithm)
})


testthat::test_that("n_workers follows the dispatch levels when they are named", {
  ec <- setup_ExecutionConfig(
    backend = "mirai",
    n_workers_outer = 4L,
    n_workers_algorithm = 2L
  )
  # The pool is sized by the level that dispatches; algorithm workers are threads inside
  # a worker and add none of their own.
  expect_identical(ec@n_workers, 4L)
  expect_identical(ec@n_workers_algorithm, 2L)
})


testthat::test_that("algorithm threads need no backend", {
  # Threads run in the calling process, so there is nothing to dispatch to.
  ec <- setup_ExecutionConfig(backend = "none", n_workers_algorithm = 8L)
  expect_identical(ec@n_workers_algorithm, 8L)
  expect_identical(ec@n_workers, 1L)
})


testthat::test_that("two parallel dispatch levels are rejected", {
  # An outer fold runs in a worker process and cannot dispatch again from inside one.
  expect_error(
    setup_ExecutionConfig(
      backend = "mirai",
      n_workers_outer = 2L,
      n_workers_tuning = 4L
    ),
    "Only one dispatch level"
  )
})


testthat::test_that("a dispatch level is rejected when nothing dispatches", {
  expect_error(
    setup_ExecutionConfig(backend = "none", n_workers_outer = 4L),
    "must be 1 or unset when backend is 'none'"
  )
})


testthat::test_that("named levels reach the run, and compose", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("ranger")
  set.seed(2026L)
  n <- 400L
  x <- data.frame(matrix(stats::rnorm(n * 8L), n, 8L))
  x[["y"]] <- factor(ifelse(x[["X1"]] + stats::rnorm(n) > 0, "yes", "no"))
  mod <- train(
    x,
    hyperparameters = setup_Ranger(num_trees = 50L),
    outer_resampling_config = setup_Resampler(
      n_resamples = 4L,
      type = "KFold",
      seed = 1L
    ),
    execution_config = setup_ExecutionConfig(
      backend = "mirai",
      n_workers_outer = 4L,
      n_workers_algorithm = 2L,
      seed = 2026L
    ),
    verbosity = 0L
  )
  # Four folds dispatched to four processes, each fitting with two threads. The threads
  # have to survive the hop into the worker, which is the part a sequential fold config
  # would silently drop.
  expect_identical(
    unname(vapply(
      mod@models,
      function(m) m@hyperparameters@n_workers,
      integer(1L)
    )),
    rep(2L, 4L)
  )
})


testthat::test_that("a level that cannot run is clamped, not obeyed", {
  # The same config is reasonably pointed at data that tunes and data that does not.
  workers <- getFromNamespace("get_n_workers", "rtemis")(
    algorithm = "CART",
    hyperparameters = setup_CART(),
    outer_resampling_config = NULL,
    n_workers = 1L,
    n_workers_tuning = 8L,
    verbosity = 0L
  )
  expect_identical(workers[["tuning"]], 1L)
})
