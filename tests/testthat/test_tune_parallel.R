# test_tune_parallel.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# Tuning dispatches through the same `progress_plapply()` as outer resampling, so a grid
# search reproduces across backends and worker counts. Grid cells take one RNG substream
# each, keyed by cell index.

tune_dat <- local({
  set.seed(2026L)
  n <- 120L
  a <- stats::rnorm(n)
  b <- stats::rnorm(n)
  data.frame(
    a = a,
    b = b,
    y = factor(ifelse(a + b + stats::rnorm(n) > 0, "yes", "no"))
  )
})

tune_config <- setup_GridSearch(
  resampler_config = setup_Resampler(
    n_resamples = 3L,
    type = "KFold",
    seed = 7L
  )
)

fit_tuned <- function(backend, n_workers, seed = 2026L, ...) {
  train(
    tune_dat,
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L, 4L), xval = 10L),
    tuner_config = tune_config,
    execution_config = setup_ExecutionConfig(
      backend = backend,
      n_workers = n_workers,
      seed = seed,
      ...
    ),
    verbosity = 0L
  )
}

# The validation metrics per hyperparameter combination: what tuning actually decides on.
tuning_metrics <- function(mod) {
  as.numeric(unlist(mod@tuner@tuning_results[["validation"]]))
}


# %% Cross-backend reproducibility ----
testthat::test_that("tuning reproduces across backends", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  sequential <- tuning_metrics(fit_tuned("none", 1L))
  expect_identical(sequential, tuning_metrics(fit_tuned("mirai", 2L)))
  # Substreams are keyed by cell index, so the worker count cannot change the answer.
  expect_identical(sequential, tuning_metrics(fit_tuned("mirai", 3L)))
})

testthat::test_that("the future backend agrees with the others", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("future")
  expect_identical(
    tuning_metrics(fit_tuned("none", 1L)),
    tuning_metrics(
      fit_tuned("future", 2L, future_plan = "multisession")
    )
  )
})

testthat::test_that("a tuned run picks the same hyperparameters either way", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  expect_identical(
    fit_tuned("none", 1L)@hyperparameters@hyperparameters[["maxdepth"]],
    fit_tuned("mirai", 2L)@hyperparameters@hyperparameters[["maxdepth"]]
  )
})


# %% Grid cells receive substreams ----
testthat::test_that("the tuning dispatch receives one substream per cell", {
  # The end-to-end effect of a cell's seed depends on whether the algorithm draws at all,
  # so assert the plumbing directly: the dispatcher is handed as many substreams as there
  # are cells, and a different master seed produces different ones.
  # The tracer is evaluated in the traced function's own frame, so the collector has to
  # be somewhere it can reach: the global environment, cleaned up on the way out.
  assign(".rtemis_tune_trace", list(), envir = globalenv())
  tracer <- quote({
    if (identical(kind, "tune")) {
      assign(
        ".rtemis_tune_trace",
        c(
          get(".rtemis_tune_trace", envir = globalenv()),
          list(list(n = length(X), seeds = seeds))
        ),
        envir = globalenv()
      )
    }
  })
  suppressMessages(trace(
    rtemis:::progress_plapply,
    tracer = tracer,
    print = FALSE
  ))
  on.exit(
    {
      suppressMessages(untrace(rtemis:::progress_plapply))
      rm(".rtemis_tune_trace", envir = globalenv())
    },
    add = TRUE
  )
  invisible(fit_tuned("none", 1L, seed = 2026L))
  invisible(fit_tuned("none", 1L, seed = 99L))
  calls <- get(".rtemis_tune_trace", envir = globalenv())
  expect_length(calls, 2L)
  # 3 combinations x 3 inner resamples.
  expect_identical(calls[[1L]][["n"]], 9L)
  expect_length(calls[[1L]][["seeds"]], 9L)
  expect_false(identical(calls[[1L]][["seeds"]], calls[[2L]][["seeds"]]))
})


# %% Failure policy ----
# A grid-cell failure is fatal only under "stop"; "stop_outer" tolerates cells and is
# fatal for outer folds alone (specs/observability.md section 7).
failing_tune_dat <- local({
  set.seed(2026L)
  n <- 100L
  data.frame(
    a = stats::rnorm(n),
    y = factor(c(rep("common", n - 1L), "rare"))
  )
})

fit_failing_tune <- function(on_error) {
  train(
    failing_tune_dat,
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L), xval = 0L),
    tuner_config = tune_config,
    execution_config = setup_ExecutionConfig(
      backend = "mirai",
      n_workers = 2L,
      seed = 2026L,
      on_error = on_error
    ),
    verbosity = 0L
  )
}

testthat::test_that("unscorable combinations are reported, not passed over", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  # Every combination fails on at least one inner resample here, so none can be ranked.
  # The empty selection does not stay empty -- it becomes NA_integer_, and every tuned
  # hyperparameter is set to NA -- so this has to be said out loud rather than passed
  # over. `rtemis.core::warn()` reports through the message stream, not `warning()`.
  for (on_error in c("continue", "stop_outer")) {
    expect_message(
      tryCatch(fit_failing_tune(on_error), error = function(e) NULL),
      "cannot select a winner"
    )
  }
})

testthat::test_that("an unrankable grid leaves the tuned values NA", {
  # The mechanism behind the warning above, pinned so a future change to the selection
  # code cannot quietly restore the silent path: an empty selection passes through
  # `as.integer()` as NA_integer_, and indexing the grid with it yields a row of NAs.
  grid <- data.frame(
    param_combo_id = 1:3,
    maxdepth = c(2L, 3L, 4L),
    minsplit = c(20L, 30L, 40L)
  )
  empty_selection <- which.max(c(NaN, NaN, NaN))
  expect_length(empty_selection, 0L)
  combo_id <- as.integer(data.table::as.data.table(grid)[empty_selection, 1])
  expect_identical(combo_id, NA_integer_)
  best <- rtemis:::grid_row_values(grid, combo_id, -1)
  expect_identical(best[["maxdepth"]], NA_integer_)
  expect_identical(best[["minsplit"]], NA_integer_)
})
