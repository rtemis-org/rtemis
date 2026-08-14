# test_shared_memory.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# Handing workers the training data through shared memory rather than serializing a copy
# to each. The load-bearing property is that it changes nothing about the answer.

share_payload <- getFromNamespace("share_payload", "rtemis")
workers_are_local <- getFromNamespace("workers_are_local", "rtemis")
SHARE_MIN_BYTES <- getFromNamespace("SHARE_MIN_BYTES", "rtemis")

# Comfortably over the sharing threshold.
big_payload <- as.data.frame(matrix(stats::rnorm(2e5), ncol = 10L))
small_payload <- data.frame(a = 1:10)

is_shared_obj <- function(x) {
  isTRUE(mori::is_shared(x))
}


# %% workers_are_local ----
testthat::test_that("mirai and sequential workers are local", {
  expect_true(workers_are_local("mirai", NULL))
  expect_true(workers_are_local("none", NULL))
})

testthat::test_that("future plans are classified by locality", {
  expect_true(workers_are_local("future", "multisession"))
  expect_true(workers_are_local("future", "multicore"))
  expect_true(workers_are_local("future", "future.mirai::mirai_multisession"))
  expect_false(workers_are_local("future", "remote"))
  # `cluster` accepts remote hostnames and its name alone does not say which, so it
  # cannot be assumed local.
  expect_false(workers_are_local("future", "cluster"))
  expect_false(workers_are_local("future", NULL))
})


# %% share_payload: policy ----
testthat::test_that("'none' never shares", {
  testthat::skip_if_not_installed("mori")
  expect_false(is_shared_obj(share_payload(
    big_payload,
    "none",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  )))
})

testthat::test_that("NULL payloads pass through", {
  expect_null(share_payload(
    NULL,
    "always",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  ))
})

testthat::test_that("'auto' shares a large payload for local parallel workers", {
  testthat::skip_if_not_installed("mori")
  expect_true(is_shared_obj(share_payload(
    big_payload,
    "auto",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  )))
})

testthat::test_that("'auto' declines when sharing would not pay", {
  testthat::skip_if_not_installed("mori")
  declines <- function(...) {
    !is_shared_obj(share_payload(..., verbosity = 0L))
  }
  # Below the size threshold: fixed cost, no benefit.
  expect_true(declines(small_payload, "auto", "mirai", n_workers = 4L))
  # Nothing is transferred when there is one worker.
  expect_true(declines(big_payload, "auto", "mirai", n_workers = 1L))
  # Forked workers already share these pages copy-on-write.
  expect_true(declines(
    big_payload,
    "auto",
    "future",
    "multicore",
    n_workers = 4L
  ))
  # Shared memory is local RAM.
  expect_true(declines(big_payload, "auto", "future", "remote", n_workers = 4L))
})

testthat::test_that("'always' ignores the size threshold", {
  testthat::skip_if_not_installed("mori")
  expect_lt(as.numeric(utils::object.size(small_payload)), SHARE_MIN_BYTES)
  expect_true(is_shared_obj(share_payload(
    small_payload,
    "always",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  )))
  # Including with no parallelism at all, which is what lets a run be compared against
  # its own shared counterpart.
  expect_true(is_shared_obj(share_payload(
    small_payload,
    "always",
    "none",
    n_workers = 1L,
    verbosity = 0L
  )))
})

testthat::test_that("'always' errors when workers are not local", {
  testthat::skip_if_not_installed("mori")
  expect_error(
    share_payload(
      big_payload,
      "always",
      "future",
      "remote",
      n_workers = 4L,
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
})


# %% share_payload: what it produces ----
testthat::test_that("sharing collapses the serialized payload", {
  testthat::skip_if_not_installed("mori")
  shared <- share_payload(
    big_payload,
    "always",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  )
  # A shared object serializes as its region name, a few hundred bytes, rather than its
  # contents -- which is the entire point.
  expect_lt(
    length(serialize(shared, NULL)),
    length(serialize(big_payload, NULL)) / 100
  )
})

testthat::test_that("a shared object reads back as the original", {
  testthat::skip_if_not_installed("mori")
  shared <- share_payload(
    big_payload,
    "always",
    "mirai",
    n_workers = 4L,
    verbosity = 0L
  )
  expect_identical(dim(shared), dim(big_payload))
  expect_equal(shared[[1L]], big_payload[[1L]])
  expect_equal(shared[5:10, ], big_payload[5:10, ])
})

testthat::test_that("containers survive sharing", {
  testthat::skip_if_not_installed("mori")
  share_it <- function(x) {
    share_payload(x, "always", "mirai", n_workers = 4L, verbosity = 0L)
  }
  # data.frame
  expect_s3_class(share_it(big_payload), "data.frame")
  # data.table, key intact
  dt <- data.table::as.data.table(big_payload)
  data.table::setkeyv(dt, "V1")
  shared_dt <- share_it(dt)
  expect_true(is_shared_obj(shared_dt))
  expect_identical(data.table::key(shared_dt), "V1")
  expect_equal(nrow(shared_dt), nrow(dt))
  # tibble
  testthat::skip_if_not_installed("tibble")
  shared_tbl <- share_it(tibble::as_tibble(big_payload))
  expect_true(is_shared_obj(shared_tbl))
  expect_s3_class(shared_tbl, "tbl_df")
})

testthat::test_that("a shared global is not counted against future's size limit", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("future")
  # `object.size()` reports a shared object at full nominal size, so if future's globals
  # accounting used it, a large shared payload would be refused despite a few hundred
  # bytes crossing the wire -- which would make the future backend unusable with sharing.
  # It does not, and this pins that.
  payload <- as.data.frame(matrix(stats::rnorm(4e5), ncol = 10L))
  shared <- share_payload(
    payload,
    "always",
    "mirai",
    n_workers = 2L,
    verbosity = 0L
  )
  nominal <- as.numeric(utils::object.size(shared))
  old <- options(future.globals.maxSize = nominal / 4)
  on.exit(
    {
      options(old)
      future::plan("sequential")
    },
    add = TRUE
  )
  future::plan("multisession", workers = 2L)
  expect_identical(
    future::value(future::future(nrow(shared), seed = TRUE)),
    nrow(payload)
  )
})

testthat::test_that("a shared object arrives shared in a worker", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("mirai")
  shared <- share_payload(
    big_payload,
    "always",
    "mirai",
    n_workers = 2L,
    verbosity = 0L
  )
  mirai::daemons(2L, dispatcher = TRUE)
  on.exit(mirai::daemons(0L), add = TRUE)
  out <- mirai::mirai_map(
    1:2,
    function(i, d) {
      list(shared = mori::is_shared(d), n = nrow(d), v = d[[1L]][1L])
    },
    .args = list(d = shared)
  )[]
  expect_true(out[[1L]][["shared"]])
  expect_identical(out[[1L]][["n"]], nrow(big_payload))
  expect_equal(out[[1L]][["v"]], big_payload[[1L]][1L])
})


# %% ExecutionConfig ----
testthat::test_that("shared_memory defaults to 'none'", {
  expect_identical(
    setup_ExecutionConfig(backend = "none")@shared_memory,
    "none"
  )
})

testthat::test_that("shared_memory is validated", {
  expect_error(setup_ExecutionConfig(backend = "none", shared_memory = "yes"))
  expect_identical(
    setup_ExecutionConfig(
      backend = "none",
      shared_memory = "auto"
    )@shared_memory,
    "auto"
  )
})


# %% End to end: the answer does not change ----
shm_dat <- local({
  set.seed(2026L)
  n <- 300L
  a <- stats::rnorm(n)
  b <- stats::rnorm(n)
  data.frame(
    a = a,
    b = b,
    y = factor(ifelse(a + b + stats::rnorm(n) > 0, "yes", "no"))
  )
})

shm_resampler <- setup_Resampler(n_resamples = 4L, type = "KFold", seed = 2026L)

fit_shm <- function(shared_memory, backend = "mirai", n_workers = 2L) {
  train(
    shm_dat,
    hyperparameters = setup_CART(xval = 10L, prune_cp = 0.03),
    outer_resampling_config = shm_resampler,
    execution_config = setup_ExecutionConfig(
      backend = backend,
      n_workers = n_workers,
      seed = 2026L,
      shared_memory = shared_memory
    ),
    verbosity = 0L
  )
}

fold_predictions <- function(mod) {
  lapply(mod@predicted_test, as.integer)
}

testthat::test_that("outer resampling gives the same answer shared or not", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("mirai")
  baseline <- fold_predictions(fit_shm("none"))
  expect_identical(baseline, fold_predictions(fit_shm("always")))
  expect_identical(baseline, fold_predictions(fit_shm("auto")))
})

testthat::test_that("sharing changes nothing in a sequential run either", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mori")
  expect_identical(
    fold_predictions(fit_shm("none", backend = "none", n_workers = 1L)),
    fold_predictions(fit_shm("always", backend = "none", n_workers = 1L))
  )
})

testthat::test_that("tuning gives the same answer shared or not", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("mirai")
  fit <- function(shared_memory) {
    train(
      shm_dat,
      hyperparameters = setup_CART(
        maxdepth = tune_over(2L, 3L, 4L),
        xval = 10L
      ),
      tuner_config = setup_GridSearch(
        resampler_config = setup_Resampler(
          n_resamples = 3L,
          type = "KFold",
          seed = 7L
        )
      ),
      execution_config = setup_ExecutionConfig(
        backend = "mirai",
        n_workers = 2L,
        seed = 2026L,
        shared_memory = shared_memory
      ),
      verbosity = 0L
    )
  }
  expect_identical(
    as.numeric(unlist(fit("none")@tuner@tuning_results[["validation"]])),
    as.numeric(unlist(fit("always")@tuner@tuning_results[["validation"]]))
  )
})
