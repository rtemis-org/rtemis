# test_Resampler.R
# ::rtemis::
# EDG rtemis.org

# library(testthat)

# StratSubConfig ----
test_that("StratSubConfig succeeds", {
  rsp <- StratSubConfig(
    n_resamples = 10L,
    stratify_var = NULL,
    train_p = .75,
    strat_n_bins = 4L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, StratSubConfig)
})

# KFoldConfig ----
test_that("KFoldConfig succeeds", {
  rsp <- KFoldConfig(
    n_resamples = 10L,
    stratify_var = NULL,
    strat_n_bins = 4L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, KFoldConfig)
})

# BootstrapConfig ----
test_that("BootstrapConfig succeeds", {
  rsp <- BootstrapConfig(
    n_resamples = 10L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, BootstrapConfig)
})

# StratBootConfig ----
test_that("StratBootConfig succeeds", {
  rsp <- StratBootConfig(
    n_resamples = 10L,
    stratify_var = NULL,
    train_p = .75,
    strat_n_bins = 4L,
    target_length = NULL,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, StratBootConfig)
})

# LOOCVConfig ----
test_that("LOOCVConfig succeeds", {
  rsp <- LOOCVConfig(
    n_resamples = 10L
  )
  expect_s7_class(rsp, LOOCVConfig)
})

# CustomConfig ----
test_that("CustomConfig succeeds", {
  rsp <- CustomConfig(
    n_resamples = 10L
  )
  expect_s7_class(rsp, CustomConfig)
})

# setup_KFold() defaults ----
test_that("setup_KFold() succeeds", {
  rsp <- setup_KFold()
  expect_s7_class(rsp, ResamplerConfig)
})

# setup_KFold() kfold ----
test_that("setup_KFold() kfold succeeds", {
  rsp <- setup_KFold()
  expect_s7_class(rsp, KFoldConfig)
})

# setup_KFold() strat_sub ----
test_that("setup_KFold() strat_sub succeeds", {
  rsp <- setup_StratSub()
  expect_s7_class(rsp, StratSubConfig)
})

# setup_KFold() strat_boot ----
test_that("setup_KFold() strat_boot succeeds", {
  rsp <- setup_StratBoot()
  expect_s7_class(rsp, StratBootConfig)
})

test_that("setup_KFold() strat_boot fails with invalid train_p", {
  expect_error(
    setup_StratBoot(train_p = 1)
  )
})

# setup_KFold() bootstrap ----
test_that("setup_KFold() bootstrap succeeds", {
  rsp <- setup_Bootstrap()
  expect_s7_class(rsp, BootstrapConfig)
})

# setup_KFold() loocv ----
test_that("setup_KFold() loocv succeeds", {
  rsp <- setup_LOOCV()
  expect_s7_class(rsp, LOOCVConfig)
})

# Resampler ----
test_that("Resampler() succeeds", {
  res <- Resampler(
    type = "Custom",
    resamples = list(),
    config = setup_KFold()
  )
  expect_s7_class(res, Resampler)
})

# resample() vector ----
## KFold ----
test_that("resample() vector succeeds", {
  res <- resample(iris[[1]], setup_KFold())
  expect_s7_class(res, Resampler)
})

## StratSub ----
test_that("resample() vector succeeds with StratSub", {
  res <- resample(iris[[1]], setup_StratSub())
  expect_s7_class(res, Resampler)
})

## StratBoot ----
test_that("resample() vector succeeds with StratBoot", {
  res <- resample(iris[[1]], setup_StratBoot())
  expect_s7_class(res, Resampler)
})

## Bootstrap ----
test_that("resample() vector succeeds with Bootstrap", {
  res <- resample(iris[[1]], setup_Bootstrap())
  expect_s7_class(res, Resampler)
})

## LOOCV ----
test_that("resample() vector succeeds with LOOCV", {
  res <- resample(iris[[1]], setup_LOOCV())
  expect_s7_class(res, Resampler)
})

# resample() data.frame ----
test_that("resample() data.frame succeeds", {
  res <- resample(iris, setup_KFold())
  expect_s7_class(res, Resampler)
})

# resample() data.table ----
test_that("resample() data.table succeeds", {
  res <- resample(as.data.table(iris), setup_KFold())
  expect_s7_class(res, Resampler)
})


# %% id_strat names a column ----
test_that("resample() groups cases by the column id_strat names", {
  d <- data.frame(subject = rep(sprintf("s%d", 1:6), each = 2L), y = rnorm(12L))
  res <- resample(
    d,
    setup_StratSub(n_resamples = 2L, id_strat = "subject"),
    verbosity = 0L
  )
  expect_s7_class(res, Resampler)
  # The point of the grouping: a subject is wholly in the training set or
  # wholly out of it, never split across the two.
  for (train_idx in res@resamples) {
    test_idx <- setdiff(seq_len(nrow(d)), train_idx)
    expect_length(
      intersect(d[["subject"]][train_idx], d[["subject"]][test_idx]),
      0L
    )
  }
  # The config carries the name, not the values: one string that means the same
  # thing to any reader, where a per-case vector is true of one row order only.
  expect_identical(res@config@id_strat, "subject")
})


test_that("train() groups by the id_strat column and keeps it out of the features", {
  # The column identifies cases rather than describing them, so it must reach
  # the resampler and not the learner. Left in it would be a high-cardinality
  # feature, and `check_supervised()` rejects string IDs outright.
  set.seed(1)
  d <- data.frame(
    subject = rep(sprintf("s%02d", 1:30), each = 3L),
    x1 = rnorm(90L),
    y = rnorm(90L)
  )
  mod <- train(
    d,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_KFold(
      n_resamples = 3L,
      id_strat = "subject"
    ),
    verbosity = 0L
  )
  expect_identical(mod@xnames, "x1")
  for (train_idx in mod@outer_resampler@resamples) {
    test_idx <- setdiff(seq_len(nrow(d)), train_idx)
    expect_length(
      intersect(d[["subject"]][train_idx], d[["subject"]][test_idx]),
      0L
    )
  }
})


test_that("resample() narrows any tabular input to its outcome column", {
  # One column is still a frame: narrowing only when `NCOL(x) > 1` left a list
  # to reach the resamplers, which failed on coercion, and made a name
  # unresolvable in the one frame that certainly holds it. `[[` also indexes a
  # matrix in column-major order, so a wide matrix narrowed to a single cell.
  one_column <- iris["Sepal.Length"]
  expect_length(
    resample(
      one_column,
      setup_KFold(n_resamples = 3L),
      verbosity = 0L
    )@resamples,
    3L
  )
  named <- resample(
    one_column,
    setup_KFold(n_resamples = 3L, stratify_var = "Sepal.Length"),
    verbosity = 0L
  )
  expect_length(named@resamples, 3L)
  # Every resample of a 150-case frame indexes cases, not columns.
  for (train_idx in named@resamples) {
    expect_true(all(train_idx <= nrow(one_column)))
    expect_gt(length(train_idx), 1L)
  }
  matrix_input <- resample(
    as.matrix(iris[, 1:4]),
    setup_KFold(n_resamples = 3L),
    verbosity = 0L
  )
  expect_length(matrix_input@resamples, 3L)
  expect_gt(length(matrix_input@resamples[[1L]]), 1L)
})


test_that("resample() rejects a column name it cannot resolve", {
  d <- data.frame(subject = rep(c("a", "b"), 3L), y = rnorm(6L))
  expect_error(
    resample(
      d,
      setup_KFold(n_resamples = 2L, id_strat = "nope"),
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
  # A bare vector carries no columns to look a name up in.
  expect_error(
    resample(
      rnorm(6L),
      setup_KFold(n_resamples = 2L, id_strat = "subject"),
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
})


test_that("resample() stratifies on the column stratify_var names", {
  # Regression: the name was passed on as if it were the values, so
  # `as.numeric()` made it NA and `cut()` failed on an unusable bin count.
  res <- resample(
    iris,
    setup_StratSub(
      n_resamples = 3L,
      stratify_var = "Sepal.Width"
    ),
    verbosity = 0L
  )
  expect_s7_class(res, Resampler)
  expect_length(res@resamples, 3L)
})


# %% Custom ----
test_that("resample() returns the resamples a Custom config was given", {
  res <- resample(
    rnorm(20L),
    setup_Custom(resamples = list(1:15, 6:20)),
    verbosity = 0L
  )
  expect_s7_class(res, Resampler)
  expect_equal(unname(res@resamples), list(1:15, 6:20))
  # Only the supplied resamples can say how many there are.
  expect_identical(res@config@n_resamples, 2L)
})


test_that("Custom resamples are required and must address the data", {
  expect_error(setup_Custom(), class = "rtemis_value_error")
  expect_error(
    resample(
      rnorm(10L),
      setup_Custom(resamples = list(1:20)),
      verbosity = 0L
    ),
    class = "rtemis_range_error"
  )
})


test_that("a Custom resampler has no wire form", {
  # Its resamples are positions in one dataset: the type name travels between
  # implementations, the indices do not.
  expect_error(
    .list_to_ResamplerConfig(list(type = "Custom", n_resamples = 3L)),
    class = "rtemis_unsupported_error"
  )
})

test_that("resample() accepts a bare outcome vector", {
  # A resampler bounds only `n_cases`, so `resolve_data_bounds()` must not
  # reach for `features()`, which requires at least two columns.
  expect_s7_class(
    resample(rnorm(6L), setup_KFold(n_resamples = 2L), verbosity = 0L),
    Resampler
  )
})
