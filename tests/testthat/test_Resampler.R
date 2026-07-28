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

# setup_Resampler() defaults ----
test_that("setup_Resampler() succeeds", {
  rsp <- setup_Resampler()
  expect_s7_class(rsp, ResamplerConfig)
})

# setup_Resampler() kfold ----
test_that("setup_Resampler() kfold succeeds", {
  rsp <- setup_Resampler(type = "KFold")
  expect_s7_class(rsp, KFoldConfig)
})

# setup_Resampler() strat_sub ----
test_that("setup_Resampler() strat_sub succeeds", {
  rsp <- setup_Resampler(type = "StratSub")
  expect_s7_class(rsp, StratSubConfig)
})

# setup_Resampler() strat_boot ----
test_that("setup_Resampler() strat_boot succeeds", {
  rsp <- setup_Resampler(type = "StratBoot")
  expect_s7_class(rsp, StratBootConfig)
})

test_that("setup_Resampler() strat_boot fails with invalid train_p", {
  expect_error(
    setup_Resampler(type = "StratBoot", train_p = 1)
  )
})

# setup_Resampler() bootstrap ----
test_that("setup_Resampler() bootstrap succeeds", {
  rsp <- setup_Resampler(type = "Bootstrap")
  expect_s7_class(rsp, BootstrapConfig)
})

# setup_Resampler() loocv ----
test_that("setup_Resampler() loocv succeeds", {
  rsp <- setup_Resampler(type = "LOOCV")
  expect_s7_class(rsp, LOOCVConfig)
})

# Resampler ----
test_that("Resampler() succeeds", {
  res <- Resampler(
    type = "Custom",
    resamples = list(),
    config = setup_Resampler()
  )
  expect_s7_class(res, Resampler)
})

# resample() vector ----
## KFold ----
test_that("resample() vector succeeds", {
  res <- resample(iris[[1]], setup_Resampler(type = "KFold"))
  expect_s7_class(res, Resampler)
})

## StratSub ----
test_that("resample() vector succeeds with StratSub", {
  res <- resample(iris[[1]], setup_Resampler(type = "StratSub"))
  expect_s7_class(res, Resampler)
})

## StratBoot ----
test_that("resample() vector succeeds with StratBoot", {
  res <- resample(iris[[1]], setup_Resampler(type = "StratBoot"))
  expect_s7_class(res, Resampler)
})

## Bootstrap ----
test_that("resample() vector succeeds with Bootstrap", {
  res <- resample(iris[[1]], setup_Resampler(type = "Bootstrap"))
  expect_s7_class(res, Resampler)
})

## LOOCV ----
test_that("resample() vector succeeds with LOOCV", {
  res <- resample(iris[[1]], setup_Resampler(type = "LOOCV"))
  expect_s7_class(res, Resampler)
})

# resample() data.frame ----
test_that("resample() data.frame succeeds", {
  res <- resample(iris, setup_Resampler())
  expect_s7_class(res, Resampler)
})

# resample() data.table ----
test_that("resample() data.table succeeds", {
  res <- resample(as.data.table(iris), setup_Resampler())
  expect_s7_class(res, Resampler)
})


# %% id_strat is checked declaratively ----
test_that("resample() enforces id_strat's data_bound", {
  d <- data.frame(a = rnorm(6L), y = rnorm(6L))
  ok <- setup_Resampler(
    type = "StratSub",
    n_resamples = 2L,
    id_strat = c("a", "b", "a", "b", "a", "b")
  )
  expect_s7_class(resample(d, ok, verbosity = 0L), Resampler)
  # `data_bound = "n_cases"` replaces the hand-written length check train() used
  # to carry, so it now fires for every caller, not just train().
  bad <- setup_Resampler(
    type = "StratSub",
    n_resamples = 2L,
    id_strat = c("a", "b")
  )
  expect_error(resample(d, bad, verbosity = 0L), class = "rtemis_length_error")
})

test_that("resample() accepts a bare outcome vector", {
  # A resampler bounds only `n_cases`, so `resolve_data_bounds()` must not
  # reach for `features()`, which requires at least two columns.
  expect_s7_class(
    resample(rnorm(6L), setup_Resampler(n_resamples = 2L), verbosity = 0L),
    Resampler
  )
})
