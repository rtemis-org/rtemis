# test-Resampler.R
# ::rtemis::
# EDG rtemis.org

# library(testthat)

# StratSubParams ----
test_that("StratSubParams succeeds", {
  rsp <- StratSubParams(
    n = 10L,
    stratify_var = NULL,
    train_p = .75,
    strat_n_bins = 4L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, StratSubParams)
})

# KFoldParams ----
test_that("KFoldParams succeeds", {
  rsp <- KFoldParams(
    n = 10L,
    stratify_var = NULL,
    strat_n_bins = 4L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, KFoldParams)
})

# BootstrapParams ----
test_that("BootstrapParams succeeds", {
  rsp <- BootstrapParams(
    n = 10L,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, BootstrapParams)
})

# StratBootParams ----
test_that("StratBootParams succeeds", {
  rsp <- StratBootParams(
    n = 10L,
    stratify_var = NULL,
    train_p = .75,
    strat_n_bins = 4L,
    target_length = NULL,
    id_strat = NULL,
    seed = NULL
  )
  expect_s7_class(rsp, StratBootParams)
})

# LOOCVParams ----
test_that("LOOCVParams succeeds", {
  rsp <- LOOCVParams(
    n = 10L
  )
  expect_s7_class(rsp, LOOCVParams)
})

# CustomParams ----
test_that("CustomParams succeeds", {
  rsp <- CustomParams(
    n = 10L
  )
  expect_s7_class(rsp, CustomParams)
})

# setup_Resampler() defaults ----
test_that("setup_Resampler() succeeds", {
  rsp <- setup_Resampler()
  expect_s7_class(rsp, ResamplerConfig)
})

# setup_Resampler() kfold ----
test_that("setup_Resampler() kfold succeeds", {
  rsp <- setup_Resampler(type = "KFold")
  expect_s7_class(rsp, KFoldParams)
})

# setup_Resampler() strat_sub ----
test_that("setup_Resampler() strat_sub succeeds", {
  rsp <- setup_Resampler(type = "StratSub")
  expect_s7_class(rsp, StratSubParams)
})

# setup_Resampler() strat_boot ----
test_that("setup_Resampler() strat_boot succeeds", {
  rsp <- setup_Resampler(type = "StratBoot")
  expect_s7_class(rsp, StratBootParams)
})

test_that("setup_Resampler() strat_boot fails with invalid train_p", {
  expect_error(
    setup_Resampler(type = "StratBoot", train_p = 1)
  )
})

# setup_Resampler() bootstrap ----
test_that("setup_Resampler() bootstrap succeeds", {
  rsp <- setup_Resampler(type = "Bootstrap")
  expect_s7_class(rsp, BootstrapParams)
})

# setup_Resampler() loocv ----
test_that("setup_Resampler() loocv succeeds", {
  rsp <- setup_Resampler(type = "LOOCV")
  expect_s7_class(rsp, LOOCVParams)
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
