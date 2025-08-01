# test-Hyperparameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# Hyperparameters ----
hpr <- Hyperparameters(
  algorithm = "Custom",
  hyperparameters = list(alpha = c(0, 1), beta = 2),
  tunable_hyperparameters = "alpha",
  fixed_hyperparameters = "beta"
)
test_that("Hyperparameters succeeds", {
  expect_s7_class(hpr, Hyperparameters)
})

test_that("`tuned` is set correctly", {
  expect_identical(hpr@tuned, 0L)
})

# CARTHyperparameters ----
test_that("CARTHyperparameters() errors", {
  expect_error(CARTHyperparameters())
})

# get_params_need_tuning ----
test_that("get_params_need_tuning() succeeds", {
  expect_type(get_params_need_tuning(hpr), "list")
})

# Check printing of hp that need tuning ----
setup_CART()
setup_CART(prune_cp = c(.001, .01, .1))
setup_CART(prune_cp = c(.001, .01, .1), minsplit = c(2L, 10L))
setup_CART(
  prune_cp = c(.001, .01, .1),
  minsplit = c(2L, 10L),
  minbucket = c(1L, 10L)
)

# CARTHyperparameters ----
# setup_CART ----
cart_hpr <- setup_CART()
test_that("setup_CART() succeeds", {
  expect_s7_class(cart_hpr, CARTHyperparameters)
})

# needs_tuning ----
test_that("needs_tuning() succeeds", {
  expect_type(needs_tuning(cart_hpr), "logical")
})

# GLMNETHyperparameters ----
test_that("GLMNETHyperparameters() errors", {
  expect_error(GLMNETHyperparameters())
})

# setup_GLMNET ----
test_that("setup_GLMNET() succeeds", {
  expect_s7_class(setup_GLMNET(), GLMNETHyperparameters)
})

# LightCARTHyperparameters ----
test_that("LightCARTHyperparameters() errors", {
  expect_error(LightCARTHyperparameters())
})

# setup_LightCART ----
test_that("setup_LightCART() succeeds", {
  expect_s7_class(setup_LightCART(), LightCARTHyperparameters)
})

# LightRFHyperparameters ----
test_that("LightRFHyperparameters() errors", {
  expect_error(LightRFHyperparameters())
})

# setup_LightRF ----
lrf_hpr <- setup_LightRF()
lrf_hpr
test_that("setup_LightRF() succeeds", {
  expect_s7_class(setup_LightRF(), LightRFHyperparameters)
})

# LightGBMHyperparameters ----
test_that("LightGBMHyperparameters() errors", {
  expect_error(LightGBMHyperparameters())
})

# setup_LightGBM ----
lgbm_hpr <- setup_LightGBM(
  num_leaves = c(4, 8, 16),
  learning_rate = c(.001, .01, .1)
)
test_that("setup_LightGBM() succeeds", {
  expect_s7_class(setup_LightGBM(), LightGBMHyperparameters)
})

# LightRuleFitHyperparameters ----
test_that("LightRuleFitHyperparameters() errors", {
  expect_error(LightRuleFitHyperparameters())
})

# setup_LightRuleFit ----
lrft_hpr <- setup_LightRuleFit()
lrft_hpr
test_that("setup_LightRuleFit() succeeds", {
  expect_s7_class(setup_LightRuleFit(), LightRuleFitHyperparameters)
})

# IsotonicHyperparameters ----
test_that("IsotonicHyperparameters() errors", {
  expect_error(IsotonicHyperparameters())
})

# setup_Isotonic ----
test_that("setup_Isotonic() succeeds", {
  expect_s7_class(setup_Isotonic(), IsotonicHyperparameters)
})

# RadialSVMHyperparameters ----
test_that("RadialSVMHyperparameters() errors", {
  expect_error(RadialSVMHyperparameters())
})

# setup_LinearSVM ----
test_that("setup_LinearSVM() succeeds", {
  expect_s7_class(setup_LinearSVM(), LinearSVMHyperparameters)
})

# setup_RadialSVM ----
test_that("setup_RadialSVM() succeeds", {
  expect_s7_class(setup_RadialSVM(), RadialSVMHyperparameters)
})

# TabNetHyperparameters ----
test_that("TabNetHyperparameters() errors", {
  expect_error(TabNetHyperparameters())
})

# setup_TabNet ----
test_that("setup_TabNet() succeeds", {
  expect_s7_class(setup_TabNet(), TabNetHyperparameters)
})

# setup_Ranger ----
test_that("setup_Ranger() succeeds", {
  expect_s7_class(setup_Ranger(), RangerHyperparameters)
})
