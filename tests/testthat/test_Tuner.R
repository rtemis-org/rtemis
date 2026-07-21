# test_Tuner.R
# ::rtemis::
# 2025- EDG rtemis.org

# Note: Tuning is tested in test_Supervised.R with `train()`

# TunerConfig ----
tn_pr <- setup_GridSearch()
tn_pr
desc(tn_pr)
test_that("TunerConfig is abstract; subclasses carry the config", {
  # The superclass holds only `type` + the computed `config`; only tuner
  # subclasses can be instantiated.
  expect_error(TunerConfig())
  expect_s7_class(tn_pr, TunerConfig)
  expect_identical(tn_pr@type, "GridSearch")
})

# setup_GridSearch() ----
test_that("setup_GridSearch() succeeds", {
  expect_s7_class(setup_GridSearch(), GridSearchConfig)
})
