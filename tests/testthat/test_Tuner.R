# test-Tuner.R
# ::rtemis::
# 2025 EDG rtemis.org

# Note: Actual tuning is tested in test-Supervised.R with `train()`

# TunerConfig ----
tn_pr <- setup_GridSearch()
tn_pr
desc(tn_pr)
test_that("TunerConfig succeeds", {
  expect_s7_class(TunerConfig(), TunerConfig)
})

# setup_GridSearch() ----
test_that("setup_GridSearch() succeeds", {
  expect_s7_class(setup_GridSearch(), GridSearchConfig)
})
