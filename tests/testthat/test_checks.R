# test_checks.R
# ::rtemis::
# 2025 EDG rtemis.org

# Test do_call ----
test_that("do_call() succeeds", {
  expect_equal(do_call(sum, list(1, 2, 3)), 6)
})
