# test_CheckData.R
# ::rtemis::
# 2025 EDG rtemis.org

# check_data() ----
check_data(iris)
test_that("check_data() succeeds", {
  expect_s7_class(check_data(iris), CheckData)
})
