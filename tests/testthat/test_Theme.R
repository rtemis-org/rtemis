# test_Theme.R
# ::rtemis::
# 2025 EDG rtemis.org

# Theme ----
test_that("Theme succeeds", {
  expect_s7_class(Theme(), Theme)
})

# theme_black ----
test_that("theme_black succeeds", {
  expect_s7_class(theme_black(), Theme)
})

# theme_blackgrid
test_that("theme_blackgrid succeeds", {
  expect_s7_class(theme_blackgrid(), Theme)
})

# theme_light ----
test_that("theme_light succeeds", {
  expect_s7_class(theme_white(), Theme)
})

# Test `$` and `[[` methods ----
theme <- theme_darkgraygrid()
test_that("Theme$ and Theme[[ succeed", {
  expect_equal(theme[["fg"]], "#ffffff")
  expect_equal(theme[["fg"]], theme[["fg"]])
})
