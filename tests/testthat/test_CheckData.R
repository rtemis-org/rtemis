# test_CheckData.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% check_data() ----
test_that("check_data() succeeds", {
  x <- data.frame(
    a = c(1, 2, 3, NA),
    b = c("A", "B", "C", "D"),
    c = c(1.5, NA, 3.5, NA)
  )
  x_cd <- check_data(x, get_na_case_pct = TRUE, get_na_feature_pct = TRUE)
  expect_s7_class(x_cd, CheckData)
  expect_equal(x_cd$n_na, 3)
  expect_equal(x_cd$n_cols_anyna, 2)
  expect_equal(nrow(x_cd$na_feature_pct), 2)
  expect_equal(x_cd$na_feature_pct$Feature, c("a", "c"))
  expect_equal(x_cd$na_feature_pct$Pct_NA, c(0.25, 0.5))
  expect_equal(nrow(x_cd$na_case_pct), 2)
  expect_equal(x_cd$na_case_pct$Case, c(2, 4))
  expect_equal(x_cd$na_case_pct$Pct_NA, c(1 / 3, 2 / 3))
})
