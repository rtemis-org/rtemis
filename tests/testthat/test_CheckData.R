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
})
