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


# %% to_html.CheckData ----
test_that("to_html() renders a CheckData object", {
  # The rendering reads properties by name, so a renamed property breaks it
  # silently as long as nothing calls it. Exercising it here is what keeps the
  # names in step.
  cd <- check_data(iris, name = "iris")
  out <- to_html(cd, name = "iris")
  expect_s3_class(out, "rtemis_html_element")
  expect_match(out, "^<div class=\"checkData\"")
  expect_match(out, "</div>$")
  expect_match(out, "iris", fixed = TRUE)
  # The object's class is reported, from `object_class`.
  expect_match(out, "data.frame", fixed = TRUE)
  expect_match(out, "150", fixed = TRUE)
})


test_that("to_html() reports clean data as such", {
  # iris will not do: rows 102 and 143 are identical, so it has a duplicate.
  x <- data.frame(a = 1:5, b = letters[1:5])
  out <- to_html(check_data(x, name = "clean"), name = "clean")
  expect_match(out, "Everything looks good", fixed = TRUE)
})


test_that("to_html() reports issues and recommendations", {
  x <- data.frame(a = c(1, NA, 3), b = c("p", "q", "r"), const = rep(1, 3L))
  x <- rbind(x, x[1L, ])
  out <- to_html(check_data(x, name = "messy"), name = "messy")
  expect_match(out, "constant", fixed = TRUE)
  expect_match(out, "duplicate", fixed = TRUE)
  expect_match(out, "'NA' values", fixed = TRUE)
  expect_match(out, "Remove the constant", fixed = TRUE)
  expect_false(grepl("Everything looks good", out, fixed = TRUE))
})


test_that("to_html() escapes text that came from the data", {
  x <- data.frame(a = 1:3)
  out <- to_html(check_data(x, name = "a<b&c"), name = "a<b&c")
  expect_match(out, "a&lt;b&amp;c", fixed = TRUE)
  expect_false(grepl(">a<b&c<", out, fixed = TRUE))
})
