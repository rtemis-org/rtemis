# test_colorsystem.R
# ::rtemis::
# 2025 EDG rtemis.org

# show_col ----
x <- list(
  highlight_col = highlight_col,
  col_object = col_object,
  col_outer = col_outer,
  col_tuner = col_tuner,
  col_info = col_info
)

out <- show_col(x, title = "rtemis Color System")
test_that("show_col() works", {
  expect_true(is.character(out))
})

# fmt_gradient ----
out <- fmt_gradient(
  "Supervised",
  colors = c(rtemis_teal, rtemis_light_teal),
  bold = TRUE
)
test_that("fmt_gradient() works", {
  expect_true(is.character(out))
})
