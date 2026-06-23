# test_colorsystem.R
# ::rtemis::
# 2025- EDG rtemis.org

# show_col ----
x <- list(
  highlight_col = highlight_col,
  col_warn = col_warn,
  col_error = col_error,
  col_success = col_success,
  col_preprocessor = col_preprocessor,
  col_decom = col_decom,
  col_outer = col_outer,
  col_tuner = col_tuner,
  col_info = col_info
)

out <- show_col(x, title = "rtemis Color System")
cat(out)
test_that("show_col() works", {
  expect_true(is.character(out))
})

# fmt_gradient ----
out <- fmt_gradient(
  "Supervised",
  colors = c(rtemis_colors[["orange"]], rtemis_colors[["red"]]),
  bold = TRUE
)
test_that("fmt_gradient() works", {
  expect_true(is.character(out))
})
