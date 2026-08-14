# test_read.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# read() parquet ----
test_that("read() reads a parquet whose strings are an Arrow view type", {
  skip_if_not_installed("arrow")
  path <- test_path("fixtures", "string_view.parquet")

  # The defect this guards: arrow reads the Table and cannot convert it.
  expect_error(
    as.data.frame(arrow::read_parquet(path, as_data_frame = FALSE)),
    "utf8_view"
  )

  out <- read(path, verbosity = 0L)
  expect_identical(names(out), c("id", "name", "score", "grp"))
  expect_identical(
    vapply(out, function(column) class(column)[[1L]], character(1L)),
    c(
      id = "integer",
      name = "character",
      score = "numeric",
      grp = "character"
    )
  )
  expect_identical(out[["id"]], 1:5)
  expect_identical(out[["name"]], c("alpha", "beta", "gamma", "delta", NA))
  expect_identical(out[["grp"]], c("a", "b", "a", "c", "b"))
})


test_that("read() leaves a parquet carrying no view type untouched", {
  skip_if_not_installed("arrow")
  x <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    d = as.Date("2026-01-01") + 0:2,
    f = factor(c("p", "q", "p")),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  arrow::write_parquet(x, path)

  # The cast is a no-op: the Table is returned as it came, so the R attributes
  # arrow stores in the file's metadata still reach the frame.
  tbl <- arrow::read_parquet(path, as_data_frame = FALSE)
  expect_identical(materialize_arrow_views(tbl), tbl)

  out <- read(path, verbosity = 0L, output = "data.frame")
  expect_identical(out, x)
})
