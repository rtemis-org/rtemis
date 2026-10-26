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
  # Compared by value: nanoparquet returns a Date backed by integer, where
  # as.Date() returns one backed by double.
  expect_equal(out, x)
})


# read() URL ----
# A downloader that writes `lines` to the destination file and records the URL
# it was asked for, so the URL path is tested without network access.
.mock_download <- function(lines, env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls[["urls"]] <- character()
  testthat::local_mocked_bindings(
    download_file = function(url, destfile, quiet) {
      calls[["urls"]] <- c(calls[["urls"]], url)
      calls[["destfile"]] <- destfile
      writeLines(lines, destfile)
      0L
    },
    .env = env
  )
  calls
}


test_that("read() downloads a URL and reads it by its extension", {
  calls <- .mock_download(c("a,b", "1,x", "2,y"))
  url <- "https://example.org/data/small.csv?raw=true#top"
  out <- read(url, verbosity = 0L, output = "data.frame")
  expect_identical(calls[["urls"]], url)
  expect_identical(basename(calls[["destfile"]]), "small.csv")
  expect_equal(out, data.frame(a = 1:2, b = c("x", "y")))
  # The temporary directory is deleted once the file is read.
  expect_false(dir.exists(dirname(calls[["destfile"]])))
})


test_that("read() reads a downloaded RDS file as binary", {
  x <- data.frame(a = 1:3, b = c(0.5, 1.5, 2.5))
  src <- tempfile(fileext = ".rds")
  on.exit(unlink(src), add = TRUE)
  saveRDS(x, src)
  testthat::local_mocked_bindings(
    download_file = function(url, destfile, quiet) {
      file.copy(src, destfile)
      0L
    }
  )
  out <- read(
    "https://example.org/x.rds",
    verbosity = 0L,
    output = "data.frame"
  )
  expect_equal(out, x)
})


test_that("read() rejects URLs it cannot read safely", {
  .mock_download("a\n1")
  expect_error(
    read("ftp://example.org/x.csv", verbosity = 0L),
    "URL schemes not allowed",
    class = "rtemis_input_error"
  )
  expect_error(
    read("https://user:secret@example.org/x.csv", verbosity = 0L),
    "user credentials",
    class = "rtemis_input_error"
  )
  expect_error(
    read("https://example.org/", verbosity = 0L),
    "names no file",
    class = "rtemis_input_error"
  )
  expect_error(
    read("https://example.org/x.csv", datadir = tempdir(), verbosity = 0L),
    "`datadir` applies to local files only",
    class = "rtemis_input_error"
  )
})


test_that("a failed download is an rtemis_io_error naming the URL", {
  testthat::local_mocked_bindings(
    download_file = function(url, destfile, quiet) {
      warning("cannot open URL: HTTP status was '404 Not Found'")
    }
  )
  expect_error(
    read("https://example.org/missing.csv", verbosity = 0L),
    "missing.csv.*404 Not Found",
    class = "rtemis_io_error"
  )
})


test_that("url_filename() keeps the remote name as a safe local name", {
  expect_identical(url_filename("https://h.org/a/b.csv.gz"), "b.csv.gz")
  expect_identical(url_filename("HTTPS://h.org/b.parquet?x=1"), "b.parquet")
  expect_identical(url_filename("https://h.org/my%20data.csv"), "my_20data.csv")
  expect_identical(url_filename("https://h.org/a/b.tsv/"), "b.tsv")
  expect_error(url_filename("https://h.org"), "names no file")
  expect_error(url_filename("https://h.org/.."), "names no file")
})


test_that("read() downloads a URL over the network", {
  skip_on_cran()
  skip_if_offline()
  out <- read(
    "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv",
    verbosity = 0L
  )
  expect_identical(dim(out), c(150L, 5L))
})
