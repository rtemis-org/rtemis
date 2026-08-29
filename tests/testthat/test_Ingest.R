# test_Ingest.R
# ::rtemis::
# 2026- EDG rtemis.org

# `ingest()`: a data file normalized to Parquet, with every reading decision
# taken from the config rather than from a reader's default.
#
# The point of the step is that types are decided *once*. What these assert is
# that the decisions in the config are the ones that reach the file, and that
# the file then declares them -- so nothing downstream has to infer.

skip_if_not_installed("arrow")

.csv <- function(text) {
  f <- tempfile(fileext = ".csv")
  writeLines(text, f)
  f
}

.labels <- c("x,g,y", "1.0,a,no", "2.0,b,yes", "3.0,a,no", "4.0,b,yes")


test_that("a delimited file becomes Parquet that declares its types", {
  # The whole reason the step exists: after it, `g` and `y` are dictionary
  # columns in the file, so every reader agrees without inferring.
  out <- tempfile(fileext = ".parquet")
  m <- ingest(.csv(.labels), out, verbosity = 0L)
  expect_true(file.exists(out))
  dtypes <- data_profile(read(out, verbosity = 0L))@columns[["dtype"]]
  expect_identical(dtypes, c("number", "categorical", "categorical"))
  expect_identical(m[["n_rows"]], 4L)
  expect_identical(m[["n_cols"]], 3L)
})


test_that("the config decides, not the reader's default", {
  # `read()` defaults `character2factor` to FALSE; `setup_Ingest()` to TRUE.
  # Both are reachable, and which one ran is in the manifest.
  out <- tempfile(fileext = ".parquet")
  m <- ingest(
    .csv(.labels),
    out,
    config = setup_Ingest(character2factor = FALSE),
    verbosity = 0L
  )
  dtypes <- data_profile(read(out, verbosity = 0L))@columns[["dtype"]]
  expect_identical(dtypes, c("number", "string", "string"))
  expect_false(m[["config"]][["character2factor"]])
})


test_that("the manifest names the engine that read the file", {
  # Unrecoverable later: once a native executor exists, nothing else could say
  # which implementation produced a given Parquet.
  out <- tempfile(fileext = ".parquet")
  m <- ingest(.csv(.labels), out, verbosity = 0L)
  expect_identical(m[["engine"]], "R")
  expect_identical(
    m[["rtemis_version"]],
    as.character(utils::packageVersion("rtemis"))
  )
  expect_false(is.null(m[["data_input"]]))
})


test_that("an existing output is not overwritten without being asked", {
  out <- tempfile(fileext = ".parquet")
  ingest(.csv(.labels), out, verbosity = 0L)
  expect_error(
    ingest(.csv(.labels), out, verbosity = 0L),
    "exists",
    class = "rtemis_file_error"
  )
  expect_no_error(ingest(.csv(.labels), out, overwrite = TRUE, verbosity = 0L))
})


test_that("ingesting a Parquet preserves what it already declares", {
  # A Parquet in is not a no-op -- the config still applies -- but a column that
  # already declares itself a factor stays one.
  first <- tempfile(fileext = ".parquet")
  ingest(.csv(.labels), first, verbosity = 0L)
  second <- tempfile(fileext = ".parquet")
  ingest(first, second, verbosity = 0L)
  expect_identical(
    data_profile(read(second, verbosity = 0L))@columns[["dtype"]],
    c("number", "categorical", "categorical")
  )
})


test_that("setup_Ingest rejects a value outside its enum", {
  expect_error(setup_Ingest(delim_reader = "nope"))
  expect_error(setup_Ingest(parquet_reader = "nope"))
  expect_error(setup_Ingest(xlsx_sheet = 0L))
})
