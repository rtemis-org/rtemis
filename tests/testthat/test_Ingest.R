# test_Ingest.R
# ::rtemis::
# 2026- EDG rtemis.org

# `ingest()`: a data file normalized to Parquet, with every reading decision
# taken from the config rather than from a reader's default.
#
# The point of the step is that types are decided *once*. What these assert is
# that the decisions in the config are the ones that reach the file, that the
# file then declares them, and that a setting belonging to another format is
# refused rather than silently ignored.

skip_if_not_installed("arrow")

.csv <- function(text) {
  f <- tempfile(fileext = ".csv")
  writeLines(text, f)
  f
}

.labels <- c("x,g,y", "1.0,a,no", "2.0,b,yes", "3.0,a,no", "4.0,b,yes")

.dtypes <- function(path) {
  data_profile(read(path, verbosity = 0L))@columns[["dtype"]]
}


test_that("a delimited file becomes Parquet that declares its types", {
  # The whole reason the step exists: after it, `g` and `y` are dictionary
  # columns in the file, so every reader agrees without inferring.
  out <- tempfile(fileext = ".parquet")
  m <- ingest(.csv(.labels), out, verbosity = 0L)
  expect_true(file.exists(out))
  expect_identical(.dtypes(out), c("number", "categorical", "categorical"))
  expect_identical(m[["format"]], "delimited")
  expect_identical(m[["n_rows"]], 4L)
})


test_that("the config decides, not the reader's default", {
  # `read()` defaults `character2factor` to FALSE; `setup_DelimitedIngest()` to TRUE.
  out <- tempfile(fileext = ".parquet")
  m <- ingest(
    .csv(.labels),
    out,
    config = setup_DelimitedIngest(character2factor = FALSE),
    verbosity = 0L
  )
  expect_identical(.dtypes(out), c("number", "string", "string"))
  expect_false(m[["config"]][["character2factor"]])
})


test_that("declared types override what the reader inferred", {
  # Inference is a guess made from the values a file happens to hold. Where the
  # user knows, saying so beats every heuristic -- and the record then reports
  # what was declared rather than what was guessed.
  out <- tempfile(fileext = ".parquet")
  ingest(
    .csv(c("code,y", "1,no", "2,yes", "1,no")),
    out,
    config = setup_DelimitedIngest(columns = c(code = "categorical")),
    verbosity = 0L
  )
  # `code` reads as an integer and is declared a category.
  expect_identical(.dtypes(out), c("categorical", "categorical"))
})


test_that("a declared type for a column that is not there is refused", {
  expect_error(
    ingest(
      .csv(.labels),
      tempfile(fileext = ".parquet"),
      config = setup_DelimitedIngest(columns = c(nope = "integer")),
      verbosity = 0L
    ),
    "not in the data",
    class = "rtemis_value_error"
  )
})


# %% The family ----

test_that("a format takes only the settings it has", {
  # The reason this is a family and not one flat config: a Parquet declares its
  # own types, so a separator is not a setting it ignores -- it is not a
  # setting. Offering one would put it in a document that never takes effect.
  expect_error(setup_ParquetIngest(sep = ","), "unused argument")
  expect_error(setup_DelimitedIngest(sheet = 2L), "unused argument")
  expect_error(setup_RDSIngest(reader = "arrow"), "unused argument")
  expect_no_error(setup_DelimitedIngest(sep = ";"))
  expect_no_error(setup_XLSXIngest(sheet = 2L))
})


test_that("each format builds its own class", {
  for (format in names(INGEST_CLASSES)) {
    cfg <- do.call(INGEST_SETUP[[format]], list())
    expect_s7_class(cfg, INGEST_CLASSES[[format]])
    expect_identical(cfg@format, format)
  }
})


test_that("the format is derived from the file, and a config that disagrees is refused", {
  # `format` is what the file *is*. A delimited config handed a Parquet says
  # things a Parquet reader would ignore, and ignoring them is how a run does
  # something other than what it was asked.
  parquet <- tempfile(fileext = ".parquet")
  ingest(.csv(.labels), parquet, verbosity = 0L)
  expect_error(
    ingest(
      parquet,
      tempfile(fileext = ".parquet"),
      config = setup_DelimitedIngest(),
      verbosity = 0L
    ),
    "would not apply",
    class = "rtemis_value_error"
  )
  # With no config, the format comes from the file and it just works.
  expect_no_error(ingest(
    parquet,
    tempfile(fileext = ".parquet"),
    verbosity = 0L
  ))
})


test_that("an extension naming no supported format is refused", {
  f <- tempfile(fileext = ".xyz")
  writeLines("a,b", f)
  expect_error(
    ingest(f, tempfile(fileext = ".parquet"), verbosity = 0L),
    "no supported format",
    class = "rtemis_value_error"
  )
})


# %% The manifest ----

test_that("the manifest names the engine that read the file", {
  # Unrecoverable later: once a native executor exists, nothing else could say
  # which implementation produced a given Parquet.
  m <- ingest(.csv(.labels), tempfile(fileext = ".parquet"), verbosity = 0L)
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


# %% The wire ----

test_that("every format round-trips through its document", {
  for (format in names(INGEST_CLASSES)) {
    cfg <- do.call(INGEST_SETUP[[format]], list())
    f <- tempfile(fileext = ".json")
    write_config(cfg, f, overwrite = TRUE, verbosity = 0L)
    expect_identical(class(read_config(f)), class(cfg), info = format)
  }
})


test_that("declared types survive the document", {
  cfg <- setup_DelimitedIngest(columns = c(Age = "integer", Dx = "categorical"))
  f <- tempfile(fileext = ".json")
  write_config(cfg, f, overwrite = TRUE, verbosity = 0L)
  expect_identical(read_config(f)@columns, cfg@columns)
})
