# test_DataProfile.R
# ::rtemis::
# 2026- EDG rtemis.org

# `DataProfile`: the facts a validator needs about a dataset, in a form that
# travels. The properties under test are the ones a second implementation has
# to reproduce -- the type vocabulary, and the shapes that make the document
# small and iterable.

# %% profile_dtype ----
test_that("every R column type maps onto a declared token", {
  cases <- list(
    number = c(1.5, 2.5),
    integer = 1:3,
    categorical = factor(c("a", "b")),
    string = c("a", "b"),
    boolean = c(TRUE, FALSE),
    temporal = Sys.Date() + 1:2
  )
  for (expected in names(cases)) {
    expect_identical(
      profile_dtype(cases[[expected]]),
      expected,
      info = expected
    )
  }
  expect_true(all(vapply(
    names(cases),
    function(n) n %in% PROFILE_DTYPES,
    logical(1L)
  )))
})


test_that("a Date is temporal, not a number", {
  # `is.numeric()` is FALSE for a Date but TRUE for its storage, and an ordering
  # that reached the numeric branch first would call it a number -- letting
  # FEATURE_TYPE_UNSUPPORTED pass a column `train()` rejects.
  expect_identical(profile_dtype(Sys.Date() + 1:3), "temporal")
  expect_identical(profile_dtype(Sys.time() + 1:3), "temporal")
})


test_that("an ordered factor is categorical", {
  expect_identical(
    profile_dtype(factor(c("a", "b"), ordered = TRUE)),
    "categorical"
  )
})


test_that("an unmapped type falls back to a declared token", {
  # Never the class name: a token PROFILE_DTYPES does not declare would fail
  # the schema and any evaluator reading it.
  expect_identical(profile_dtype(complex(real = 1, imaginary = 1)), "other")
  expect_identical(profile_dtype(list(1, 2)), "other")
})


# %% data_profile ----
test_that("data_profile() measures columns in order", {
  set.seed(2026L)
  x <- data.frame(
    num = c(1, 2, 2, NA),
    fct = factor(c("a", "b", "a", "b")),
    txt = c("p", "q", "r", "s"),
    stringsAsFactors = FALSE
  )
  p <- data_profile(x)
  expect_s7_class(p, DataProfile)
  expect_identical(p@n_rows, 4L)
  expect_identical(p@columns[["name"]], c("num", "fct", "txt"))
  expect_identical(p@columns[["dtype"]], c("number", "categorical", "string"))
  # Distinct counts exclude missing values: an encoder makes no column for NA.
  expect_identical(p@columns[["n_distinct"]], c(2L, 2L, 4L))
  expect_identical(p@columns[["n_missing"]], c(1L, 0L, 0L))
  expect_identical(p@n_complete_cases, 3L)
})


test_that("level counts are carried in long form for categoricals only", {
  x <- data.frame(
    g = factor(c("a", "a", "b")),
    n = c(1, 2, 3),
    s = c("p", "q", "r"),
    stringsAsFactors = FALSE
  )
  p <- data_profile(x)
  expect_identical(names(p@level_counts), c("column", "level", "n"))
  expect_identical(p@level_counts[["column"]], c("g", "g"))
  expect_identical(p@level_counts[["level"]], c("a", "b"))
  expect_identical(p@level_counts[["n"]], c(2L, 1L))
})


test_that("a high-cardinality categorical column carries no level counts", {
  # The bound is what keeps the document a description rather than a copy of
  # the column: an identifier read as a category would put one row per case in.
  n <- PROFILE_MAX_LEVELS + 1L
  x <- data.frame(id = factor(seq_len(n)), y = rnorm(n))
  p <- data_profile(x)
  expect_identical(NROW(p@level_counts), 0L)
  # The distinct count is still reported, so DIM_P_GT_N still knows the width.
  expect_identical(p@columns[["n_distinct"]][[1L]], n)
})


test_that("an unused factor level is not a level the data takes", {
  # Observed only: `n_distinct` counts what is there, and a CSV read anywhere
  # else has no declared-but-unused levels to report.
  x <- data.frame(g = factor(c("a", "a"), levels = c("a", "b")))
  p <- data_profile(x)
  expect_identical(p@level_counts[["level"]], "a")
  expect_identical(p@columns[["n_distinct"]], 1L)
})


test_that("duplicates and fingerprint are opt-out and opt-in", {
  x <- data.frame(a = c(1, 1, 2), b = c(1, 1, 3))
  expect_identical(data_profile(x)@n_duplicates, 1L)
  expect_null(data_profile(x, n_duplicates = FALSE)@n_duplicates)
  expect_null(data_profile(x)@fingerprint)
  fp <- data_profile(x, fingerprint = TRUE)@fingerprint
  expect_s7_class(fp, DataFingerprint)
  expect_identical(fp@n_rows, 3L)
})


test_that("the profile is bounded by columns, not rows", {
  # The property that lets it travel: 10x the rows must not grow the document.
  small <- data_profile(data.frame(
    g = factor(rep(c("a", "b"), 5L)),
    n = rnorm(10L)
  ))
  large <- data_profile(data.frame(
    g = factor(rep(c("a", "b"), 50L)),
    n = rnorm(100L)
  ))
  expect_identical(NROW(small@columns), NROW(large@columns))
  expect_identical(NROW(small@level_counts), NROW(large@level_counts))
})


test_that("data_profile() accepts every tabular structure", {
  x <- data.frame(a = 1:3, b = factor(c("x", "y", "x")))
  expect_identical(
    data_profile(x)@columns,
    data_profile(data.table::as.data.table(x))@columns
  )
})


# %% to_json ----
test_that("to_json() emits the shapes the wire needs", {
  j <- to_json(data_profile(data.frame(g = factor(c("a", "b")), n = c(1, 2))))
  expect_identical(j[[".class"]], "DataProfile")
  # Row-oriented: an array of objects, which is what a filter iterates -- not
  # an object of parallel arrays, which one cannot.
  txt <- as.character(jsonlite::toJSON(j[["columns"]]))
  expect_match(txt, '^\\[\\{', info = txt)
  expect_match(txt, '\\{"name":"g","dtype":"categorical"', fixed = FALSE)
})
