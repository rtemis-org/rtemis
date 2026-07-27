# test_DataFingerprint.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# Minimal stand-in for a fitted model: `warn_fingerprint_mismatch()` reads only
# `@data_fingerprint` and `@algorithm`, so building real models would make these
# tests slow without testing anything more.
Supervised_stub <- function(fingerprint, algorithm = "Stub") {
  S7::new_class(
    name = "SupervisedStub",
    package = NULL,
    properties = list(
      data_fingerprint = NULL | DataFingerprint,
      algorithm = class_character
    )
  )(data_fingerprint = fingerprint, algorithm = algorithm)
}


# %% Construction ----
test_that("data_fingerprint() records hash and structure", {
  fp <- data_fingerprint(iris)
  expect_s7_class(fp, DataFingerprint)
  expect_identical(fp@method, "object")
  expect_identical(fp@algorithm, "sha256")
  # sha256 hex is 64 characters.
  expect_identical(nchar(fp@hash), 64L)
  expect_identical(fp@n_rows, 150L)
  expect_identical(fp@n_cols, 5L)
  expect_identical(fp@column_names, names(iris))
  expect_null(fp@source)
})


test_that("data_fingerprint() is deterministic", {
  expect_identical(data_fingerprint(iris)@hash, data_fingerprint(iris)@hash)
})


test_that("a DataFingerprint without a hash is rejected", {
  # There is no meaningful default; bare construction must fail informatively.
  expect_error(DataFingerprint(), "hash")
})


test_that("data_fingerprint() validates method and algorithm", {
  expect_error(
    data_fingerprint(iris, method = "bogus"),
    class = "rtemis_value_error"
  )
  expect_error(
    data_fingerprint(iris, algorithm = "bogus"),
    class = "rtemis_value_error"
  )
})


# %% portability ----
test_that("portability is computed from method and cannot be set", {
  expect_identical(
    data_fingerprint(iris, method = "object")@portability,
    "single_language"
  )
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(iris, f, row.names = FALSE)
  expect_identical(
    data_fingerprint(iris, method = "file", source = f)@portability,
    "cross_language"
  )
  # A computed property has no setter: a fingerprint cannot make a false claim
  # about its own reproducibility.
  fp <- data_fingerprint(iris)
  expect_error(fp@portability <- "cross_language")
})


# %% method "object" ----
test_that("'object' hashes R representation, not just logical content", {
  # This is exactly why it is single_language: the same logical table in a
  # different container differs.
  dt <- data.table::as.data.table(iris)
  expect_false(
    same_data(data_fingerprint(iris), data_fingerprint(dt))
  )
})


test_that("'object' detects a changed value", {
  changed <- iris
  changed[["Sepal.Length"]][[1L]] <- 99
  expect_false(
    same_data(data_fingerprint(iris), data_fingerprint(changed))
  )
})


# %% method "file" ----
test_that("'file' requires a source and hashes its bytes", {
  expect_error(
    data_fingerprint(iris, method = "file"),
    class = "rtemis_null_input"
  )
  expect_error(
    data_fingerprint(iris, method = "file", source = "no-such-file.csv"),
    class = "rtemis_value_error"
  )
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(iris, f, row.names = FALSE)
  fp <- data_fingerprint(iris, method = "file", source = f)
  expect_identical(fp@source, f)
  expect_identical(fp@hash, digest::digest(file = f, algo = "sha256"))
})


# %% method "table" ----
test_that("'table' hashes logical content across R containers", {
  skip_if_not_installed("arrow")
  # The whole point of this method: a data.frame and a data.table holding
  # identical data must agree, or the "cross_language" claim is false. arrow
  # stores R class/attributes under the schema metadata key "r", which differs
  # between the two; `.hash_table()` strips it.
  dt <- data.table::as.data.table(iris)
  expect_true(
    same_data(
      data_fingerprint(iris, method = "table"),
      data_fingerprint(dt, method = "table")
    )
  )
})


test_that("'table' still detects a changed value", {
  skip_if_not_installed("arrow")
  changed <- iris
  changed[["Sepal.Length"]][[1L]] <- 99
  expect_false(
    same_data(
      data_fingerprint(iris, method = "table"),
      data_fingerprint(changed, method = "table")
    )
  )
})


# %% algorithm ----
test_that("algorithm is settable and recorded", {
  fp <- data_fingerprint(iris, algorithm = "xxh3_64")
  expect_identical(fp@algorithm, "xxh3_64")
  expect_false(identical(fp@hash, data_fingerprint(iris)@hash))
})


# %% same_data ----
test_that("same_data() refuses to compare incomparable fingerprints", {
  skip_if_not_installed("arrow")
  # Different methods measure different things; reporting them as unequal
  # would be as wrong as reporting them equal.
  expect_false(
    same_data(
      data_fingerprint(iris, method = "object"),
      data_fingerprint(iris, method = "table")
    )
  )
  expect_false(
    same_data(
      data_fingerprint(iris),
      data_fingerprint(iris, algorithm = "md5")
    )
  )
})


test_that("same_data() type-checks its arguments", {
  expect_error(
    same_data(data_fingerprint(iris), "x"),
    class = "rtemis_type_error"
  )
})


# %% fingerprint_diff ----
test_that("fingerprint_diff() reports how two datasets differ", {
  fp <- data_fingerprint(iris)
  expect_null(fingerprint_diff(fp, data_fingerprint(iris)))
  expect_match(
    fingerprint_diff(fp, data_fingerprint(iris[1:100, ])),
    "number of rows"
  )
  expect_match(
    fingerprint_diff(fp, data_fingerprint(iris[, 1:4])),
    "number of columns"
  )
  renamed <- iris
  names(renamed)[[1L]] <- "X"
  expect_match(
    fingerprint_diff(fp, data_fingerprint(renamed)),
    "column names"
  )
  changed <- iris
  changed[["Sepal.Length"]][[1L]] <- 99
  expect_match(
    fingerprint_diff(fp, data_fingerprint(changed)),
    "different values"
  )
})


test_that("fingerprint_diff() flags incomparable fingerprints as such", {
  expect_match(
    fingerprint_diff(
      data_fingerprint(iris),
      data_fingerprint(iris, algorithm = "md5")
    ),
    "not comparable"
  )
})


# %% warn_fingerprint_mismatch ----
test_that("warn_fingerprint_mismatch() stays quiet on matching data", {
  fp <- data_fingerprint(iris)
  models <- list(
    a = Supervised_stub(fp),
    b = Supervised_stub(fp)
  )
  expect_false(warn_fingerprint_mismatch(models))
})


test_that("warn_fingerprint_mismatch() reports how the data differ", {
  models <- list(
    full = Supervised_stub(data_fingerprint(iris)),
    subset = Supervised_stub(data_fingerprint(iris[1:100, ]))
  )
  # Printed, not a real R warning: comparing models across different data is
  # often deliberate, so it must not be escalatable to an error.
  expect_message(
    expect_true(warn_fingerprint_mismatch(models)),
    "different number of rows"
  )
  expect_no_warning(warn_fingerprint_mismatch(models))
})


test_that("warn_fingerprint_mismatch() skips models without a fingerprint", {
  # Degrades quietly rather than crying wolf: a model trained before
  # fingerprinting, or a nested sub-model, carries none.
  models <- list(
    a = Supervised_stub(data_fingerprint(iris)),
    b = Supervised_stub(NULL)
  )
  expect_false(warn_fingerprint_mismatch(models))
  # Fewer than two comparable fingerprints means nothing to compare.
  expect_false(warn_fingerprint_mismatch(list(a = Supervised_stub(NULL))))
})


# %% train() integration ----
test_that("train() attaches a fingerprint of the training data", {
  mod <- train(iris, algorithm = "CART", verbosity = 0L)
  expect_s7_class(mod@data_fingerprint, DataFingerprint)
  expect_identical(mod@data_fingerprint@n_rows, 150L)
  expect_identical(mod@data_fingerprint@n_cols, 5L)
  # It is the fingerprint of the data as supplied.
  expect_true(same_data(mod@data_fingerprint, data_fingerprint(iris)))
})


test_that("two models on the same data share a fingerprint", {
  m1 <- train(iris, algorithm = "CART", verbosity = 0L)
  m2 <- train(iris, algorithm = "LightCART", verbosity = 0L)
  expect_true(same_data(m1@data_fingerprint, m2@data_fingerprint))
})


# %% repr ----
test_that("repr shows a short hash, dimensions and portability", {
  out <- repr(data_fingerprint(iris), output_type = "plain")
  expect_match(out, "DataFingerprint")
  expect_match(out, "150 x 5")
  expect_match(out, "single_language")
  # Abbreviated, not the full 64-character digest.
  expect_false(grepl(data_fingerprint(iris)@hash, out, fixed = TRUE))
})
