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


# %% encoding ----
test_that("encoding names the byte recipe, and carries its format version", {
  expect_identical(
    data_fingerprint(iris, method = "object")@encoding,
    paste0("r-serialize-v", DATA_HASH_SERIALIZE_VERSION)
  )
  # The version is in the token, so bumping the pinned constant renames the
  # encoding and old fingerprints report as not comparable rather than as
  # different data -- which is what that constant's own comment demands.
  expect_match(
    data_fingerprint(iris)@encoding,
    as.character(DATA_HASH_SERIALIZE_VERSION),
    fixed = TRUE
  )
  skip_if_not_installed("arrow")
  expect_identical(
    data_fingerprint(iris, method = "table")@encoding,
    "arrow-ipc"
  )
})


test_that("a DataFingerprint without an encoding is rejected", {
  # It is the field that decides comparability, so a fingerprint lacking one
  # cannot be compared with anything and is not a fingerprint.
  expect_error(
    DataFingerprint(hash = "abc"),
    "encoding"
  )
})


test_that("two implementations' 'object' hashes are not comparable", {
  # The whole reason `encoding` is on the wire. A Python fingerprint says
  # `method = "object"` exactly as R's does, so keying comparability on `method`
  # would compare pickle bytes against R serialization bytes and report the
  # result as *different data* -- a confident wrong answer where "not
  # comparable" is the truth.
  r_fp <- data_fingerprint(iris)
  foreign <- DataFingerprint(
    method = "object",
    encoding = "python-pickle-v5",
    algorithm = "sha256",
    hash = r_fp@hash,
    n_rows = r_fp@n_rows,
    n_cols = r_fp@n_cols,
    column_names = r_fp@column_names,
    language = "Python",
    data_structure = "DataFrame"
  )
  expect_identical(foreign@method, r_fp@method)
  expect_false(same_data(r_fp, foreign))
  expect_match(fingerprint_diff(r_fp, foreign), "not comparable")
  expect_match(fingerprint_diff(r_fp, foreign), "python-pickle-v5")
})


# %% language and data_structure ----
test_that("a fingerprint records what produced it", {
  # Recorded facts, not a claim about who else could reproduce the digest.
  # Whether a second implementation can is a question a specification answers,
  # and none exists; see `plan/fingerprint-portability.md`.
  fp <- data_fingerprint(iris, method = "object")
  expect_identical(fp@language, "R")
  expect_identical(fp@data_structure, "data.frame")
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(iris, f, row.names = FALSE)
  from_file <- data_fingerprint(iris, method = "file", source = f)
  expect_identical(from_file@language, "R")
  expect_identical(from_file@data_structure, "data.frame")
})


test_that("'table' records the container even though it normalizes it away", {
  skip_if_not_installed("arrow")
  # Recorded because it is a fact about the input, but under this encoding it
  # cannot be the cause of a mismatch: one canonical form is hashed whatever
  # the container, which is what the container test below pins.
  expect_identical(
    data_fingerprint(iris, method = "table")@data_structure,
    "data.frame"
  )
  expect_identical(
    data_fingerprint(
      data.table::as.data.table(iris),
      method = "table"
    )@data_structure,
    "data.table"
  )
})


test_that("data_structure names the container, not the classes it inherits", {
  # A tibble is c("tbl_df", "tbl", "data.frame") and a matrix is
  # c("matrix", "array"); the form it was held in is the first, and it is the
  # first thing to look at when two "object" digests over one dataset disagree.
  expect_identical(
    data_fingerprint(data.table::as.data.table(iris))@data_structure,
    "data.table"
  )
  expect_identical(
    data_fingerprint(tibble::as_tibble(iris))@data_structure,
    "tbl_df"
  )
  expect_identical(
    data_fingerprint(as.matrix(iris[, 1:4]))@data_structure,
    "matrix"
  )
})


test_that("a foreign fingerprint keeps the values its writer recorded", {
  # Stored, so a record written elsewhere reads back as what it is rather than
  # being described by this build's assumptions.
  foreign <- DataFingerprint(
    method = "object",
    encoding = "julia-serialize-v1",
    hash = "abc",
    language = "Julia",
    data_structure = "DataFrame"
  )
  expect_identical(foreign@language, "Julia")
  expect_identical(foreign@data_structure, "DataFrame")
  # And it is still not comparable with anything this build writes.
  expect_false(same_data(foreign, data_fingerprint(iris)))
})


test_that("a fingerprint cannot omit what produced it", {
  expect_error(
    DataFingerprint(
      method = "object",
      encoding = "r-serialize-v3",
      hash = "abc",
      data_structure = "data.frame"
    ),
    "language"
  )
  expect_error(
    DataFingerprint(
      method = "object",
      encoding = "r-serialize-v3",
      hash = "abc",
      language = "R"
    ),
    "data_structure"
  )
})


# %% method "object" ----
test_that("'object' hashes R representation, not just logical content", {
  # This is exactly what `@data_structure` is recorded for: the same logical
  # table in a different container differs.
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
  # Computed independently, from the whole file read into memory: the streaming
  # connection `.hash_file()` uses must reach the same hash.
  expect_identical(
    fp@hash,
    as.vector(as.character(openssl::sha256(readBin(f, "raw", file.size(f)))))
  )
})


# %% method "table" ----
test_that("'table' hashes logical content across R containers", {
  skip_if_not_installed("arrow")
  # The whole point of this method: every container the package accepts, plus
  # the matrix `decomp()` reduces to, must give one hash. arrow stores R
  # class/attributes under the schema metadata key "r", which differs between
  # them and `.hash_table()` strips; `as_arrow_table()` has no matrix method, so
  # that conversion happens there too.
  # This is an R-side property and says nothing about whether a second
  # implementation reproduces the digest, which turns on what that
  # implementation writes; see `.hash_table()`.
  one_table <- iris[, 1:4]
  containers <- list(
    as.data.frame(one_table),
    tibble::as_tibble(one_table),
    data.table::as.data.table(one_table),
    as.data.frame(data.table::as.data.table(one_table)),
    as.matrix(one_table)
  )
  hashes <- vapply(
    containers,
    function(z) data_fingerprint(z, method = "table")@hash,
    character(1L)
  )
  expect_length(unique(hashes), 1L)
})


test_that("'object' sees ALTREP where 'table' does not", {
  # The least visible representation difference of the set. These two frames
  # are `identical()`; one column is still a compact integer sequence and the
  # other was materialized by touching an element, and serialization version 3
  # writes the compact form compactly. Ordinary operations materialize, so this
  # is reachable without anyone doing anything unusual.
  lazy <- data.frame(i = 1:1000L)
  eager <- lazy
  eager[["i"]][[1L]] <- 1L
  expect_true(identical(lazy, eager))
  expect_false(same_data(data_fingerprint(lazy), data_fingerprint(eager)))
  skip_if_not_installed("arrow")
  expect_true(
    same_data(
      data_fingerprint(lazy, method = "table"),
      data_fingerprint(eager, method = "table")
    )
  )
})


test_that("'object' does not survive the container, and says so", {
  # The counterpart of the above, pinned because it is the default method and
  # the difference is invisible at the R level: these two are `identical()`.
  a <- as.data.frame(iris[, 1:4])
  b <- as.data.frame(data.table::as.data.table(iris[, 1:4]))
  expect_true(identical(a, b))
  expect_false(same_data(data_fingerprint(a), data_fingerprint(b)))
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
  fp <- data_fingerprint(iris, algorithm = "blake2b")
  expect_identical(fp@algorithm, "blake2b")
  expect_false(identical(fp@hash, data_fingerprint(iris)@hash))
})


test_that("every accepted algorithm produces a hash", {
  # Guards the enum against `.hash_bytes()`: a name accepted by the validator
  # but missing a dispatch branch would only fail at hashing time.
  for (algorithm in DATA_HASH_ALGORITHMS) {
    fp <- data_fingerprint(iris, algorithm = algorithm)
    expect_identical(fp@algorithm, algorithm)
    expect_match(fp@hash, "^[0-9a-f]+$")
    # A bare character, carrying no attributes. The hash backend returns a
    # classed one, `prop_string` accepts it because it is still a character,
    # and the class then breaks `write_record()` at `jsonlite::toJSON()`.
    expect_null(attributes(fp@hash))
  }
})


test_that("'object' hashes exclude the serialization header", {
  # The header names the R that wrote the stream, so a fingerprint that
  # included it would differ across R builds and locales for identical data.
  full <- serialize(iris, connection = NULL, version = 3L)
  offset <- .serialization_offset(full)
  expect_gt(offset, 0L)
  expect_lt(offset, length(full))
  # Seeking past the header must give what dropping it would have given.
  expect_identical(
    data_fingerprint(iris)@hash,
    as.vector(as.character(openssl::sha256(full[-seq_len(offset)])))
  )
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


test_that("fingerprint_diff() claims changed values only where it can know", {
  # The residual branch is reached by elimination, so what it may assert
  # depends on what the method hashed. An "object" hash sees representation as
  # well as content: these two frames are `identical()` and differ only in the
  # order their attributes are stored, which serializes differently.
  a <- as.data.frame(iris[, 1:4])
  b <- as.data.frame(data.table::as.data.table(iris[, 1:4]))
  expect_true(identical(a, b))
  expect_match(
    fingerprint_diff(data_fingerprint(a), data_fingerprint(b)),
    "different values or representation"
  )
  # Arrow normalizes the representation away, so there the values are the only
  # thing left to differ.
  skip_if_not_installed("arrow")
  changed <- iris
  changed[["Sepal.Length"]][[1L]] <- 99
  expect_identical(
    fingerprint_diff(
      data_fingerprint(iris, method = "table"),
      data_fingerprint(changed, method = "table")
    ),
    "same shape and column names, different values"
  )
})


test_that("fingerprint_diff() names the container when that is what differs", {
  # The recorded fact earning its keep: the same data in two containers is the
  # most common reason two "object" digests disagree, and the message says so
  # rather than leaving a reader to hunt for a change to the values.
  message <- fingerprint_diff(
    data_fingerprint(iris),
    data_fingerprint(data.table::as.data.table(iris))
  )
  expect_match(message, "data.frame vs data.table", fixed = TRUE)
  expect_match(message, "r-serialize-v3", fixed = TRUE)
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
  mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  expect_s7_class(mod@data_fingerprint, DataFingerprint)
  expect_identical(mod@data_fingerprint@n_rows, 150L)
  expect_identical(mod@data_fingerprint@n_cols, 5L)
  # It is the fingerprint of the data as supplied.
  expect_true(same_data(mod@data_fingerprint, data_fingerprint(iris)))
})


test_that("two models on the same data share a fingerprint", {
  m1 <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  m2 <- train(iris, hyperparameters = setup_LightCART(), verbosity = 0L)
  expect_true(same_data(m1@data_fingerprint, m2@data_fingerprint))
})


# %% repr ----
test_that("repr shows a short hash, dimensions and what computed it", {
  out <- repr(data_fingerprint(iris), output_type = "plain")
  expect_match(out, "DataFingerprint")
  expect_match(out, "150 x 5")
  expect_match(out, "R / data.frame", fixed = TRUE)
  # Abbreviated, not the full 64-character digest.
  expect_false(grepl(data_fingerprint(iris)@hash, out, fixed = TRUE))
})
