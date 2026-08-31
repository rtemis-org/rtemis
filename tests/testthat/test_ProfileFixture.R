# test_ProfileFixture.R
# ::rtemis::
# 2026- EDG rtemis.org

# rtemis's half of the profile conformance contract.
#
# `profile/v1/fixture.json` names a Parquet file per case and carries the
# `profile/v1` document rtemis measures for it. The CLI's polars profiler and
# rtemislive's DuckDB one read the same file and must produce the same document;
# this asserts the reference implementation still produces what was recorded.
#
# Parquet because it *declares* its types. A delimited file would make this a
# test of three type inferences rather than of three profilers, and the
# inferences legitimately differ -- which is what the ingest step exists to
# settle.
#
# It matters because profiles are *inputs* to `checks/v1/corpus.json`. Every
# host can evaluate every rule identically and still answer differently, if they
# measured the dataset differently -- and nothing else compares the three.

.fixture_path <- function() {
  # Generated into the schema repo by `just schemas`; the copy under `inst/` is
  # what a check without that checkout reads.
  system.file("profile", "fixture.json", package = "rtemis")
}


test_that("the reference implementation reproduces the recorded fixture", {
  path <- .fixture_path()
  skip_if(
    !nzchar(path) || !file.exists(path),
    "no vendored profile fixture; run `just schemas`"
  )
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_gt(length(doc[["cases"]]), 0L)

  for (case in doc[["cases"]]) {
    f <- system.file(
      "profile",
      "fixtures",
      basename(case[["data"]]),
      package = "rtemis"
    )
    expect_true(nzchar(f), info = case[["data"]])
    measured <- to_json(data_profile(read(f, verbosity = 0L)))
    # Compared through JSON so the assertion is about the *document*, which is
    # what every other implementation sees, rather than about R's object.
    expect_identical(
      jsonlite::toJSON(measured, auto_unbox = TRUE, na = "null", null = "null"),
      jsonlite::toJSON(
        case[["profile"]],
        auto_unbox = TRUE,
        na = "null",
        null = "null"
      ),
      info = case[["id"]]
    )
  }
})


# `other` is the fallback for a type nothing else matches -- a BLOB, a struct, a
# list -- and none of them belongs in a supervised frame, so no fixture case
# produces one. Every other dtype is covered, `categorical` included: Parquet
# carries a dictionary column, which is what a factor is.
PROFILE_FIXTURE_UNREACHABLE <- "other"


test_that("the fixture covers every dtype but the fallback", {
  # A fixture exercising four of seven types would let three mappings drift
  # unnoticed.
  path <- .fixture_path()
  skip_if(
    !nzchar(path) || !file.exists(path),
    "no vendored profile fixture; run `just schemas`"
  )
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  seen <- unique(unlist(lapply(doc[["cases"]], function(case) {
    vapply(
      case[["profile"]][["columns"]],
      function(col) col[["dtype"]],
      character(1L)
    )
  })))
  expect_setequal(setdiff(PROFILE_DTYPES, PROFILE_FIXTURE_UNREACHABLE), seen)
})
