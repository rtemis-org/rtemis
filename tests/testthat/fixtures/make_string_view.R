# make_string_view.R
# ::rtemis::
# 2026- EDG rtemis.org

# Regenerate string_view.parquet, the file behind `read()`'s regression test for
# Arrow view types. Run from the package root:
#   Rscript tests/testthat/fixtures/make_string_view.R
#
# `arrow` writes and reads the `string_view` type but exposes no constructor for
# it, so the type object has to be lifted off a file that already carries one:
# the fixture seeds its own replacement. The first one came from a parquet
# written by polars through the rtemis CLI's `.save`, which is the case this
# guards -- polars writes every string column as `string_view`, and its parquet
# writer fixes the compatibility level, so the type cannot be avoided from the
# writing side.

library(arrow)

fixture <- file.path("tests", "testthat", "fixtures", "string_view.parquet")
stopifnot(file.exists(fixture))

# Lift the `string_view` type off the current fixture.
string_view <- read_parquet(fixture, as_data_frame = FALSE)$schema$field(
  1L
)$type
stopifnot(string_view$ToString() == "string_view")

x <- data.frame(
  id = c(1L, 2L, 3L, 4L, 5L),
  name = c("alpha", "beta", "gamma", "delta", NA),
  score = c(1.5, 2.25, 3.75, 4, 5.125),
  grp = c("a", "b", "a", "c", "b"),
  stringsAsFactors = FALSE
)

tbl <- as_arrow_table(x)$cast(schema(
  field("id", int32()),
  field("name", string_view),
  field("score", float64()),
  field("grp", string_view)
))
write_parquet(tbl, fixture, compression = "uncompressed")

# The written file must still carry the view type, or the test guards nothing.
stopifnot(
  read_parquet(fixture, as_data_frame = FALSE)$schema$field(
    1L
  )$type$ToString() ==
    "string_view"
)
