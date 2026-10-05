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

# Type name of the second column of a parquet file, read without converting to
# a data.frame. `[[` on a Schema selects a field; the Field's `type` and the
# type's `ToString` are R6 members, read with `get()`.
second_type <- function(file) {
  fields <- infer_schema(read_parquet(file, as_data_frame = FALSE))
  get("type", envir = fields[[2L]])
}
type_name <- function(type) get("ToString", envir = type)()

# Lift the `string_view` type off the current fixture.
string_view <- second_type(fixture)
stopifnot(type_name(string_view) == "string_view")

x <- data.frame(
  id = c(1L, 2L, 3L, 4L, 5L),
  name = c("alpha", "beta", "gamma", "delta", NA),
  score = c(1.5, 2.25, 3.75, 4, 5.125),
  grp = c("a", "b", "a", "c", "b"),
  stringsAsFactors = FALSE
)

# Conversion from R has no `string_view` path; the Table's `cast` method
# converts after the table is built.
cast <- get("cast", envir = as_arrow_table(x))
tbl <- cast(schema(
  field("id", int32()),
  field("name", string_view),
  field("score", float64()),
  field("grp", string_view)
))
write_parquet(tbl, fixture, compression = "uncompressed")

# The written file must still carry the view type, or the test guards nothing.
stopifnot(type_name(second_type(fixture)) == "string_view")
