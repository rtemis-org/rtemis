# tools/test-filter.R
# ::rtemis::
# 2026- EDG rtemis.org

# Run a filtered subset of the testthat suite and write a short verdict to its
# own file, separate from the log.
#
# Why the verdict is a separate file: a filtered run of this suite takes many
# minutes, so it has to be started in the background and waited on. A waiter
# greps for the verdict, and if the verdict lived in the log it would match the
# echoed command, R's startup banner, or a test name -- and report a result
# before the run has produced one. The verdict file does not exist until the run
# ends, so its existence *is* the completion signal.
#
# Run via `just test-filter <pattern>`, which sets the two variables below.

filter <- Sys.getenv("RTEMIS_TEST_FILTER", unset = "")
out <- Sys.getenv("RTEMIS_TEST_OUT", unset = "")
stopifnot(nzchar(out))

res <- testthat::test_local(
  filter = if (nzchar(filter)) filter else NULL,
  reporter = "silent",
  stop_on_failure = FALSE
)
df <- as.data.frame(res)

totals <- sprintf(
  "failed=%d error=%d skipped=%d passed=%d files=%d",
  sum(df[["failed"]]),
  sum(df[["error"]]),
  sum(df[["skipped"]]),
  sum(df[["passed"]]),
  length(unique(df[["file"]]))
)
bad <- df[df[["failed"]] > 0L | df[["error"]], c("file", "test", "failed", "error")]
lines <- c(
  totals,
  if (nrow(bad) > 0L) c("", utils::capture.output(print(bad, row.names = FALSE)))
)
writeLines(lines, file.path(out, "verdict"))
