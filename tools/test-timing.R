# tools/test-timing.R
# ::rtemis::
# 2026- EDG rtemis.org

# Time every test under the settings of a CRAN check: the installed package,
# `NOT_CRAN` false, cores limited, the full test group. Writes `tests.csv`
# (one row per test) and `files.csv` (per-file totals, slowest first) to the
# output directory given as the first argument.
#
# Run via `just test-timing <out>`, or on Windows via the test-timing workflow.

out <- commandArgs(TRUE)[[1L]]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(NOT_CRAN = "false", `_R_CHECK_LIMIT_CORES_` = "TRUE", CI = "")
Sys.unsetenv(c("RTEMIS_TEST_GROUP", "RTEMIS_RUN_PARALLEL_TESTS"))

res <- testthat::test_dir(
  "tests/testthat",
  package = "rtemis",
  load_package = "installed",
  reporter = testthat::ListReporter$new(),
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
)
tests <- as.data.frame(res)[, c(
  "file",
  "test",
  "nb",
  "failed",
  "skipped",
  "error",
  "user",
  "system",
  "real"
)]
tests <- tests[order(-tests[["real"]]), ]
utils::write.csv(tests, file.path(out, "tests.csv"), row.names = FALSE)
files <- stats::aggregate(
  cbind(real, user, system, nb, failed, skipped) ~ file,
  data = tests,
  FUN = sum
)
files <- files[order(-files[["real"]]), ]
utils::write.csv(files, file.path(out, "files.csv"), row.names = FALSE)
cat(
  "Total: ",
  round(sum(tests[["real"]])),
  " s over ",
  nrow(tests),
  " tests; ",
  sum(tests[["failed"]]),
  " failed, ",
  sum(tests[["error"]]),
  " errored.\n",
  sep = ""
)
print(utils::head(files, 30L), row.names = FALSE)
