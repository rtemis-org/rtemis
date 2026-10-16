# tools/test-timing.R
# ::rtemis::
# 2026- EDG rtemis.org

# Time every test under the settings of a CRAN check: the installed package,
# `NOT_CRAN` false, cores limited, the full test group. Writes `tests.csv`
# (one row per test) and `files.csv` (per-file totals, slowest first) to the
# output directory given as the first argument. In `files.csv`, `wall` is the
# elapsed time of the whole file and `setup` is the part spent outside
# `test_that()` blocks, such as fits at the top level of a file.
#
# Run via `just test-timing <out>`, or on Windows via the test-timing workflow.

out <- commandArgs(TRUE)[[1L]]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(NOT_CRAN = "false", `_R_CHECK_LIMIT_CORES_` = "TRUE", CI = "")
Sys.unsetenv(c("RTEMIS_TEST_GROUP", "RTEMIS_RUN_PARALLEL_TESTS"))

# %% FileClockReporter ----
# Records the elapsed time from the start to the end of each test file.
FileClockReporter <- R6::R6Class(
  "FileClockReporter",
  inherit = testthat::Reporter,
  public = list(
    started = NULL,
    wall = numeric(),
    start_file = function(filename) {
      self[["started"]] <- list(file = filename, at = proc.time()[["elapsed"]])
    },
    end_file = function() {
      started <- self[["started"]]
      self[["wall"]][[started[["file"]]]] <- proc.time()[["elapsed"]] -
        started[["at"]]
    }
  )
)

list_reporter <- testthat::ListReporter$new()
clock <- FileClockReporter$new()
started <- proc.time()[["elapsed"]]
testthat::test_dir(
  "tests/testthat",
  package = "rtemis",
  load_package = "installed",
  reporter = testthat::MultiReporter$new(list(list_reporter, clock)),
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
)
elapsed <- proc.time()[["elapsed"]] - started
tests <- as.data.frame(list_reporter$get_results())[, c(
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
files[["wall"]] <- unname(clock[["wall"]][files[["file"]]])
files[["setup"]] <- files[["wall"]] - files[["real"]]
files <- files[order(-files[["wall"]]), ]
utils::write.csv(files, file.path(out, "files.csv"), row.names = FALSE)
cat(
  "Elapsed: ",
  round(elapsed),
  " s; inside tests: ",
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
