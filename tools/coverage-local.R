# coverage-local.R
# ::rtemis::
# 2026- EDG rtemis.org

# Run from the repository root. Local artifacts include coverage and test output.
local({
  args <- commandArgs(trailingOnly = TRUE)
  output <- if (length(args)) args[[1L]] else "coverage/local"
  for (package in c("covr", "DT", "htmltools")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      rtemis.core::abort("Install ", package, " to run local coverage.")
    }
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, mustWork = TRUE)

  # Retain the test log before removing the temporary instrumented installation.
  library_path <- tempfile("rtemis-coverage-library-")
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)
  test_log <- file.path(
    library_path,
    "rtemis",
    "rtemis-tests",
    "testthat.Rout"
  )
  on.exit(
    {
      for (path in c(test_log, paste0(test_log, ".fail"))) {
        if (file.exists(path)) {
          file.copy(path, file.path(output, basename(path)), overwrite = TRUE)
        }
      }
    },
    add = TRUE,
    after = FALSE
  )

  # The environment applies to the instrumented package's test subprocesses.
  withr::local_envvar(c(
    NOT_CRAN = "true",
    R_CLI_NUM_COLORS = "0",
    CI = "false",
    RTEMIS_TEST_GROUP = "all",
    RTEMIS_RUN_PARALLEL_TESTS = "true"
  ))
  coverage <- covr::package_coverage(
    ".",
    type = "tests",
    quiet = FALSE,
    clean = FALSE,
    install_path = library_path
  )
  saveRDS(coverage, file.path(output, "coverage.rds"))
  write.csv(
    covr::zero_coverage(coverage),
    file.path(output, "uncovered.csv"),
    row.names = FALSE
  )
  covr::report(coverage, file = file.path(output, "index.html"), browse = FALSE)
  print(coverage)

  # CheckReporter records skipped dependency and platform tests in its summary.
  summary <- grep("^\\[ FAIL ", readLines(test_log, warn = FALSE), value = TRUE)
  if (!length(summary)) {
    rtemis.core::abort("Inspect testthat.Rout: the test summary is missing.")
  }
  summary <- tail(summary, 1L)
  cat(summary, "\nCoverage artifacts: ", output, "\n", sep = "")
  if (!grepl("\\| SKIP 0 \\|", summary)) {
    rtemis.core::abort(
      "Resolve the skipped tests reported in testthat.Rout and rerun local coverage."
    )
  }
})
