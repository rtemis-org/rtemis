# test-groups.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% ci_test_groups ----
ci_test_groups <- function(path) {
  files <- sort(list.files(path, pattern = "^test.*[.][rR]$"))
  labels <- sub("[.][rR]$", "", sub("^test[-_]", "", files))
  groups <- stats::setNames(rep("general", length(labels)), labels)
  groups[labels == "Supervised"] <- "supervised"
  groups[
    labels %in%
      c(
        "BiasVariance",
        "Calibration",
        "Conformal",
        "ExplainSupervised",
        "LINAD",
        "LightGBMParameters",
        "TrainPreflight",
        "Tuner"
      )
  ] <- "fitting"
  groups
}


# %% cran_test_files ----
# The test files `R CMD check` runs on CRAN, which together reach every backend
# package outside the rtemis ecosystem once: one fit per supervised,
# decomposition, and clustering algorithm (`Supervised`, `Decomposition`,
# `Clustering`), KernelSHAP through shapr (`ExplainSupervised`), mice,
# missRanger, and timeDate (`Preprocessor`), arrow and nanoparquet (`Ingest`),
# pROC (`model_plot_roc`), visNetwork (`draw`), mori (`shared_memory`), and
# mirai (`ExecutionConfig`). The first four end at a top-level `skip_on_cran()`
# after their CRAN section. CI and local runs execute every file, including
# `AIReview`, which tests rtemis.llm through a stub of its model call.
cran_test_files <- c(
  "Clustering",
  "Decomposition",
  "ExecutionConfig",
  "ExplainSupervised",
  "Ingest",
  "Preprocessor",
  "Supervised",
  "draw",
  "model_plot_roc",
  "shared_memory"
)


# %% ci_on_cran ----
# Whether the session is a CRAN check by testthat's convention, the one
# `skip_on_cran()` reads: noninteractive, with `NOT_CRAN` not set to true.
ci_on_cran <- function() {
  !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}


# %% ci_test_filter ----
# A `test_check()` filter for the files of `group`; with `cran = TRUE`, for
# those of its files in `cran_test_files`. NULL selects every file.
ci_test_filter <- function(group, path, cran = FALSE) {
  if (!group %in% c("all", "general", "supervised", "fitting")) {
    stop("Unknown RTEMIS_TEST_GROUP: ", group)
  }
  if (group == "all" && !cran) {
    return(NULL)
  }
  groups <- ci_test_groups(path)
  selected <- if (group == "all") {
    names(groups)
  } else {
    names(groups)[groups == group]
  }
  if (cran) {
    selected <- intersect(selected, cran_test_files)
  }
  if (!length(selected)) {
    stop("No test files in CI group: ", group)
  }
  paste(utils::glob2rx(selected), collapse = "|")
}


# %% run_test_group ----
# Runs one test script under `R CMD check`, which times each script in `tests/`
# separately. `group` is a CI group or `cran`. On CRAN (`ci_on_cran()`), only
# `testthat-cran.R` runs, with every file in `cran_test_files`, so the check
# loads rtemis for tests once. Elsewhere, `testthat-cran.R` exits, and a group
# script runs its group when `RTEMIS_TEST_GROUP` is unset, `all`, or names it.
# A script that exits does so before loading rtemis. Outside CI, a
# `SlowReporter` lists every test that takes at least 2 s in
# `tests/testthat-<group>.Rout`.
run_test_group <- function(group) {
  on_cran <- ci_on_cran()
  if (group == "cran") {
    if (!on_cran) {
      message("Skipping the CRAN test selection: this is not a CRAN check.")
      return(invisible(NULL))
    }
    filter <- ci_test_filter("all", "testthat", cran = TRUE)
  } else {
    if (on_cran) {
      message(
        "Skipping CI group '",
        group,
        "': on CRAN, testthat-cran.R runs the CRAN test selection."
      )
      return(invisible(NULL))
    }
    selected <- Sys.getenv("RTEMIS_TEST_GROUP", "all")
    ci_test_filter(selected, "testthat") # errors on an unknown group
    if (!selected %in% c("all", group)) {
      message(
        "Skipping CI group '",
        group,
        "': RTEMIS_TEST_GROUP is '",
        selected,
        "'."
      )
      return(invisible(NULL))
    }
    filter <- ci_test_filter(group, "testthat")
  }
  library(rtemis)
  library(testthat)
  reporter <- if (identical(Sys.getenv("CI"), "true")) {
    "progress"
  } else {
    testthat::MultiReporter$new(list(
      testthat::CheckReporter$new(),
      testthat::SlowReporter$new(min_time = 2)
    ))
  }
  testthat::test_check("rtemis", filter = filter, reporter = reporter)
}
