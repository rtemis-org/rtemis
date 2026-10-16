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


# %% ci_test_filter ----
ci_test_filter <- function(group, path) {
  if (!group %in% c("all", "general", "supervised", "fitting")) {
    stop("Unknown RTEMIS_TEST_GROUP: ", group)
  }
  if (group == "all") {
    return(NULL)
  }
  groups <- ci_test_groups(path)
  selected <- names(groups)[groups == group]
  if (!length(selected)) {
    stop("No test files in CI group: ", group)
  }
  paste(utils::glob2rx(selected), collapse = "|")
}


# %% run_test_group ----
# Runs one CI group under `R CMD check`, which times each script in `tests/`
# separately, so the check log reports the time of every group. The group runs
# when `RTEMIS_TEST_GROUP` is unset, `all`, or names it. Outside CI, a
# `SlowReporter` lists every test that takes at least 2 s in
# `tests/testthat-<group>.Rout`.
run_test_group <- function(group) {
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
  reporter <- if (identical(Sys.getenv("CI"), "true")) {
    "progress"
  } else {
    testthat::MultiReporter$new(list(
      testthat::CheckReporter$new(),
      testthat::SlowReporter$new(min_time = 2)
    ))
  }
  testthat::test_check(
    "rtemis",
    filter = ci_test_filter(group, "testthat"),
    reporter = reporter
  )
}
