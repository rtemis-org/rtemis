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
