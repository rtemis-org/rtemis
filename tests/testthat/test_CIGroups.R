# test_CIGroups.R
# ::rtemis::
# 2026- EDG rtemis.org

source(test_path("..", "ci", "test-groups.R"), local = TRUE)

# %% CI coverage ----
test_that("CI groups cover every test file exactly once", {
  path <- test_path()
  groups <- ci_test_groups(path)
  selected <- lapply(c("contracts", "supervised", "fitting"), function(group) {
    testthat::find_test_scripts(
      path,
      filter = ci_test_filter(group, path),
      full.names = FALSE
    )
  })
  expect_setequal(
    unlist(selected),
    testthat::find_test_scripts(path, full.names = FALSE)
  )
  expect_identical(anyDuplicated(unlist(selected)), 0L)
  expect_true(all(lengths(selected) > 0L))
  expect_identical(groups[["Supervised"]], "supervised")
  expect_identical(groups[["ExplainSupervised"]], "fitting")
  expect_identical(groups[["SchemaContract"]], "contracts")
  expect_null(ci_test_filter("all", path))
  expect_error(ci_test_filter("typo", path), "Unknown RTEMIS_TEST_GROUP")
})


test_that("new tests enter the default CI group automatically", {
  path <- withr::local_tempdir()
  new_files <- c(
    "test_NewContract.R",
    "test-Dotted.name.r",
    "testNoSeparator.R"
  )
  file.create(file.path(path, c(new_files, "test_Supervised.R")))
  expect_identical(ci_test_groups(path)[["NewContract"]], "contracts")
  expect_match("NewContract", ci_test_filter("contracts", path))
  expect_setequal(
    testthat::find_test_scripts(
      path,
      filter = ci_test_filter("contracts", path),
      full.names = FALSE
    ),
    new_files
  )
  expect_error(ci_test_filter("fitting", path), "No test files")
})
