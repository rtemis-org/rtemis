# test_CIGroups.R
# ::rtemis::
# 2026- EDG rtemis.org

source(test_path("..", "ci", "test-groups.R"), local = TRUE)

# %% CI coverage ----
test_that("CI groups cover every test file exactly once", {
  path <- test_path()
  groups <- ci_test_groups(path)
  selected <- lapply(c("general", "supervised", "fitting"), function(group) {
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
  expect_identical(groups[["SchemaContract"]], "general")
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
  expect_identical(ci_test_groups(path)[["NewContract"]], "general")
  expect_match("NewContract", ci_test_filter("general", path))
  expect_setequal(
    testthat::find_test_scripts(
      path,
      filter = ci_test_filter("general", path),
      full.names = FALSE
    ),
    new_files
  )
  expect_error(ci_test_filter("fitting", path), "No test files")
})


test_that("parallel integration requires explicit opt-in in automated runs", {
  withr::local_envvar(c(CI = NA, CODEX_CI = NA, RTEMIS_RUN_PARALLEL_TESTS = NA))
  expect_no_error(skip_ci_parallel_integration())
  for (flag in c("CI", "CODEX_CI")) {
    for (value in c("true", "TRUE", "1")) {
      withr::with_envvar(stats::setNames(value, flag), {
        expect_condition(skip_ci_parallel_integration(), class = "skip")
        withr::with_envvar(
          c(RTEMIS_RUN_PARALLEL_TESTS = "true"),
          expect_no_error(skip_ci_parallel_integration())
        )
      })
    }
  }
  for (value in c("false", "FALSE", "0")) {
    withr::with_envvar(
      c(RTEMIS_RUN_PARALLEL_TESTS = value),
      expect_condition(skip_ci_parallel_integration(), class = "skip")
    )
  }
  withr::with_envvar(
    c(CI = "false", CODEX_CI = "0"),
    expect_no_error(skip_ci_parallel_integration())
  )
  withr::with_envvar(
    c(RTEMIS_RUN_PARALLEL_TESTS = "typo"),
    expect_error(skip_ci_parallel_integration(), "must be")
  )
})


test_that("parallel opt-in never overrides a CRAN guard", {
  withr::local_envvar(c(NOT_CRAN = "false", RTEMIS_RUN_PARALLEL_TESTS = "true"))
  expect_no_error(skip_ci_parallel_integration())
  expect_condition(testthat::skip_on_cran(), class = "skip")
})
