# testthat.R
# ::rtemis::
# 2026- EDG rtemis.org

library(rtemis)
library(testthat)

source(file.path("ci", "test-groups.R"))

test_check(
  "rtemis",
  filter = ci_test_filter(Sys.getenv("RTEMIS_TEST_GROUP", "all"), "testthat"),
  reporter = if (identical(Sys.getenv("CI"), "true")) "progress" else "check"
)
