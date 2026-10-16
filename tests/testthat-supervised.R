# testthat-supervised.R
# ::rtemis::
# 2026- EDG rtemis.org

library(rtemis)
library(testthat)

source(file.path("ci", "test-groups.R"))

run_test_group("supervised")
