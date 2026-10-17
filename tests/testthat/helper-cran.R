# helper-cran.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% outer_folds ----
# Outer resamples for a test of whether a resampled model trains, predicts and
# reports, as opposed to a test of how resamples are drawn or aggregated: two
# under a CRAN check, three elsewhere. The condition is the one
# `testthat::skip_on_cran()` reads, so CI and local runs fit three folds.
outer_folds <- function() {
  on_cran <- !interactive() &&
    !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
  if (on_cran) 2L else 3L
}
