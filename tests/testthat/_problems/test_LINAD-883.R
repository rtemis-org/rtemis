# Extracted from test_LINAD.R:883

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rtemis", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(data.table)
set.seed(2026)
.n <- 300
.x <- data.frame(
  a = rnorm(.n),
  b = rnorm(.n),
  c = rnorm(.n),
  g = factor(sample(c("p", "q", "r"), .n, replace = TRUE))
)
.y <- 2 *
  .x[["a"]] +
  ifelse(.x[["b"]] < 0, -3, 3) +
  0.5 * .x[["c"]] +
  ifelse(.x[["g"]] == "p", 1, 0) +
  rnorm(.n, sd = 0.3)
.datr <- data.frame(.x, y = .y)
.xm <- rtemis:::linad_design_matrix(.x)
.w <- rep(1, .n)

# test -------------------------------------------------------------------------
unbinned <- rtemis:::linad_context(.x)
binned <- rtemis:::linad_context(.x, n_bins = 8L)
for (j in seq_along(unbinned[["numeric_names"]])) {
  expect_gt(length(unbinned[["numeric_breaks"]][[j]]), 7L)
  expect_lte(length(binned[["numeric_breaks"]][[j]]), 7L)
  # Every retained boundary is one the unbinned search would also have tried.
  expect_true(all(
    binned[["numeric_breaks"]][[j]] %in% unbinned[["numeric_breaks"]][[j]]
  ))
}
fitted <- rtemis:::linad_fit(
  .x,
  .xm,
  .y,
  .w,
  "Regression",
  rtemis:::linad_settings(setup_LINAD(max_leaves = 4L, split_binning = 8L)),
  verbosity = 0L
)
