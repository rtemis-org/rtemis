# Extracted from test_LINAD.R:1372

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
set.seed(2026)
x <- rnorm(500L, 0, 3)
dat <- data.frame(x = x, y = x^2 + 12 + rnorm(500L, 0, 1.5))
root_split <- function(mod) {
  frame <- mod@model@frame
  frame[!is.na(frame[["split_feature"]]), ][1L, "split_value"]
}
fit <- function(...) {
  train(
    dat,
    hyperparameters = setup_LINAD(
      max_leaves = 2L,
      learning_rate = 1,
      gamma = 0,
      force_max_leaves = TRUE,
      ...
    ),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
}
