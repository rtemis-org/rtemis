# Extracted from test_LINAD.R:1677

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
set.seed(3)
n <- 600L
h <- factor(sample(letters[1:12], n, replace = TRUE))
x <- rnorm(n)
dat <- data.frame(
  h = h,
  x = x,
  y = 2 * x + as.numeric(h) / 4 + rnorm(n, 0, 0.5)
)
mod <- train(
  dat,
  hyperparameters = setup_LINAD(
    max_leaves = 4L,
    split_search = "exhaustive",
    force_max_leaves = TRUE
  ),
  execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
  verbosity = 0L
)
