# Extracted from test_LINAD.R:1506

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
n <- 400L
X <- as.data.frame(matrix(rnorm(n * 6L), n, 6L))
names(X) <- paste0("x", 1:6)
X[["y"]] <- ifelse(X[["x1"]] > 0, 3 * X[["x2"]] - 2 * X[["x3"]], 5) +
  rnorm(n, 0, 1)
fit <- function(...) {
  train(
    X,
    hyperparameters = setup_LINAD(
      max_leaves = 10L,
      node_model = "ridge",
      min_cases_node_model = 30L,
      force_max_leaves = TRUE,
      ...
    ),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
}
shape <- function(mod) {
  frame <- mod@model@frame
  frame[["n"]][frame[["is_leaf"]]]
}
expect_equal(shape(fit(node_test = "bic")), shape(fit()))
