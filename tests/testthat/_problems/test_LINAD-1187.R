# Extracted from test_LINAD.R:1187

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
gram <- rtemis:::linad_gram(.xm, .y, .w, NULL)
count_terms <- function(rule) {
  coefficients <- rtemis:::linad_forward(
    gram[["G"]],
    gram[["Xty"]],
    nvmax = 8L,
    syy = gram[["syy"]],
    sample_weight = gram[["sw"]],
    stop_rule = rule
  )
  sum(abs(coefficients) > 0)
}
