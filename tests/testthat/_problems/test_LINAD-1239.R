# Extracted from test_LINAD.R:1239

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
skip_if_not_installed("leaps")
gram <- rtemis:::linad_gram(.xm, .y, .w, NULL)
for (nvmax in 1:3) {
  ours <- rtemis:::linad_forward(
    gram[["G"]],
    gram[["Xty"]],
    nvmax = nvmax,
    penalty = 0,
    stop_rule = "none"
  )
  reference <- leaps::regsubsets(
    x = .xm[, -1L, drop = FALSE],
    y = .y,
    nvmax = nvmax,
    method = "forward"
  )
  selected <- names(coef(reference, nvmax))[-1L]
  expect_setequal(colnames(.xm)[which(abs(ours) > 0)][-1L], selected)
}
