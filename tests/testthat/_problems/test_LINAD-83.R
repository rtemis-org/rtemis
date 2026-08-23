# Extracted from test_LINAD.R:83

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
predictors <- as.matrix(.x[, c("a", "b", "c")])
centered <- sweep(predictors, 2L, colMeans(predictors), "-")
gram <- rtemis:::linad_gram(centered, .y - mean(.y), .w, NULL)
for (nvmax in 1:3) {
  ours <- rtemis:::linad_forward(
    gram[["G"]],
    gram[["Xty"]],
    nvmax,
    intercept = FALSE
  )
  reference <- leaps::regsubsets(
    predictors,
    .y,
    nvmax = nvmax,
    method = "forward"
  )
  expected <- stats::coef(reference, id = nvmax)
  selected <- ours[abs(ours) > 1e-10]
  expect_length(selected, length(expected) - 1L)
  expect_equal(
    sort(unname(selected)),
    sort(unname(expected[-1L])),
    tolerance = 1e-6
  )
}
