# Extracted from test_LINAD.R:1056

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
configurations <- list(
  "forward/stump" = rtemis::setup_LINAD(max_leaves = 8L),
  "forward/exhaustive" = rtemis::setup_LINAD(
    max_leaves = 8L,
    split_search = "exhaustive"
  ),
  "ridge" = rtemis::setup_LINAD(max_leaves = 8L, node_model = "ridge"),
  "constant" = rtemis::setup_LINAD(max_leaves = 8L, node_model = "constant"),
  "constant/soft" = rtemis::setup_LINAD(
    max_leaves = 8L,
    node_model = "constant",
    gamma = 0.5
  ),
  "cart" = rtemis::setup_LINAD(
    max_leaves = 8L,
    node_model = "constant",
    gamma = 0,
    learning_rate = 1,
    line_search = "none"
  ),
  "rate 1" = rtemis::setup_LINAD(max_leaves = 8L, learning_rate = 1),
  "soft" = rtemis::setup_LINAD(max_leaves = 8L, gamma = 0.5),
  "no line search" = rtemis::setup_LINAD(
    max_leaves = 8L,
    line_search = "none"
  ),
  "global selection" = rtemis::setup_LINAD(
    max_leaves = 8L,
    node_selection = "global"
  ),
  "least squares" = rtemis::setup_LINAD(
    max_leaves = 8L,
    constant_rule = "least_squares"
  ),
  "binned" = rtemis::setup_LINAD(max_leaves = 8L, split_binning = 8L)
)
for (label in names(configurations)) {
  hyperparameters <- configurations[[label]]
  hyperparameters@hyperparameters <- list(force_max_leaves = TRUE)
  fitted <- rtemis:::linad_fit(
    x = .x,
    xm = .xm,
    y = .y,
    case_weights = .w,
    type = "Regression",
    settings = rtemis:::linad_settings(hyperparameters),
    verbosity = 0L
  )
  model <- rtemis:::LinearAdditiveTree(
    frame = fitted[["frame"]],
    coefficients = fitted[["coefficients"]],
    steps = fitted[["steps"]],
    n_leaves = as.integer(fitted[["n_leaves"]]),
    xnames = names(.x),
    xlev = lapply(Filter(is.factor, .x), levels),
    design_assign = as.integer(attr(.xm, "assign")),
    design_scale = rtemis:::linad_scaling(.xm)[["scale"]],
    type = "Regression",
    y_levels = NULL,
    leaf_curve = NULL
  )
  model@frame[["is_leaf"]] <- model@frame[["node"]] %in%
    model@steps[[model@n_leaves]]
  expect_identical(
    rtemis:::linad_check_tree(model),
    character(),
    info = label
  )
}
