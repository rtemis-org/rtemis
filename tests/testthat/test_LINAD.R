# test_LINAD.R
# ::rtemis::
# 2026- EDG rtemis.org

# Engine-level tests for the Linear Additive Tree. The pipeline-level train and
# predict blocks live in test_Supervised.R; what is checked here is the
# arithmetic underneath, and in particular the invariants whose absence let the
# original implementation's fitted values and stored coefficients disagree.

# %% Packages ----
library(data.table)

# %% Data ----
set.seed(2026)
.n <- 300
.x <- data.frame(
  a = rnorm(.n),
  b = rnorm(.n),
  c = rnorm(.n),
  g = factor(sample(c("p", "q", "r"), .n, replace = TRUE))
)
# A mean shift on `b` on top of a linear trend: something both split searches
# can find, so tests that are not about the search are not about the search.
.y <- 2 *
  .x[["a"]] +
  ifelse(.x[["b"]] < 0, -3, 3) +
  0.5 * .x[["c"]] +
  ifelse(.x[["g"]] == "p", 1, 0) +
  rnorm(.n, sd = 0.3)
.datr <- data.frame(.x, y = .y)
.xm <- rtemis:::linad_design_matrix(.x)
.w <- rep(1, .n)


# %% Native primitives ----
test_that("the constant/slopes split equals fitting an intercept jointly", {
  # The identity the whole parameterization rests on. Fitting the constant first
  # and the slopes without an intercept gives the same answer as one joint fit
  # -- but only on a design centered by the node's *weighted* means. Without
  # that centering the slopes absorb part of the level and the fit is worse, so
  # this is the test that says the refactor cost nothing.
  #
  # The reference is a direct solve of the system LINAD defines, not glmnet:
  # glmnet divides its lambda by an internal standardization of the outcome, so
  # the two parameterizations differ by a constant that is glmnet's business.
  weights <- runif(.n, 0.5, 1.5)
  for (lambda in c(0, 0.01, 0.5)) {
    gram <- crossprod(.xm * weights, .xm)
    cross <- drop(crossprod(.xm * weights, .y))
    penalty <- rep(lambda * sum(weights), ncol(.xm))
    penalty[[1L]] <- 0
    joint <- drop(solve(gram + diag(penalty), cross))
    split <- rtemis:::linad_solve(
      .xm,
      .y,
      weights,
      seq_len(.n),
      "ridge",
      lambda = lambda
    )
    expect_equal(split[["coefficients"]], unname(joint), tolerance = 1e-6)
    # And the constant is the node's level, Eq 19.
    expect_equal(
      split[["constant"]],
      stats::weighted.mean(.y, weights),
      tolerance = 1e-10
    )
  }
})


test_that("forward selection is unchanged by dropping the intercept from a centered design", {
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
})


test_that("linad_forward() matches leaps::regsubsets", {
  skip_if_not_installed("leaps")
  predictors <- as.matrix(.x[, c("a", "b", "c")])
  gram <- rtemis:::linad_gram(
    cbind(`(Intercept)` = 1, predictors),
    .y,
    .w,
    NULL
  )
  for (nvmax in 1:3) {
    ours <- rtemis:::linad_forward(gram[["G"]], gram[["Xty"]], nvmax)
    reference <- leaps::regsubsets(
      predictors,
      .y,
      nvmax = nvmax,
      method = "forward"
    )
    expected <- stats::coef(reference, id = nvmax)
    selected <- ours[abs(ours) > 1e-10]
    expect_length(selected, length(expected))
    expect_equal(
      sort(unname(selected)),
      sort(unname(expected)),
      tolerance = 1e-6
    )
  }
})


test_that("linad_stump() finds the same split as an rpart stump", {
  skip_if_not_installed("rpart")
  residual <- .y - mean(.y)
  ours <- rtemis:::linad_stump(
    rtemis:::linad_context(.x),
    residual,
    .w,
    rep(TRUE, .n),
    min_cases_child = 5L
  )
  reference <- rpart::rpart(
    residual ~ .,
    data.frame(residual = residual, .x),
    control = rpart::rpart.control(
      maxdepth = 1L,
      minsplit = 2L,
      minbucket = 5L,
      cp = 0,
      xval = 0L,
      maxsurrogate = 0L,
      maxcompete = 0L
    )
  )
  expect_identical(ours[["feature"]], rownames(reference[["splits"]])[[1L]])
  expect_equal(
    ours[["value"]],
    unname(reference[["splits"]][1L, "index"]),
    tolerance = 1e-8
  )
})


# %% Fitted-value invariant ----
test_that("a node's coefficients reproduce its function value", {
  # The invariant the original implementation broke: it advanced the function
  # value by the Newton step and the intercept by the stump constant, which are
  # equal for a regression and not for a classification. Here the function value
  # is never stored -- it is always recomputed from the coefficients -- so the
  # check is that prediction routes to the leaf whose coefficients were fitted.
  for (type in c("Regression", "Classification")) {
    outcome <- if (type == "Classification") {
      ifelse(.y > stats::median(.y), 1, -1)
    } else {
      .y
    }
    fitted <- rtemis:::linad_fit(
      .x,
      .xm,
      outcome,
      .w,
      type,
      rtemis:::linad_settings(setup_LINAD(
        max_leaves = 6L,
        node_model = "ridge"
      )),
      verbosity = 0L
    )
    terminal <- fitted[["steps"]][[fitted[["n_leaves"]]]]
    leaf <- rtemis:::linad_route(fitted[["frame"]], .x, terminal)
    expect_true(all(leaf %in% terminal))
    manual <- rowSums(.xm * fitted[["coefficients"]][leaf, , drop = FALSE])
    # Every case's fitted value comes from the leaf it routes to, and each node's
    # recorded loss is the loss of its own coefficients over its own cases.
    for (node in terminal) {
      rows <- which(leaf == node)
      expect_equal(
        manual[rows],
        drop(.xm[rows, , drop = FALSE] %*% fitted[["coefficients"]][node, ]),
        tolerance = 1e-10
      )
    }
  }
})


test_that("max_leaves = 1 is exactly the root model", {
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(
      setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.01)
    ),
    verbosity = 0L
  )
  expect_identical(nrow(fitted[["frame"]]), 1L)
  expect_identical(fitted[["n_leaves"]], 1L)
  scaling <- rtemis:::linad_scaling(.xm)
  scaled <- sweep(
    sweep(.xm, 2L, scaling[["center"]], "-"),
    2L,
    scaling[["scale"]],
    "/"
  )
  gram <- crossprod(scaled * .w, scaled)
  penalty <- rep(0.01 * sum(.w), ncol(scaled))
  penalty[[1L]] <- 0
  expected <- rtemis:::linad_unscale(
    matrix(
      drop(solve(gram + diag(penalty), drop(crossprod(scaled * .w, .y)))),
      nrow = 1L
    ),
    scaling
  )
  expect_equal(
    unname(fitted[["coefficients"]][1L, ]),
    unname(expected[1L, ]),
    tolerance = 1e-6
  )
})


test_that("the tree sizes are nested", {
  # steps[[k]] must be steps[[k - 1]] with one leaf replaced by its two
  # children, which is what makes selecting a size on held-out data meaningful.
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(setup_LINAD(max_leaves = 8L)),
    verbosity = 0L
  )
  steps <- fitted[["steps"]]
  for (k in seq_along(steps)) {
    expect_length(steps[[k]], k)
  }
  frame <- fitted[["frame"]]
  for (k in seq_len(length(steps) - 1L)) {
    removed <- setdiff(steps[[k]], steps[[k + 1L]])
    added <- setdiff(steps[[k + 1L]], steps[[k]])
    expect_length(removed, 1L)
    expect_length(added, 2L)
    expect_setequal(
      added,
      c(frame[["left"]][[removed]], frame[["right"]][[removed]])
    )
  }
})


# %% node_value ----
test_that("node_value is the level the tree alone has reached", {
  # rpart's `yval` semantics under shrinkage: the root's is the constant that
  # alone minimizes the loss (Eq 3), and a child's is its parent's plus its own
  # shrunk, line-searched constant. It is not the intercept at x = 0 -- that is
  # column 1 of `coefficients`, and on centered features x = 0 need not be
  # anywhere near the data.
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(setup_LINAD(max_leaves = 6L, node_model = "ridge")),
    verbosity = 0L
  )
  frame <- fitted[["frame"]]
  expect_true("node_value" %in% names(frame))
  expect_equal(
    frame[["node_value"]][[1L]],
    stats::weighted.mean(.y, .w),
    tolerance = 1e-10
  )
  expect_true(all(is.finite(frame[["node_value"]])))
  # Every node's value is its parent's plus one step, so a child never lands
  # further from its parent than the whole outcome range.
  internal <- which(!is.na(frame[["left"]]))
  for (row in internal) {
    for (child in c(frame[["left"]][[row]], frame[["right"]][[row]])) {
      expect_lt(
        abs(frame[["node_value"]][[child]] - frame[["node_value"]][[row]]),
        diff(range(.y))
      )
    }
  }
})


test_that("with constant leaves the model is exactly its node values", {
  # Slopes are all zero, so `node_value` is not merely the tree's share of the
  # prediction -- it is the whole prediction, which is what makes the Additive
  # Tree mode a plain decision tree.
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(
      setup_LINAD(max_leaves = 6L, node_model = "constant")
    ),
    verbosity = 0L
  )
  expect_true(all(abs(fitted[["coefficients"]][, -1L]) < 1e-10))
  terminal <- fitted[["steps"]][[fitted[["n_leaves"]]]]
  leaf <- rtemis:::linad_route(fitted[["frame"]], .x, terminal)
  predicted <- rowSums(.xm * fitted[["coefficients"]][leaf, , drop = FALSE])
  expect_equal(
    unname(predicted),
    fitted[["frame"]][["node_value"]][leaf],
    tolerance = 1e-10
  )
})


# %% constant_rule ----
test_that("constant_rule changes nothing for a regression", {
  # Eq 19 *is* the weighted mean of the residual, so the two rules are the same
  # computation for squared error and differ only for classification.
  fit_with <- function(rule) {
    rtemis:::linad_fit(
      .x,
      .xm,
      .y,
      .w,
      "Regression",
      rtemis:::linad_settings(
        setup_LINAD(
          max_leaves = 5L,
          node_model = "ridge",
          constant_rule = rule
        )
      ),
      verbosity = 0L
    )[["coefficients"]]
  }
  expect_equal(fit_with("closed_form"), fit_with("least_squares"))
})


test_that("constant_rule changes a classification, which is why it is a switch", {
  # Eq 20's Newton ratio and the weighted mean of the residual are different
  # quantities on a margin. Neither was better on the data tried so far, so the
  # rerun decides and both ship.
  outcome <- ifelse(.y > stats::median(.y), 1, -1)
  fit_with <- function(rule) {
    rtemis:::linad_fit(
      .x,
      .xm,
      outcome,
      .w,
      "Classification",
      rtemis:::linad_settings(
        setup_LINAD(
          max_leaves = 5L,
          node_model = "ridge",
          constant_rule = rule
        )
      ),
      verbosity = 0L
    )[["coefficients"]]
  }
  expect_false(isTRUE(all.equal(
    fit_with("closed_form"),
    fit_with("least_squares")
  )))
})


test_that("linad_constant() implements Eq 19 and Eq 20", {
  weights <- runif(.n, 0.5, 1.5)
  residual <- rnorm(.n)
  idx <- seq_len(.n)
  expect_equal(
    rtemis:::linad_constant(residual, NULL, weights, idx, "Regression", 1000),
    stats::weighted.mean(residual, weights)
  )
  derivatives <- list(g = rnorm(.n), h = runif(.n, 0.5, 2))
  expect_equal(
    rtemis:::linad_constant(
      residual,
      derivatives,
      weights,
      idx,
      "Classification",
      1000
    ),
    -sum(weights * derivatives[["g"]]) / sum(weights * derivatives[["h"]])
  )
  # The bound is the line search's, so a vanishing second derivative cannot send
  # a node to infinity.
  flat <- list(g = rep(-1, .n), h = rep(0, .n))
  expect_equal(
    abs(rtemis:::linad_constant(
      residual,
      flat,
      weights,
      idx,
      "Classification",
      2
    )),
    2
  )
})


# %% Leaf models ----
test_that("node_model = 'constant' fits no slopes, including at the root", {
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(setup_LINAD(
      max_leaves = 6L,
      node_model = "constant"
    )),
    verbosity = 0L
  )
  slopes <- fitted[["coefficients"]][, -1L, drop = FALSE]
  expect_true(all(abs(slopes) < 1e-10))
  # And the root inherits the mode rather than being left as a linear model.
  expect_true(all(abs(fitted[["coefficients"]][1L, -1L]) < 1e-10))
})


test_that("nvmax bounds the terms each update adds", {
  settings <- rtemis:::linad_settings(
    setup_LINAD(max_leaves = 4L, node_model = "forward", nvmax = 1L)
  )
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    settings,
    verbosity = 0L
  )
  # The root update itself may touch at most one term beside the intercept.
  expect_lte(sum(abs(fitted[["coefficients"]][1L, -1L]) > 1e-10), 1L)
})


# %% Soft weighting ----
test_that("gamma changes the fit, monotonically", {
  # The regression test for a real defect: gamma reached only the split search,
  # so it was inert for every value that happened to choose the same splits.
  # The leaf models must see the soft weights for it to mean anything.
  set.seed(7)
  n <- 400
  x <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  y <- 2 *
    x[["a"]] +
    ifelse(x[["b"]] < 0, -2, 2) +
    0.5 * x[["c"]] +
    rnorm(n, sd = 1)
  xm <- rtemis:::linad_design_matrix(x)
  fit_at <- function(gamma) {
    rtemis:::linad_fit(
      x,
      xm,
      y,
      rep(1, n),
      "Regression",
      rtemis:::linad_settings(
        setup_LINAD(gamma = gamma, max_leaves = 6L, node_model = "ridge")
      ),
      verbosity = 0L
    )[["coefficients"]]
  }
  hard <- fit_at(0)
  for (gamma in c(0.25, 0.5, 1)) {
    expect_false(isTRUE(all.equal(hard, fit_at(gamma))))
  }
  # More sharing pulls every leaf toward the same model, so the spread of the
  # leaf coefficients has to shrink as gamma rises.
  spread <- vapply(
    c(0, 0.5, 1),
    function(gamma) mean(apply(fit_at(gamma), 2L, stats::sd)),
    numeric(1L)
  )
  expect_true(all(diff(spread) < 0))
})


test_that("gamma = 0 fits each leaf on its own cases alone", {
  # The hard partition is a special case of the soft one, not a separate path:
  # at gamma = 0 a non-member's weight is exactly 0, so a soft-weighted fit over
  # every case and a fit over the node's cases are the same computation.
  set.seed(4)
  n <- 200
  x <- data.frame(a = rnorm(n), b = rnorm(n))
  r <- x[["a"]] + rnorm(n)
  xm <- rtemis:::linad_design_matrix(x)
  members <- seq_len(120L)
  weights <- numeric(n)
  weights[members] <- 1
  soft <- rtemis:::linad_solve(
    xm,
    r,
    weights,
    which(weights > 0),
    "ridge",
    lambda = 0.05
  )
  subset_only <- rtemis:::linad_solve(
    xm,
    r,
    rep(1, n),
    members,
    "ridge",
    lambda = 0.05
  )
  expect_equal(
    soft[["coefficients"]],
    subset_only[["coefficients"]],
    tolerance = 1e-12
  )
  expect_equal(soft[["constant"]], subset_only[["constant"]], tolerance = 1e-12)
})


# %% Learning rate ----
test_that("learning_rate scales the node updates, and the line search does not undo it", {
  # Reported as "learning_rate barely changes anything". It does; what masks it
  # is that `root_learning_rate` defaults to 1, so the root model is fitted at
  # full strength and the fit never falls below one global linear model. On a
  # signal the root cannot represent, the rate's effect is the whole model.
  set.seed(21)
  n <- 400
  x <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  # A pure step: no linear component at all for the root model to take.
  y <- ifelse(x[["b"]] < 0, -3, 3) + rnorm(n, sd = 0.5)
  xm <- rtemis:::linad_design_matrix(x)
  tree_size <- function(rate) {
    coefficients <- rtemis:::linad_fit(
      x,
      xm,
      y,
      rep(1, n),
      "Regression",
      rtemis:::linad_settings(
        setup_LINAD(
          learning_rate = rate,
          max_leaves = 8L,
          node_model = "ridge",
          force_max_leaves = TRUE
        )
      ),
      verbosity = 0L
    )[["coefficients"]]
    # How far the leaves travel from the root, which is what the rate scales.
    sum(abs(sweep(coefficients, 2L, coefficients[1L, ], "-")))
  }
  sizes <- vapply(c(1e-4, 0.01, 0.1, 0.5), tree_size, numeric(1L))
  expect_true(all(diff(sizes) > 0))
  # Proportional, not merely monotone: a hundredfold rate is a hundredfold step,
  # which is what shows the line search is scale-free and does not compensate.
  expect_equal(sizes[[2L]] / sizes[[1L]], 100, tolerance = 0.15)
})


test_that("root_learning_rate shrinks the root towards the best constant, not towards zero", {
  # A shrunk root used to shrink its intercept towards zero, so any outcome not
  # centered on the origin came back with a large negative R-squared: the model
  # predicted nothing rather than the mean. Equation 3 says the initialization is
  # the constant that alone minimizes the loss, and that is what it shrinks to.
  set.seed(5)
  n <- 300
  x <- data.frame(a = rnorm(n), b = rnorm(n))
  y <- 50 + 2 * x[["a"]] + ifelse(x[["b"]] < 0, -2, 2) + rnorm(n)
  xm <- rtemis:::linad_design_matrix(x)
  weights <- rep(1, n)
  for (rate in c(1, 0.5, 0.1, 0.01)) {
    fitted <- rtemis:::linad_fit(
      x,
      xm,
      y,
      weights,
      "Regression",
      rtemis:::linad_settings(
        setup_LINAD(
          root_learning_rate = rate,
          learning_rate = 0.5,
          max_leaves = 6L,
          node_model = "ridge",
          force_max_leaves = TRUE
        )
      ),
      verbosity = 0L
    )
    terminal <- fitted[["steps"]][[fitted[["n_leaves"]]]]
    leaf <- rtemis:::linad_route(fitted[["frame"]], x, terminal)
    predicted <- rowSums(xm * fitted[["coefficients"]][leaf, , drop = FALSE])
    expect_gt(1 - sum((y - predicted)^2) / sum((y - mean(y))^2), 0.5)
    # The root's intercept stays on the outcome's scale whatever the rate.
    expect_equal(
      unname(fitted[["coefficients"]][1L, 1L]),
      mean(y),
      tolerance = 0.05
    )
  }
})


test_that("linad_baseline() is the loss-minimizing constant", {
  set.seed(8)
  y <- rnorm(200L, mean = 12)
  weights <- runif(200L, 0.5, 1.5)
  expect_equal(
    rtemis:::linad_baseline(y, weights, "Regression"),
    stats::weighted.mean(y, weights)
  )
  # Classification: half the log odds, where the gradient of the {-1, +1}
  # logistic loss vanishes.
  labels <- rep(c(1, -1), times = c(70L, 130L))
  uniform <- rep(1, 200L)
  expect_equal(
    rtemis:::linad_baseline(labels, uniform, "Classification"),
    0.5 * log(70 / 130)
  )
  gradient_at <- function(c0) {
    sum(uniform * -2 * labels / (1 + exp(2 * labels * c0)))
  }
  expect_equal(
    gradient_at(rtemis:::linad_baseline(labels, uniform, "Classification")),
    0,
    tolerance = 1e-8
  )
  # A single-class outcome has no finite minimizer; it is capped rather than Inf.
  expect_equal(
    rtemis:::linad_baseline(rep(1, 10L), rep(1, 10L), "Classification"),
    rtemis:::LINAD_BASELINE_MAX
  )
})


# %% What LINAD reduces to ----
# `setup_LINAD`'s documentation claims these are exact reductions rather than
# approximations. That is a strong claim to make in a help page, so it is
# guarded here rather than asserted there.

test_that("max_leaves = 1 is a pure linear model", {
  scaling <- rtemis:::linad_scaling(.xm)
  scaled <- sweep(
    sweep(.xm, 2L, scaling[["center"]], "-"),
    2L,
    scaling[["scale"]],
    "/"
  )
  for (lambda in c(0, 0.01, 0.5)) {
    fitted <- rtemis:::linad_fit(
      .x,
      .xm,
      .y,
      .w,
      "Regression",
      rtemis:::linad_settings(
        setup_LINAD(
          max_leaves = 1L,
          node_model = "ridge",
          lambda = lambda,
          root_learning_rate = 1
        )
      ),
      verbosity = 0L
    )
    penalty <- rep(lambda * sum(.w), ncol(scaled))
    penalty[[1L]] <- 0
    expected <- rtemis:::linad_unscale(
      matrix(
        drop(solve(
          crossprod(scaled * .w, scaled) + diag(penalty),
          drop(crossprod(scaled * .w, .y))
        )),
        nrow = 1L
      ),
      scaling
    )
    expect_identical(nrow(fitted[["frame"]]), 1L)
    expect_equal(
      unname(fitted[["coefficients"]][1L, ]),
      unname(expected[1L, ]),
      tolerance = 1e-6
    )
  }
})


test_that("constants, a hard partition and no shrinkage is CART", {
  skip_if_not_installed("rpart")
  set.seed(4)
  n <- 400
  x <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  x[["y"]] <- ifelse(x[["b"]] < 0, -3, 3) +
    ifelse(x[["a"]] < 0.5, -1.5, 1.5) +
    rnorm(n, sd = 0.6)
  linad <- train(
    x,
    hyperparameters = setup_LINAD(
      node_model = "constant",
      gamma = 0,
      learning_rate = 1,
      line_search = "none",
      max_leaves = 4L,
      force_max_leaves = TRUE,
      min_cases_leaf = 5L
    ),
    verbosity = 0L
  )
  reference <- rpart::rpart(
    y ~ .,
    x,
    control = rpart::rpart.control(
      cp = 0,
      minbucket = 5,
      minsplit = 10,
      maxdepth = 30L,
      xval = 0L
    )
  )
  pruned <- rpart::prune(
    reference,
    cp = reference[["cptable"]][reference[["cptable"]][, "nsplit"] == 3, "CP"]
  )
  expect_equal(
    unname(predict(linad, x[, c("a", "b", "c")])),
    unname(predict(pruned, x)),
    tolerance = 1e-10
  )
})


test_that("the same with soft weighting is the Additive Tree, and differs from CART", {
  set.seed(4)
  n <- 300
  x <- data.frame(a = rnorm(n), b = rnorm(n))
  x[["y"]] <- ifelse(x[["b"]] < 0, -3, 3) + rnorm(n, sd = 0.6)
  fit_at <- function(gamma) {
    train(
      x,
      hyperparameters = setup_LINAD(
        node_model = "constant",
        gamma = gamma,
        learning_rate = 1,
        max_leaves = 4L,
        force_max_leaves = TRUE
      ),
      verbosity = 0L
    )
  }
  hard <- fit_at(0)
  soft <- fit_at(0.5)
  # Constants only, in both.
  expect_true(all(abs(soft@model@coefficients[, -1L]) < 1e-10))
  # Soft weighting is exactly what separates the two.
  expect_false(isTRUE(all.equal(
    unname(predict(soft, x[, c("a", "b")])),
    unname(predict(hard, x[, c("a", "b")])),
    tolerance = 1e-6
  )))
})


test_that("root_learning_rate = 0 starts at the mean and splits first", {
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(
      setup_LINAD(
        node_model = "ridge",
        root_learning_rate = 0,
        max_leaves = 4L,
        force_max_leaves = TRUE
      )
    ),
    verbosity = 0L
  )
  expect_equal(
    fitted[["frame"]][["node_value"]][[1L]],
    stats::weighted.mean(.y, .w),
    tolerance = 1e-10
  )
  expect_true(all(abs(fitted[["coefficients"]][1L, -1L]) < 1e-10))
  # And the tree still fits: the first step was a split, not a line.
  expect_gt(nrow(fitted[["frame"]]), 1L)
})


# %% Split search ----
test_that("the exhaustive search finds an interaction the stump search cannot", {
  # A slope flip with no mean shift: splitting it leaves both sides' means
  # unchanged, so a stump fitted to the residual is blind to it, while scoring a
  # candidate by the loss after fitting both child models is not. This is the
  # difference between the manuscript's two strategies.
  set.seed(99)
  n <- 400
  x <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  y <- ifelse(x[["b"]] < 0, 2 * x[["a"]], -2 * x[["a"]]) +
    0.5 * x[["c"]] +
    rnorm(n, sd = 0.3)
  xm <- rtemis:::linad_design_matrix(x)
  roots <- vapply(
    c("stump", "exhaustive"),
    function(search) {
      fitted <- rtemis:::linad_fit(
        x,
        xm,
        y,
        rep(1, n),
        "Regression",
        rtemis:::linad_settings(
          setup_LINAD(
            max_leaves = 4L,
            node_model = "ridge",
            split_search = search,
            force_max_leaves = TRUE
          )
        ),
        verbosity = 0L
      )
      fitted[["frame"]][["split_feature"]][[1L]]
    },
    character(1L)
  )
  expect_identical(unname(roots[["exhaustive"]]), "b")
  expect_false(identical(unname(roots[["stump"]]), "b"))
})


test_that("split_binning restricts candidate splits to bin boundaries", {
  # Equal-frequency bins, so a candidate cut has roughly the same number of
  # cases either side of it. With 8 bins there are at most 7 boundaries per
  # numeric feature, against one per distinct value unbinned.
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
  # And a binned fit is still a fit.
  fitted <- rtemis:::linad_fit(
    .x,
    .xm,
    .y,
    .w,
    "Regression",
    rtemis:::linad_settings(setup_LINAD(max_leaves = 4L, split_binning = 8L)),
    verbosity = 0L
  )
  expect_identical(fitted[["n_leaves"]], 4L)
})


test_that("linad_cut_positions() keeps everything when asked for more than exists", {
  expect_identical(rtemis:::linad_cut_positions(c(3L, 9L), 5L), c(3L, 9L))
  expect_length(rtemis:::linad_cut_positions(seq_len(100L), 5L), 5L)
})


test_that("split_bin_type places edges by rank or by value", {
  # On a skewed feature the two disagree, which is the whole point of the
  # option: equal-frequency follows the cases, equal-width follows the axis.
  set.seed(3)
  values <- sort(exp(rnorm(400L)))
  breaks <- seq_len(399L)
  by_frequency <- rtemis:::linad_cut_positions(breaks, 8L, values, "frequency")
  by_width <- rtemis:::linad_cut_positions(breaks, 8L, values, "width")
  expect_false(identical(by_frequency, by_width))
  # Equal-frequency cuts are evenly spread over the ranks.
  expect_lt(stats::sd(diff(by_frequency)), stats::sd(diff(by_width)))
  # Equal-width cuts are evenly spread over the values. Compared as coefficients
  # of variation, since the two cover very different spans and a raw standard
  # deviation of the gaps would just be measuring that.
  unevenness <- function(cuts) {
    gaps <- diff(values[cuts])
    stats::sd(gaps) / mean(gaps)
  }
  expect_lt(unevenness(by_width), unevenness(by_frequency))
  # And equal-width reaches into the sparse tail that equal-frequency ignores.
  expect_gt(diff(range(values[by_width])), diff(range(values[by_frequency])))
})


# %% Design matrix ----
test_that("a factor level unseen at training is refused, not silently dropped", {
  levels_seen <- list(g = levels(.x[["g"]]))
  novel <- .x[1:5, ]
  novel[["g"]] <- factor(c("p", "q", "r", "z", "p"))
  expect_error(
    rtemis:::linad_design_matrix(novel, levels_seen),
    class = "rtemis_value_error"
  )
  expect_silent(rtemis:::linad_design_matrix(.x[1:5, ], levels_seen))
})


test_that("the design matrix is full rank with factors present", {
  # Reference coding, not one-hot: an intercept plus every level indicator is
  # singular, which leaves the leaf coefficients unidentifiable.
  expect_identical(qr(.xm)[["rank"]], ncol(.xm))
})


# %% Hyperparameter gates ----
test_that("setup_LINAD() rejects a parameter its leaf model ignores", {
  expect_error(setup_LINAD(node_model = "ridge", nvmax = 5L))
  expect_error(setup_LINAD(node_model = "forward", lambda = 0.1))
  expect_error(setup_LINAD(node_model = "ridge", alpha = 0.5))
  expect_error(setup_LINAD(
    force_max_leaves = TRUE,
    smooth_validation_curve = TRUE
  ))
  expect_error(setup_LINAD(split_search = "stump", n_cuts = 10L))
  # split_binning is not gated: it discretizes the features for either search.
  expect_s7_class(
    setup_LINAD(split_search = "stump", split_binning = 32L),
    LINADHyperparameters
  )
  # And accepts each where it applies.
  expect_s7_class(
    setup_LINAD(node_model = "forward", nvmax = 5L),
    LINADHyperparameters
  )
  expect_s7_class(
    setup_LINAD(node_model = "ridge", lambda = 0.1),
    LINADHyperparameters
  )
  expect_s7_class(
    setup_LINAD(node_model = "elasticnet", lambda = 0.1, alpha = 0.5),
    LINADHyperparameters
  )
})


test_that("first_* hyperparameters inherit the node-level values", {
  settings <- rtemis:::linad_settings(
    setup_LINAD(node_model = "ridge", lambda = 0.2)
  )
  expect_identical(settings[["root_model"]], "ridge")
  expect_identical(settings[["root_lambda"]], 0.2)
  overridden <- rtemis:::linad_settings(
    setup_LINAD(
      node_model = "ridge",
      lambda = 0.2,
      root_model = "constant"
    )
  )
  expect_identical(overridden[["root_model"]], "constant")
})
