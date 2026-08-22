# test_LINADForest.R
# ::rtemis::
# 2026- EDG rtemis.org

# Engine-level tests for the bagged ensemble. The pipeline-level train and
# predict blocks live in test_Supervised.R; what is checked here are the
# identities -- a one-tree forest against a single tree, the reproducibility of
# the RNG substreams, and the jackknife against a direct implementation of its
# own formula. Accuracy claims belong to the benchmark, not to a suite.

# %% Packages ----
library(data.table)

# %% Data ----
set.seed(2026)
.n <- 200
.x <- data.frame(
  a = rnorm(.n),
  b = rnorm(.n),
  c = rnorm(.n),
  d = rnorm(.n),
  g = factor(sample(c("p", "q"), .n, replace = TRUE))
)
.y <- 2 *
  .x[["a"]] +
  ifelse(.x[["b"]] < 0, -3, 3) +
  0.5 * .x[["c"]] +
  rnorm(.n, sd = 0.3)
.datr <- data.frame(.x, y = .y)


# %% A one-tree forest is one tree ----
test_that("a forest of one full-sample tree equals a single LINAD fit", {
  # The identity that catches an aggregation, subsetting or encoding error: with
  # every row in the bag, every feature in the tree and no size selection, the
  # forest's one tree has seen exactly what `train_LINAD` sees.
  hyperparameters <- rtemis::setup_LINADForest(
    n_trees = 1L,
    max_leaves = 5L,
    force_max_leaves = TRUE
  )
  settings <- rtemis:::linadforest_settings(hyperparameters, ncol(.x))
  grown <- rtemis:::linadforest_tree(
    x = .x,
    y = .y,
    case_weights = rep(1, .n),
    type = "Regression",
    y_levels = NULL,
    settings = settings,
    bag = seq_len(.n)
  )
  reference <- rtemis:::linad_fit(
    x = .x,
    xm = rtemis:::linad_design_matrix(.x),
    y = .y,
    case_weights = rep(1, .n),
    type = "Regression",
    settings = rtemis:::linad_settings(
      rtemis::setup_LINAD(
        max_leaves = 5L,
        learning_rate = 1,
        force_max_leaves = TRUE
      )
    ),
    verbosity = 0L
  )
  expect_identical(
    grown[["tree"]]@n_leaves,
    as.integer(reference[["n_leaves"]])
  )
  expect_equal(grown[["tree"]]@coefficients, reference[["coefficients"]])
  expect_identical(grown[["tree"]]@steps, reference[["steps"]])
  # Every row was drawn, so nothing is out of bag and there is nothing to score.
  expect_length(grown[["oob"]], 0L)
})


# %% Reproducibility ----
test_that("a forest is reproducible, and parallel dispatch does not change it", {
  fit <- function(n_workers, backend) {
    rtemis::train(
      .datr,
      hyperparameters = rtemis::setup_LINADForest(
        n_trees = 4L,
        max_leaves = 4L,
        mtry_split = 2L,
        force_max_leaves = TRUE
      ),
      execution_config = rtemis::setup_ExecutionConfig(
        seed = 42L,
        backend = backend,
        n_workers = n_workers
      ),
      verbosity = 0L
    )
  }
  once <- fit(1L, "none")
  again <- fit(1L, "none")
  expect_equal(
    stats::predict(once, .x),
    stats::predict(again, .x)
  )
  # One substream per tree, assigned by tree index, so the trees do not depend
  # on which worker picked them up.
  skip_if_not_installed("mirai")
  parallel <- fit(2L, "mirai")
  expect_equal(
    stats::predict(parallel, .x),
    stats::predict(once, .x)
  )
})


# %% Feature sampling ----
test_that("mtry_split never leaves a node unsplittable", {
  # A node is expanded once and its proposal cached, so a feature sample that
  # admitted no split would close the node for good. The invariant is that
  # sampling one feature per split reaches the same tree size as scanning all of
  # them -- an absolute leaf count would instead pin whatever the growth rules
  # happen to allow.
  grow <- function(mtry_split) {
    hyperparameters <- rtemis::setup_LINADForest(
      n_trees = 1L,
      max_leaves = 6L,
      mtry_split = mtry_split,
      force_max_leaves = TRUE
    )
    settings <- rtemis:::linadforest_settings(hyperparameters, ncol(.x))
    set.seed(7)
    rtemis:::linadforest_tree(
      x = .x,
      y = .y,
      case_weights = rep(1, .n),
      type = "Regression",
      y_levels = NULL,
      settings = settings,
      bag = seq_len(.n)
    )[["tree"]]
  }
  sampled <- grow(1L)
  every <- grow(NULL)
  expect_identical(sampled@n_leaves, every@n_leaves)
  expect_gt(sampled@n_leaves, 1L)
})


test_that("sampling features changes the fit", {
  settings_for <- function(...) {
    rtemis:::linadforest_settings(
      rtemis::setup_LINADForest(
        n_trees = 1L,
        max_leaves = 5L,
        force_max_leaves = TRUE,
        ...
      ),
      ncol(.x)
    )
  }
  grow <- function(settings, seed) {
    set.seed(seed)
    rtemis:::linadforest_tree(
      x = .x,
      y = .y,
      case_weights = rep(1, .n),
      type = "Regression",
      y_levels = NULL,
      settings = settings,
      bag = seq_len(.n)
    )[["tree"]]
  }
  full <- grow(settings_for(), 1L)
  sampled <- grow(settings_for(mtry_split = 1L), 1L)
  expect_false(isTRUE(all.equal(
    rtemis:::predict_super(full, .x),
    rtemis:::predict_super(sampled, .x)
  )))
  # A tree given three features holds three, and no more.
  narrow <- grow(settings_for(mtry_tree = 3L), 1L)
  expect_length(narrow@xnames, 3L)
  expect_true(all(narrow@xnames %in% names(.x)))
})


# %% Out-of-bag aggregation ----
test_that("out-of-bag predictions average only the trees that left a case out", {
  oob <- list(c(1L, 2L), c(2L, 3L))
  predictions <- list(c(10, 20), c(30, 40))
  aggregated <- rtemis:::linadforest_oob_prediction(oob, predictions, 4L)
  expect_equal(aggregated[[1L]], 10)
  expect_equal(aggregated[[2L]], mean(c(20, 30)))
  expect_equal(aggregated[[3L]], 40)
  # A case no tree left out has no estimate, rather than one from trees that
  # saw it.
  expect_true(is.na(aggregated[[4L]]))
})


# %% The infinitesimal jackknife ----
test_that("the jackknife matches a direct implementation of its formula", {
  set.seed(11)
  n_train <- 30L
  n_trees <- 12L
  n_new <- 7L
  bag_counts <- matrix(
    as.integer(rpois(n_train * n_trees, 1)),
    n_train,
    n_trees
  )
  predictions <- matrix(rnorm(n_new * n_trees), n_new, n_trees)
  observed <- rtemis:::linadforest_jackknife(predictions, bag_counts)

  # Wager, Hastie & Efron (2014), written out case by case.
  expected <- numeric(n_new)
  for (i in seq_len(n_new)) {
    centered <- predictions[i, ] - mean(predictions[i, ])
    covariances <- vapply(
      seq_len(n_train),
      function(k) {
        mean((bag_counts[k, ] - mean(bag_counts[k, ])) * centered)
      },
      numeric(1L)
    )
    v_ij <- sum(covariances^2)
    correction <- (n_train / n_trees^2) * sum(centered^2)
    expected[[i]] <- sqrt(max(v_ij - correction, 0))
  }
  expect_equal(observed, expected)
})


test_that("the jackknife blocks over new cases without changing its answer", {
  set.seed(12)
  n_train <- 20L
  n_trees <- 8L
  n_new <- as.integer(rtemis:::LINADFOREST_JACKKNIFE_BLOCK + 5L)
  bag_counts <- matrix(
    as.integer(rpois(n_train * n_trees, 1)),
    n_train,
    n_trees
  )
  predictions <- matrix(rnorm(n_new * n_trees), n_new, n_trees)
  blocked <- rtemis:::linadforest_jackknife(predictions, bag_counts)
  first <- rtemis:::linadforest_jackknife(
    predictions[1:3, , drop = FALSE],
    bag_counts
  )
  expect_length(blocked, n_new)
  expect_equal(blocked[1:3], first)
})


test_that("a single tree has no jackknife to report", {
  # One draw has no spread, and a zero would claim certainty rather than
  # absence.
  expect_true(all(is.na(rtemis:::linadforest_jackknife(
    matrix(1:3, 3L, 1L),
    matrix(1L, 5L, 1L)
  ))))
})


# %% Settings ----
test_that("mtry defaults reach every feature, and mtry_split is capped by mtry_tree", {
  settings <- rtemis:::linadforest_settings(rtemis::setup_LINADForest(), 7L)
  expect_identical(settings[["mtry_tree"]], 7L)
  expect_identical(settings[["mtry_split"]], 7L)
  # `mtry_split` samples from the features the tree holds, not from every
  # feature in the data.
  settings <- rtemis:::linadforest_settings(
    rtemis::setup_LINADForest(mtry_tree = 3L, mtry_split = 5L),
    7L
  )
  expect_identical(settings[["mtry_tree"]], 3L)
  expect_identical(settings[["mtry_split"]], 3L)
})


# %% Degenerate data ----
test_that("a bag that misses a factor level still fits and predicts", {
  # A rare level is absent from most bootstrap samples, which leaves that
  # level's design column constant within the tree. The tree's own levels are
  # the training levels, so the column exists and prediction on a case carrying
  # the level cannot fail -- but nothing else in the suite exercises it.
  set.seed(11)
  n <- 120L
  rare <- data.frame(
    v1 = rnorm(n),
    v2 = rnorm(n),
    g = factor(c(rep("a", n - 3L), "b", "b", "c"), levels = c("a", "b", "c"))
  )
  outcome <- rare[["v1"]] +
    ifelse(rare[["v2"]] > 0, 2, -2) +
    rnorm(n, sd = 0.2)
  model <- rtemis::train(
    data.frame(rare, y = outcome),
    hyperparameters = rtemis::setup_LINADForest(n_trees = 8L, max_leaves = 4L),
    execution_config = rtemis::setup_ExecutionConfig(
      seed = 5L,
      backend = "none"
    ),
    verbosity = 0L
  )
  expect_length(stats::predict(model, rare), n)
  expect_identical(nrow(rtemis::get_varimp(model)@data), 3L)
})


test_that("too few out-of-bag cases keeps every leaf rather than selecting on noise", {
  settings <- rtemis:::linadforest_settings(
    rtemis::setup_LINADForest(n_trees = 1L, max_leaves = 4L),
    ncol(.x)
  )
  # A bag holding all but a handful of cases leaves fewer out than a validation
  # curve can be read from.
  bag <- c(seq_len(.n - 3L), rep(1L, 3L))
  grown <- rtemis:::linadforest_tree(
    x = .x,
    y = .y,
    case_weights = rep(1, .n),
    type = "Regression",
    y_levels = NULL,
    settings = settings,
    bag = bag
  )
  expect_length(grown[["oob"]], 3L)
  expect_null(grown[["tree"]]@leaf_curve)
  expect_identical(
    grown[["tree"]]@n_leaves,
    length(grown[["tree"]]@steps)
  )
})
