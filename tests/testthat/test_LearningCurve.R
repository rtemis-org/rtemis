# test_LearningCurve.R
# ::rtemis::
# 2026- EDG rtemis.org

# The learning curve is one shape across algorithms whose units of progress
# differ, and the early stopping that reads it must be inert when unused.

# %% The curve's contract ----
test_that("Every stepwise algorithm reports its curve in one shape", {
  set.seed(1)
  n <- 300L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- ifelse(dat[["b"]] < 0, -2, 2) +
    1.5 * dat[["a"]] +
    rnorm(n, 0, 0.8)
  training <- dat[1:200, ]
  validation <- dat[201:300, ]
  fit <- function(hyperparameters) {
    train(
      training,
      dat_validation = validation,
      hyperparameters = hyperparameters,
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L
    )
  }
  configurations <- list(
    LINAD = setup_LINAD(max_leaves = 10L),
    LINADForest = setup_LINADForest(n_trees = 3L, max_leaves = 6L)
  )
  if (requireNamespace("lightgbm", quietly = TRUE)) {
    configurations[["LightGBM"]] <- setup_LightGBM()
  }
  for (label in names(configurations)) {
    curve <- get_learning_curve(fit(configurations[[label]]))
    expect_s3_class(curve, "data.frame", exact = FALSE)
    expect_true(
      all(c("iteration", "loss_training", "loss_validation") %in% names(curve)),
      info = label
    )
    expect_type(attr(curve, "unit"), "character")
    expect_type(attr(curve, "selected"), "integer")
    expect_gt(nrow(curve), 1L)
    # Both series are populated when a validation set was given.
    expect_true(any(!is.na(curve[["loss_training"]])), info = label)
    expect_true(any(!is.na(curve[["loss_validation"]])), info = label)
  }
})


test_that("An algorithm that does not train in steps reports no curve", {
  set.seed(1)
  n <- 120L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- dat[["a"]] + rnorm(n)
  mod <- train(
    dat,
    hyperparameters = setup_CART(),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  # NULL is the answer, not a dispatch error.
  expect_null(get_learning_curve(mod))
  expect_error(plot_learning(mod), "records no learning curve")
})


test_that("A forest reports one row per tree and iteration", {
  set.seed(1)
  n <- 200L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- 2 * dat[["a"]] + rnorm(n)
  mod <- train(
    dat,
    hyperparameters = setup_LINADForest(n_trees = 4L, max_leaves = 5L),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  curve <- get_learning_curve(mod)
  expect_true("tree" %in% names(curve))
  expect_setequal(unique(curve[["tree"]]), seq_len(4L))
  per_tree <- vapply(
    mod@model@trees,
    function(tree) length(tree@steps),
    integer(1L)
  )
  expect_equal(nrow(curve), sum(per_tree))
})


test_that("plot_learning() draws every algorithm that has a curve", {
  set.seed(1)
  n <- 300L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- ifelse(dat[["b"]] < 0, -2, 2) + dat[["a"]] + rnorm(n, 0, 0.8)
  mod <- train(
    dat[1:200, ],
    dat_validation = dat[201:300, ],
    hyperparameters = setup_LINAD(max_leaves = 10L),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  expect_s3_class(plot_learning(mod), "plotly")
})


# %% Early stopping ----
test_that("patience is inert when unset", {
  # The guard on the engine change: growth must be bit-identical to what it was
  # before validation could reach it.
  set.seed(7)
  n <- 400L
  dat <- as.data.frame(matrix(rnorm(n * 5L), n, 5L))
  names(dat) <- paste0("x", 1:5)
  dat[["y"]] <- ifelse(dat[["x1"]] > 0, 3, -3) +
    2 * dat[["x2"]] +
    rnorm(n, 0, 2)
  training <- dat[1:250, ]
  validation <- dat[251:400, ]
  fit <- function(...) {
    train(
      training,
      hyperparameters = setup_LINAD(max_leaves = 15L, ...),
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L,
      dat_validation = validation
    )
  }
  without <- fit()
  with_null <- fit(patience = NULL)
  features <- training[paste0("x", 1:5)]
  expect_equal(
    predict(without, features),
    predict(with_null, features),
    tolerance = 1e-12
  )
  expect_equal(without@model@coefficients, with_null@model@coefficients)
  expect_equal(length(without@model@steps), length(with_null@model@steps))
})


test_that("patience bounds growth and the size is still the curve's argmin", {
  set.seed(7)
  n <- 450L
  dat <- as.data.frame(matrix(rnorm(n * 6L), n, 6L))
  names(dat) <- paste0("x", 1:6)
  # Two real splits and a training set small enough that forty leaves overfit,
  # so the validation curve turns up well before growth would stop on its own.
  dat[["y"]] <- ifelse(dat[["x1"]] > 0, 3, -3) +
    2 * dat[["x2"]] +
    rnorm(n, 0, 4)
  fit <- function(patience) {
    train(
      dat[1:150, ],
      dat_validation = dat[151:450, ],
      hyperparameters = setup_LINAD(
        max_leaves = 40L,
        node_model = "constant",
        patience = patience
      ),
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L
    )
  }
  unbounded <- fit(NULL)
  stopped <- fit(1L)
  expect_lt(length(stopped@model@steps), length(unbounded@model@steps))
  # Stopping bounds growth; the size is still the argmin of the curve reached,
  # not simply the size growth halted at.
  expect_equal(
    stopped@model@n_leaves,
    which.min(stopped@model@leaf_curve)
  )
  # And the curve the growth loop watched agrees with the one recomputed
  # afterwards, which is what makes the stopping decision trustworthy.
  expect_length(stopped@model@leaf_curve, length(stopped@model@steps))
})


test_that("A forest lets each tree stop on its own out-of-bag cases", {
  set.seed(7)
  n <- 600L
  dat <- as.data.frame(matrix(rnorm(n * 5L), n, 5L))
  names(dat) <- paste0("x", 1:5)
  dat[["y"]] <- ifelse(dat[["x1"]] > 0, 3, -3) +
    2 * dat[["x2"]] +
    rnorm(n, 0, 2.5)
  fit <- function(patience) {
    train(
      dat,
      hyperparameters = setup_LINADForest(
        n_trees = 4L,
        max_leaves = 30L,
        node_model = "constant",
        patience = patience
      ),
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L
    )
  }
  sizes <- function(mod) {
    vapply(mod@model@trees, function(tree) length(tree@steps), integer(1L))
  }
  expect_lt(sum(sizes(fit(1L))), sum(sizes(fit(NULL))))
})


# %% What tuning selects is what gets built ----
test_that("A tuned LINAD keeps the leaf count tuning selected", {
  # A grid cell selects its size on the inner resample's held-out fold, and the
  # final fit gets no validation set -- `dat_validation` must be NULL when
  # outer resampling is set. Without carrying the selection the model that
  # ships keeps every leaf it grew, larger than the one that was measured.
  set.seed(5)
  n <- 500L
  dat <- as.data.frame(matrix(rnorm(n * 4L), n, 4L))
  names(dat) <- paste0("x", 1:4)
  dat[["y"]] <- ifelse(dat[["x1"]] > 0, 2, -2) + dat[["x2"]] + rnorm(n, 0, 1.5)
  res <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = tune_over(6L, 20L)),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, seed = 2026),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  kept <- vapply(res@models, function(mod) mod@model@n_leaves, integer(1L))
  carried <- vapply(
    res@models,
    function(mod) as.integer(mod@hyperparameters[["best_n_leaves"]]),
    integer(1L)
  )
  grown <- vapply(
    res@models,
    function(mod) length(mod@model@steps),
    integer(1L)
  )
  expect_equal(kept, pmin(carried, grown))
  # At least one fold should prune, or the guard proves nothing on this data.
  expect_true(any(kept < grown))
})


test_that("best_n_leaves is run state, not configuration", {
  # It is written by the Tuner from data, so it must not travel in a config
  # that another run would replay.
  expect_identical(
    role_prop_names(LINADHyperparameters, "state"),
    "best_n_leaves"
  )
  hyperparameters <- setup_LINAD()
  hyperparameters@best_n_leaves <- 7L
  expect_identical(hyperparameters@hyperparameters[["best_n_leaves"]], 7L)
  expect_false("best_n_leaves" %in% names(serializable_props(hyperparameters)))
})
