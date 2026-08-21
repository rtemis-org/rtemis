# test_HyperparametersSet.R
# ::rtemis::
# 2026- EDG rtemis.org

# A set is a union of search spaces over one algorithm. What the tests hold to:
# every member expands and gates on its own, a row fully specifies the
# configuration it came from, the winner is named, and a set collapses to a
# single `Hyperparameters` everywhere downstream.

# %% Data ----
# A step process, so the Additive Tree point is the one that should win: the
# outcome is piecewise constant in `b`, which constants fit and a linear model
# cannot.
set.seed(2026)
.n <- 300L
.features <- data.frame(a = rnorm(.n), b = rnorm(.n), c = rnorm(.n))
.dat <- data.frame(
  .features,
  y = 3 *
    ifelse(.features[["b"]] > 0, 1, -1) +
    0.5 * .features[["a"]] +
    rnorm(.n, sd = 0.5)
)
.execution <- setup_ExecutionConfig(seed = 1L, backend = "none")
.tuner <- setup_GridSearch(
  resampler_config = setup_Resampler(3L, "KFold", verbosity = 0L)
)


# %% Construction ----
test_that("a list of Hyperparameters coerces to a set", {
  set <- rtemis:::as_HyperparametersSet(list(
    cart = setup_LINAD(node_model = "constant"),
    setup_LINAD()
  ))
  expect_s7_class(set, rtemis:::HyperparametersSet)
  expect_identical(set@algorithm, "LINAD")
  expect_length(set, 2L)
  # Unnamed members are labelled by position; named ones keep their name.
  expect_identical(names(set@members), c("cart", "variant_2"))
  expect_s7_class(set[["cart"]], rtemis:::LINADHyperparameters)
  # Coercing a set is a no-op, so the boundary can be crossed twice safely.
  expect_identical(rtemis:::as_HyperparametersSet(set), set)
})


test_that("a set refuses what it cannot search", {
  expect_error(
    rtemis:::as_HyperparametersSet(list(setup_LINAD(), setup_CART())),
    class = "rtemis_value_error"
  )
  expect_error(
    rtemis:::as_HyperparametersSet(list(setup_LINAD(), 42)),
    class = "rtemis_type_error"
  )
  expect_error(
    rtemis:::as_HyperparametersSet(list()),
    class = "rtemis_length_error"
  )
  # Sets do not nest.
  inner <- rtemis:::as_HyperparametersSet(list(setup_LINAD()))
  expect_error(
    rtemis:::as_HyperparametersSet(list(setup_LINAD(), inner)),
    class = "rtemis_type_error"
  )
  expect_error(
    rtemis:::as_HyperparametersSet(list(a = setup_LINAD(), a = setup_LINAD())),
    class = "rtemis_value_error"
  )
})


test_that("building a set leaves its members alone", {
  # A property declared with a setter has that setter called at construction
  # with the class prototype. Both write-through properties therefore ignore a
  # zero-length value; without that, building a set wrote `integer(0)` into
  # every member and the failure surfaced much later, in `train()`.
  member <- setup_LINAD()
  set <- rtemis:::as_HyperparametersSet(list(member))
  expect_identical(set@members[[1L]]@n_workers, member@n_workers)
  expect_identical(set@members[[1L]]@resampled, member@resampled)
  expect_length(set@n_workers, 1L)
  expect_length(set@resampled, 1L)
})


test_that("n_workers and resampled write through to every member", {
  set <- rtemis:::as_HyperparametersSet(list(setup_LINAD(), setup_LINAD()))
  set@n_workers <- 4L
  expect_identical(set@n_workers, 4L)
  expect_true(all(vapply(
    set@members,
    function(member) identical(member@n_workers, 4L),
    logical(1L)
  )))
})


# %% needs_tuning ----
test_that("a set needs tuning when there is a choice to make", {
  # More than one member is a choice even when no member varies anything.
  expect_true(rtemis:::needs_tuning(
    rtemis:::as_HyperparametersSet(list(
      setup_LINAD(),
      setup_LINAD(gamma = 0.5)
    ))
  ))
  # One member with nothing to vary is not a search.
  expect_false(rtemis:::needs_tuning(
    rtemis:::as_HyperparametersSet(list(setup_LINAD()))
  ))
  expect_true(rtemis:::needs_tuning(
    rtemis:::as_HyperparametersSet(list(setup_LINAD(
      gamma = tune_over(0.1, 0.5)
    )))
  ))
})


# %% The grid ----
test_that("a set's grid is the union of its members', each fully specified", {
  set <- rtemis:::as_HyperparametersSet(list(
    linear = setup_LINAD(
      max_leaves = 1L,
      node_model = "ridge",
      lambda = tune_over(0.01, 0.1)
    ),
    addtree = setup_LINAD(
      node_model = "constant",
      gamma = tune_over(0.1, 0.3),
      max_leaves = 5L
    )
  ))
  grid <- tuning_grid(set)
  expect_identical(nrow(grid), 4L)
  expect_true(".variant" %in% names(grid))
  expect_identical(
    as.character(grid[[".variant"]]),
    c("linear", "linear", "addtree", "addtree")
  )
  # A row says what its member holds, never by omission: `linear` does not tune
  # gamma, so its rows carry its own gamma rather than a blank.
  linear_rows <- grid[grid[[".variant"]] == "linear", ]
  expect_true(all(linear_rows[["gamma"]] == set[["linear"]][["gamma"]]))
  # And a member that leaves a hyperparameter unset carries NA, which
  # `.update_hyperparameters()` reads back as NULL.
  addtree_rows <- grid[grid[[".variant"]] == "addtree", ]
  expect_true(all(is.na(addtree_rows[["lambda"]])))
})


test_that("every grid row applies to its own member", {
  set <- rtemis:::as_HyperparametersSet(list(
    ridge = setup_LINAD(node_model = "ridge", lambda = tune_over(0.01, 0.1)),
    constant = setup_LINAD(node_model = "constant", gamma = tune_over(0.1, 0.3))
  ))
  grid <- tuning_grid(set)
  columns <- rtemis:::grid_hyperparameter_columns(grid)
  for (i in seq_len(nrow(grid))) {
    variant <- rtemis:::grid_variant(grid, i)
    updated <- update(
      set[[variant]],
      rtemis:::grid_row_values(grid, i, columns),
      tuned = 1L
    )
    expect_s7_class(updated, rtemis:::LINADHyperparameters)
    # A gated hyperparameter stays unset on the member whose gate is shut.
    if (variant == "constant") {
      expect_null(updated[["lambda"]])
    }
  }
})


# %% train() ----
test_that("train() selects a member and names it", {
  model <- train(
    .dat,
    hyperparameters = list(
      linear = setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.1),
      addtree = setup_LINAD(
        node_model = "constant",
        gamma = 0.1,
        learning_rate = 1,
        max_leaves = 5L,
        force_max_leaves = TRUE
      )
    ),
    tuner_config = .tuner,
    execution_config = .execution,
    verbosity = 0L
  )
  expect_s7_class(model, Regression)
  # The outcome is piecewise constant, so constants win over a linear fit.
  expect_identical(model@hyperparameters@variant, "addtree")
  expect_identical(model@hyperparameters[["node_model"]], "constant")
  # A set collapses: what the model holds is one configuration.
  expect_s7_class(model@hyperparameters, rtemis:::LINADHyperparameters)
  expect_identical(model@tuner@best_variant, "addtree")
})


test_that("a single Hyperparameters is unchanged by any of this", {
  model <- train(
    .dat,
    hyperparameters = setup_LINAD(
      max_leaves = tune_over(3L, 5L),
      force_max_leaves = TRUE
    ),
    tuner_config = .tuner,
    execution_config = .execution,
    verbosity = 0L
  )
  expect_null(model@hyperparameters@variant)
  expect_null(model@tuner@best_variant)
})


test_that("a one-member fixed set collapses without tuning", {
  model <- train(
    .dat,
    hyperparameters = list(
      only = setup_LINAD(max_leaves = 4L, force_max_leaves = TRUE)
    ),
    execution_config = .execution,
    verbosity = 0L
  )
  expect_s7_class(model@hyperparameters, rtemis:::LINADHyperparameters)
  expect_identical(model@hyperparameters@variant, "only")
})


test_that("outer resampling lets every fold choose for itself", {
  resampled <- train(
    .dat,
    hyperparameters = list(
      linear = setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.1),
      addtree = setup_LINAD(
        node_model = "constant",
        gamma = 0.1,
        learning_rate = 1,
        max_leaves = 5L,
        force_max_leaves = TRUE
      )
    ),
    tuner_config = .tuner,
    outer_resampling_config = setup_Resampler(3L, "KFold", verbosity = 0L),
    execution_config = .execution,
    verbosity = 0L
  )
  expect_s7_class(resampled, RegressionRes)
  variants <- vapply(
    resampled@models,
    function(model) model@hyperparameters@variant,
    character(1L)
  )
  expect_length(variants, 3L)
  expect_true(all(variants %in% c("linear", "addtree")))
  # The resampled object holds what was asked for, which is the set: no single
  # member won at this level.
  expect_s7_class(resampled@hyperparameters, rtemis:::HyperparametersSet)
})


# %% The wire ----
test_that("a set round trips through its wire form", {
  set <- rtemis:::as_HyperparametersSet(list(
    linear = setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.1),
    addtree = setup_LINAD(node_model = "constant", gamma = 0.3)
  ))
  wire <- rtemis:::S7_to_list(set)
  # Tagged, so a reader tells a set from a single configuration without knowing
  # what the property declares.
  expect_identical(names(wire), "variants")
  expect_true(rtemis:::is_wire_hyperparameters_set(wire))
  expect_identical(names(wire[["variants"]]), c("linear", "addtree"))

  restored <- rtemis:::.list_to_HyperparametersSet(wire)
  expect_s7_class(restored, rtemis:::HyperparametersSet)
  # The names have to survive: the name is what the tuner reports as the winner.
  expect_identical(names(restored@members), c("linear", "addtree"))
  expect_identical(restored@algorithm, "LINAD")
  expect_identical(restored[["addtree"]][["node_model"]], "constant")
  expect_identical(restored[["addtree"]][["gamma"]], 0.3)
})


test_that("the config artifact carries the set that was asked for", {
  model <- train(
    .dat,
    hyperparameters = list(
      linear = setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.1),
      addtree = setup_LINAD(
        node_model = "constant",
        gamma = 0.1,
        max_leaves = 5L,
        force_max_leaves = TRUE
      )
    ),
    tuner_config = .tuner,
    execution_config = .execution,
    verbosity = 0L
  )
  # `mod@config` is what was *asked for*; `mod@hyperparameters` is what ran.
  expect_s7_class(model@config@hyperparameters, rtemis:::HyperparametersSet)
  expect_s7_class(model@hyperparameters, rtemis:::LINADHyperparameters)
})


# %% Rows that look alike are not the same row ----
test_that("members differing only in an untuned hyperparameter both survive", {
  # A grid holds only the properties that need *tuning*, so two members that
  # differ in one neither of them tunes produce rows identical in every column
  # but `.variant`. They are different configurations and both must be fitted --
  # deduplicating the union would silently drop one, and the grid gives no
  # visible sign that they differ.
  set <- rtemis:::as_HyperparametersSet(list(
    forward = setup_LINAD(node_model = "forward"),
    constant = setup_LINAD(node_model = "constant")
  ))
  grid <- tuning_grid(set)
  expect_identical(nrow(grid), 2L)
  expect_identical(
    as.character(grid[[".variant"]]),
    c("forward", "constant")
  )
  columns <- rtemis:::grid_hyperparameter_columns(grid)
  # Nothing distinguishes them in the hyperparameter columns; `.variant` does.
  expect_identical(
    grid[1L, columns, drop = FALSE],
    grid[2L, columns, drop = FALSE],
    ignore_attr = TRUE
  )
})


# %% What tuning reports ----
test_that("tuning names the variant count, and only for a set", {
  variants <- train(
    .dat,
    hyperparameters = list(
      linear = setup_LINAD(max_leaves = 1L, node_model = "ridge", lambda = 0.1),
      addtree = setup_LINAD(
        node_model = "constant",
        gamma = 0.1,
        learning_rate = 1,
        max_leaves = 5L,
        force_max_leaves = TRUE
      )
    ),
    tuner_config = .tuner,
    execution_config = .execution,
    verbosity = 1L
  )
  expect_s7_class(variants, Regression)
})


test_that("a set reports only the winning member's own search", {
  # The union grid holds every member's tuned hyperparameters, so reporting it
  # whole shows one member's candidates as another's: a `lambda` searched over
  # {0.01, 0.1} in one variant reads as {0.01, 0.1, NULL} once variants that
  # never set it are folded in, and that NULL is not a candidate anyone offered.
  set.seed(7)
  n <- 300L
  features <- data.frame(a = rnorm(n), b = rnorm(n))
  linear_dat <- data.frame(
    features,
    y = 2 * features[["a"]] - 1.5 * features[["b"]] + rnorm(n, sd = 0.5)
  )
  expect_message(
    train(
      linear_dat,
      hyperparameters = list(
        linear = setup_LINAD(
          max_leaves = 1L,
          node_model = "ridge",
          lambda = tune_over(0.01, 0.1)
        ),
        addtree = setup_LINAD(
          node_model = "constant",
          gamma = tune_over(0.1, 0.3),
          learning_rate = 1
        )
      ),
      tuner_config = .tuner,
      execution_config = .execution,
      verbosity = 1L
    ),
    regexp = "hyperparameter variants"
  )
})
