# test_Tuner.R
# ::rtemis::
# 2025- EDG rtemis.org

# Note: Tuning is tested in test_Supervised.R with `train()`

# TunerConfig ----
tn_pr <- setup_GridSearch()
tn_pr
desc(tn_pr)
test_that("TunerConfig is abstract; subclasses carry the config", {
  # The superclass holds only `type` + the computed `config`; only tuner
  # subclasses can be instantiated.
  expect_error(TunerConfig())
  expect_s7_class(tn_pr, TunerConfig)
  expect_identical(tn_pr@type, "GridSearch")
})

# setup_GridSearch() ----
test_that("setup_GridSearch() succeeds", {
  expect_s7_class(setup_GridSearch(), GridSearchConfig)
})

test_that("randomize_p is required by, and only by, a randomized search", {
  # Cross-field rule: `tune_GridSearch()` multiplies the combination count by
  # `randomize_p`, so an unset value has to fail here rather than deep in
  # tuning with an opaque `sample()` error.
  expect_error(setup_GridSearch(search_type = "randomized"), "randomize_p")
  expect_s7_class(
    setup_GridSearch(search_type = "randomized", randomize_p = 0.5),
    GridSearchConfig
  )
  # ...and is meaningless for an exhaustive search.
  expect_error(setup_GridSearch(randomize_p = 0.5), "randomize_p")
  expect_null(setup_GridSearch()@randomize_p)
  # Bounds are exclusive at both ends.
  expect_error(setup_GridSearch(search_type = "randomized", randomize_p = 0))
  expect_error(setup_GridSearch(search_type = "randomized", randomize_p = 1))
})

# .list_to_TunerConfig() ----
test_that(".list_to_TunerConfig() falls back to setup_GridSearch() defaults", {
  # Fields the incoming list omits must take the `setup_*` default rather than
  # being passed through as NULL.
  tc <- .list_to_TunerConfig(list(type = "GridSearch", config = list()))
  expect_s7_class(tc, GridSearchConfig)
  expect_s7_class(tc@resampler_config, ResamplerConfig)
  expect_identical(tc@search_type, "exhaustive")
  expect_identical(tc@metrics_aggregate_fn, "mean")
  # A supplied resampler_config is decoded into its own config object.
  tc <- .list_to_TunerConfig(list(
    type = "GridSearch",
    config = list(resampler_config = list(type = "KFold", n_resamples = 3L))
  ))
  expect_identical(tc@resampler_config@n_resamples, 3L)
  # An unrecognized key is now named rather than dropped, and the nested
  # resampler is checked as strictly as the top level.
  expect_error(
    .list_to_TunerConfig(list(
      type = "GridSearch",
      config = list(resampler_config = list(type = "KFold", n = 3L))
    )),
    "did you mean `n_resamples`",
    fixed = TRUE
  )
  expect_error(
    .list_to_TunerConfig(list(type = "Bogus")),
    class = "rtemis_unsupported_error"
  )
})


test_that("a conditional grid search fits exactly the previewed combinations", {
  # The end-to-end contract: `tuning_grid()` is what `tune_GridSearch()` runs,
  # a combination excluded by a gate reaches the backend as NULL, and the
  # winning hyperparameters carry that NULL through to the final fit.
  skip_if_not_installed("hal9001")
  set.seed(2026)
  n <- 60L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- 2 * dat[["a"]] + rnorm(n)
  dat <- set_outcome(dat, "y")

  hyperparameters <- setup_HAL(
    smoothness_orders = tune_over(0L, 1L),
    reduce_basis = tune_over(0.1, 0.5)
  )
  previewed <- tuning_grid(hyperparameters)
  expect_identical(NROW(previewed), 3L)

  mod <- train(
    dat,
    hyperparameters = hyperparameters,
    tuner_config = setup_GridSearch(
      resampler_config = setup_Resampler(n_resamples = 2L)
    ),
    verbosity = 0L
  )
  fitted_grid <- mod@tuner@tuning_results[["param_grid"]]
  expect_equal(
    fitted_grid[, names(previewed), drop = FALSE],
    previewed,
    ignore_attr = TRUE
  )

  best <- mod@hyperparameters
  if (best@smoothness_orders == 0L) {
    expect_true(best@reduce_basis %in% c(0.1, 0.5))
  } else {
    expect_null(best@reduce_basis)
  }
})


test_that("a randomized search samples combinations, not grid rows", {
  # The sampled combinations are each run on every resample, so their scores
  # are comparable and the per-combination aggregation has a full set of rows.
  set.seed(1)
  n <- 80L
  dat <- set_outcome(
    data.frame(a = rnorm(n), b = rnorm(n), y = rnorm(n)),
    "y"
  )
  mod <- train(
    dat,
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L, 4L, 5L, 6L, 7L)),
    tuner_config = setup_GridSearch(
      search_type = "randomized",
      randomize_p = 0.5,
      resampler_config = setup_Resampler(n_resamples = 3L)
    ),
    verbosity = 0L
  )
  fitted_grid <- mod@tuner@tuning_results[["param_grid"]]
  expect_identical(NROW(fitted_grid), 3L)
  expect_identical(
    fitted_grid[["param_combo_id"]],
    seq_len(3L)
  )
  # Every sampled combination is scored, and each is one of the declared values.
  expect_identical(
    NROW(mod@tuner@tuning_results[["validation"]]),
    3L
  )
  expect_true(all(fitted_grid[["maxdepth"]] %in% 2:7))
  expect_false(anyDuplicated(fitted_grid[["maxdepth"]]) > 0L)
  expect_true(
    mod@hyperparameters@hyperparameters[["maxdepth"]] %in%
      fitted_grid[["maxdepth"]]
  )
})


test_that("a randomized search always keeps at least one combination", {
  # randomize_p excludes 0, but rounding reaches it on a small grid.
  set.seed(1)
  n <- 60L
  dat <- set_outcome(
    data.frame(a = rnorm(n), b = rnorm(n), y = rnorm(n)),
    "y"
  )
  mod <- train(
    dat,
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L)),
    tuner_config = setup_GridSearch(
      search_type = "randomized",
      randomize_p = 0.1,
      resampler_config = setup_Resampler(n_resamples = 2L)
    ),
    verbosity = 0L
  )
  expect_identical(NROW(mod@tuner@tuning_results[["param_grid"]]), 1L)
})

# %% tune_over ----
test_that("tune_over takes candidates as arguments or as one vector", {
  d <- setup_LightRF(max_depth = tune_over(3L, 4L, 5L))@max_depth
  expect_s7_class(d, HyperparameterDomain)
  expect_identical(d@candidates, list(3L, 4L, 5L))

  # A computed grid goes straight in, with no splicing incantation: this is the
  # form a user with a log-spaced grid actually writes.
  lambdas <- 10^seq(-4, 0, length.out = 5)
  d_vec <- setup_LightRF(lambda_l2 = tune_over(lambdas))@lambda_l2
  expect_identical(d_vec@candidates, as.list(lambdas))

  # A list argument is the same thing, and is what a vector-valued
  # hyperparameter needs.
  expect_identical(
    setup_LightRF(max_depth = tune_over(list(3L, 4L)))@max_depth@candidates,
    list(3L, 4L)
  )
})


test_that("a domain is rejected by a hyperparameter that is not tunable", {
  # `device_type` declares no search space, so its type does not admit one.
  err <- expect_error(setup_LightRF(device_type = tune_over("cpu", "gpu")))
  expect_match(conditionMessage(err), "device_type", fixed = TRUE)
  expect_match(conditionMessage(err), "HyperparameterDomain", fixed = TRUE)
})


test_that("a domain needs something to search", {
  expect_error(tune_over(3L), "at least two candidates")
  expect_error(tune_over(), class = "rtemis_input_error")
})


test_that("a bare vector on a vector-valued hyperparameter is corrected", {
  # The one reading that cannot be inferred: this vector is a single value of
  # `split_select_weights`, not a set of candidates for it.
  Arch <- S7::new_class(
    name = "Arch",
    package = NULL,
    properties = list(
      hidden_units = prop_integer(
        c(64L, 32L),
        min = 1L,
        vector = TRUE,
        tunable = TRUE,
        description = "Units per hidden layer."
      )
    )
  )
  err <- expect_error(Arch(hidden_units = tune_over(c(12L, 6L, 2L))))
  expect_match(conditionMessage(err), "single value", fixed = TRUE)
  # Written either unambiguous way, it works.
  expect_length(
    Arch(
      hidden_units = tune_over(c(12L, 6L), c(24L, 12L))
    )@hidden_units@candidates,
    2L
  )
  expect_length(
    Arch(
      hidden_units = tune_over(list(c(12L, 6L), c(24L, 12L)))
    )@hidden_units@candidates,
    2L
  )
})


test_that("every candidate is validated against the hyperparameter", {
  # Bounds declared on the spec are checked per candidate, and the failing one
  # is named.
  err <- expect_error(setup_LightRF(feature_fraction = tune_over(0.5, 1.5)))
  expect_match(conditionMessage(err), "candidate 2", fixed = TRUE)
  # A cleaner runs ahead of the class and rejects its own candidate directly.
  expect_error(setup_LightRF(num_leaves = tune_over(1024L, 0L)))
})
