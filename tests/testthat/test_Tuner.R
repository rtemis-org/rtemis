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
  # `n` is not a key this reconstructor accepts: we own the whole stack and the
  # API is unsettled, so there are no compatibility shims for our own formats.
  # An unrecognized key leaves `n_resamples` unset, which the validator
  # rejects.
  expect_error(
    .list_to_TunerConfig(list(
      type = "GridSearch",
      config = list(resampler_config = list(type = "KFold", n = 3L))
    ))
  )
  expect_error(
    .list_to_TunerConfig(list(type = "Bogus")),
    class = "rtemis_unsupported_error"
  )
})
