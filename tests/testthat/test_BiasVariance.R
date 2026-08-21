# test_BiasVariance.R
# ::rtemis::
# 2026- EDG rtemis.org

# `bias_variance()` estimates two quantities whose *relative* behavior theory
# pins down exactly, so the tests assert the theory rather than stored numbers:
# a deeper tree must trade bias for variance, bagging must cut a tree's variance
# while leaving its bias, and measuring bias against a noisy outcome rather than
# the true function must inflate it by the noise variance.

# %% Data ----
# `y = 2a + 1.5b + N(0, 1)`, so the true function and the noise variance are
# both known and every assertion below has a right answer.
set.seed(2026)
.n <- 400L
.features <- data.frame(a = rnorm(.n), b = rnorm(.n), c = rnorm(.n))
.truth <- 2 * .features[["a"]] + 1.5 * .features[["b"]]
.noise_variance <- 1
.dat <- data.frame(
  .features,
  y = .truth + rnorm(.n, sd = sqrt(.noise_variance))
)
.in_training <- sample.int(.n, 300L)
.train <- .dat[.in_training, ]
.test <- .dat[-.in_training, ]
.test_truth <- .truth[-.in_training]

.resampler <- setup_Resampler(
  n_resamples = 20L,
  type = "StratBoot",
  verbosity = 0L
)
.execution <- setup_ExecutionConfig(seed = 1L, backend = "none")

.bv <- function(
  hyperparameters,
  true_values = .test_truth,
  execution = .execution
) {
  bias_variance(
    .train,
    hyperparameters = hyperparameters,
    dat_test = .test,
    true_values = true_values,
    resampler_config = .resampler,
    execution_config = execution,
    verbosity = 0L
  )
}


# %% Shape ----
test_that("bias_variance() returns one estimate per test case", {
  result <- .bv(setup_CART())
  expect_s7_class(result, rtemis:::BiasVariance)
  expect_length(result@bias_squared, nrow(.test))
  expect_length(result@variance, nrow(.test))
  expect_true(all(result@bias_squared >= 0))
  expect_true(all(result@variance >= 0))
  expect_equal(result@mean_variance, mean(result@variance))
  expect_equal(result@mean_bias_squared, mean(result@bias_squared))
})


# %% The decomposition behaves as theory says ----
test_that("a correctly specified model has almost no bias and almost no variance", {
  # The process is linear, so a penalized linear model is the right shape for it
  # and both terms should be small next to a tree's.
  linear <- .bv(setup_GLMNET(lambda = 0.1))
  tree <- .bv(setup_CART())
  expect_lt(linear@mean_bias_squared, tree@mean_bias_squared)
  expect_lt(linear@mean_variance, tree@mean_variance)
})


test_that("growing a tree deeper trades bias for variance", {
  # The trade-off the decomposition exists to expose: the deeper tree fits the
  # training sample more closely, so its average prediction is closer to the
  # truth and its predictions move more between samples.
  shallow <- .bv(setup_CART())
  deep <- .bv(setup_CART(maxdepth = 30L, minsplit = 2L, cp = 0))
  expect_lt(deep@mean_bias_squared, shallow@mean_bias_squared)
  expect_gt(deep@mean_variance, shallow@mean_variance)
})


test_that("bagging cuts a tree's variance and leaves its bias", {
  # What a random forest is *for*, stated as a measurement.
  tree <- .bv(setup_CART())
  forest <- .bv(setup_Ranger())
  expect_lt(forest@mean_variance, tree@mean_variance / 2)
})


test_that("measuring bias against the outcome inflates it by the noise variance", {
  # Without `true_values` the reference carries the irreducible noise, so what
  # is reported is bias^2 + sigma^2. Measured at 0.997 against a true 1.0.
  exact <- .bv(setup_CART())
  contaminated <- .bv(setup_CART(), true_values = NULL)
  gap <- contaminated@mean_bias_squared - exact@mean_bias_squared
  expect_equal(gap, .noise_variance, tolerance = 0.15)
})


# %% Reproducibility ----
test_that("bias_variance() is reproducible, and parallel matches sequential", {
  once <- .bv(setup_CART())
  again <- .bv(setup_CART())
  expect_identical(once@variance, again@variance)
  expect_identical(once@bias_squared, again@bias_squared)
  skip_if_not_installed("mirai")
  parallel <- .bv(
    setup_CART(),
    execution = setup_ExecutionConfig(
      seed = 1L,
      backend = "mirai",
      n_workers = 2L
    )
  )
  expect_identical(parallel@variance, once@variance)
})


# %% Rejections ----
test_that("bias_variance() refuses a search space, with a corrective message", {
  # `setup_GLMNET()` searches `lambda` by default, so this is reachable without
  # asking for it and the message has to say what to do about it.
  expect_error(
    .bv(setup_GLMNET()),
    class = "rtemis_value_error"
  )
})


test_that("bias_variance() refuses inputs it cannot decompose", {
  expect_error(
    bias_variance(
      .train,
      setup_CART(),
      dat_test = .test,
      true_values = .test_truth[-1L],
      resampler_config = .resampler,
      verbosity = 0L
    ),
    class = "rtemis_length_error"
  )
  # `true_values` without a test set could not be known to be aligned with one.
  expect_error(
    bias_variance(
      .train,
      setup_CART(),
      true_values = .test_truth,
      resampler_config = .resampler,
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
  # One resample has no variance to report.
  expect_error(
    bias_variance(
      .train,
      setup_CART(),
      dat_test = .test,
      resampler_config = setup_Resampler(
        n_resamples = 1L,
        type = "StratBoot",
        verbosity = 0L
      ),
      verbosity = 0L
    ),
    class = "rtemis_range_error"
  )
})


test_that("bias_variance() refuses a multiclass outcome", {
  # 0-1 loss does not decompose additively into bias and variance.
  multiclass <- .train
  multiclass[["y"]] <- factor(sample(c("a", "b", "c"), nrow(.train), TRUE))
  expect_error(
    bias_variance(
      multiclass,
      setup_CART(),
      resampler_config = .resampler,
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


# %% Classification ----
test_that("bias_variance() decomposes a binary outcome on the probability scale", {
  binary <- .train
  binary[["y"]] <- factor(
    ifelse(binary[["y"]] > stats::median(binary[["y"]]), "hi", "lo"),
    levels = c("lo", "hi")
  )
  binary_test <- .test
  binary_test[["y"]] <- factor(
    ifelse(binary_test[["y"]] > stats::median(.train[["y"]]), "hi", "lo"),
    levels = c("lo", "hi")
  )
  result <- bias_variance(
    binary,
    setup_CART(),
    dat_test = binary_test,
    resampler_config = .resampler,
    execution_config = .execution,
    verbosity = 0L
  )
  expect_s7_class(result, rtemis:::BiasVariance)
  # Probabilities live in [0, 1], so neither term can exceed 1.
  expect_true(all(result@variance <= 1))
  expect_true(all(result@bias_squared <= 1))
})
