# bias_variance.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% make_bias_variance_runner ----
#' Build the per-resample body
#'
#' Returns the closure `progress_plapply()` dispatches once per resample. Built
#' by a factory rather than inline so serializing it does not ship the calling
#' frame -- the training data, the test set and every intermediate -- to each
#' worker.
#'
#' @param x tabular data: Training set.
#' @param resamples List: Training index vectors, one per resample.
#' @param hyperparameters `Hyperparameters` object.
#' @param features tabular data: Test-set features to predict.
#' @param execution_config `ExecutionConfig` object: Config each nested `train()`
#' runs under.
#'
#' @return Function of `(i)` returning a numeric vector of predictions.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_bias_variance_runner <- function(
  x,
  resamples,
  hyperparameters,
  features,
  execution_config
) {
  force(x)
  force(resamples)
  force(hyperparameters)
  force(features)
  force(execution_config)
  function(i) {
    model <- train(
      x[resamples[[i]], , drop = FALSE],
      hyperparameters = hyperparameters,
      execution_config = execution_config,
      verbosity = 0L
    )
    as.numeric(predict(model, features))
  }
} # /rtemis::make_bias_variance_runner


# %% bias_variance ----
#' Estimate Bias and Variance
#'
#' Decompose a learner's error into the part that comes from being
#' systematically wrong and the part that comes from being unstable, by
#' refitting it over many resamples of the training set and watching what its
#' predictions do at a **fixed** test set.
#'
#' @details
#' For squared-error loss, the expected error at a point decomposes as
#'
#' \deqn{E[(y - \hat{f}(x))^2] = \sigma^2 + (E[\hat{f}(x)] - f(x))^2 + Var[\hat{f}(x)]}
#'
#' -- irreducible noise, squared bias, and variance. This function estimates the
#' second and third per test case, by fitting the model on `n_resamples`
#' resamples of the training set and reading the spread and the average of the
#' predictions each makes at the same test cases.
#'
#' The test set is held out **once** and every resample is drawn from what is
#' left, which is what makes the predictions comparable across resamples: a test
#' set that moved would confound the model's variability with the sample's.
#'
#' @section Whether the bias is really the bias:
#' Squared bias is measured against `true_values` when you have them, and
#' against the observed outcome when you do not. The difference is not
#' cosmetic:
#'
#' \describe{
#'   \item{With `true_values`}{`bias_squared` is the bias, exactly. Only
#'     simulated data has this, which is what makes a simulation study the place
#'     to ask how a learner divides its error.}
#'   \item{Without}{The observed outcome carries the irreducible noise, so what
#'     is reported is \eqn{bias^2 + \sigma^2} and no amount of resampling
#'     separates them. It stays comparable **between learners on the same data**
#'     -- they share the same \eqn{\sigma^2} -- so a ranking is still meaningful
#'     even though the level is inflated.}
#' }
#'
#' @section What the numbers are, exactly:
#' `variance` is the sample variance across resamples (dividing by
#' `n_resamples - 1`), which is unbiased for the variance of the fitted
#' function. `bias_squared` is the plug-in estimate, and is inflated by roughly
#' `variance / n_resamples` because the mean prediction is itself estimated --
#' so prefer many resamples over few, and treat a `bias_squared` of the same
#' order as `variance / n_resamples` as indistinguishable from zero.
#'
#' @section Classification:
#' The decomposition above is for squared error, and 0-1 loss does not
#' decompose additively into bias and variance. For a binary outcome this
#' function therefore decomposes the squared error **of the predicted
#' probability** of the positive class, which does. `true_values` is then the
#' true probability, not the class. Multiclass is not supported.
#'
#' @param x tabular data: Full dataset, outcome in the last column. Resamples
#' are drawn from it after the test set is held out.
#' @param hyperparameters `Hyperparameters` object: Must be fixed, not a search
#' space -- this measures one model, not a tuning run.
#' @param dat_test Optional tabular data: Test set. NULL holds one out of `x`.
#' @param true_values Optional Numeric vector: The true function at the test
#' cases, in their order. Regression: `f(x)`. Classification: the probability of
#' the positive class. NULL measures bias against the observed outcome, which
#' includes irreducible noise.
#' @param resampler_config Optional `ResamplerConfig` object: How training sets
#' are drawn. NULL uses 100 stratified bootstraps.
#' @param test_p Numeric (0, 1): Fraction of `x` held out as the test set.
#' Ignored when `dat_test` is given.
#' @param execution_config `ExecutionConfig` object: Supplies the seed and the
#' workers the resamples are fitted over.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return `BiasVariance` object.
#'
#' @references
#' Geman S, Bienenstock E, Doursat R (1992). Neural Networks and the
#' Bias/Variance Dilemma. \emph{Neural Computation}, 4(1), 1-58.
#' \doi{10.1162/neco.1992.4.1.1}
#'
#' @author EDG
#' @export
#' @examples
#' set.seed(2026)
#' n <- 200L
#' features <- data.frame(a = rnorm(n), b = rnorm(n))
#' dat <- data.frame(features, y = 2 * features$a + rnorm(n))
#' bv <- bias_variance(
#'   dat,
#'   hyperparameters = setup_CART(),
#'   resampler_config = setup_StratBoot(n_resamples = 10L),
#'   verbosity = 0L
#' )
#' bv
bias_variance <- function(
  x,
  hyperparameters,
  dat_test = NULL,
  true_values = NULL,
  resampler_config = NULL,
  test_p = 0.3,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Input ----
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(execution_config, ExecutionConfig)
  if (needs_tuning(hyperparameters)) {
    # Some algorithms tune by default -- `setup_GLMNET()` searches `lambda` --
    # so this is reachable without asking for it, and the message has to say
    # what to do. Allowing it is not a smaller decision than it looks: each
    # resample would run its own inner search, multiplying the cost by the grid,
    # and the quantity measured becomes the variability of a *tuning procedure*
    # rather than of a model. That is a legitimate thing to want and a different
    # thing to report, so it is opted into by fixing the values, not by default.
    rtemis.core::abort(
      "bias_variance() measures one fixed model, but these hyperparameters define a search space. ",
      "Set the tunable values to single values, e.g. setup_GLMNET(lambda = 0.1).",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (test_p <= 0 || test_p >= 1) {
    rtemis.core::abort(
      "`test_p` must be in (0, 1), not ",
      test_p,
      ".",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  check_supervised(x = x, verbosity = verbosity)
  type <- supervised_type(x)
  if (type == "Classification" && nlevels(outcome(x)) > 2L) {
    rtemis.core::abort(
      "bias_variance() does not support multiclass classification: 0-1 loss does not decompose additively.",
      class = "rtemis_unsupported_error"
    )
  }
  if (is.null(resampler_config)) {
    resampler_config <- setup_StratBoot(
      n_resamples = 100L
    )
  }
  check_is_S7(resampler_config, ResamplerConfig)
  if (is.null(resampler_config@seed)) {
    # Without this the resamples come from the ambient RNG, so two identical
    # calls in one session give different answers -- which for a function whose
    # output is an estimate of variability is the worst place to be unseeded.
    # A `ResamplerConfig` seed governs how data is split and the execution seed
    # governs computation; tying them here is only a default, and a config that
    # names its own seed keeps it.
    resampler_config@seed <- execution_config@seed
  }

  # Test set ----
  # Held out once, so every resample predicts the same cases: a test set that
  # moved would confound the model's variability with the sample's.
  if (is.null(dat_test)) {
    if (!is.null(true_values)) {
      rtemis.core::abort(
        "`true_values` needs `dat_test`, so the two are known to be aligned.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    held_out <- resample(
      outcome(x),
      config = setup_StratSub(
        n_resamples = 1L,
        train_p = 1 - test_p,
        seed = execution_config@seed
      ),
      verbosity = 0L
    )@resamples[[1L]]
    dat_test <- x[-held_out, , drop = FALSE]
    x <- x[held_out, , drop = FALSE]
  }
  test_features <- features(dat_test)
  reference <- if (is.null(true_values)) {
    if (verbosity > 0L) {
      info(
        "No `true_values`: squared bias is measured against the observed ",
        "outcome and so includes the irreducible noise."
      )
    }
    if (type == "Classification") {
      # The positive class is the second level, matching `predict()`.
      as.numeric(outcome(dat_test) == levels(outcome(dat_test))[[2L]])
    } else {
      as.numeric(outcome(dat_test))
    }
  } else {
    if (!is.numeric(true_values)) {
      rtemis.core::abort(
        "`true_values` must be numeric, not ",
        class(true_values)[[1L]],
        ".",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    if (length(true_values) != NROW(dat_test)) {
      rtemis.core::abort(
        "`true_values` must have one value per test case: got ",
        length(true_values),
        " for ",
        NROW(dat_test),
        ".",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
    true_values
  }

  # Resamples ----
  resampler <- resample(x, config = resampler_config, verbosity = 0L)
  n_resamples <- length(resampler@resamples)
  if (n_resamples < 2L) {
    rtemis.core::abort(
      "Need at least 2 resamples to estimate a variance, not ",
      n_resamples,
      ".",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  if (verbosity > 0L) {
    msg0(
      "Estimating bias and variance over ",
      highlight(n_resamples),
      " resamples of ",
      highlight(NROW(x)),
      " cases, at ",
      highlight(NROW(dat_test)),
      " fixed test cases."
    )
  }

  # Fit ----
  # A resample runs inside a worker and must not dispatch again, so the nested
  # `train()` gets a sequential config -- the same rule outer resampling follows.
  dispatching <- execution_config@backend != "none" &&
    execution_config@n_workers > 1L
  inner_config <- if (dispatching) {
    ExecutionConfig(
      backend = "none",
      n_workers = 1L,
      seed = execution_config@seed,
      on_error = execution_config@on_error
    )
  } else {
    execution_config
  }
  predictions <- progress_plapply(
    seq_len(n_resamples),
    make_bias_variance_runner(
      x = x,
      resamples = resampler@resamples,
      hyperparameters = hyperparameters,
      features = test_features,
      execution_config = inner_config
    ),
    backend = execution_config@backend,
    n_workers = execution_config@n_workers,
    future_plan = resolve_future_plan(
      execution_config@backend,
      execution_config@future_plan
    ),
    # One substream per resample, by index, so the estimate does not depend on
    # the worker count.
    seeds = rng_substreams(execution_config@seed, n_resamples),
    label = "Resamples",
    stop_on_error = TRUE,
    verbosity = verbosity
  )
  predicted <- matrix(
    unlist(predictions, use.names = FALSE),
    nrow = NROW(dat_test),
    ncol = n_resamples
  )

  # Decompose ----
  # `var()` divides by `n - 1`, which is unbiased for the variance of the fitted
  # function. The squared bias is the plug-in estimate and is inflated by about
  # `variance / n_resamples`, since the mean prediction is itself estimated.
  variance <- apply(predicted, 1L, stats::var)
  bias_squared <- (rowMeans(predicted) - reference)^2
  BiasVariance(
    bias_squared = bias_squared,
    mean_bias_squared = mean(bias_squared),
    sd_bias_squared = stats::sd(bias_squared),
    variance = variance,
    mean_variance = mean(variance),
    sd_variance = stats::sd(variance)
  )
} # /rtemis::bias_variance
