# test_TrainPreflight.R
# ::rtemis::
# 2026- EDG rtemis.org

# `train(preflight = TRUE)`: the configuration checked against the data before
# the run starts spending. Off by default, so the tests that matter most are the
# ones showing it changes nothing when it is off.

.preflight_data <- function(n = 60L, minority = 30L) {
  set.seed(2026L)
  data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = factor(rep(c("no", "yes"), times = c(n - minority, minority)))
  )
}


test_that("preflight is off by default and changes nothing", {
  # A config the pre-flight would reject still trains, because nothing asked.
  dat <- .preflight_data(60L, minority = 4L)
  expect_no_error(
    train(
      dat,
      hyperparameters = setup_LightRF(),
      outer_resampling_config = setup_Resampler(n_resamples = 10L),
      verbosity = 0L
    )
  )
})


test_that("preflight stops on an error finding, naming the code", {
  dat <- .preflight_data(60L, minority = 4L)
  expect_error(
    train(
      dat,
      hyperparameters = setup_LightRF(),
      outer_resampling_config = setup_Resampler(n_resamples = 10L),
      preflight = TRUE,
      verbosity = 0L
    ),
    "RESAMPLE_MIN_CLASS",
    class = "rtemis_preflight_error"
  )
})


test_that("preflight lets a clean configuration through", {
  expect_no_error(
    train(
      .preflight_data(),
      hyperparameters = setup_LightRF(),
      outer_resampling_config = setup_Resampler(n_resamples = 5L),
      preflight = TRUE,
      verbosity = 0L
    )
  )
})


test_that("preflight reports warnings without stopping", {
  # A constant predictor is a warning: the run completes, and the caller is
  # told. Whether that result is wanted is their judgment, not the check's.
  # A *factor* constant, because `check_supervised()` rejects a character
  # feature outright and would abort before the pre-flight is reached.
  dat <- .preflight_data()
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]
  expect_no_error(
    train(
      dat,
      hyperparameters = setup_LightRF(),
      outer_resampling_config = setup_Resampler(n_resamples = 5L),
      preflight = TRUE,
      verbosity = 0L
    )
  )
})
