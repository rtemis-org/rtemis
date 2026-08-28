# test_TrainPreflight.R
# ::rtemis::
# 2026- EDG rtemis.org

# `train(preflight = TRUE)`: the configuration checked against the data before
# the run starts spending. Off by default, so the tests that matter most are the
# ones showing it changes nothing when it is off.
#
# Three of these fit models to prove the pre-flight let the run through, and
# together they cost about nine seconds -- poor value inside a CRAN check, whose
# whole budget is around ten minutes. They carry `skip_on_cran()`.
#
# The test that stays is the one that *aborts*: no model is fitted, it costs
# under a tenth of a second, and it exercises the feature's actual point. What
# CRAN then does not verify is that a clean config still trains -- the paired
# "and this one works" half of the no-false-positive claim. That belongs to CI,
# which runs the whole suite, and is why it should.

.preflight_data <- function(n = 60L, minority = 30L) {
  set.seed(2026L)
  data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = factor(rep(c("no", "yes"), times = c(n - minority, minority)))
  )
}


test_that("preflight is off by default and changes nothing", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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


test_that("a SuperConfigLive is checked without being asked", {
  # The gate. `SuperConfigLive` binds its data rather than naming a path, which
  # is the shape a run submitted over the wire arrives in -- so the check
  # belongs to the type, not to each caller remembering the argument. Nothing
  # here passes `preflight`.
  cfg <- setup_SuperConfigLive(
    dat_training = .preflight_data(60L, minority = 4L),
    hyperparameters = setup_LightRF(),
    outer_resampling_config = setup_Resampler(n_resamples = 10L),
    verbosity = 0L
  )
  expect_error(
    train(cfg),
    "RESAMPLE_MIN_CLASS",
    class = "rtemis_preflight_error"
  )
})


test_that("a SuperConfigLive caller can still opt out", {
  skip_on_cran()
  cfg <- setup_SuperConfigLive(
    dat_training = .preflight_data(60L, minority = 4L),
    hyperparameters = setup_LightRF(),
    outer_resampling_config = setup_Resampler(n_resamples = 10L),
    verbosity = 0L
  )
  expect_no_error(train(cfg, preflight = FALSE))
})


test_that("the pre-flight reads every config property the checks read", {
  # `preflight_config()` reassembles a `SuperConfig` because the inner `train()`
  # holds its configuration as separate arguments. A property `config_parts()`
  # reads and the reassembly drops makes the pre-flight report something other
  # than `validate_config()` reports on the same run -- silently, and only for
  # configs that set it.
  #
  # Derived rather than listed: the property names come out of `config_parts()`
  # itself, so a new one arrives here without this test being edited.
  read_by_checks <- unique(unlist(regmatches(
    deparse(body(config_parts)),
    gregexpr(
      '(?<=config_prop\\(config, ")[^"]+',
      deparse(body(config_parts)),
      perl = TRUE
    )
  )))
  expect_gt(length(read_by_checks), 0L)
  expect_true(
    all(read_by_checks %in% names(formals(preflight_config))),
    info = paste(
      "config_parts() reads properties preflight_config() cannot carry:",
      paste(
        setdiff(read_by_checks, names(formals(preflight_config))),
        collapse = ", "
      )
    )
  )
})
