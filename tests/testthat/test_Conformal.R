# test_Conformal.R
# ::rtemis::
# 2026- EDG rtemis.org

# The load-bearing tests here are the coverage simulations. Everything else --
# shapes, refusals, resolved names -- can be right while the arithmetic is
# wrong, and only repeated sampling can tell the difference between a valid
# region and a plausible-looking one.
#
# Each simulation is sized so its Monte Carlo standard error is small against
# the tolerance it asserts, and seeded, so a failure is a regression rather than
# a bad day. See `plan/conformal.md`.

# %% Fixtures ----

# A regression whose noise does not depend on the features: split conformal's
# constant width is the right shape for it, which makes it the baseline the
# adaptive constructions are compared against.
.conformal_reg <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- data.frame(a = stats::rnorm(n), b = stats::rnorm(n))
  x[["y"]] <- 2 * x[["a"]] + stats::rnorm(n, sd = 0.5)
  x
}


# Heteroscedastic: the noise grows with |a|, so a constant-width interval is
# too wide where the model is confident and too narrow where it is not. This is
# what CQR exists for.
.conformal_hetero <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- data.frame(a = stats::runif(n, -3, 3), b = stats::rnorm(n))
  x[["y"]] <- 2 * x[["a"]] + stats::rnorm(n, sd = 0.2 + 0.6 * abs(x[["a"]]))
  x
}


# Three classes with genuinely overlapping conditional distributions, so the
# Bayes error is well away from zero and a prediction set has something to say.
.conformal_clf <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- data.frame(a = stats::rnorm(n), b = stats::rnorm(n))
  eta <- 1.5 * x[["a"]] - x[["b"]]
  prob <- exp(cbind(0, eta, -eta))
  prob <- prob / rowSums(prob)
  x[["y"]] <- factor(apply(prob, 1L, function(p) {
    sample(c("A", "B", "C"), 1L, prob = p)
  }))
  x
}


.split_model <- function(x, n_train, n_calibration, hyperparameters) {
  train(
    x[seq_len(n_train), ],
    dat_test = x[n_train + seq_len(n_calibration), ],
    hyperparameters = hyperparameters,
    verbosity = 0L
  )
}


.features <- function(x) {
  x[, c("a", "b"), drop = FALSE]
}


# Replace named entries outright. `utils::modifyList()` recurses into a
# list-valued entry and merges it by name, so replacing an *unnamed* list -- a
# region's `sets` -- silently leaves the original in place and the malformed
# input under test never reaches the validator.
.replace <- function(args, ...) {
  new <- list(...)
  args[names(new)] <- new
  args
}


# %% Configs ----

test_that("a conformal config carries its construction and its level", {
  expect_s7_class(setup_SplitConformal(), SplitConformalConfig)
  expect_s7_class(setup_CVPlus(), CVPlusConfig)
  expect_s7_class(setup_CQR(), CQRConfig)
  expect_identical(setup_SplitConformal()@type, "Split")
  expect_identical(setup_CVPlus()@type, "CVPlus")
  expect_identical(setup_CQR()@type, "CQR")
  expect_identical(setup_SplitConformal(alpha = 0.05)@alpha, 0.05)
  expect_null(setup_SplitConformal()@score)
  expect_identical(setup_SplitConformal(score = "LAC")@score, "LAC")
})


test_that("a conformal config rejects an out-of-range level", {
  expect_error(setup_SplitConformal(alpha = 0))
  expect_error(setup_SplitConformal(alpha = 1))
  expect_error(setup_SplitConformal(alpha = -0.1))
  expect_error(setup_SplitConformal(score = "nope"))
})


test_that("an unseeded conformal config records the seed it drew", {
  # The `ExecutionConfig` contract: an unseeded run is still reproducible,
  # because the value that ran is on the config and therefore in the record.
  cfg <- setup_SplitConformal()
  expect_true(is.integer(cfg@seed))
  expect_gte(cfg@seed, 0L)
  expect_identical(setup_SplitConformal(seed = 7L)@seed, 7L)
  # Drawn from the caller's stream, so a seeded caller gets a seeded config.
  set.seed(1L)
  first <- setup_CVPlus()@seed
  set.seed(1L)
  expect_identical(setup_CVPlus()@seed, first)
})


test_that("a conformal config serializes its settings as siblings of the tag", {
  wire <- serializable_props(setup_SplitConformal(alpha = 0.2, seed = 3L))
  expect_identical(wire[["type"]], "Split")
  expect_identical(wire[["alpha"]], 0.2)
  expect_identical(wire[["seed"]], 3L)
  expect_false("config" %in% names(wire))
})


# %% Order statistics ----

test_that("the conformal order statistic is the finite-sample one", {
  # `ceiling((n + 1) * (1 - alpha))`, not the plain empirical quantile: the
  # `+ 1` counts the fresh case as if it were already calibrated, and it is
  # what makes the guarantee hold in finite samples.
  expect_identical(rtemis:::conformal_order(100L, 0.1), 91L)
  expect_identical(rtemis:::conformal_order(19L, 0.05), 19L)
  expect_identical(rtemis:::conformal_order(9L, 0.1), 9L)
})


test_that("a calibration set too small for alpha is refused, not widened", {
  # The order statistic falls off the end below `ceiling(1 / alpha) - 1` cases.
  # An infinite region is the valid answer and a useless one, so it is reported
  # as the input problem it is.
  expect_error(
    rtemis:::conformal_quantile(runif(18L), 0.05),
    "at least 19 calibration cases"
  )
  expect_no_error(rtemis:::conformal_quantile(runif(19L), 0.05))
})


test_that("the conformal quantile is the k-th smallest calibration score", {
  scores <- c(5, 1, 4, 2, 3, 9, 7, 6, 8, 10)
  # n = 10, alpha = 0.2 -> k = ceiling(11 * 0.8) = 9 -> the 9th smallest.
  expect_identical(rtemis:::conformal_quantile(scores, 0.2), 9)
})


# %% Split conformal, regression ----

test_that("split conformal returns a symmetric interval around the prediction", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  region <- conformal(mod, .features(x[251:300, ]), verbosity = 0L)

  expect_s7_class(region, PredictionInterval)
  expect_identical(region@method, "Split")
  expect_identical(region@score, "absolute")
  expect_identical(region@n_calibration, 100L)
  expect_length(region@lower, 50L)
  # One number added and subtracted, so every width is 2q and the interval is
  # centered on the point prediction.
  expect_equal(region@upper - region@predicted, rep(region@q, 50L))
  expect_equal(region@predicted - region@lower, rep(region@q, 50L))
  expect_equal(region@width, rep(2 * region@q, 50L))
})


test_that("split conformal delivers the nominal guarantee, and says so", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  region <- conformal(
    mod,
    .features(x[251:300, ]),
    config = setup_SplitConformal(alpha = 0.2),
    verbosity = 0L
  )
  expect_equal(region@coverage, 0.8)
})


test_that("a smaller alpha buys a wider interval", {
  x <- .conformal_reg(400L, seed = 11L)
  mod <- .split_model(x, 200L, 150L, setup_GLM())
  newdata <- .features(x[351:400, ])
  wide <- conformal(
    mod,
    newdata,
    config = setup_SplitConformal(alpha = 0.01),
    verbosity = 0L
  )
  narrow <- conformal(
    mod,
    newdata,
    config = setup_SplitConformal(alpha = 0.3),
    verbosity = 0L
  )
  expect_gt(mean(wide@width), mean(narrow@width))
})


# %% Coverage simulations ----

test_that("split conformal attains nominal coverage over replications", {
  # The test this feature stands on. Coverage conditional on one calibration
  # set is Beta-distributed around `1 - alpha`, so a single run says little;
  # averaged over replications it must sit at the nominal level.
  skip_on_cran()
  set.seed(4242L)
  coverage <- vapply(
    seq_len(120L),
    function(i) {
      x <- .conformal_reg(260L)
      mod <- .split_model(x, 150L, 60L, setup_GLM())
      held_out <- x[211:260, ]
      region <- conformal(mod, .features(held_out), verbosity = 0L)
      mean(
        held_out[["y"]] >= region@lower & held_out[["y"]] <= region@upper
      )
    },
    numeric(1L)
  )
  # 120 replications x 50 cases: the standard error of the mean is near 0.006,
  # so 0.02 is a wide band around a correct implementation and a tight one
  # around a construction off by an order statistic.
  expect_equal(mean(coverage), 0.9, tolerance = 0.02)
})


test_that("CV+ attains nominal coverage over replications", {
  skip_on_cran()
  set.seed(5252L)
  coverage <- vapply(
    seq_len(60L),
    function(i) {
      x <- .conformal_reg(220L)
      mod <- train(
        x[1:150, ],
        outer_resampling_config = setup_Resampler(5L, "KFold"),
        hyperparameters = setup_GLM(),
        verbosity = 0L
      )
      held_out <- x[151:220, ]
      region <- conformal(mod, .features(held_out), verbosity = 0L)
      mean(
        held_out[["y"]] >= region@lower & held_out[["y"]] <= region@upper
      )
    },
    numeric(1L)
  )
  # CV+ guarantees `1 - 2 * alpha` and in practice lands near `1 - alpha`. The
  # assertion is the guarantee, which is what the object promises; the second
  # is the practice, and would catch a construction that met the bound only by
  # being uselessly wide.
  expect_gte(mean(coverage), 0.8)
  expect_equal(mean(coverage), 0.9, tolerance = 0.03)
})


test_that("a misspecified model keeps coverage and pays in width", {
  # Conformal makes no claim about the model, only about exchangeability. A
  # model fitted to the wrong functional form must still cover -- with
  # intervals wide enough to say so, which is the whole "width is the quality
  # measure" point.
  skip_on_cran()
  set.seed(6262L)
  n <- 400L
  x <- data.frame(a = stats::runif(n, -3, 3), b = stats::rnorm(n))
  # Strongly nonlinear in `a`; the model below sees only a linear term.
  x[["y"]] <- x[["a"]]^3 + stats::rnorm(n, sd = 0.3)

  bad <- train(
    x[1:200, ],
    dat_test = x[201:300, ],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  good <- train(
    x[1:200, ],
    dat_test = x[201:300, ],
    hyperparameters = setup_Ranger(),
    verbosity = 0L
  )
  held_out <- x[301:400, ]
  bad_region <- conformal(bad, .features(held_out), verbosity = 0L)
  good_region <- conformal(good, .features(held_out), verbosity = 0L)

  expect_gte(
    conformal_metrics(bad_region, held_out[["y"]])[["coverage"]],
    0.8
  )
  expect_gt(mean(bad_region@width), 2 * mean(good_region@width))
})


test_that("a shifted test distribution loses coverage", {
  # Exchangeability is the whole assumption, and this is what its failure looks
  # like: the same model, the same calibration set, the same alpha, and no
  # symptom anywhere in the returned numbers. Tested so the limitation is a
  # documented property rather than a surprise.
  skip_on_cran()
  set.seed(7272L)
  x <- .conformal_hetero(400L, seed = 7272L)
  mod <- .split_model(x, 200L, 150L, setup_Ranger())

  # Drawn from the tail the calibration set barely reaches, where the noise is
  # widest -- a shift in the covariates, not in the relationship.
  shifted <- data.frame(a = stats::runif(200L, 2.5, 3), b = stats::rnorm(200L))
  shifted[["y"]] <- 2 *
    shifted[["a"]] +
    stats::rnorm(200L, sd = 0.2 + 0.6 * abs(shifted[["a"]]))

  in_distribution <- conformal(
    mod,
    .features(x[351:400, ]),
    verbosity = 0L
  )
  out_of_distribution <- conformal(mod, .features(shifted), verbosity = 0L)

  expect_gte(
    conformal_metrics(in_distribution, x[["y"]][351:400])[["coverage"]],
    0.8
  )
  expect_lt(
    conformal_metrics(out_of_distribution, shifted[["y"]])[["coverage"]],
    0.85
  )
})


# %% Split conformal, classification ----

test_that("split conformal builds label sets and preserves empty ones", {
  x <- .conformal_clf(600L, seed = 2026L)
  mod <- .split_model(x, 300L, 150L, setup_Ranger())
  region <- conformal(mod, .features(x[451:600, ]), verbosity = 0L)

  expect_s7_class(region, PredictionSet)
  expect_identical(region@score, "APS")
  expect_identical(region@classes, c("A", "B", "C"))
  expect_length(region@sets, 150L)
  expect_true(all(vapply(region@sets, is.character, logical(1L))))
  expect_true(all(unlist(region@sets) %in% region@classes))
  expect_identical(region@set_size, lengths(region@sets))
  expect_identical(dim(region@predicted_prob), c(150L, 3L))
  expect_equal(rowSums(region@predicted_prob), rep(1, 150L))
})


test_that("randomized APS discriminates where the deterministic score cannot", {
  # Without the draw, every case whose true label ranks last scores exactly 1,
  # so a model erring on more than `alpha` of its cases puts the threshold at 1
  # and every set holds every label. This is the assertion that the draw is
  # load-bearing rather than cosmetic.
  x <- .conformal_clf(600L, seed = 99L)
  mod <- .split_model(x, 300L, 150L, setup_Ranger())
  region <- conformal(
    mod,
    .features(x[451:600, ]),
    config = setup_SplitConformal(seed = 7L),
    verbosity = 0L
  )
  expect_lt(mean(region@set_size), length(region@classes))
  expect_gt(mean(region@set_size == 1L), 0)
})


test_that("APS and LAC are both valid and trade set size for adaptivity", {
  skip_on_cran()
  x <- .conformal_clf(900L, seed = 2026L)
  mod <- .split_model(x, 400L, 250L, setup_Ranger())
  held_out <- x[651:900, ]
  aps <- conformal(
    mod,
    .features(held_out),
    config = setup_SplitConformal(seed = 7L),
    verbosity = 0L
  )
  lac <- conformal(
    mod,
    .features(held_out),
    config = setup_SplitConformal(score = "LAC"),
    verbosity = 0L
  )
  expect_gte(conformal_metrics(aps, held_out[["y"]])[["coverage"]], 0.85)
  expect_gte(conformal_metrics(lac, held_out[["y"]])[["coverage"]], 0.85)
  # LAC buys the smaller sets it is chosen for.
  expect_lte(mean(lac@set_size), mean(aps@set_size))
})


test_that("classification coverage holds over replications", {
  skip_on_cran()
  set.seed(8282L)
  coverage <- vapply(
    seq_len(60L),
    function(i) {
      x <- .conformal_clf(500L)
      mod <- .split_model(x, 250L, 150L, setup_Ranger())
      held_out <- x[401:500, ]
      region <- conformal(mod, .features(held_out), verbosity = 0L)
      conformal_metrics(region, held_out[["y"]])[["coverage"]]
    },
    numeric(1L)
  )
  expect_equal(mean(coverage), 0.9, tolerance = 0.03)
})


test_that("a binary outcome is set-valued over both of its classes", {
  # `Classification` stores one probability column for a binary outcome, so the
  # widening back to two is a real step and not a formality.
  x <- .conformal_clf(600L, seed = 5L)
  x[["y"]] <- factor(ifelse(x[["y"]] == "A", "yes", "no"))
  mod <- .split_model(x, 300L, 150L, setup_GLM())
  region <- conformal(
    mod,
    .features(x[451:600, ]),
    config = setup_SplitConformal(seed = 3L),
    verbosity = 0L
  )
  expect_identical(region@classes, c("no", "yes"))
  expect_identical(dim(region@predicted_prob), c(150L, 2L))
  expect_equal(rowSums(region@predicted_prob), rep(1, 150L))
  expect_gte(conformal_metrics(region, x[["y"]][451:600])[["coverage"]], 0.82)
})


test_that("a score belonging to the other outcome type is refused", {
  x <- .conformal_reg(200L, seed = 1L)
  reg <- .split_model(x, 120L, 60L, setup_GLM())
  expect_error(
    conformal(
      reg,
      .features(x[181:200, ]),
      config = setup_SplitConformal(score = "APS"),
      verbosity = 0L
    ),
    "this is a regression"
  )

  xc <- .conformal_clf(300L, seed = 1L)
  clf <- .split_model(xc, 180L, 80L, setup_Ranger())
  expect_error(
    conformal(
      clf,
      .features(xc[261:300, ]),
      config = setup_SplitConformal(score = "absolute"),
      verbosity = 0L
    ),
    "this is a classification"
  )
})


# %% Reproducibility ----

test_that("a seeded region reproduces exactly", {
  x <- .conformal_clf(400L, seed = 13L)
  mod <- .split_model(x, 200L, 120L, setup_Ranger())
  newdata <- .features(x[321:400, ])
  first <- conformal(
    mod,
    newdata,
    config = setup_SplitConformal(seed = 42L),
    verbosity = 0L
  )
  second <- conformal(
    mod,
    newdata,
    config = setup_SplitConformal(seed = 42L),
    verbosity = 0L
  )
  expect_identical(first@sets, second@sets)
  expect_identical(first@q, second@q)
})


test_that("conformal does not disturb the caller's RNG", {
  # Two draws in one: the APS uniforms, and the seed `ranger::predict()` takes
  # from the R stream when it is not given one -- which a fold construction
  # would consume once per fold.
  x <- .conformal_clf(300L, seed = 17L)
  mod <- .split_model(x, 180L, 80L, setup_Ranger())
  set.seed(1L)
  expected <- stats::runif(3L)
  set.seed(1L)
  conformal(
    mod,
    .features(x[261:300, ]),
    config = setup_SplitConformal(seed = 42L),
    verbosity = 0L
  )
  expect_identical(stats::runif(3L), expected)
})


# %% Calibration provenance ----

test_that("supplied calibration data overrides the stored split", {
  x <- .conformal_reg(400L, seed = 3L)
  mod <- .split_model(x, 200L, 60L, setup_GLM())
  supplied <- x[261:400, ]
  region <- conformal(
    mod,
    .features(x[261:280, ]),
    calibration = supplied,
    verbosity = 0L
  )
  expect_identical(region@n_calibration, 140L)
  # A supplied set is fingerprinted; the stored split is the model's own data
  # and is already identified by the model.
  expect_s7_class(region@calibration_fingerprint, DataFingerprint)
})


test_that("a model with no held-out data refuses rather than guessing", {
  x <- .conformal_reg(200L, seed = 3L)
  mod <- train(x[1:150, ], hyperparameters = setup_GLM(), verbosity = 0L)
  expect_error(
    conformal(mod, .features(x[151:200, ]), verbosity = 0L),
    "no calibration data"
  )
})


test_that("a validation split is not taken as calibration data", {
  # Defensible for most algorithms and a trap for four of them, and rtemis
  # cannot see which users selected on. Refusing costs an argument; guessing
  # costs a silent guarantee.
  x <- .conformal_reg(300L, seed = 3L)
  mod <- train(
    x[1:150, ],
    dat_validation = x[151:250, ],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_error(
    conformal(mod, .features(x[251:300, ]), verbosity = 0L),
    "no calibration data"
  )
})


test_that("a test split that fitted the probability calibrator is refused", {
  # `calibrate()` takes the probabilities to fit on as an argument, and the
  # test split's are a natural thing to hand it. Where that happened, the split
  # is no longer exchangeable with a fresh case.
  x <- .conformal_clf(400L, seed = 21L)
  x[["y"]] <- factor(ifelse(x[["y"]] == "A", "yes", "no"))
  mod <- .split_model(x, 200L, 100L, setup_GLM())
  calibrated <- calibrate(
    mod,
    predicted_probabilities = mod@predicted_prob_test[, 1L],
    true_labels = mod@y_test,
    verbosity = 0L
  )
  expect_error(
    conformal(calibrated, .features(x[301:400, ]), verbosity = 0L),
    "fitted on this model's test split"
  )
  # And supplying untouched calibration data is the way through.
  expect_no_error(
    conformal(
      calibrated,
      .features(x[301:400, ]),
      calibration = x[301:400, ],
      config = setup_SplitConformal(seed = 5L),
      verbosity = 0L
    )
  )
})


# %% CV+ over resamples ----

test_that("CV+ reads the fold models and names what it resolved to", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- train(
    x[1:200, ],
    outer_resampling_config = setup_Resampler(5L, "KFold", seed = 1L),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  region <- conformal(mod, .features(x[201:300, ]), verbosity = 0L)

  expect_s7_class(region, PredictionInterval)
  expect_identical(region@method, "CVPlus")
  # Every case calibrates, which is the whole point: nothing is spent.
  expect_identical(region@n_calibration, 200L)
  # A fold construction has no single threshold.
  expect_null(region@q)
  # And it says which guarantee it carries, which is the conservative one.
  expect_equal(region@coverage, 0.8)
  expect_true(all(region@lower <= region@upper))
})


test_that("leave-one-out folds resolve to jackknife+", {
  x <- .conformal_reg(120L, seed = 4L)
  mod <- train(
    x[1:60, ],
    outer_resampling_config = setup_Resampler(type = "LOOCV"),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  region <- conformal(mod, .features(x[61:80, ]), verbosity = 0L)
  expect_identical(region@method, "JackknifePlus")
  expect_identical(region@n_calibration, 60L)
})


test_that("a classification over folds resolves to cross-conformal", {
  x <- .conformal_clf(400L, seed = 6L)
  mod <- train(
    x[1:250, ],
    outer_resampling_config = setup_Resampler(5L, "KFold", seed = 1L),
    hyperparameters = setup_Ranger(),
    verbosity = 0L
  )
  region <- conformal(mod, .features(x[251:400, ]), verbosity = 0L)
  expect_s7_class(region, PredictionSet)
  expect_identical(region@method, "CrossConformal")
  expect_identical(region@n_calibration, 250L)
  expect_gte(
    conformal_metrics(region, x[["y"]][251:400])[["coverage"]],
    0.8
  )
})


test_that("CV+ refuses folds that do not partition the cases", {
  # The precondition is a property of the indices, so it is checked on them: a
  # stratified subsample holds cases out more than once and others not at all.
  x <- .conformal_reg(300L, seed = 8L)
  mod <- train(
    x[1:200, ],
    outer_resampling_config = setup_Resampler(4L, "StratSub", seed = 1L),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_error(
    conformal(mod, .features(x[201:300, ]), verbosity = 0L),
    "held out exactly once"
  )
})


test_that("CV+ takes no calibration data, and split conformal no resamples", {
  x <- .conformal_reg(300L, seed = 9L)
  res_mod <- train(
    x[1:200, ],
    outer_resampling_config = setup_Resampler(5L, "KFold", seed = 1L),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_error(
    conformal(
      res_mod,
      .features(x[201:300, ]),
      calibration = x[201:300, ],
      verbosity = 0L
    ),
    "takes no `calibration` data"
  )
  expect_error(
    conformal(
      res_mod,
      .features(x[201:300, ]),
      config = setup_SplitConformal(),
      verbosity = 0L
    ),
    "applies to a single fitted model"
  )

  single <- .split_model(x, 150L, 100L, setup_GLM())
  expect_error(
    conformal(
      single,
      .features(x[251:300, ]),
      config = setup_CVPlus(),
      verbosity = 0L
    ),
    "outer resampling"
  )
})


# %% CQR ----

test_that("CQR adapts its width where split conformal cannot", {
  skip_on_cran()
  x <- .conformal_hetero(900L, seed = 2026L)
  mod <- train(
    x[1:400, ],
    hyperparameters = setup_Ranger(quantreg = TRUE),
    verbosity = 0L
  )
  calibration <- x[401:650, ]
  held_out <- x[651:900, ]
  cqr <- conformal(
    mod,
    .features(held_out),
    calibration = calibration,
    config = setup_CQR(),
    verbosity = 0L
  )
  split <- conformal(
    mod,
    .features(held_out),
    calibration = calibration,
    verbosity = 0L
  )

  expect_identical(cqr@method, "CQR")
  expect_equal(cqr@coverage, 0.9)
  expect_gte(conformal_metrics(cqr, held_out[["y"]])[["coverage"]], 0.82)
  # The point of the method: width tracks the noise, which is a function of
  # `a` here, where split conformal's constant width cannot.
  expect_gt(stats::cor(cqr@width, abs(held_out[["a"]])), 0.4)
  expect_lt(mean(cqr@width), mean(split@width))
})


test_that("CQR refuses a backend that cannot answer a quantile query", {
  x <- .conformal_hetero(300L, seed = 12L)
  mod <- train(x[1:150, ], hyperparameters = setup_GLM(), verbosity = 0L)
  expect_error(
    conformal(
      mod,
      .features(x[251:300, ]),
      calibration = x[151:250, ],
      config = setup_CQR(),
      verbosity = 0L
    ),
    "cannot predict quantiles"
  )
})


test_that("CQR refuses a Ranger forest not trained for quantiles", {
  x <- .conformal_hetero(300L, seed = 12L)
  mod <- train(x[1:150, ], hyperparameters = setup_Ranger(), verbosity = 0L)
  expect_error(
    conformal(
      mod,
      .features(x[251:300, ]),
      calibration = x[151:250, ],
      config = setup_CQR(),
      verbosity = 0L
    ),
    "quantreg = TRUE"
  )
})


test_that("CQR reports an inverting correction rather than an inverted band", {
  # `q` goes negative when the model's band over-covers, and CQR narrows by that
  # much. The training noise is large for `a > 0` and negligible for `a < 0`, so
  # the forest fits a wide band on one side and a narrow one on the other.
  # Calibrating only on the wide side, with outcomes sitting at the band's
  # center, makes the correction that side's half-width; the cases to bound come
  # from the narrow side, whose own bands are a fraction of it, so the band
  # comes back inverted by an order of magnitude rather than marginally.
  # Reported as the cause -- an over-wide quantile model -- rather than left to
  # the class validator, whose message describes only the symptom.
  set.seed(77L)
  a <- stats::runif(600L, -3, 3)
  training <- data.frame(a = a, b = stats::rnorm(600L))
  training[["y"]] <- 2 * a + stats::rnorm(600L, sd = ifelse(a > 0, 10, 0.01))
  wide_side <- stats::runif(200L, 0.5, 3)
  calibration <- data.frame(a = wide_side, b = stats::rnorm(200L))
  calibration[["y"]] <- 2 * wide_side + stats::rnorm(200L, sd = 0.01)
  held_out <- data.frame(
    a = stats::runif(100L, -3, -0.5),
    b = stats::rnorm(100L)
  )

  mod <- train(
    training,
    hyperparameters = setup_Ranger(quantreg = TRUE),
    verbosity = 0L
  )
  expect_error(
    conformal(
      mod,
      held_out,
      calibration = calibration,
      config = setup_CQR(),
      verbosity = 0L
    ),
    "over-covers by more than the width of its own band"
  )
})


test_that("CQR requires calibration data and a numeric outcome", {
  x <- .conformal_hetero(300L, seed = 12L)
  mod <- train(
    x[1:150, ],
    dat_test = x[151:250, ],
    hyperparameters = setup_Ranger(quantreg = TRUE),
    verbosity = 0L
  )
  # The stored split holds predictions, not the features a quantile query needs.
  expect_error(
    conformal(
      mod,
      .features(x[251:300, ]),
      config = setup_CQR(),
      verbosity = 0L
    ),
    "needs `calibration` data"
  )

  xc <- .conformal_clf(300L, seed = 12L)
  clf <- train(xc[1:150, ], hyperparameters = setup_Ranger(), verbosity = 0L)
  expect_error(
    conformal(
      clf,
      .features(xc[251:300, ]),
      calibration = xc[151:250, ],
      config = setup_CQR(),
      verbosity = 0L
    ),
    "this is a classification"
  )
})


# %% Reading a region ----

test_that("conformal_metrics scores coverage and size", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  held_out <- x[251:300, ]
  region <- conformal(mod, .features(held_out), verbosity = 0L)
  scored <- conformal_metrics(region, held_out[["y"]])

  expect_s3_class(scored, "data.frame")
  expect_identical(nrow(scored), 1L)
  expect_named(scored, c("coverage", "mean_width", "median_width", "n"))
  expect_identical(scored[["n"]], 50L)
  expect_equal(
    scored[["coverage"]],
    mean(held_out[["y"]] >= region@lower & held_out[["y"]] <= region@upper)
  )
})


test_that("conformal_metrics scores a set-valued region", {
  x <- .conformal_clf(500L, seed = 2026L)
  mod <- .split_model(x, 250L, 150L, setup_Ranger())
  held_out <- x[401:500, ]
  region <- conformal(mod, .features(held_out), verbosity = 0L)
  scored <- conformal_metrics(region, held_out[["y"]])

  expect_named(
    scored,
    c("coverage", "mean_set_size", "singleton_rate", "empty_rate", "n")
  )
  expect_gte(scored[["coverage"]], 0.8)
  expect_equal(scored[["mean_set_size"]], mean(region@set_size))
})


test_that("conformal_metrics rejects outcomes of the wrong length", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  region <- conformal(mod, .features(x[251:300, ]), verbosity = 0L)
  expect_error(
    conformal_metrics(region, x[["y"]][251:290]),
    "40 values for 50 bounded cases"
  )
})


test_that("a region prints its construction, its guarantee and its size", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  region <- conformal(mod, .features(x[251:300, ]), verbosity = 0L)
  printed <- repr(region, output_type = "plain")
  expect_match(printed, "split conformal")
  expect_match(printed, "50 cases")
  expect_match(printed, "90% guaranteed")
  expect_match(printed, "mean width")

  cv_mod <- train(
    x[1:200, ],
    outer_resampling_config = setup_Resampler(5L, "KFold", seed = 1L),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  cv_printed <- repr(
    conformal(cv_mod, .features(x[201:300, ]), verbosity = 0L),
    output_type = "plain"
  )
  # Both numbers, so neither the conservative bound nor the requested level can
  # be read as the other.
  expect_match(cv_printed, "80% guaranteed \\(90% nominal\\)")
})


test_that("a region publishes its provenance and summary, not its bounds", {
  x <- .conformal_reg(300L, seed = 2026L)
  mod <- .split_model(x, 150L, 100L, setup_GLM())
  region <- conformal(mod, .features(x[251:300, ]), verbosity = 0L)
  wire <- to_json(region)

  expect_identical(wire[["method"]], "Split")
  expect_identical(wire[["alpha"]], 0.1)
  expect_equal(wire[["coverage"]], 0.9)
  expect_identical(wire[["n_calibration"]], 100L)
  expect_true("mean_width" %in% names(wire))
  # Per-case bulk stays on the object, as it does for a `SHAP`.
  expect_false(any(c("lower", "upper", "predicted") %in% names(wire)))
})


# %% Class contracts ----

test_that("a region must name the construction that produced it", {
  expect_error(
    PredictionInterval(
      algorithm = "GLM",
      config = setup_SplitConformal(),
      score = "absolute",
      n_calibration = 10L,
      predicted = c(1, 2),
      lower = c(0, 1),
      upper = c(2, 3)
    ),
    "@method"
  )
})


test_that("an interval rejects bounds that do not describe its cases", {
  args <- list(
    algorithm = "GLM",
    config = setup_SplitConformal(),
    method = "Split",
    score = "absolute",
    n_calibration = 10L,
    predicted = c(1, 2),
    lower = c(0, 1),
    upper = c(2, 3)
  )
  expect_no_error(do.call(PredictionInterval, args))
  expect_error(
    do.call(PredictionInterval, .replace(args, lower = 0)),
    "one value per case"
  )
  expect_error(
    do.call(PredictionInterval, .replace(args, upper = c(0, 0))),
    "at least @lower"
  )
})


test_that("a set rejects labels outside its classes", {
  args <- list(
    algorithm = "Ranger",
    config = setup_SplitConformal(),
    method = "Split",
    score = "APS",
    n_calibration = 10L,
    sets = list(c("a"), character()),
    predicted_prob = matrix(c(0.7, 0.4, 0.3, 0.6), nrow = 2L),
    classes = c("a", "b")
  )
  expect_no_error(do.call(PredictionSet, args))
  expect_error(
    do.call(PredictionSet, .replace(args, sets = list("a", "z"))),
    "only hold labels named in @classes"
  )
  expect_error(
    do.call(PredictionSet, .replace(args, classes = c("a", "b", "c"))),
    "one column per class"
  )
})
