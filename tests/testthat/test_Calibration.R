# test_Calibration.R
# ::rtemis::
# EDG rtemis.org

# Key
# {Algorithm}[method]<Class> Further conditions

# Setup ----
# library(rtemis)
# library(testthat)
library(data.table)

# Data ----
## Regression Data ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.table(x, g, y)
resr <- resample(datr)
datr_train <- datr[resr$Fold_1, ]
datr_test <- datr[-resr$Fold_1, ]

## Classification Data ----
### Binary ----
datc2 <- data.frame(
  gn = factor(sample(c("alpha", "beta", "gamma"), 100, replace = TRUE)),
  iris[51:150, ]
)
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Fold_1, ]
datc2_test <- datc2[-resc2$Fold_1, ]

### 3-class ----
datc3 <- iris
resc3 <- resample(datc3)
datc3_train <- datc3[resc3$Fold_1, ]
datc3_test <- datc3[-resc3$Fold_1, ]

### Miscalibrated binary scores ----
# A well-ranked but badly scaled score: cubing a calibrated probability leaves
# the ordering untouched and the magnitudes wrong, which is exactly the input
# calibration exists for. Ranking is preserved, so AUC of the raw score is the
# ceiling any monotonic calibrator can reach and the floor it must not fall
# below.
miscalibrated_scores <- function(n = 600L, seed = 2026L) {
  set.seed(seed)
  x <- rnorm(n)
  p_true <- plogis(1.5 * x)
  y <- rbinom(n, 1L, p_true)
  list(
    score = p_true^3,
    labels = factor(
      ifelse(y == 1L, "pos", "neg"),
      levels = c("neg", "pos")
    ),
    y = y
  )
}

auc_of <- function(score, y) {
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  r <- rank(score)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

brier_of <- function(p, y) mean((p - y)^2)

log_loss_of <- function(p, y) {
  pc <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(pc) + (1 - y) * log(1 - pc))
}

# Fit a calibrator on (score, labels) and return the calibrated probabilities.
calibrated_probs <- function(hyperparameters, dat, newscore = NULL) {
  mod <- train(
    data.table(
      predicted_probabilities = dat[["score"]],
      true_labels = dat[["labels"]]
    ),
    hyperparameters = hyperparameters,
    verbosity = 0L
  )
  newscore <- if (is.null(newscore)) dat[["score"]] else newscore
  as.numeric(rtemis:::positive_prob(predict(
    mod,
    data.frame(predicted_probabilities = newscore)
  )))
}


# MonotonicHAL calibration properties ----
## Monotonicity ----
test_that("MonotonicHAL calibration is monotonic on a dense grid", {
  skip_if_not_installed("hal9001")
  # The invariant the algorithm exists for, tested directly rather than
  # through a proxy. The grid extends past the observed score range on both
  # sides, so extrapolation beyond the training support is covered too: the
  # basis functions are constant out there, and a constant tail is still
  # non-decreasing. Both smoothness orders are checked, since they build
  # different bases -- indicators at 0, hinges at 1 -- and only the
  # non-negative coefficients make either of them monotonic.
  dat <- miscalibrated_scores()
  lo <- min(dat[["score"]])
  hi <- max(dat[["score"]])
  pad <- 0.05 * (hi - lo)
  grid <- seq(lo - pad, hi + pad, length.out = 1000L)
  for (order in c(0L, 1L)) {
    calibrated <- calibrated_probs(
      setup_MonotonicHAL(smoothness_orders = order, seed = 2026L),
      dat,
      grid
    )
    expect_true(all(diff(calibrated) >= -1e-10))
  }
})

test_that("MonotonicHAL calibration inverts no ranks on observed scores", {
  skip_if_not_installed("hal9001")
  # Same guarantee read on the actual calibration scores rather than a grid.
  # Ties are expected -- at smoothness_orders = 0 the map is a step function
  # and flat stretches are the point -- so only strict decreases fail.
  dat <- miscalibrated_scores()
  for (order in c(0L, 1L)) {
    calibrated <- calibrated_probs(
      setup_MonotonicHAL(smoothness_orders = order, seed = 2026L),
      dat
    )
    ordered <- calibrated[order(dat[["score"]])]
    expect_true(all(diff(ordered) >= -1e-10))
  }
})

test_that("MonotonicHAL calibration does not degrade AUC", {
  skip_if_not_installed("hal9001")
  # A monotonic map cannot reorder, but a piecewise-constant one merges
  # distinct scores into ties, and ties legitimately cost trapezoidal AUC.
  # The assertion is therefore one-sided: calibration may raise AUC, and must
  # not lower it by more than tie-breaking accounts for. A large drop would
  # mean the fit had flattened away real signal.
  dat <- miscalibrated_scores()
  baseline <- auc_of(dat[["score"]], dat[["y"]])
  for (order in c(0L, 1L)) {
    calibrated <- calibrated_probs(
      setup_MonotonicHAL(smoothness_orders = order, seed = 2026L),
      dat
    )
    expect_gte(auc_of(calibrated, dat[["y"]]), baseline - 0.01)
  }
  # At the default order the map is strictly increasing, so ranks survive
  # intact and AUC is carried through exactly.
  default_cal <- calibrated_probs(setup_MonotonicHAL(seed = 2026L), dat)
  expect_equal(auc_of(default_cal, dat[["y"]]), baseline, tolerance = 1e-8)
})

## Calibration improves ----
test_that("MonotonicHAL calibration improves Brier score and log loss", {
  skip_if_not_installed("hal9001")
  dat <- miscalibrated_scores()
  calibrated <- calibrated_probs(setup_MonotonicHAL(seed = 2026L), dat)
  expect_lt(
    brier_of(calibrated, dat[["y"]]),
    brier_of(dat[["score"]], dat[["y"]])
  )
  expect_lt(
    log_loss_of(calibrated, dat[["y"]]),
    log_loss_of(dat[["score"]], dat[["y"]])
  )
})

## No boundary degeneracy ----
test_that("Neither calibrator returns an exact 0 or 1", {
  skip_if_not_installed("hal9001")
  # A calibrated probability of exactly 0 or 1 asserts certainty and makes log
  # loss infinite for a single case there whose label disagrees. MonotonicHAL
  # fits on the logit scale and cannot reach either endpoint; Isotonic would
  # otherwise fit a uniformly-labelled block at exactly 0 or 1 and is bounded
  # away from both. This is a regression test for that failure mode.
  dat <- miscalibrated_scores()
  for (hp in list(setup_MonotonicHAL(seed = 2026L), setup_Isotonic())) {
    calibrated <- calibrated_probs(hp, dat)
    expect_true(all(calibrated > 0))
    expect_true(all(calibrated < 1))
  }
})

test_that("Isotonic bounds probabilities by the calibration set size", {
  # The bound is 1 / (2 * n): half a case, the finest distinction n cases can
  # support. A perfectly separated input is the case that reaches it, since
  # every block is uniformly labelled.
  n <- 60L
  score <- seq(0.01, 0.99, length.out = n)
  labels <- factor(
    rep(c("neg", "pos"), each = n / 2L),
    levels = c("neg", "pos")
  )
  calibrated <- calibrated_probs(
    setup_Isotonic(),
    list(score = score, labels = labels)
  )
  eps <- 1 / (2 * n)
  expect_gte(min(calibrated), eps)
  expect_lte(max(calibrated), 1 - eps)
  # Bounding moves only the saturated blocks, so the map stays non-decreasing.
  expect_true(all(diff(calibrated) >= -1e-10))
})

test_that("Isotonic regression fits are not bounded", {
  # The bound is a statement about probabilities, so a regression outcome --
  # on its own scale -- must pass through untouched.
  set.seed(2026)
  x <- seq(0, 1, length.out = 100L)
  mod <- train(
    data.table(x = x, y = 5 * x + rnorm(100L, sd = 0.1)),
    hyperparameters = setup_Isotonic(),
    verbosity = 0L
  )
  predicted <- as.numeric(predict(mod, data.frame(x = x)))
  expect_gt(max(predicted), 1)
})

## Isotonic equivalence ----
test_that("Unpenalized MonotonicHAL at order 0 approximates Isotonic", {
  skip_if_not_installed("hal9001")
  # hal9001 documents that a monotonic HAL with smoothness_orders = 0 and the
  # lasso penalty removed is the NPMLE over the monotonic class, which is what
  # isotonic regression computes. The two agree in the interior but cannot
  # agree at the boundary: isotonic reaches exactly 0 and 1, and a logit-scale
  # fit never does. The tolerance is therefore loose and on the mean.
  dat <- miscalibrated_scores()
  hal0 <- calibrated_probs(
    setup_MonotonicHAL(smoothness_orders = 0L, penalized = FALSE, seed = 2026L),
    dat
  )
  isotonic <- calibrated_probs(setup_Isotonic(), dat)
  expect_gt(cor(hal0, isotonic), 0.95)
  expect_lt(mean(abs(hal0 - isotonic)), 0.05)
})

## Degenerate inputs ----
test_that("MonotonicHAL calibration survives degenerate inputs", {
  skip_if_not_installed("hal9001")
  # None of these may hang or return NaN. They are the inputs a calibration
  # set can genuinely take when a resample is small or a fold is unlucky.
  fit_ok <- function(score, labels) {
    p <- tryCatch(
      calibrated_probs(
        setup_MonotonicHAL(seed = 2026L),
        list(score = score, labels = labels)
      ),
      error = function(e) NULL
    )
    # A refusal to fit is acceptable; silently returning NaN is not.
    is.null(p) || all(is.finite(p))
  }
  lv <- c("neg", "pos")

  # Tiny n.
  expect_true(fit_ok(
    c(0.1, 0.4, 0.6, 0.9),
    factor(c("neg", "neg", "pos", "pos"), levels = lv)
  ))

  # Perfectly separated scores.
  expect_true(fit_ok(
    c(0.01, 0.02, 0.03, 0.97, 0.98, 0.99),
    factor(c("neg", "neg", "neg", "pos", "pos", "pos"), levels = lv)
  ))

  # A calibration set containing a single class.
  set.seed(2026)
  expect_true(fit_ok(
    runif(40L),
    factor(rep("neg", 40L), levels = lv)
  ))
})


# calibrate() calibrator selection ----
## Default ----
test_that("calibrate() defaults to Isotonic on Classification", {
  mod <- train(
    datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  cal <- calibrate(
    mod,
    predicted_probabilities = mod@predicted_prob_training,
    true_labels = mod@y_training,
    verbosity = 0L
  )
  expect_s7_class(cal, CalibratedClassification)
  expect_identical(cal@calibrator, "Isotonic")
  # The calibrator that ran is serialized, so a run is reproducible from its
  # output alone.
  expect_identical(to_json(cal)[["calibrator"]], "Isotonic")
})

test_that("calibrate() defaults to Isotonic on ClassificationRes", {
  resmod <- train(
    datc2,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    verbosity = 0L
  )
  cal <- calibrate(
    resmod,
    resampler_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    verbosity = 0L
  )
  expect_s7_class(cal, CalibratedClassificationRes)
  expect_identical(cal@calibrator, "Isotonic")
  expect_identical(to_json(cal)[["calibrator"]], "Isotonic")
})

## Explicit requests are honored ----
test_that("calibrate() honors an explicit MonotonicHAL request", {
  skip_if_not_installed("hal9001")
  # No substitution in either direction: the caller named an algorithm and the
  # object must report the one that actually ran.
  mod <- train(
    datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  cal <- calibrate(
    mod,
    predicted_probabilities = mod@predicted_prob_training,
    true_labels = mod@y_training,
    hyperparameters = setup_MonotonicHAL(seed = 2026L),
    verbosity = 0L
  )
  expect_identical(cal@calibrator, "MonotonicHAL")
  expect_identical(to_json(cal)[["calibrator"]], "MonotonicHAL")
})

test_that("calibrate() propagates a calibration fit failure", {
  skip_if_not_installed("hal9001")
  # Calibration does not silently substitute a different algorithm when the
  # requested one fails.
  mod <- train(
    datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  local_mocked_bindings(
    hal_fit = function(...) stop("synthetic backend failure")
  )
  expect_error(
    calibrate(
      mod,
      predicted_probabilities = mod@predicted_prob_training,
      true_labels = mod@y_training,
      hyperparameters = setup_MonotonicHAL(),
      verbosity = 0L
    ),
    "synthetic backend failure"
  )
})

test_that("calibrate() honors an explicit Isotonic request", {
  mod <- train(
    datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  cal <- calibrate(
    mod,
    predicted_probabilities = mod@predicted_prob_training,
    true_labels = mod@y_training,
    hyperparameters = setup_Isotonic(),
    verbosity = 0L
  )
  expect_identical(cal@calibrator, "Isotonic")
})

## IFW guard ----
test_that("calibrate() rejects IFW on ClassificationRes", {
  skip_if_not_installed("hal9001")
  resmod <- train(
    datc2,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    verbosity = 0L
  )
  expect_error(
    calibrate(
      resmod,
      hyperparameters = setup_MonotonicHAL(ifw = TRUE),
      verbosity = 0L
    ),
    "IFW"
  )
})


# available_calibration() ----
test_that("available_calibration() lists the monotonic calibrators", {
  algs <- available_calibration(verbosity = 0L)
  # The other available_* functions return a named character vector carrying
  # class "list" so `printls()` renders it; match that, not typeof().
  expect_identical(class(algs), "list")
  expect_named(algs, c("Isotonic", "MonotonicHAL"))
  # Descriptions are read from the supervised table, not restated, so they
  # cannot drift from what available_supervised() prints.
  supervised <- available_supervised(verbosity = 0L)
  expect_identical(algs[["Isotonic"]], supervised[["Isotonic"]])
  expect_identical(algs[["MonotonicHAL"]], supervised[["MonotonicHAL"]])
})

test_that("every calibration algorithm is a registered supervised algorithm", {
  # A name here that no setup_ function backs would abort inside calibrate().
  for (alg in names(available_calibration(verbosity = 0L))) {
    expect_s7_class(
      do.call(paste0("setup_", alg), list()),
      Hyperparameters
    )
  }
})
