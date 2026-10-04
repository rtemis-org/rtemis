# test_SupervisedReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Interval helpers, each against an independent computation ----
test_that("review_binom_interval() is the Clopper-Pearson interval", {
  # The beta-quantile form of Clopper-Pearson, written out independently of
  # binom.test().
  x <- 17L
  n <- 25L
  alpha <- 0.05
  expected <- c(
    stats::qbeta(alpha / 2, x, n - x + 1L),
    stats::qbeta(1 - alpha / 2, x + 1L, n - x)
  )
  expect_equal(review_binom_interval(x, n, 0.95), expected)
  expect_true(all(is.na(review_binom_interval(0L, 0L, 0.95))))
})

test_that("review_balanced_accuracy_interval() matches the hand computation", {
  hits <- c(8L, 15L, 3L)
  totals <- c(10L, 20L, 5L)
  adjusted <- (hits + 1) / (totals + 2)
  se <- sqrt(sum(adjusted * (1 - adjusted) / totals)) / 3
  ba <- mean(hits / totals)
  expected <- ba + c(-1, 1) * stats::qnorm(0.975) * se
  expect_equal(review_balanced_accuracy_interval(hits, totals, 0.95), expected)
  # A class absent from the test set does not enter the mean.
  expect_equal(
    review_balanced_accuracy_interval(c(hits, 0L), c(totals, 0L), 0.95),
    expected
  )
  # Perfect recall keeps a nonzero width.
  perfect <- review_balanced_accuracy_interval(c(10L, 10L), c(10L, 10L), 0.95)
  expect_identical(perfect[[2L]], 1)
  expect_lt(perfect[[1L]], 1)
})

test_that("review_auc_delong() matches a pairwise-kernel DeLong computation", {
  set.seed(2026)
  positive <- rep(c(TRUE, FALSE), c(30L, 45L))
  prob <- round(stats::plogis(rnorm(75L) + positive), 2L)
  # Independent: the Mann-Whitney kernel over every pair, with ties at 1/2.
  pos <- prob[positive]
  neg <- prob[!positive]
  psi <- outer(pos, neg, function(a, b) (a > b) + 0.5 * (a == b))
  auc <- mean(psi)
  v10 <- rowMeans(psi)
  v01 <- colMeans(psi)
  se <- sqrt(stats::var(v10) / length(pos) + stats::var(v01) / length(neg))
  expected <- c(auc, auc + c(-1, 1) * stats::qnorm(0.975) * se)
  expect_equal(review_auc_delong(prob, positive, 0.95), expected)
  expect_true(all(is.na(review_auc_delong(
    prob[1:3],
    c(TRUE, FALSE, FALSE),
    0.95
  ))))
})

test_that("review_skill() is a paired t interval on the per-case reduction", {
  set.seed(2026)
  loss_baseline <- rexp(40L)
  loss <- loss_baseline * runif(40L, 0.3, 1.1)
  tt <- stats::t.test(loss_baseline, loss, paired = TRUE, conf.level = 0.9)
  scale <- mean(loss_baseline)
  expected <- c(
    1 - mean(loss) / scale,
    as.numeric(tt[["conf.int"]]) / scale
  )
  expect_equal(review_skill(loss, loss_baseline, 0.9), expected)
  expect_true(all(is.na(review_skill(loss, rep(0, 40L), 0.9))))
  # A skill cannot exceed 1: the upper bound is clipped.
  expect_lte(review_skill(rep(0, 3L), c(1, 10, 100), 0.95)[[3L]], 1)
})

test_that("review_baseline_finding() distinguishes better, worse and neither", {
  better <- review_baseline_finding(
    "BASELINE_AUC",
    c(0.6, 0.8),
    0.5,
    "AUC",
    "0.5",
    "a"
  )
  worse <- review_baseline_finding(
    "BASELINE_AUC",
    c(0.2, 0.4),
    0.5,
    "AUC",
    "0.5",
    "a"
  )
  neither <- review_baseline_finding(
    "BASELINE_AUC",
    c(0.4, 0.6),
    0.5,
    "AUC",
    "0.5",
    "a"
  )
  expect_identical(better@severity, "note")
  expect_match(better@message, "is better than", fixed = TRUE)
  expect_identical(worse@severity, "warning")
  expect_match(worse@message, "is worse than", fixed = TRUE)
  expect_identical(neither@severity, "warning")
  expect_match(neither@message, "cannot be distinguished", fixed = TRUE)
})


# %% Models ----
idx <- c(1:40, 51:90, 101:140)
mod_iris <- train(
  iris[idx, ],
  dat_test = iris[-idx, ],
  hyperparameters = setup_CART(),
  verbosity = 0L
)
rev_iris <- review(mod_iris)

test_that("review() returns a SupervisedReview whose findings cite existing rows", {
  expect_s7_class(rev_iris, SupervisedReview)
  expect_identical(rev_iris@type, "Classification")
  expect_identical(rev_iris@description, desc(mod_iris))
  keys <- rev_iris@basis[["key"]]
  expect_false(anyDuplicated(keys) > 0L)
  for (f in rev_iris@findings) {
    expect_true(all(f@basis %in% keys), info = f@code)
    expect_identical(f@plain, unname(REVIEW_PLAIN[[f@code]]))
  }
  expect_identical(rev_iris@limitations, REVIEW_LIMITATIONS)
})

test_that("the basis states every training and test metric and the baseline", {
  basis <- rev_iris@basis
  value <- function(key) basis[["value"]][basis[["key"]] == key]
  overall_training <- mod_iris@metrics_training[["overall"]]
  overall_test <- mod_iris@metrics_test[["overall"]]
  for (metric in names(overall_training)) {
    expect_equal(
      value(paste0("performance.", metric, ".training")),
      overall_training[[metric]]
    )
    expect_equal(
      value(paste0("performance.", metric, ".test")),
      overall_test[[metric]]
    )
    expect_equal(
      value(paste0("performance.", metric, ".difference")),
      overall_training[[metric]] - overall_test[[metric]]
    )
  }
  expect_equal(value("sample.n_training"), 120)
  expect_equal(value("sample.n_test"), 30)
  expect_equal(value("sample.n_predictors"), 4)
  # The training majority is the first level on a tie; its share of the test
  # set is the baseline accuracy.
  expect_equal(value("baseline.accuracy"), 1 / 3)
  expect_equal(value("baseline.balanced_accuracy"), 1 / 3)
  expect_equal(value("setting.confidence_level"), 0.95)
})

test_that("a model clearly better than its baseline gets notes, not warnings", {
  codes <- review_codes(rev_iris)
  expect_identical(
    codes,
    c(
      "SINGLE_SPLIT",
      "TEST_PRECISION",
      "BASELINE_ACCURACY",
      "BASELINE_BALANCED_ACCURACY"
    )
  )
  severities <- vapply(rev_iris@findings, function(f) f@severity, character(1L))
  expect_true(all(severities == "note"))
})

test_that("min_cases_per_predictor decides FEW_CASES_PER_PREDICTOR", {
  # 40 cases in the smallest class over 4 predictors is exactly 10.
  expect_false("FEW_CASES_PER_PREDICTOR" %in% review_codes(rev_iris))
  stricter <- review(mod_iris, min_cases_per_predictor = 10.5)
  expect_true(all(
    c("FEW_CASES_PER_PREDICTOR", "PRESELECTION_RISK") %in%
      review_codes(stricter)
  ))
  expect_equal(
    stricter@basis[["value"]][
      stricter@basis[["key"]] == "setting.min_cases_per_predictor"
    ],
    10.5
  )
})

test_that("review() validates its settings", {
  expect_error(
    review(mod_iris, confidence_level = 1),
    class = "rtemis_input_error"
  )
  expect_error(
    review(mod_iris, min_cases_per_predictor = 0),
    class = "rtemis_input_error"
  )
})

test_that("constant predictions and never-predicted classes are found", {
  constant <- mod_iris
  constant@predicted_test <- factor(
    rep("setosa", 30L),
    levels = levels(iris[["Species"]])
  )
  codes <- review_codes(review(constant))
  expect_true("CONSTANT_PREDICTIONS" %in% codes)
  # One finding for one cause: a constant prediction misses every other class.
  expect_false("CLASS_NEVER_PREDICTED" %in% codes)

  missing_class <- mod_iris
  missing_class@predicted_test <- factor(
    rep(c("setosa", "versicolor"), each = 15L),
    levels = levels(iris[["Species"]])
  )
  out <- review(missing_class)
  expect_false("CONSTANT_PREDICTIONS" %in% review_codes(out))
  never <- Filter(function(f) f@code == "CLASS_NEVER_PREDICTED", out@findings)
  expect_length(never, 1L)
  expect_match(never[[1L]]@message, "'virginica'", fixed = TRUE)
  expect_identical(never[[1L]]@basis, "sample.class_3.test")
})

test_that("a model without a test set is reported as unassessed", {
  mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  out <- review(mod)
  expect_identical(review_codes(out), "NO_TEST_SET")
  expect_false(any(out@basis[["section"]] == "baseline"))
  expect_false(any(grepl("\\.test$", out@basis[["key"]])))
})

test_that("an overfit model on pure noise is flagged and does not beat baseline", {
  set.seed(2026)
  n <- 300L
  noise <- data.frame(matrix(rnorm(n * 5L), n, 5L))
  noise[["y"]] <- factor(sample(c("a", "b"), n, replace = TRUE))
  mod <- train(
    noise[1:200, ],
    dat_test = noise[201:300, ],
    hyperparameters = setup_CART(cp = 0, minsplit = 2L, minbucket = 1L),
    verbosity = 0L
  )
  out <- review(mod)
  codes <- review_codes(out)
  expect_true("GENERALIZATION_GAP" %in% codes)
  gap <- Filter(function(f) f@code == "GENERALIZATION_GAP", out@findings)[[1L]]
  expect_identical(gap@severity, "warning")
  # Untuned, so the suggestion is to tune.
  expect_match(gap@suggestion, "^Tune")
  ba <- Filter(
    function(f) f@code == "BASELINE_BALANCED_ACCURACY",
    out@findings
  )[[1L]]
  expect_identical(ba@severity, "warning")
})

test_that("p > n is reported, replaces the gap finding, and follows the trait", {
  testthat::skip_if_not_installed("glmnet")
  set.seed(2026)
  n <- 60L
  p <- 80L
  wide <- data.frame(matrix(rnorm(n * p), n, p))
  wide[["y"]] <- factor(rep(c("a", "b"), length.out = n))
  mod <- train(
    wide[1:40, ],
    dat_test = wide[41:60, ],
    hyperparameters = setup_GLMNET(alpha = 1, lambda = 0.05),
    verbosity = 0L
  )
  out <- review(mod)
  codes <- review_codes(out)
  expect_true(all(c("DIM_P_GT_N", "PRESELECTION_RISK") %in% codes))
  expect_false("FEW_CASES_PER_PREDICTOR" %in% codes)
  expect_false("GENERALIZATION_GAP" %in% codes)
  dim <- Filter(function(f) f@code == "DIM_P_GT_N", out@findings)[[1L]]
  expected <- if (isFALSE(algorithm_handles_p_gt_n(mod@algorithm))) {
    "warning"
  } else {
    "note"
  }
  expect_identical(dim@severity, expected)
  # Sample and dimensionality findings come before anything else.
  first_other <- min(which(
    !codes %in%
      c(
        "SINGLE_SPLIT",
        "TEST_PRECISION",
        "DIM_P_GT_N",
        "FEW_CASES_PER_PREDICTOR",
        "PRESELECTION_RISK"
      )
  ))
  expect_gt(
    first_other,
    max(which(codes %in% c("DIM_P_GT_N", "PRESELECTION_RISK")))
  )
})

test_that("binary probability findings agree with the model's own metrics", {
  d <- iris[51:150, ]
  d[["Species"]] <- factor(d[["Species"]])
  set.seed(2026)
  d[["noise"]] <- rnorm(100L)
  d <- d[, c("Sepal.Length", "noise", "Species")]
  i2 <- c(1:35, 51:85)
  mod <- train(
    d[i2, ],
    dat_test = d[-i2, ],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  out <- review(mod)
  value <- function(key) out@basis[["value"]][out@basis[["key"]] == key]
  overall_test <- mod@metrics_test[["overall"]]
  positive_level <- levels(d[["Species"]])[[mod@binclasspos]]
  prob <- positive_prob(mod@predicted_prob_test)
  auc <- review_auc_delong(prob, mod@y_test == positive_level, 0.95)
  expect_equal(auc[[1L]], overall_test[["auc"]])
  expect_equal(value("performance.auc.test_lower"), auc[[2L]])
  prevalence <- mean(mod@y_training == positive_level)
  y01 <- as.numeric(mod@y_test == positive_level)
  expect_equal(value("baseline.brier_score"), mean((y01 - prevalence)^2))
  expect_equal(
    value("baseline.brier_skill"),
    1 - overall_test[["brier_score"]] / mean((y01 - prevalence)^2)
  )
  expect_true(all(c("BASELINE_AUC", "BASELINE_BRIER") %in% review_codes(out)))
})

test_that("a regression review states the training-mean baseline", {
  set.seed(2026)
  n <- 150L
  x1 <- rnorm(n)
  reg <- data.frame(x1 = x1, x2 = rnorm(n), y = 2 * x1 + rnorm(n))
  mod <- train(
    reg[1:100, ],
    dat_test = reg[101:150, ],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  out <- review(mod)
  value <- function(key) out@basis[["value"]][out@basis[["key"]] == key]
  training_mean <- mean(reg[["y"]][1:100])
  y_test <- reg[["y"]][101:150]
  expect_equal(value("baseline.mse"), mean((y_test - training_mean)^2))
  expect_lte(value("baseline.rsq"), 0)
  expect_equal(value("performance.mse.test"), mod@metrics_test@metrics[["mse"]])
  mse <- Filter(function(f) f@code == "BASELINE_MSE", out@findings)[[1L]]
  expect_identical(mse@severity, "note")
  expect_false("GENERALIZATION_GAP" %in% review_codes(out))
})


# %% Class contract ----
test_that("a review cannot cite missing basis rows or repeat a key", {
  basis <- data.frame(key = "a", section = "sample", label = "A", value = 1)
  finding <- new_review_finding("SINGLE_SPLIT", "note", "m", basis = "b")
  expect_error(
    new_supervised_review(
      algorithm = "CART",
      type = "Classification",
      description = "d",
      basis = basis,
      findings = list(finding)
    ),
    "do not exist: b",
    class = "rtemis_schema_error"
  )
  expect_error(
    new_supervised_review(
      algorithm = "CART",
      type = "Classification",
      description = "d",
      basis = rbind(basis, basis),
      findings = list()
    ),
    "duplicated: a",
    class = "rtemis_schema_error"
  )
  expect_error(new_review_finding("NOT_A_CODE", "note", "m", basis = "a"))
})

test_that("every review code carries its plain text", {
  expect_setequal(names(REVIEW_PLAIN), REVIEW_CODES)
})

test_that("repr states the basis before the findings", {
  out <- repr(rev_iris, output_type = "plain")
  expect_lt(regexpr("Basis", out), regexpr("Findings", out))
  expect_lt(regexpr("Findings", out), regexpr("Limitations", out))
  expect_match(out, "SINGLE_SPLIT", fixed = TRUE)
})

test_that("a review record validates against its published schema", {
  testthat::skip_if_not_installed("jsonvalidate")
  validator <- function(cls) {
    schema <- S7_to_JSONSchema(
      cls,
      id = paste0("https://schema.rtemis.org/test/", tolower(cls@name), ".json")
    )
    schema[["$id"]] <- NULL
    schema
  }
  finding_schema <- validator(ReviewFinding)
  finding_schema[["properties"]][["$schema"]] <- NULL
  review_schema <- validator(SupervisedReview)
  # Inline the real finding schema wherever the review references it, so the
  # findings are checked against their own contract rather than a stand-in.
  inline <- function(node) {
    if (!is.list(node)) {
      return(node)
    }
    ref <- node[["$ref"]]
    if (is.character(ref) && length(ref) == 1L && startsWith(ref, "https://")) {
      expect_match(ref, "reviewfinding", fixed = TRUE)
      return(finding_schema)
    }
    lapply(node, inline)
  }
  review_schema <- inline(review_schema)
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(
      review_schema,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    engine = "ajv"
  )
  doc <- record_object(rev_iris)
  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", digits = NA)
  expect_true(validate(json, verbose = TRUE))
  # Negative case: a severity outside the vocabulary is rejected.
  doc[["findings"]][[1L]][["severity"]] <- "fatal"
  bad <- jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", digits = NA)
  expect_false(validate(bad))
})


# %% Resampled models ----
.binary <- iris[51:150, ]
.binary[["Species"]] <- factor(.binary[["Species"]])
set.seed(2026)
.binary[["noise"]] <- rnorm(100L)
.binary <- .binary[, c("Sepal.Length", "Sepal.Width", "noise", "Species")]
mod_res <- train(
  .binary,
  hyperparameters = setup_GLM(),
  outer_resampling_config = setup_KFold(5L),
  verbosity = 0L
)
rev_res <- review(mod_res)
.value <- function(review, key) {
  review@basis[["value"]][review@basis[["key"]] == key]
}

test_that("a resampled review states fold means, SDs and the headline per fold", {
  expect_s7_class(rev_res, SupervisedReview)
  for (metric in names(mod_res@metrics_training@mean_metrics)) {
    expect_equal(
      .value(rev_res, paste0("performance.", metric, ".training")),
      mod_res@metrics_training@mean_metrics[[metric]]
    )
    expect_equal(
      .value(rev_res, paste0("performance.", metric, ".test")),
      mod_res@metrics_test@mean_metrics[[metric]]
    )
    expect_equal(
      .value(rev_res, paste0("performance.", metric, ".test_sd")),
      mod_res@metrics_test@sd_metrics[[metric]]
    )
  }
  for (j in seq_along(mod_res@models)) {
    expect_equal(
      .value(
        rev_res,
        paste0("performance.balanced_accuracy.resample_", j, ".test")
      ),
      mod_res@models[[j]]@metrics_test[["overall"]][["balanced_accuracy"]]
    )
  }
  expect_equal(.value(rev_res, "sample.n_resamples"), 5)
  expect_equal(.value(rev_res, "sample.n_test"), 100)
  expect_equal(.value(rev_res, "sample.n_test_cases"), 100)
})

test_that("pooled out-of-sample estimates are computed from the fold predictions", {
  y <- do.call(c, lapply(mod_res@models, function(m) m@y_test))
  predicted <- do.call(c, lapply(mod_res@models, function(m) m@predicted_test))
  prob <- unlist(lapply(mod_res@models, function(m) {
    positive_prob(m@predicted_prob_test)
  }))
  expect_equal(
    .value(rev_res, "performance.accuracy.pooled"),
    mean(y == predicted)
  )
  positive_level <- levels(y)[[mod_res@models[[1L]]@binclasspos]]
  pos <- prob[y == positive_level]
  neg <- prob[y != positive_level]
  auc <- mean(outer(pos, neg, function(a, b) (a > b) + 0.5 * (a == b)))
  expect_equal(.value(rev_res, "performance.auc.pooled"), auc)
  # Each resample's baseline comes from its own training cases.
  baseline <- unlist(lapply(mod_res@models, function(m) {
    majority <- names(which.max(table(m@y_training)))
    rep(majority, length(m@y_test))
  }))
  expect_equal(
    .value(rev_res, "baseline.accuracy"),
    mean(as.character(y) == baseline)
  )
  better <- sum(vapply(
    mod_res@models,
    function(m) {
      m@metrics_test[["overall"]][["balanced_accuracy"]] > 0.5
    },
    logical(1L)
  ))
  expect_equal(.value(rev_res, "baseline.resamples_better"), better)
})

test_that("a k-fold review reports variation and pooled precision, not a single split", {
  codes <- review_codes(rev_res)
  expect_true(all(
    c("TEST_PRECISION", "FOLD_VARIATION", "BASELINE_BALANCED_ACCURACY") %in%
      codes
  ))
  expect_false(any(
    c("SINGLE_SPLIT", "NO_TEST_SET", "OVERLAPPING_TEST_SETS") %in% codes
  ))
})

test_that("overlapping test sets get no pooled intervals or tested comparisons", {
  set.seed(2026)
  n <- 60L
  x1 <- rnorm(n)
  reg <- data.frame(x1 = x1, x2 = rnorm(n), y = 2 * x1 + rnorm(n))
  mod <- train(
    reg,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_Bootstrap(4L),
    verbosity = 0L
  )
  out <- review(mod)
  codes <- review_codes(out)
  expect_true(all(c("OVERLAPPING_TEST_SETS", "FOLD_VARIATION") %in% codes))
  expect_false(any(
    c("TEST_PRECISION", "BASELINE_MSE", "GENERALIZATION_GAP") %in% codes
  ))
  expect_false(any(grepl(
    "\\.pooled$|\\.test_lower$|_skill",
    out@basis[["key"]]
  )))
  expect_lt(.value(out, "sample.n_test_cases"), .value(out, "sample.n_test"))
  # A bootstrap resample's repeated draws count as one case each.
  unique_cases <- vapply(
    mod@outer_resampler@resamples[mod@resample_ids],
    function(idx) length(unique(idx)),
    integer(1L)
  )
  expect_equal(.value(out, "sample.n_training"), min(unique_cases))
})

test_that("a resampled regression baseline uses each resample's training mean", {
  set.seed(2026)
  n <- 120L
  x1 <- rnorm(n)
  reg <- data.frame(x1 = x1, x2 = rnorm(n), y = 3 + 2 * x1 + rnorm(n))
  mod <- train(
    reg,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_KFold(4L),
    verbosity = 0L
  )
  out <- review(mod)
  y <- unlist(lapply(mod@models, function(m) m@y_test))
  baseline <- unlist(lapply(mod@models, function(m) {
    rep(mean(m@y_training), length(m@y_test))
  }))
  expect_equal(.value(out, "baseline.mse"), mean((y - baseline)^2))
  mse <- Filter(function(f) f@code == "BASELINE_MSE", out@findings)[[1L]]
  expect_identical(mse@severity, "note")
})

test_that("a tuned value at an extendable grid edge is reported", {
  mod <- train(
    iris,
    hyperparameters = setup_CART(
      maxdepth = tune_over(1L, 2L, 3L),
      cp = tune_over(0.001, 0.01)
    ),
    outer_resampling_config = setup_KFold(3L),
    verbosity = 0L
  )
  out <- review(mod)
  selected <- vapply(
    mod@models,
    function(m) m@tuner@best_hyperparameters[["maxdepth"]],
    numeric(1L)
  )
  # 1 is maxdepth's declared minimum, so only the upper edge can be extended.
  expect_equal(.value(out, "tuning.maxdepth.at_edge"), sum(selected == 3))
  expect_identical(
    "TUNING_GRID_EDGE" %in% review_codes(out),
    any(selected == 3)
  )
  # Two values searched: every choice is an edge, so cp is not checked.
  expect_false(any(startsWith(out@basis[["key"]], "tuning.cp.")))
})

test_that("a single-split tuned model states its selected value", {
  idx <- c(1:40, 51:90, 101:140)
  mod <- train(
    iris[idx, ],
    dat_test = iris[-idx, ],
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L, 4L)),
    verbosity = 0L
  )
  out <- review(mod)
  expect_equal(
    .value(out, "tuning.maxdepth.selected"),
    mod@tuner@best_hyperparameters[["maxdepth"]]
  )
})
