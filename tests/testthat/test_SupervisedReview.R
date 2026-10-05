# test_SupervisedReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Statistical helpers, each against an independent computation ----
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
  # Balanced accuracy averages over every class: one absent from the test set
  # leaves it undefined rather than redefined over the classes present.
  expect_true(all(is.na(
    review_balanced_accuracy_interval(c(hits, 0L), c(totals, 0L), 0.95)
  )))
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

test_that("review_auc_delong() gives no interval at complete separation", {
  separated <- c(TRUE, TRUE, FALSE, FALSE)
  perfect <- review_auc_delong(c(0.9, 0.8, 0.2, 0.1), separated, 0.95)
  reversed <- review_auc_delong(c(0.1, 0.2, 0.8, 0.9), separated, 0.95)
  expect_identical(perfect[[1L]], 1)
  expect_identical(reversed[[1L]], 0)
  expect_true(all(is.na(perfect[2:3])))
  expect_true(all(is.na(reversed[2:3])))
})

test_that("review_mean_interval() is undefined without spread", {
  expect_true(all(is.na(review_mean_interval(c(2, 2, 2), 0.95))))
  expect_true(all(is.na(review_mean_interval(1, 0.95))))
  v <- c(1, 4, 2, 8)
  expect_equal(
    review_mean_interval(v, 0.9),
    as.numeric(stats::t.test(v, conf.level = 0.9)[["conf.int"]])
  )
})

test_that("review_loss_difference() is the paired t interval of the loss reduction", {
  set.seed(2026)
  loss_baseline <- rexp(40L)
  loss <- loss_baseline * runif(40L, 0.3, 1.1)
  tt <- stats::t.test(loss_baseline, loss, paired = TRUE, conf.level = 0.9)
  expect_equal(
    review_loss_difference(loss, loss_baseline, 0.9),
    c(mean(loss_baseline - loss), as.numeric(tt[["conf.int"]]))
  )
  # Proportional losses: the skill ratio is exactly 0.5 for every case, and
  # the interval is of the mean reduction, on the loss scale -- not of the
  # ratio.
  difference <- review_loss_difference(c(0.5, 5, 50), c(1, 10, 100), 0.95)
  expect_equal(difference[[1L]], mean(c(0.5, 5, 50)))
  expect_lt(difference[[2L]], 0)
})

test_that("review_mcnemar() pairs the two predictors' results case by case", {
  # 100 cases: the baseline is right on 50, the model on 56, and the model
  # corrects six baseline errors without introducing any.
  baseline_correct <- rep(c(TRUE, FALSE), each = 50L)
  model_correct <- baseline_correct
  model_correct[51:56] <- TRUE
  test <- review_mcnemar(model_correct, baseline_correct)
  expect_identical(test[["b"]], 6L)
  expect_identical(test[["c"]], 0L)
  # Exact: P(6 of 6 discordant pairs favor the model) = 2^-6, doubled.
  expect_equal(test[["p_value"]], 2 * 0.5^6)
  # The unpaired one-sample binomial test against 0.5 misses the pairing.
  unpaired <- stats::binom.test(56L, 100L, p = 0.5)[["p.value"]]
  expect_gt(unpaired, 0.05)
  expect_lt(test[["p_value"]], 0.05)
  # No discordant cases: no evidence of a difference.
  expect_identical(
    review_mcnemar(baseline_correct, baseline_correct)[["p_value"]],
    1
  )
})

test_that("review_interval_outcome() distinguishes better, worse and neither", {
  expect_identical(review_interval_outcome(c(0.6, 0.8), 0.5), "better")
  expect_identical(review_interval_outcome(c(0.2, 0.4), 0.5), "worse")
  expect_identical(
    review_interval_outcome(c(0.4, 0.6), 0.5),
    "indistinguishable"
  )
  better <- review_baseline_finding("BASELINE_AUC", "better", "AUC", "0.5")
  worse <- review_baseline_finding("BASELINE_AUC", "worse", "AUC", "0.5")
  neither <- review_baseline_finding(
    "BASELINE_AUC",
    "indistinguishable",
    "AUC",
    "0.5"
  )
  expect_identical(better@severity, "note")
  expect_match(better@message, "is better than", fixed = TRUE)
  expect_identical(worse@severity, "warning")
  expect_match(worse@message, "is worse than", fixed = TRUE)
  expect_match(
    neither@message,
    "does not provide clear evidence of a difference",
    fixed = TRUE
  )
})


# %% helpers ----
.cell <- function(table, key_column, key, column) {
  table[[column]][table[[key_column]] == key]
}
.perf <- function(review, metric, column) {
  .cell(review@performance, "metric", metric, column)
}
.base <- function(review, metric, column) {
  .cell(review@baseline, "metric", metric, column)
}
.tune <- function(review, name, column) {
  .cell(review@tuning, "hyperparameter", name, column)
}
.finding <- function(review, code) {
  Filter(function(f) f@code == code, review@findings)[[1L]]
}


# %% Single split ----
idx <- c(1:40, 51:90, 101:140)
mod_iris <- train(
  iris[idx, ],
  dat_test = iris[-idx, ],
  hyperparameters = setup_CART(),
  verbosity = 0L
)
rev_iris <- review(mod_iris)

test_that("review() returns a SupervisedReview with typed tables", {
  expect_s7_class(rev_iris, SupervisedReview)
  expect_identical(rev_iris@type, "Classification")
  expect_identical(rev_iris@description, desc(mod_iris))
  expect_identical(rev_iris@confidence_level, 0.95)
  expect_identical(rev_iris@min_cases_per_predictor, 10)
  expect_identical(rev_iris@limitations, REVIEW_LIMITATIONS)
  expect_null(rev_iris@tuning)
  for (f in rev_iris@findings) {
    expect_identical(f@plain, unname(REVIEW_PLAIN[[f@code]]))
  }
})

test_that("the review states every training and test metric and the baseline", {
  overall_training <- mod_iris@metrics_training[["overall"]]
  overall_test <- mod_iris@metrics_test[["overall"]]
  expect_identical(rev_iris@performance[["metric"]], names(overall_training))
  for (metric in names(overall_training)) {
    expect_equal(
      .perf(rev_iris, metric, "training"),
      overall_training[[metric]]
    )
    expect_equal(.perf(rev_iris, metric, "test"), overall_test[[metric]])
    expect_equal(
      .perf(rev_iris, metric, "difference"),
      overall_training[[metric]] - overall_test[[metric]]
    )
    expect_true(is.na(.perf(rev_iris, metric, "test_sd")))
    expect_true(is.na(.perf(rev_iris, metric, "pooled")))
  }
  sample <- rev_iris@sample
  expect_identical(sample[["n_training"]], 120L)
  expect_identical(sample[["n_test"]], 30L)
  expect_identical(sample[["n_predictors"]], 4L)
  expect_identical(sample[["n_learner_columns"]], 4L)
  expect_null(sample[["n_resamples"]])
  expect_identical(rev_iris@class_counts[["training"]], rep(40L, 3L))
  expect_identical(rev_iris@class_counts[["test"]], rep(10L, 3L))
  # The training majority is the first level on a tie; its share of the test
  # set is the baseline accuracy.
  expect_equal(.base(rev_iris, "accuracy", "baseline"), 1 / 3)
  expect_identical(.base(rev_iris, "accuracy", "reference"), "majority_class")
  expect_identical(.base(rev_iris, "accuracy", "method"), "exact_mcnemar")
  expect_equal(.base(rev_iris, "balanced_accuracy", "baseline"), 1 / 3)
  expect_identical(.base(rev_iris, "balanced_accuracy", "reference"), "chance")
  expect_identical(.base(rev_iris, "accuracy", "outcome"), "better")
})

test_that("accuracy against the majority class uses the paired McNemar test", {
  y <- mod_iris@y_test
  majority <- levels(y)[[1L]]
  test <- review_mcnemar(
    mod_iris@predicted_test == y,
    y == majority
  )
  expect_equal(.base(rev_iris, "accuracy", "p_value"), test[["p_value"]])
  expect_match(
    .finding(rev_iris, "BASELINE_ACCURACY")@message,
    "exact McNemar",
    fixed = TRUE
  )
})

test_that("a model clearly better than its baseline gets notes, not warnings", {
  expect_identical(
    review_codes(rev_iris),
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
  # 40 cases in the smallest class over 4 learner columns is exactly 10.
  expect_false("FEW_CASES_PER_PREDICTOR" %in% review_codes(rev_iris))
  stricter <- review(mod_iris, min_cases_per_predictor = 10.5)
  expect_true(all(
    c("FEW_CASES_PER_PREDICTOR", "PRESELECTION_RISK") %in%
      review_codes(stricter)
  ))
  few <- .finding(stricter, "FEW_CASES_PER_PREDICTOR")
  expect_identical(few@severity, "note")
  expect_match(few@message, "rule of thumb", fixed = TRUE)
  expect_identical(stricter@min_cases_per_predictor, 10.5)
})

test_that("review() validates its settings and draws no random numbers", {
  expect_error(
    review(mod_iris, confidence_level = 1),
    class = "rtemis_input_error"
  )
  expect_error(
    review(mod_iris, min_cases_per_predictor = 0),
    class = "rtemis_input_error"
  )
  set.seed(1)
  before <- .Random.seed
  review(mod_iris)
  expect_identical(.Random.seed, before)
})

test_that("an unrounded confidence level is printed as given", {
  out <- repr(review(mod_iris, confidence_level = 0.999), output_type = "plain")
  expect_match(out, "99.9% CI", fixed = TRUE)
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
  expect_match(
    .finding(out, "CLASS_NEVER_PREDICTED")@message,
    "'virginica'",
    fixed = TRUE
  )
})

test_that("a class absent from the test set leaves balanced accuracy undefined", {
  mod <- train(
    iris[idx, ],
    dat_test = iris[41:50, ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  out <- review(mod)
  codes <- review_codes(out)
  expect_true("ABSENT_TEST_CLASSES" %in% codes)
  expect_match(
    .finding(out, "ABSENT_TEST_CLASSES")@message,
    "'versicolor', 'virginica'",
    fixed = TRUE
  )
  # The model's own metric is unavailable, and the review does not substitute
  # a different one.
  expect_true(is.na(.perf(out, "balanced_accuracy", "test")))
  expect_true(is.na(.perf(out, "balanced_accuracy", "lower")))
  expect_true(is.na(.base(out, "balanced_accuracy", "model")))
  expect_false(any(
    c("BASELINE_BALANCED_ACCURACY", "TEST_PRECISION") %in% codes
  ))
  # Every test case is setosa, so identical predictions say nothing.
  expect_false("CONSTANT_PREDICTIONS" %in% codes)
})

test_that("one test case gives no intervals and no constant-prediction finding", {
  mod <- train(
    mtcars[1:25, c("wt", "mpg")],
    dat_test = mtcars[26, c("wt", "mpg")],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  out <- review(mod)
  codes <- review_codes(out)
  expect_false(any(
    c("TEST_PRECISION", "CONSTANT_PREDICTIONS", "BASELINE_MSE") %in% codes
  ))
  expect_true(all(is.na(out@performance[["lower"]])))
  # R-squared on one case is undefined, not infinite.
  expect_true(is.na(.perf(out, "rsq", "test")))
  # Past the model's own description, nothing undefined is printed.
  printed <- sub("^.*?\n.*?\n", "", repr(out, output_type = "plain"))
  expect_false(grepl("NA|Inf", printed))
})

test_that("an AUC without a usable interval gets no verdict", {
  d <- iris[51:150, c("Petal.Length", "Species")]
  d[["Species"]] <- factor(d[["Species"]])
  mod <- train(
    d[c(1:40, 51:90), ],
    dat_test = d[c(41:50, 91:100), ],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  prob <- positive_prob(mod@predicted_prob_test)
  positive <- mod@y_test == levels(mod@y_test)[[mod@binclasspos]]
  auc <- review_auc_delong(prob, positive, 0.95)
  out <- review(mod)
  expect_identical(
    is.na(.base(out, "auc", "outcome")),
    !review_finite(auc)
  )
  expect_identical(
    "BASELINE_AUC" %in% review_codes(out),
    review_finite(auc)
  )
})

test_that("a model without a test set is reported as unassessed", {
  mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  out <- review(mod)
  expect_identical(review_codes(out), "NO_TEST_SET")
  expect_null(out@baseline)
  expect_null(out@sample[["n_test"]])
  expect_true(all(is.na(out@performance[["test"]])))
  expect_true(all(is.na(out@class_counts[["test"]])))
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
  gap <- .finding(out, "GENERALIZATION_GAP")
  expect_identical(gap@severity, "warning")
  # An observation with possible explanations, not a diagnosis.
  expect_match(gap@message, "may indicate overfitting", fixed = TRUE)
  # Untuned, so the suggestion is to tune.
  expect_match(gap@suggestion, "^Tune")
  expect_false(identical(
    .base(out, "balanced_accuracy", "outcome"),
    "better"
  ))
})

test_that("p > n is reported alongside, not instead of, other findings", {
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
  expected <- if (isFALSE(algorithm_handles_p_gt_n(mod@algorithm))) {
    "warning"
  } else {
    "note"
  }
  expect_identical(.finding(out, "DIM_P_GT_N")@severity, expected)
})

test_that("predictor counts follow a partial decomposition", {
  mod <- train(
    iris,
    hyperparameters = setup_CART(),
    decomposition_config = setup_PCA(
      k = 1L,
      features = c("Sepal.Length", "Sepal.Width")
    ),
    verbosity = 0L
  )
  # The learner receives the two retained predictors and one component.
  expect_identical(length(mod@xnames), 3L)
  out <- review(mod)
  expect_identical(out@sample[["n_predictors"]], 4L)
  expect_identical(out@sample[["n_learner_columns"]], 3L)
  expect_identical(out@sample[["n_components"]], 1L)
  expect_equal(out@sample[["cases_per_predictor"]], 50 / 3)
})

test_that("binary probability comparisons agree with the model's own metrics", {
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
  overall_test <- mod@metrics_test[["overall"]]
  positive_level <- levels(d[["Species"]])[[mod@binclasspos]]
  prob <- positive_prob(mod@predicted_prob_test)
  auc <- review_auc_delong(prob, mod@y_test == positive_level, 0.95)
  expect_equal(auc[[1L]], overall_test[["auc"]])
  expect_equal(.perf(out, "auc", "lower"), auc[[2L]])
  expect_equal(.base(out, "auc", "model_lower"), auc[[2L]])
  prevalence <- mean(mod@y_training == positive_level)
  y01 <- as.numeric(mod@y_test == positive_level)
  loss_baseline <- (y01 - prevalence)^2
  expect_equal(.base(out, "brier_score", "baseline"), mean(loss_baseline))
  expect_identical(.base(out, "brier_score", "method"), "paired_t")
  expect_equal(
    .base(out, "brier_score", "skill"),
    1 - overall_test[["brier_score"]] / mean(loss_baseline)
  )
  expect_equal(
    c(
      .base(out, "brier_score", "difference"),
      .base(out, "brier_score", "difference_lower"),
      .base(out, "brier_score", "difference_upper")
    ),
    review_loss_difference((y01 - prob)^2, loss_baseline, 0.95)
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
  training_mean <- mean(reg[["y"]][1:100])
  y_test <- reg[["y"]][101:150]
  expect_equal(.base(out, "mse", "baseline"), mean((y_test - training_mean)^2))
  expect_lte(.base(out, "rsq", "baseline"), 0)
  expect_equal(.base(out, "mse", "model"), mod@metrics_test@metrics[["mse"]])
  expect_equal(.perf(out, "mse", "test"), mod@metrics_test@metrics[["mse"]])
  expect_null(out@class_counts)
  expect_identical(.finding(out, "BASELINE_MSE")@severity, "note")
  expect_false("GENERALIZATION_GAP" %in% review_codes(out))
})


# %% Class contract ----
test_that("every review code carries its plain text", {
  expect_setequal(names(REVIEW_PLAIN), REVIEW_CODES)
  expect_error(new_review_finding("NOT_A_CODE", "note", "m"))
})

test_that("repr prints one row per metric, then the baseline and findings", {
  out <- repr(rev_iris, output_type = "plain")
  expect_lt(regexpr("Performance", out), regexpr("Baseline comparisons", out))
  expect_lt(regexpr("Baseline comparisons", out), regexpr("Findings", out))
  expect_lt(regexpr("Findings", out), regexpr("Limitations", out))
  expect_match(out, "Training - test", fixed = TRUE)
  expect_match(out, "Balanced Accuracy\\s+0\\.\\d{3}\\s+1\\.000\\s+-0\\.\\d{3}")
  # Each comparison names its reference.
  expect_match(out, "(most common training class)", fixed = TRUE)
  expect_match(out, "(chance level)", fixed = TRUE)
})


# %% .review_validator ----
# The generated SupervisedReview schema with the real ReviewFinding schema
# inlined wherever it is referenced, so findings are checked against their own
# contract rather than a stand-in.
.review_validator <- function() {
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
  jsonvalidate::json_validator(
    jsonlite::toJSON(
      inline(review_schema),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    engine = "ajv"
  )
}

.review_json <- function(doc) {
  # Serialized as rtemis's writers do: a missing table cell is null.
  jsonlite::toJSON(
    doc,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

test_that("a review record validates against its published schema", {
  testthat::skip_if_not_installed("jsonvalidate")
  validate <- .review_validator()
  doc <- record_object(rev_iris)
  expect_true(validate(.review_json(doc), verbose = TRUE))
  # Negative cases: values outside their vocabularies.
  bad <- doc
  bad[["findings"]][[1L]][["severity"]] <- "fatal"
  expect_false(validate(.review_json(bad)))
  bad <- doc
  bad[["baseline"]][["outcome"]][[1L]] <- "excellent"
  expect_false(validate(.review_json(bad)))
  bad <- doc
  bad[["baseline"]][["method"]][[1L]] <- "bootstrap"
  expect_false(validate(.review_json(bad)))
  bad <- doc
  bad[["baseline"]][["reference"]][[1L]] <- "oracle"
  expect_false(validate(.review_json(bad)))
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

test_that("a resampled review states fold means and SDs", {
  expect_s7_class(rev_res, SupervisedReview)
  for (metric in names(mod_res@metrics_training@mean_metrics)) {
    expect_equal(
      .perf(rev_res, metric, "training"),
      mod_res@metrics_training@mean_metrics[[metric]]
    )
    expect_equal(
      .perf(rev_res, metric, "test"),
      mod_res@metrics_test@mean_metrics[[metric]]
    )
    expect_equal(
      .perf(rev_res, metric, "training_sd"),
      mod_res@metrics_training@sd_metrics[[metric]]
    )
    expect_equal(
      .perf(rev_res, metric, "test_sd"),
      mod_res@metrics_test@sd_metrics[[metric]]
    )
  }
  sample <- rev_res@sample
  expect_identical(sample[["n_resamples"]], 5L)
  expect_identical(sample[["n_resamples_requested"]], 5L)
  expect_identical(sample[["n_test"]], 100L)
  expect_identical(sample[["n_test_cases"]], 100L)
  out <- repr(rev_res, output_type = "plain")
  expect_match(out, "mean (SD) over 5 resamples", fixed = TRUE)
  expect_match(out, "Accuracy\\s+0\\.\\d{3} \\(0\\.\\d{3}\\)")
})

test_that("a resampled review makes no interval, test or verdict", {
  # Resamples share training cases; their results are dependent.
  expect_true(all(is.na(rev_res@performance[["lower"]])))
  expect_true(all(is.na(rev_res@baseline[["model_lower"]])))
  expect_true(all(is.na(rev_res@baseline[["difference_lower"]])))
  expect_true(all(is.na(rev_res@baseline[["p_value"]])))
  expect_true(all(is.na(rev_res@baseline[["outcome"]])))
  expect_true(all(rev_res@baseline[["method"]] == "descriptive"))
  codes <- review_codes(rev_res)
  expect_true("FOLD_VARIATION" %in% codes)
  expect_false(any(
    c(
      "SINGLE_SPLIT",
      "NO_TEST_SET",
      "TEST_PRECISION",
      "GENERALIZATION_GAP",
      "BASELINE_ACCURACY",
      "BASELINE_BALANCED_ACCURACY",
      "BASELINE_AUC",
      "BASELINE_BRIER"
    ) %in%
      codes
  ))
  expect_match(
    .finding(rev_res, "FOLD_VARIATION")@message,
    "not independent",
    fixed = TRUE
  )
})

test_that("pooled values are descriptive and exclude AUC", {
  y <- do.call(c, lapply(mod_res@models, function(m) m@y_test))
  predicted <- do.call(c, lapply(mod_res@models, function(m) m@predicted_test))
  expect_equal(.perf(rev_res, "accuracy", "pooled"), mean(y == predicted))
  expect_true(is.na(.perf(rev_res, "auc", "pooled")))
})

test_that("each resample is compared with its own baseline", {
  baseline_accuracy <- vapply(
    mod_res@models,
    function(m) {
      majority <- names(which.max(table(m@y_training)))
      mean(as.character(m@y_test) == majority)
    },
    numeric(1L)
  )
  model_accuracy <- vapply(
    mod_res@models,
    function(m) m@metrics_test[["overall"]][["accuracy"]],
    numeric(1L)
  )
  expect_equal(.base(rev_res, "accuracy", "baseline"), mean(baseline_accuracy))
  expect_equal(
    .base(rev_res, "accuracy", "difference"),
    mean(model_accuracy - baseline_accuracy)
  )
  expect_identical(
    .base(rev_res, "accuracy", "resamples_better"),
    sum(model_accuracy > baseline_accuracy)
  )
  better <- sum(vapply(
    mod_res@models,
    function(m) m@metrics_test[["overall"]][["balanced_accuracy"]] > 0.5,
    logical(1L)
  ))
  expect_identical(
    .base(rev_res, "balanced_accuracy", "resamples_better"),
    better
  )
})

test_that("resampled AUC is the mean of fold AUCs, unaffected by fold-specific score scales", {
  # An increasing transformation of one fold's scores leaves that fold's AUC
  # unchanged; a pooled AUC would change, the mean of fold AUCs does not.
  transformed <- mod_res
  models <- transformed@models
  models[[1L]]@predicted_prob_test <- models[[1L]]@predicted_prob_test^3
  transformed@models <- models
  out <- review(transformed)
  expect_equal(.base(out, "auc", "model"), .base(rev_res, "auc", "model"))
  expect_equal(
    .base(rev_res, "auc", "model"),
    mod_res@metrics_test@mean_metrics[["auc"]]
  )
})

test_that("bootstrap resamples count each case once", {
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
  expect_true("FOLD_VARIATION" %in% review_codes(out))
  # Overlapping test sets: nothing is pooled.
  expect_true(all(is.na(out@performance[["pooled"]])))
  expect_lt(out@sample[["n_test_cases"]], out@sample[["n_test"]])
  unique_cases <- vapply(
    mod@outer_resampler@resamples[mod@resample_ids],
    function(idx) length(unique(idx)),
    integer(1L)
  )
  expect_identical(out@sample[["n_training"]], min(unique_cases))
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
  baseline_mse <- vapply(
    mod@models,
    function(m) mean((m@y_test - mean(m@y_training))^2),
    numeric(1L)
  )
  expect_equal(.base(out, "mse", "baseline"), mean(baseline_mse))
  expect_identical(.base(out, "mse", "resamples_better"), 4L)
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
  expect_identical(.tune(out, "maxdepth", "n_at_edge"), sum(selected == 3))
  expect_identical(
    "TUNING_GRID_EDGE" %in% review_codes(out),
    any(selected == 3)
  )
  # Two values searched: every choice is an edge, so cp is not checked.
  expect_false("cp" %in% out@tuning[["hyperparameter"]])
})

test_that("a single-split tuned model states its selected value", {
  mod <- train(
    iris[idx, ],
    dat_test = iris[-idx, ],
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L, 4L)),
    verbosity = 0L
  )
  out <- review(mod)
  expect_equal(
    .tune(out, "maxdepth", "selected"),
    mod@tuner@best_hyperparameters[["maxdepth"]]
  )
  expect_identical(.tune(out, "maxdepth", "n_values"), 3L)
})

test_that("a resampled review record validates against its published schema", {
  testthat::skip_if_not_installed("jsonvalidate")
  validate <- .review_validator()
  expect_true(validate(.review_json(record_object(rev_res)), verbose = TRUE))
})


# %% Markdown ----
.md_lines <- function(x) strsplit(to_markdown(x), "\n", fixed = TRUE)[[1L]]

test_that("Markdown states the review's sections in print order", {
  for (rev in list(rev_iris, rev_res)) {
    md <- .md_lines(rev)
    headings <- md[startsWith(md, "## ")]
    expect_identical(
      headings,
      paste0(
        "## ",
        c(
          review_performance_title(rev),
          review_baseline_title(rev),
          "Findings",
          "Limitations"
        )
      )
    )
    expect_identical(md[[1L]], rev@description)
    expect_true(review_sample_line(rev) %in% md)
    expect_true(endsWith(to_markdown(rev), "\n"))
  }
})

test_that("Markdown tables and lists carry the values print shows", {
  for (rev in list(rev_iris, rev_res)) {
    md <- .md_lines(rev)
    printed <- repr(rev, output_type = "plain")
    rows <- review_performance_rows(rev)
    for (i in seq_len(NROW(rows))[-1L]) {
      expect_true(
        paste0("| ", paste(rows[i, ], collapse = " | "), " |") %in% md
      )
      for (cell in rows[i, ][nzchar(rows[i, ])]) {
        expect_match(printed, cell, fixed = TRUE)
      }
    }
    for (line in review_baseline_text(rev)) {
      expect_true(paste0("- ", line) %in% md)
      expect_match(printed, line, fixed = TRUE)
    }
    for (f in rev@findings) {
      expect_true(
        paste0("- **`", f@code, "`** (", f@severity, "): ", f@message) %in% md
      )
    }
    expect_true(all(paste0("- ", rev@limitations) %in% md))
  }
})

test_that("Markdown of a tuned review lists the tuned hyperparameters", {
  mod <- train(
    iris[idx, ],
    dat_test = iris[-idx, ],
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L, 4L)),
    verbosity = 0L
  )
  rev <- review(mod)
  md <- .md_lines(rev)
  expect_true("## Tuning" %in% md)
  tuning <- md[startsWith(md, "- `maxdepth`: searched 2 to 4 (3 values)")]
  expect_length(tuning, 1L)
  expect_match(
    tuning,
    paste0("selected ", rev@tuning[["selected"]][[1L]], "$")
  )
})


# %% write_text ----
test_that("write_text writes a review and selects the format by extension", {
  path <- tempfile(fileext = ".md")
  expect_identical(write_text(rev_iris, path, verbosity = 0L), path)
  expect_identical(
    paste0(paste(readLines(path), collapse = "\n"), "\n"),
    to_markdown(rev_iris)
  )
  txt <- tempfile(fileext = ".txt")
  expect_error(
    write_text(rev_iris, txt, verbosity = 0L),
    "Cannot select a format",
    class = "rtemis_value_error"
  )
  expect_false(file.exists(txt))
  write_text(rev_iris, txt, format = "markdown", verbosity = 0L)
  expect_identical(readLines(txt), readLines(path))
  expect_error(
    write_text(rev_iris, tempfile(fileext = ".md"), format = "html"),
    class = "rtemis_value_error"
  )
})

test_that("write_text accepts only reports", {
  expect_error(
    write_text(mod_iris, tempfile(fileext = ".md"), verbosity = 0L),
    "Can't find method"
  )
})
