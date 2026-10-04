# review.R
# ::rtemis::
# 2026- EDG rtemis.org

# `review()` for a trained supervised model, single-split or resampled. The
# classes and the finding vocabulary live in `280_SupervisedReview.R`.
#
# A single split is one fold and a resampled model one fold per successful
# outer resample, so both take one path. Inference -- confidence intervals,
# tests, better/worse verdicts -- is made only for a single split, whose test
# cases are independent of a fixed fitted model. Resamples share training
# cases, so their test results are dependent; a resampled model is described
# by the distribution of its per-resample results instead.
#
# Every interval here is analytic, so a review draws no random numbers and the
# same model always gets the same review.
#
# spec: rtemis/review-method

# %% review_binom_interval ----
#' Clopper-Pearson interval for a proportion
#'
#' @param successes Integer: Number of successes.
#' @param n Integer: Number of trials.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 2: lower and upper bound; `NA` when `n` is
#'   zero.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_binom_interval <- function(successes, n, level) {
  if (n == 0L) {
    return(c(NA_real_, NA_real_))
  }
  as.numeric(stats::binom.test(successes, n, conf.level = level)[["conf.int"]])
} # /rtemis::review_binom_interval


# %% review_balanced_accuracy_interval ----
#' Interval for balanced accuracy from per-class binomial variances
#'
#' Balanced accuracy is the mean of the per-class recalls, which are
#' independent binomial proportions, so its variance is the sum of theirs over
#' the squared number of classes. Each variance is computed with one success
#' and one failure added to its class, which keeps the interval from
#' collapsing to a point when a recall is 0 or 1. Balanced accuracy is defined
#' over every class, so a class absent from the test set leaves it undefined.
#'
#' @param hits Integer vector: Correct predictions per class.
#' @param totals Integer vector: Test cases per class.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 2, clipped to \[0, 1\]; `NA` when a class
#'   has no test cases.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_balanced_accuracy_interval <- function(hits, totals, level) {
  if (length(totals) == 0L || any(totals == 0L)) {
    return(c(NA_real_, NA_real_))
  }
  k <- length(totals)
  estimate <- mean(hits / totals)
  adjusted <- (hits + 1) / (totals + 2)
  se <- sqrt(sum(adjusted * (1 - adjusted) / totals)) / k
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(max(0, estimate - z * se), min(1, estimate + z * se))
} # /rtemis::review_balanced_accuracy_interval


# %% review_auc_delong ----
#' AUC and its DeLong interval
#'
#' DeLong, DeLong and Clarke-Pearson (1988), with midranks for ties. The
#' estimated variance is zero at complete separation (AUC 0 or 1), where the
#' normal approximation gives no usable interval; the bounds are then `NA`.
#'
#' @param prob Numeric vector: Predicted probability of the positive class.
#' @param positive Logical vector: Whether each case is positive.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 3: AUC, lower and upper bound, the bounds
#'   clipped to \[0, 1\]; AUC `NA` with fewer than two cases in either class,
#'   bounds `NA` also at zero estimated variance.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_auc_delong <- function(prob, positive, level) {
  pos <- prob[positive]
  neg <- prob[!positive]
  m <- length(pos)
  n <- length(neg)
  if (m < 2L || n < 2L) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  rank_all <- rank(c(pos, neg))
  v10 <- (rank_all[seq_len(m)] - rank(pos)) / n
  v01 <- 1 - (rank_all[m + seq_len(n)] - rank(neg)) / m
  estimate <- mean(v10)
  se <- sqrt(stats::var(v10) / m + stats::var(v01) / n)
  if (!(se > 0)) {
    return(c(estimate, NA_real_, NA_real_))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(estimate, max(0, estimate - z * se), min(1, estimate + z * se))
} # /rtemis::review_auc_delong


# %% review_mean_interval ----
#' t interval for a mean
#'
#' @param v Numeric vector.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 2; `NA` with fewer than two values or zero
#'   spread, where the t interval is undefined.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_mean_interval <- function(v, level) {
  n <- length(v)
  if (n < 2L || !(stats::sd(v) > 0)) {
    return(c(NA_real_, NA_real_))
  }
  half <- stats::qt(1 - (1 - level) / 2, df = n - 1L) * stats::sd(v) / sqrt(n)
  mean(v) + c(-half, half)
} # /rtemis::review_mean_interval


# %% review_loss_difference ----
#' Paired mean loss reduction over a baseline, with its t interval
#'
#' The per-case reduction is the baseline's loss minus the model's, so a
#' positive value favors the model. Its paired t interval is an interval for
#' the mean reduction, on the loss scale; it is not an interval for the skill
#' score, whose denominator is itself estimated.
#'
#' @param loss Numeric vector: Per-case loss of the model.
#' @param loss_baseline Numeric vector: Per-case loss of the baseline.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 3: mean reduction, lower and upper bound.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_loss_difference <- function(loss, loss_baseline, level) {
  reduction <- loss_baseline - loss
  c(mean(reduction), review_mean_interval(reduction, level))
} # /rtemis::review_loss_difference


# %% review_mcnemar ----
#' Exact McNemar test of two classifiers' accuracy on the same cases
#'
#' Only the discordant cases carry information: `b` cases the model gets right
#' and the baseline wrong, `c` the reverse. Under equal accuracy each
#' discordant case is equally likely to go either way, so `b` is binomial with
#' probability 1/2 out of `b + c`. The two-sided exact p-value is 1 with no
#' discordant cases.
#'
#' @param model_correct Logical vector: Model correct on each case.
#' @param baseline_correct Logical vector: Baseline correct on each case.
#'
#' @return List with `b`, `c` and `p_value` (two-sided).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_mcnemar <- function(model_correct, baseline_correct) {
  b <- sum(model_correct & !baseline_correct)
  c <- sum(!model_correct & baseline_correct)
  p_value <- if (b + c == 0L) {
    1
  } else {
    stats::binom.test(b, b + c, p = 0.5)[["p.value"]]
  }
  list(b = b, c = c, p_value = p_value)
} # /rtemis::review_mcnemar


# %% review_predictors ----
#' Predictor counts for one fitted model
#'
#' `xnames` holds the columns the learner received, after preprocessing and
#' decomposition: retained predictors plus components. The input width comes
#' from the training data's fingerprint (all columns but the outcome).
#'
#' @param m `Supervised` object: One fitted model.
#' @param fingerprint Optional `DataFingerprint`: Of the training data.
#'
#' @return List with `input` (input predictors, or NULL), `learner` (columns
#'   the learner received), `components` (fitted decomposition components, or
#'   NULL) and `decomposition` (algorithm, or NULL).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_predictors <- function(m, fingerprint) {
  components <- NULL
  decomposition <- NULL
  if (!is.null(m@decomposition)) {
    components <- NCOL(m@decomposition@transformed)
    decomposition <- m@decomposition@algorithm
  }
  list(
    input = if (!is.null(fingerprint)) fingerprint@n_cols - 1L,
    learner = length(m@xnames),
    components = components,
    decomposition = decomposition
  )
} # /rtemis::review_predictors


# %% fmt_review_num ----
# Three decimal places in fixed notation; values too small to show that way
# (p-values, mostly) in two significant digits.
fmt_review_num <- function(x) {
  if (!is.finite(x)) {
    return(format(x))
  }
  if (x != 0 && abs(x) < 0.001) {
    return(format(signif(x, 2L)))
  }
  sprintf("%.3f", x)
}


# %% fmt_review_level ----
# The confidence level as a percentage, unrounded: 0.999 is 99.9%.
fmt_review_level <- function(level) {
  paste0(format(level * 100, digits = 15L), "%")
}


# %% fmt_review_interval ----
fmt_review_interval <- function(interval, level) {
  paste0(
    fmt_review_level(level),
    " CI ",
    fmt_review_num(interval[[1L]]),
    " to ",
    fmt_review_num(interval[[2L]])
  )
}


# %% review_defined ----
# Undefined values (NaN, infinite) as NA.
review_defined <- function(v) {
  v[!is.finite(v)] <- NA_real_
  v
}


# %% review_finite ----
# Whether every value is a finite number.
review_finite <- function(x) {
  length(x) > 0L && all(is.finite(x))
}


# %% review_interval_outcome ----
#' Compare an interval with a reference value
#'
#' @param interval Numeric vector of length 2, oriented so that higher is
#'   better.
#' @param reference Numeric: Reference value on the same scale.
#'
#' @return Character: One of `REVIEW_BASELINE_OUTCOMES`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_interval_outcome <- function(interval, reference) {
  if (interval[[1L]] > reference) {
    "better"
  } else if (interval[[2L]] < reference) {
    "worse"
  } else {
    "indistinguishable"
  }
} # /rtemis::review_interval_outcome


# %% review_baseline_finding ----
#' A baseline comparison finding
#'
#' Worse is a warning; a model not shown to differ from the baseline is a
#' warning too, because nothing establishes that it improves on predicting
#' without the predictors, but the message says only that the evidence is not
#' clear.
#'
#' @param code Character: Finding code.
#' @param outcome Character: One of `REVIEW_BASELINE_OUTCOMES`.
#' @param what Character: What was compared, as the subject of a sentence.
#' @param baseline_text Character: The baseline, as the end of a sentence.
#'
#' @return `ReviewFinding` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline_finding <- function(code, outcome, what, baseline_text) {
  new_review_finding(
    code = code,
    severity = if (outcome == "better") "note" else "warning",
    message = switch(
      outcome,
      better = paste0(what, " is better than ", baseline_text, "."),
      worse = paste0(what, " is worse than ", baseline_text, "."),
      indistinguishable = paste0(
        what,
        " does not provide clear evidence of a difference from ",
        baseline_text,
        "; limited precision can cause this."
      )
    )
  )
} # /rtemis::review_baseline_finding


# %% review_folds ----
#' The evaluation units of a model, in one shape
#'
#' A single-split model is one fold; a resampled model has one per successful
#' outer resample. Each fold carries its training outcome as the model saw it
#' and with each case once, its test outcome, test predictions,
#' positive-class test probabilities (binary classification only), its
#' training and test metrics as one-row data.frames, and its predictor counts.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#'
#' @return List of folds.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_folds <- function(x) {
  resampled <- S7_inherits(x, SupervisedRes)
  models <- if (resampled) x@models else list(x)
  lapply(seq_along(models), function(j) {
    m <- models[[j]]
    # A bootstrap resample draws some cases more than once. The baseline is
    # fit to the resample as the model saw it; counting cases counts each
    # once.
    y_training_cases <- m@y_training
    if (resampled) {
      idx <- x@outer_resampler@resamples[[x@resample_ids[[j]]]]
      if (length(idx) == length(y_training_cases)) {
        y_training_cases <- y_training_cases[!duplicated(idx)]
      }
    }
    one_row <- function(metrics) {
      if (is.null(metrics)) {
        return(NULL)
      }
      if (m@type == "Classification") metrics[["overall"]] else metrics@metrics
    }
    list(
      y_training = m@y_training,
      y_training_cases = y_training_cases,
      y_test = m@y_test,
      predicted_test = m@predicted_test,
      prob_test = if (m@type == "Classification") {
        positive_prob(m@predicted_prob_test)
      },
      metrics_training = one_row(m@metrics_training),
      metrics_test = one_row(m@metrics_test),
      predictors = review_predictors(m, x@data_fingerprint)
    )
  })
} # /rtemis::review_folds


# %% review_test_cases ----
#' Distinct cases across the test sets of a resampled model
#'
#' Test cases are the cases outside each resample's training indices, as
#' `train()` takes them. With k-fold resampling every case is tested once;
#' repeated or bootstrap resampling tests some cases more than once.
#'
#' @param x `SupervisedRes` object.
#'
#' @return Integer: Number of distinct test cases.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_test_cases <- function(x) {
  resamples <- x@outer_resampler@resamples[x@resample_ids]
  test_sets <- lapply(seq_along(resamples), function(j) {
    training <- unique(resamples[[j]])
    n_cases <- length(training) + length(x@models[[j]]@y_test)
    setdiff(seq_len(n_cases), training)
  })
  length(unique(unlist(test_sets)))
} # /rtemis::review_test_cases


# %% review_pool ----
#' Concatenate one element across folds
#'
#' @param folds List from `review_folds()`.
#' @param name Character: Element name.
#'
#' @return The element's values from every fold, in fold order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_pool <- function(folds, name) {
  do.call(c, lapply(folds, `[[`, name))
} # /rtemis::review_pool


# %% review_classification_intervals ----
#' Test estimates and intervals for classification metrics (single split)
#'
#' @param y Factor: Test outcome.
#' @param predicted Factor: Test predictions.
#' @param prob Optional Numeric: Positive-class probabilities (binary).
#' @param binclasspos Integer: Position of the positive level (binary).
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Named list: for each metric with an interval, a numeric vector of
#'   estimate, lower and upper bound.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_classification_intervals <- function(
  y,
  predicted,
  prob,
  binclasspos,
  level
) {
  lv <- levels(y)
  confusion <- table(factor(y, levels = lv), factor(predicted, levels = lv))
  hits <- as.vector(diag(confusion))
  totals <- as.vector(rowSums(confusion))
  predicted_totals <- as.vector(colSums(confusion))
  proportion <- function(x, n) {
    c(if (n > 0L) x / n else NA_real_, review_binom_interval(x, n, level))
  }
  out <- list(
    accuracy = proportion(sum(hits), length(y)),
    balanced_accuracy = c(
      if (all(totals > 0L)) mean(hits / totals) else NA_real_,
      review_balanced_accuracy_interval(hits, totals, level)
    )
  )
  if (length(lv) == 2L) {
    pos <- binclasspos
    neg <- 3L - pos
    out[["sensitivity"]] <- proportion(hits[[pos]], totals[[pos]])
    out[["specificity"]] <- proportion(hits[[neg]], totals[[neg]])
    out[["ppv"]] <- proportion(hits[[pos]], predicted_totals[[pos]])
    out[["npv"]] <- proportion(hits[[neg]], predicted_totals[[neg]])
    if (!is.null(prob)) {
      out[["auc"]] <- review_auc_delong(prob, y == lv[[pos]], level)
    }
  }
  out
} # /rtemis::review_classification_intervals


# %% review_regression_intervals ----
#' Test estimates and intervals for regression metrics (single split)
#'
#' @param y Numeric: Test outcome.
#' @param predicted Numeric: Test predictions.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Named list as for `review_classification_intervals()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_regression_intervals <- function(y, predicted, level) {
  errors <- y - predicted
  mse <- c(mean(errors^2), pmax(0, review_mean_interval(errors^2, level)))
  list(
    mae = c(
      mean(abs(errors)),
      pmax(0, review_mean_interval(abs(errors), level))
    ),
    mse = mse,
    rmse = sqrt(mse)
  )
} # /rtemis::review_regression_intervals


# %% review_pooled ----
#' Pooled out-of-sample values of a resampled model (descriptive)
#'
#' Computed over all out-of-sample predictions when every case is tested once.
#' Only metrics that are averages over cases pool meaningfully: AUC is not
#' pooled, because it would rank scores from different fitted models against
#' each other.
#'
#' @param y Pooled test outcome.
#' @param predicted Pooled test predictions.
#' @param prob Optional pooled positive-class probabilities.
#' @param binclasspos Optional Integer: Position of the positive level.
#'
#' @return Named numeric vector, one value per pooled metric.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_pooled <- function(y, predicted, prob, binclasspos) {
  if (is.factor(y)) {
    values <- review_classification_intervals(
      y,
      predicted,
      prob = NULL,
      binclasspos = binclasspos,
      level = 0.95
    )
    out <- vapply(values, `[[`, numeric(1L), 1L)
    if (!is.null(prob) && nlevels(y) == 2L) {
      y01 <- as.numeric(y == levels(y)[[binclasspos]])
      out[["brier_score"]] <- mean((y01 - prob)^2)
    }
    out
  } else {
    errors <- y - predicted
    c(
      mae = mean(abs(errors)),
      mse = mean(errors^2),
      rmse = sqrt(mean(errors^2)),
      rsq = 1 - sum(errors^2) / sum((y - mean(y))^2)
    )
  }
} # /rtemis::review_pooled


# %% review_performance ----
#' One row per metric: training, test, their difference
#'
#' One fold gives training, test, their difference and the test interval.
#' Several folds give the mean and standard deviation of each over resamples,
#' the difference of the means, and the pooled out-of-sample value when every
#' case was tested once; no interval, the resamples being dependent.
#'
#' @param folds List from `review_folds()`.
#' @param intervals Optional named list from `review_*_intervals()` (single
#'   split).
#' @param pooled Optional named numeric from `review_pooled()` (resampled).
#'
#' @return data.frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_performance <- function(folds, intervals, pooled) {
  resampled <- length(folds) > 1L
  has_test <- !is.null(folds[[1L]][["metrics_test"]])
  metrics <- names(folds[[1L]][["metrics_training"]])
  rows <- lapply(metrics, function(metric) {
    training <- vapply(
      folds,
      function(f) f[["metrics_training"]][[metric]],
      numeric(1L)
    )
    test <- if (has_test) {
      vapply(
        folds,
        function(f) f[["metrics_test"]][[metric]] %||% NA_real_,
        numeric(1L)
      )
    } else {
      NA_real_
    }
    interval <- intervals[[metric]] %||% rep(NA_real_, 3L)
    sd_or_na <- function(v) {
      if (resampled && length(v) > 1L) stats::sd(v) else NA_real_
    }
    data.frame(
      metric = metric,
      training = mean(training),
      training_sd = sd_or_na(training),
      test = mean(test),
      test_sd = if (has_test) sd_or_na(test) else NA_real_,
      difference = mean(training) - mean(test),
      pooled = if (metric %in% names(pooled)) pooled[[metric]] else NA_real_,
      lower = interval[[2L]],
      upper = interval[[3L]]
    )
  })
  out <- do.call(rbind, rows)
  # Undefined values are NA -- never NaN or infinite, as R-squared is on a
  # single test case -- so serialization and printing treat them alike.
  numeric_columns <- vapply(out, is.numeric, logical(1L))
  out[numeric_columns] <- lapply(out[numeric_columns], review_defined)
  out
} # /rtemis::review_performance


# %% review_sample ----
#' The review's sample record
#'
#' Predictor counts are read per fold, since preprocessing and decomposition
#' can differ between resamples: `n_learner_columns` and `cases_per_predictor`
#' are taken at the fold that gives the fewest cases per learner column.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param minority Integer vector: The case count each fold's ratio uses --
#'   minority-class training cases for classification, training cases for
#'   regression.
#'
#' @return Named list with every member of `SupervisedReview@sample`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_sample <- function(x, folds, minority) {
  resampled <- S7_inherits(x, SupervisedRes)
  has_test <- !is.null(folds[[1L]][["y_test"]])
  n_test <- sum(vapply(folds, function(f) length(f[["y_test"]]), integer(1L)))
  learner <- vapply(
    folds,
    function(f) f[["predictors"]][["learner"]],
    integer(1L)
  )
  ratios <- minority / learner
  worst <- which.min(ratios)
  predictors <- folds[[worst]][["predictors"]]
  list(
    n_training = min(vapply(
      folds,
      function(f) length(f[["y_training_cases"]]),
      integer(1L)
    )),
    n_test = if (has_test) n_test,
    n_test_cases = if (has_test) {
      if (resampled) review_test_cases(x) else n_test
    },
    n_resamples = if (resampled) length(folds),
    n_resamples_requested = if (resampled) {
      length(x@outer_resampler@resamples)
    },
    n_predictors = predictors[["input"]],
    n_learner_columns = predictors[["learner"]],
    n_components = predictors[["components"]],
    decomposition = predictors[["decomposition"]],
    cases_per_predictor = ratios[[worst]]
  )
} # /rtemis::review_sample


# %% review_sample_findings ----
#' Sample-size, resampling and dimensionality findings
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param sample List: The review's `sample`.
#' @param context List: `has_test`, `headline_label`, `headline_interval`
#'   (estimate, lower, upper; single split), `fold_test` (headline per fold),
#'   `folds_better` (count), `absent` (classes absent from the test set, or
#'   for resampled models the number of resamples missing a class),
#'   `min_cases_per_predictor`, `level`.
#'
#' @return List of `ReviewFinding`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_sample_findings <- function(x, sample, context) {
  findings <- list()
  resampled <- !is.null(sample[["n_resamples"]])
  level <- context[["level"]]
  n_training <- sample[["n_training"]]

  # Evaluation ----
  if (!context[["has_test"]]) {
    findings <- c(
      findings,
      new_review_finding(
        code = "NO_TEST_SET",
        severity = "warning",
        message = paste0(
          "The model was evaluated only on its ",
          n_training,
          " training cases; performance on new cases cannot be assessed."
        ),
        suggestion = "Hold out a test set, or use outer resampling."
      )
    )
  } else if (!resampled) {
    findings <- c(
      findings,
      new_review_finding(
        code = "SINGLE_SPLIT",
        severity = "note",
        message = paste0(
          "Performance was estimated on a single split, with ",
          sample[["n_test"]],
          ngettext(sample[["n_test"]], " test case", " test cases"),
          ". The test intervals reflect the number of test cases, ",
          "not how much the estimate would change with a different split."
        ),
        suggestion = paste0(
          "Use outer resampling to test on every case and to measure ",
          "variation between splits; repeat it when cases are few."
        )
      )
    )
  }
  interval <- context[["headline_interval"]]
  if (!resampled && context[["has_test"]] && review_finite(interval)) {
    findings <- c(
      findings,
      new_review_finding(
        code = "TEST_PRECISION",
        severity = "note",
        message = paste0(
          "Test ",
          context[["headline_label"]],
          " is ",
          fmt_review_num(interval[[1L]]),
          " (",
          fmt_review_interval(interval[2:3], level),
          "), from ",
          sample[["n_test"]],
          " test cases."
        )
      )
    )
  }
  absent <- context[["absent"]]
  if (length(absent) > 0L) {
    findings <- c(
      findings,
      new_review_finding(
        code = "ABSENT_TEST_CLASSES",
        severity = "note",
        message = if (resampled) {
          paste0(
            "In ",
            absent,
            " of ",
            sample[["n_resamples"]],
            " resamples, some class has no test cases; balanced accuracy and ",
            "per-class metrics are undefined for those resamples."
          )
        } else {
          paste0(
            ngettext(length(absent), "Class ", "Classes "),
            paste0("'", absent, "'", collapse = ", "),
            ngettext(length(absent), " has", " have"),
            " no test cases; balanced accuracy and the metrics of ",
            ngettext(length(absent), "that class", "those classes"),
            " are undefined."
          )
        },
        suggestion = "Stratify the split by outcome class."
      )
    )
  }
  if (resampled && context[["has_test"]]) {
    fold_test <- context[["fold_test"]]
    fold_test <- fold_test[!is.na(fold_test)]
    findings <- c(
      findings,
      new_review_finding(
        code = "FOLD_VARIATION",
        severity = "note",
        message = paste0(
          if (length(fold_test) > 0L) {
            paste0(
              "Test ",
              context[["headline_label"]],
              " ranged from ",
              fmt_review_num(min(fold_test)),
              " to ",
              fmt_review_num(max(fold_test)),
              " across ",
              length(fold_test),
              " resamples (mean ",
              fmt_review_num(mean(fold_test)),
              if (length(fold_test) > 1L) {
                paste0(", SD ", fmt_review_num(stats::sd(fold_test)))
              },
              "); "
            )
          },
          context[["folds_better"]],
          " of ",
          sample[["n_resamples"]],
          " resamples outperformed their baseline. Resamples share training ",
          "cases, so their results are not independent, and no confidence ",
          "interval or test is computed from them."
        )
      )
    )
  }

  # Dimensionality ----
  handles <- algorithm_handles_p_gt_n(x@algorithm)
  dim_suggestion <- paste0(
    "Judge the model by held-out performance. Consider a regularized or ",
    "sparse algorithm, or a decomposition step in train(), which is fitted on ",
    "training cases only."
  )
  columns <- sample[["n_learner_columns"]]
  seen <- paste0(
    columns,
    ngettext(columns, " column", " columns"),
    if (!is.null(sample[["n_components"]])) {
      paste0(
        ", including ",
        sample[["n_components"]],
        " components from ",
        sample[["decomposition"]]
      )
    }
  )
  training_cases <- if (resampled) {
    paste0(n_training, " training cases in its smallest resample")
  } else {
    paste0(n_training, " training cases")
  }
  dim_p_gt_n <- columns > n_training
  few_cases <- !dim_p_gt_n &&
    sample[["cases_per_predictor"]] < context[["min_cases_per_predictor"]]
  if (dim_p_gt_n) {
    findings <- c(
      findings,
      new_review_finding(
        code = "DIM_P_GT_N",
        severity = if (isFALSE(handles)) "warning" else "note",
        message = paste0(
          "The learner received ",
          seen,
          " but has ",
          training_cases,
          "; with more predictors than cases, training performance is not ",
          "evidence of predictive ability.",
          if (isFALSE(handles)) {
            paste0(
              " ",
              x@algorithm,
              " is not designed for more predictors than cases."
            )
          }
        ),
        suggestion = dim_suggestion
      )
    )
  } else if (few_cases) {
    findings <- c(
      findings,
      new_review_finding(
        code = "FEW_CASES_PER_PREDICTOR",
        severity = "note",
        message = paste0(
          "There are ",
          fmt_review_num(sample[["cases_per_predictor"]]),
          if (x@type == "Classification") {
            " minority-class training cases"
          } else {
            " training cases"
          },
          " per learner column (",
          seen,
          "), fewer than ",
          format(context[["min_cases_per_predictor"]]),
          ". This is a rule of thumb from logistic regression (events per ",
          "variable), not a validated sample-size requirement for every ",
          "algorithm."
        ),
        suggestion = dim_suggestion
      )
    )
  }
  if (dim_p_gt_n || few_cases) {
    findings <- c(
      findings,
      new_review_finding(
        code = "PRESELECTION_RISK",
        severity = "note",
        message = paste0(
          "With few cases per predictor, predictors are often selected ",
          "before modeling. Selection or filtering of predictors done on all ",
          "the data before it was passed to train() biases evaluation; this ",
          "review cannot determine whether that happened. Preprocessing, ",
          "decomposition and tuning within train() use training cases only."
        ),
        suggestion = paste0(
          "Avoid selecting predictors on all the data before train(). To ",
          "reduce the predictors, use a sparse algorithm or a decomposition ",
          "step in train(); any selection done outside rtemis must use only ",
          "the training cases of each split."
        )
      )
    )
  }
  findings
} # /rtemis::review_sample_findings


# %% review_gap_finding ----
#' Generalization gap finding (single split)
#'
#' A diagnostic heuristic, not a test of the train-test difference: it
#' compares the training value of the headline metric with the test interval.
#'
#' @param x `Supervised` object.
#' @param training Numeric: Training value of the headline metric.
#' @param interval Numeric vector: Estimate, lower and upper bound of the test
#'   headline metric.
#' @param higher_is_better Logical: Direction of the headline metric.
#' @param headline_label Character: Label of the headline metric.
#' @param level Numeric: Confidence level.
#'
#' @return List of zero or one `ReviewFinding`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_gap_finding <- function(
  x,
  training,
  interval,
  higher_is_better,
  headline_label,
  level
) {
  if (!review_finite(c(training, interval))) {
    return(list())
  }
  beyond <- if (higher_is_better) {
    training > interval[[3L]]
  } else {
    training < interval[[2L]]
  }
  if (!beyond) {
    return(list())
  }
  list(new_review_finding(
    code = "GENERALIZATION_GAP",
    severity = "warning",
    message = paste0(
      "Training ",
      headline_label,
      " of ",
      fmt_review_num(training),
      if (higher_is_better) " lies above the " else " lies below the ",
      "test ",
      fmt_review_interval(interval[2:3], level),
      ". Training performance is better than held-out performance; this may ",
      "indicate overfitting, and differences between the training and test ",
      "cases may also contribute."
    ),
    suggestion = review_gap_suggestion(x)
  ))
} # /rtemis::review_gap_finding


# %% review_prediction_findings ----
#' Constant predictions and never-predicted classes
#'
#' Observations about these test predictions. Constant predictions are an
#' observation only with at least two test cases whose outcomes vary; for
#' classification,
#' constant labels with varying probabilities point at the decision
#' threshold rather than at the scores.
#'
#' @param y Test outcome, pooled.
#' @param predicted Test predictions, pooled.
#' @param prob Optional pooled positive-class probabilities.
#' @param classification Logical.
#'
#' @return List of `ReviewFinding`, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_prediction_findings <- function(y, predicted, prob, classification) {
  # With one test case, or an outcome that does not vary among the test
  # cases, identical predictions are expected and say nothing about the model.
  if (length(y) < 2L || length(unique(y)) < 2L) {
    return(list())
  }
  if (classification) {
    lv <- levels(y)
    predicted_levels <- unique(as.character(predicted))
    if (length(predicted_levels) == 1L) {
      scores_vary <- !is.null(prob) && length(unique(prob)) > 1L
      return(list(new_review_finding(
        code = "CONSTANT_PREDICTIONS",
        severity = "warning",
        message = paste0(
          "All ",
          length(y),
          " test cases were predicted as '",
          predicted_levels,
          "'.",
          if (scores_vary) {
            paste0(
              " The predicted probabilities varied, so the decision ",
              "threshold placed every case in one class."
            )
          }
        ),
        suggestion = if (scores_vary) {
          paste0(
            "Check the decision threshold and the class balance."
          )
        } else {
          paste0(
            "Check the outcome, the class balance and the hyperparameters."
          )
        }
      )))
    }
    never <- lv[lv %in% as.character(y) & !(lv %in% predicted_levels)]
    if (length(never) > 0L) {
      return(list(new_review_finding(
        code = "CLASS_NEVER_PREDICTED",
        severity = "warning",
        message = paste0(
          ngettext(length(never), "Class ", "Classes "),
          paste0("'", never, "'", collapse = ", "),
          ngettext(length(never), " occurs", " occur"),
          " among the test cases but ",
          ngettext(length(never), "was", "were"),
          " never predicted."
        ),
        suggestion = paste0(
          "Consider class weights, resampling the rarer classes, or ",
          "adjusting the decision threshold."
        )
      )))
    }
    return(list())
  }
  if (length(unique(predicted)) == 1L) {
    return(list(new_review_finding(
      code = "CONSTANT_PREDICTIONS",
      severity = "warning",
      message = paste0(
        "All ",
        length(y),
        " test predictions were ",
        fmt_review_num(predicted[[1L]]),
        "."
      ),
      suggestion = "Check the outcome and the hyperparameters."
    )))
  }
  list()
} # /rtemis::review_prediction_findings


# %% review_tuned ----
review_tuned <- function(x) {
  if (S7_inherits(x, SupervisedRes)) {
    !is.null(x@tuner_config)
  } else {
    !is.null(x@tuner)
  }
} # /rtemis::review_tuned


# %% review_gap_suggestion ----
review_gap_suggestion <- function(x) {
  if (!review_tuned(x)) {
    paste0(
      "Tune the hyperparameters that control model complexity -- for example ",
      "regularization strength, tree depth, minimum node size or the number ",
      "of boosting iterations -- with resampling, or choose a simpler model."
    )
  } else {
    paste0(
      "Hyperparameters were tuned. Check whether the selected values lie at ",
      "the edge of the searched range, and extend the search toward simpler ",
      "models."
    )
  }
} # /rtemis::review_gap_suggestion


# %% review_tuning ----
#' Tuning table and grid-edge findings
#'
#' A selected value at the smallest or largest value searched suggests the
#' best value may lie beyond the grid. Only numeric hyperparameters searched
#' over at least three values are checked, because with two every choice is
#' an edge; an edge that is also a bound the hyperparameter declares cannot be
#' extended and is not reported.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#'
#' @return List with `table` (data.frame, or NULL for an untuned model or no
#'   checked hyperparameter) and `findings`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_tuning <- function(x) {
  resampled <- S7_inherits(x, SupervisedRes)
  models <- if (resampled) x@models else list(x)
  tuners <- Filter(Negate(is.null), lapply(models, function(m) m@tuner))
  if (length(tuners) == 0L) {
    return(list(table = NULL, findings = list()))
  }
  grid <- tuners[[1L]]@tuning_results[["param_grid"]]
  hp_class <- S7_class(tuners[[1L]]@hyperparameters)
  rows <- list()
  findings <- list()
  for (name in names(tuners[[1L]]@best_hyperparameters)) {
    values <- grid[[name]]
    if (!is.numeric(values)) {
      next
    }
    values <- sort(unique(values))
    if (length(values) < 3L) {
      next
    }
    selected <- vapply(
      tuners,
      function(t) {
        value <- t@best_hyperparameters[[name]]
        if (is.numeric(value) && length(value) == 1L) value else NA_real_
      },
      numeric(1L)
    )
    spec <- if (name %in% names(hp_class@properties)) {
      get_spec_fields(hp_class@properties[[name]])
    } else {
      list()
    }
    lowest <- values[[1L]]
    highest <- values[[length(values)]]
    at_bound <- function(value, bound) !is.null(bound) && value == bound
    low_extendable <- !at_bound(lowest, spec[["minimum"]]) &&
      !at_bound(lowest, spec[["exclusive_minimum"]])
    high_extendable <- !at_bound(highest, spec[["maximum"]]) &&
      !at_bound(highest, spec[["exclusive_maximum"]])
    at_edge <- (low_extendable & selected == lowest) |
      (high_extendable & selected == highest)
    at_edge[is.na(at_edge)] <- FALSE
    rows[[length(rows) + 1L]] <- data.frame(
      hyperparameter = name,
      min = lowest,
      max = highest,
      n_values = length(values),
      selected = if (resampled) NA_real_ else selected[[1L]],
      n_at_edge = sum(at_edge)
    )
    if (any(at_edge)) {
      findings <- c(
        findings,
        new_review_finding(
          code = "TUNING_GRID_EDGE",
          severity = "note",
          message = paste0(
            if (resampled) {
              paste0(
                sum(at_edge),
                " of ",
                length(at_edge),
                " resamples selected ",
                name,
                " at the edge of the values searched"
              )
            } else {
              paste0(
                "The selected ",
                name,
                " of ",
                fmt_review_num(selected[[1L]]),
                " is at the edge of the values searched"
              )
            },
            " (",
            fmt_review_num(lowest),
            " to ",
            fmt_review_num(highest),
            "); the best value may lie beyond it."
          ),
          suggestion = paste0(
            "Extend the search range of ",
            name,
            " past that edge."
          )
        )
      )
    }
  }
  list(
    table = if (length(rows) > 0L) do.call(rbind, rows),
    findings = findings
  )
} # /rtemis::review_tuning


# %% review.Supervised ----
method(review, Supervised) <- function(
  x,
  confidence_level = NULL,
  min_cases_per_predictor = NULL,
  ...
) {
  review_supervised(x, confidence_level, min_cases_per_predictor)
} # /rtemis::review.Supervised


# %% review.SupervisedRes ----
method(review, SupervisedRes) <- function(
  x,
  confidence_level = NULL,
  min_cases_per_predictor = NULL,
  ...
) {
  review_supervised(x, confidence_level, min_cases_per_predictor)
} # /rtemis::review.SupervisedRes


# %% review_supervised ----
#' Review a single-split or resampled supervised model
#'
#' A single split is reviewed as one fold, a resampled model as one fold per
#' successful outer resample; the out-of-sample predictions of all folds are
#' pooled for intervals and baseline comparisons when the test sets do not
#' overlap.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param confidence_level Optional Numeric (0, 1): Confidence level.
#' @param min_cases_per_predictor Optional Numeric (0, Inf): Threshold.
#'
#' @return `SupervisedReview` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_supervised <- function(x, confidence_level, min_cases_per_predictor) {
  confidence_level <- confidence_level %||% 0.95
  min_cases_per_predictor <- min_cases_per_predictor %||% 10
  check_unit_open_scalar(confidence_level)
  check_pos_double_scalar(min_cases_per_predictor)
  review_body(
    x,
    review_folds(x),
    level = confidence_level,
    min_cases_per_predictor = min_cases_per_predictor
  )
} # /rtemis::review_supervised


# %% review_body ----
#' Assemble a review from folds
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param level Numeric (0, 1): Confidence level.
#' @param min_cases_per_predictor Numeric: Threshold.
#'
#' @return `SupervisedReview` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_body <- function(x, folds, level, min_cases_per_predictor) {
  classification <- x@type == "Classification"
  resampled <- S7_inherits(x, SupervisedRes)
  has_test <- !is.null(folds[[1L]][["y_test"]])
  intervals <- NULL
  pooled <- NULL
  class_counts <- NULL
  absent <- NULL
  prob <- NULL
  binclasspos <- NULL

  # Pooled test outcomes and predictions ----
  if (has_test) {
    y <- review_pool(folds, "y_test")
    predicted <- review_pool(folds, "predicted_test")
  }
  if (classification) {
    lv <- levels(folds[[1L]][["y_training"]])
    binclasspos <- if (resampled) x@models[[1L]]@binclasspos else x@binclasspos
    counts_training <- matrix(
      vapply(
        folds,
        function(f) {
          as.vector(table(factor(f[["y_training_cases"]], levels = lv)))
        },
        integer(length(lv))
      ),
      nrow = length(lv)
    )
    minority <- apply(counts_training, 2L, min)
    class_counts <- data.frame(
      level = lv,
      training = apply(counts_training, 1L, min),
      test = if (has_test) {
        vapply(lv, function(l) sum(y == l), integer(1L), USE.NAMES = FALSE)
      } else {
        NA_integer_
      }
    )
    if (has_test) {
      if (length(lv) == 2L) {
        prob <- review_pool(folds, "prob_test")
        if (length(prob) != length(y)) {
          prob <- NULL
        }
      }
      missing_class <- vapply(
        folds,
        function(f) any(!lv %in% as.character(f[["y_test"]])),
        logical(1L)
      )
      if (any(missing_class)) {
        absent <- if (resampled) {
          sum(missing_class)
        } else {
          lv[!lv %in% as.character(y)]
        }
      }
    }
    headline <- "balanced_accuracy"
    headline_label <- "balanced accuracy"
    higher_is_better <- TRUE
  } else {
    minority <- vapply(
      folds,
      function(f) length(f[["y_training_cases"]]),
      integer(1L)
    )
    headline <- "mse"
    headline_label <- "mean squared error"
    higher_is_better <- FALSE
  }
  sample <- review_sample(x, folds, minority)

  # Single split: intervals. Resampled: pooled values, descriptive ----
  if (has_test) {
    if (!resampled) {
      intervals <- if (classification) {
        review_classification_intervals(y, predicted, prob, binclasspos, level)
      } else {
        review_regression_intervals(y, predicted, level)
      }
    } else if (sample[["n_test_cases"]] == sample[["n_test"]]) {
      pooled <- review_pooled(y, predicted, prob, binclasspos)
    }
  }
  performance <- review_performance(folds, intervals, pooled)

  # Baseline ----
  baseline <- NULL
  findings_baseline <- list()
  findings_predictions <- list()
  folds_better <- NULL
  if (has_test) {
    compared <- review_baseline(x, folds, intervals, level, binclasspos)
    baseline <- compared[["table"]]
    findings_baseline <- compared[["findings"]]
    folds_better <- compared[["folds_better"]]
    findings_predictions <- review_prediction_findings(
      y,
      predicted,
      prob,
      classification
    )
  }

  # Findings ----
  sample_findings <- review_sample_findings(
    x,
    sample,
    list(
      has_test = has_test,
      headline_label = headline_label,
      headline_interval = intervals[[headline]],
      fold_test = if (has_test) {
        vapply(
          folds,
          function(f) f[["metrics_test"]][[headline]] %||% NA_real_,
          numeric(1L)
        )
      },
      folds_better = folds_better,
      absent = absent,
      min_cases_per_predictor = min_cases_per_predictor,
      level = level
    )
  )
  findings_gap <- list()
  if (has_test && !resampled) {
    findings_gap <- review_gap_finding(
      x,
      training = performance[["training"]][performance[["metric"]] == headline],
      interval = intervals[[headline]],
      higher_is_better = higher_is_better,
      headline_label = headline_label,
      level = level
    )
  }
  tuning <- review_tuning(x)

  SupervisedReview(
    algorithm = x@algorithm,
    type = x@type,
    description = desc(x),
    confidence_level = level,
    min_cases_per_predictor = min_cases_per_predictor,
    sample = sample,
    class_counts = class_counts,
    performance = performance,
    baseline = baseline,
    tuning = tuning[["table"]],
    findings = c(
      sample_findings,
      findings_predictions,
      findings_baseline,
      findings_gap,
      tuning[["findings"]]
    )
  )
} # /rtemis::review_body


# %% review_baseline_row ----
# One row of the baseline table; unset cells are NA.
review_baseline_row <- function(
  metric,
  reference,
  method,
  model = NA_real_,
  model_interval = c(NA_real_, NA_real_),
  baseline = NA_real_,
  difference = c(NA_real_, NA_real_, NA_real_),
  skill = NA_real_,
  p_value = NA_real_,
  outcome = NA_character_,
  resamples_better = NA_integer_
) {
  nan_to_na <- review_defined
  data.frame(
    metric = metric,
    reference = reference,
    method = method,
    model = nan_to_na(model),
    model_lower = nan_to_na(model_interval[[1L]]),
    model_upper = nan_to_na(model_interval[[2L]]),
    baseline = nan_to_na(baseline),
    difference = nan_to_na(difference[[1L]]),
    difference_lower = nan_to_na(difference[[2L]]),
    difference_upper = nan_to_na(difference[[3L]]),
    skill = nan_to_na(skill),
    p_value = p_value,
    outcome = outcome,
    resamples_better = resamples_better
  )
} # /rtemis::review_baseline_row


# %% review_fold_baseline ----
#' One fold's baseline predictions and per-case results
#'
#' The baseline is fit to the fold's training data as the model saw it: its
#' most common class (classification) or mean outcome (regression).
#'
#' @param f One fold from `review_folds()`.
#' @param binclasspos Optional Integer: Position of the positive level.
#'
#' @return List of per-case and per-fold values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_fold_baseline <- function(f, binclasspos) {
  y <- f[["y_test"]]
  if (is.factor(y)) {
    lv <- levels(y)
    majority <- lv[[which.max(table(factor(f[["y_training"]], levels = lv)))]]
    present <- lv[lv %in% as.character(y)]
    out <- list(
      majority = majority,
      model_correct = as.character(f[["predicted_test"]]) == as.character(y),
      baseline_correct = as.character(y) == majority,
      # Any constant prediction recalls one present class fully and the others
      # not at all; with every class present, its balanced accuracy is 1/K.
      chance = if (length(present) == length(lv)) 1 / length(lv) else NA_real_
    )
    prob <- f[["prob_test"]]
    if (!is.null(prob) && length(lv) == 2L && length(prob) == length(y)) {
      positive_level <- lv[[binclasspos]]
      y01 <- as.numeric(y == positive_level)
      prevalence <- mean(f[["y_training"]] == positive_level)
      out[["positive_level"]] <- positive_level
      out[["brier"]] <- (y01 - prob)^2
      out[["brier_baseline"]] <- (y01 - prevalence)^2
    }
    out
  } else {
    training_mean <- mean(f[["y_training"]])
    list(
      errors = y - f[["predicted_test"]],
      baseline_errors = y - training_mean,
      rsq_baseline = 1 -
        sum((y - training_mean)^2) / sum((y - mean(y))^2)
    )
  }
} # /rtemis::review_fold_baseline


# %% review_baseline ----
#' Baseline table and findings
#'
#' A single split is compared with inference: the exact McNemar test for
#' accuracy, the interval of balanced accuracy and AUC against their chance
#' level, and paired t intervals of the per-case loss reduction for the Brier
#' score, MSE and MAE. Each verdict is two-sided at the review's confidence
#' level. A resampled model is described: the mean over resamples of the
#' model's and the baseline's values, their mean difference, and the number
#' of resamples where the model did better; no verdict, the resamples being
#' dependent.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param intervals Optional named list from `review_*_intervals()` (single
#'   split).
#' @param level Numeric: Confidence level.
#' @param binclasspos Optional Integer: Position of the positive level.
#'
#' @return List with `table`, `findings` and `folds_better` (headline count,
#'   or NULL for a single split).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline <- function(x, folds, intervals, level, binclasspos) {
  resampled <- S7_inherits(x, SupervisedRes)
  fb <- lapply(folds, review_fold_baseline, binclasspos = binclasspos)
  fold_metric <- function(name) {
    vapply(
      folds,
      function(f) f[["metrics_test"]][[name]] %||% NA_real_,
      numeric(1L)
    )
  }
  mean_available <- function(v) {
    if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  }
  count_better <- function(model, baseline, higher_is_better = TRUE) {
    better <- if (higher_is_better) model > baseline else model < baseline
    as.integer(sum(better, na.rm = TRUE))
  }
  if (resampled) {
    review_baseline_resampled(
      x,
      folds,
      fb,
      fold_metric,
      mean_available,
      count_better
    )
  } else {
    review_baseline_single(x, folds[[1L]], fb[[1L]], intervals, level)
  }
} # /rtemis::review_baseline


# %% review_baseline_resampled ----
#' Descriptive baseline comparisons for a resampled model
#'
#' @return List with `table`, `findings` (empty) and `folds_better`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline_resampled <- function(
  x,
  folds,
  fb,
  fold_metric,
  mean_available,
  count_better
) {
  per_fold <- function(name, fn = mean) {
    vapply(fb, function(b) fn(b[[name]]), numeric(1L))
  }
  row <- function(metric, reference, model, baseline, higher_is_better = TRUE) {
    available <- !is.na(model) & !is.na(baseline)
    review_baseline_row(
      metric,
      reference = reference,
      method = "descriptive",
      model = mean_available(model),
      baseline = mean_available(baseline),
      difference = c(
        if (any(available)) {
          mean(
            if (higher_is_better) {
              model[available] - baseline[available]
            } else {
              baseline[available] - model[available]
            }
          )
        } else {
          NA_real_
        },
        NA_real_,
        NA_real_
      ),
      resamples_better = count_better(model, baseline, higher_is_better)
    )
  }
  if (x@type == "Classification") {
    rows <- list(
      row(
        "accuracy",
        "majority_class",
        fold_metric("accuracy"),
        per_fold("baseline_correct")
      ),
      row(
        "balanced_accuracy",
        "chance",
        fold_metric("balanced_accuracy"),
        vapply(fb, `[[`, numeric(1L), "chance")
      )
    )
    auc <- fold_metric("auc")
    if (!all(is.na(auc))) {
      rows <- c(rows, list(row("auc", "chance", auc, rep(0.5, length(auc)))))
    }
    if (!is.null(fb[[1L]][["brier"]])) {
      brier <- per_fold("brier")
      brier_baseline <- per_fold("brier_baseline")
      brier_row <- row(
        "brier_score",
        "training_prevalence",
        brier,
        brier_baseline,
        higher_is_better = FALSE
      )
      brier_row[["skill"]] <- 1 - mean(brier) / mean(brier_baseline)
      rows <- c(rows, list(brier_row))
    }
    folds_better <- rows[[2L]][["resamples_better"]]
  } else {
    mse <- vapply(fb, function(b) mean(b[["errors"]]^2), numeric(1L))
    mse_baseline <- vapply(
      fb,
      function(b) mean(b[["baseline_errors"]]^2),
      numeric(1L)
    )
    mae <- vapply(fb, function(b) mean(abs(b[["errors"]])), numeric(1L))
    mae_baseline <- vapply(
      fb,
      function(b) mean(abs(b[["baseline_errors"]])),
      numeric(1L)
    )
    mse_row <- row(
      "mse",
      "training_mean",
      mse,
      mse_baseline,
      higher_is_better = FALSE
    )
    mse_row[["skill"]] <- 1 - mean(mse) / mean(mse_baseline)
    mae_row <- row(
      "mae",
      "training_mean",
      mae,
      mae_baseline,
      higher_is_better = FALSE
    )
    mae_row[["skill"]] <- 1 - mean(mae) / mean(mae_baseline)
    rows <- list(
      mse_row,
      mae_row,
      row(
        "rsq",
        "training_mean",
        fold_metric("rsq"),
        vapply(fb, `[[`, numeric(1L), "rsq_baseline")
      )
    )
    folds_better <- mse_row[["resamples_better"]]
  }
  list(
    table = do.call(rbind, rows),
    findings = list(),
    folds_better = folds_better
  )
} # /rtemis::review_baseline_resampled


# %% review_baseline_single ----
#' Baseline comparisons with inference for a single split
#'
#' @return List with `table`, `findings` and `folds_better` (NULL).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline_single <- function(x, fold, b, intervals, level) {
  alpha <- 1 - level
  rows <- list()
  findings <- list()
  add <- function(row, finding = NULL) {
    rows[[length(rows) + 1L]] <<- row
    if (!is.null(finding)) {
      findings[[length(findings) + 1L]] <<- finding
    }
  }
  if (x@type == "Classification") {
    # Accuracy: exact McNemar ----
    test <- review_mcnemar(b[["model_correct"]], b[["baseline_correct"]])
    accuracy <- intervals[["accuracy"]]
    baseline_accuracy <- mean(b[["baseline_correct"]])
    outcome <- if (test[["p_value"]] < alpha) {
      if (test[["b"]] > test[["c"]]) "better" else "worse"
    } else {
      "indistinguishable"
    }
    add(
      review_baseline_row(
        "accuracy",
        reference = "majority_class",
        method = "exact_mcnemar",
        model = accuracy[[1L]],
        model_interval = accuracy[2:3],
        baseline = baseline_accuracy,
        difference = c(accuracy[[1L]] - baseline_accuracy, NA_real_, NA_real_),
        p_value = test[["p_value"]],
        outcome = outcome
      ),
      review_baseline_finding(
        "BASELINE_ACCURACY",
        outcome,
        what = paste0(
          "Test accuracy of ",
          fmt_review_num(accuracy[[1L]]),
          " (",
          test[["b"]],
          " cases correct only for the model, ",
          test[["c"]],
          " only for the baseline; exact McNemar p = ",
          fmt_review_num(test[["p_value"]]),
          ")"
        ),
        baseline_text = paste0(
          "the ",
          fmt_review_num(baseline_accuracy),
          " of always predicting '",
          b[["majority"]],
          "', the most common training class"
        )
      )
    )

    # Balanced accuracy and AUC: interval against chance ----
    against_chance <- function(metric, chance, code, label) {
      interval <- intervals[[metric]]
      usable <- review_finite(interval) && !is.na(chance)
      outcome <- if (usable) {
        review_interval_outcome(interval[2:3], chance)
      } else {
        NA_character_
      }
      add(
        review_baseline_row(
          metric,
          reference = "chance",
          method = "interval_vs_reference",
          model = interval[[1L]],
          model_interval = interval[2:3],
          baseline = chance,
          difference = interval - chance,
          outcome = outcome
        ),
        if (usable) {
          review_baseline_finding(
            code,
            outcome,
            what = paste0(
              "Test ",
              label,
              " of ",
              fmt_review_num(interval[[1L]]),
              " (",
              fmt_review_interval(interval[2:3], level),
              ")"
            ),
            baseline_text = paste0(
              "the chance level of ",
              fmt_review_num(chance)
            )
          )
        }
      )
    }
    against_chance(
      "balanced_accuracy",
      b[["chance"]],
      "BASELINE_BALANCED_ACCURACY",
      "balanced accuracy"
    )
    if (!is.null(intervals[["auc"]])) {
      against_chance("auc", 0.5, "BASELINE_AUC", "AUC")
    }

    # Brier score: paired loss reduction ----
    if (!is.null(b[["brier"]])) {
      add_loss_row(
        add,
        metric = "brier_score",
        reference = "training_prevalence",
        loss = b[["brier"]],
        loss_baseline = b[["brier_baseline"]],
        level = level,
        code = "BASELINE_BRIER",
        label = "Brier score",
        baseline_text = paste0(
          "predicting the training proportion of '",
          b[["positive_level"]],
          "' for every case"
        )
      )
    }
  } else {
    add_loss_row(
      add,
      metric = "mse",
      reference = "training_mean",
      loss = b[["errors"]]^2,
      loss_baseline = b[["baseline_errors"]]^2,
      level = level,
      code = "BASELINE_MSE",
      label = "mean squared error",
      baseline_text = "predicting the training mean for every case"
    )
    add_loss_row(
      add,
      metric = "mae",
      reference = "training_mean",
      loss = abs(b[["errors"]]),
      loss_baseline = abs(b[["baseline_errors"]]),
      level = level,
      code = NULL
    )
    add(review_baseline_row(
      "rsq",
      reference = "training_mean",
      method = "descriptive",
      model = fold[["metrics_test"]][["rsq"]],
      baseline = b[["rsq_baseline"]],
      difference = c(
        fold[["metrics_test"]][["rsq"]] - b[["rsq_baseline"]],
        NA_real_,
        NA_real_
      )
    ))
  }
  list(table = do.call(rbind, rows), findings = findings, folds_better = NULL)
} # /rtemis::review_baseline_single


# %% add_loss_row ----
#' Add a loss comparison to a single-split baseline table
#'
#' @param add Function: The table and findings accumulator.
#' @param metric Character: Metric name.
#' @param reference Character: Baseline reference.
#' @param loss,loss_baseline Numeric: Per-case losses.
#' @param level Numeric: Confidence level.
#' @param code Optional Character: Finding code; NULL adds no finding.
#' @param label Character: Metric label for the message.
#' @param baseline_text Character: The baseline, for the message.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
add_loss_row <- function(
  add,
  metric,
  reference,
  loss,
  loss_baseline,
  level,
  code,
  label = NULL,
  baseline_text = NULL
) {
  difference <- review_loss_difference(loss, loss_baseline, level)
  usable <- review_finite(difference)
  outcome <- if (usable) {
    review_interval_outcome(difference[2:3], 0)
  } else {
    NA_character_
  }
  skill <- if (mean(loss_baseline) > 0) {
    1 - mean(loss) / mean(loss_baseline)
  } else {
    NA_real_
  }
  add(
    review_baseline_row(
      metric,
      reference = reference,
      method = "paired_t",
      model = mean(loss),
      baseline = mean(loss_baseline),
      difference = difference,
      skill = skill,
      outcome = outcome
    ),
    if (usable && !is.null(code)) {
      review_baseline_finding(
        code,
        outcome,
        what = paste0(
          "Test ",
          label,
          " of ",
          fmt_review_num(mean(loss)),
          " (mean reduction from the baseline's ",
          fmt_review_num(mean(loss_baseline)),
          ": ",
          fmt_review_num(difference[[1L]]),
          ", ",
          fmt_review_interval(difference[2:3], level),
          ")"
        ),
        baseline_text = baseline_text
      )
    }
  )
  invisible(NULL)
} # /rtemis::add_loss_row
