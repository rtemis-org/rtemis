# review.R
# ::rtemis::
# 2026- EDG rtemis.org

# `review()` for a trained supervised model, single-split or resampled. The
# classes and the finding vocabulary live in `280_SupervisedReview.R`.
#
# A single split is one fold and a resampled model one fold per successful
# outer resample, so both take one path. Out-of-sample predictions are pooled
# across folds for intervals and baseline comparisons, which is valid when each
# case is tested once (k-fold); when test sets overlap (repeated or bootstrap
# resampling) no pooled interval is computed and the review describes the
# variation between resamples instead.
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
#' collapsing to a point when a recall is 0 or 1.
#'
#' @param hits Integer vector: Correct predictions per class.
#' @param totals Integer vector: Test cases per class; classes absent from the
#'   test set are dropped.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 2, clipped to \[0, 1\].
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_balanced_accuracy_interval <- function(hits, totals, level) {
  present <- totals > 0L
  hits <- hits[present]
  totals <- totals[present]
  k <- length(totals)
  if (k == 0L) {
    return(c(NA_real_, NA_real_))
  }
  estimate <- mean(hits / totals)
  adjusted <- (hits + 1) / (totals + 2)
  se <- sqrt(sum(adjusted * (1 - adjusted) / totals)) / k
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(max(0, estimate - z * se), min(1, estimate + z * se))
} # /rtemis::review_balanced_accuracy_interval


# %% review_auc_delong ----
#' AUC and its DeLong interval
#'
#' DeLong, DeLong and Clarke-Pearson (1988), with midranks for ties.
#'
#' @param prob Numeric vector: Predicted probability of the positive class.
#' @param positive Logical vector: Whether each case is positive.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 3: AUC, lower and upper bound, the bounds
#'   clipped to \[0, 1\]; `NA` with fewer than two cases in either class.
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
  z <- stats::qnorm(1 - (1 - level) / 2)
  c(estimate, max(0, estimate - z * se), min(1, estimate + z * se))
} # /rtemis::review_auc_delong


# %% review_mean_interval ----
#' t interval for a mean
#'
#' @param v Numeric vector.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 2; `NA` with fewer than two values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_mean_interval <- function(v, level) {
  n <- length(v)
  if (n < 2L) {
    return(c(NA_real_, NA_real_))
  }
  half <- stats::qt(1 - (1 - level) / 2, df = n - 1L) * stats::sd(v) / sqrt(n)
  mean(v) + c(-half, half)
} # /rtemis::review_mean_interval


# %% review_skill ----
#' Skill score against a baseline, with a paired interval
#'
#' Skill is `1 - mean(loss) / mean(loss_baseline)`, the mean per-case loss
#' reduction over the baseline's mean loss. Its interval is the paired t
#' interval of the per-case reduction, on the same scale.
#'
#' @param loss Numeric vector: Per-case loss of the model.
#' @param loss_baseline Numeric vector: Per-case loss of the baseline.
#' @param level Numeric (0, 1): Confidence level.
#'
#' @return Numeric vector of length 3: skill, lower and upper bound, the upper
#'   bound clipped to 1, a loss being nonnegative; `NA` when the baseline has
#'   no loss.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_skill <- function(loss, loss_baseline, level) {
  scale <- mean(loss_baseline)
  if (!is.finite(scale) || scale == 0) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  reduction <- loss_baseline - loss
  out <- c(mean(reduction), review_mean_interval(reduction, level)) / scale
  out[[3L]] <- min(1, out[[3L]])
  out
} # /rtemis::review_skill


# %% review_predictors ----
#' Predictors the learner sees, and what counted them
#'
#' @param x `Supervised` object.
#'
#' @return List with `p` (input columns), `k` (decomposition components or
#'   NULL), `effective` (what the learner sees) and `decomposition` (algorithm
#'   or NULL).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_predictors <- function(x) {
  p <- length(x@xnames)
  k <- NULL
  decomposition <- NULL
  if (!is.null(x@decomposition)) {
    k <- x@decomposition@config[["k"]]
    decomposition <- x@decomposition@algorithm
  }
  list(
    p = p,
    k = k,
    effective = k %||% p,
    decomposition = decomposition
  )
} # /rtemis::review_predictors


# %% fmt_review_num ----
fmt_review_num <- function(x) {
  ddSci(x, decimal_places = 3L)
}


# %% fmt_review_interval ----
fmt_review_interval <- function(interval, level) {
  paste0(
    round(level * 100),
    "% CI ",
    fmt_review_num(interval[[1L]]),
    " to ",
    fmt_review_num(interval[[2L]])
  )
}


# %% review_baseline_outcome ----
#' Compare an interval with a baseline value
#'
#' @param interval Numeric vector of length 2, oriented so that higher is
#'   better.
#' @param baseline Numeric: Baseline value on the same scale.
#'
#' @return Character: One of `REVIEW_BASELINE_OUTCOMES`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline_outcome <- function(interval, baseline) {
  if (interval[[1L]] > baseline) {
    "better"
  } else if (interval[[2L]] < baseline) {
    "worse"
  } else {
    "indistinguishable"
  }
} # /rtemis::review_baseline_outcome


# %% review_baseline_finding ----
#' A baseline comparison finding
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
    message = paste0(
      what,
      switch(
        outcome,
        better = " is better than ",
        worse = " is worse than ",
        indistinguishable = " cannot be distinguished from "
      ),
      baseline_text,
      "."
    )
  )
} # /rtemis::review_baseline_finding


# %% review_folds ----
#' The evaluation units of a model, in one shape
#'
#' A single-split model is one fold; a resampled model has one per successful
#' outer resample. Each fold carries its training outcome as the model saw it
#' and with each case once, its test outcome, test
#' predictions, positive-class test probabilities (binary classification only)
#' and its training and test metrics as one-row data.frames.
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
      metrics_test = one_row(m@metrics_test)
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
#' Test estimates and intervals for classification metrics
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
  with_estimate <- function(x, n, interval) c(x / n, interval)
  out <- list(
    accuracy = with_estimate(
      sum(hits),
      length(y),
      review_binom_interval(sum(hits), length(y), level)
    ),
    balanced_accuracy = c(
      mean((hits / totals)[totals > 0L]),
      review_balanced_accuracy_interval(hits, totals, level)
    )
  )
  if (length(lv) == 2L) {
    pos <- binclasspos
    neg <- 3L - pos
    proportion <- function(x, n) {
      c(if (n > 0L) x / n else NA_real_, review_binom_interval(x, n, level))
    }
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
#' Test estimates and intervals for regression metrics
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


# %% review_performance ----
#' One row per metric: training, test, their difference, and the interval
#'
#' One fold gives training, test and their difference. Several folds give the
#' mean and standard deviation of each over resamples, the difference of the
#' means, and the pooled out-of-sample value. The interval is of the test
#' value for one fold and of the pooled value for several.
#'
#' @param folds List from `review_folds()`.
#' @param intervals Optional named list from `review_*_intervals()`.
#'
#' @return data.frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_performance <- function(folds, intervals) {
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
    data.frame(
      metric = metric,
      training = mean(training),
      training_sd = if (resampled) stats::sd(training) else NA_real_,
      test = mean(test),
      test_sd = if (resampled && has_test) stats::sd(test) else NA_real_,
      difference = mean(training) - mean(test),
      pooled = if (resampled) interval[[1L]] else NA_real_,
      lower = interval[[2L]],
      upper = interval[[3L]]
    )
  })
  do.call(rbind, rows)
} # /rtemis::review_performance


# %% review_sample_findings ----
#' Sample-size, resampling and dimensionality findings
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param sample List: The review's `sample`.
#' @param context List: `has_test`, `intervals` (logical: whether intervals
#'   were computed), `headline_label`, `headline_interval` (estimate, lower,
#'   upper), `fold_test` (headline per fold), `folds_better` (count),
#'   `min_cases_per_predictor`, `level`.
#'
#' @return List with `findings` (list of `ReviewFinding`) and `dim_p_gt_n`
#'   (logical).
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
          "The model was evaluated on its ",
          n_training,
          " training cases only; generalization cannot be assessed."
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
          "Performance was estimated on a single split of ",
          sample[["n_test"]],
          " test cases. The test intervals reflect the number of test cases, ",
          "not how much the estimate would change with a different split."
        ),
        suggestion = paste0(
          "Use outer resampling to test on every case and to measure ",
          "variation between splits; repeat it when cases are few."
        )
      )
    )
  }
  if (context[["has_test"]] && context[["intervals"]]) {
    interval <- context[["headline_interval"]]
    findings <- c(
      findings,
      new_review_finding(
        code = "TEST_PRECISION",
        severity = "note",
        message = if (resampled) {
          paste0(
            "Pooled out-of-sample ",
            context[["headline_label"]],
            " is ",
            fmt_review_num(interval[[1L]]),
            " (",
            fmt_review_interval(interval[2:3], level),
            "), from ",
            sample[["n_test"]],
            " predictions over ",
            sample[["n_resamples"]],
            " resamples. The interval reflects the number of cases, not the ",
            "variation between resamples."
          )
        } else {
          paste0(
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
        }
      )
    )
  }
  if (resampled && context[["has_test"]]) {
    if (!context[["intervals"]]) {
      findings <- c(
        findings,
        new_review_finding(
          code = "OVERLAPPING_TEST_SETS",
          severity = "note",
          message = paste0(
            "The test sets overlap: ",
            sample[["n_test"]],
            " predictions were made for ",
            sample[["n_test_cases"]],
            " distinct cases. Repeated predictions of a case are not ",
            "independent, so no pooled interval is computed and the ",
            "comparisons with the baseline and with training performance are ",
            "not tested."
          ),
          suggestion = paste0(
            "Use k-fold resampling, which tests every case once, to obtain ",
            "intervals."
          )
        )
      )
    }
    fold_test <- context[["fold_test"]]
    findings <- c(
      findings,
      new_review_finding(
        code = "FOLD_VARIATION",
        severity = "note",
        message = paste0(
          "Test ",
          context[["headline_label"]],
          " ranged from ",
          fmt_review_num(min(fold_test)),
          " to ",
          fmt_review_num(max(fold_test)),
          " across ",
          sample[["n_resamples"]],
          " resamples (mean ",
          fmt_review_num(mean(fold_test)),
          ", SD ",
          fmt_review_num(stats::sd(fold_test)),
          "); ",
          context[["folds_better"]],
          " of ",
          sample[["n_resamples"]],
          " resamples outperformed their baseline."
        )
      )
    )
  }

  # Dimensionality ----
  handles <- algorithm_handles_p_gt_n(x@algorithm)
  dim_severity <- if (isFALSE(handles)) "warning" else "note"
  dim_suggestion <- paste0(
    "Judge the model by test performance only, estimated with resampling. ",
    "Prefer a regularized or sparse algorithm, or reduce the predictors inside ",
    "the training pipeline (a decomposition or feature selection fitted on ",
    "training cases only)."
  )
  effective <- sample[["n_components"]] %||% sample[["n_predictors"]]
  seen <- if (is.null(sample[["n_components"]])) {
    paste0("at least ", effective, " predictors")
  } else {
    paste0(effective, " components from ", sample[["decomposition"]])
  }
  training_cases <- if (resampled) {
    paste0(n_training, " training cases in its smallest resample")
  } else {
    paste0(n_training, " training cases")
  }
  dim_p_gt_n <- effective > n_training
  few_cases <- !dim_p_gt_n &&
    sample[["cases_per_predictor"]] < context[["min_cases_per_predictor"]]
  if (dim_p_gt_n) {
    findings <- c(
      findings,
      new_review_finding(
        code = "DIM_P_GT_N",
        severity = dim_severity,
        message = paste0(
          "The learner sees ",
          seen,
          " but has only ",
          training_cases,
          ", so training performance is not evidence of signal.",
          if (isFALSE(handles)) {
            paste0(" ", x@algorithm, " does not regularize in this regime.")
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
        severity = dim_severity,
        message = paste0(
          "There are ",
          fmt_review_num(sample[["cases_per_predictor"]]),
          if (x@type == "Classification") {
            " minority-class training cases"
          } else {
            " training cases"
          },
          " per predictor seen by the learner (",
          seen,
          "), fewer than ",
          format(context[["min_cases_per_predictor"]]),
          "."
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
          "before training. If that selection used the test cases, every ",
          "estimate in this review is optimistic; the fitted model cannot ",
          "show whether this happened."
        ),
        suggestion = paste0(
          "Make any selection of predictors a step of the training pipeline, ",
          "fitted on training cases only."
        )
      )
    )
  }
  list(findings = findings, dim_p_gt_n = dim_p_gt_n)
} # /rtemis::review_sample_findings


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


# %% review_gap_finding ----
#' Generalization gap finding
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param training Numeric: Training value of the headline metric (mean over
#'   resamples when resampled).
#' @param interval Numeric vector: Estimate, lower and upper bound of the test
#'   (or pooled out-of-sample) headline metric.
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
  resampled <- S7_inherits(x, SupervisedRes)
  beyond <- if (higher_is_better) {
    training > interval[[3L]]
  } else {
    training < interval[[2L]]
  }
  if (!isTRUE(beyond)) {
    return(list())
  }
  list(new_review_finding(
    code = "GENERALIZATION_GAP",
    severity = "warning",
    message = paste0(
      if (resampled) "Mean training " else "Training ",
      headline_label,
      " of ",
      fmt_review_num(training),
      if (higher_is_better) " lies above the " else " lies below the ",
      if (resampled) "pooled out-of-sample " else "test ",
      fmt_review_interval(interval[2:3], level),
      ", indicating overfitting."
    ),
    suggestion = review_gap_suggestion(x)
  ))
} # /rtemis::review_gap_finding


# %% review_sample ----
#' The review's sample record
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param cases_per_predictor Numeric: Training cases (minority-class cases for
#'   classification) per predictor seen by the learner.
#'
#' @return Named list with every member of `SupervisedReview@sample`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_sample <- function(x, folds, cases_per_predictor) {
  resampled <- S7_inherits(x, SupervisedRes)
  has_test <- !is.null(folds[[1L]][["y_test"]])
  n_test <- sum(vapply(folds, function(f) length(f[["y_test"]]), integer(1L)))
  predictors <- review_predictors(if (resampled) x@models[[1L]] else x)
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
    n_predictors = predictors[["p"]],
    n_components = predictors[["k"]],
    decomposition = predictors[["decomposition"]],
    cases_per_predictor = cases_per_predictor
  )
} # /rtemis::review_sample


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
  predictors <- review_predictors(if (resampled) x@models[[1L]] else x)
  effective <- predictors[["k"]] %||% predictors[["p"]]
  intervals <- NULL
  class_counts <- NULL

  # Pooled out-of-sample predictions ----
  if (has_test) {
    y <- review_pool(folds, "y_test")
    predicted <- review_pool(folds, "predicted_test")
    fold_sizes <- vapply(folds, function(f) length(f[["y_test"]]), integer(1L))
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
    cases_per_predictor <- min(counts_training) / effective
    class_counts <- data.frame(
      level = lv,
      training = apply(counts_training, 1L, min),
      test = if (has_test) {
        vapply(lv, function(l) sum(y == l), integer(1L), USE.NAMES = FALSE)
      } else {
        NA_integer_
      }
    )
    headline <- "balanced_accuracy"
    headline_label <- "balanced accuracy"
    higher_is_better <- TRUE
  } else {
    cases_per_predictor <- min(vapply(
      folds,
      function(f) length(f[["y_training_cases"]]),
      integer(1L)
    )) /
      effective
    headline <- "mse"
    headline_label <- "mean squared error"
    higher_is_better <- FALSE
  }
  sample <- review_sample(x, folds, cases_per_predictor)
  intervals_valid <- has_test && sample[["n_test_cases"]] == sample[["n_test"]]
  if (intervals_valid) {
    intervals <- if (classification) {
      prob <- if (length(lv) == 2L) review_pool(folds, "prob_test")
      if (!is.null(prob) && length(prob) != length(y)) {
        prob <- NULL
      }
      review_classification_intervals(y, predicted, prob, binclasspos, level)
    } else {
      review_regression_intervals(y, predicted, level)
    }
  }
  performance <- review_performance(folds, intervals)

  # Baseline ----
  baseline <- NULL
  findings_baseline <- list()
  findings_predictions <- list()
  folds_better <- NULL
  if (has_test) {
    compared <- review_baseline(
      x,
      folds,
      y = y,
      predicted = predicted,
      intervals = intervals,
      level = level,
      fold_sizes = fold_sizes,
      binclasspos = if (classification) binclasspos
    )
    baseline <- compared[["table"]]
    findings_baseline <- compared[["findings"]]
    folds_better <- compared[["folds_better"]]
    findings_predictions <- review_prediction_findings(
      y,
      predicted,
      classification
    )
  }

  # Findings ----
  sample_findings <- review_sample_findings(
    x,
    sample,
    list(
      has_test = has_test,
      intervals = intervals_valid,
      headline_label = headline_label,
      headline_interval = intervals[[headline]],
      fold_test = if (has_test) {
        vapply(folds, function(f) f[["metrics_test"]][[headline]], numeric(1L))
      },
      folds_better = folds_better,
      min_cases_per_predictor = min_cases_per_predictor,
      level = level
    )
  )
  findings_gap <- list()
  if (intervals_valid && !sample_findings[["dim_p_gt_n"]]) {
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
      sample_findings[["findings"]],
      findings_predictions,
      findings_baseline,
      findings_gap,
      tuning[["findings"]]
    )
  )
} # /rtemis::review_body


# %% review_prediction_findings ----
#' Constant predictions and never-predicted classes
#'
#' @param y Test outcome, pooled.
#' @param predicted Test predictions, pooled.
#' @param classification Logical.
#'
#' @return List of `ReviewFinding`, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_prediction_findings <- function(y, predicted, classification) {
  if (classification) {
    lv <- levels(y)
    predicted_levels <- unique(as.character(predicted))
    if (length(predicted_levels) == 1L) {
      return(list(new_review_finding(
        code = "CONSTANT_PREDICTIONS",
        severity = "warning",
        message = paste0(
          "Every one of the ",
          length(y),
          " test predictions was '",
          predicted_levels,
          "'."
        ),
        suggestion = paste0(
          "Check the outcome, the class balance and the hyperparameters; ",
          "the model is not separating the classes."
        )
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
        "Every one of the ",
        length(y),
        " test predictions was ",
        fmt_review_num(predicted[[1L]]),
        "."
      ),
      suggestion = paste0(
        "Check the outcome and the hyperparameters; the model is not using ",
        "the predictors."
      )
    )))
  }
  list()
} # /rtemis::review_prediction_findings


# %% review_baseline_row ----
# One row of the baseline table; unset cells are NA.
review_baseline_row <- function(
  metric,
  model = NA_real_,
  model_interval = c(NA_real_, NA_real_),
  baseline = NA_real_,
  skill = c(NA_real_, NA_real_, NA_real_),
  p_value = NA_real_,
  outcome = NA_character_,
  resamples_better = NA_integer_
) {
  data.frame(
    metric = metric,
    model = model,
    model_lower = model_interval[[1L]],
    model_upper = model_interval[[2L]],
    baseline = baseline,
    skill = skill[[1L]],
    skill_lower = skill[[2L]],
    skill_upper = skill[[3L]],
    p_value = p_value,
    outcome = outcome,
    resamples_better = resamples_better
  )
} # /rtemis::review_baseline_row


# %% review_baseline ----
#' Baseline table and findings
#'
#' The baseline predicts, for each fold's test cases, the most common class of
#' that fold's training cases (classification) or that fold's training mean
#' (regression), as the model saw them. Intervals, p-values and skill-score
#' intervals are computed only when the review's intervals are valid; a
#' resampled model also counts the resamples that beat their own baseline on
#' the headline metric.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param y Pooled test outcome.
#' @param predicted Pooled test predictions.
#' @param intervals Optional named list from `review_*_intervals()`.
#' @param level Numeric: Confidence level.
#' @param fold_sizes Integer: Test cases per fold.
#' @param binclasspos Optional Integer: Position of the positive level.
#'
#' @return List with `table`, `findings` and `folds_better` (count, or NULL
#'   for a single split).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline <- function(
  x,
  folds,
  y,
  predicted,
  intervals,
  level,
  fold_sizes,
  binclasspos
) {
  resampled <- S7_inherits(x, SupervisedRes)
  fold_index <- rep(seq_along(folds), fold_sizes)
  rows <- list()
  findings <- list()
  folds_better <- NULL
  where <- if (resampled) "Pooled out-of-sample " else "Test "
  if (x@type == "Classification") {
    lv <- levels(y)
    majority <- vapply(
      folds,
      function(f) {
        lv[[which.max(table(factor(f[["y_training"]], levels = lv)))]]
      },
      character(1L)
    )
    baseline_predicted <- factor(rep(majority, fold_sizes), levels = lv)
    recall_mean <- function(truth, guess) {
      present <- lv[lv %in% as.character(truth)]
      mean(vapply(
        present,
        function(l) mean(guess[truth == l] == l),
        numeric(1L)
      ))
    }
    nir <- mean(y == baseline_predicted)
    baseline_ba <- recall_mean(y, baseline_predicted)
    if (resampled) {
      folds_better <- sum(vapply(
        seq_along(folds),
        function(j) {
          idx <- fold_index == j
          isTRUE(
            folds[[j]][["metrics_test"]][["balanced_accuracy"]] >
              recall_mean(y[idx], baseline_predicted[idx])
          )
        },
        logical(1L)
      ))
    }
    accuracy <- intervals[["accuracy"]] %||% c(mean(y == predicted), NA, NA)
    ba <- intervals[["balanced_accuracy"]] %||%
      c(recall_mean(y, predicted), NA, NA)
    tested <- !is.null(intervals)
    accuracy_p <- if (tested) {
      stats::binom.test(
        sum(y == predicted),
        length(y),
        p = nir,
        alternative = "greater"
      )[["p.value"]]
    } else {
      NA_real_
    }
    accuracy_outcome <- if (tested) {
      review_baseline_outcome(accuracy[2:3], nir)
    } else {
      NA_character_
    }
    ba_outcome <- if (tested) {
      review_baseline_outcome(ba[2:3], baseline_ba)
    } else {
      NA_character_
    }
    rows <- c(
      rows,
      list(
        review_baseline_row(
          "accuracy",
          model = accuracy[[1L]],
          model_interval = accuracy[2:3],
          baseline = nir,
          p_value = accuracy_p,
          outcome = accuracy_outcome
        ),
        review_baseline_row(
          "balanced_accuracy",
          model = ba[[1L]],
          model_interval = ba[2:3],
          baseline = baseline_ba,
          outcome = ba_outcome,
          resamples_better = folds_better %||% NA_integer_
        )
      )
    )
    if (tested) {
      findings <- c(
        findings,
        review_baseline_finding(
          code = "BASELINE_ACCURACY",
          outcome = accuracy_outcome,
          what = paste0(
            where,
            "accuracy of ",
            fmt_review_num(accuracy[[1L]]),
            " (",
            fmt_review_interval(accuracy[2:3], level),
            ")"
          ),
          baseline_text = paste0(
            "the ",
            fmt_review_num(nir),
            " achieved by always predicting the most common training class ",
            "(one-sided exact binomial p = ",
            fmt_review_num(accuracy_p),
            ")"
          )
        ),
        review_baseline_finding(
          code = "BASELINE_BALANCED_ACCURACY",
          outcome = ba_outcome,
          what = paste0(
            where,
            "balanced accuracy of ",
            fmt_review_num(ba[[1L]]),
            " (",
            fmt_review_interval(ba[2:3], level),
            ")"
          ),
          baseline_text = paste0(
            "the baseline's ",
            fmt_review_num(baseline_ba)
          )
        )
      )
      auc <- intervals[["auc"]]
      if (!is.null(auc) && !anyNA(auc)) {
        auc_outcome <- review_baseline_outcome(auc[2:3], 0.5)
        rows <- c(
          rows,
          list(review_baseline_row(
            "auc",
            model = auc[[1L]],
            model_interval = auc[2:3],
            baseline = 0.5,
            outcome = auc_outcome
          ))
        )
        findings <- c(
          findings,
          review_baseline_finding(
            code = "BASELINE_AUC",
            outcome = auc_outcome,
            what = paste0(
              where,
              "AUC of ",
              fmt_review_num(auc[[1L]]),
              " (",
              fmt_review_interval(auc[2:3], level),
              ")"
            ),
            baseline_text = "0.5, the AUC of random scores"
          )
        )
      }
    }
    prob <- if (length(lv) == 2L) review_pool(folds, "prob_test")
    if (!is.null(prob) && length(prob) == length(y)) {
      positive_level <- lv[[binclasspos]]
      y01 <- as.numeric(y == positive_level)
      prevalence <- rep(
        vapply(
          folds,
          function(f) mean(f[["y_training"]] == positive_level),
          numeric(1L)
        ),
        fold_sizes
      )
      loss <- (y01 - prob)^2
      loss_baseline <- (y01 - prevalence)^2
      skill <- if (tested) {
        review_skill(loss, loss_baseline, level)
      } else {
        c(1 - mean(loss) / mean(loss_baseline), NA, NA)
      }
      brier_outcome <- if (tested && !anyNA(skill)) {
        review_baseline_outcome(skill[2:3], 0)
      } else {
        NA_character_
      }
      rows <- c(
        rows,
        list(review_baseline_row(
          "brier_score",
          model = mean(loss),
          baseline = mean(loss_baseline),
          skill = skill,
          outcome = brier_outcome
        ))
      )
      if (!is.na(brier_outcome)) {
        findings <- c(
          findings,
          review_baseline_finding(
            code = "BASELINE_BRIER",
            outcome = brier_outcome,
            what = paste0(
              where,
              "Brier score of ",
              fmt_review_num(mean(loss)),
              " (skill score ",
              fmt_review_num(skill[[1L]]),
              ", ",
              fmt_review_interval(skill[2:3], level),
              ")"
            ),
            baseline_text = paste0(
              "the ",
              fmt_review_num(mean(loss_baseline)),
              " of predicting the training proportion of '",
              positive_level,
              "' for every case"
            )
          )
        )
      }
    }
  } else {
    training_means <- vapply(
      folds,
      function(f) mean(f[["y_training"]]),
      numeric(1L)
    )
    baseline_predicted <- rep(training_means, fold_sizes)
    errors <- y - predicted
    baseline_errors <- y - baseline_predicted
    tested <- !is.null(intervals)
    if (resampled) {
      folds_better <- sum(vapply(
        seq_along(folds),
        function(j) {
          idx <- fold_index == j
          isTRUE(mean(errors[idx]^2) < mean(baseline_errors[idx]^2))
        },
        logical(1L)
      ))
    }
    skill <- if (tested) {
      review_skill(errors^2, baseline_errors^2, level)
    } else {
      c(1 - mean(errors^2) / mean(baseline_errors^2), NA, NA)
    }
    mse_outcome <- if (tested && !anyNA(skill)) {
      review_baseline_outcome(skill[2:3], 0)
    } else {
      NA_character_
    }
    rsq_of <- function(e) 1 - sum(e^2) / sum((y - mean(y))^2)
    rows <- list(
      review_baseline_row(
        "mse",
        model = mean(errors^2),
        model_interval = intervals[["mse"]][2:3] %||% c(NA_real_, NA_real_),
        baseline = mean(baseline_errors^2),
        skill = skill,
        outcome = mse_outcome,
        resamples_better = folds_better %||% NA_integer_
      ),
      review_baseline_row(
        "mae",
        model = mean(abs(errors)),
        model_interval = intervals[["mae"]][2:3] %||% c(NA_real_, NA_real_),
        baseline = mean(abs(baseline_errors))
      ),
      review_baseline_row(
        "rsq",
        model = rsq_of(errors),
        baseline = rsq_of(baseline_errors)
      )
    )
    if (!is.na(mse_outcome)) {
      findings <- c(
        findings,
        review_baseline_finding(
          code = "BASELINE_MSE",
          outcome = mse_outcome,
          what = paste0(
            where,
            "mean squared error of ",
            fmt_review_num(mean(errors^2)),
            " (skill score ",
            fmt_review_num(skill[[1L]]),
            ", ",
            fmt_review_interval(skill[2:3], level),
            ")"
          ),
          baseline_text = paste0(
            "the ",
            fmt_review_num(mean(baseline_errors^2)),
            " of predicting the training mean for every case"
          )
        )
      )
    }
  }
  list(
    table = do.call(rbind, rows),
    findings = findings,
    folds_better = folds_better
  )
} # /rtemis::review_baseline
