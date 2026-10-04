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
    "% interval ",
    fmt_review_num(interval[[1L]]),
    " to ",
    fmt_review_num(interval[[2L]])
  )
}


# %% review_basis ----
#' Start collecting basis rows
#'
#' An environment, so `add_basis()` can append rows from anywhere in a review
#' and `basis_table()` returns them in the order they were added.
#'
#' @return Environment with a `rows` list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_basis <- function() {
  b <- new.env(parent = emptyenv())
  b[["rows"]] <- list()
  b
} # /rtemis::review_basis


# %% add_basis ----
#' Append one basis row
#'
#' @param b Environment from `review_basis()`.
#' @param key Character: Stable key.
#' @param section Character: One of `REVIEW_BASIS_SECTIONS`.
#' @param label Character: Human-readable name.
#' @param value Optional Numeric: The value. NULL adds no row.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
add_basis <- function(b, key, section, label, value) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  b[["rows"]][[length(b[["rows"]]) + 1L]] <- data.frame(
    key = key,
    section = section,
    label = label,
    value = as.numeric(value)
  )
  invisible(NULL)
} # /rtemis::add_basis


# %% basis_table ----
basis_table <- function(b) {
  out <- do.call(rbind, b[["rows"]])
  rownames(out) <- NULL
  out
} # /rtemis::basis_table


# %% review_baseline_finding ----
#' A baseline comparison finding
#'
#' Three outcomes, read from the interval: its lower end above the baseline is
#' better, its upper end below is worse, and an interval containing the
#' baseline cannot be distinguished from it.
#'
#' @param code Character: Finding code.
#' @param interval Numeric vector of length 2: Interval of the compared value,
#'   oriented so that higher is better.
#' @param baseline Numeric: Baseline value on the same scale.
#' @param what Character: What was compared, as the subject of a sentence.
#' @param baseline_text Character: The baseline, as the end of a sentence.
#' @param basis Character vector: Basis keys.
#'
#' @return `ReviewFinding` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline_finding <- function(
  code,
  interval,
  baseline,
  what,
  baseline_text,
  basis
) {
  better <- interval[[1L]] > baseline
  worse <- interval[[2L]] < baseline
  new_review_finding(
    code = code,
    severity = if (better) "note" else "warning",
    message = paste0(
      what,
      if (better) {
        " is better than "
      } else if (worse) {
        " is worse than "
      } else {
        " cannot be distinguished from "
      },
      baseline_text,
      "."
    ),
    basis = basis
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


# %% review_add_performance ----
#' Add every metric's training and test values to the basis
#'
#' One fold gives training, test and their difference. Several folds give the
#' mean and standard deviation of each across resamples, and the difference of
#' the means; the headline metric's per-resample values follow. Intervals, when
#' given, are of the pooled out-of-sample estimate for several folds and of the
#' test estimate for one.
#'
#' @param b Environment from `review_basis()`.
#' @param folds List from `review_folds()`.
#' @param fold_names Character: Resample identifiers.
#' @param intervals Optional named list from `review_*_intervals()`.
#' @param headline Character: Headline metric name.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_add_performance <- function(b, folds, fold_names, intervals, headline) {
  resampled <- length(folds) > 1L
  has_test <- !is.null(folds[[1L]][["metrics_test"]])
  for (metric in names(folds[[1L]][["metrics_training"]])) {
    label <- label_metrics(metric)
    key <- paste0("performance.", metric)
    training <- vapply(
      folds,
      function(f) f[["metrics_training"]][[metric]],
      numeric(1L)
    )
    test <- if (has_test) {
      vapply(
        folds,
        function(f) {
          value <- f[["metrics_test"]][[metric]]
          if (is.null(value)) NA_real_ else value
        },
        numeric(1L)
      )
    }
    suffix <- if (resampled) ", mean over resamples)" else ")"
    add_basis(
      b,
      paste0(key, ".training"),
      "performance",
      paste0(label, " (training", suffix),
      mean(training)
    )
    if (resampled) {
      add_basis(
        b,
        paste0(key, ".training_sd"),
        "performance",
        paste0(label, " (training, SD over resamples)"),
        stats::sd(training)
      )
    }
    if (has_test && !all(is.na(test))) {
      add_basis(
        b,
        paste0(key, ".test"),
        "performance",
        paste0(label, " (test", suffix),
        mean(test)
      )
      if (resampled) {
        add_basis(
          b,
          paste0(key, ".test_sd"),
          "performance",
          paste0(label, " (test, SD over resamples)"),
          stats::sd(test)
        )
      }
      add_basis(
        b,
        paste0(key, ".difference"),
        "performance",
        paste0(label, " (training minus test", suffix),
        mean(training) - mean(test)
      )
      interval <- intervals[[metric]]
      if (!is.null(interval)) {
        where <- if (resampled) "pooled out-of-sample" else "test"
        if (resampled) {
          add_basis(
            b,
            paste0(key, ".pooled"),
            "performance",
            paste0(label, " (pooled out-of-sample)"),
            interval[[1L]]
          )
        }
        add_basis(
          b,
          paste0(key, ".test_lower"),
          "performance",
          paste0(label, " (", where, ", interval lower)"),
          interval[[2L]]
        )
        add_basis(
          b,
          paste0(key, ".test_upper"),
          "performance",
          paste0(label, " (", where, ", interval upper)"),
          interval[[3L]]
        )
      }
      if (resampled && metric == headline) {
        for (j in seq_along(folds)) {
          add_basis(
            b,
            paste0(key, ".resample_", j, ".training"),
            "performance",
            paste0(label, " (", fold_names[[j]], ", training)"),
            training[[j]]
          )
          add_basis(
            b,
            paste0(key, ".resample_", j, ".test"),
            "performance",
            paste0(label, " (", fold_names[[j]], ", test)"),
            test[[j]]
          )
        }
      }
    }
  }
  invisible(NULL)
} # /rtemis::review_add_performance


# %% review_sample_findings ----
#' Sample-size, resampling and dimensionality findings
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param context List: `resampled`, `n_folds`, `n_training` (smallest across
#'   folds), `n_test` (pooled), `n_test_cases` (distinct), `has_test`,
#'   `intervals` (logical: whether pooled intervals were computed),
#'   `predictors`, `cases_per_predictor`, `min_cases_per_predictor`,
#'   `headline`, `headline_label`, `headline_interval`, `fold_test`
#'   (headline per fold), `folds_better` (count), `level`.
#'
#' @return List with `findings` (list of `ReviewFinding`) and `dim_p_gt_n`
#'   (logical).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_sample_findings <- function(x, context) {
  findings <- list()
  headline_key <- paste0("performance.", context[["headline"]])
  level <- context[["level"]]
  n_training <- context[["n_training"]]

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
        basis = "sample.n_training",
        suggestion = "Hold out a test set, or use outer resampling."
      )
    )
  } else if (!context[["resampled"]]) {
    findings <- c(
      findings,
      new_review_finding(
        code = "SINGLE_SPLIT",
        severity = "note",
        message = paste0(
          "Performance was estimated on a single split of ",
          context[["n_test"]],
          " test cases. The test intervals reflect the number of test cases, ",
          "not how much the estimate would change with a different split."
        ),
        basis = c("sample.n_training", "sample.n_test"),
        suggestion = paste0(
          "Use outer resampling to test on every case and to measure ",
          "variation between splits; repeat it when cases are few."
        )
      )
    )
  }
  if (context[["has_test"]] && context[["intervals"]]) {
    findings <- c(
      findings,
      new_review_finding(
        code = "TEST_PRECISION",
        severity = "note",
        message = if (context[["resampled"]]) {
          paste0(
            "Pooled out-of-sample ",
            context[["headline_label"]],
            " has a ",
            fmt_review_interval(context[["headline_interval"]][2:3], level),
            ", from ",
            context[["n_test"]],
            " predictions over ",
            context[["n_folds"]],
            " resamples. The interval reflects the number of cases, not the ",
            "variation between resamples."
          )
        } else {
          paste0(
            "Test ",
            context[["headline_label"]],
            " has a ",
            fmt_review_interval(context[["headline_interval"]][2:3], level),
            ", from ",
            context[["n_test"]],
            " test cases."
          )
        },
        basis = c(
          "sample.n_test",
          if (context[["resampled"]]) {
            paste0(headline_key, ".pooled")
          } else {
            paste0(headline_key, ".test")
          },
          paste0(headline_key, ".test_lower"),
          paste0(headline_key, ".test_upper")
        )
      )
    )
  }
  if (context[["resampled"]] && context[["has_test"]]) {
    if (!context[["intervals"]]) {
      findings <- c(
        findings,
        new_review_finding(
          code = "OVERLAPPING_TEST_SETS",
          severity = "note",
          message = paste0(
            "The test sets overlap: ",
            context[["n_test"]],
            " predictions were made for ",
            context[["n_test_cases"]],
            " distinct cases, as with repeated or bootstrap resampling. ",
            "Pooled intervals would count repeated predictions of a case as ",
            "independent evidence, so none are computed and no comparison ",
            "with the baseline or the training performance is tested; the ",
            "variation between resamples is described instead."
          ),
          basis = c("sample.n_test", "sample.n_test_cases"),
          suggestion = paste0(
            "Use k-fold resampling, which tests every case once, for interval ",
            "estimates."
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
          context[["n_folds"]],
          " resamples (mean ",
          fmt_review_num(mean(fold_test)),
          ", SD ",
          fmt_review_num(stats::sd(fold_test)),
          "); ",
          context[["folds_better"]],
          " of ",
          context[["n_folds"]],
          " resamples did better than their baseline."
        ),
        basis = c(
          paste0(headline_key, ".test"),
          paste0(headline_key, ".test_sd"),
          paste0(headline_key, ".resample_", seq_along(fold_test), ".test"),
          "baseline.resamples_better"
        )
      )
    )
  }

  # Dimensionality ----
  predictors <- context[["predictors"]]
  handles <- algorithm_handles_p_gt_n(x@algorithm)
  dim_severity <- if (isFALSE(handles)) "warning" else "note"
  dim_suggestion <- paste0(
    "Judge the model by test performance only, estimated with resampling. ",
    "Prefer a regularized or sparse algorithm, or reduce the predictors inside ",
    "the training pipeline (a decomposition or feature selection fitted on ",
    "training cases only)."
  )
  predictor_basis <- if (is.null(predictors[["k"]])) {
    "sample.n_predictors"
  } else {
    c("sample.n_predictors", "sample.n_components")
  }
  seen <- if (is.null(predictors[["k"]])) {
    paste0("at least ", predictors[["effective"]], " predictors")
  } else {
    paste0(
      predictors[["effective"]],
      " components from ",
      predictors[["decomposition"]]
    )
  }
  training_cases <- if (context[["resampled"]]) {
    paste0(n_training, " training cases in its smallest resample")
  } else {
    paste0(n_training, " training cases")
  }
  dim_p_gt_n <- predictors[["effective"]] > n_training
  few_cases <- !dim_p_gt_n &&
    context[["cases_per_predictor"]] < context[["min_cases_per_predictor"]]
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
        basis = c("sample.n_training", predictor_basis),
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
          fmt_review_num(context[["cases_per_predictor"]]),
          if (x@type == "Classification") {
            " minority-class training cases"
          } else {
            " training cases"
          },
          " per predictor seen by the learner (",
          seen,
          "), fewer than ",
          format_review_value(context[["min_cases_per_predictor"]]),
          "."
        ),
        basis = c(
          "sample.cases_per_predictor",
          "setting.min_cases_per_predictor",
          predictor_basis
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
          "With this many predictors per case, predictors are often selected ",
          "before training. If any selection or filtering used the test ",
          "cases, every estimate in this review is optimistic; the fitted ",
          "model cannot show whether that happened."
        ),
        basis = c("sample.n_training", predictor_basis),
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
#' Tuning basis rows and grid-edge findings
#'
#' A selected value at the smallest or largest value searched suggests the
#' best value may lie beyond the grid. Only numeric hyperparameters searched
#' over at least three values are checked, because with two every choice is
#' an edge; an edge that is also a bound the hyperparameter declares cannot be
#' extended and is not reported.
#'
#' @param b Environment from `review_basis()`.
#' @param x `Supervised` or `SupervisedRes` object.
#'
#' @return List of `ReviewFinding`, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_tuning <- function(b, x) {
  models <- if (S7_inherits(x, SupervisedRes)) x@models else list(x)
  tuners <- Filter(Negate(is.null), lapply(models, function(m) m@tuner))
  if (length(tuners) == 0L) {
    return(list())
  }
  resampled <- S7_inherits(x, SupervisedRes)
  grid <- tuners[[1L]]@tuning_results[["param_grid"]]
  hp_class <- S7_class(tuners[[1L]]@hyperparameters)
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
    at_bound <- function(value, bound) !is.null(bound) && value == bound
    low_extendable <- !at_bound(values[[1L]], spec[["minimum"]]) &&
      !at_bound(values[[1L]], spec[["exclusive_minimum"]])
    high_extendable <- !at_bound(values[[length(values)]], spec[["maximum"]]) &&
      !at_bound(values[[length(values)]], spec[["exclusive_maximum"]])
    at_edge <- (low_extendable & selected == values[[1L]]) |
      (high_extendable & selected == values[[length(values)]])
    at_edge[is.na(at_edge)] <- FALSE
    key <- paste0("tuning.", name)
    add_basis(
      b,
      paste0(key, ".grid_min"),
      "tuning",
      paste0(name, " (smallest value searched)"),
      values[[1L]]
    )
    add_basis(
      b,
      paste0(key, ".grid_max"),
      "tuning",
      paste0(name, " (largest value searched)"),
      values[[length(values)]]
    )
    add_basis(
      b,
      paste0(key, ".grid_size"),
      "tuning",
      paste0(name, " (values searched)"),
      length(values)
    )
    if (resampled) {
      add_basis(
        b,
        paste0(key, ".at_edge"),
        "tuning",
        paste0(name, " (resamples selecting an extendable edge)"),
        sum(at_edge)
      )
    } else {
      add_basis(
        b,
        paste0(key, ".selected"),
        "tuning",
        paste0(name, " (selected)"),
        selected[[1L]]
      )
    }
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
            fmt_review_num(values[[1L]]),
            " to ",
            fmt_review_num(values[[length(values)]]),
            "); the best value may lie beyond it."
          ),
          basis = c(
            paste0(key, ".grid_min"),
            paste0(key, ".grid_max"),
            if (resampled) paste0(key, ".at_edge") else paste0(key, ".selected")
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
  findings
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
#' @param headline Character: Headline metric name.
#' @param headline_label Character: Its label.
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
  headline,
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
  key <- paste0("performance.", headline)
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
      ": the model overfits."
    ),
    basis = c(
      paste0(key, ".training"),
      if (resampled) paste0(key, ".pooled") else paste0(key, ".test"),
      if (higher_is_better) {
        paste0(key, ".test_upper")
      } else {
        paste0(key, ".test_lower")
      }
    ),
    suggestion = review_gap_suggestion(x)
  ))
} # /rtemis::review_gap_finding


# %% review_add_sample ----
#' Sample rows shared by both supervised types
#'
#' @return List with `n_training` (smallest across folds), `n_test` (pooled),
#'   `n_test_cases` (distinct) and `predictors`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_add_sample <- function(b, x, folds) {
  resampled <- S7_inherits(x, SupervisedRes)
  has_test <- !is.null(folds[[1L]][["y_test"]])
  n_training <- min(vapply(
    folds,
    function(f) length(f[["y_training_cases"]]),
    integer(1L)
  ))
  n_test <- sum(vapply(folds, function(f) length(f[["y_test"]]), integer(1L)))
  n_test_cases <- if (resampled) review_test_cases(x) else n_test
  predictors <- review_predictors(if (resampled) x@models[[1L]] else x)
  if (resampled) {
    add_basis(
      b,
      "sample.n_resamples",
      "sample",
      "Outer resamples (successful)",
      length(folds)
    )
    add_basis(
      b,
      "sample.n_resamples_requested",
      "sample",
      "Outer resamples (requested)",
      length(x@outer_resampler@resamples)
    )
    add_basis(
      b,
      "sample.n_training",
      "sample",
      "Training cases (smallest resample)",
      n_training
    )
    add_basis(
      b,
      "sample.n_test",
      "sample",
      "Out-of-sample predictions (all resamples)",
      n_test
    )
    add_basis(
      b,
      "sample.n_test_cases",
      "sample",
      "Distinct cases predicted out of sample",
      n_test_cases
    )
  } else {
    add_basis(b, "sample.n_training", "sample", "Training cases", n_training)
    if (has_test) {
      add_basis(b, "sample.n_test", "sample", "Test cases", n_test)
    }
  }
  add_basis(
    b,
    "sample.n_predictors",
    "sample",
    "Predictors (input columns)",
    predictors[["p"]]
  )
  if (!is.null(predictors[["k"]])) {
    add_basis(
      b,
      "sample.n_components",
      "sample",
      paste0(
        "Components passed to the learner (",
        predictors[["decomposition"]],
        ")"
      ),
      predictors[["k"]]
    )
  }
  list(
    n_training = n_training,
    n_test = n_test,
    n_test_cases = n_test_cases,
    predictors = predictors
  )
} # /rtemis::review_add_sample


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
  folds <- review_folds(x)
  fold_names <- if (S7_inherits(x, SupervisedRes)) x@resample_ids else "test"
  review_body(
    x,
    folds,
    fold_names,
    level = confidence_level,
    min_cases_per_predictor = min_cases_per_predictor
  )
} # /rtemis::review_supervised


# %% review_body ----
#' Assemble a review from folds
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List from `review_folds()`.
#' @param fold_names Character: Resample identifiers.
#' @param level Numeric (0, 1): Confidence level.
#' @param min_cases_per_predictor Numeric: Threshold.
#'
#' @return `SupervisedReview` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_body <- function(x, folds, fold_names, level, min_cases_per_predictor) {
  b <- review_basis()
  classification <- x@type == "Classification"
  resampled <- S7_inherits(x, SupervisedRes)
  has_test <- !is.null(folds[[1L]][["y_test"]])

  # Sample ----
  sample <- review_add_sample(b, x, folds)
  intervals_valid <- has_test && sample[["n_test_cases"]] == sample[["n_test"]]

  # Pooled out-of-sample predictions and baselines ----
  if (has_test) {
    y <- review_pool(folds, "y_test")
    predicted <- review_pool(folds, "predicted_test")
    fold_sizes <- vapply(folds, function(f) length(f[["y_test"]]), integer(1L))
  }
  if (classification) {
    lv <- levels(folds[[1L]][["y_training"]])
    binary <- length(lv) == 2L
    binclasspos <- if (resampled) x@models[[1L]]@binclasspos else x@binclasspos
    counts_training <- vapply(
      folds,
      function(f) {
        as.vector(table(factor(f[["y_training_cases"]], levels = lv)))
      },
      integer(length(lv))
    )
    counts_training <- matrix(counts_training, nrow = length(lv))
    cases_per_predictor <- min(counts_training) /
      sample[["predictors"]][["effective"]]
    for (i in seq_along(lv)) {
      add_basis(
        b,
        paste0("sample.class_", i, ".training"),
        "sample",
        paste0(
          "Training cases of '",
          lv[[i]],
          if (resampled) "' (smallest resample)" else "'"
        ),
        min(counts_training[i, ])
      )
      if (has_test) {
        add_basis(
          b,
          paste0("sample.class_", i, ".test"),
          "sample",
          paste0(
            if (resampled) {
              "Out-of-sample predictions of '"
            } else {
              "Test cases of '"
            },
            lv[[i]],
            "'"
          ),
          sum(y == lv[[i]])
        )
      }
    }
    add_basis(
      b,
      "sample.cases_per_predictor",
      "sample",
      paste0(
        "Minority-class training cases per predictor",
        if (resampled) " (smallest resample)"
      ),
      cases_per_predictor
    )
    headline <- "balanced_accuracy"
    headline_label <- "balanced accuracy"
    higher_is_better <- TRUE
    if (has_test) {
      prob <- if (binary) review_pool(folds, "prob_test")
      if (!is.null(prob) && length(prob) != length(y)) {
        prob <- NULL
      }
      majority <- vapply(
        folds,
        function(f) {
          lv[[which.max(table(factor(f[["y_training"]], levels = lv)))]]
        },
        character(1L)
      )
      baseline_predicted <- factor(rep(majority, fold_sizes), levels = lv)
      intervals <- if (intervals_valid) {
        review_classification_intervals(y, predicted, prob, binclasspos, level)
      }
    }
  } else {
    n_training <- sample[["n_training"]]
    cases_per_predictor <- n_training / sample[["predictors"]][["effective"]]
    add_basis(
      b,
      "sample.cases_per_predictor",
      "sample",
      paste0(
        "Training cases per predictor",
        if (resampled) " (smallest resample)"
      ),
      cases_per_predictor
    )
    headline <- "mse"
    headline_label <- "mean squared error"
    higher_is_better <- FALSE
    if (has_test) {
      training_means <- vapply(
        folds,
        function(f) mean(f[["y_training"]]),
        numeric(1L)
      )
      baseline_predicted <- rep(training_means, fold_sizes)
      intervals <- if (intervals_valid) {
        review_regression_intervals(y, predicted, level)
      }
    }
  }

  # Performance ----
  review_add_performance(
    b,
    folds,
    fold_names,
    if (has_test) intervals,
    headline
  )

  # Baseline ----
  findings_baseline <- list()
  findings_predictions <- list()
  folds_better <- NULL
  if (has_test) {
    baseline <- review_baseline(
      b,
      x,
      folds,
      y = y,
      predicted = predicted,
      baseline_predicted = baseline_predicted,
      prob = if (classification) prob,
      intervals = intervals,
      level = level,
      fold_sizes = fold_sizes,
      binclasspos = if (classification) binclasspos
    )
    findings_baseline <- baseline[["findings"]]
    folds_better <- baseline[["folds_better"]]
    findings_predictions <- review_prediction_findings(
      y,
      predicted,
      classification
    )
  }

  # Settings ----
  add_basis(
    b,
    "setting.confidence_level",
    "setting",
    "Confidence level of intervals",
    level
  )
  add_basis(
    b,
    "setting.min_cases_per_predictor",
    "setting",
    "Minimum cases per predictor",
    min_cases_per_predictor
  )

  # Findings ----
  fold_test <- if (has_test) {
    vapply(folds, function(f) f[["metrics_test"]][[headline]], numeric(1L))
  }
  sample_findings <- review_sample_findings(
    x,
    list(
      resampled = resampled,
      n_folds = length(folds),
      n_training = sample[["n_training"]],
      n_test = sample[["n_test"]],
      n_test_cases = sample[["n_test_cases"]],
      has_test = has_test,
      intervals = intervals_valid,
      predictors = sample[["predictors"]],
      cases_per_predictor = cases_per_predictor,
      min_cases_per_predictor = min_cases_per_predictor,
      headline = headline,
      headline_label = headline_label,
      headline_interval = if (intervals_valid) intervals[[headline]],
      fold_test = fold_test,
      folds_better = folds_better,
      level = level
    )
  )
  findings_gap <- list()
  if (intervals_valid && !sample_findings[["dim_p_gt_n"]]) {
    training <- mean(vapply(
      folds,
      function(f) f[["metrics_training"]][[headline]],
      numeric(1L)
    ))
    findings_gap <- review_gap_finding(
      x,
      training = training,
      interval = intervals[[headline]],
      higher_is_better = higher_is_better,
      headline = headline,
      headline_label = headline_label,
      level = level
    )
  }
  findings_tuning <- review_tuning(b, x)

  new_supervised_review(
    algorithm = x@algorithm,
    type = x@type,
    description = desc(x),
    basis = basis_table(b),
    findings = c(
      sample_findings[["findings"]],
      findings_predictions,
      findings_baseline,
      findings_gap,
      findings_tuning
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
        basis = "sample.n_test",
        suggestion = paste0(
          "Check the outcome, the class balance and the hyperparameters; ",
          "the model is not separating the classes."
        )
      )))
    }
    present <- lv[lv %in% as.character(y)]
    never <- which(lv %in% present & !(lv %in% predicted_levels))
    if (length(never) > 0L) {
      return(list(new_review_finding(
        code = "CLASS_NEVER_PREDICTED",
        severity = "warning",
        message = paste0(
          ngettext(length(never), "Class ", "Classes "),
          paste0("'", lv[never], "'", collapse = ", "),
          ngettext(length(never), " occurs", " occur"),
          " among the test cases but ",
          ngettext(length(never), "was", "were"),
          " never predicted."
        ),
        basis = paste0("sample.class_", never, ".test"),
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
      basis = "sample.n_test",
      suggestion = paste0(
        "Check the outcome and the hyperparameters; the model is not using ",
        "the predictors."
      )
    )))
  }
  list()
} # /rtemis::review_prediction_findings


# %% review_baseline ----
#' Baseline rows and findings
#'
#' The baseline predicts, for each fold's test cases, the most common class of
#' that fold's training cases (classification) or that fold's training mean
#' (regression). Comparisons with intervals are made only when intervals are
#' valid; a resampled model also counts the resamples that beat their own
#' baseline on the headline metric.
#'
#' @return List with `findings` and `folds_better` (count, or NULL for a
#'   single split).
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_baseline <- function(
  b,
  x,
  folds,
  y,
  predicted,
  baseline_predicted,
  prob,
  intervals,
  level,
  fold_sizes,
  binclasspos
) {
  resampled <- S7_inherits(x, SupervisedRes)
  fold_index <- rep(seq_along(folds), fold_sizes)
  findings <- list()
  folds_better <- NULL
  if (x@type == "Classification") {
    lv <- levels(y)
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
    single_majority <- length(unique(as.character(baseline_predicted))) == 1L
    add_basis(
      b,
      "baseline.accuracy",
      "baseline",
      if (single_majority) {
        paste0(
          "Accuracy of always predicting '",
          as.character(baseline_predicted[[1L]]),
          "' (training majority)"
        )
      } else {
        "Accuracy of predicting each resample's training majority"
      },
      nir
    )
    add_basis(
      b,
      "baseline.balanced_accuracy",
      "baseline",
      "Balanced accuracy of the baseline",
      baseline_ba
    )
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
      add_basis(
        b,
        "baseline.resamples_better",
        "baseline",
        "Resamples with test balanced accuracy above their baseline",
        folds_better
      )
    }
    if (!is.null(intervals)) {
      correct <- sum(y == predicted)
      accuracy_p <- stats::binom.test(
        correct,
        length(y),
        p = nir,
        alternative = "greater"
      )[["p.value"]]
      add_basis(
        b,
        "baseline.accuracy_p_value",
        "baseline",
        "P-value, accuracy above baseline (one-sided exact binomial)",
        accuracy_p
      )
      where <- if (resampled) "Pooled out-of-sample " else "Test "
      point_key <- function(metric) {
        paste0("performance.", metric, if (resampled) ".pooled" else ".test")
      }
      findings <- c(
        findings,
        review_baseline_finding(
          code = "BASELINE_ACCURACY",
          interval = intervals[["accuracy"]][2:3],
          baseline = nir,
          what = paste0(
            where,
            "accuracy of ",
            fmt_review_num(intervals[["accuracy"]][[1L]]),
            " (",
            fmt_review_interval(intervals[["accuracy"]][2:3], level),
            ")"
          ),
          baseline_text = paste0(
            "the ",
            fmt_review_num(nir),
            " of predicting the most common training class (one-sided exact binomial p = ",
            fmt_review_num(accuracy_p),
            ")"
          ),
          basis = c(
            point_key("accuracy"),
            "performance.accuracy.test_lower",
            "performance.accuracy.test_upper",
            "baseline.accuracy",
            "baseline.accuracy_p_value"
          )
        ),
        review_baseline_finding(
          code = "BASELINE_BALANCED_ACCURACY",
          interval = intervals[["balanced_accuracy"]][2:3],
          baseline = baseline_ba,
          what = paste0(
            where,
            "balanced accuracy of ",
            fmt_review_num(intervals[["balanced_accuracy"]][[1L]]),
            " (",
            fmt_review_interval(intervals[["balanced_accuracy"]][2:3], level),
            ")"
          ),
          baseline_text = paste0(
            "the ",
            fmt_review_num(baseline_ba),
            " of the baseline"
          ),
          basis = c(
            point_key("balanced_accuracy"),
            "performance.balanced_accuracy.test_lower",
            "performance.balanced_accuracy.test_upper",
            "baseline.balanced_accuracy"
          )
        )
      )
      auc <- intervals[["auc"]]
      if (!is.null(auc) && !anyNA(auc)) {
        add_basis(
          b,
          "baseline.auc",
          "baseline",
          "AUC of uninformative scores",
          0.5
        )
        findings <- c(
          findings,
          review_baseline_finding(
            code = "BASELINE_AUC",
            interval = auc[2:3],
            baseline = 0.5,
            what = paste0(
              where,
              "AUC of ",
              fmt_review_num(auc[[1L]]),
              " (",
              fmt_review_interval(auc[2:3], level),
              ")"
            ),
            baseline_text = "0.5, the AUC of uninformative scores",
            basis = c(
              point_key("auc"),
              "performance.auc.test_lower",
              "performance.auc.test_upper",
              "baseline.auc"
            )
          )
        )
      }
    }
    if (!is.null(prob) && length(lv) == 2L) {
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
      add_basis(
        b,
        "baseline.brier_score",
        "baseline",
        paste0(
          "Brier score of predicting the training prevalence of '",
          positive_level,
          "'"
        ),
        mean(loss_baseline)
      )
      if (!is.null(intervals)) {
        skill <- review_skill(loss, loss_baseline, level)
        add_basis(
          b,
          "baseline.brier_skill",
          "baseline",
          "Brier skill score",
          skill[[1L]]
        )
        add_basis(
          b,
          "baseline.brier_skill_lower",
          "baseline",
          "Brier skill score (interval lower)",
          skill[[2L]]
        )
        add_basis(
          b,
          "baseline.brier_skill_upper",
          "baseline",
          "Brier skill score (interval upper)",
          skill[[3L]]
        )
        if (!anyNA(skill)) {
          findings <- c(
            findings,
            review_baseline_finding(
              code = "BASELINE_BRIER",
              interval = skill[2:3],
              baseline = 0,
              what = paste0(
                if (resampled) {
                  "Pooled out-of-sample Brier score of "
                } else {
                  "Test Brier score of "
                },
                fmt_review_num(mean(loss)),
                ", a Brier skill score of ",
                fmt_review_num(skill[[1L]]),
                " (",
                fmt_review_interval(skill[2:3], level),
                "),"
              ),
              baseline_text = paste0(
                "predicting the training prevalence of '",
                positive_level,
                "' for every case (Brier score ",
                fmt_review_num(mean(loss_baseline)),
                ")"
              ),
              basis = c(
                "baseline.brier_score",
                "baseline.brier_skill",
                "baseline.brier_skill_lower",
                "baseline.brier_skill_upper"
              )
            )
          )
        }
      }
    }
  } else {
    errors <- y - predicted
    baseline_errors <- y - baseline_predicted
    add_basis(
      b,
      "baseline.mse",
      "baseline",
      if (resampled) {
        "MSE of predicting each resample's training mean"
      } else {
        "MSE of predicting the training mean"
      },
      mean(baseline_errors^2)
    )
    add_basis(
      b,
      "baseline.mae",
      "baseline",
      if (resampled) {
        "MAE of predicting each resample's training mean"
      } else {
        "MAE of predicting the training mean"
      },
      mean(abs(baseline_errors))
    )
    add_basis(
      b,
      "baseline.rsq",
      "baseline",
      paste(label_metrics("rsq"), "of the baseline"),
      1 - sum(baseline_errors^2) / sum((y - mean(y))^2)
    )
    if (resampled) {
      folds_better <- sum(vapply(
        seq_along(folds),
        function(j) {
          idx <- fold_index == j
          isTRUE(mean(errors[idx]^2) < mean(baseline_errors[idx]^2))
        },
        logical(1L)
      ))
      add_basis(
        b,
        "baseline.resamples_better",
        "baseline",
        "Resamples with test MSE below their baseline",
        folds_better
      )
    }
    if (!is.null(intervals)) {
      skill <- review_skill(errors^2, baseline_errors^2, level)
      add_basis(
        b,
        "baseline.mse_skill",
        "baseline",
        "MSE skill score",
        skill[[1L]]
      )
      add_basis(
        b,
        "baseline.mse_skill_lower",
        "baseline",
        "MSE skill score (interval lower)",
        skill[[2L]]
      )
      add_basis(
        b,
        "baseline.mse_skill_upper",
        "baseline",
        "MSE skill score (interval upper)",
        skill[[3L]]
      )
      if (!anyNA(skill)) {
        findings <- c(
          findings,
          review_baseline_finding(
            code = "BASELINE_MSE",
            interval = skill[2:3],
            baseline = 0,
            what = paste0(
              if (resampled) {
                "Pooled out-of-sample mean squared error of "
              } else {
                "Test mean squared error of "
              },
              fmt_review_num(mean(errors^2)),
              ", an MSE skill score of ",
              fmt_review_num(skill[[1L]]),
              " (",
              fmt_review_interval(skill[2:3], level),
              "),"
            ),
            baseline_text = paste0(
              "predicting the training mean for every case (MSE ",
              fmt_review_num(mean(baseline_errors^2)),
              ")"
            ),
            basis = c(
              "baseline.mse",
              "baseline.mse_skill",
              "baseline.mse_skill_lower",
              "baseline.mse_skill_upper"
            )
          )
        )
      }
    }
  }
  list(findings = findings, folds_better = folds_better)
} # /rtemis::review_baseline
