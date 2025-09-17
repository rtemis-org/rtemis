# metrics.R
# ::rtemis::
# 2019- EDG rtemis.org

#' Error functions
#'
#' Convenience functions for calculating loss.
#' These can be passed as arguments to learners that support custom loss functions.
#'
#' @rdname error
#' @param x Vector of True values
#' @param y Vector of predicted values
#' @param na.rm Logical: If TRUE, remove NA values before computation.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mae <- function(x, y, na.rm = TRUE) {
  error <- x - y
  mean(abs(error), na.rm = na.rm)
} # /rtemis::mae

#' @rdname error
#' @keywords internal
#' @noRd
mse <- function(x, y, na.rm = TRUE) {
  error <- x - y
  mean(error^2, na.rm = na.rm)
} # /rtemis::mse

#' Weighted MSE
#'
#' @rdname error
#' @keywords internal
#' @noRd
msew <- function(x, y, weights = rep(1, length(y)), na.rm = TRUE) {
  error <- x - y
  error <- error * weights
  mean(error^2, na.rm = na.rm)
} # /rtemis::msew

#' @rdname error
#' @keywords internal
#' @noRd
rmse <- function(x, y, na.rm = TRUE) {
  sqrt(mse(x, y, na.rm = na.rm))
} # rtemis::rmse

#' R-squared
#'
#' @param x Float, vector: True values
#' @param y Float, vector: predicted values
#' @author EDG
#' @keywords internal
#' @noRd
rsq <- function(x, y) {
  SSE <- sum((x - y)^2)
  # Sum of Squares due to Regression (SSR) a.k.a. Explained Sum of Squares (ESS)
  # SSR <- sum((mean(x) - y)^2)
  # Total Sum of Squares (TSS or SST)
  SST <- sum((x - mean(x))^2)
  # R-squared a.k.a. Coefficient of Determination i.e. percent variance explained
  1 - (SSE / SST)
} # rtemis::rsq


#' Log Loss for a binary classifier
#'
#' @param true_int Integer vector, {0, 1}: True labels (1 is the positive class).
#' @param predicted_prob Float, vector: predicted probabilities.
#' @param eps Float: Small value to prevent log(0).
#'
#' @author EDG
#' @keywords internal
#' @noRd
logloss <- function(true_int, predicted_prob, eps = 1e-16) {
  predicted_prob <- pmax(pmin(predicted_prob, 1 - eps), eps)
  -mean(
    true_int * log(predicted_prob) + (1 - true_int) * log(1 - predicted_prob)
  )
} # rtemis::logloss


#' Sensitivity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param predicted predicted labels
#' @param harmonize Logical: If TRUE, run `factor_harmonize` first
#' @param verbosity Integer: Verbosity level.
#'
#' @export
#' @keywords internal
#' @noRd
sensitivity <- function(true, predicted, harmonize = FALSE, verbosity = 1L) {
  if (harmonize) {
    predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  }
  pos_index <- true == levels(true)[1]
  condition_pos <- sum(pos_index)
  true_pos <- sum(true[pos_index] == predicted[pos_index])
  true_pos / condition_pos
}


#' Specificity
#'
#' The first factor level is considered the positive case.
#'
#' @param true True labels
#' @param predicted predicted labels
#' @param harmonize Logical: If TRUE, run `factor_harmonize` first
#' @param verbosity Integer: Verbosity level.
#'
#' @keywords internal
#' @noRd
specificity <- function(true, predicted, harmonize = FALSE, verbosity = 1L) {
  if (harmonize) {
    predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  }
  neg_index <- true == levels(true)[2]
  condition_neg <- sum(neg_index)
  true_neg <- sum(true[neg_index] == predicted[neg_index])
  true_neg / condition_neg
}

#' Balanced Accuracy
#'
#' Balanced Accuracy of a binary classifier
#'
#' BAcc = .5 * (Sensitivity + Specificity)
#'
#' @param true Factor: True labels.
#' @param predicted Factor: Predicted labels.
#' @param harmonize Logical: passed to `sensitivity()` and `specificity`, which use `factor_harmonize`.
#' @param verbosity Integer: Verbosity level.
#'
#' @keywords internal
#' @noRd
bacc <- function(true, predicted, harmonize = FALSE, verbosity = 1L) {
  0.5 *
    (sensitivity(
      true,
      predicted,
      harmonize = harmonize,
      verbosity = verbosity
    ) +
      specificity(
        true,
        predicted,
        harmonize = harmonize,
        verbosity = verbosity
      ))
} # /rtemis::bacc

#' Precision (aka PPV)
#'
#' The first factor level is considered the positive case.
#'
#' @param true Factor: True labels
#' @param predicted Factor: predicted labels
#' @param harmonize Logical: If TRUE, run `factor_harmonize` first
#' @param verbosity Integer: Verbosity level.
#'
#' @keywords internal
#' @noRd
precision <- function(true, predicted, harmonize = FALSE, verbosity = 1L) {
  if (harmonize) {
    predicted <- factor_harmonize(true, predicted, verbosity = verbosity)
  }
  tbl <- table(predicted, true)
  predicted_totals <- rowSums(tbl)[1]
  hits <- diag(tbl)[1]

  if (hits == 0 && predicted_totals == 0) {
    1
  } else {
    hits / predicted_totals
  }
} # /rtemis::precision

#' Factor harmonize
#'
#' @param reference Reference factor.
#' @param x Input factor.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Factor: x with levels in the same order as reference.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
factor_harmonize <- function(reference, x, verbosity = 1L) {
  if (!is.factor(x) || !is.factor(reference)) {
    cli::cli_abort("Inputs must be factors")
  }
  if (!all(levels(x) == levels(reference))) {
    if (!all(levels(x) %in% levels(reference))) {
      if (verbosity > 0L) {
        msg("Levels of x:")
      }
      levels(x)
      if (verbosity > 0L) {
        msg("levels of reference:")
      }
      levels(reference)
      cli::cli_abort("Levels of two inputs do not match")
    }
    if (verbosity > 0L) {
      msg("Input factor levels are not in the same order, correcting")
    }
    x <- factor(x, levels = levels(reference))
  }
  x
} # /rtemis::factor_harmonize


#' F1 score
#'
#' Calculate the F1 score for classification:
#'
#' \deqn{F1 = 2 \frac{Recall \cdot Precision}{Recall + Precision}}{F1 = 2 * (Recall * Precision)/(Recall + Precision)}
#'
#' @param recall Float \[0, 1\]: Recall a.k.a. Sensitivity
#' @param precision Float \[0, 1\]: Precision a.k.a. Positive Predictive Value
#'
#' @author EDG
#' @keywords internal
#' @noRd
f1 <- function(precision, recall) {
  2 * (recall * precision) / (recall + precision)
} # /rtemis::f1


# auc.R
# ::rtemis::
# 2019-23 EDG rtemis.org

#' Area under the ROC Curve
#'
#' Get the Area under the ROC curve to assess classifier performance.
#'
#' Important Note: We assume that true labels are a factor where the first level
#' is the "positive" case, a.k.a. the event. All methods used here, "pROC",
#' "auc_pairs", "ROCR", have been setup to expect this. This goes against the
#' default setting for both "pROC" and "ROCR", which will not give an AUC less
#' than .5 because they will reorder levels. We don't want this because you
#' can have a classifier perform worse than .5 and it can be very confusing if
#' levels are reordered automatically and different functions give you different
#' AUC.
#'
#' @param preds Numeric, Vector: Probabilities or model scores
#' (e.g. c(.32, .75, .63), etc)
#' @param labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param method Character: "pROC", "auc_pairs", or "ROCR": Method to use.
#' Will use `pROC::roc`, `auc_pairs()`, `ROCR::performance`, respectively.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' preds <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' auc(preds, labels, method = "ROCR")
#' auc(preds, labels, method = "pROC")
#' auc(preds, labels, method = "auc_pairs")
#' }
auc <- function(true_int, predicted_prob, method = "lightAUC", verbosity = 0L) {
  # Checks ----
  method <- match.arg(method)
  check_inherits(true_int, "integer")
  check_float01inc(predicted_prob)
  # method <- match.arg(method)
  if (length(unique(true_int)) == 1) {
    return(NaN)
  }
  if (method == "lightAUC") {
    check_dependencies("lightAUC")
    auc. <- lightAUC::lightAUC(probs = predicted_prob, actuals = true_int)
  } else if (method == "ROCR") {
    check_dependencies("ROCR")
    .pred <- try(ROCR::prediction(
      predicted_prob,
      true_int,
      label.ordering = rev(levels(true_int))
    ))
    auc. <- try(ROCR::performance(.pred, "auc")@y.values[[1]])
  } # else if (method == "pROC") {
  # check_dependencies("pROC")
  #   check_dependencies("pROC")
  #   .auc <- try(pROC::roc(
  #     true_int, predicted_prob,
  #     levels = rev(levels(true_int)),
  #     direction = "<"
  #   )[["auc"]])
  # }

  if (inherits(auc., "try-error")) {
    auc. <- NaN
  }

  if (verbosity > 0L) {
    msg("AUC =", auc.)
  }
  auc.
} # rtemis::auc


#' Area under the Curve by pairwise concordance
#'
#' Get the Area under the ROC curve to assess classifier performance using pairwise concordance
#'
#' The first level of `true.labels` must be the positive class, and high numbers in
#' `estimated.score` should correspond to the positive class.
#'
#' @param estimated.score Float, Vector: Probabilities or model scores
#' (e.g. c(.32, .75, .63), etc)
#' @param true.labels True labels of outcomes (e.g. c(0, 1, 1))
#' @param verbosity Integer: Verbosity level.
#'
#' @examples
#' \dontrun{
#' true.labels <- factor(c("a", "a", "a", "b", "b", "b", "b"))
#' estimated.score <- c(0.7, 0.55, 0.45, 0.25, 0.6, 0.7, 0.2)
#' auc_pairs(estimated.score, true.labels, verbosity = 1L)
#' }
#'
#' @keywords internal
#' @noRd
auc_pairs <- function(estimated.score, true.labels, verbosity = 1L) {
  true.labels <- as.factor(true.labels)
  true.levels <- levels(true.labels)
  n.levels <- length(true.levels)
  if (n.levels == 2) {
    outer.diff <- outer(
      estimated.score[true.labels == true.levels[1]],
      estimated.score[true.labels == true.levels[2]],
      "-"
    )
    .auc <- mean((outer.diff > 0) + .5 * (outer.diff == 0))
  } else {
    cli::cli_abort(
      "Multiclass AUC does not have a unique definition and is not yet implemented"
    )
  }
  if (verbosity > 0L) {
    msg("Positive class:", true.levels[1])
    msg("AUC =", .auc)
  }
  invisible(.auc)
} # rtemis::auc_pairs


#' Brier_Score
#'
#' Calculate the Brier_Score for classification:
#'
#' \deqn{BS = \frac{1}{N} \sum_{i=1}^{N} (y_i - p_i)^2}{BS = 1/N * sum_{i=1}^{N} (y_i - p_i)^2}
#'
#' @param true_int Integer vector, {0, 1}: True labels
#' @param predicted_prob Numeric vector, \[0, 1\]: predicted probabilities
#'
#' @author EDG
#' @keywords internal
#' @noRd
brier_score <- function(true_int, predicted_prob) {
  true_int <- clean_int(true_int)
  check_float01inc(predicted_prob)
  mean((true_int - predicted_prob)^2)
} # /rtemis::brier_score

#' Convert labels to integers
#'
#' Convert factor labels to integers where the positive class is 1 and the negative class is 0.
#'
#' @param x Factor: True labels.
#' @param binclasspos Integer: Position of the factor level which is the positive class (binary classification only).
#'
#' @return Integer vector: 0, 1 where 1 is the positive class as defined by binclasspos.
#'
#' @author EDG
#' @keywords internal
#' @noRd
labels2int <- function(x, binclasspos = 2L) {
  stopifnot(is.factor(x))
  # Convert factor to 0, 1 where 1 is the positive class as defined by binclasspos
  if (binclasspos == 1L) {
    xi <- 2L - as.integer(x)
  } else {
    xi <- as.integer(x) - 1L
  }
  xi
} # rtemis::labels2int

# classification_metrics() ----
#' Classification Metrics
#'
#' @details
#' Note that auc_method = "pROC" is the only one that will output an AUC even if
#' one or more predicted probabilities are NA.
#'
#' @param true_labels Factor: True labels.
#' @param predicted_labels Factor: predicted values.
#' @param predicted_prob Numeric vector: predicted probabilities.
#' @param binclasspos Integer: Factor level position of the positive class in binary classification.
#' @param calc_auc Logical: If TRUE, calculate AUC. May be slow in very large datasets.
#' @param calc_brier Logical: If TRUE, calculate Brier_Score.
#' @param auc_method Character: "lightAUC", "pROC", "ROCR".
#' @param sample Character: Sample name.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `ClassificationMetrics` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume positive class is "b"
#' true_labels <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
#' predicted_labels <- factor(c("a", "b", "a", "b", "b", "a", "b", "b", "b", "a"))
#' predicted_prob <- c(0.3, 0.55, 0.45, 0.75, 0.57, 0.3, 0.8, 0.63, 0.62, 0.39)
#'
#' classification_metrics(true_labels, predicted_labels, predicted_prob)
#' classification_metrics(true_labels, predicted_labels, 1 - predicted_prob, binclasspos = 1L)
#' }
classification_metrics <- function(
  true_labels,
  predicted_labels,
  predicted_prob = NULL,
  binclasspos = 2L,
  calc_auc = TRUE,
  calc_brier = TRUE,
  auc_method = "lightAUC",
  sample = character(),
  verbosity = 0L
) {
  # Checks ----
  # Binary class probabilities only for now
  if (length(predicted_prob) > length(true_labels)) {
    predicted_prob <- NULL
  }
  n_classes <- nlevels(true_labels)

  # Check same levels in
  if (!all(levels(true_labels) == levels(predicted_labels))) {
    cli::cli_abort(
      "True and predicted labels must have the same levels, in the same order.",
      "\n     levels(true_labels): ",
      paste(levels(true_labels), collapse = ", "),
      "\nlevels(predicted_labels): ",
      paste(levels(predicted_labels), collapse = ", ")
    )
  }

  # Positive class ----
  # For confusion table, make positive class the first factor level
  if (n_classes == 2 && binclasspos == 2L) {
    true_labels <- factor(true_labels, levels = rev(levels(true_labels)))
    predicted_labels <- factor(
      predicted_labels,
      levels = rev(levels(predicted_labels))
    )
  }
  true_levels <- levels(true_labels)

  # Levels already set so that the first level is the positive class
  Positive_Class <- if (n_classes == 2) true_levels[1] else NA
  if (verbosity > 0L) {
    if (n_classes == 2) {
      msg(
        "There are two outcome classes:",
        highlight(paste(rev(true_levels), collapse = ", "))
      )
      msg("        The positive class is:", highlight(Positive_Class))
    } else {
      msg(
        "There are",
        n_classes,
        "classes:",
        highlight(paste(rev(true_levels), collapse = ", "))
      )
    }
  }
  tbl <- table(true_labels, predicted_labels)
  # attr(tbl, "dimnames") <- list(Reference = true_levels, Predicted = true_levels)
  names(attributes(tbl)[["dimnames"]]) <- c("Reference", "Predicted")

  Class <- list()
  Overall <- list()
  Class[["Totals"]] <- rowSums(tbl)
  Class[["Predicted_totals"]] <- colSums(tbl)
  Total <- sum(tbl)
  Class[["Hits"]] <- diag(tbl)
  # Class[["Misses"]] <- Class[["Totals"]] - Class[["Hits"]]
  Class[["Sensitivity"]] <- Class[["Hits"]] / Class[["Totals"]]
  Class[["Condition_negative"]] <- Total - Class[["Totals"]]
  Class[["True_negative"]] <- Total -
    Class[["Predicted_totals"]] -
    (Class[["Totals"]] - Class[["Hits"]])
  Class[["Specificity"]] <- Class[["True_negative"]] /
    Class[["Condition_negative"]]
  Class[["Balanced_Accuracy"]] <- .5 *
    (Class[["Sensitivity"]] + Class[["Specificity"]])
  # PPV = true positive / predicted condition positive
  Class[["PPV"]] <- Class[["Hits"]] / Class[["Predicted_totals"]]
  # NPV  = true negative / predicted condition negative
  Class[["NPV"]] <- Class[["True_negative"]] /
    (Total - Class[["Predicted_totals"]])
  Class[["F1"]] <- 2 *
    (Class[["PPV"]] * Class[["Sensitivity"]]) /
    (Class[["PPV"]] + Class[["Sensitivity"]])

  # Binary vs Multiclass ----
  if (n_classes == 2) {
    Overall[["Sensitivity"]] <- Class[["Sensitivity"]][1]
    Overall[["Specificity"]] <- Class[["Specificity"]][1]
    Overall[["Balanced_Accuracy"]] <- Class[["Balanced_Accuracy"]][1]
    Overall[["PPV"]] <- Class[["PPV"]][1]
    Overall[["NPV"]] <- Class[["NPV"]][1]
    Overall[["F1"]] <- Class[["F1"]][1]
  } else {
    Overall[["Balanced_Accuracy"]] <- mean(Class[["Sensitivity"]])
    Overall[["F1"]] <- mean(Class[["F1"]])
  }
  Overall[["Accuracy"]] <- sum(Class[["Hits"]]) / Total

  # Probability-based metrics ----
  if (!is.null(predicted_prob) && n_classes == 2L) {
    # Positive class has been set to first level
    true_int <- 2L - as.integer(true_labels)
    if (calc_auc) {
      Overall[["AUC"]] <- auc(
        true_int = true_int,
        predicted_prob = predicted_prob,
        method = auc_method
      )
    }
    if (calc_brier) {
      Overall[["Brier_Score"]] <- brier_score(true_int, predicted_prob)
    }
    # Overall[["Log loss"]] <- logloss(true_int, predicted_prob)
  }

  # Outro ----
  Overall <- as.data.frame(do.call(cbind, Overall))
  rownames(Overall) <- "Overall"
  Class <- (data.frame(
    Sensitivity = Class[["Sensitivity"]],
    Specificity = Class[["Specificity"]],
    Balanced_Accuracy = Class[["Balanced_Accuracy"]],
    PPV = Class[["PPV"]],
    NPV = Class[["NPV"]],
    F1 = Class[["F1"]]
  ))

  ClassificationMetrics(
    sample = sample,
    Confusion_Matrix = tbl,
    Overall = Overall,
    Class = Class,
    Positive_Class = Positive_Class
  )
} # /rtemis::classification_metrics


# regression_metrics() ----
#' Regression Metrics
#'
#' @param true Numeric vector: True values.
#' @param predicted Numeric vector: Predicted values.
#' @param na.rm Logical: If TRUE, remove NA values before computation.
#' @param sample Character: Sample name (e.g. "training", "test").
#'
#' @return `RegressionMetrics` object.
#'
#' @author EDG
#' @export
regression_metrics <- function(
  true,
  predicted,
  na.rm = TRUE,
  sample = character()
) {
  RegressionMetrics(
    MAE = mae(true, predicted, na.rm = na.rm),
    MSE = mse(true, predicted, na.rm = na.rm),
    RMSE = rmse(true, predicted, na.rm = na.rm),
    Rsq = rsq(true, predicted),
    sample = sample
  )
} # /rtemis::regression_metrics
