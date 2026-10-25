# roc_curve.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% roc_curve ----
#' ROC curve coordinates
#'
#' Compute the points of one or more ROC curves from true labels and predicted
#' probabilities, returning tidy `(class, fpr, tpr, auc)` rows. The rows serve any consumer of the
#' curve data, such as a client that renders the curve.
#'
#' Binary problems yield a single curve for the positive class (the second
#' level of `true_labels`, rtemis convention); multiclass problems yield one
#' one-vs-rest curve per class. Points are ordered along the curve; pass
#' `max_points` to down-sample very long curves (one vertex per distinct score)
#' to a compact, smooth line.
#'
#' @param true_labels Factor: True outcome labels.
#' @param predicted_prob Numeric vector or matrix \[0, 1\]: Predicted
#'   probabilities for the positive class (second level of `true_labels`), as a
#'   vector or a one-column matrix. For multiclass, a matrix with exactly one
#'   column per class, in factor-level order (rtemis's convention); the columns
#'   are labeled with the factor levels regardless of any names on the input.
#' @param max_points Optional Integer \[2, Inf): Cap on the number of vertices
#'   per curve. `NULL` keeps full resolution.
#'
#' @return `data.frame` with columns `class`, `fpr`, `tpr`, `auc` (the AUC is
#'   constant within a class group, repeated per vertex). Zero rows when no
#'   curve is defined.
#'
#' @author EDG
#' @export
#' @examples
#' true_labels <- factor(c("A", "B", "A", "A", "B", "A", "B", "B", "A", "B"))
#' predicted_prob <- c(0.1, 0.4, 0.35, 0.8, 0.65, 0.2, 0.9, 0.55, 0.3, 0.7)
#' roc_curve(true_labels, predicted_prob)
roc_curve <- function(
  true_labels,
  predicted_prob,
  max_points = NULL
) {
  check_dependencies("pROC")
  true_labels <- as.factor(true_labels)
  class_levels <- levels(true_labels)
  n_classes <- length(class_levels)
  if (n_classes < 2L) {
    rtemis.core::abort(
      "`true_labels` must have at least two levels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Compute one binary ROC curve: `response` is a two-level factor (positive
  # second), `predictor` the positive-class score (higher = more positive).
  one_curve <- function(response, predictor, class_label) {
    predictor <- suppressWarnings(as.numeric(predictor))
    keep <- is.finite(predictor) & !is.na(response)
    response <- droplevels(response[keep])
    predictor <- predictor[keep]
    if (length(predictor) < 2L || nlevels(response) != 2L) {
      return(NULL)
    }
    r <- pROC::roc(
      response = response,
      predictor = predictor,
      levels = levels(response),
      direction = "<",
      quiet = TRUE
    )
    fpr <- 1 - r[["specificities"]]
    tpr <- r[["sensitivities"]]
    ord <- order(fpr, tpr)
    fpr <- fpr[ord]
    tpr <- tpr[ord]
    if (!is.null(max_points) && length(fpr) > max_points) {
      idx <- unique(round(seq(1, length(fpr), length.out = max_points)))
      fpr <- fpr[idx]
      tpr <- tpr[idx]
    }
    data.frame(
      class = class_label,
      fpr = fpr,
      tpr = tpr,
      auc = as.numeric(r[["auc"]]),
      stringsAsFactors = FALSE
    )
  }

  if (n_classes == 2L) {
    # Positive class is the second level. Its score arrives three ways: a bare
    # vector, the one-column matrix `Classification@predicted_prob_*` holds, or
    # a column-per-class matrix whose second column is that score.
    score <- if (is.null(dim(predicted_prob)) || NCOL(predicted_prob) == 1L) {
      predicted_prob
    } else {
      if (NCOL(predicted_prob) != n_classes) {
        rtemis.core::abort(
          "Binary `predicted_prob` matrix must have one column (the positive class) or one column per class, in factor-level order.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      predicted_prob[, 2L]
    }
    out <- one_curve(true_labels, score, class_levels[2L])
  } else {
    if (is.null(dim(predicted_prob)) || NCOL(predicted_prob) != n_classes) {
      rtemis.core::abort(
        "Multiclass `predicted_prob` must be a matrix with one column per class, in factor-level order.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    # Columns are the classes by position; label them with the factor levels so
    # the curve's `class` is unambiguous regardless of any names on the input.
    colnames(predicted_prob) <- class_levels
    # One-vs-rest: each class in turn is the positive case.
    pieces <- lapply(seq_len(n_classes), function(k) {
      response <- factor(
        true_labels == class_levels[k],
        levels = c(FALSE, TRUE)
      )
      one_curve(response, predicted_prob[, k], class_levels[k])
    })
    out <- do.call(rbind, pieces)
  }

  if (is.null(out)) {
    out <- data.frame(
      class = character(0),
      fpr = numeric(0),
      tpr = numeric(0),
      auc = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  out
} # /rtemis::roc_curve


# %% roc_table ----
#' ROC Curves of a Classification Model
#'
#' The ROC curves of a fitted classification model for each sample it stores
#' predicted probabilities for: the data [plot_roc()] draws, as a table.
#'
#' @details
#' Binary models have one curve, for the positive class; multiclass models have
#' one one-vs-rest curve per class. For a resampled model, `variant =
#' "aggregate"` pools the predictions of every outer resample into one curve
#' per sample and class, and `variant = "per_resample"` gives one curve per
#' resample, labeled in the `resample` column. A resample whose probabilities
#' are missing has an undefined curve, with `auc` NA. `omitted` counts the
#' cases whose probability or label is missing.
#'
#' [roc_curve()] computes the curve of supplied labels and probabilities.
#'
#' @param x `Classification` or `ClassificationRes` object.
#' @param sample Optional Character \{"training", "validation", "test"\}: The
#'   sample whose curves to return. `NULL` returns every sample with predicted
#'   probabilities.
#' @param variant Character \{"aggregate", "per_resample"\}: For a resampled
#'   model, pooled curves or one curve per resample. An ordinary model has
#'   aggregate curves only.
#' @param positive Optional Character: Positive class of a binary model. `NULL`
#'   uses the second level of the outcome.
#' @param max_points Optional Integer \[2, Inf): Cap on the number of vertices
#'   per curve. `NULL` keeps every vertex.
#'
#' @return data.table with columns `sample`; `resample`, for `variant =
#'   "per_resample"`; `class`; `fpr`; `tpr`; `auc`, constant within a curve;
#'   and `omitted`.
#'
#' @author EDG
#' @export
#' @examplesIf requireNamespace("rpart", quietly = TRUE)
#' mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
#' roc_table(mod)
roc_table <- function(
  x,
  sample = NULL,
  variant = "aggregate",
  positive = NULL,
  max_points = NULL
) {
  if (!S7_inherits(x, Classification) && !S7_inherits(x, ClassificationRes)) {
    rtemis.core::abort(
      "`x` must be a Classification or ClassificationRes model.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.null(sample)) {
    check_character_scalar(sample)
  }
  if (!is.null(max_points)) {
    max_points <- clean_posint(max_points)
    if (max_points < 2L) {
      rtemis.core::abort(
        "`max_points` must be at least 2.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  curves <- data.table::as.data.table(roc_plot_data(
    x,
    what = sample %||% "all",
    variant = variant,
    positive = positive,
    labelify = FALSE
  ))
  data.table::setnames(curves, c("split", "fold"), c("sample", "resample"))
  # Down-sampled within each curve, keeping its first and last vertex.
  if (!is.null(max_points)) {
    curves <- curves[,
      .SD[unique(round(seq(1, .N, length.out = min(.N, max_points))))],
      by = c("sample", "resample", "class")
    ]
  }
  if (variant == "aggregate") {
    data.table::set(curves, j = "resample", value = NULL)
  }
  data.table::setcolorder(
    curves,
    intersect(
      c("sample", "resample", "class", "fpr", "tpr", "auc", "omitted"),
      names(curves)
    )
  )
  curves[]
} # /rtemis::roc_table
