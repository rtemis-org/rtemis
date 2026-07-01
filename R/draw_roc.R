# draw_roc.R
# ::rtemis::
# 2025- EDG rtemis.org

#' ROC curve coordinates
#'
#' Compute the points of one or more ROC curves from true labels and predicted
#' probabilities, returning tidy `(class, fpr, tpr, auc)` rows rather than a
#' plot. This is the shared engine behind [draw_roc] and is reused anywhere the
#' curve data is needed without plotly (e.g. shipping the curve to a client).
#'
#' Binary problems yield a single curve for the positive class (the second
#' level of `true_labels`, rtemis convention); multiclass problems yield one
#' one-vs-rest curve per class. Points are ordered along the curve; pass
#' `max_points` to down-sample very long curves (one vertex per distinct score)
#' to a compact, smooth line.
#'
#' @param true_labels Factor: True outcome labels.
#' @param predicted_prob Numeric vector \[0, 1\]: Predicted probabilities for the
#'   positive class (second level of `true_labels`). Or, for multiclass, a
#'   matrix of predicted probabilities with exactly one column per class, in
#'   factor-level order (rtemis's convention); the columns are labelled with
#'   the factor levels regardless of any names on the input.
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
    # Positive class is the second level; `predicted_prob` is its score (a
    # vector), or a column-per-class matrix whose second column is that score.
    score <- if (is.null(dim(predicted_prob))) {
      predicted_prob
    } else {
      if (NCOL(predicted_prob) != n_classes) {
        rtemis.core::abort(
          "Binary `predicted_prob` matrix must have one column per class (in factor-level order).",
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


#' Draw ROC curve
#'
#' @param true_labels Factor: True outcome labels.
#' @param predicted_prob Numeric vector \[0, 1\]: Predicted probabilities for the positive class (i.e. second level of outcome).
#' Or, for multiclass, a matrix of predicted probabilities with one column per class.
#' Or, a list of such vectors/matrices to draw multiple ROC curves on the same plot.
#' @param main Character: Main title for the plot.
#' @param theme `Theme` object.
#' @param palette Character vector: Colors to use.
#' @param legend Logical: If TRUE, draw legend.
#' @param legend_title Character: Title for the legend.
#' @param legend_xy Numeric vector: Position of the legend in the form c(x, y).
#' @param legend_xanchor Character: X anchor for the legend.
#' @param legend_yanchor Character: Y anchor for the legend.
#' @param auc_dp Integer: Number of decimal places for AUC values.
#' @param xlim Numeric vector: Limits for the x-axis.
#' @param ylim Numeric vector: Limits for the y-axis.
#' @param diagonal Logical: If TRUE, draw diagonal line.
#' @param diagonal_col Character: Color for the diagonal line.
#' @param axes_square Logical: If TRUE, make axes square.
#' @param filename Character: If provided, save the plot to this file.
#' @param ... Additional arguments passed to [draw_scatter].
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examplesIf interactive()
#' # Binary classification
#' true_labels <- factor(c("A", "B", "A", "A", "B", "A", "B", "B", "A", "B"))
#' predicted_prob <- c(0.1, 0.4, 0.35, 0.8, 0.65, 0.2, 0.9, 0.55, 0.3, 0.7)
#' draw_roc(true_labels, predicted_prob)
draw_roc <- function(
  true_labels,
  predicted_prob,
  main = NULL,
  theme = choose_theme(getOption("rtemis_theme")),
  palette = get_palette(getOption("rtemis_palette")),
  legend = TRUE,
  legend_title = "Group (AUC)",
  legend_xy = c(1, 0),
  legend_xanchor = "right",
  legend_yanchor = "bottom",
  auc_dp = 3L,
  xlim = c(-0.05, 1.05),
  ylim = c(-0.05, 1.05),
  diagonal = TRUE,
  diagonal_col = NULL,
  axes_square = TRUE,
  filename = NULL,
  ...
) {
  # List of probabilities
  probl <- if (!is.list(predicted_prob)) {
    list(predicted_prob)
  } else {
    predicted_prob
  }
  labelsl <- if (!is.list(true_labels)) {
    list(true_labels)
  } else {
    true_labels
  }
  # Check N sets
  if (length(probl) != length(labelsl)) {
    rtemis.core::abort(
      "You must have the same N of sets of `predicted_prob` and `true_labels`.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  # Check lengths of corresponding sets
  # NROW() works for both vectors and matrices
  for (i in seq_along(probl)) {
    if (NROW(probl[[i]]) != length(labelsl[[i]])) {
      rtemis.core::abort(
        "You must have the same N of `predicted_prob` and `true_labels`.",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
  }

  # Build one curve per (set, class) via roc_curve(). Binary sets contribute a
  # single curve (named by the set); multiclass sets contribute one one-vs-rest
  # curve per class (named "<set>: <class>" so the legend stays unambiguous).
  .names <- names(probl)
  if (is.null(.names)) {
    .names <- rep("", length(probl))
  }
  FPR <- list()
  TPR <- list()
  AUC <- list()
  group_names <- character()
  for (i in seq_along(probl)) {
    cv <- roc_curve(labelsl[[i]], probl[[i]])
    cls <- unique(cv[["class"]])
    for (cl in cls) {
      sub <- cv[cv[["class"]] == cl, , drop = FALSE]
      if (nrow(sub) == 0L) {
        next
      }
      FPR[[length(FPR) + 1L]] <- sub[["fpr"]]
      TPR[[length(TPR) + 1L]] <- sub[["tpr"]]
      AUC[[length(AUC) + 1L]] <- sub[["auc"]][1L]
      label_parts <- c(.names[i], if (length(cls) > 1L) cl else NULL)
      label_parts <- label_parts[nzchar(label_parts)]
      group_names[length(group_names) + 1L] <- paste(
        label_parts,
        collapse = ": "
      )
    }
  }

  theme@config[["zerolines"]] <- FALSE
  draw_scatter(
    x = FPR,
    y = TPR,
    xlab = "False Positive Rate",
    ylab = "True Positive Rate",
    main = main,
    theme = theme,
    palette = palette,
    mode = "lines",
    group_names = paste0(group_names, " (", ddSci(unlist(AUC), auc_dp), ")"),
    legend = legend,
    legend_title = legend_title,
    legend_xy = legend_xy,
    legend_xanchor = legend_xanchor,
    legend_yanchor = legend_yanchor,
    xlim = xlim,
    ylim = ylim,
    diagonal = diagonal,
    diagonal_col = diagonal_col,
    axes_square = axes_square,
    order_on_x = FALSE,
    filename = filename,
    ...
  )
} # /rtemis::draw_roc
