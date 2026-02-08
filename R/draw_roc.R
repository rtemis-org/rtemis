# draw_roc.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Draw ROC curve
#'
#' @param true_labels Factor: True outcome labels.
#' @param predicted_prob Numeric vector \[0, 1\]: Predicted probabilities for the positive class (i.e. second level of outcome).
#' Or, for multiclass, a matrix of predicted probabilities with one column per class.
#' Or, a list of such vectors/matrices to draw multiple ROC curves on the same plot.
#' @param multiclass_fill_labels Logical: If TRUE, fill in labels for multiclass ROC curves.
#' If FALSE, column names of `predicted_prob` must match levels of `true_labels`.
#' @param main Character: Main title for the plot.
#' @param theme `Theme` object.
#' @param col Color vector.
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
#'
#' @examplesIf interactive()
#' # Binary classification
#' true_labels <- factor(c("A", "B", "A", "A", "B", "A", "B", "B", "A", "B"))
#' predicted_prob <- c(0.1, 0.4, 0.35, 0.8, 0.65, 0.2, 0.9, 0.55, 0.3, 0.7)
#' draw_roc(true_labels, predicted_prob)
draw_roc <- function(
  true_labels,
  predicted_prob,
  multiclass_fill_labels = TRUE,
  main = NULL,
  theme = choose_theme(),
  col = rtpalette(rtemis_palette),
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
    cli::cli_abort(
      "You must have the same N of sets of `predicted_prob` and `true_labels`."
    )
  }

  # Binary vs. Multiclass
  # Determine number of classes from number of columns in predicted_prob
  # If ncol is NULL, it is binary classification
  n_classes <- unique(sapply(probl, \(x) {
    if (is.null(ncol(x))) {
      2L
    } else {
      ncol(x)
    }
  }))

  if (length(n_classes) > 1) {
    cli::cli_abort(
      "You must have the same number of classes in each set of `predicted_prob`."
    )
  }

  # Check lengths of corresponding sets
  # NROW() works for both vectors and matrices
  for (i in seq_along(probl)) {
    if (NROW(probl[[i]]) != length(labelsl[[i]])) {
      cli::cli_abort(
        "You must have the same N of `predicted_prob` and `true_labels`."
      )
    }
  }

  if (n_classes == 2L) {
    .roc <- lapply(seq_along(probl), \(i) {
      pROC::roc(
        response = labelsl[[i]],
        predictor = probl[[i]],
        levels = levels(labelsl[[i]]),
        direction = "<"
      )
    })
  } else {
    .roc <- lapply(seq_along(probl), \(i) {
      pred <- probl[[i]]
      if (is.null(colnames(pred))) {
        if (multiclass_fill_labels) {
          colnames(pred) <- levels(labelsl[[i]])
        } else {
          cli::cli_abort(
            "For multiclass, `predicted_prob` must have column names matching levels of `true_labels`."
          )
        }
      }
      pROC::multiclass.roc(
        response = labelsl[[i]],
        predictor = pred,
        levels = levels(labelsl[[i]])
      )
    })
  }

  .names <- names(probl)

  if (n_classes == 2L) {
    TPR <- lapply(.roc, \(r) r[["sensitivities"]])
    FPR <- lapply(.roc, \(r) 1 - r[["specificities"]])
    AUC <- lapply(.roc, \(r) r[["auc"]])
  } else {
    TPR <- lapply(.roc, \(r) r[["rocs"]][[1]][["sensitivities"]])
    FPR <- lapply(.roc, \(r) 1 - r[["rocs"]][[1]][["specificities"]])
    AUC <- lapply(.roc, \(r) r[["auc"]])
  }
  names(TPR) <- names(FPR) <- names(AUC) <- .names
  theme@config[["zerolines"]] <- FALSE
  draw_scatter(
    x = FPR,
    y = TPR,
    xlab = "False Positive Rate",
    ylab = "True Positive Rate",
    main = main,
    theme = theme,
    col = col,
    mode = "lines",
    group_names = paste0(.names, " (", ddSci(unlist(AUC), auc_dp), ")"),
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
