# draw_confusion.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Plot confusion matrix
#'
#' @param x `ClassificationMetrics` object produced by [classification_metrics] or confusion matrix
#' where rows are the reference and columns are the estimated classes. For binary classification,
#' the first row and column are the positive class.
#' @param xlab Character: x-axis label. Default is "Predicted".
#' @param ylab Character: y-axis label. Default is "Reference".
#' @param true_col Color for true positives & true negatives.
#' @param false_col Color for false positives & false negatives.
#' @param font_size Integer: font size.
#' @param main Character: plot title.
#' @param main_y Numeric: y position of the title.
#' @param main_yanchor Character: y anchor of the title.
#' @param theme `Theme` object.
#' @param margin List: Plot margins.
#' @param filename Character: file name to save the plot. Default is NULL.
#' @param file_width Numeric: width of the file. Default is 500.
#' @param file_height Numeric: height of the file. Default is 500.
#' @param file_scale Numeric: scale of the file. Default is 1.
#'
#' @return `plotly` object.
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
#' metrics <- classification_metrics(true_labels, predicted_labels, predicted_prob)
#' draw_confusion(metrics)
#' }
draw_confusion <- function(
  x,
  xlab = "Predicted",
  ylab = "Reference",
  true_col = "#43A4AC",
  false_col = "#FA9860",
  font_size = 18,
  main = NULL,
  main_y = 1,
  main_yanchor = "bottom",
  theme = choose_theme(),
  margin = list(l = 20, r = 5, b = 5, t = 20),
  # write to file
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
) {
  # Input ----
  if (S7_inherits(x, ClassificationMetrics)) {
    x <- x@metrics[["Confusion_Matrix"]]
  }

  if (is.null(dim(x)) || ncol(x) != nrow(x)) {
    cli::cli_abort("The confusion matrix must be a square matrix.")
  }

  # Metrics ----
  nclasses <- ncol(x)
  total <- sum(x)
  class_totals <- rowSums(x)
  condition_negative <- total - class_totals
  predicted_totals <- colSums(x)
  hits <- diag(x)
  misses <- class_totals - hits
  class_sensitivity <- hits / class_totals
  true_negative <- total - predicted_totals - (class_totals - hits)
  class_specificity <- true_negative / condition_negative
  class_balancedAccuracy <- .5 * (class_sensitivity + class_specificity)
  # PPV = true positive / predicted condition positive
  class_ppv <- hits / predicted_totals
  # NPV  = true negative / predicted condition negative
  class_npv <- true_negative / (total - predicted_totals)

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]], theme[["grid_alpha"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  legend_col <- labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])

  # Colors ----
  pos_color <- colorRamp(colors = c(theme[["bg"]], true_col))
  neg_color <- colorRamp(colors = c(theme[["bg"]], false_col))

  # Fonts ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = theme[["labs_col"]]
  )

  # Plot ----
  plt <- plotly::plot_ly(
    type = "scatter",
    mode = "lines"
  )

  # Add colored tiles & counts ----
  for (i in seq_len(nclasses)) {
    for (j in seq_len(nclasses)) {
      plt <- make_plotly_conf_tile(
        p = plt,
        x = x,
        i = i,
        j = j,
        pos_color = pos_color,
        neg_color = neg_color,
        font_size = font_size,
        theme = theme
      )
    }
  }

  # Layout ----
  plt <- plotly::layout(
    plt,
    xaxis = list(
      side = "above",
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      autorange = "reversed",
      scaleanchor = "x",
      scaleratio = 1
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      ),
      xref = "paper",
      x = theme[["main_adj"]],
      yref = "paper",
      y = main_y,
      yanchor = main_yanchor
    ),
    showlegend = FALSE,
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin
  ) # /layout

  # Class labels ----
  # Add class labels above and to the left of the plot
  # Left
  plt <- plotly::add_annotations(
    plt,
    x = rep(-0.125, nclasses),
    y = seq_len(nclasses) - 0.5,
    text = colnames(x),
    # textposition = "middle right",
    font = f,
    showarrow = FALSE,
    textangle = -90
  )
  # Above
  plt <- plotly::add_annotations(
    plt,
    x = seq_len(nclasses) - 0.5,
    y = rep(-0.125, nclasses),
    text = colnames(x),
    # textposition = "bottom center",
    font = f,
    showarrow = FALSE
  )

  # x-axis label "Predicted"
  plt <- plotly::add_annotations(
    plt,
    x = nclasses / 2,
    y = ifelse(nclasses == 2, -.3, -0.5),
    text = xlab,
    font = f,
    showarrow = FALSE
  )

  # y-axis label "Reference"
  plt <- plotly::add_annotations(
    plt,
    x = ifelse(nclasses == 2, -.3, -0.5),
    y = nclasses / 2,
    text = ylab,
    font = f,
    showarrow = FALSE,
    textangle = -90
  )

  # Metrics ----
  if (nclasses == 2) {
    # Sens./Spec. ----
    # Rect: Sens./Spec. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses, nclasses + 0.3, nclasses + 0.3, nclasses),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .075),
      showlegend = FALSE
    )

    # Text: Sens. & Spec.
    plt <- plotly::add_annotations(
      plt,
      x = rep(nclasses + 0.15, 2),
      y = c(.5, 1.5),
      text = paste0(
        c("Sensitivity\n", "Specificity\n"),
        c(ddSci(class_sensitivity[1], 3), ddSci(class_specificity[1], 3))
      ),
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # PPV/NPV ----
    # Rect: PPV/NPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(nclasses, nclasses, nclasses + .3, nclasses + .3, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .075),
      showlegend = FALSE
    )

    # Text: PPV & NPV
    plt <- plotly::add_annotations(
      plt,
      x = c(.5, 1.5),
      y = rep(nclasses + 0.15, 2),
      text = paste0(
        c("PPV\n", "NPV\n"),
        c(ddSci(class_ppv[1], 3), ddSci(class_npv[1], 3))
      ),
      font = f,
      showarrow = FALSE
    )
  } else {
    # PPV ----
    # Text: "PPV" at bottom left corner
    plt <- plotly::add_annotations(
      plt,
      x = -0.05,
      y = nclasses + .1,
      xanchor = "right",
      yanchor = "middle",
      text = "PPV",
      font = f,
      showarrow = FALSE
    )

    # Rect: PPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(nclasses, nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .075),
      showlegend = FALSE
    )

    # Text: Per-class PPV
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = i - 0.5,
        y = nclasses + .1,
        text = ddSci(class_ppv[i], 3),
        font = f,
        showarrow = FALSE
      )
    }

    # NPV ----
    # Label: "NPV" at bottom left corner
    plt <- plotly::add_annotations(
      plt,
      x = -0.05,
      y = nclasses + .3,
      xanchor = "right",
      yanchor = "middle",
      text = "NPV",
      font = f,
      showarrow = FALSE
    )

    # Rect: NPV bg
    plt <- plotly::add_trace(
      plt,
      x = c(0, nclasses, nclasses, 0, 0),
      y = c(
        nclasses + 0.2,
        nclasses + 0.2,
        nclasses + 0.4,
        nclasses + 0.4,
        nclasses + 0.2
      ),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .05),
      showlegend = FALSE
    )

    # Text: Per-class NPV
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = i - 0.5,
        y = nclasses + .3,
        text = ddSci(class_npv[i], 3),
        font = f,
        showarrow = FALSE
      )
    }

    # Sensitivity ----
    # Label: "Sens." top right vertically
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.1,
      y = -.05,
      yanchor = "bottom",
      text = "Sens.",
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # Rect: Sens. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses, nclasses + 0.2, nclasses + 0.2, nclasses),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .075),
      showlegend = FALSE
    )

    # Text: Per-class Sens.
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = nclasses + 0.1,
        y = i - 0.5,
        text = ddSci(class_sensitivity[i], 3),
        font = f,
        showarrow = FALSE,
        textangle = -90
      )
    }

    # Specificity ----
    # Label: "Spec." top right vertically
    plt <- plotly::add_annotations(
      plt,
      x = nclasses + 0.3,
      y = -.05,
      yanchor = "bottom",
      text = "Spec.",
      font = f,
      showarrow = FALSE,
      textangle = -90
    )

    # Rect: Spec. bg
    plt <- plotly::add_trace(
      plt,
      x = c(nclasses + 0.2, nclasses + 0.4, nclasses + 0.4, nclasses + 0.2),
      y = c(0, 0, nclasses, nclasses),
      line = list(color = "transparent"),
      fill = "toself",
      fillcolor = plotly::toRGB(theme[["fg"]], alpha = .05),
      showlegend = FALSE
    )

    # Text: Per-class Spec.
    for (i in seq_len(nclasses)) {
      plt <- plotly::add_annotations(
        plt,
        x = nclasses + 0.3,
        y = i - 0.5,
        text = ddSci(class_specificity[i], 3),
        font = f,
        showarrow = FALSE,
        textangle = -90
      )
    }
  }

  # Balanced Accuracy ----
  # Rect: BA bg
  ba_pad <- ifelse(nclasses == 2, 0.3, 0.4)
  plt <- plotly::add_trace(
    plt,
    x = c(nclasses, nclasses + ba_pad, nclasses + ba_pad, nclasses),
    y = c(nclasses, nclasses, nclasses + ba_pad, nclasses + ba_pad),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = plotly::toRGB(theme[["fg"]], alpha = .025),
    showlegend = FALSE
  )

  # Text: Balanced accuracy
  ba_pad <- ifelse(nclasses == 2, 0.15, 0.2)
  ba <- ifelse(
    nclasses == 2,
    class_balancedAccuracy[1],
    mean(class_balancedAccuracy)
  )
  plt <- plotly::add_annotations(
    plt,
    x = nclasses + ba_pad,
    y = nclasses + ba_pad,
    xanchor = "center",
    yanchor = "middle",
    text = paste0("BA\n", ddSci(ba, 3)),
    font = f,
    showarrow = FALSE
  )

  # Disable hoverinfo
  plt <- plotly::style(plt, hoverinfo = "none")

  # Write to file ----
  if (!is.null(filename)) {
    export_plotly(
      plt,
      filename = filename,
      width = file_width,
      height = file_height,
      scale = file_scale
    )
  } # /export_plotly

  return(plt)
} # /rtemis::draw_confusion

make_plotly_conf_tile <- function(
  p,
  x,
  i,
  j,
  pos_color,
  neg_color,
  font_size,
  theme,
  xref = "x",
  yref = "y"
) {
  val <- x[i, j] / sum(x[i, ])
  col <- if (i == j) {
    pos_color(val)
  } else {
    neg_color(val)
  }
  col <- rgb(col[1], col[2], col[3], maxColorValue = 255)
  # Add colored tile
  p <- plotly::add_trace(
    p,
    x = c(j - 1, j - 1, j, j, j - 1),
    y = c(i, i - 1, i - 1, i, i),
    line = list(color = "transparent"),
    fill = "toself",
    fillcolor = col
  )
  # Add text
  p <- plotly::add_trace(
    p,
    x = j - 0.5,
    y = i - 0.5,
    mode = "text",
    text = paste0("<b>", x[i, j], "</b>"),
    textposition = "middle center",
    textfont = list(
      family = theme[["font_family"]],
      color = ifelse(val > 0.5, theme[["bg"]], theme[["fg"]]),
      size = font_size
    ),
    showlegend = FALSE
  )

  return(p)
} # /rtemis::make_plotly_conf_tile
