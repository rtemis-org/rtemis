# draw_volcano
# ::rtemis::
# 2022 EDG rtemis.org
# allow custom grouping

# References
# https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js

#' Volcano Plot
#'
#' @param x Numeric vector: Input values, e.g. log2 fold change, coefficients, etc.
#' @param pvals Numeric vector: p-values.
#' @param xnames Character vector: `x` names.
#' @param group Factor: Used to color code points. If NULL, significant points
#' below `x_thresh`, non-significant points, and significant points
#' above `x_thresh` will be plotted with the first, second and third
#' color of `palette`.
#' @param x_thresh Numeric x-axis threshold separating low from high.
#' @param p_thresh Numeric: p-value threshold of significance.
#' @param p_adjust_method Character: p-value adjustment method.
#' "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#' Default = "holm". Use "none" for raw p-values.
#' @param p_transform function.
#' @param legend Logical: If TRUE, show legend. Will default to FALSE, if
#' `group = NULL`, otherwise to TRUE.
#' @param legend_lo Character: Legend to annotate significant points below the
#' `x_thresh`.
#' @param legend_hi Character: Legend to annotate significant points above the
#' `x_thresh`.
#' @param label_lo Character: label for low values.
#' @param label_hi Character: label for high values.
#' @param main Character: Main title.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param margin Named list of plot margins.
#' Default = `list(b = 65, l = 65, t = 50, r = 10, pad = 0)`.
#' @param xlim Numeric vector, length 2: x-axis limits.
#' @param ylim Numeric vector, length 2: y-axis limits.
#' @param alpha Numeric: point transparency.
#' @param hline Numeric: If defined, draw a horizontal line at this y value.
#' @param hline_col Color for `hline`.
#' @param hline_width Numeric: Width for `hline`.
#' @param hline_dash Character: Type of line to draw: "solid", "dot", "dash",
#' "longdash", "dashdot", or "longdashdot".
#' @param hline_annotate Character: Text of horizontal line annotation if
#' `hline` is set.
#' @param hline_annotation_x Numeric: x position to place annotation with paper
#' as reference. 0: to the left of the plot area; 1: to the right of the plot area.
#' @param annotate Logical: If TRUE, annotate significant points.
#' @param annotate_col Color for annotations.
#' @param theme `Theme` object.
#' @param font_size Integer: Font size.
#' @param palette Character: Name of \pkg{rtemis} palette to use.
#' @param legend_x_lo Numeric: x position of `legend_lo`.
#' @param legend_x_hi Numeric: x position of `legend_hi`.
#' @param legend_y Numeric: y position for `legend_lo` and `legend_hi`.
#' @param annotate_n Integer: Number of significant points to annotate.
#' @param ax_lo Numeric: Sets the x component of the arrow tail about the arrow head for
#' significant points below `x_thresh`.
#' @param ay_lo Numeric: Sets the y component of the arrow tail about the arrow head for
#' significant points below `x_thresh`.
#' @param ax_hi Numeric: Sets the x component of the arrow tail about the arrow head for
#' significant points above `x_thresh`.
#' @param ay_hi Numeric: Sets the y component of the arrow tail about the arrow head for
#' significant points above `x_thresh`.
#' @param annotate_alpha Numeric: Transparency for annotations.
#' @param hovertext Character vector: Text to display on hover.
#' @param displayModeBar Logical: If TRUE, display plotly mode bar.
#' @param filename Character: Path to save the plot image.
#' @param file_width Numeric: Width of the saved plot image.
#' @param file_height Numeric: Height of the saved plot image.
#' @param file_scale Numeric: Scale of the saved plot image.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to [draw_scatter].
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' set.seed(2019)
#' x <- rnormmat(500, 500)
#' y <- x[, 3] + x[, 5] - x[, 9] + x[, 15] + rnorm(500)
#' mod <- massGLM(y, x)
#' draw_volcano(mod$summary$`Coefficient y`, mod$summary$`p_value y`)
draw_volcano <- function(
  x,
  pvals,
  xnames = NULL,
  group = NULL,
  x_thresh = 0,
  p_thresh = .05,
  p_adjust_method = c(
    "holm",
    "hochberg",
    "hommel",
    "bonferroni",
    "BH",
    "BY",
    "fdr",
    "none"
  ),
  p_transform = function(x) -log10(x),
  legend = NULL,
  legend_lo = NULL,
  legend_hi = NULL,
  label_lo = "Low",
  label_hi = "High",
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  xlim = NULL,
  ylim = NULL,
  alpha = NULL,
  hline = NULL,
  hline_col = NULL,
  hline_width = 1,
  hline_dash = "solid",
  hline_annotate = NULL,
  hline_annotation_x = 1,
  theme = choose_theme(),
  annotate = TRUE,
  annotate_col = theme[["labs_col"]],
  font_size = 16,
  palette = NULL,
  legend_x_lo = NULL,
  legend_x_hi = NULL,
  legend_y = .97,
  annotate_n = 7L,
  ax_lo = NULL, # 40,
  ay_lo = NULL,
  ax_hi = NULL, # -40,
  ay_hi = NULL,
  annotate_alpha = .7,
  hovertext = NULL,
  displayModeBar = "hover",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  verbosity = 1L,
  ...
) {
  xname <- deparse(substitute(x))
  p_adjust_method <- match.arg(p_adjust_method)
  filt <- !is.na(x) & !is.na(pvals)
  if (is.null(xnames)) {
    xnames <- names(x)
  } else {
    xnames <- xnames
  }
  if (!is.null(group)) {
    group <- group[filt]
  }
  x <- x[filt]
  pvals <- pvals[filt]
  if (is.null(xnames)) {
    xnames <- paste("Feature", seq_along(x))
  }
  if (is.null(legend)) {
    legend <- !is.null(group)
  }

  p_adjusted <- p.adjust(pvals, method = p_adjust_method)
  index_ltpthresh <- p_adjusted < p_thresh
  p_transformed <- p_transform(p_adjusted)
  if (is.null(xlab)) {
    xlab <- labelify(xname)
  }

  # Default to lo - ns - hi groups
  if (is.null(group)) {
    group <- rep("NS", length(pvals))
    group[index_ltpthresh & x < x_thresh] <- label_lo
    group[index_ltpthresh & x > x_thresh] <- label_hi
    group <- factor(group, levels = c(label_lo, "NS", label_hi))
    if (is.null(palette)) {
      palette <- list("#43A4AC", "#7f7f7f", "#FA9860")
    }
  }

  group.counts <- table(group)
  include <- group.counts > 0
  if (verbosity > 0L) {
    cat("Group counts:\n")
    print(group.counts)
  }

  if (is.null(palette)) {
    palette <- rtpalette(rtemis_palette)
  }

  # Theme ----
  check_is_S7(theme, Theme)

  # y-axis label ----
  if (is.null(ylab)) {
    ylab <- fn2label(p_transform, "p-value")
    if (p_adjust_method != "none") {
      ylab <- paste0(ylab, " (", p_adjust_method, "-corrected)")
    }
  }

  # Plot ----
  if (is.null(hovertext)) {
    hovertext <- xnames
  }
  plt <- draw_scatter(
    x,
    p_transformed,
    main = main,
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    alpha = alpha,
    theme = theme,
    margin = margin,
    legend = legend,
    group = group,
    palette = palette[include],
    hovertext = hovertext,
    ...
  )

  # High - Low legend ----
  autolegend_x_lo <- is.null(legend_x_lo)
  if (autolegend_x_lo) {
    # legend.x.lo <- Filter(\(x) x < x.thresh, x) |> range() |> diff() * -.2 + x.thresh
    legend_x_lo <- x_thresh - abs(diff(c(x_thresh, min(x, na.rm = TRUE)))) * .2
  }

  autolegend_x_hi <- is.null(legend_x_hi)
  if (autolegend_x_hi) {
    # legend.x.hi <- Filter(\(x) x > x.thresh, x) |> range() |> diff() * .2 + x.thresh
    legend_x_hi <- x_thresh + abs(diff(c(x_thresh, max(x, na.rm = TRUE)))) * .2
  }

  legxdiff <- legend_x_hi - legend_x_lo

  if (autolegend_x_lo) {
    legend_x_lo <- x_thresh - legxdiff / 2
  }
  if (autolegend_x_hi) {
    legend_x_hi <- x_thresh + legxdiff / 2
  }

  if (group.counts[1] > 0 && !is.null(legend_lo)) {
    plt <- plt |>
      plotly::add_annotations(
        x = legend_x_lo,
        y = legend_y,
        text = legend_lo,
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          color = palette[[1]],
          family = theme[["font_family"]],
          size = font_size
        )
      )
  }

  if (group.counts[3] > 0 && !is.null(legend_hi)) {
    plt <- plt |>
      plotly::add_annotations(
        x = legend_x_hi,
        y = legend_y,
        text = legend_hi,
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          color = palette[[3]],
          family = theme[["font_family"]],
          size = font_size
        )
      )
  }

  # Annotations ----
  if (annotate) {
    index_ltxthresh <- x < x_thresh
    index_gtxthresh <- x > x_thresh

    index_lo <- index_ltpthresh & index_ltxthresh
    index_hi <- index_ltpthresh & index_gtxthresh
    annotate_n_lo <- annotate_n_hi <- annotate_n
    if (sum(index_lo) < annotate_n) {
      annotate_n_lo <- sum(index_lo)
    }
    if (sum(index_hi) < annotate_n) {
      annotate_n_hi <- sum(index_hi)
    }

    if (annotate_n_lo > 0) {
      lo_ord <- order(pvals[index_lo])
      lo_x <- x[index_lo][lo_ord[seq_len(annotate_n_lo)]]
      lo_pval <- p_transformed[index_lo][lo_ord[seq_len(annotate_n_lo)]]
      lo_name <- xnames[index_lo][lo_ord[seq_len(annotate_n_lo)]]

      if (is.null(ay_lo)) {
        if (is.null(ay_lo)) {
          ay_lo <- drange(order(lo_pval), 30, -30)
        }
      }
      if (is.null(ax_lo)) {
        ax_lo <- 5 + 5 * annotate_n_lo
      }
      plt <- plt |>
        plotly::add_annotations(
          x = lo_x,
          y = lo_pval,
          text = lo_name,
          arrowhead = 4,
          arrowcolor = adjustcolor(theme[["fg"]], .33),
          arrowsize = .5,
          arrowwidth = 1,
          ax = ax_lo,
          ay = ay_lo,
          xanchor = "left",
          font = list(
            size = 16,
            family = theme[["font_family"]],
            color = adjustcolor(theme[["fg"]], annotate_alpha)
          )
        )
    }

    # Annotate 10 most significant increasing
    if (annotate_n_hi > 0) {
      hi_ord <- order(pvals[index_ltpthresh & index_gtxthresh])
      hi_x <- x[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(
        annotate_n_hi
      )]]
      hi_pval <- p_transformed[
        index_ltpthresh & index_gtxthresh
      ][hi_ord[seq_len(annotate_n_hi)]]
      hi_name <- xnames[index_ltpthresh & index_gtxthresh][hi_ord[seq_len(
        annotate_n_hi
      )]]

      if (is.null(ay_hi)) {
        ay_hi <- drange(order(hi_pval), 50, -50)
      }
      if (is.null(ax_hi)) {
        ax_hi <- -5 - 5 * annotate_n_hi
      }
      plt <- plt |>
        plotly::add_annotations(
          x = hi_x,
          y = hi_pval,
          text = hi_name,
          arrowhead = 4,
          arrowcolor = adjustcolor(theme[["fg"]], .33),
          arrowsize = .5,
          arrowwidth = 1,
          ax = ax_hi,
          ay = ay_hi,
          xanchor = "right",
          font = list(
            size = 16,
            family = theme[["font_family"]],
            color = adjustcolor(theme[["fg"]], annotate_alpha)
          )
        )
    }
  }

  # hline ----
  if (!is.null(hline)) {
    if (is.null(hline_col)) {
      hline_col <- theme[["fg"]]
    }
    hline_col <- recycle(hline_col, hline)
    hline_width <- recycle(hline_width, hline)
    hline_dash <- recycle(hline_dash, hline)
    hlinel <- lapply(seq_along(hline), function(i) {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = hline[i],
        y1 = hline[i],
        line = list(
          color = hline_col[i],
          width = hline_width[i],
          dash = hline_dash[i]
        )
      )
    })
    plt <- plotly::layout(plt, shapes = hlinel)

    # Annotate horizontal lines on the right border of the plot
    if (!is.null(hline_annotate)) {
      plt <- plt |>
        plotly::add_annotations(
          xref = "paper",
          yref = "y",
          xanchor = "right",
          yanchor = "bottom",
          x = hline_annotation_x,
          y = hline,
          text = hline_annotate,
          font = list(
            family = theme[["font_family"]],
            size = font_size,
            color = annotate_col
          ),
          showarrow = FALSE
        )
    }
  }

  plt |> plotly::config(toImageButtonOptions = list(format = "svg"))

  # Config ----
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = "svg",
      width = file_width,
      height = file_height,
      scale = file_scale
    )
  )

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

  plt
} # /rtemis::draw_volcano
