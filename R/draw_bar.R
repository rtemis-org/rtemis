# draw_bar.R
# ::rtemis::
# 2019-22 EDG rtemis.org

#' Interactive Barplots
#'
#' Draw interactive barplots using `plotly`
#'
#' @details
#' See [rdocs.rtemis.org/draw](https://rdocs.rtemis.org/draw) for detailed documentation.
#'
#' @param x vector (possibly named), matrix, or data.frame: If matrix or
#' data.frame, rows are groups (can be 1 row), columns are features
#' @param main Character: Main plot title.
#' @param xlab Character: x-axis label.
#' @param ylab  Character: y-axis label.
#' @param alpha Float (0, 1]: Transparency for bar colors.
#' @param theme `Theme` object.
#' @param palette Character vector: Colors to use.
#' @param barmode Character: Type of bar plot to make: "group", "relative",
#' "stack", "overlay". Default = "group". Use
#' "relative" for stacked bars, wich handles negative values correctly,
#' unlike "stack", as of writing.
#' @param group_names Character, vector, length = NROW(x): Group names.
#' Default = NULL, which uses `rownames(x)`
#' @param order_by_val Logical: If TRUE, order bars by increasing value.
#' Only use for single group data.
#' @param ylim Float, vector, length 2: y-axis limits.
#' @param hovernames Character, vector: Optional character vector to show on
#' hover over each bar.
#' @param feature_names Character, vector, length = NCOL(x): Feature names.
#' Default = NULL, which uses `colnames(x)`
#' @param font_size  Float: Font size for all labels.
#' @param legend Logical: If TRUE, draw legend. Default = NULL, and will be
#' turned on if there is more than one feature present
#' @param legend_col Color: Legend text color. Default = NULL, determined by
#' theme
#' @param hline Float: If defined, draw a horizontal line at this y value.
#' @param hline_col Color for `hline`.
#' @param hline_width Float: Width for `hline`.
#' @param hline_dash Character: Type of line to draw: "solid", "dot", "dash",
#' "longdash", "dashdot",
#' or "longdashdot"
#' @param hline_annotate Character: Text of horizontal line annotation if
#' `hline` is set
#' @param hline_annotation_x Numeric: x position to place annotation with paper
#' as reference. 0: to the left of the plot area; 1: to the right of the plot area
#' @param margin Named list: plot margins.
#' @param padding Integer: N pixels to pad plot.
#' @param horizontal Logical: If TRUE, plot bars horizontally
#' @param annotate Logical: If TRUE, annotate stacked bars
#' @param annotate_col Color for annotations
#' @param legend_xy Numeric, vector, length 2: x and y for plotly's legend
#' @param legend_orientation "v" or "h" for vertical or horizontal
#' @param legend_xanchor Character: Legend's x anchor: "left", "center",
#' "right", "auto"
#' @param legend_yanchor Character: Legend's y anchor: "top", "middle",
#' "bottom", "auto"
#' @param automargin_x Logical: If TRUE, automatically set x-axis margins
#' @param automargin_y Logical: If TRUE, automatically set y-axis margins
#' @param displayModeBar Logical: If TRUE, show plotly's modebar
#' @param modeBar_file_format Character: "svg", "png", "jpeg", "pdf" / any
#' output file type supported by plotly and your system
# @param print_plot Logical: If TRUE, print plot, otherwise return it invisibly
#' @param filename Character: Path to file to save static plot.
#' @param file_width Integer: File width in pixels for when `filename` is
#' set.
#' @param file_height Integer: File height in pixels for when `filename`
#' is set.
#' @param file_scale Numeric: If saving to file, scale plot by this number
#' @param verbosity Integer: Verbosity level.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' draw_bar(VADeaths, legend_xy = c(0, 1))
#' draw_bar(VADeaths, legend_xy = c(1, 1), legend_xanchor = "left")
#' # simple individual bars
#' a <- c(4, 7, 2)
#' draw_bar(a)
#' # if input is a data.frame, each row is a group and each column is a feature
#' b <- data.frame(x = c(3, 5, 7), y = c(2, 1, 8), z = c(4, 5, 2))
#' rownames(b) <- c("Jen", "Ben", "Ren")
#' draw_bar(b)
#' # stacked
#' draw_bar(b, barmode = "stack")
draw_bar <- function(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  alpha = 1,
  horizontal = FALSE,
  theme = choose_theme(getOption("rtemis_theme")),
  palette = get_palette(getOption("rtemis_palette")),
  barmode = c("group", "relative", "stack", "overlay"),
  group_names = NULL,
  order_by_val = FALSE,
  ylim = NULL,
  hovernames = NULL,
  feature_names = NULL,
  font_size = 16,
  annotate = FALSE,
  annotate_col = theme[["labs_col"]],
  legend = NULL,
  legend_col = NULL,
  legend_xy = c(1, 1),
  legend_orientation = "v",
  legend_xanchor = "left",
  legend_yanchor = "auto",
  hline = NULL,
  hline_col = NULL,
  hline_width = 1,
  hline_dash = "solid",
  hline_annotate = NULL,
  hline_annotation_x = 1,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  automargin_x = TRUE,
  automargin_y = TRUE,
  padding = 0,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  verbosity = 0L
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  barmode <- match.arg(barmode)
  if (!is.null(main)) {
    main <- paste0("<b>", main, "</b>")
  }

  dat <- as.data.frame(x)
  if (NROW(dat) == 1 && barmode != "stack") {
    dat <- as.data.frame(t(dat))
  }

  # Order by val ----
  if (order_by_val) {
    if (NCOL(dat) > 1) {
      order_ <- order(sapply(dat, mean, na.rm = TRUE))
      dat <- dat[, order_]
    } else {
      order_ <- order(dat[[1]])
      dat <- dat[order_, , drop = FALSE]
    }
    if (!is.null(group_names)) {
      group_names <- group_names[order_]
    }
    if (!is.null(hovernames)) hovernames <- hovernames[order_]
  }

  # Group names ----
  group_names_ <- group_names
  if (is.null(group_names)) {
    if (!is.null(rownames(dat))) group_names_ <- rownames(dat)
  } else if (is.numeric(group_names)) {
    group_names_ <- dat[, group_names]
    rownames(dat) <- group_names_
    dat <- dat[, group_names_]
  }

  if (verbosity > 0L) {
    msg("group_names_:", group_names_, "\n")
  }

  # Feature names ----
  feature_names_ <- feature_names
  if (is.null(feature_names_)) {
    if (!is.null(colnames(dat))) {
      feature_names_ <- labelify(colnames(dat))
    } else {
      feature_names_ <- paste0("Feature", seq_len(NCOL(dat)))
    }
  }

  if (verbosity > 0L) {
    msg("feature_names_:", feature_names_, "\n")
  }
  if (is.null(legend)) {
    legend <- length(feature_names_) > 1
  }

  # Colors ----
  p <- NCOL(dat)
  col <- recycle(palette, seq(p))[seq(p)]

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])

  # Derived
  if (is.null(legend_col)) {
    legend_col <- labs_col
  }

  if (!is.null(hovernames)) {
    hovernames <- matrix(hovernames)
    if (NCOL(hovernames) == 1 && p > 1) {
      hovernames <- matrix(rep(hovernames, p), ncol = p)
    }
  }

  # plot_ly ----
  group_names_ <- factor(group_names_, levels = group_names_)
  plt <- plotly::plot_ly(
    x = if (horizontal) dat[[1]] else group_names_,
    y = if (horizontal) group_names_ else dat[[1]],
    type = "bar",
    name = feature_names_[1],
    text = hovernames[, 1],
    marker = list(color = plotly::toRGB(if (p > 1) col[1] else col, alpha)),
    showlegend = legend
  )
  if (p > 1) {
    for (i in seq_len(p)[-1]) {
      plt <- plotly::add_trace(
        plt,
        x = if (horizontal) dat[[i]] else group_names_,
        y = if (horizontal) group_names_ else dat[[i]],
        name = feature_names_[i],
        text = hovernames[, i],
        marker = list(color = plotly::toRGB(col[i], alpha))
      )
    }
  }

  if (annotate) {
    if (barmode != "stack") {
      warning("Set barmode to 'stack' to allow annotation")
    } else {
      if (horizontal) {
        for (i in seq_len(ncol(dat))) {
          plt <- plt |>
            plotly::add_annotations(
              xref = "x",
              yref = "y",
              x = rowSums(dat[, seq_len(i - 1), drop = FALSE]) + dat[, i] / 2,
              y = seq_len(nrow(dat)) - 1,
              text = paste(dat[, i]),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        }
      } else {
        for (i in seq_len(ncol(dat))) {
          plt <- plt |>
            plotly::add_annotations(
              xref = "x",
              yref = "y",
              x = seq_len(nrow(dat)) - 1,
              y = rowSums(dat[, seq_len(i - 1), drop = FALSE]) + dat[, i] / 2,
              text = paste(signif(dat[, i], 2)),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        }
      }
    }
  }

  # Layout ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  tickfont <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = theme[["tick_labels_col"]]
  )
  legend_ <- list(
    x = legend_xy[1],
    y = legend_xy[2],
    xanchor = legend_xanchor,
    yanchor = legend_yanchor,
    bgcolor = "#ffffff00",
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = legend_col
    ),
    orientation = legend_orientation
  )

  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = ylab,
      # showline = axes_visible,
      # mirror = axes_mirrored,
      range = ylim,
      titlefont = f,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = FALSE,
      automargin = automargin_y
    ),
    xaxis = list(
      title = xlab,
      # showline = axes_visible,
      # mirror = axes_mirrored,
      titlefont = f,
      showgrid = FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      automargin = automargin_x
    ),
    barmode = barmode, # group works without actual groups too
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      ),
      xref = "paper",
      x = theme[["main_adj"]]
    ),
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    # showlegend = legend,
    legend = legend_
  )

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

  # Padding
  plt[["sizingPolicy"]][["padding"]] <- padding

  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar_file_format,
      width = file_width,
      height = file_height
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
} # /rtemis::draw_bar.R
