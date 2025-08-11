# draw_dist.R
# ::rtemis::
# 2019- EDG rtemis.org

# check whether list is reordered with ridge

#' Draw Distributions using Histograms and Density Plots
#'
#' Draw Distributions using Histograms and Density Plots using `plotly`.
#'
#' If input is data.frame, non-numeric variables will be removed.
#'
#' @param x Numeric vector / data.frame / list: Input. If not a vector, each column / each element is drawn.
#' @param type Character: "density" or "histogram".
#' @param mode Character: "overlap", "ridge". How to plot different groups; on the same axes ("overlap"), or on separate plots with the same x-axis ("ridge").
#' @param group Vector: Will be converted to factor; levels define group members.
#' @param main Character: Main title for the plot.
#' @param xlab Character: Label for the x-axis.
#' @param ylab Character: Label for the y-axis.
#' @param col Color: Colors for the plot.
#' @param alpha Numeric: Alpha transparency for plot elements.
#' @param plot_bg Color: Background color for plot area.
#' @param theme Theme object.
#' @param palette Character: Color palette to use.
#' @param axes_square Logical: If TRUE, draw a square plot to fill the graphic device. Default = FALSE.
#' @param group_names Character: Names for the groups.
#' @param font_size Numeric: Font size for plot text.
#' @param font_alpha Numeric: Alpha transparency for font.
#' @param legend Logical: If TRUE, draw legend. Default = NULL, which will be set to TRUE if x is a list of more than 1 element.
#' @param legend_xy Numeric, vector, length 2: Relative x, y position for legend. Default = c(0, 1).
#' @param legend_col Color: Color for the legend text.
#' @param legend_bg Color: Background color for legend.
#' @param legend_border_col Color: Border color for legend.
#' @param bargap Numeric: The gap between adjacent histogram bars in plot fraction.
#' @param vline Numeric, vector: If defined, draw a vertical line at this x value(s).
#' @param vline_col Color: Color for `vline`.
#' @param vline_width Numeric: Width for `vline`.
#' @param vline_dash Character: Type of line to draw: "solid", "dot", "dash", "longdash", "dashdot", or "longdashdot".
#' @param text Character: If defined, add this text over the plot.
#' @param text_x Numeric: x-coordinate for `text`.
#' @param text_xref Character: "x": `text_x` refers to plot's x-axis; "paper": `text_x` refers to plotting area from 0-1.
#' @param text_xanchor Character: "auto", "left", "center", "right".
#' @param text_y Numeric: y-coordinate for `text`.
#' @param text_yref Character: "y": `text_y` refers to plot's y-axis; "paper": `text_y` refers to plotting area from 0-1.
#' @param text_yanchor Character: "auto", "top", "middle", "bottom".
#' @param text_col Color: Color for `text`.
#' @param margin List: Margins for the plot.
#' @param automargin_x Logical: If TRUE, automatically adjust x-axis margins.
#' @param automargin_y Logical: If TRUE, automatically adjust y-axis margins.
#' @param zerolines Logical: If TRUE, draw lines at y = 0.
#' @param density_kernel Character: Kernel to use for density estimation.
#' @param density_bw Character: Bandwidth to use for density estimation.
#' @param histnorm Character: NULL, "percent", "probability", "density", "probability density".
#' @param histfunc Character: "count", "sum", "avg", "min", "max".
#' @param hist_n_bins Integer: Number of bins to use if type = "histogram".
#' @param barmode Character: Barmode for histogram. Default = "overlay".
#' @param ridge_sharex Logical: If TRUE, draw single x-axis when `mode = "ridge"`.
#' @param ridge_y_labs Logical: If TRUE, show individual y labels when `mode = "ridge"`.
#' @param ridge_order_on_mean Logical: If TRUE, order groups by mean value when `mode = "ridge"`.
#' @param displayModeBar Logical: If TRUE, display the mode bar.
#' @param modeBar_file_format Character: File format for mode bar. Default = "svg".
#' @param width Numeric: Force plot size to this width. Default = NULL, i.e. fill available space.
#' @param height Numeric: Force plot size to this height. Default = NULL, i.e. fill available space.
#' @param filename Character: Path to file to save static plot.
#' @param file_width Integer: File width in pixels for when `filename` is set.
#' @param file_height Integer: File height in pixels for when `filename` is set.
#' @param file_scale Numeric: If saving to file, scale plot by this number.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' # Will automatically use only numeric columns
#' draw_dist(iris)
#' draw_dist(iris[["Sepal.Length"]], group = iris[["Species"]])
#' }
#'
draw_dist <- function(
  x,
  type = c("density", "histogram"),
  mode = c("overlap", "ridge"),
  group = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = .75,
  plot_bg = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  axes_square = FALSE,
  group_names = NULL,
  font_size = 16,
  font_alpha = .8,
  legend = NULL,
  legend_xy = c(0, 1),
  legend_col = NULL,
  legend_bg = "#FFFFFF00",
  legend_border_col = "#FFFFFF00",
  bargap = .05,
  vline = NULL,
  vline_col = theme[["fg"]],
  vline_width = 1,
  vline_dash = "dot",
  text = NULL,
  text_x = 1,
  text_xref = "paper",
  text_xanchor = "left",
  text_y = 1,
  text_yref = "paper",
  text_yanchor = "top",
  text_col = theme[["fg"]],
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  automargin_x = TRUE,
  automargin_y = TRUE,
  zerolines = FALSE,
  density_kernel = "gaussian",
  density_bw = "SJ",
  histnorm = c(
    "",
    "density",
    "percent",
    "probability",
    "probability density"
  ),
  histfunc = c("count", "sum", "avg", "min", "max"),
  hist_n_bins = 20,
  barmode = "overlay", # TODO: alternatives
  ridge_sharex = TRUE,
  ridge_y_labs = FALSE,
  ridge_order_on_mean = TRUE,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  width = NULL,
  height = NULL,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  type <- match.arg(type)
  mode <- match.arg(mode)
  if (!is.null(main)) {
    main <- paste0("<b>", main, "</b>")
  }
  .xname <- labelify(deparse(substitute(x)))

  # Data ----

  # '- Group ----
  if (!is.null(group)) {
    if (is.factor(group)) {
      group <- droplevels(group)
    } else {
      group <- as.factor(group)
    }
    x <- as.data.frame(x)
    # Can't have multiple vectors in `x` and `group`
    if (length(x) > 1 && !is.null(group)) {
      cli::cli_abort(
        "Can't have both multiple vectors in `x` and `group` defined."
      )
    }
    x <- split(x, group)
    x <- sapply(x, as.vector)
    if (is.null(group_names)) {
      group_names <- levels(group)
    }
    names(x) <- .names <- group_names
  }

  if (!is.list(x)) {
    x <- list(x)
  }
  n_groups <- length(x)
  if (n_groups == 1 && is.null(xlab)) {
    xlab <- .xname
  }

  # Remove non-numeric vectors
  which_nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  if (length(which_nonnum) > 0) {
    for (i in rev(which_nonnum)) {
      x[[i]] <- NULL
    }
  }

  if (is.null(legend)) {
    legend <- length(x) > 1
  }
  if (!is.null(group_names)) {
    .names <- group_names
  } else {
    .names <- labelify(names(x))
  }
  if (is.null(.names)) {
    .names <- paste("Feature", seq_along(x))
  }

  # Colors ----
  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }
  n_groups <- length(x)
  if (is.null(col)) {
    col <- recycle(palette, seq(n_groups))[seq(n_groups)]
  }

  if (length(col) < n_groups) {
    col <- rep(col, n_groups / length(col))
  }

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])
  if (!theme[["axes_visible"]]) {
    tick_col <- labs_col <- "transparent"
  }

  # '- Axis font ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )

  # '- Tick font ----
  tickfont <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = theme[["tick_labels_col"]]
  )

  # Derived
  if (is.null(legend_col)) {
    legend_col <- labs_col
  }

  # Size ----
  if (axes_square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # Ridge ----
  if (mode == "ridge") {
    axis <- list(
      showline = FALSE,
      # mirror = axes_mirrored,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = zerolines
    )
    ridge_groups <- if (ridge_order_on_mean) {
      order(sapply(x, mean), decreasing = TRUE)
    } else {
      seq_len(n_groups)
    }
  }

  # plotly ----
  # z <- if (mode == "overlap") rep(1, n_groups) else seq_len(n_groups)
  # plt <- vector("list", n_groups)

  .text <- lapply(x, function(i) {
    paste(
      "mean =",
      ddSci(mean(i, na.rm = TRUE)),
      "\nsd =",
      ddSci(sd(i, na.rm = TRUE))
    )
  })

  # '- { Density } ----
  if (type == "density") {
    if (is.null(ylab)) {
      ylab <- "Density"
    }
    xl_density <- lapply(
      x,
      density,
      na.rm = TRUE,
      kernel = density_kernel,
      bw = density_bw
    )

    if (mode == "overlap") {
      # '- Density overlap ----
      plt <- plotly::plot_ly(
        width = width,
        height = height
      )
      for (i in seq_len(n_groups)) {
        plt <- plotly::add_trace(
          plt,
          x = xl_density[[i]][["x"]],
          y = xl_density[[i]][["y"]],
          type = "scatter",
          mode = "none",
          fill = "tozeroy",
          fillcolor = plotly::toRGB(col[[i]], alpha),
          name = .names[i],
          hovertext = .text[[i]],
          hoverinfo = "text",
          showlegend = legend
        )
      }
    } else {
      # '- Density ridge ----
      plt <- lapply(ridge_groups, function(i) {
        plotly::plot_ly(
          x = xl_density[[i]][["x"]],
          y = xl_density[[i]][["y"]],
          type = "scatter",
          mode = "none",
          fill = "tozeroy",
          fillcolor = plotly::toRGB(col[[i]], alpha),
          name = .names[i],
          hovertext = .text[[i]],
          hoverinfo = "text",
          showlegend = legend,
          width = width,
          height = height
        ) |>
          plotly::layout(
            xaxis = axis,
            yaxis = c(
              list(
                title = list(
                  text = .names[i],
                  font = f
                )
              ),
              axis
            )
          )
      })
    }
  } # End mode == "density"

  # '- { Histogram } ----
  if (type == "histogram") {
    # https://plotly.com/r/reference/#histogram-bingroup
    bingroup <- 1
    histnorm <- match.arg(histnorm)
    histfunc <- match.arg(histfunc)
    # if (is.null(ylab)) ylab <- "Count"

    if (mode == "overlap") {
      # '-  Histogram overlap ----
      plt <- plotly::plot_ly(
        width = width,
        height = height
      )
      for (i in seq_len(n_groups)) {
        plt <- plotly::add_trace(
          plt,
          x = x[[i]],
          type = "histogram",
          marker = list(color = plotly::toRGB(col[i], alpha)),
          name = .names[i],
          hovertext = .text[[i]],
          hoverinfo = "text",
          histnorm = histnorm,
          histfunc = histfunc,
          nbinsx = hist_n_bins,
          showlegend = legend,
          bingroup = bingroup
        )
      }
      plt <- plotly::layout(plt, barmode = barmode, bargap = bargap)
    } else {
      # '- Histogram ridge ----
      plt <- lapply(ridge_groups, function(i) {
        plotly::plot_ly(
          x = x[[i]],
          type = "histogram",
          histnorm = histnorm,
          histfunc = histfunc,
          nbinsx = hist_n_bins,
          marker = list(color = plotly::toRGB(col[i], alpha)),
          name = .names[i],
          hovertext = .text[[i]],
          hoverinfo = "text",
          showlegend = legend,
          width = width,
          height = height,
          bingroup = bingroup
        ) |>
          plotly::layout(
            xaxis = axis,
            yaxis = c(
              list(
                title = list(
                  text = .names[i],
                  font = f
                )
              ),
              axis
            ),
            bargap = bargap
          )
      })
    }
  }

  if (mode == "ridge") {
    plt <- plotly::subplot(
      plt,
      nrows = n_groups,
      shareX = ridge_sharex,
      # shareY = ridge_sharey,
      titleY = ridge_y_labs
    )
  }

  # Layout ----
  zerocol <- adjustcolor(theme[["zerolines_col"]], theme[["zerolines_alpha"]])
  # '- layout ----
  .legend <- list(
    x = legend_xy[1],
    y = legend_xy[2],
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = legend_col
    ),
    bgcolor = legend_bg,
    bordercolor = legend_border_col
  )

  plt <- plotly::layout(
    plt,
    xaxis = list(
      title = list(
        text = xlab,
        font = f
      ),
      showline = FALSE,
      # mirror = axes_mirrored,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = FALSE,
      automargin = automargin_x
    ),
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
    showlegend = legend,
    legend = .legend
  )

  if (mode == "overlap") {
    plt <- plotly::layout(
      plt,
      yaxis = list(
        title = list(
          text = ylab,
          font = f
        ),
        showline = FALSE,
        # mirror = axes_mirrored,
        showgrid = theme[["grid"]],
        gridcolor = grid_col,
        gridwidth = theme[["grid_lwd"]],
        tickcolor = tick_col,
        tickfont = tickfont,
        zeroline = zerolines,
        zerolinecolor = zerocol,
        zerolinewidth = theme[["zerolines_lwd"]],
        automargin = automargin_y
      )
    )
  }

  # vline ----
  if (!is.null(vline)) {
    plt <- plotly::layout(
      plt,
      shapes = plotly_vline(
        vline,
        color = vline_col,
        width = vline_width,
        dash = vline_dash
      )
    )
  }

  # text ----
  if (!is.null(text)) {
    plt <- plotly::layout(
      plt,
      annotations = list(
        text = text,
        x = text_x,
        xref = text_xref,
        xanchor = text_xanchor,
        y = text_y,
        yref = text_yref,
        yanchor = text_yanchor,
        font = list(
          color = text_col,
          family = theme[["font_family"]],
          size = font_size
        ),
        showarrow = FALSE
      )
    )
  }

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
} # /rtemis::draw_dist
