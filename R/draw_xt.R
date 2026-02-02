# draw_xt.R
# ::rtemis::
# 2024 EDG rtemis.org

# Multiple legends
# https://plotly.com/python/legend/#adding-multiple-legends
# https://plotly.com/r/legend/

#' Plot timeseries data
#'
#' @details
#' We are switching to `palette` being a color vector instead of the name of a built-in palette.
#'
#' @param x Datetime vector or list of vectors.
#' @param y Numeric vector or named list of vectors: y-axis data.
#' @param x2 Datetime vector or list of vectors, optional: must be provided if `y2` does not
#' correspond to values in `x`. A single x-axis will be drawn for all values in `x` and `x2`.
#' @param y2 Numeric vector, optional: If provided, a second y-axis will be added to the right
#' side of the plot.
#' @param which_xy Integer vector: Indices of `x` and `y` to plot.
#' If not provided, will select up to the first two x-y traces.
#' @param which_xy2 Integer vector: Indices of `x2` and `y2` to plot.
#' If not provided, will select up to the first two x2-y2 traces.
#' @param shade_bin Integer vector \{0, 1\}: Time points in `x` to shade on the plot. For example,
#' if there are 10 time points in `x`, and you want to shade time points 3 to 7,
#' `shade_bin = c(0, 0, 1, 1, 1, 1, 1, 0, 0, 0)`. Only set `shade_bin` or `shade_interval`, not
#' both.
#' @param shade_interval List of numeric vectors: Intervals to shade on the plot. Only set
#' `shade_bin` or `shade_interval`, not both.
#' @param shade_col Color: Color to shade intervals.
#' @param shade_x Numeric vector: x-values to use for shading.
#' @param shade_name Character: Name for shaded intervals.
#' @param shade_showlegend Logical: If TRUE, show legend for shaded intervals.
#' @param ynames Character vector, optional: Names for each vector in `y`.
#' @param y2names Character vector, optional: Names for each vector in `y2`.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param y2lab Character: y2-axis label.
#' @param xunits Character: x-axis units.
#' @param yunits Character: y-axis units.
#' @param y2units Character: y2-axis units.
#' @param yunits_col Color for y-axis units.
#' @param y2units_col Color for y2-axis units.
#' @param zt Numeric vector: Zeitgeber time. If provided, will be shown on the x-axis instead of
#' `x`. To be used only with a single `x` vector and no `x2`.
#' @param show_zt Logical: If TRUE, show zt on x-axis, if zt is provided.
#' @param show_zt_every Integer: Show zt every `show_zt_every` ticks. If NULL, will be calculated
#' to be `x_nticks` +/- 1 if `x_nticks` is not 0, otherwise 12 +/- 1.
#' @param zt_nticks Integer: Number of zt ticks to show. Only used if `show_zt_every` is NULL.
#' The actual number of ticks shown will depend on the periodicity of zt, so that zt = 0 is always
#' included.
#' @param main Character: Main title.
#' @param main_y Numeric: Y position of main title.
#' @param main_yanchor Character: "top", "middle", "bottom".
#' @param x_nticks Integer: Number of ticks on x-axis.
#' @param y_nticks Integer: Number of ticks on y-axis.
#' @param show_rangeslider Logical: If TRUE, show a range slider.
#' @param slider_start Numeric: Start of range slider.
#' @param slider_end Numeric: End of range slider.
#' @param theme `Theme` object.
#' @param palette Color list: will be used to draw each vector in `y` and `y2`, in order.
#' @param font_size Numeric: Font size for text.
#' @param yfill Character: Fill type for y-axis: "none", "tozeroy", "tonexty".
#' @param y2fill Character: Fill type for y2-axis: "none", "tozeroy", "tonexty".
#' @param fill_alpha Numeric: Fill opacity for y-axis.
#' @param yline_width Numeric: Line width for y-axis lines.
#' @param y2line_width Numeric: Line width for y2-axis lines.
#' @param x_showspikes Logical: If TRUE, show spikes on x-axis.
#' @param spike_dash Character: Dash type for spikes: "solid", "dot", "dash", "longdash",
#' "dashdot", "longdashdot".
#' @param spike_col Color for spikes.
#' @param x_spike_thickness Numeric: Thickness of spikes. `-2` avoids drawing border around spikes.
#' @param tickfont_size Numeric: Font size for tick labels.
#' @param x_tickmode Character: "auto", "linear", "array".
#' @param x_tickvals Numeric vector: Tick positions.
#' @param x_ticktext Character vector: Tick labels.
#' @param x_tickangle Numeric: Angle of tick labels.
#' @param legend_x Numeric: X position of legend.
#' @param legend_y Numeric: Y position of legend.
#' @param legend_xanchor Character: "left", "center", "right".
#' @param legend_yanchor Character: "top", "middle", "bottom".
#' @param legend_orientation Character: "v" for vertical, "h" for horizontal.
#' @param margin Named list with 4 numeric values: "l", "r", "t", "b" for left, right, top, bottom
#' margins.
#' @param x_standoff Numeric: Distance from x-axis to x-axis label.
#' @param y_standoff Numeric: Distance from y-axis to y-axis label.
#' @param y2_standoff Numeric: Distance from y2-axis to y2-axis label.
#' @param hovermode Character: "closest", "x", "x unified".
#' @param displayModeBar Logical: If TRUE, display plotly mode bar.
#' @param modeBar_file_format Character: "png", "svg", "jpeg", "webp", "pdf": file format for mode
#' bar image export.
#' @param scrollZoom Logical: If TRUE, enable zooming by scrolling.
#' @param filename Character: Path to save the plot image.
#' @param file_width Numeric: Width of the saved plot image.
#' @param file_height Numeric: Height of the saved plot image.
#' @param file_scale Numeric: Scale of the saved plot image.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' datetime <- seq(
#'   as.POSIXct("2020-01-01 00:00"),
#'   as.POSIXct("2020-01-02 00:00"),
#'   by = "hour"
#' )
#' df <- data.frame(
#'   datetime = datetime,
#'   value1 = rnorm(length(datetime)),
#'   value2 = rnorm(length(datetime))
#' )
#' draw_xt(df, x = df[, 1], y = df[, 2:3])
#' }
draw_xt <- function(
  x,
  y,
  x2 = NULL,
  y2 = NULL,
  which_xy = NULL,
  which_xy2 = NULL,
  # Shade intervals
  shade_bin = NULL,
  shade_interval = NULL,
  shade_col = NULL,
  shade_x = NULL,
  shade_name = "",
  shade_showlegend = FALSE,
  ynames = NULL,
  y2names = NULL,
  xlab = NULL,
  ylab = NULL,
  y2lab = NULL,
  xunits = NULL,
  yunits = NULL,
  y2units = NULL,
  yunits_col = NULL,
  y2units_col = NULL,
  zt = NULL,
  show_zt = TRUE,
  show_zt_every = NULL,
  zt_nticks = 18L,
  main = NULL,
  main_y = 1,
  main_yanchor = "bottom",
  x_nticks = 0,
  y_nticks = 0,
  show_rangeslider = NULL,
  slider_start = NULL,
  slider_end = NULL,
  theme = choose_theme(),
  palette = rtpalette(rtemis_palette),
  font_size = 16,
  yfill = "none",
  y2fill = "none",
  fill_alpha = .2,
  yline_width = 2,
  y2line_width = 2,
  x_showspikes = TRUE,
  spike_dash = "solid",
  spike_col = NULL,
  x_spike_thickness = -2,
  tickfont_size = 16,
  x_tickmode = "auto",
  x_tickvals = NULL,
  x_ticktext = NULL,
  x_tickangle = NULL,
  # legend
  legend_x = 0,
  legend_y = 1.1,
  legend_xanchor = "left",
  legend_yanchor = "top",
  legend_orientation = "h",
  margin = list(l = 75, r = 75, b = 75, t = 75),
  # axis labels
  x_standoff = 20L,
  y_standoff = 20L,
  y2_standoff = 20L,
  hovermode = "x",
  # config
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  scrollZoom = TRUE,
  filename = NULL,
  file_width = 960,
  file_height = 500,
  file_scale = 1
) {
  # Names ----
  .xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  .x2name <- labelify(gsub(".*\\$", "", deparse(substitute(x2))))
  if (!is.null(x2) && .xname != .x2name) {
    .xname <- NULL
  }
  .yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  .y2name <- labelify(gsub(".*\\$", "", deparse(substitute(y2))))

  # Data ----

  # Data to lists
  if (!is.null(y2) && is.null(x2)) {
    x2 <- x
  }
  if (!is.list(x)) {
    x <- list(x)
  }
  if (!is.list(y)) {
    y <- list(y)
  }
  if (!is.null(y2) && !is.list(y2)) {
    y2 <- list(y2)
  }
  if (!is.null(y2) && !is.list(x2)) {
    x2 <- list(x2)
  }

  # Recycle x and x2 as needed
  if (length(y) > 1 && length(x) == 1) {
    x <- rep(x, length(y))
  }
  if (!is.null(y2) && length(y2) > 1 && length(x2) == 1) {
    x2 <- rep(x2, length(y2))
  }
  if (length(x) != length(y)) {
    cli::cli_abort("x and y must be the same length")
  }
  if (!is.null(y2) && length(x2) != length(y2)) {
    cli::cli_abort("x2 and y2 must be the same length")
  }

  # Which traces to plot ----
  # By default, plot up to two for each y axis
  if (is.null(which_xy)) {
    if (length(x) > 2) {
      x <- x[1:2]
      y <- y[1:2]
    }
  } else {
    x <- x[which_xy]
    y <- y[which_xy]
  }

  if (is.null(which_xy2)) {
    if (length(x2) > 2) {
      x2 <- x2[1:2]
      y2 <- y2[1:2]
    }
  } else {
    x2 <- x2[which_xy2]
    y2 <- y2[which_xy2]
  }

  # Rangeslider ----
  if (is.null(show_rangeslider)) {
    show_rangeslider <- length(x[[1]]) > 500
  }

  # Check args ----
  if (!is.null(shade_bin) && !is.null(shade_interval)) {
    cli::cli_abort("Only set shade_bin or shade_interval, not both")
  }

  # Names ----
  if (is.null(ynames)) {
    ynames <- if (is.null(names(y))) {
      if (length(y) > 1) {
        paste(.yname, seq_along(y), sep = "_")
      } else {
        .yname
      }
    } else {
      names(y)
    }
  }

  if (!is.null(y2) && is.null(y2names)) {
    y2names <- if (is.null(names(y2))) {
      if (length(y2) > 1) {
        paste(.y2name, seq_along(y2), sep = "_")
      } else {
        .y2name
      }
    } else {
      names(y2)
    }
  }

  # Add units
  if (!is.null(yunits)) {
    if (is.null(yunits_col)) {
      yunits_col <- if (length(y) == 1) {
        palette[[1]]
      } else {
        "#00ff00"
      }
    }
    yunits <- paste0(
      "(",
      '<span style="color:',
      yunits_col,
      ';">',
      yunits,
      "</span>",
      ")"
    )
    ynames <- paste(ynames, yunits)
  }
  if (!is.null(y2units)) {
    if (is.null(y2units_col)) {
      y2units_col <- if (length(y2) == 1) {
        palette[[length(x) + 1]]
      } else {
        "#ff0000"
      }
    }
    y2units <- paste0(
      "(",
      '<span style="color:',
      y2units_col,
      ';">',
      y2units,
      "</span>",
      ")"
    )
    y2names <- paste(y2names, y2units)
  }

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]], theme[["grid_alpha"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  legend_col <- labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])
  if (!theme[["axes_visible"]]) {
    tick_col <- labs_col <- "transparent"
  }
  if (is.null(spike_col)) {
    spike_col <- theme[["fg"]]
  }
  zero_col <- adjustcolor(theme[["zerolines_col"]], theme[["zerolines_alpha"]])

  # Colors ----
  # if (is.null(line1.fill.col)) line1.fill.col <- plotly::toRGB(line1.col, alpha = 0.4)
  # if (is.null(line2.fill.col) && !is.null(y2)) {
  #   line2.fill.col <- plotly::toRGB(line2.col, alpha = 0.4)
  # }
  palette_y <- palette[seq_along(y)]
  palette_y2 <- palette[length(y) + seq_along(y2)]

  if (length(y) > 1 && length(yfill) == 1) {
    yfill <- rep(yfill, length(y))
  }
  stopifnot(length(yfill) == length(y))

  if (length(y2) > 1 && length(y2fill) == 1) {
    y2fill <- rep(y2fill, length(y2))
  }

  if (!is.null(y2)) {
    stopifnot(length(y2fill) == length(y2))
  }

  # Fonts ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  tick_font <- list(
    family = theme[["font_family"]],
    size = tickfont_size,
    color = theme[["tick_labels_col"]]
  )

  # Calculate shade_interval from shade_bin ----
  if (!is.null(shade_bin)) {
    shade_bin_p <- c(0, shade_bin, 0)
    shade_bin_starts <- which(diff(shade_bin_p) == 1)
    shade_bin_ends <- which(diff(shade_bin_p) == -1)
    shade_interval <- lapply(
      seq_along(shade_bin_starts),
      \(i) c(shade_bin_starts[i], shade_bin_ends[i])
    )
  }

  # zt ----
  if (show_zt && !is.null(zt)) {
    x_tickmode <- "array"
    if (is.null(show_zt_every)) {
      # Get periodicity of ZT
      idi0 <- which(zt == 0)
      # Get differences between 0s
      diff_idi0 <- diff(idi0)[1]
      # Pick show.zt.every to be perfect divisor of diff_idi0 so that total length is closest to zt.nticks
      # a) diff_idi0 %% show.zt.every must be 0
      # b) length(zt) / show.zt.every must be closest to zt.nticks
      sze <- round(length(zt) / zt_nticks)
      i <- 0
      # if diff_idi0 %% sze != 0, search for closest integer above or below sze
      sze_high <- sze_low <- sze
      while (diff_idi0 %% sze_low != 0) {
        sze_low <- sze_low - 1
      }
      while (diff_idi0 %% sze_high != 0) {
        sze_high <- sze_high + 1
      }
      show_zt_every <- c(sze_low, sze_high)[which.min(abs(c(
        sze - sze_low,
        sze - sze_high
      )))]
    }
    idi <- seq(1, length(zt), by = show_zt_every)
    # Make sure 0 is included
    while (!0 %in% zt[idi]) {
      idi <- idi + 1
    }
    idi <- idi[idi <= length(zt)]
    x_tickvals <- x[[1]][idi]
    x_ticktext <- zt[idi]
    if (is.null(xlab)) xlab <- "ZT"
  }

  # Plot ----
  # if (!is.null(x.ticktext)) {
  #   stopifnot(!is.null(x.tickvals))
  #   plt <- plotly::plot_ly(
  #     x = x.tickvals,
  #     y = NA,
  #     type = "scatter",
  #     mode = "lines",
  #     text = x.ticktext
  #   )
  # } else {
  #   plt <- plotly::plot_ly(type = "scatter", mode = "lines")
  # }
  plt <- plotly::plot_ly(type = "scatter", mode = "lines")
  # browser()

  # Shade intervals ----
  if (!is.null(shade_interval)) {
    if (is.null(shade_x)) {
      shade_x <- x[[1]]
    }
    if (is.null(shade_col)) {
      shade_col <- plotly::toRGB(theme[["fg"]], 0.15)
    }
    ymax <- max(unlist(y), unlist(y2))
    # Draw shaded rectangles
    for (i in seq_along(shade_interval)) {
      plt <- plotly::add_trace(
        plt,
        x = c(
          shade_x[shade_interval[[i]][1]],
          shade_x[shade_interval[[i]][2]],
          shade_x[shade_interval[[i]][2]],
          shade_x[shade_interval[[i]][1]]
        ),
        y = c(0, 0, ymax, ymax),
        fill = "toself",
        fillcolor = shade_col,
        line = list(color = "transparent"),
        yaxis = "y",
        xaxis = "x",
        name = shade_name,
        legendgroup = if (shade_showlegend) shade_name else NULL,
        showlegend = shade_showlegend && i == 1
      )
    }
  } # /shade.interval

  for (i in seq_along(y)) {
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      line = list(color = palette_y[[i]], width = yline_width),
      fill = yfill[[i]],
      fillcolor = plotly::toRGB(palette_y[[i]], alpha = fill_alpha),
      name = ynames[[i]],
      legendgroup = if (!is.null(y2)) "legend_y" else NULL
    )
  } # /y scatter

  if (!is.null(y2)) {
    for (i in seq_along(y2)) {
      plt <- plotly::add_trace(
        plt,
        x = x2[[i]],
        y = y2[[i]],
        line = list(color = palette_y2[[i]], width = y2line_width),
        fill = y2fill[[i]],
        fillcolor = plotly::toRGB(palette_y2[[i]], alpha = fill_alpha),
        name = y2names[[i]],
        legendgroup = "legend_y2",
        yaxis = "y2"
      )
    }
  } # /y2 scatter

  # Labels ----
  if (is.null(xlab)) {
    xlab <- .xname
  }
  if (!is.null(xunits)) {
    xlab <- paste0(xlab, " (", xunits, ")")
  }

  if (!is.null(yunits)) {
    ylab <- if (is.null(ylab)) {
      if (length(y) == 1) {
        ynames
      } else {
        yunits
      }
    } else {
      paste(ylab, yunits)
    }
  }

  if (!is.null(y2units)) {
    y2lab <- if (is.null(y2lab)) {
      if (length(y2) == 1) {
        y2names
      } else {
        y2units
      }
    } else {
      paste(y2lab, y2units)
    }
  }

  # Layout ----
  plt <- plotly::layout(
    plt,
    xaxis = list(
      title = list(
        text = xlab,
        standoff = x_standoff,
        font = f
      ),
      nticks = x_nticks,
      showspikes = x_showspikes,
      spikedash = spike_dash,
      spikecolor = spike_col,
      spikethickness = x_spike_thickness,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickmode = x_tickmode,
      tickvals = x_tickvals,
      ticktext = x_ticktext,
      tickangle = x_tickangle,
      tickcolor = tick_col,
      tickfont = tick_font,
      zeroline = theme[["zerolines"]],
      zerolinecolor = zero_col,
      zerolinewidth = theme[["zerolines_lwd"]]
    ), # /layout > xaxis
    yaxis = list(
      title = list(
        text = ylab,
        standoff = y_standoff,
        font = f
      ),
      nticks = y_nticks,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tick_font,
      zeroline = theme[["zerolines"]],
      zerolinecolor = zero_col,
      zerolinewidth = theme[["zerolines_lwd"]],
      standoff = y_standoff
    ), # /layout > yaxis
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
    legend = list(
      x = legend_x,
      y = legend_y,
      xanchor = legend_xanchor,
      yanchor = legend_yanchor,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = legend_col
      ),
      orientation = legend_orientation,
      bgcolor = "#ffffff00"
    ), # /layout > legend
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    hovermode = hovermode
  ) # /layout

  if (!is.null(y2)) {
    plt <- plt |>
      plotly::layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(
            text = y2lab,
            standoff = y2_standoff,
            font = f
          ),
          tickfont = tick_font
        )
      )
  } # /yaxis2 layout

  # Config ----
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar_file_format,
      width = file_width,
      height = file_height
    ),
    scrollZoom = scrollZoom
  ) # /config

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

  # Rangeslider ----
  if (show_rangeslider) {
    if (is.null(slider_start)) {
      slider_start <- x[[1]][1]
    }
    if (is.null(slider_end)) {
      idi <- min(500, length(x[[1]]))
      slider_end <- x[[1]][idi]
    }
    plt <- plt |>
      plotly::rangeslider(start = slider_start, end = slider_end)
  } # /rangeslider

  return(plt)
} # /rtemis::draw_xt

# tickmode = "array", tickvals: placement, ticktext: labels
