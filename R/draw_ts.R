# draw_ts.R
# ::rtemis::
# 2022 EDG rtemis.org

# => recalc limits for fn = "sum"

#' Interactive Timeseries Plots
#'
#' Draw interactive timeseries plots using `plotly`
#'
#' @param x Numeric vector of values to plot or list of vectors
#' @param time Numeric or Date vector of time corresponding to values of `x`
#' @param window Integer: apply `roll_fn` over this many units of time
#' @param group Factor defining groups
#' @param roll_fn Character: "mean", "median", "max", or "sum": Function to apply on
#' rolling windows of `x`
#' @param roll_col Color for rolling line
#' @param roll_alpha Numeric: transparency for rolling line
#' @param roll_lwd Numeric: width of rolling line
#' @param roll_name Rolling function name (for annotation)
#' @param alpha Numeric \[0, 1\]: Transparency
#' @param align Character: "center", "right", or "left"
#' @param group_names Character vector of group names
#' @param xlab Character: x-axis label
#' @param n_xticks Integer: number of x-axis ticks to use (approximately)
#  @param tickmode
#' @param scatter_type Character: "scatter" or "lines"
#' @param legend Logical: If TRUE, show legend
#' @param x_showspikes Logical: If TRUE, show x-axis spikes on hover
#' @param y_showspikes Logical: If TRUE, show y-axis spikes on hover
#' @param spikedash Character: dash type string ("solid", "dot", "dash",
#' "longdash", "dashdot", or "longdashdot") or a dash length list in px
#' (eg "5px,10px,2px,2px")
#' @param displayModeBar Logical: If TRUE, display plotly's modebar
#' @param theme Theme object.
#' @param palette Character: palette name, or list of colors
#' @param filename Character: Path to filename to save plot
#' @param spikemode Character: If "toaxis", spike line is drawn from the data
#' point to the axis the series is plotted on. If "across", the line is drawn
#' across the entire plot area, and supercedes "toaxis". If "marker", then a
#' marker dot is drawn on the axis the series is plotted on
#' @param spikesnap Character: "data", "cursor", "hovered data". Determines
#' whether spikelines are stuck to the cursor or to the closest datapoints.
#' @param spikecolor Color for spike lines
#' @param spikethickness Numeric: spike line thickness
#' @param modeBar_file_format Character: modeBar image export file format
#' @param file_width Numeric: image export width
#' @param file_height Numeric: image export height
#' @param file_scale Numeric: image export scale
#' @param ... Additional arguments to be passed to [draw_scatter]
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' time <- sample(seq(as.Date("2020-03-01"), as.Date("2020-09-23"), length.out = 140))
#' x1 <- rnorm(140)
#' x2 <- rnorm(140, 1, 1.2)
#' # Single timeseries
#' draw_ts(x1, time)
#' # Multiple timeseries input as list
#' draw_ts(list(Alpha = x1, Beta = x2), time)
#' # Multiple timeseries grouped by group, different lengths
#' time1 <- sample(seq(as.Date("2020-03-01"), as.Date("2020-07-23"), length.out = 100))
#' time2 <- sample(seq(as.Date("2020-05-01"), as.Date("2020-09-23"), length.out = 140))
#' time <- c(time1, time2)
#' x <- c(rnorm(100), rnorm(140, 1, 1.5))
#' group <- c(rep("Alpha", 100), rep("Beta", 140))
#' draw_ts(x, time, 7, group)
#' }
#'
draw_ts <- function(
  x,
  time,
  window = 7L,
  group = NULL,
  roll_fn = c("mean", "median", "max", "none"),
  roll_col = NULL,
  roll_alpha = 1,
  roll_lwd = 2,
  roll_name = NULL,
  alpha = NULL,
  align = "center",
  group_names = NULL,
  xlab = "Time",
  n_xticks = 12,
  #   tickmode = "array",
  scatter_type = "scatter",
  legend = TRUE,
  x_showspikes = TRUE,
  y_showspikes = FALSE,
  spikedash = "solid",
  spikemode = "across",
  spikesnap = "hovered data",
  spikecolor = NULL,
  spikethickness = 1,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  theme = choose_theme(),
  palette = rtemis_palette,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  ...
) {
  # Arguments ----
  roll_fn <- match.arg(roll_fn)
  if (roll_fn == "none") {
    window <- NULL
  }

  # Timeseries ----
  if (!is.null(group)) {
    x <- split(x, group)
    time <- split(time, group)
  }

  if (is.data.frame(x)) {
    x <- as.list(x)
  }

  if (!is.list(x)) {
    x <- list(x)
  }

  if (is.data.frame(time)) {
    time <- as.list(time)
  }

  if (!is.list(time)) {
    time <- list(time)
  }

  if (is.null(group_names)) {
    group_names <- if (!is.null(names(x))) {
      names(x)
    } else {
      paste("Group", seq_along(x))
    }
  }

  idx <- lapply(time, order)
  time <- lapply(seq_along(time), \(i) time[[i]][idx[[i]]])
  if (length(time) < length(x)) {
    time <- rep(time, length(x) / length(time))
    idx <- rep(idx, length(x) / length(idx))
  }
  x <- lapply(seq_along(x), \(i) x[[i]][idx[[i]]])
  # xtl <- lapply(seq_along(x), \(i) zoo::zoo(x[[i]], time[[i]]))

  if (!is.null(window) && window > 0) {
    avg_line <- switch(
      roll_fn,
      mean = lapply(
        x,
        \(xt) data.table::frollmean(xt, n = window, align = align)
      ),
      median = lapply(
        x,
        \(xt) data.table::frollapply(xt, n = window, median, align = align)
      ),
      max = lapply(
        x,
        \(xt) data.table::frollapply(xt, n = window, max, align = align)
      ),
      sum = lapply(x, \(xt) data.table::frollsum(xt, n = window, align = align))
    )
  }

  # Palette ----
  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }
  if (is.null(roll_col)) {
    roll_col <- palette[seq_along(x)]
  }

  # draw_scatter ----
  plt <- draw_scatter(
    time,
    x,
    xlab = xlab,
    theme = theme,
    palette = palette,
    alpha = alpha,
    group_names = group_names,
    legend = legend,
    scatter_type = scatter_type,
    x_showspikes = x_showspikes,
    y_showspikes = y_showspikes,
    spikedash = spikedash,
    spikemode = spikemode,
    spikesnap = spikesnap,
    spikecolor = spikecolor,
    spikethickness = spikethickness,
    ...
  )

  # Rolling function line ----
  if (is.null(roll_name)) {
    roll_name <- paste0("Rolling ", roll_fn, " (window=", window, ")")
  }

  if (!is.null(window)) {
    for (i in seq_along(x)) {
      plt <- plt |>
        plotly::add_trace(
          x = time[[i]],
          y = avg_line[[i]],
          type = "scatter",
          mode = "lines",
          line = list(
            color = plotly::toRGB(roll_col[[i]], alpha = roll_alpha),
            width = roll_lwd
          ),
          name = roll_name
        )
    }
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
} # rtemis::draw_ts
