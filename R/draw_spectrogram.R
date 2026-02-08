# draw_spectrogram.R
# ::rtemis::
# 2023 EDG rtemis.org
# https://plotly.com/r/heatmaps/

#' Interactive Spectrogram
#'
#' Draw interactive spectrograms using `plotly`
#'
#' To set custom colors, use a minimum of `lo` and `hi`, optionally also
#' `lomid`, `mid`, `midhi` colors and set `colorscale = NULL`.
#'
#' @param x Numeric: Time.
#' @param y Numeric: Frequency.
#' @param z Numeric: Power.
#' @param colorgrad_n Integer: Number of colors in the gradient.
#' @param colors Character: Custom colors for the gradient.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param zlab Character: z-axis label.
#' @param hover_xlab Character: x-axis label for hover.
#' @param hover_ylab Character: y-axis label for hover.
#' @param hover_zlab Character: z-axis label for hover.
#' @param zmin Numeric: Minimum value for color scale.
#' @param zmax Numeric: Maximum value for color scale.
#' @param zauto Logical: If TRUE, automatically set zmin and zmax.
#' @param hoverlabel_align Character: Alignment of hover labels.
#' @param colorscale Character: Color scale.
#' @param colorbar_y Numeric: Y position of colorbar.
#' @param colorbar_yanchor Character: Y anchor of colorbar.
#' @param colorbar_xpad Numeric: X padding of colorbar.
#' @param colorbar_ypad Numeric: Y padding of colorbar.
#' @param colorbar_len Numeric: Length of colorbar.
#' @param colorbar_title_side Character: Side of colorbar title.
#' @param showgrid Logical: If TRUE, show grid.
#' @param space Character: Color space for gradient.
#' @param lo Character: Low color for gradient.
#' @param lomid Character: Low-mid color for gradient.
#' @param mid Character: Mid color for gradient.
#' @param midhi Character: Mid-high color for gradient.
#' @param hi Character: High color for gradient.
#' @param grid_gap Integer: Space between cells.
#' @param limits Numeric, length 2: Determine color range. Default = NULL, which automatically centers values around 0.
#' @param main Character: Main title.
#' @param key_title Character: Title of the key.
#' @param showticklabels Logical: If TRUE, show tick labels.
#' @param theme `Theme` object.
#' @param font_size Numeric: Font size.
#' @param padding Numeric: Padding between cells.
#' @param displayModeBar Logical: If TRUE, display the plotly mode bar.
#' @param modeBar_file_format Character: File format for image exports from the mode bar.
#' @param filename Character: Filename to save the plot. Default is NULL.
#' @param file_width Numeric: Width of exported image.
#' @param file_height Numeric: Height of exported image.
#' @param file_scale Numeric: Scale of exported image.
#' @param ... Additional arguments to be passed to `heatmaply::heatmaply`.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' # Example data
#' time <- seq(0, 10, length.out = 100)
#' freq <- seq(1, 100, length.out = 100)
#' power <- outer(time, freq, function(t, f) sin(t) * cos(f))
#'draw_spectrogram(
#'   x = time,
#'   y = freq,
#'   z = power
#' )
draw_spectrogram <- function(
  x,
  y,
  z,
  colorgrad_n = 101,
  colors = NULL,
  xlab = "Time",
  ylab = "Frequency",
  zlab = "Power",
  hover_xlab = xlab,
  hover_ylab = ylab,
  hover_zlab = zlab,
  zmin = NULL,
  zmax = NULL,
  zauto = TRUE,
  hoverlabel_align = "right",
  colorscale = "Jet",
  colorbar_y = .5,
  colorbar_yanchor = "middle",
  colorbar_xpad = 0,
  colorbar_ypad = 0,
  colorbar_len = .75,
  colorbar_title_side = "bottom",
  showgrid = FALSE,
  space = "rgb",
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  grid_gap = 0,
  limits = NULL,
  main = NULL,
  key_title = NULL,
  showticklabels = NULL,
  theme = choose_theme(),
  font_size = NULL,
  padding = 0,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  ...
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Tick Labels ----
  if (is.null(showticklabels)) {
    showticklabels <- c(
      ifelse(NCOL(x) < 50, TRUE, FALSE),
      ifelse(NROW(x) < 50, TRUE, FALSE)
    )
  }

  if (is.null(font_size)) {
    font_size <- 17.0769 - 0.2692 * ncol(x)
  }

  # Limits ----
  if (is.null(limits)) {
    maxabs <- max(abs(x), na.rm = TRUE)
    if (.2 < maxabs && maxabs < 1) {
      maxabs <- 1
    }
    limits <- c(-maxabs, maxabs)
  }

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  fg <- plotly::toRGB(theme[["fg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]])
  # tick_col <- plotly::toRGB(theme[["tick_col"]])
  tick_labels_col <- plotly::toRGB(theme[["tick_labels_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])

  # Colors ----
  if (is.null(mid)) {
    mid <- theme[["bg"]]
  }
  colors <- colorgrad(
    n = colorgrad_n,
    colors = colors,
    space = space,
    lo = lo,
    lomid = lomid,
    mid = mid,
    midhi = midhi,
    hi = hi
  )

  # Plot ----
  plt <- plotly::plot_ly()
  plt <- plt |>
    plotly::add_trace(
      x = x,
      y = y,
      z = z,
      type = "heatmap",
      zauto = zauto,
      zmin = zmin,
      zmax = zmax,
      colorscale = colorscale,
      colors = colors,
      hovertemplate = paste0(
        hover_xlab,
        ":<b> %{x:.3f}</b><br>",
        hover_ylab,
        ":<b> %{y:.3f}</b><br>",
        hover_zlab,
        ":<b> %{z:.3f}</b><extra></extra>"
      ),
      showlegend = FALSE
    )

  # Layout ----
  # '- layout ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  tickfont <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = tick_labels_col
  )
  .legend <- list(
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = fg
    )
  )

  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = list(
        text = ylab,
        font = f
      ),
      titlefont = f,
      showgrid = showgrid,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickfont = tickfont
    ),
    xaxis = list(
      title = list(
        text = xlab,
        font = f
      ),
      titlefont = f,
      showgrid = showgrid,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickfont = tickfont
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
    legend = .legend,
    hoverlabel = list(align = hoverlabel_align)
  )

  # Manual theme colors

  ## y axis tick label colors
  # plt[["x"]][["layoutAttrs"]][[2]][["yaxis2"]][["tickfont"]][["color"]]
  ## x axis tick label colors
  # plt[["x"]][["layoutAttrs"]][[2]][["xaxis"]][["tickfont"]][["color"]] <- "rgba(255, 0, 0, 1)"
  ## edge lines must be invisible
  plt[["x"]][["layout"]][["yaxis"]][["linecolor"]] <- plt[["x"]][["layout"]][[
    "xaxis2"
  ]][["linecolor"]] <- theme[["bg"]]

  # Manual layout ----
  # Set padding
  plt[["sizingPolicy"]][["padding"]] <- padding

  # Colorbar ----
  # https://plotly.com/r/reference/#scatter-marker-colorbar
  plt <- plt |>
    plotly::colorbar(
      y = colorbar_y,
      yanchor = colorbar_yanchor,
      title = list(
        text = zlab,
        font = f,
        side = colorbar_title_side
      ),
      tickfont = tickfont,
      xpad = colorbar_xpad,
      ypad = colorbar_ypad,
      len = colorbar_len
    )

  # Config ----
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
} # /rtemis::draw_spectrogram
