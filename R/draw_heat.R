# draw_heat.R
# ::rtemis::
# 2016 EDG rtemis.org

#' Heatmap with `plotly`
#'
#' Draw a heatmap using `plotly`.
#'
#' @param z Input matrix.
#' @param x Vectors for x-axis.
#' @param y Vectors for y-axis.
#' @param title Plot title.
#' @param col Set of colors to make gradient from.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param zlab z value label.
#' @param transpose Logical: If TRUE, transpose matrix.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
draw_heat <- function(
  z,
  x = NULL,
  y = NULL,
  title = NULL,
  col = penn_heat(21),
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  transpose = TRUE
) {
  # NS ----
  requireNamespace("plotly")

  # Labels ----
  if (is.null(xlab)) {
    xlab <- " "
  }
  if (is.null(ylab)) {
    ylab <- " "
  }
  if (is.null(zlab)) {
    zlab <- "value"
  }

  # Axes ----
  x <- unique(x)
  y <- unique(y)

  # Fonts ----
  font <- list(
    color = plotly::toRGB("grey50")
  )

  x.axis <- list(
    title = xlab,
    titlefont = font
  )

  y.axis <- list(
    title = ylab,
    titlefont = font
  )

  colorbar <- list(
    title = zlab,
    titlefont = font
  )

  # plotly ----
  p <- plotly::plot_ly(
    z = z,
    x = x,
    y = y,
    transpose = transpose,
    type = "heatmap",
    colors = col,
    colorbar = colorbar,
    text = paste("Value =", ddSci(z)),
    hoverinfo = "all"
  ) |>
    plotly::layout(
      title = title,
      xaxis = x.axis,
      yaxis = y.axis
    )
  p
} # /rtemis::draw_heat
