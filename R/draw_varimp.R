# draw_varimp.R
# ::rtemis::
# 2017 EDG rtemis.org

#' Interactive Variable Importance Plot
#'
#' Plot variable importance using `plotly`
#'
#' A simple `plotly` wrapper to plot horizontal barplots, sorted by value,
#' which can be used to visualize variable importance, model coefficients, etc.
#'
#' @param x Numeric vector: Input.
#' @param names Vector, string: Names of features.
#' @param main Character: Main title.
#' @param type Character: "bar" or "line".
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param plot_top Integer: Plot this many top features.
#' @param orientation Character: "h" or "v".
#' @param line_width Numeric: Line width.
#' @param labelify Logical: If TRUE, labelify feature names.
#' @param col Vector, colors: Single value, or multiple values to define bar
#' (feature) color(s).
#' @param alpha Numeric: Transparency.
#' @param palette Character: Name of \pkg{rtemis} palette to use.
#' @param mar Vector, numeric, length 4: Plot margins in pixels (NOT inches).
#' @param font_size Integer: Overall font size to use (essentially for the
#' title at this point).
#' @param axis_font_size Integer: Font size to use for axis labels and tick labels.
#' @param theme `Theme` object.
#' @param showlegend Logical: If TRUE, show legend.
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
#' @examplesIf interactive()
#' # made-up data
#' x <- rnorm(10)
#' names(x) <- paste0("Feature_", seq(x))
#' draw_varimp(x)
#' draw_varimp(x, orientation = "h")
draw_varimp <- function(
  x,
  names = NULL,
  main = NULL,
  type = c("bar", "line"),
  xlab = NULL,
  ylab = NULL,
  plot_top = 1, # 1 or less means plot this percent
  orientation = "v",
  line_width = 12,
  labelify = TRUE,
  col = NULL,
  alpha = 1,
  palette = rtemis_palette,
  mar = NULL,
  font_size = 16,
  axis_font_size = 14,
  theme = choose_theme(),
  showlegend = TRUE,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  type <- match.arg(type)
  if (is.null(mar)) {
    mar <- if (is.null(main)) c(20, 20, 20, 20) else c(20, 20, 40, 20)
  }

  # Theme ----
  check_is_S7(theme, Theme)

  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])

  ## Axis font ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )

  ## Tick font ----
  tickfont <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = theme[["tick_labels_col"]]
  )

  # Data ----
  if (NCOL(x) > 1 && NROW(x) > 1) {
    cli::cli_abort("x must be a vector or single row or column")
  }

  ## Names ----
  if (is.null(names)) {
    if (is.null(names(x))) {
      .names <- if (NCOL(x) == 1) {
        labelify(rownames(x))
      } else {
        labelify(colnames(x))
      }
    } else {
      .names <- labelify(names(x))
    }
  } else {
    .names <- labelify(names)
  }

  x <- as.numeric(x)
  if (length(.names) == 0) {
    .names <- paste("Feature", seq_along(x))
  }

  ## Index ----
  index <- if (plot_top <= 1) {
    order(abs(x))[(length(x) - plot_top * length(x)):length(x)]
  } else {
    if (plot_top > length(x)) {
      plot_top <- length(x)
    }
    order(abs(x))[(length(x) - plot_top + 1):length(x)]
  }
  x <- x[index]
  .names <- .names[index]
  # reorder to arrange negative to positive
  index <- order(x)
  x <- x[index]
  .names <- .names[index]
  y <- factor(.names, levels = .names)

  # Colors ----
  if (is.null(col)) {
    col <- palette[[1]]
  }
  col <- color_adjust(col, alpha = alpha)

  # plotly ----
  if (type == "bar") {
    plt <- plotly::plot_ly(
      x = if (orientation == "h") x else y,
      y = if (orientation == "h") y else x,
      type = "bar",
      marker = list(
        color = col,
        line = list(width = NULL)
      ),
      showlegend = FALSE
    )
  } else {
    # Plot each x[i] value as a line segment from 0 to x[i]
    plt <- plotly::plot_ly()
    for (i in seq_along(x)) {
      plt <- plotly::add_trace(
        plt,
        x = if (orientation == "h") c(0, x[i]) else c(y[i], y[i]),
        y = if (orientation == "h") c(y[i], y[i]) else c(0, x[i]),
        type = "scatter",
        mode = "lines",
        line = list(color = col, width = line_width),
        name = .names[i],
        showlegend = FALSE,
        # Show "_name[i]: value" on hover
        hoverinfo = "text",
        hovertext = paste0(.names[i], ": ", ddSci(x[i]))
      )
    }
  }

  # Layout ----
  if (is.null(xlab)) {
    xlab <- if (orientation == "h") "Variable Importance" else ""
  }
  if (is.null(ylab)) {
    ylab <- if (orientation == "h") "" else "Variable Importance"
  }
  plt <- plotly::layout(
    plt,
    margin = list(
      b = mar[1],
      l = mar[2],
      t = mar[3],
      r = mar[4],
      pad = 0
    ), # inner plot area padding
    xaxis = list(
      title = list(
        text = xlab,
        font = f
      ),
      # showline = axes_visible,
      # mirror = axes_mirrored,
      showgrid = FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(
        text = ylab,
        font = f
      ),
      # showline = axes_visible,
      # mirror = axes_mirrored,
      showgrid = theme[["grid"]],
      # gridcolor = grid_col,
      # gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = FALSE
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
    plot_bgcolor = plot_bg
  )

  # Remove padding
  plt[["sizingPolicy"]][["padding"]] <- 0

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
} # draw_varimp
