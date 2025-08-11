# draw_pie.R
# ::rtemis::
# 2019 EDG rtemis.org

#' Interactive Pie Chart
#'
#' Draw interactive pie charts using `plotly`.
#'
#' @param x data.frame: Input: Either a) 1 numeric column with categories defined by rownames, or
#' b) two columns, the first is category names, the second numeric or c) a numeric vector with categories defined using
#' the `category.names` argument.
#' @param main Character: Plot title. Default = NULL, which results in `colnames(x)[1]`.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param col Character: Colors for the pie slices.
#' @param alpha Numeric: Alpha for the pie slices.
#' @param bg Character: Background color.
#' @param plot_bg Character: Plot background color.
#' @param theme Theme object.
#' @param palette Character: Color palette to use.
#' @param category_names Character, vector, length = NROW(x): Category names. Default = NULL, which uses
#' either `rownames(x)`, or the first column of `x` if `ncol(x) = 2`.
#' @param textinfo Character: Info to show over each slice: "label", "percent", "label+percent".
#' @param font_size Integer: Font size for labels.
#' @param labs_col Character: Color of labels.
#' @param legend Logical: If TRUE, show legend.
#' @param legend_col Character: Color for legend.
#' @param sep_col Character: Separator color.
#' @param margin List: Margin settings.
#' @param padding Numeric: Padding between cells.
#' @param displayModeBar Logical: If TRUE, display the plotly mode bar.
#' @param modeBar_file_format Character: File format for image exports from the mode bar.
#' @param filename Character: File name to save plot.
#' @param file_width Integer: Width for saved file.
#' @param file_height Integer: Height for saved file.
#' @param file_scale Numeric: Scale for saved file.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' draw_pie(VADeaths[, 1, drop = FALSE])
#' }
draw_pie <- function(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = .8,
  bg = NULL,
  plot_bg = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  category_names = NULL,
  textinfo = "label+percent",
  font_size = 16,
  labs_col = NULL,
  legend = TRUE,
  legend_col = NULL,
  sep_col = NULL,
  margin = list(b = 50, l = 50, t = 50, r = 20),
  padding = 0,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Names ----
  .input_name <- deparse(substitute(x))
  .rownames <- rownames(x)
  .colnames <- colnames(x)
  x <- as.data.frame(x)

  .cat_names <- category_names

  if (NCOL(x) == 2) {
    .cat_names <- as.character(x[, 1])
    x <- x[, 2, drop = FALSE]
    if (is.null(main)) main <- .colnames[2]
  }

  if (is.null(.cat_names)) {
    if (!is.null(.rownames)) {
      .cat_names <- .rownames
    } else {
      .cat_names <- LETTERS[seq_len(NROW(x))]
    }
  }

  if (is.null(main)) {
    if (!is.null(.colnames)) {
      main <- labelify(.colnames[1])
    } else {
      main <- labelify(.input_name)
    }
  }

  if (!is.null(main)) {
    main <- paste0("<b>", main, "</b>")
  }

  # Colors ----
  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }
  p <- NROW(x)
  if (is.null(col)) {
    col <- palette[seq_len(p)]
  }
  if (length(col) < p) {
    col <- rep(col, p / length(col))
  }

  # Theme ----
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])

  if (is.null(legend_col)) {
    legend_col <- labs_col
  }
  sep_col <- if (is.null(sep_col)) bg else plotly::toRGB(sep_col)

  # plotly ----
  plt <- plotly::plot_ly(
    labels = .cat_names,
    values = x[, 1],
    type = "pie",
    textinfo = textinfo,
    insidetextfont = list(color = "#FFFFFF"),
    outsidetextfont = list(color = labs_col),
    marker = list(
      colors = unlist(col),
      line = list(color = sep_col, width = 1)
    )
  )

  ## layout ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  .legend <- list(
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = legend_col
    )
  )
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = ylab,
      showline = FALSE,
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    xaxis = list(
      title = xlab,
      showline = FALSE,
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      )
    ),
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    showlegend = legend,
    legend = .legend
  )

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
} # rtemis::draw_pie.R
