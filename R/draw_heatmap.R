# draw_heatmap.R
# ::rtemis::
# 2017 EDG rtemis.org

#' Interactive Heatmaps
#'
#' Draw interactive heatmaps using `heatmaply`.
#'
#' @param x Input matrix.
#' @param Rowv Logical or dendrogram. If Logical: Compute dendrogram and reorder rows. Defaults to FALSE. If dendrogram: use as is, without reordering. See more at `heatmaply::heatmaply("Rowv")`.
#' @param Colv Logical or dendrogram. If Logical: Compute dendrogram and reorder columns. Defaults to FALSE. If dendrogram: use as is, without reordering. See more at `heatmaply::heatmaply("Colv")`.
#' @param cluster Logical: If TRUE, set `Rowv` and `Colv` to TRUE.
#' @param symm Logical: If TRUE, treat `x` symmetrically - `x` must be a square matrix.
#' @param cellnote Matrix with values to be displayed on hover. Defaults to `ddSci(x)`.
#' @param colorgrad_n Integer: Number of colors in gradient. Default = 101.
#' @param colors Character vector: Colors to use in gradient.
#' @param space Character: Color space to use. Default = "rgb".
#' @param lo Character: Color for low values. Default = "#18A3AC".
#' @param lomid Character: Color for low-mid values.
#' @param mid Character: Color for mid values.
#' @param midhi Character: Color for mid-high values.
#' @param hi Character: Color for high values. Default = "#F48024".
#' @param k_row Integer: Number of desired number of groups by which to color dendrogram branches in the rows. Default = 1.
#' @param k_col Integer: Number of desired number of groups by which to color dendrogram branches in the columns. Default = 1.
#' @param grid_gap Integer: Space between cells. Default = 0 (no space).
#' @param limits Float, length 2: Determine color range. Default = NULL, which automatically centers values around 0.
#' @param margins Float, length 4: Heatmap margins.
#' @param main Character: Main title.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param key_title Character: Title for the color key.
#' @param showticklabels Logical: If TRUE, show tick labels.
#' @param colorbar_len Numeric: Length of the colorbar.
#' @param row_side_colors Data frame: Column names will be label names, cells should be label colors. See `heatmaply::heatmaply("row_side_colors")`.
#' @param row_side_palette Color palette function. See `heatmaply::heatmaply("row_side_palette")`.
#' @param col_side_colors Data frame: Column names will be label names, cells should be label colors. See `heatmaply::heatmaply("col_side_colors")`.
#' @param col_side_palette Color palette function. See `heatmaply::heatmaply("col_side_palette")`.
#' @param font_size Numeric: Font size.
#' @param padding Numeric: Padding between cells.
#' @param displayModeBar Logical: If TRUE, display the plotly mode bar.
#' @param modeBar_file_format Character: File format for image exports from the mode bar.
#' @param filename Character: File name to save the plot.
#' @param file_width Numeric: Width of exported image.
#' @param file_height Numeric: Height of exported image.
#' @param file_scale Numeric: Scale of exported image.
#' @param plot_method Character: Plot method to use. Default = "plotly".
#' @param theme `Theme` object.
#' @param ... Additional arguments to be passed to `heatmaply::heatmaply`.
#'
#' @return `plotly` object.`
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' x <- rnormmat(200, 20)
#' xcor <- cor(x)
#' draw_heatmap(xcor)
#' }
draw_heatmap <- function(
  x,
  Rowv = TRUE,
  Colv = TRUE,
  cluster = FALSE,
  symm = FALSE,
  cellnote = NULL,
  colorgrad_n = 101,
  colors = NULL,
  space = "rgb",
  lo = "#18A3AC",
  lomid = NULL,
  mid = NULL,
  midhi = NULL,
  hi = "#F48024",
  k_row = 1,
  k_col = 1,
  grid_gap = 0,
  limits = NULL,
  margins = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  key_title = NULL,
  showticklabels = NULL,
  colorbar_len = .7,
  plot_method = "plotly",
  theme = choose_theme(),
  row_side_colors = NULL,
  row_side_palette = NULL,
  col_side_colors = NULL,
  col_side_palette = NULL,
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
  check_dependencies("heatmaply")

  # Colnames ----
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(NCOL(x))
  }
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(NROW(x))
  }

  # Margins ----
  # By default, allow 7 px per character
  if (is.null(margins)) {
    bottom <- max(nchar(colnames(x))) * 7 + 15
    left <- max(nchar(rownames(x))) * 7 + 10
    margins <- c(bottom, left, 50, 50)
  }

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

  # Cluster ----
  if (cluster) {
    Rowv <- Colv <- TRUE
  }

  # Cellnote ----
  if (!is.null(cellnote)) {
    if (cellnote == "values") cellnote <- matrix(ddSci(x), NROW(x), NCOL(x))
  }

  # heatmaply ----
  ggp2text <- ggplot2::element_text(
    family = theme[["font_family"]],
    color = theme[["tick_labels_col"]]
  )
  ggp2theme <- ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = theme[["bg"]]),
    plot.background = ggplot2::element_rect(fill = theme[["bg"]]),
    legend.text = ggplot2::element_text(color = theme[["fg"]]),
    legend.background = ggplot2::element_rect(fill = theme[["bg"]]),
    text = ggp2text,
    title = ggp2text,
    axis.text = ggp2text,
    axis.text.x = ggp2text,
    axis.text.y = ggp2text,
    axis.title.x = ggp2text,
    axis.title.y = ggp2text,
    plot.subtitle = ggp2text,
    plot.caption = ggp2text
  )

  # if (is.character(palette)) palette <- rtpalette(palette)

  # Dendrogram ----
  # for now, set to theme[["fg"]]
  Rowv <- x |>
    dist() |>
    hclust() |>
    as.dendrogram() |>
    dendextend::set("branches_k_color", k = 1) |>
    dendextend::set("branches_lwd", 1) |>
    dendextend::set("branches_col", theme[["fg"]]) |>
    dendextend::ladderize()
  #    rotate_DendSer(ser_weight = dist(x))
  Colv <- x |>
    t() |>
    dist() |>
    hclust() |>
    as.dendrogram() |>
    dendextend::set("branches_k_color", k = 1) |>
    dendextend::set("branches_lwd", 1) |>
    dendextend::set("branches_col", theme[["fg"]]) |>
    dendextend::ladderize()

  plt <- suppressWarnings(heatmaply::heatmaply(
    x,
    Rowv = Rowv,
    Colv = Colv,
    symm = symm,
    cellnote = cellnote,
    colors = colors,
    grid_gap = grid_gap,
    limits = limits,
    margins = margins,
    key_title = key_title,
    xlab = xlab,
    ylab = ylab,
    # main = main,
    k_row = k_row,
    k_col = k_col,
    plot_method = plot_method,
    colorbar_len = colorbar_len,
    showticklabels = showticklabels,
    heatmap_layers = ggp2theme,
    row_side_colors = row_side_colors,
    row_side_palette = row_side_palette,
    col_side_colors = col_side_colors,
    col_side_palette = col_side_palette
    # side_color_layers = ggp2theme,
    # file = filename
  ))

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
      color = bg
    )
  )

  plt <- plotly::layout(
    plt,
    yaxis2 = list(
      title = list(
        # text = ylab,
        font = f
      ), # gets assigned to dendrogram
      titlefont = f,
      # showgrid = FALSE,
      tickcolor = bg,
      showline = FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickfont = tickfont
    ),
    xaxis = list(
      title = list(
        # text = xlab,
        font = f
      ),
      titlefont = f,
      # showgrid = FALSE,
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
    plot_bgcolor = bg,
    legend = .legend
    # margin = margin
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

  ## Override colorbar tick font color to theme[["fg"]]
  plt[["x"]][["data"]][[3]][["colorbar"]][["tickfont"]] <- list(
    family = theme[["font_family"]],
    color = tick_labels_col
  )
  plt[["x"]][["data"]][[3]][["colorbar"]][["tickcolor"]] <- theme[["tick_col"]]

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
} # /rtemis::draw_heatmap
