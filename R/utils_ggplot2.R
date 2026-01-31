# multigplot.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Multipanel **ggplot2** plots
#'
#' Plot a panel of **gplot2** plots
#'
#' @param plots List of ggplot2 plots
#' @param nrows Integer: number of rows for panel arrangement. Defaults to number of rows required
#' to plot 2 plots per row
#' @param byrow Logical: If TRUE, draw plots in order provided by row, otherwise by column.
#'
#' @return Called for its side effect of plotting a panel of **ggplot2** plots.
#'
#' @author EDG
#' @export
multigplot <- function(plots = NULL, nrows = NULL, byrow = TRUE) {
  nplots <- length(plots)
  if (is.null(nrows)) {
    nrows <- ceiling(nplots / 2)
  }
  ncols <- ceiling(nplots / nrows)
  layout <- matrix(seq(nrows * ncols), nrows, byrow = byrow)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))

  for (i in seq(nplots)) {
    id <- which(layout == i, arr.ind = TRUE)
    print(
      plots[[i]],
      vp = grid::viewport(
        layout.pos.row = id[1],
        layout.pos.col = id[2]
      )
    )
  }
  invisible()
} # rtemis::multigplot


# theme_ggplot2.R
# ::rtemis::
# 2019 EDG

#' \pkg{rtemis} `ggplot2` light theme
#'
#' @param base_size Float: Base font size.
#' @param base_family Character: Font family.
#' @param base_line_size Float: Line size.
#' @param base_rect_size Float: Size for rect elements.
#' @param axis_text_size_rel Float: Relative size for axis text.
#' @param legend_key_fill Color: Fill color for legend.
#' @param legend_text_size_rel Float: Relative size for legend text.
#' @param legend_position Character: Legend position, "top", "bottom", "right", "left" Default  = "right"
#' @param strip_background_fill Color: Fill color from facet labels.
#'
#' @return `ggplot2`, `theme` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' (p <- ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
#'   geom_point() +
#'   ggtheme_light())
#' }
ggtheme_light <- function(
  base_size = 14,
  base_family = "Helvetica Neue",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  axis_text_size_rel = 1,
  legend_key_fill = NA,
  legend_text_size_rel = 1,
  legend_position = "right",
  strip_background_fill = "grey85"
) {
  half_line <- base_size / 2

  ggplot2::theme(
    line = ggplot2::element_line(
      colour = "black",
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black",
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(
      size = ggplot2::rel(axis_text_size_rel), # delta
      colour = "grey30"
    ),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
    ),
    axis.ticks = ggplot2::element_line(colour = "grey20"),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line / 2),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(colour = NA),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key = ggplot2::element_rect(
      fill = legend_key_fill, # delta
      colour = "white"
    ),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(
      size = ggplot2::rel(legend_text_size_rel)
    ), # delta
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend_position = legend_position,
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(
      fill = "grey92",
      colour = NA
    ),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = "white"),
    panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(
      fill = strip_background_fill,
      colour = NA
    ),
    strip.text = ggplot2::element_text(
      colour = "grey10",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line
      )
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = "white"),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
} # rtemis::theme_rtemis

#' \pkg{rtemis} `ggplot2` dark theme
#'
#' @inheritParams ggtheme_light
#' @param strip_background_fill Color: Fill color from facet labels.
#'
#' @return `ggplot2`, `theme` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' p <- ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
#'   geom_point() +
#'   ggtheme_light()
#' p
ggtheme_dark <- function(
  base_size = 14,
  base_family = "Helvetica Neue",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  axis_text_size_rel = 1,
  legend_key_fill = NA,
  legend_text_size_rel = 1,
  legend_position = "right",
  strip_background_fill = "gray25"
) {
  half_line <- base_size / 2

  ggplot2::theme(
    line = ggplot2::element_line(
      colour = "white",
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "black", # bg
      colour = "white",
      linewidth = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "white",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(
      size = ggplot2::rel(axis_text_size_rel), # delta
      colour = "gray50"
    ),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
    ),
    axis.ticks = ggplot2::element_line(colour = "gray20"),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line / 2),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(colour = NA),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key = ggplot2::element_rect(
      fill = legend_key_fill, # delta
      colour = "black"
    ),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(
      size = ggplot2::rel(legend_text_size_rel)
    ), # delta
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend_position = legend_position,
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(
      fill = "gray15",
      colour = NA
    ),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(colour = "black"),
    panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(
      fill = strip_background_fill,
      colour = NA
    ),
    strip.text = ggplot2::element_text(
      colour = "gray90",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line,
        0.8 * half_line
      )
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = "black"),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
} # /rtemis::ggtheme_dark
