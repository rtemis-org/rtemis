# theme.R
# ::rtemis::
# EDG rtemis.org

# Black ----
#' Themes for `draw_*` functions
#'
#' @param bg Color: Figure background.
#' @param plot_bg Color: Plot region background.
#' @param fg Color: Foreground color used as default for multiple elements like
#' axes and labels, which can be defined separately.
#' @param pch Integer: Point character.
#' @param cex Float: Character expansion factor.
#' @param lwd Float: Line width.
#' @param bty Character: Box type:  "o", "l", "7", "c", "u", or "]", or "n".
#' @param box_col Box color if `bty != "n"`.
#' @param box_alpha Float: Box alpha.
#' @param box_lty Integer: Box line type.
#' @param box_lwd Float: Box line width.
#' @param grid Logical: If TRUE, draw grid in plot regions.
#' @param grid_nx Integer: N of vertical grid lines.
#' @param grid_ny Integer: N of horizontal grid lines.
#' @param grid_col Grid color.
#' @param grid_alpha Float: Grid alpha.
#' @param grid_lty Integer: Grid line type.
#' @param grid_lwd Float: Grid line width.
#' @param axes_visible Logical: If TRUE, draw axes.
#' @param axes_col Axes colors.
#' @param tick_col Tick color.
#' @param tick_alpha Float: Tick alpha.
#' @param tick_labels_col Tick labels' color.
#' @param tck `graphics::parr`'s tck argument: Tick length, can be negative.
#' @param tcl `graphics::parr`'s tcl argument.
#' @param x_axis_side Integer: Side to place x-axis.
#' @param y_axis_side Integer: Side to place y-axis.
#' @param labs_col Labels' color.
#' @param x_axis_line Numeric: `graphics::axis`'s `line` argument for the x-axis.
#' @param x_axis_las Numeric: `graphics::axis`'s `las` argument for the x-axis.
#' @param x_axis_padj Numeric: x-axis' `padj`: Adjustment for the x-axis
#' tick labels' position.
#' @param x_axis_hadj Numeric: x-axis' `hadj`.
#' @param y_axis_line Numeric: `graphics::axis`'s `line` argument for the y-axis.
#' @param y_axis_las Numeric: `graphics::axis`'s `las` argument for the y-axis.
#' @param y_axis_padj Numeric: y-axis' `padj`.
#' @param y_axis_hadj Numeric: y-axis' `hadj`.
#' @param xlab_line Numeric: Line to place `xlab`.
#' @param ylab_line Numeric: Line to place `ylab`.
#' @param zerolines Logical: If TRUE, draw lines on x = 0, y = 0, if within
#' plot limits.
#' @param zerolines_col Zerolines color.
#' @param zerolines_alpha Float: Zerolines alpha.
#' @param zerolines_lty Integer: Zerolines line type.
#' @param zerolines_lwd Float: Zerolines line width.
#' @param main_line Float: How many lines away from the plot region to draw
#' title.
#' @param main_adj Float: How to align title.
#' @param main_font Integer: 1: Regular, 2: Bold.
#' @param main_col Title color.
#' @param font_family Character: Font to be used throughout plot.
#'
#' @return `Theme` object.
#'
#' @rdname theme
#' @export
#'
#' @examples
#' theme <- theme_black(font_family = "Geist")
#' theme
theme_black <- function(
  bg = "#000000",
  plot_bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = FALSE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = fg,
  grid_alpha = .2,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = fg,
  tick_alpha = .5,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "black",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_black

#' @rdname theme
#' @export

theme_blackgrid <- function(
  bg = "#000000",
  plot_bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = fg,
  grid_alpha = .2,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = fg,
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "blackgrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_blackgrid


#' @rdname theme
#' @export

theme_blackigrid <- function(
  bg = "#000000",
  plot_bg = "#1A1A1A",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = bg,
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = fg,
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "blackigrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_darkgrid

# Darkgray ----

#' @rdname theme
#' @export
theme_darkgray <- function(
  bg = "#121212",
  plot_bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = FALSE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = fg,
  grid_alpha = .2,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = fg,
  tick_alpha = .5,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "darkgray",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_darkgray

#' @rdname theme
#' @export
theme_darkgraygrid <- function(
  bg = "#121212",
  plot_bg = "transparent",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = "#404040",
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "#00000000",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "darkgraygrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_darkgraygrid

#' @rdname theme
#' @export
theme_darkgrayigrid <- function(
  bg = "#121212",
  plot_bg = "#202020",
  fg = "#ffffff",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = bg,
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "transparent",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "darkgrayigrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_darkgrayigrid

# White ----

#' @rdname theme
#' @export
theme_white <- function(
  bg = "#ffffff",
  plot_bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = FALSE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = fg,
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = fg,
  tick_alpha = .5,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "white",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_white

#' @rdname theme
#' @export
theme_whitegrid <- function(
  bg = "#ffffff",
  plot_bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = "#c0c0c0",
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "#00000000",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "whitegrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_whitegrid

#' @rdname theme
#' @export
theme_whiteigrid <- function(
  bg = "#ffffff",
  plot_bg = "#E6E6E6",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = bg,
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "transparent",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "whiteigrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_whiteigrid

# Grays ----

#' @rdname theme
#' @export
theme_lightgraygrid <- function(
  bg = "#dfdfdf",
  plot_bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = "#c0c0c0",
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "#00000000",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "lightgraygrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_lightgray


#' @rdname theme
#' @export
theme_mediumgraygrid <- function(
  bg = "#b3b3b3",
  plot_bg = "transparent",
  fg = "#000000",
  pch = 16,
  cex = 1,
  lwd = 2,
  # box --
  bty = "n",
  box_col = fg,
  box_alpha = 1,
  box_lty = 1,
  box_lwd = .5,
  # grid --
  grid = TRUE,
  grid_nx = NULL,
  grid_ny = NULL,
  grid_col = "#d0d0d0",
  grid_alpha = 1,
  grid_lty = 1,
  grid_lwd = 1,
  # axes --
  axes_visible = TRUE,
  axes_col = "transparent",
  tick_col = "#00000000",
  tick_alpha = 1,
  tick_labels_col = fg,
  tck = -0.01,
  tcl = NA,
  x_axis_side = 1,
  y_axis_side = 2,
  labs_col = fg,
  x_axis_line = 0,
  x_axis_las = 0,
  x_axis_padj = -1.1,
  x_axis_hadj = .5,
  y_axis_line = 0,
  y_axis_las = 1,
  y_axis_padj = .5,
  y_axis_hadj = .5,
  xlab_line = 1.4,
  ylab_line = 2,
  # zerolines --
  zerolines = TRUE,
  zerolines_col = fg,
  zerolines_alpha = .5,
  zerolines_lty = 1,
  zerolines_lwd = 1,
  # title --
  main_line = .25,
  main_adj = 0,
  main_font = 2,
  main_col = fg,
  font_family = "Helvetica"
) {
  Theme(
    name = "mediumgraygrid",
    config = list(
      bg = bg,
      plot_bg = plot_bg,
      fg = fg,
      pch = pch,
      cex = cex,
      lwd = lwd,
      # box --
      bty = bty,
      box_col = box_col,
      box_alpha = box_alpha,
      box_lty = box_lty,
      box_lwd = box_lwd,
      # grid --
      grid = grid,
      grid_nx = grid_nx,
      grid_ny = grid_ny,
      grid_col = grid_col,
      grid_alpha = grid_alpha,
      grid_lty = grid_lty,
      grid_lwd = grid_lwd,
      # axes --
      axes_visible = axes_visible,
      axes_col = axes_col,
      tick_col = tick_col,
      tick_alpha = tick_alpha,
      tick_labels_col = tick_labels_col,
      tck = tck,
      tcl = tcl,
      x_axis_side = x_axis_side,
      y_axis_side = y_axis_side,
      labs_col = labs_col,
      x_axis_line = x_axis_line,
      x_axis_las = x_axis_las,
      x_axis_padj = x_axis_padj,
      x_axis_hadj = x_axis_hadj,
      y_axis_line = y_axis_line,
      y_axis_las = y_axis_las,
      y_axis_padj = y_axis_padj,
      y_axis_hadj = y_axis_hadj,
      xlab_line = xlab_line,
      ylab_line = ylab_line,
      # zerolines --
      zerolines = zerolines,
      zerolines_col = zerolines_col,
      zerolines_alpha = zerolines_alpha,
      zerolines_lty = zerolines_lty,
      zerolines_lwd = zerolines_lwd,
      # title --
      main_line = main_line,
      main_adj = main_adj,
      main_font = main_font,
      main_col = main_col,
      font_family = font_family
    )
  )
} # /rtemis::theme_mediumdgray

#' Print available \pkg{rtemis} themes
#'
#' @return Called for its side effect of printing available themes.
#'
#' @export
available_themes <- function() {
  cat(highlight("  Available themes:\n"))
  cat('    "white", "whitegrid", "whiteigrid,\n')
  cat('    "black", "blackgrid", "blackigrid",\n')
  cat('    "darkgray", "darkgraygrid", "darkgrayigrid",\n')
  cat('    "lightgraygrid", "mediumgraygrid"\n')
  invisible()
}


#' Select an rtemis theme
#'
#' @param x Character: Name of theme to select. If not defined, will use `getOption("rtemis_theme", "whitegrid")`.
#' @param config List: Optional list of config to override the defaults.
#'
#' @return `Theme` object.
#'
#' @author EDG
#' @export
choose_theme <- function(
  x = c(
    "white",
    "whitegrid",
    "whiteigrid",
    "black",
    "blackgrid",
    "blackigrid",
    "darkgray",
    "darkgraygrid",
    "darkgrayigrid",
    "lightgraygrid",
    "mediumgraygrid"
  ),
  config = NULL
) {
  if (length(x) > 1) {
    x <- getOption("rtemis_theme", "whitegrid")
  }
  if (is.null(config)) {
    config <- list()
  }

  do_call(paste0("theme_", x), config)
} # /rtemis::theme
