% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_scatter.R
\name{draw_scatter}
\alias{draw_scatter}
\title{Interactive Scatter Plots}
\usage{
draw_scatter(
  x,
  y = NULL,
  fit = NULL,
  se_fit = FALSE,
  se_times = 1.96,
  include_fit_name = TRUE,
  cluster = NULL,
  cluster_params = list(k = 2),
  group = NULL,
  rsq = TRUE,
  mode = "markers",
  order_on_x = NULL,
  main = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  axes_square = FALSE,
  group_names = NULL,
  font_size = 16,
  marker_col = NULL,
  marker_size = 8,
  symbol = "circle",
  fit_col = NULL,
  fit_alpha = 0.8,
  fit_lwd = 2.5,
  line_shape = "linear",
  se_col = NULL,
  se_alpha = 0.4,
  scatter_type = "scatter",
  show_marginal_x = FALSE,
  show_marginal_y = FALSE,
  marginal_x = x,
  marginal_y = y,
  marginal_x_y = NULL,
  marginal_y_x = NULL,
  marginal_col = NULL,
  marginal_alpha = 0.333,
  marginal_size = 10,
  legend = NULL,
  legend_title = NULL,
  legend_trace = TRUE,
  legend_xy = c(0, 0.98),
  legend_xanchor = "left",
  legend_yanchor = "auto",
  legend_orientation = "v",
  legend_col = NULL,
  legend_bg = "#FFFFFF00",
  legend_border_col = "#FFFFFF00",
  legend_borderwidth = 0,
  legend_group_gap = 0,
  x_showspikes = FALSE,
  y_showspikes = FALSE,
  spikedash = "solid",
  spikemode = "across",
  spikesnap = "hovered data",
  spikecolor = NULL,
  spikethickness = 1,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  main_y = 1,
  main_yanchor = "bottom",
  subtitle_x = 0.02,
  subtitle_y = 0.99,
  subtitle_xref = "paper",
  subtitle_yref = "paper",
  subtitle_xanchor = "left",
  subtitle_yanchor = "top",
  automargin_x = TRUE,
  automargin_y = TRUE,
  xlim = NULL,
  ylim = NULL,
  axes_equal = FALSE,
  diagonal = FALSE,
  diagonal_col = NULL,
  diagonal_dash = "dot",
  diagonal_alpha = 0.66,
  fit_params = NULL,
  vline = NULL,
  vline_col = theme[["fg"]],
  vline_width = 1,
  vline_dash = "dot",
  hline = NULL,
  hline_col = theme[["fg"]],
  hline_width = 1,
  hline_dash = "dot",
  hovertext = NULL,
  width = NULL,
  height = NULL,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  scrollZoom = TRUE,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  verbosity = 0L
)
}
\arguments{
\item{x}{Numeric, vector/data.frame/list: x-axis data. If y is NULL and \code{NCOL(x) > 1}, first two columns used as \code{x} and \code{y}, respectively.}

\item{y}{Numeric, vector/data.frame/list: y-axis data.}

\item{fit}{Character: Fit method.}

\item{se_fit}{Logical: If TRUE, include standard error of the fit.}

\item{se_times}{Numeric: Multiplier for standard error.}

\item{include_fit_name}{Logical: If TRUE, include fit name in legend.}

\item{cluster}{Character: Clustering method.}

\item{cluster_params}{List: Parameters for clustering.}

\item{group}{Factor: Grouping variable.}

\item{rsq}{Logical: If TRUE, print R-squared values in legend if \code{fit} is set.}

\item{mode}{Character, vector: "markers", "lines", "markers+lines".}

\item{order_on_x}{Logical: If TRUE, order \code{x} and \code{y} on \code{x}.}

\item{main}{Character: Main title.}

\item{subtitle}{Character: Subtitle.}

\item{xlab}{Character: x-axis label.}

\item{ylab}{Character: y-axis label.}

\item{col}{Color for markers.}

\item{alpha}{Numeric: Alpha for markers.}

\item{theme}{Theme object.}

\item{palette}{Character: Color palette.}

\item{axes_square}{Logical: If TRUE, draw a square plot.}

\item{group_names}{Character: Names for groups.}

\item{font_size}{Numeric: Font size.}

\item{marker_col}{Color for markers.}

\item{marker_size}{Numeric: Marker size.}

\item{symbol}{Character: Marker symbol.}

\item{fit_col}{Color for fit line.}

\item{fit_alpha}{Numeric: Alpha for fit line.}

\item{fit_lwd}{Numeric: Line width for fit line.}

\item{line_shape}{Character: Line shape for line plots. Options: "linear", "hv", "vh", "hvh", "vhv".}

\item{se_col}{Color for standard error band.}

\item{se_alpha}{Numeric: Alpha for standard error band.}

\item{scatter_type}{Character: Scatter plot type.}

\item{show_marginal_x}{Logical: If TRUE, add marginal distribution line markers on x-axis.}

\item{show_marginal_y}{Logical: If TRUE, add marginal distribution line markers on y-axis.}

\item{marginal_x}{Numeric: Data for marginal distribution on x-axis.}

\item{marginal_y}{Numeric: Data for marginal distribution on y-axis.}

\item{marginal_x_y}{Numeric: Y position of marginal markers on x-axis.}

\item{marginal_y_x}{Numeric: X position of marginal markers on y-axis.}

\item{marginal_col}{Color for marginal markers.}

\item{marginal_alpha}{Numeric: Alpha for marginal markers.}

\item{marginal_size}{Numeric: Size of marginal markers.}

\item{legend}{Logical: If TRUE, draw legend.}

\item{legend_title}{Character: Title for legend.}

\item{legend_trace}{Logical: If TRUE, draw legend trace. (For when you have \code{fit} and don't want a trace for the markers.)}

\item{legend_xy}{Numeric: Position of legend.}

\item{legend_xanchor}{Character: X anchor for legend.}

\item{legend_yanchor}{Character: Y anchor for legend.}

\item{legend_orientation}{Character: Orientation of legend.}

\item{legend_col}{Color for legend text.}

\item{legend_bg}{Color for legend background.}

\item{legend_border_col}{Color for legend border.}

\item{legend_borderwidth}{Numeric: Border width for legend.}

\item{legend_group_gap}{Numeric: Gap between legend groups.}

\item{x_showspikes}{Logical: If TRUE, show spikes on x-axis.}

\item{y_showspikes}{Logical: If TRUE, show spikes on y-axis.}

\item{spikedash}{Character: Dash type for spikes.}

\item{spikemode}{Character: Spike mode.}

\item{spikesnap}{Character: Spike snap mode.}

\item{spikecolor}{Color for spikes.}

\item{spikethickness}{Numeric: Thickness of spikes.}

\item{margin}{List: Plot margins.}

\item{main_y}{Numeric: Y position of main title.}

\item{main_yanchor}{Character: Y anchor for main title.}

\item{subtitle_x}{Numeric: X position of subtitle.}

\item{subtitle_y}{Numeric: Y position of subtitle.}

\item{subtitle_xref}{Character: X reference for subtitle.}

\item{subtitle_yref}{Character: Y reference for subtitle.}

\item{subtitle_xanchor}{Character: X anchor for subtitle.}

\item{subtitle_yanchor}{Character: Y anchor for subtitle.}

\item{automargin_x}{Logical: If TRUE, automatically adjust x-axis margins.}

\item{automargin_y}{Logical: If TRUE, automatically adjust y-axis margins.}

\item{xlim}{Numeric: Limits for x-axis.}

\item{ylim}{Numeric: Limits for y-axis.}

\item{axes_equal}{Logical: If TRUE, set equal scaling for axes.}

\item{diagonal}{Logical: If TRUE, add diagonal line.}

\item{diagonal_col}{Color for diagonal line.}

\item{diagonal_dash}{Character: "solid", "dash", "dot", "dashdot", "longdash", "longdashdot". Dash type for diagonal line.}

\item{diagonal_alpha}{Numeric: Alpha for diagonal line.}

\item{fit_params}{Hyperparameters: Parameters for fit.}

\item{vline}{Numeric: X position for vertical line.}

\item{vline_col}{Color for vertical line.}

\item{vline_width}{Numeric: Width for vertical line.}

\item{vline_dash}{Character: Dash type for vertical line.}

\item{hline}{Numeric: Y position for horizontal line.}

\item{hline_col}{Color for horizontal line.}

\item{hline_width}{Numeric: Width for horizontal line.}

\item{hline_dash}{Character: Dash type for horizontal line.}

\item{hovertext}{List: Hover text for markers.}

\item{width}{Numeric: Width of plot.}

\item{height}{Numeric: Height of plot.}

\item{displayModeBar}{Logical: If TRUE, display mode bar.}

\item{modeBar_file_format}{Character: File format for mode bar.}

\item{scrollZoom}{Logical: If TRUE, enable scroll zoom.}

\item{filename}{Character: Filename to save plot.}

\item{file_width}{Numeric: Width of saved file.}

\item{file_height}{Numeric: Height of saved file.}

\item{file_scale}{Numeric: Scale of saved file.}

\item{verbosity}{Integer: Verbosity level.}
}
\value{
\code{plotly} object.
}
\description{
Draw interactive scatter plots using \code{plotly}.
}
\examples{
\dontrun{
draw_scatter(iris$Sepal.Length, iris$Petal.Length,
  fit = "gam", se_fit = TRUE, group = iris$Species
)
}
}
\author{
EDG
}
