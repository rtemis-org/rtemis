# draw_survfit.R
# ::rtemis::
# 2025 EDG rtemis.org

# draw_scatter(time, survival_prob, mode = "lines", line_shape = "hv")
# ?median lines, error bands, nrisk_table

#' Draw a survfit object
#'
#' Draw a `survfit` object using [draw_scatter].
#'
#' @inheritParams draw_scatter
#'
#' @param x `survfit` object created by [survival::survfit].
#' @param mode Character, vector: "markers", "lines", "markers+lines".
# @param plot_median Logical: If `TRUE`, draw line(s) at 50% survival.
#' @param xlim Numeric vector of length 2: x-axis limits.
#' @param ylim Numeric vector of length 2: y-axis limits.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param main Character: Main title.
#' @param symbol Character: Symbol to use for the points.
#' @param nrisk_table Logical: If `TRUE`, subplot a table of the number at risk at each time point.
#' @param ... Additional arguments passed to [draw_scatter].
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' # Get the lung dataset
#' data(cancer, package = "survival")
#' sf1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = lung)
#' draw_survfit(sf1)
#' sf2 <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
#' draw_survfit(sf2)
#' # with N at risk table
#' draw_survfit(sf2)
draw_survfit <- function(
  x,
  # plot_median = TRUE,
  mode = "lines",
  symbol = "cross",
  line_shape = "hv",
  xlim = NULL,
  ylim = NULL,
  xlab = "Time",
  ylab = "Survival",
  main = NULL,
  legend_xy = c(1, 1),
  legend_xanchor = "right",
  legend_yanchor = "top",
  theme = choose_theme(getOption("rtemis_theme")),
  nrisk_table = FALSE,
  filename = NULL,
  ...
) {
  # Checks ----
  check_inherits(x, "survfit")

  # Data ----
  nstrata <- if (is.null(x[["strata"]])) {
    1
  } else {
    length(x[["strata"]])
  }

  if (nstrata > 1) {
    .group <- unlist(sapply(seq_len(nstrata), function(i) {
      rep(i, x[["strata"]][i])
    }))
  } else {
    .group <- rep(1, length(x[["time"]]))
  }

  # Limits ----
  if (is.null(xlim)) {
    xlim <- c(0, max(x[["time"]], na.rm = TRUE))
  }
  if (is.null(ylim)) {
    ylim <- c(0, 1)
  }

  # Plot ----
  draw_scatter(
    x = split(x[["time"]], .group),
    y = split(x[["surv"]], .group),
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    main = main,
    theme = theme,
    mode = mode,
    symbol = symbol,
    line_shape = line_shape,
    filename = filename,
    legend_xy = legend_xy,
    legend_xanchor = legend_xanchor,
    legend_yanchor = legend_yanchor,
    ...
  )
} # /rtemis::draw_survfit
