# draw_pvals.R
# ::rtemis::
# 2021 EDG rtemis.org

#' Barplot p-values using [draw_bar]
#'
#' Plot 1 - p-values as a barplot
#'
#' @param x Float, vector: p-values.
#' @param xnames Character, vector: feature names.
#' @param yname Character: outcome name.
#' @param p_adjust_method Character: method for [p.adjust].
#' @param pval_hline Float: Significance level at which to plot horizontal line.
#' @param hline_col Color for `pval_hline`.
#' @param hline_dash Character: type of line to draw.
#' @param ... Additional arguments passed to [draw_bar].
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' draw_pvals(c(0.01, 0.02, 0.03), xnames = c("Feature1", "Feature2", "Feature3"))
#' }
#' @author EDG
draw_pvals <- function(
  x,
  xnames = NULL,
  yname = NULL,
  p_adjust_method = "none",
  pval_hline = .05,
  hline_col = "#FE4AA3",
  hline_dash = "dash",
  ...
) {
  if (is.null(xnames)) {
    xnames <- names(x)
  }
  if (is.null(yname)) {
    yname <- deparse(substitute(x))
  }

  draw_bar(
    1 - p.adjust(x, method = p_adjust_method),
    group_names = xnames,
    legend = FALSE,
    ylab = if (p_adjust_method == "none") {
      "1 - p-value"
    } else {
      paste0("1 - ", p_adjust_method, "-adjusted p-value")
    },
    hline = 1 - pval_hline,
    hline_col = hline_col,
    hline_dash = hline_dash,
    ...
  )
} # /rtemis::draw_pvals
