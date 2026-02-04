# S7_MassUni.R
# ::rtemis::
# 2025 EDG rtemis.org

# MassGLM ----
#' @title MassGLM
#'
#' @description
#' Superclass for mass-univariate models.
#'
#' @author EDG
#' @noRd
MassGLM <- new_class(
  name = "MassGLM",
  properties = list(
    summary = class_data.table,
    ynames = class_character,
    xnames = class_character,
    coefnames = class_character,
    family = class_character
  )
) # /rtemis::MassGLM

# Show MassGLM ----
method(repr, MassGLM) <- function(
  x,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("MassGLM"),
    highlight(length(x@ynames)),
    " GLMs of family ",
    bold(x@family),
    " with ",
    highlight(length(x@xnames)),
    ngettext(length(x@xnames), " predictor", " predictors"),
    " each.",
    "\nAvailable coefficients: ",
    paste(highlight(x@coefnames), collapse = ", "),
    "\n"
  )
} # /rtemis::show.MassGLM


# Print MassGLM ----
#' Print MassGLM
#'
#' @param x MassGLM object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
print.MassGLM <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
}

method(print, MassGLM) <- print.MassGLM

# Plot MassGLM ----
#' Plot MassGLM using volcano plot
#'
#' @param x MassGLM object
#' @param coefname Character: Name of coefficient to plot. If `NULL`, the first coefficient is used.
#' @param p_adjust_method Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param p_transform Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param theme `Theme` object
#' @param verbosity Integer: Verbosity level.
#'
#' @param ... Additional arguments passed to [draw_volcano] or [draw_bar]
#'
#' @author EDG
#' @export
plot.MassGLM <- function(
  x,
  coefname = NULL,
  p_adjust_method = "holm",
  p_transform = function(x) -log10(x),
  xlab = "Coefficient",
  ylab = NULL,
  theme = choose_theme(),
  verbosity = 1L,
  ...
) {
  if (is.null(coefname)) {
    coefname <- x@coefnames[1]
  }
  if (!coefname %in% x@coefnames) {
    cli::cli_abort(c(
      "i" = "{.var coefname} must be one of available coefnames: {.strong {x@coefnames}}",
      "x" = "You asked for: {.strong {coefname}}"
    ))
  }
  if (verbosity > 0L) {
    msg(
      "Plotting coefficients for",
      highlight(coefname),
      "x",
      length(x@ynames),
      "outcomes."
    )
  }

  # y-axis label ----
  if (is.null(ylab)) {
    ylab <- fn2label(p_transform, "p-value")
    ylab <- paste(ylab, "for", coefname)
    if (p_adjust_method != "none") {
      ylab <- paste0(ylab, " (", labelify(p_adjust_method), "-corrected)")
    }
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", coefname)]]
  pvals <- x@summary[[paste0("p_value_", coefname)]]
  draw_volcano(
    x = coefs,
    pvals = pvals,
    xnames = x@ynames,
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    theme = theme,
    xlab = xlab,
    ylab = ylab,
    ...
  )
} # /rtemis::plot.MassGLM

method(plot, MassGLM) <- plot.MassGLM


# Plot Manhattan ----
#' @name
#' plot_manhattan
#'
#' @param x MassGLM object.
#' @param coefname Character: Name of coefficient to plot. If `NULL`, the first coefficient is used.
#' @param p_adjust_method Character: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" -
#' p-value adjustment method.
#' @param p_transform Function to transform p-values for plotting. Default is `function(x) -log10(x)`.
#' @param ylab Character: y-axis label.
#' @param theme `Theme` object.
#' @param col_pos Character: Color for positive significant coefficients.
#' @param col_neg Character: Color for negative significant coefficients.
#' @param alpha Numeric: Transparency level for the bars.
#' @param ... Additional arguments passed to [draw_bar].
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # x: outcome of interest as first column, optional covariates in the other columns
#' # y: features whose association with x we want to study
#' set.seed(2022)
#' y <- data.table(rnormmat(500, 40))
#' x <- data.table(
#'   x1 = y[[3]] - y[[5]] + y[[14]] + rnorm(500),
#'   x2 = y[[21]] + rnorm(500)
#' )
#' massmod <- massGLM(x, y)
#' plot_manhattan(massmod)
#' }
plot_manhattan.MassGLM <- method(plot_manhattan, MassGLM) <- function(
  x,
  coefname = NULL,
  p_adjust_method = c(
    "holm",
    "hochberg",
    "hommel",
    "bonferroni",
    "BH",
    "BY",
    "fdr",
    "none"
  ),
  p_transform = function(x) -log10(x),
  ylab = NULL,
  theme = choose_theme(),
  col_pos = "#43A4AC",
  col_neg = "#FA9860",
  alpha = 0.8,
  ...
) {
  p_adjust_method <- match.arg(p_adjust_method)
  if (is.null(coefname)) {
    coefname <- x@coefnames[1]
  }
  if (!coefname %in% x@coefnames) {
    stop(
      "coefname must be one of the coefnames available in the MassGLM object."
    )
  }

  # y-axis label ----
  if (is.null(ylab)) {
    ylab <- fn2label(p_transform, "p-value")
    ylab <- paste(ylab, "for", coefname)
    if (p_adjust_method != "none") {
      ylab <- paste0(ylab, " (", labelify(p_adjust_method), "-corrected)")
    }
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", coefname)]]
  pvals <- x@summary[[paste0("p_value_", coefname)]]
  pvals <- p.adjust(pvals, method = p_adjust_method)
  signif_pos_idi <- pvals < 0.05 & coefs > 0
  signif_neg_idi <- pvals < 0.05 & coefs < 0
  col <- rep(
    adjustcolor(theme[["fg"]], alpha.f = alpha),
    length(pvals)
  )
  col[signif_pos_idi] <- adjustcolor(col_pos, alpha.f = alpha)
  col[signif_neg_idi] <- adjustcolor(col_neg, alpha.f = alpha)

  draw_bar(
    x = p_transform(pvals),
    theme = theme,
    col = col,
    group_names = x@ynames,
    ylab = ylab,
    ...
  )
} # /rtemis::plot_manhattan.MassGLM
