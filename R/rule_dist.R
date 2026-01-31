# rule_dist.R
# ::rtemis::
# 2019 EDG rtemis.org

#' Rule distance
#'
#' Calculate pairwise distance among a set of rules or between two sets of rules,
#' where each rule defines a subpopulation.
#'
#' If only rules1 is provided, computes pairwise distance among rules1, otherwise computes pairwise
#' distance between rules1 and rules2.
#'
#' @param x Data frame / matrix: Input features (cases by features).
#' @param rules1 Character, vector: Rules as combination of conditions on the features of `x`.
#' @param rules2 String, vector, Optional: Rules as combination of conditions on the features of
#' `x`.
#' @param print_plot Logical: If TRUE, plot heatmap for calculated distance.
#' @param plot_type Character: "static", "interactive": type of graphics to use, base or plotly,
#' respectively.
#' @param heat_lo Color: Heatmap low color.
#' @param heat_mid Color: Heatmap mid color. Default = NA (i.e. create gradient from `heat_lo` to
#' `heat_hi`)
#' @param heat_hi Colo: Heatmap hi colo.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `rtrule_dist` object.
#'
#' @author EDG
#' @export

rule_dist <- function(
  x,
  rules1,
  rules2 = NULL,
  print_plot = TRUE,
  plot_type = c("static", "interactive"),
  heat_lo = "black",
  heat_mid = NA,
  heat_hi = "#F48024",
  verbosity = 1L
) {
  #  Match cases by rules
  cxr1 <- match_cases_by_rules(x, rules1, verbosity)

  if (!is.null(rules2)) {
    cxr2 <- match_cases_by_rules(x, rules2, verbosity)
  }

  # Rule by rule distance
  if (is.null(rules2)) {
    rxr.hamming <- apply(cxr1, 2, function(i) matrixStats::colSums2(i != cxr1))
  } else {
    rxr.hamming <- sapply(
      seq_along(rules1),
      function(i) matrixStats::colSums2(cxr1[, i] != cxr2)
    )
  }

  # Rule total distance
  rules_total_dist1 <- matrixStats::colSums2(rxr.hamming)

  # Order rules by total distance
  rules_ordered1 <- order(rules_total_dist1, decreasing = TRUE)

  rules_total_dist2 <- rules_ordered2 <- NULL
  if (!is.null(rules2)) {
    rules_total_dist2 <- matrixStats::rowSums2(rxr.hamming)

    # Order rules by total distance
    rules_ordered2 <- order(rules_total_dist2, decreasing = TRUE)
  }

  # Plot ----
  if (print_plot) {
    draw_heatmap(
      rxr.hamming,
      Rowv = TRUE,
      Colv = TRUE,
      limits = c(0, nrow(x)),
      lo = heat_lo,
      mid = heat_mid,
      hi = heat_hi
    )
  }

  out <- list(
    rxr.hamming = rxr.hamming,
    rules_total_dist1 = rules_total_dist1,
    rules_ordered1 = rules_ordered1
  )

  if (!is.null(rules2)) {
    out[["rules_total_dist2"]] <- rules_total_dist2
    out[["rules_ordered2"]] <- rules_ordered2
  }

  class(out) <- c("rtrule_dist", "list")
  out
} # /rtemis::rule_dist


hamming <- function(x, y) {
  sum(x != y)
} # /rtemis::hamming
