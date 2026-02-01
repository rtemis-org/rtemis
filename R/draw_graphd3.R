# draw_graphD3
# ::rtemis::
# EDG rtemis.org

#' Plot graph using \pkg{networkD3}
#'
#' @param net \pkg{igraph} network.
#' @param groups Vector, length n nodes indicating group/cluster/community membership of nodes in `net`.
#' @param color_scale D3 colorscale (e.g. `networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20b);")`).
#' @param edge_col Color for edges.
#' @param node_col Color for nodes.
#' @param node_alpha Float \[0, 1\]: Node opacity.
#' @param edge_alpha Float \[0, 1\]: Edge opacity.
#' @param zoom Logical: If TRUE, graph is zoomable.
#' @param legend Logical: If TRUE, display legend for groups.
#' @param palette Vector of colors, or Character defining a builtin palette - get options with `rtpalette()`.
#' @param theme `Theme` object.
#' @param ... Additional arguments to pass to `networkD3`.
#'
#' @return `forceNetwork` object.
#'
#' @author EDG
#' @export
draw_graphD3 <- function(
  net,
  groups = NULL,
  color_scale = NULL,
  edge_col = NULL,
  node_col = NULL,
  node_alpha = .5,
  edge_alpha = .33,
  zoom = TRUE,
  legend = FALSE,
  palette = rtemis_palette,
  theme = choose_theme(),
  ...
) {
  # Dependencies ----
  check_dependencies("networkD3")

  # Theme ----
  check_is_S7(theme, Theme)

  netd3 <- networkD3::igraph_to_networkD3(net)
  if (is.null(groups)) {
    netd3[["nodes"]][["group"]] <- "A"
  } else {
    netd3[["nodes"]][["group"]] <- groups
  }

  # Colors ----
  if (is.null(node_col) && length(unique(netd3[["nodes"]][["group"]])) == 1) {
    node_col <- theme[["fg"]]
  }

  if (is.null(color_scale)) {
    if (length(unique(netd3[["nodes"]][["group"]])) == 1) {
      color_scale <- paste0(
        'd3.scaleOrdinal().domain(["A"]).range(["',
        adjustcolor(node_col, node_alpha),
        '"]);'
      )
    } else {
      if (is.character(palette)) {
        palette <- adjustcolor(unlist(rtpalette(palette)), node_alpha)
      }
      ngroups <- length(unique(groups))
      .groups <- paste0(sort(unique(groups)), collapse = '", "')
      if (ngroups > length(palette)) {
        palette <- rep(palette, ngroups / length(palette))
      }
      .colors <- paste0(palette[seq(ngroups)], collapse = '", "')
      color_scale <- paste0(
        'd3.scaleOrdinal().domain(["',
        .groups,
        '"]).range(["',
        .colors,
        '"]);'
      )
    }
  }

  if (is.null(edge_col)) {
    if (is.null(groups)) {
      edge_col <- adjustcolor("#18A3AC", edge_alpha)
    } else {
      edge_col <- adjustcolor(theme[["fg"]], edge_alpha)
    }
  } else {
    edge_col <- adjustcolor(edge_col, edge_alpha)
  }

  # Plot ----
  fn <- networkD3::forceNetwork(
    Links = netd3[["links"]],
    Nodes = netd3[["nodes"]],
    Source = "source",
    Target = "target",
    NodeID = "name",
    Group = "group",
    colourScale = color_scale,
    linkColour = edge_col,
    opacity = 1,
    legend = legend,
    zoom = zoom
  )

  # fn$x$nodes$border <- border.groups
  fn <- htmlwidgets::onRender(
    fn,
    'function(el, x) { d3.selectAll("circle").style("stroke", d => "#ffffff00"); }'
  )

  fn
} # /rtemis::draw_graphD3
