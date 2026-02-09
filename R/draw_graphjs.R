# draw_graphjs.R
# ::rtemis::
# EDG rtemis.org

#' Plot network using \pkg{threejs::graphjs}
#'
#' Interactive plotting of an \pkg{igraph} net using \pkg{threejs}.
#'
#' @param net \pkg{igraph} network.
#' @param vertex_size Numeric: Vertex size.
#' @param vertex_col Color for vertices.
#' @param vertex_label_col Color for vertex labels.
#' @param vertex_label_alpha Numeric: Transparency for `vertex_label_col`.
#' @param vertex_frame_col Color for vertex border (frame).
#' @param vertex_label Character vector: Vertex labels. Default = NULL, which will keep existing names in `net` if any. Set to NA to avoid printing vertex labels.
#' @param vertex_shape Character, vector, length 1 or N nodes: Vertex shape. See `graphjs("vertex_shape")`.
#' @param edge_col Color for edges.
#' @param edge_alpha Numeric: Transparency for edges.
#' @param edge_curved Numeric: Curvature of edges.
#' @param edge_width Numeric: Edge thickness.
#' @param layout Character: one of: "fr", "dh", "drl", "gem", "graphopt", "kk", "lgl", "mds", "sugiyama", corresponding to all the available layouts in \pkg{igraph}.
#' @param coords Output of precomputed \pkg{igraph} layout. If provided, `layout` is ignored.
#' @param layout_args List of arguments to pass to `layout` function.
#' @param cluster Character: one of: "edge_betweenness", "fast_greedy", "infomap", "label_prop", "leading_eigen", "louvain", "optimal", "spinglass", "walktrap", corresponding to all the available \pkg{igraph} clustering functions.
#' @param groups Output of precomputed \pkg{igraph} clustering. If provided, `cluster` is ignored.
#' @param cluster_config List of arguments to pass to `cluster` function.
#' @param cluster_mark_groups Logical: If TRUE, draw polygons to indicate clusters, if `groups` or `cluster` are defined.
#' @param cluster_color_vertices Logical: If TRUE, color vertices by cluster membership.
#' @param main Character: Main title.
#' @param theme `Theme` object.
#' @param palette Color vector or name of rtemis palette.
#' @param mar Numeric vector, length 4: `par`'s margin argument.
#' @param filename Character: If provided, save plot to this filepath.
#' @param verbosity Integer: Verbosity level.
#' @param ... Extra arguments to pass to `igraph::plot.igraph()`.
#'
#' @return `scatterplotThree` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' library(igraph)
#' g <- make_ring(10)
#' draw_graphjs(g)
draw_graphjs <- function(
  net,
  vertex_size = 1,
  vertex_col = NULL,
  vertex_label_col = NULL,
  vertex_label_alpha = .66,
  vertex_frame_col = NA,
  vertex_label = NULL,
  vertex_shape = "circle",
  edge_col = NULL,
  edge_alpha = .5,
  edge_curved = .35,
  edge_width = 2,
  layout = c(
    "fr",
    "dh",
    "drl",
    "gem",
    "graphopt",
    "kk",
    "lgl",
    "mds",
    "sugiyama"
  ),
  coords = NULL,
  layout_args = list(),
  cluster = NULL,
  groups = NULL,
  cluster_config = list(),
  cluster_mark_groups = TRUE,
  cluster_color_vertices = FALSE,
  main = "",
  theme = choose_theme(),
  palette = getOption("rtemis_palette", "rtms"),
  mar = rep(0, 4),
  filename = NULL,
  verbosity = 1L,
  ...
) {
  # Dependencies ----
  check_dependencies("igraph", "threejs")

  # Theme ----
  check_is_S7(theme, Theme)

  if (is.character(palette)) {
    palette <- unname(unlist(rtpalette(palette)))
  }

  # Vertex names ----
  # by default use names in input net.
  if (!is.null(vertex_label)) {
    igraph::igraph.options(net, vertex_label = vertex_label)
  }

  # Layout ----
  layout <- match.arg(layout)
  if (is.null(coords) && !is.null(layout)) {
    coords <- do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net, dim = 3), layout_args)
    )
    if (layout == "sugiyama") coords <- coords[["layout"]]
  }

  # Cluster ----
  if (is.null(groups) && !is.null(cluster)) {
    groups <- do.call(
      getFromNamespace(paste0("cluster_", cluster), "igraph"),
      c(list(net), cluster_config)
    )
  }

  if (is.null(vertex_col)) {
    vertex_col <- if (!is.null(groups)) {
      palette <- recycle(palette, length(unique(groups[["membership"]])))
      palette[groups[["membership"]]]
    } else {
      theme[["fg"]]
    }
  }

  if (is.null(vertex_label_col)) {
    vertex_label_col <- theme[["fg"]]
  }
  vertex_label_col <- adjustcolor(vertex_label_col, vertex_label_alpha)

  # Leave edge_col as NULL for auto-coloring with groups
  if (is.null(edge_col) && is.null(groups)) {
    edge_col <- "#18A3AC"
  }

  # Plot ----
  threejs::graphjs(
    net,
    layout = coords,
    vertex.color = vertex_col,
    vertex.size = vertex_size,
    vertex.shape = vertex_shape,
    vertex.label = vertex_label,
    edge.color = edge_col,
    edge.alpha = edge_alpha,
    edge.width = edge_width,
    main = main,
    bg = theme[["bg"]],
    vertex.label.color = vertex_label_col,
    vertex.frame.color = vertex_frame_col,
    edge.curved = edge_curved,
    vertex.label.family = theme[["font_family"]],
    font.main = theme[["font_family"]],
    stroke = NULL,
    verbosity = verbosity,
    ...
  )
} # /rtemis::draw_graphjs
