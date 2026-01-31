# igraph_layout
# ::rtemis::
# 2022 EDG rtemis.org

#' igraph Layout Wrapper
#'
#' Wrapper for various igraph layout functions
#'
#' @param net \pkg{igraph} network.
#' @param layout Character: Layout method. One of:
#'   * "bipartite"
#'   * "circle"
#'   * "drl"
#'   * "fr"
#'   * "grid"
#'   * "kk"
#'   * "mds"
#'   * "nicely"
#'   * "sphere"
#'   * "star"
#'   * "sugiyama"
#'   * "tree"
#' @param dims Integer: Number of dimensions. 2 or 3.
#' @return Matrix of x, y (and z) coordinates.
#' @author EDG
#'
#' @keywords internal
#' @noRd
igraph_layout <- function(net, layout = "mds", dims = 2L) {
  if (layout == "bipartite") {
    igraph::layout_as_bipartite(net)
  } else if (layout == "star") {
    out <- igraph::layout_as_star(net)
    if (dims == 3) {
      out <- cbind(out, 0)
    }
    out
  } else if (layout == "circle") {
    out <- igraph::layout_in_circle(net)
    if (dims == 3) {
      out <- cbind(out, 0)
    }
    out
  } else if (layout == "sphere") {
    igraph::layout_on_sphere(net)
  } else if (layout == "tree") {
    out <- igraph::layout_as_tree(net)
    if (dims == 3) {
      out <- cbind(out, 0)
    }
    out
  } else if (layout == "nicely") {
    igraph::layout_nicely(net, dim = dims)
  } else if (layout == "grid") {
    igraph::layout_on_grid(net, dim = dims)
  } else if (layout %in% c("drl", "fr", "kk", "mds")) {
    do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net), dim = dims)
    )
  } else if (layout == "sugiyama") {
    out <- igraph::layout_with_sugiyama(net)$layout
    if (dims == 3) {
      out <- cbind(out, 0)
    }
    out
  } else {
    out <- do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net))
    )
    if (dims == 3) {
      out <- cbind(out, 0)
    }
    out
  }
} # /rtemis::igraph_layout


# dims: drl, fr, kk, mds

# graph_node_metrics.R
# ::rtemis::
# 2021 EDG rtemis.org

#' Node-wise (i.e. vertex-wise) graph metrics
#'
#' @param x \pkg{igraph} network.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `data.frame`.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' datcor <- cor(rnormmat(20, 20, seed = 2021))
#' datcor[sample(seq(datcor), 250)] <- 0
#' x <- igraph::graph_from_adjacency_matrix(
#'   adjmatrix = datcor,
#'   mode = "lower",
#'   weighted = TRUE,
#'   diag = FALSE
#' )
#'
#' graph_node_metrics(x)
#' }
graph_node_metrics <- function(x, verbosity = 1L) {
  if (!inherits(x, "igraph")) {
    cli::cli_abort("Input must be igraph object")
  }

  .nodes <- as.character(igraph::V(x))
  .edgeweights <- igraph::E(x)[["weight"]]
  .degree <- igraph::degree(x)
  .strength <- igraph::strength(x)

  if (any(.edgeweights < 0)) {
    if (verbosity > 0L) {
      msg("Negative weights found: Betweenness and Closeness will be NA")
    }
    .betweenness <- .closeness <- NA
  } else {
    .betweenness <- igraph::betweenness(x)
    .closeness <- igraph::closeness(x)
  }

  .eigen_centrality <- igraph::eigen_centrality(x)[["vector"]]
  .hub_score <- igraph::hub_score(x)[["vector"]]
  .page_rank <- igraph::page_rank(x)[["vector"]]
  .authority_score <- igraph::authority_score(x)[["vector"]]

  data.frame(
    Node = .nodes,
    Degree = .degree,
    Strength = .strength,
    Betwenness = .betweenness,
    Closeness = .closeness,
    Eigen_Centrality = .eigen_centrality,
    Hub_Score = .hub_score,
    Page_Rank = .page_rank,
    Authority_Score = .authority_score
  )
} # /rtemis::graph_node_metrics


# lotri2edgeList.R
# ::rtemis::
# 2016 EDG rtemis.org

#' Connectivity Matrix to Edge List
#'
#' Turn the lower triangle of a connectivity matrix (e.g. correlation matrix or similar)
#'  to an edge list of the form:
#'       Source, Target, Weight
#'
#' The output can be read, for example, into gephi
#' @param A Square matrix
#' @param filename Character: Path for csv file. Defaults to "conmat2edgelist.csv"
#' @param verbosity Integer: Verbosity level.
#'
#' @return Data frame with columns: NodeA, NodeB, Weight
#'
#' @author EDG
#' @export
#'
#' @examples
#' A <- matrix(rnorm(100), nrow = 10)
#' A[lower.tri(A)] <- t(A)[lower.tri(A)]
#' diag(A) <- 1
#' edgelist <- lotri2edgeList(A, filename = NULL)
#' head(edgelist)
lotri2edgeList <- function(A, filename = NULL, verbosity = 1L) {
  # Check A is a square matrix
  dim.A <- dim(A)
  if (verbosity > 0L) {
    msg("Input dimensions are", dim.A)
  }
  if (dim.A[1] != dim.A[2]) {
    cli::cli_abort("Error: Input matrix is not square.")
  }
  n <- dim.A[1]
  l <- list()
  # low tri has n(n-1)/2
  l[["w"]] <- A[lower.tri(A)]
  c <- 1
  l[["r"]] <- c()
  l[["c"]] <- c()
  for (i in 1:(n - 1)) {
    l[["r"]] <- c(l[["r"]], (c + 1):n)
    l[["c"]] <- c(l[["c"]], rep(c, n - c))
    c <- c + 1
  }
  out <- data.frame(NodeA = l[["c"]], NodeB = l[["r"]], Weight = l[["w"]])
  gephiout <- data.frame(
    Source = l[["c"]],
    Target = l[["r"]],
    Weight = l[["w"]]
  )
  if (!is.null(filename)) {
    write.table(
      gephiout,
      file = filename,
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      sep = ","
    )
    msg0("Saved ", highlight(filename), ".")
  }
  invisible(out)
} # /rtemis::lotri2edgelist
