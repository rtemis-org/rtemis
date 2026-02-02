# cluster.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Perform Clustering
#'
#' Perform clustering on the rows (usually cases) of a dataset.
#'
#' @details
#' See [rdocs.rtemis.org/cluster](https://rdocs.rtemis.org/cluster) for detailed documentation.
#'
#' @param x Matrix or data.frame: Data to cluster. Rows are cases to be clustered.
#' @param algorithm Character: Clustering algorithm.
#' @param config List: Algorithm-specific config.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Clustering` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' iris_km <- cluster(exc(iris, "Species"), algorithm = "KMeans")
#' }
cluster <- function(
  x,
  algorithm = "KMeans",
  config = NULL,
  verbosity = 1L
) {
  # Checks ----
  if (is.null(config)) {
    config <- get_default_clusterparams(algorithm)
  }
  check_is_S7(config, ClusteringConfig)

  # Intro ----
  start_time <- intro(verbosity = verbosity)

  # Data ----
  if (verbosity > 0L) {
    summarize_unsupervised(x)
  }

  # Cluster ----
  algorithm <- get_clust_name(algorithm)
  if (verbosity > 0L) {
    msg0(bold(paste0("Clustering with ", algorithm, "...")))
  }
  clust <- do_call(
    fn = get_clust_fn(algorithm),
    args = list(x = x, config = config, verbosity = verbosity)
  )

  # Clusters ----
  clusters <- do_call(
    fn = get_clustpredict_fn(algorithm),
    args = list(clust = clust)
  )

  if (!is.null(config[["k"]])) {
    # For algorithms where k is specified in config
    k <- config[["k"]]
  } else {
    # For algorithms where k is not prescribed, but determined from the clustering result
    k <- length(unique(clusters))
    if (verbosity > 0L) {
      msg0(paste0("Found ", highlight(k), " clusters."))
    }
  }

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Clustering(
    algorithm = algorithm,
    clust = clust,
    k = k,
    clusters = clusters,
    config = config
  )
} # /rtemis::cluster
