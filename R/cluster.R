# cluster.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Perform Clustering
#'
#' Perform clustering on the rows (usually cases) of a dataset.
#'
#' @param x Matrix or data.frame: Data to cluster. Rows are cases to be clustered.
#' @param algorithm Character: Clustering algorithm.
#' @param parameters List: Algorithm-specific parameters.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Clustering object.
#'
#' @author EDG
#' @export
cluster <- function(
  x,
  algorithm = "KMeans",
  parameters = NULL,
  verbosity = 1L
) {
  # Checks ----
  if (is.null(parameters)) {
    parameters <- get_default_clusterparams(algorithm)
  }
  check_is_S7(parameters, ClusteringParameters)

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
    args = list(x = x, parameters = parameters, verbosity = verbosity)
  )

  # Clusters ----
  clusters <- do_call(
    fn = get_clustpredict_fn(algorithm),
    args = list(clust = clust)
  )

  if (!is.null(parameters[["k"]])) {
    # For algorithms where k is specified in parameters
    k <- parameters[["k"]]
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
    parameters = parameters
  )
} # /rtemis::cluster
