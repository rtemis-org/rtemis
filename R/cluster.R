# cluster.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster ----
#' Perform Clustering
#'
#' Perform clustering on the rows (usually cases) of a dataset.
#'
#' @details
#' See [docs.rtemis.org/r](https://docs.rtemis.org/r/) for detailed documentation.
#'
#' @param x Matrix, data.frame, or `ClusterConfig` object: Data to cluster (rows
#' are cases to be clustered), or a `ClusterConfig` recipe (from
#' [setup_ClusterConfig]) carrying the data path, algorithm config, and output
#' directory.
#' @param algorithm Character: Clustering algorithm.
#' @param config List: Algorithm-specific config.
#' @param outdir Character, optional: Output directory. If not NULL, the returned
#' `Clustering` object is saved there as an `.rds` file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Clustering` object.
#'
#' @author EDG
#' @export
#' @examples
#' iris_km <- cluster(exc(iris, "Species"), algorithm = "KMeans")
cluster <- function(
  x,
  algorithm = "KMeans",
  config = NULL,
  outdir = NULL,
  verbosity = 1L
) {
  # ClusterConfig dispatch ----
  if (S7_inherits(x, ClusterConfig)) {
    # `ClusterConfig` is a recipe: `dat_path` may be unbound. Require it at
    # cluster time (the CLI sets it from its data argument before calling).
    if (is.null(x@dat_path)) {
      rtemis.core::abort(
        "This `ClusterConfig` has no `dat_path`; set it before clustering ",
        '(e.g. `x@dat_path <- "data.csv"`).',
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    # The algorithm label prefers an explicit top-level `algorithm`, falling back
    # to the one carried by `clustering_config`, then the formal default.
    algorithm <- x@algorithm
    if (is.null(algorithm) && !is.null(x@clustering_config)) {
      algorithm <- x@clustering_config@algorithm
    }
    if (is.null(algorithm)) {
      algorithm <- "KMeans"
    }
    return(cluster(
      x = read(x@dat_path),
      algorithm = algorithm,
      config = x@clustering_config,
      outdir = x@outdir,
      verbosity = x@verbosity
    ))
  } # / cluster.ClusterConfig

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
  msg0(
    bold(paste0("Clustering with ", algorithm, "...")),
    verbosity = verbosity
  )
  clust <- cluster_(config = config, x = x, verbosity = verbosity)

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
  out <- Clustering(
    algorithm = algorithm,
    clust = clust,
    k = k,
    clusters = clusters,
    config = config
  )

  # Write ----
  if (!is.null(outdir)) {
    rt_save(
      out,
      outdir = outdir,
      file_prefix = paste0("cluster_", algorithm),
      verbosity = verbosity
    )
  }
  out
} # /rtemis::cluster
