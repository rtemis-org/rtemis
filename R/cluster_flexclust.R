# cluster_KMeans.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster_.KMeansConfig ----
#' K-means Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, KMeansConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  clust <- flexclust::cclust(
    x = x,
    k = config[["k"]],
    dist = config[["dist"]],
    method = "kmeans"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_.KMeansConfig


# %% cluster_.HardCLConfig ----
#' Hard Competitive Learning Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, HardCLConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(config, HardCLConfig)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  if (verbosity > 0L) {
    msg("Clustering with", config@algorithm, "...")
  }
  clust <- flexclust::cclust(
    x = x,
    k = config[["k"]],
    dist = config[["dist"]],
    method = "hardcl"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_.HardCLConfig


# %% cluster_.NeuralGasConfig ----
#' Neural Gas Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, NeuralGasConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  if (verbosity > 0L) {
    msg("Clustering with", config@algorithm, "...")
  }
  clust <- flexclust::cclust(
    x = x,
    k = config[["k"]],
    dist = config[["dist"]],
    method = "neuralgas"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_.NeuralGasConfig


# %% clustpredict_{KMeans,HardCL,NeuralGas} ----
#' clustpredict methods for KMeans, HardCL, NeuralGas
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_KMeans <- clustpredict_HardCL <- clustpredict_NeuralGas <- function(
  clust
) {
  check_inherits(clust, "kcca")
  as.integer(flexclust::clusters(clust))
} # /rtemis::clustpredict_{KMeans,HardCL,NeuralGas}


# %% assign_kcca ----
#' Assign new cases to the clusters of a flexclust fit
#'
#' `flexclust::clusters()` assigns each new case to its closest centroid under
#' the distance the fit used, which is the assignment rule of KMeans, HardCL,
#' and NeuralGas.
#'
#' @param config `ClusteringConfig` of the fit.
#' @param clust `kcca` object.
#' @param newdata data.frame: New cases, holding the fitted features.
#'
#' @return Integer vector: One cluster label per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
assign_kcca <- function(config, clust, newdata) {
  check_inherits(clust, "kcca")
  as.integer(flexclust::clusters(clust, newdata = as.matrix(newdata)))
} # /rtemis::assign_kcca

# %% assign_clusters_.{KMeans,HardCL,NeuralGas}Config ----
method(assign_clusters_, KMeansConfig) <- assign_kcca
method(assign_clusters_, HardCLConfig) <- assign_kcca
method(assign_clusters_, NeuralGasConfig) <- assign_kcca
