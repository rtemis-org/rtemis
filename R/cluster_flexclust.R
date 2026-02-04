# cluster_KMeans.R
# ::rtemis::
# 2025 EDG rtemis.org

#' K-means Clustering
#'
#' @keywords internal
#' @noRd
cluster_KMeans <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, KMeansConfig)

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
    method = "kmeans"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_KMeans


#' Hard Competitive Learning Clustering
#'
#' @keywords internal
#' @noRd
cluster_HardCL <- function(x, config, verbosity = 1L) {
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
} # /rtemis::cluster_HardCL


#' Neural Gas Clustering
#'
#' @keywords internal
#' @noRd
cluster_NeuralGas <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, NeuralGasConfig)

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
} # /rtemis::cluster_NeuralGas


#' clustpredict methods for KMeans, HardCL, NeuralGas
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_KMeans <- clustpredict_HardCL <- clustpredict_NeuralGas <- function(
  clust,
  newdata = NULL
) {
  check_inherits(clust, "kcca")
  flexclust::clusters(clust, newdata = newdata)
} # /rtemis::clustpredict_{KMeans,HardCL,NeuralGas}
