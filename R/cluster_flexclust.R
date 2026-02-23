# cluster_KMeans.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster_.KMeansConfig ----
#' K-means Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, KMeansConfig) <- function(config, x, verbosity = 1L) {
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
} # /rtemis::cluster_.KMeansConfig


# %% cluster_.HardCLConfig ----
#' Hard Competitive Learning Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, HardCLConfig) <- function(config, x, verbosity = 1L) {
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
method(cluster_, NeuralGasConfig) <- function(config, x, verbosity = 1L) {
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
  clust,
  newdata = NULL
) {
  check_inherits(clust, "kcca")
  flexclust::clusters(clust, newdata = newdata)
} # /rtemis::clustpredict_{KMeans,HardCL,NeuralGas}
