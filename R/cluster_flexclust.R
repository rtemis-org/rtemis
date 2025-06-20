# cluster_KMeans.R
# ::rtemis::
# 2025 EDG rtemis.org

#' K-means Clustering
#'
#' @keywords internal
#' @noRd
cluster_KMeans <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, KMeansParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters[["k"]],
    dist = parameters[["dist"]],
    method = "kmeans"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_KMeans


#' Hard Competitive Learning Clustering
#'
#' @keywords internal
#' @noRd
cluster_HardCL <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, HardCLParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters[["k"]],
    dist = parameters[["dist"]],
    method = "hardcl"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_HardCL


#' Neural Gas Clustering
#'
#' @keywords internal
#' @noRd
cluster_NeuralGas <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, NeuralGasParameters)

  # Dependencies ----
  check_dependencies("flexclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  clust <- flexclust::cclust(
    x = x,
    k = parameters[["k"]],
    dist = parameters[["dist"]],
    method = "neuralgas"
  )
  check_inherits(clust, "kcca")
  clust
} # /rtemis::cluster_NeuralGas


clustpredict_KMeans <- clustpredict_HardCL <- clustpredict_NeuralGas <- function(
  clust,
  newdata = NULL
) {
  check_inherits(clust, "kcca")
  flexclust::clusters(clust, newdata = newdata)
} # /rtemis::clustpredict_{KMeans,HardCL,NeuralGas}
