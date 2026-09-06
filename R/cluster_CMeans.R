# cluster_CMeans.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster_.CMeansConfig ----
#' C-means Clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, CMeansConfig) <- function(config, x, verbosity = 1L) {
  # Dependencies ----
  check_dependencies("e1071")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  clust <- e1071::cmeans(
    x = x,
    centers = config[["k"]],
    iter.max = config[["max_iter"]],
    verbose = verbosity > 0L,
    dist = config[["dist"]],
    method = config[["method"]],
    m = config[["m"]],
    rate.par = config[["rate_par"]],
    weights = config[["weights"]],
    control = config[["control"]]
  )
  check_inherits(clust, "fclust")
  clust
} # /rtemis::cluster_.CMeansConfig


# %% cluster_membership.CMeansConfig ----
# `cmeans()` returns memberships with column j holding cluster j's weight, and
# `$cluster` is the argmax of that matrix, so the backend's order is already
# rtemis' label order and no permutation is needed. `SoftClustering`'s validator
# re-checks that correspondence rather than trusting this comment.
#
#' @keywords internal
#' @noRd
method(cluster_membership, CMeansConfig) <- function(config, clust) {
  clust[["membership"]]
} # /rtemis::cluster_membership.CMeansConfig


# %% clustpredict_CMeans ----
clustpredict_CMeans <- function(clust) {
  check_inherits(clust, "fclust")
  clust[["cluster"]]
} # /rtemis::clustpredict_CMeans
