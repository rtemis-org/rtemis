# cluster_DBSCAN.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster_.DBSCANConfig ----
#' Density-based spatial clustering of applications with noise (DBSCAN)
#'
#' @keywords internal
#' @noRd
method(cluster_, DBSCANConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, DBSCANConfig)

  # Dependencies ----
  check_dependencies("dbscan")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  clust <- dbscan::dbscan(
    x = x,
    eps = config[["eps"]],
    minPts = config[["min_points"]],
    weights = config[["weights"]],
    borderPoints = config[["border_points"]],
    search = config[["search"]],
    bucketSize = config[["bucket_size"]],
    splitRule = config[["split_rule"]],
    approx = config[["approx"]]
  )
  check_inherits(clust, "dbscan")
  clust
} # /rtemis::cluster_.DBSCANConfig


# %% cluster_k.DBSCANConfig ----
# DBSCAN's non-noise labels do enumerate its fitted clusters -- it cannot
# produce a cluster that won no case -- so counting them is correct here, and
# only here. Label 0 is the noise sentinel and is not a cluster: without the
# `setdiff()` a fit that found three clusters and left some cases unassigned
# reports four, and a fit that found none at all reports one.
#
#' @keywords internal
#' @noRd
method(cluster_k, DBSCANConfig) <- function(config, clust) {
  length(setdiff(unique(clust[["cluster"]]), 0L))
} # /rtemis::cluster_k.DBSCANConfig


# %% clustpredict_DBSCAN ----
clustpredict_DBSCAN <- function(clust, dat_train = NULL, newdata = NULL) {
  check_inherits(clust, "dbscan")
  if (is.null(newdata)) {
    return(clust[["cluster"]])
  } else {
    predict(clust, newdata = newdata, data = dat_train)
  }
} # /rtemis::clustpredict_DBSCAN
