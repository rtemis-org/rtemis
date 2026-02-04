# cluster_DBSCAN.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Density-based spatial clustering of applications with noise (DBSCAN)
#'
#' @keywords internal
#' @noRd
cluster_DBSCAN <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, DBSCANConfig)

  # Dependencies ----
  check_dependencies("dbscan")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  if (verbosity > 0L) {
    msg("Clustering with", config@algorithm, "...")
  }
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
} # /rtemis::cluster_DBSCAN


clustpredict_DBSCAN <- function(clust, dat_train = NULL, newdata = NULL) {
  check_inherits(clust, "dbscan")
  if (is.null(newdata)) {
    return(clust[["cluster"]])
  } else {
    predict(clust, newdata = newdata, data = dat_train)
  }
} # /rtemis::clustpredict_DBSCAN
