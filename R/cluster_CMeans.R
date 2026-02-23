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
  if (verbosity > 0L) {
    msg("Clustering with", config@algorithm, "...")
  }
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


# %% clustpredict_CMeans ----
clustpredict_CMeans <- function(clust) {
  check_inherits(clust, "fclust")
  clust[["cluster"]]
} # /rtemis::clustpredict_CMeans
