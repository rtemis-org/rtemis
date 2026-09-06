# cluster_HOPACH.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://bioconductor.org/packages/hopach/
# https://rdrr.io/bioc/hopach/man/hopach.html

# %% cluster_.HOPACHConfig ----
#' Hierarchical Ordered Partitioning and Collapsing Hybrid (HOPACH)
#'
#' @keywords internal
#' @noRd
method(cluster_, HOPACHConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, HOPACHConfig)

  # Dependencies ----
  check_dependencies("hopach")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  clust <- hopach::hopach(
    data = x,
    d = config[["dist"]],
    clusters = config[["level_selection"]],
    K = config[["max_levels"]],
    kmax = config[["max_children"]],
    khigh = config[["max_children_mss"]],
    coll = config[["collapse"]],
    newmed = config[["new_medoid"]],
    mss = config[["mss"]],
    impr = config[["min_improvement"]],
    initord = config[["initial_order"]],
    ord = config[["element_order"]],
    verbose = verbosity > 1L
  )
  # `hopach()` returns a bare list, so there is no class to check. Assert the
  # structure `clustpredict_HOPACH()` reads instead, so a backend that changes
  # its return shape fails here rather than one function later.
  if (!is.list(clust) || is.null(clust[["clustering"]][["labels"]])) {
    rtemis.core::abort(
      "hopach::hopach() did not return a clustering.",
      class = c("rtemis_type_error", "rtemis_error")
    )
  }
  clust
} # /rtemis::cluster_.HOPACHConfig


# %% clustpredict_HOPACH ----
#' clustpredict method for HOPACH
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_HOPACH <- function(clust, newdata = NULL) {
  # HOPACH fits a tree over the training elements and exposes no way to place
  # an unseen case in it, so new data is refused rather than silently ignored.
  if (!is.null(newdata)) {
    rtemis.core::abort(
      "HOPACH cannot assign new data to fitted clusters.",
      class = c("rtemis_unsupported_error", "rtemis_error")
    )
  }
  labels <- clust[["clustering"]][["labels"]]
  # HOPACH labels encode a path down the tree (11, 21, 211, ...) and arrive as
  # numeric or integer depending on the depth reached, so they are neither 1:k
  # nor of the type `Clustering@clusters` declares. `unique()` rather than
  # `sort(unique())`: HOPACH orders its clusters, and label order is that
  # ordering, which sorting would discard.
  as.integer(match(labels, unique(labels)))
} # /rtemis::clustpredict_HOPACH
