# cluster_HOPACH.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://bioconductor.org/packages/hopach/
# https://rdrr.io/bioc/hopach/man/hopach.html

# %% hopach_overcollapse_msg ----
# `hopach()`'s collapsing step is allowed one merge too many, so it can reduce
# the tree to a single cluster. `hopach()` then either fails while trying to
# descend from that level, or, if the level search ends first, returns the
# one-cluster partition. Which of the two happens depends on the data and on
# `dist`, so both are reported with the same explanation.
hopach_overcollapse_msg <- paste0(
  "HOPACH collapsed its tree to a single cluster, which 'hopach' cannot ",
  "split further. Whether this happens depends on the data and on the ",
  "distance measure: rerun with another `dist`, e.g. \"euclid\" or \"cor\"."
)


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
  clust <- do_call(
    hopach::hopach,
    list(
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
    ),
    error_pattern_suggestion = list(
      "must be an array of at least two dimensions" = hopach_overcollapse_msg
    ),
    verbosity = verbosity
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
  # Every level `hopach()` can legitimately select holds at least two clusters:
  # the first level it builds has two or more and the level search only ever
  # replaces it with a collapsed level. `k < 2` therefore identifies the
  # over-collapsed level exactly, with no partition to confuse it with.
  if (as.integer(clust[["clustering"]][["k"]]) < 2L) {
    rtemis.core::abort(
      hopach_overcollapse_msg,
      class = c("rtemis_runtime_error", "rtemis_error")
    )
  }
  clust
} # /rtemis::cluster_.HOPACHConfig


# %% cluster_k.HOPACHConfig ----
# `hopach()` reports the size of the level it selected, which is the fitted
# count. Its labels are level-path codes remapped by `clustpredict_HOPACH()`,
# so counting them would be counting the remapping rather than the fit.
#
#' @keywords internal
#' @noRd
method(cluster_k, HOPACHConfig) <- function(config, clust) {
  as.integer(clust[["clustering"]][["k"]])
} # /rtemis::cluster_k.HOPACHConfig


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
