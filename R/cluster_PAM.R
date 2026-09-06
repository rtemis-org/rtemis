# cluster_PAM.R
# ::rtemis::
# 2026- EDG rtemis.org

# The PAM family: PAM itself and PAMK, which searches `k` by fitting PAM (or
# CLARA) for each candidate. Both fits are `partition` objects, so they share
# one label accessor.

# References ----
# https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/pam.html
# https://rdrr.io/cran/fpc/man/pamk.html

# %% partition_clusters ----
#' Cluster assignment of a `cluster` package partition
#'
#' @param clust `partition` object, from `cluster::pam()` or `cluster::clara()`.
#'
#' @return Integer vector: cluster assignment, one value per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
partition_clusters <- function(clust) {
  check_inherits(clust, "partition")
  # Already 1:k, so only the type is not guaranteed.
  as.integer(clust[["clustering"]])
} # /rtemis::partition_clusters


# %% cluster_.PAMConfig ----
#' Partitioning Around Medoids (PAM)
#'
#' @keywords internal
#' @noRd
method(cluster_, PAMConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, PAMConfig)

  # Dependencies ----
  check_dependencies("cluster")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  # `nstart` decides how the initial medoids are found: the backend reads
  # `medoids = if (is.numeric(nstart)) "random"`, so leaving it unset is the
  # deterministic build phase and any number draws that many random starts.
  nstart <- config[["nstart"]]
  clust <- cluster::pam(
    x = x,
    k = config[["k"]],
    metric = config[["dist"]],
    stand = config[["stand"]],
    do.swap = config[["do_swap"]],
    variant = config[["variant"]],
    nstart = if (is.null(nstart)) NA else nstart
  )
  check_inherits(clust, "pam")
  clust
} # /rtemis::cluster_.PAMConfig


# %% clustpredict_PAM ----
#' clustpredict method for PAM
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_PAM <- function(clust, newdata = NULL) {
  # `cluster` exposes no predict method for a `pam` fit.
  if (!is.null(newdata)) {
    rtemis.core::abort(
      "PAM cannot assign new data to fitted clusters.",
      class = c("rtemis_unsupported_error", "rtemis_error")
    )
  }
  partition_clusters(clust)
} # /rtemis::clustpredict_PAM


# %% cluster_.PAMKConfig ----
#' Partitioning Around Medoids with estimation of the number of clusters (PAMK)
#'
#' @keywords internal
#' @noRd
method(cluster_, PAMKConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, PAMKConfig)

  # Dependencies ----
  check_dependencies("fpc")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  args <- list(
    data = x,
    krange = config[["krange"]],
    criterion = config[["criterion"]],
    usepam = config[["use_pam"]],
    scaling = config[["scaling"]],
    alpha = config[["alpha"]],
    critout = verbosity > 1L
  )
  # `ns` has a non-NULL backend default, so it is passed only when set rather
  # than forwarded as NULL, which `pamk()` would take as the value.
  if (!is.null(config[["n_subsets"]])) {
    args[["ns"]] <- config[["n_subsets"]]
  }
  clust <- do.call(fpc::pamk, args)
  # `pamk()` returns a bare list, so there is no class to check on the result
  # itself. The fit it holds is a `pam` when `use_pam` is TRUE and a `clara`
  # otherwise, so the shared parent class is what applies.
  if (!is.list(clust) || is.null(clust[["pamobject"]])) {
    rtemis.core::abort(
      "fpc::pamk() did not return a clustering.",
      class = c("rtemis_type_error", "rtemis_error")
    )
  }
  check_inherits(clust[["pamobject"]], "partition")
  clust
} # /rtemis::cluster_.PAMKConfig


# %% cluster_k.PAMKConfig ----
# `pamk()` reports the number of clusters its criterion selected. Taken from
# there rather than from the labels, so the two cannot disagree.
#
#' @keywords internal
#' @noRd
method(cluster_k, PAMKConfig) <- function(config, clust) {
  as.integer(clust[["nc"]])
} # /rtemis::cluster_k.PAMKConfig


# %% clustpredict_PAMK ----
#' clustpredict method for PAMK
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_PAMK <- function(clust, newdata = NULL) {
  # Neither `pam` nor `clara` exposes a predict method.
  if (!is.null(newdata)) {
    rtemis.core::abort(
      "PAMK cannot assign new data to fitted clusters.",
      class = c("rtemis_unsupported_error", "rtemis_error")
    )
  }
  partition_clusters(clust[["pamobject"]])
} # /rtemis::clustpredict_PAMK
