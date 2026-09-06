# cluster_GMM.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://mclust-org.github.io/mclust/
# https://cran.r-project.org/package=mclust

# %% mclust_Mclust ----
#' Call `mclust::Mclust()` with the package loaded but not attached
#'
#' `Mclust()` builds its own call and evaluates it in the caller's frame --
#' `mc[[1]] <- as.name("mclustBIC")` -- so the name it constructs resolves
#' through the search path rather than through the namespace. rtemis reaches
#' every backend by `::`, which loads without attaching, and the call then dies
#' with "could not find function 'mclustBIC'".
#'
#' Evaluating in an environment whose parent *is* the namespace puts those
#' constructed names in scope for the duration of the call and nothing else.
#' Attaching the package instead would mutate the user's search path for the
#' rest of the session, which is the side effect `Suggests`-gating exists to
#' avoid.
#'
#' Do not simplify this to `mclust::Mclust(...)`. It fails only when the package
#' is *not* attached, so a check run in a `library(mclust)` session passes and
#' `just test` does not.
#'
#' @param args Named list: Arguments for `mclust::Mclust()`.
#'
#' @return `Mclust` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mclust_Mclust <- function(args) {
  eval(
    quote(do.call(Mclust, .args)),
    envir = list2env(list(.args = args), parent = asNamespace("mclust"))
  )
} # /rtemis::mclust_Mclust


# %% cluster_.GMMConfig ----
#' Gaussian Mixture Model clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, GMMConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, GMMConfig)

  # Dependencies ----
  check_dependencies("mclust")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  # NULL means "choose by BIC" for both of these, which is the backend's own
  # default, so an unset value is passed through rather than pruned.
  clust <- mclust_Mclust(list(
    data = x,
    G = config[["k"]],
    modelNames = config[["model_names"]],
    verbose = verbosity > 1L
  ))
  check_inherits(clust, "Mclust")
  clust
} # /rtemis::cluster_.GMMConfig


# %% cluster_k.GMMConfig ----
# Reached only when the config leaves `k` unset, i.e. when BIC selected the
# number of components. `$G` is that selection; the labels would under-count it,
# since a fitted component need not win any case.
#
#' @keywords internal
#' @noRd
method(cluster_k, GMMConfig) <- function(config, clust) {
  as.integer(clust[["G"]])
} # /rtemis::cluster_k.GMMConfig


# %% cluster_membership.GMMConfig ----
# `$z` holds the posterior probability of each component for each case, with
# column j the j-th component -- the same order `$classification` indexes, which
# is what `SoftClustering`'s validator re-checks.
#
#' @keywords internal
#' @noRd
method(cluster_membership, GMMConfig) <- function(config, clust) {
  clust[["z"]]
} # /rtemis::cluster_membership.GMMConfig


# %% clustpredict_GMM ----
#' clustpredict method for GMM
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_GMM <- function(clust, newdata = NULL) {
  # `predict.Mclust()` exists, but reaching it needs the same namespace dance as
  # the fit and rtemis has no path that asks for it yet, so new data is refused
  # rather than half-supported.
  if (!is.null(newdata)) {
    rtemis.core::abort(
      "GMM cannot assign new data to fitted clusters.",
      class = c("rtemis_unsupported_error", "rtemis_error")
    )
  }
  # `$classification` is the argmax of `$z`, already 1:G, but stored as double.
  as.integer(clust[["classification"]])
} # /rtemis::clustpredict_GMM
