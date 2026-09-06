# 140_Clustering.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% Clustering ----
#' Clustering
#'
#' @description
#' Abstract base for a clustering result. Every result is one of its two
#' variants: `HardClustering`, which assigns each case to one cluster, or
#' `SoftClustering`, which also carries a membership matrix. Membership-or-not
#' partitions the space -- exhaustively and disjointly -- so it is the variant
#' axis rather than a nullable property.
#'
#' @field algorithm Character: Algorithm name.
#' @field clust Any: Clustering object.
#' @field k Integer: Number of clusters fitted, excluding a noise label and
#' including any cluster that won no case. Not in general the number of
#' distinct labels in `clusters`.
#' @field clusters List: Cluster assignment.
#' @field config ClusteringConfig: Algorithm-specific config.
#' @field cluster_config Optional ClusterConfig: The run's input.
#' @field data_fingerprint Optional DataFingerprint: Identity of the data the
#' clustering was fitted on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Clustering <- new_class(
  name = "Clustering",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    algorithm = class_character,
    clust = class_any,
    k = class_integer,
    clusters = class_integer | class_list,
    config = ClusteringConfig,
    # See `Decomposition@decompose_config`: the run's input, which `@config`
    # alone cannot supply. Assigned by `cluster()`.
    cluster_config = NULL | ClusterConfig,
    # See `Decomposition@data_fingerprint`. Assigned by `cluster()`.
    data_fingerprint = NULL | DataFingerprint
  )
) # /Clustering


# %% HardClustering ----
#' HardClustering
#'
#' @description
#' A clustering that assigns each case to one cluster and nothing more. Adds no
#' properties; the variant exists so that the distinction is carried by the type
#' rather than by an `is.null()` test, and so methods that only make sense for
#' one kind can dispatch.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HardClustering <- new_class(
  name = "HardClustering",
  package = "rtemis",
  parent = Clustering
) # /HardClustering


# %% SoftClustering ----
#' SoftClustering
#'
#' @description
#' A clustering that also reports, for each case, a weight per cluster: a fuzzy
#' membership or a posterior probability. `@membership` is required -- the
#' variant exists because the matrix does.
#'
#' Column j corresponds to cluster label j in `@clusters`. That correspondence
#' is established by the extractor and only partly checkable here: the validator
#' verifies that each case's assigned column attains its row's maximum, which
#' catches a permutation that moves a winning column but not one that permutes
#' columns no case won.
#'
#' @field membership Matrix: One row per case, one column per cluster.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SoftClustering <- new_class(
  name = "SoftClustering",
  package = "rtemis",
  parent = Clustering,
  properties = list(membership = class_matrix),
  validator = function(self) {
    m <- self@membership
    if (!is.numeric(m)) {
      return("@membership must be numeric.")
    }
    if (anyNA(m) || !all(is.finite(m))) {
      return("@membership must be finite and contain no NA.")
    }
    if (any(m < 0) || any(m > 1)) {
      return("@membership values must be in [0, 1].")
    }
    if (!is.integer(self@clusters)) {
      return("@clusters must be an integer vector for a soft clustering.")
    }
    if (nrow(m) != length(self@clusters)) {
      return(paste0(
        "@membership must have one row per case: got ",
        nrow(m),
        " rows for ",
        length(self@clusters),
        " cases."
      ))
    }
    if (ncol(m) != self@k) {
      return(paste0(
        "@membership must have one column per cluster: got ",
        ncol(m),
        " columns for k = ",
        self@k,
        "."
      ))
    }
    row_sums <- rowSums(m)
    if (any(row_sums == 0)) {
      return("@membership must not contain all-zero rows.")
    }
    # An engineering tolerance, not a theorem: both current backends return
    # exact sums, but one accumulating in log space would not.
    if (any(abs(row_sums - 1) > sqrt(.Machine$double.eps))) {
      return("@membership rows must sum to 1.")
    }
    # Assignment consistency. Rows with no valid column are unassigned (a noise
    # sentinel) and are excluded rather than indexed, which would silently drop
    # or NA them. `ties.method = "first"`: the default is "random", which would
    # make the check non-deterministic.
    assigned <- self@clusters
    scored <- which(assigned >= 1L & assigned <= ncol(m))
    if (length(scored) > 0L) {
      got <- m[cbind(scored, assigned[scored])]
      winner <- max.col(m[scored, , drop = FALSE], ties.method = "first")
      best <- m[cbind(scored, winner)]
      if (!all(got == best)) {
        return(paste0(
          "@clusters must assign each case to a cluster attaining its maximum ",
          "membership; ",
          sum(got != best),
          " case(s) do not. This usually means @membership's columns are not ",
          "in cluster-label order."
        ))
      }
    }
    NULL
  }
) # /SoftClustering


# %% `$`.Clustering ----
# Make Clustering props `$`-accessible
method(`$`, Clustering) <- function(x, name) {
  prop(x, name)
}


# %% `.DollarNames`.Clustering ----
# `$`-autocomplete Clustering props
method(`.DollarNames`, Clustering) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}


# %% `[[`.Clustering ----
# Make Clustering props `[[`-accessible
method(`[[`, Clustering) <- function(x, index) {
  prop(x, index)
}


# %% repr.Clustering ----
method(repr, Clustering) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  paste0(
    # The variant's own name, so "KMeans HardClustering" / "GMM SoftClustering"
    # says which kind of result this is.
    repr_S7name(paste(x@algorithm, S7_class(x)@name)),
    repr_ls(props(x)[-1], pad = pad, output_type = output_type)
  )
} # /rtemis::repr.Clustering


# %% print.Clustering ----
method(print, Clustering) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Clustering
