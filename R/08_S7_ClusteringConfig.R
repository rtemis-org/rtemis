# S7_ClusteringConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# ClusteringConfig ----
#' @title ClusteringConfig
#'
#' @description
#' Clustering config class.
#'
#' @field algorithm Character: Algorithm name.
#' @field config List: Algorithm-specific config.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClusteringConfig <- new_class(
  name = "ClusteringConfig",
  properties = list(
    algorithm = class_character,
    config = class_list
  )
) # /ClusteringConfig

# Make ClusteringConfig@config `$`-accessible
method(`$`, ClusteringConfig) <- function(x, name) {
  x@config[[name]]
}


# `$`-autocomplete ClusteringConfig@config ----
method(`.DollarNames`, ClusteringConfig) <- function(x, pattern = "") {
  all_names <- names(x@config)
  grep(pattern, all_names, value = TRUE)
}

# Make ClusteringConfig@config `[[`-accessible
method(`[[`, ClusteringConfig) <- function(x, index) {
  x@config[[index]]
}


# Show ClusteringConfig ----
method(repr, ClusteringConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  output_type <- get_output_type(output_type)
  out <- repr_S7name(
    paste(x@algorithm, "ClusteringConfig"),
    pad = pad,
    output_type = output_type
  )
  paste0(
    out,
    repr_ls(props(x)[["config"]], pad = pad, output_type = output_type)
  )
} # /show


# Print ClusteringConfig ----
#' Print Method for ClusteringConfig
#'
#' @param x ClusteringConfig object.
#' @param pad Integer: Left side padding.
#'
#' @return ClusteringConfig object, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(print, ClusteringConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /print.ClusteringConfig


# KMeansConfig ----
#' @title KMeansConfig
#'
#' @description
#' ClusteringConfig subclass for K-means Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
KMeansConfig <- new_class(
  name = "KMeansConfig",
  parent = ClusteringConfig,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringConfig(
        algorithm = "KMeans",
        config = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /KMeansConfig

#' Setup KMeansConfig
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return KMeansConfig object.
#'
#' @author EDG
#' @export
setup_KMeans <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  KMeansConfig(k, dist)
} # /rtemis::setup_KMeans


# HardCLConfig ----
#' @title HardCLConfig
#'
#' @description
#' ClusteringConfig subclass for HardCL Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HardCLConfig <- new_class(
  name = "HardCLConfig",
  parent = ClusteringConfig,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringConfig(
        algorithm = "HardCL",
        config = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /HardCLConfig

#' Setup HardCLConfig
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return HardCLConfig object.
#'
#' @author EDG
#' @export
setup_HardCL <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  HardCLConfig(k, dist)
} # /rtemis::setup_HardCL


# NeuralGasConfig ----
#' @title NeuralGasConfig
#'
#' @description
#' ClusteringConfig subclass for Neural Gas Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
NeuralGasConfig <- new_class(
  name = "NeuralGasConfig",
  parent = ClusteringConfig,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringConfig(
        algorithm = "NeuralGas",
        config = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /NeuralGasConfig

#' Setup NeuralGasConfig
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return NeuralGasConfig object.
#'
#' @author EDG
#' @export
setup_NeuralGas <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  NeuralGasConfig(k, dist)
} # /rtemis::setup_NeuralGas


# CMeansConfig ----
#' @title CMeansConfig
#'
#' @description
#' ClusteringConfig subclass for CMeans Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
CMeansConfig <- new_class(
  name = "CMeansConfig",
  parent = ClusteringConfig,
  constructor = function(
    k,
    max_iter,
    dist,
    method,
    m,
    rate_par,
    weights,
    control
  ) {
    k <- clean_posint(k)
    max_iter <- clean_posint(max_iter)
    check_character(dist)
    check_character(method)
    check_floatpos(m)
    check_float01inc(rate_par)
    check_inherits(weights, "numeric")
    check_inherits(control, "list")
    new_object(
      ClusteringConfig(
        algorithm = "CMeans",
        config = list(
          k = k,
          max_iter = max_iter,
          dist = dist,
          method = method,
          m = m,
          rate_par = rate_par,
          weights = weights,
          control = control
        )
      )
    )
  }
) # /CMeansConfig

#' Setup CMeansConfig
#'
#' @param k Integer: Number of clusters.
#' @param max_iter Integer: Maximum number of iterations.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#' @param method Character: "cmeans" - fuzzy c-means clustering; "ufcl": on-line update.
#' @param m Float (>1): Degree of fuzzification.
#' @param rate_par Float (0, 1): Learning rate for the online variant.
#' @param weights Float (>0): Case weights.
#' @param control List: Control config for clustering algorithm.
#'
#' @return CMeansConfig object.
#'
#' @author EDG
#' @export
setup_CMeans <- function(
  k = 2L,
  max_iter = 100L,
  dist = c("euclidean", "manhattan"),
  method = c("cmeans", "ufcl"),
  m = 2.0,
  rate_par = NULL,
  weights = 1.0,
  control = list()
) {
  k <- clean_posint(k)
  max_iter <- clean_posint(max_iter)
  dist <- match.arg(dist)
  method <- match.arg(method)
  check_floatpos(m)
  stopifnot(m > 1)
  check_float01inc(rate_par)
  check_inherits(weights, "numeric")
  CMeansConfig(
    k = k,
    max_iter = max_iter,
    dist = dist,
    method = method,
    m = m,
    rate_par = rate_par,
    weights = weights,
    control = control
  )
} # /rtemis::setup_CMeans


# DBSCANConfig ----
#' @title DBSCANConfig
#'
#' @description
#' ClusteringConfig subclass for DBSCAN Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DBSCANConfig <- new_class(
  name = "DBSCANConfig",
  parent = ClusteringConfig,
  constructor = function(
    eps,
    min_points,
    weights,
    border_points,
    search,
    bucket_size,
    split_rule,
    approx
  ) {
    check_floatpos(eps)
    min_points <- clean_posint(min_points)
    check_inherits(weights, "numeric")
    check_inherits(border_points, "logical")
    check_inherits(search, "character")
    check_inherits(bucket_size, "integer")
    check_inherits(split_rule, "character")
    check_inherits(approx, "logical")
    new_object(
      ClusteringConfig(
        algorithm = "DBSCAN",
        config = list(
          eps = eps,
          min_points = min_points,
          weights = weights,
          border_points = border_points,
          search = search,
          bucket_size = bucket_size,
          split_rule = split_rule,
          approx = approx
        )
      )
    )
  }
) # /DBSCANConfig

#' Setup DBSCANConfig
#'
#' @param eps Float: Radius of neighborhood.
#' @param min_points Integer: Minimum number of points in a neighborhood to form a cluster.
#' @param weights Numeric vector: Weights for data points.
#' @param border_points Logical: If TRUE, assign border points to clusters.
#' @param search Character: Nearest neighbor search strategy: "kdtree", "linear", or "dist".
#' @param bucket_size Integer: Size of buckets for k-dtree search.
#' @param split_rule Character: Rule for splitting clusters: "SUGGEST", "STD", "MIDPT", "FAIR", "SL_MIDPT", "SL_FAIR".
#' @param approx Logical: If TRUE, use approximate nearest neighbor search.
#' @return DBSCANConfig object.
#'
#' @author EDG
#' @export
setup_DBSCAN <- function(
  eps = 0.5,
  min_points = 5L,
  weights = NULL,
  border_points = TRUE,
  search = c("kdtree", "linear", "dist"),
  bucket_size = 100L,
  split_rule = c("SUGGEST", "STD", "MIDPT", "FAIR", "SL_MIDPT", "SL_FAIR"),
  approx = FALSE
) {
  check_floatpos(eps)
  min_points <- clean_posint(min_points)
  check_inherits(weights, "numeric")
  check_inherits(border_points, "logical")
  search <- match.arg(search)
  check_inherits(bucket_size, "integer")
  split_rule <- match.arg(split_rule)
  check_inherits(approx, "logical")
  DBSCANConfig(
    eps = eps,
    min_points = min_points,
    weights = weights,
    border_points = border_points,
    search = search,
    bucket_size = bucket_size,
    split_rule = split_rule,
    approx = approx
  )
} # /rtemis::setup_DBSCAN
