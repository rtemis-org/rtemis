# S7_ClusteringParameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# ClusteringParameters ----
#' @title ClusteringParameters
#'
#' @description
#' Clustering parameters class.
#'
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClusteringParameters <- new_class(
  name = "ClusteringParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /ClusteringParameters

# Make ClusteringParameters@parameters `$`-accessible
method(`$`, ClusteringParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete ClusteringParameters@parameters ----
method(`.DollarNames`, ClusteringParameters) <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Make ClusteringParameters@parameters `[[`-accessible
method(`[[`, ClusteringParameters) <- function(x, index) {
  x@parameters[[index]]
}

# Show ClusteringParameters ----
method(repr, ClusteringParameters) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  output_type <- get_output_type(output_type)
  out <- show_S7name(
    paste(x@algorithm, "ClusteringParameters"),
    pad = pad,
    output_type = output_type
  )
  paste0(
    out,
    show_ls(props(x)[["parameters"]], pad = pad, output_type = output_type)
  )
} # /show

# Print ClusteringParameters ----
#' Print Method for ClusteringParameters
#'
#' @param x ClusteringParameters object.
#' @param pad Integer: Left side padding.
#'
#' @return ClusteringParameters object, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(print, ClusteringParameters) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /print.ClusteringParameters

# KMeansParameters ----
#' @title KMeansParameters
#'
#' @description
#' ClusteringParameters subclass for K-means Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
KMeansParameters <- new_class(
  name = "KMeansParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "KMeans",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /KMeansParameters

#' Setup KMmeansParameters
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return KMeansParameters object.
#'
#' @author EDG
#' @export
setup_KMeans <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  KMeansParameters(k, dist)
} # /rtemis::setup_KMeans

# HardCLParameters ----
#' @title HardCLParameters
#'
#' @description
#' ClusteringParameters subclass for HardCL Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HardCLParameters <- new_class(
  name = "HardCLParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "HardCL",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /HardCLParameters

#' Setup HardCLParameters
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return HardCLParameters object.
#'
#' @author EDG
#' @export
setup_HardCL <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  HardCLParameters(k, dist)
} # /rtemis::setup_HardCL

# NeuralGasParameters ----
#' @title NeuralGasParameters
#'
#' @description
#' ClusteringParameters subclass for Neural Gas Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
NeuralGasParameters <- new_class(
  name = "NeuralGasParameters",
  parent = ClusteringParameters,
  constructor = function(k, dist) {
    k <- clean_posint(k)
    check_inherits(dist, "character")
    new_object(
      ClusteringParameters(
        algorithm = "NeuralGas",
        parameters = list(
          k = k,
          dist = dist
        )
      )
    )
  }
) # /NeuralGasParameters

#' Setup NeuralGasParameters
#'
#' @param k Number of clusters.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#'
#' @return NeuralGasParameters object.
#'
#' @author EDG
#' @export
setup_NeuralGas <- function(k = 3L, dist = c("euclidean", "manhattan")) {
  k <- clean_posint(k)
  dist <- match.arg(dist)
  NeuralGasParameters(k, dist)
} # /rtemis::setup_NeuralGas


# CMeansParameters ----
#' @title CMeansParameters
#'
#' @description
#' ClusteringParameters subclass for CMeans Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
CMeansParameters <- new_class(
  name = "CMeansParameters",
  parent = ClusteringParameters,
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
      ClusteringParameters(
        algorithm = "CMeans",
        parameters = list(
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
) # /CMeansParameters

#' Setup CMeansParameters
#'
#' @param k Integer: Number of clusters.
#' @param max_iter Integer: Maximum number of iterations.
#' @param dist Character: Distance measure to use: 'euclidean' or 'manhattan'.
#' @param method Character: "cmeans" - fuzzy c-means clustering; "ufcl": on-line update.
#' @param m Float (>1): Degree of fuzzification.
#' @param rate_par Float (0, 1): Learning rate for the online variant.
#' @param weights Float (>0): Case weights.
#' @param control List: Control parameters for clustering algorithm.
#'
#' @return CMeansParameters object.
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
  CMeansParameters(
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


# DBSCAN Parameters ----
#' @title DBSCANParameters
#'
#' @description
#' ClusteringParameters subclass for DBSCAN Clustering.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DBSCANParameters <- new_class(
  name = "DBSCANParameters",
  parent = ClusteringParameters,
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
      ClusteringParameters(
        algorithm = "DBSCAN",
        parameters = list(
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
) # /DBSCANParameters

#' Setup DBSCANParameters
#'
#' @param eps Float: Radius of neighborhood.
#' @param min_points Integer: Minimum number of points in a neighborhood to form a cluster.
#' @param weights Numeric vector: Weights for data points.
#' @param border_points Logical: If TRUE, assign border points to clusters.
#' @param search Character: Nearest neighbor search strategy: "kdtree", "linear", or "dist".
#' @param bucket_size Integer: Size of buckets for k-dtree search.
#' @param split_rule Character: Rule for splitting clusters: "SUGGEST", "STD", "MIDPT", "FAIR", "SL_MIDPT", "SL_FAIR".
#' @param approx Logical: If TRUE, use approximate nearest neighbor search.
#' @return DBSCANParameters object.
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
  DBSCANParameters(
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
