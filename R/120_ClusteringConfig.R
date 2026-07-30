# 120_ClusteringConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# Architecture ----
# Mirrors 070_Hyperparameters.R / 150_DecompositionConfig.R: each `*Config`
# subclass declares its algorithm parameters with the `prop_*` factories,
# from which the S7 validators, the `config` list, and the JSON Schema
# (S7_to_JSONSchema) are generated. The abstract `ClusteringConfig`
# superclass provides the computed `config` list. Clustering has no tuning,
# so every parameter is a fixed scalar. Parameters that are not cleanly
# JSON-expressible (CMeans `control`, a list; the scalar-or-vector `weights`)
# are plain properties: stored and validated by class, but excluded from
# schemas.

# %% ClusteringConfig ----
#' ClusteringConfig
#'
#' @description
#' Abstract superclass for clustering configs. Subclasses declare each
#' algorithm parameter as a property; this class contributes the computed
#' `config` list.
#'
#' @field algorithm Character: Algorithm name (computed constant, overridden
#'   per subclass).
#' @field config List: Algorithm-specific parameters (computed from the
#'   subclass's properties; assignment routes back and validates).
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClusteringConfig <- new_class(
  name = "ClusteringConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    algorithm = class_character,
    config = new_property(
      class_list,
      getter = function(self) {
        own_prop_values(self, ClusteringConfig)
      },
      setter = function(self, value) {
        route_config_assignment(self, ClusteringConfig, value)
      }
    )
  )
) # /rtemis::ClusteringConfig


# %% serializable_props.ClusteringConfig ----
# Serialize as {algorithm, config} (the public shape); the per-algorithm
# properties are redundant with the computed `config`.
method(serializable_props, ClusteringConfig) <- function(x) {
  list(
    algorithm = x@algorithm,
    config = config_prop_values(x, ClusteringConfig)
  )
} # /rtemis::serializable_props.ClusteringConfig


# %% `$`.ClusteringConfig ----
# Make ClusteringConfig@config `$`-accessible
method(`$`, ClusteringConfig) <- function(x, name) {
  x@config[[name]]
}


# %% `.DollarNames`.ClusteringConfig ----
# `$`-autocomplete ClusteringConfig@config ----
method(`.DollarNames`, ClusteringConfig) <- function(x, pattern = "") {
  all_names <- names(x@config)
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.ClusteringConfig ----
# Make ClusteringConfig@config `[[`-accessible
method(`[[`, ClusteringConfig) <- function(x, index) {
  x@config[[index]]
}


# %% repr.ClusteringConfig ----
method(repr, ClusteringConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  out <- repr_S7name(
    paste(x@algorithm, "ClusteringConfig"),
    pad = pad,
    output_type = output_type
  )
  paste0(
    out,
    repr_ls(x@config, pad = pad, output_type = output_type)
  )
} # /rtemis::repr.ClusteringConfig


# %% print.ClusteringConfig ----
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
} # /rtemis::print.ClusteringConfig


# %% KMeansConfig ----
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
  properties = list(
    algorithm = prop_algorithm("KMeans"),
    k = prop_integer(3L, min = 1L, description = "Number of clusters."),
    dist = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance measure."
    )
  )
) # /rtemis::KMeansConfig


# %% setup_KMeans ----
#' Setup KMeansConfig
#'
#' @param k Integer [1, Inf): Number of clusters.
#' @param dist Character \{"euclidean", "manhattan"\}: Distance measure to use.
#'
#' @return KMeansConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' kmeans_config <- setup_KMeans(k = 4L, dist = "euclidean")
#' kmeans_config
setup_KMeans <- function(k = 3L, dist = "euclidean") {
  k <- clean_posint(k)
  KMeansConfig(k = k, dist = dist)
} # /rtemis::setup_KMeans


# %% HardCLConfig ----
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
  properties = list(
    algorithm = prop_algorithm("HardCL"),
    k = prop_integer(3L, min = 1L, description = "Number of clusters."),
    dist = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance measure."
    )
  )
) # /rtemis::HardCLConfig


# %% setup_HardCL ----
#' Setup HardCLConfig
#'
#' @param k Integer [1, Inf): Number of clusters.
#' @param dist Character \{"euclidean", "manhattan"\}: Distance measure to use.
#'
#' @return HardCLConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' hardcl_config <- setup_HardCL(k = 4L, dist = "euclidean")
#' hardcl_config
setup_HardCL <- function(k = 3L, dist = "euclidean") {
  k <- clean_posint(k)
  HardCLConfig(k = k, dist = dist)
} # /rtemis::setup_HardCL


# %% NeuralGasConfig ----
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
  properties = list(
    algorithm = prop_algorithm("NeuralGas"),
    k = prop_integer(3L, min = 1L, description = "Number of clusters."),
    dist = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance measure."
    )
  )
) # /rtemis::NeuralGasConfig


# %% setup_NeuralGas ----
#' Setup NeuralGasConfig
#'
#' @param k Integer [1, Inf): Number of clusters.
#' @param dist Character \{"euclidean", "manhattan"\}: Distance measure to use.
#'
#' @return NeuralGasConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' neuralgas_config <- setup_NeuralGas(k = 4L, dist = "euclidean")
#' neuralgas_config
setup_NeuralGas <- function(k = 3L, dist = "euclidean") {
  k <- clean_posint(k)
  NeuralGasConfig(k = k, dist = dist)
} # /rtemis::setup_NeuralGas


# %% CMeansConfig ----
#' @title CMeansConfig
#'
#' @description
#' ClusteringConfig subclass for CMeans Clustering. `weights` (scalar or
#' per-case vector) and `control` (a list) are plain properties, excluded
#' from the generated schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
CMeansConfig <- new_class(
  name = "CMeansConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("CMeans"),
    k = prop_integer(2L, min = 1L, description = "Number of clusters."),
    max_iter = prop_integer(
      100L,
      min = 1L,
      description = "Maximum number of iterations."
    ),
    dist = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance measure."
    ),
    method = prop_string(
      "cmeans",
      enum = c("cmeans", "ufcl"),
      description = "\"cmeans\" = fuzzy c-means; \"ufcl\" = on-line update."
    ),
    m = prop_float(
      2.0,
      exclusive_min = 1,
      description = "Degree of fuzzification."
    ),
    rate_par = prop_float(
      NULL,
      min = 0,
      max = 1,
      nullable = TRUE,
      description = "Learning rate for the online (ufcl) variant."
    ),
    weights = prop_float(
      1,
      vector = TRUE,
      broadcast = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Case weights: either a scalar, applied to every case, or a vector."
    ),
    control = prop_bag(
      description = "Control parameters passed to the clustering backend."
    )
  )
) # /rtemis::CMeansConfig


# %% setup_CMeans ----
#' Setup CMeansConfig
#'
#' @param k Integer [1, Inf): Number of clusters.
#' @param max_iter Integer [1, Inf): Maximum number of iterations.
#' @param dist Character \{"euclidean", "manhattan"\}: Distance measure to use.
#' @param method Character \{"cmeans", "ufcl"\}: "cmeans" - fuzzy c-means clustering; "ufcl": on-line update.
#' @param m Numeric (1, Inf): Degree of fuzzification.
#' @param rate_par Optional Numeric \[0, 1\]: Learning rate for the online variant.
#' @param weights Numeric vector: Case weights. Either a scalar, applied to every case, or a vector with one value per case.
#' @param control List: Control config for clustering algorithm.
#'
#' @return CMeansConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' cmeans_config <- setup_CMeans(k = 4L, dist = "euclidean")
#' cmeans_config
setup_CMeans <- function(
  k = 2L,
  max_iter = 100L,
  dist = "euclidean",
  method = "cmeans",
  m = 2.0,
  rate_par = NULL,
  weights = 1.0,
  control = list()
) {
  k <- clean_posint(k)
  max_iter <- clean_posint(max_iter)
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


# %% DBSCANConfig ----
#' @title DBSCANConfig
#'
#' @description
#' ClusteringConfig subclass for DBSCAN Clustering. `weights` (scalar or
#' per-case vector) is a plain property, excluded from the generated schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DBSCANConfig <- new_class(
  name = "DBSCANConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("DBSCAN"),
    eps = prop_float(
      0.5,
      exclusive_min = 0,
      description = "Radius of neighborhood."
    ),
    min_points = prop_integer(
      5L,
      min = 1L,
      description = "Minimum number of points in a neighborhood to form a cluster."
    ),
    weights = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Weights for data points. NULL = unweighted."
    ),
    border_points = prop_boolean(
      TRUE,
      description = "Assign border points to clusters."
    ),
    search = prop_string(
      "kdtree",
      enum = c("kdtree", "linear", "dist"),
      description = "Nearest neighbor search strategy."
    ),
    bucket_size = prop_integer(
      100L,
      min = 1L,
      description = "Size of buckets for the k-d tree search."
    ),
    split_rule = prop_string(
      "SUGGEST",
      enum = c("SUGGEST", "STD", "MIDPT", "FAIR", "SL_MIDPT", "SL_FAIR"),
      description = "Rule for splitting the k-d tree."
    ),
    approx = prop_boolean(
      FALSE,
      description = "Use approximate nearest neighbor search."
    )
  )
) # /rtemis::DBSCANConfig


# %% setup_DBSCAN ----
#' Setup DBSCANConfig
#'
#' @param eps Numeric (0, Inf): Radius of neighborhood.
#' @param min_points Integer [1, Inf): Minimum number of points in a neighborhood to form a cluster.
#' @param weights Optional Numeric vector: Weights for data points.
#' @param border_points Logical: If TRUE, assign border points to clusters.
#' @param search Character \{"kdtree", "linear", "dist"\}: Nearest neighbor search strategy.
#' @param bucket_size Integer [1, Inf): Size of buckets for k-d tree search.
#' @param split_rule Character \{"SUGGEST", "STD", "MIDPT", "FAIR", "SL_MIDPT", "SL_FAIR"\}: Rule for splitting the k-d tree.
#' @param approx Logical: If TRUE, use approximate nearest neighbor search.
#'
#' @return DBSCANConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' dbscan_config <- setup_DBSCAN(eps = 0.5, min_points = 5L)
#' dbscan_config
setup_DBSCAN <- function(
  eps = 0.5,
  min_points = 5L,
  weights = NULL,
  border_points = TRUE,
  search = "kdtree",
  bucket_size = 100L,
  split_rule = "SUGGEST",
  approx = FALSE
) {
  min_points <- clean_posint(min_points)
  bucket_size <- clean_posint(bucket_size)
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


# %% .list_to_ClusteringConfig ----
#' Convert a list to a ClusteringConfig object
#'
#' Internal function used to reconstruct a `ClusteringConfig` object from a named
#' list, such as the result of parsing a JSON config conforming to the
#' schema.rtemis.org clustering schema. The list must carry an `algorithm`
#' element; the remaining elements (flat, or nested under `config`) are passed to
#' that algorithm's `setup_*` function.
#'
#' @param x Named list with an `algorithm` element plus algorithm-specific
#'   parameters, e.g. `list(algorithm = "DBSCAN", config = list(eps = 0.5))`.
#'
#' @return A `ClusteringConfig` object (an algorithm-specific subclass).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_ClusteringConfig <- function(x) {
  algorithm <- x[["algorithm"]]
  if (is.null(algorithm)) {
    rtemis.core::abort(
      "`algorithm` is required to build a ClusteringConfig.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  algorithm <- get_clust_name(algorithm)
  # One shape: `{algorithm, config}`, which is what the published schema
  # declares -- a flat `{algorithm, k, ...}` is rejected by it, so accepting one
  # here would take input the contract does not. `.drop_meta_keys()` removes
  # document metadata (e.g. `$schema`), which is not a setup arg.
  check_wire_keys(x, c("algorithm", "config"), "clustering config")
  params <- .drop_meta_keys(x[["config"]])
  setup_fn <- get_clust_setup_fn(algorithm)
  check_wire_keys(
    params,
    names(formals(setup_fn)),
    paste(algorithm, "clustering")
  )
  do.call(setup_fn, params)
} # /rtemis::.list_to_ClusteringConfig
