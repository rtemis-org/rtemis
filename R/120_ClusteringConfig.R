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
# `algorithm` plus the algorithm's settings as siblings -- the shape the
# clustering schema declares. The computed `config` list is not written: it is
# a view of the same properties.
method(serializable_props, ClusteringConfig) <- function(x) {
  dispatched_props(x, ClusteringConfig, "algorithm")
} # /rtemis::serializable_props.ClusteringConfig


# %% cluster_membership.ClusteringConfig ----
# The default: no membership matrix, so `cluster()` builds a `HardClustering`.
# An algorithm that fits one overrides this; the presence of an override is what
# the roster test asserts, since a missing method is silently indistinguishable
# from a genuine absence.
method(cluster_membership, ClusteringConfig) <- function(config, clust) {
  NULL
} # /rtemis::cluster_membership.ClusteringConfig


# %% cluster_k.ClusteringConfig ----
# Deliberately an abort, not a label count. Counting distinct labels is right
# only where a backend's non-noise labels enumerate its fitted clusters; it
# over-counts a noise sentinel and under-counts a cluster that won no case. An
# algorithm that discovers `k` states where its count comes from.
method(cluster_k, ClusteringConfig) <- function(config, clust) {
  rtemis.core::abort(
    "`",
    config@algorithm,
    "` does not prescribe `k` and registers no `cluster_k()` method, so the ",
    "number of clusters it found cannot be established.",
    class = c("rtemis_unsupported_error", "rtemis_error")
  )
} # /rtemis::cluster_k.ClusteringConfig


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
      description = "Weights for data points. Unset leaves the cases unweighted."
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


# %% HOPACHConfig ----
#' @title HOPACHConfig
#'
#' @description
#' ClusteringConfig subclass for HOPACH Clustering. HOPACH determines the
#' number of clusters itself, so this config declares no `k`: the properties
#' that bound the tree are `max_levels` and `max_children`, and the number of
#' clusters found is reported by the resulting `Clustering`.
#'
#' Property names are rtemis' own; `setup_HOPACH` documents which
#' `hopach::hopach()` argument each one sets.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HOPACHConfig <- new_class(
  name = "HOPACHConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("HOPACH"),
    # `abseuclid` is documented by `hopach` but has no branch in
    # `distancematrix()`, which stops with "Distance metric abseuclid not
    # available"; it is therefore not offered.
    dist = prop_string(
      "cosangle",
      enum = c("cosangle", "abscosangle", "euclid", "cor", "abscor"),
      description = "Distance measure."
    ),
    # `hopach`'s third option, "none", returns no clustering at all, which is
    # not a clustering run.
    level_selection = prop_string(
      "best",
      enum = c("best", "greedy"),
      description = paste0(
        "Which level of the tree becomes the returned partition: ",
        "\"best\" = the level minimizing MSS, \"greedy\" = the first level ",
        "below which MSS increases."
      )
    ),
    max_levels = prop_integer(
      15L,
      min = 1L,
      max = 15L,
      description = paste0(
        "Maximum number of levels in the tree. Capped at 15, above which the ",
        "level encoding overflows."
      )
    ),
    max_children = prop_integer(
      9L,
      min = 2L,
      max = 9L,
      description = "Maximum number of children at each node."
    ),
    max_children_mss = prop_integer(
      9L,
      min = 2L,
      max = 9L,
      description = "Maximum number of children at each node when computing MSS."
    ),
    collapse = prop_string(
      "seq",
      enum = c("seq", "all"),
      description = paste0(
        "How collapsing is performed at each level: \"seq\" = collapse pairs ",
        "sequentially from the closest while MSS decreases, \"all\" = collapse ",
        "any pair that decreases MSS."
      )
    ),
    new_medoid = prop_string(
      "medsil",
      enum = c("medsil", "center", "nn", "uwnn"),
      description = paste0(
        "How the medoid of a collapsed pair is chosen: \"medsil\" = maximizer ",
        "of medoid-based silhouette, \"center\" = minimizer of average distance ",
        "to the medoid, \"nn\" = size-weighted nearest neighbor of the two ",
        "medoids' mean, \"uwnn\" = unweighted nearest neighbor."
      )
    ),
    mss = prop_string(
      "med",
      enum = c("med", "mean"),
      description = paste0(
        "Split silhouette criterion: \"med\" = median split silhouette, ",
        "\"mean\" = mean split silhouette."
      )
    ),
    min_improvement = prop_float(
      0,
      min = 0,
      max = 1,
      description = "Relative improvement in MSS required to accept a collapse step."
    ),
    initial_order = prop_string(
      "co",
      enum = c("co", "clust"),
      description = paste0(
        "How clusters in the first level are ordered: \"co\" = maximize ",
        "correlation ordering, \"clust\" = order by a binary-split HOPACH of ",
        "the cluster medoids."
      )
    ),
    # `hopach`'s third option, "co", calls `correlationordering()`, which reads
    # `dist@Size` off an `hdist` object but is handed a plain matrix on this
    # path, so it fails for every input.
    element_order = prop_string(
      "own",
      enum = c("own", "neighbor"),
      description = paste0(
        "How elements are ordered within a cluster: \"own\" = by distance from ",
        "the cluster's own medoid, \"neighbor\" = by distance to the next ",
        "cluster's medoid."
      )
    )
  )
) # /rtemis::HOPACHConfig


# %% setup_HOPACH ----
#' Setup HOPACHConfig
#'
#' Setup a `HOPACHConfig` object for Hierarchical Ordered Partitioning and
#' Collapsing Hybrid (HOPACH) clustering, via the Bioconductor package
#' 'hopach'.
#'
#' HOPACH identifies the number of clusters itself, by building an ordered
#' hierarchical tree and selecting the level with maximally homogeneous
#' clusters under the Median (or Mean) Split Silhouette criterion. There is
#' therefore no `k` to set: `max_levels` and `max_children` bound the tree, and
#' the number of clusters found is reported by the resulting `Clustering`.
#'
#' Argument names are rtemis' own. They map to `hopach::hopach()` as:
#' `dist` -> `d`, `level_selection` -> `clusters`, `max_levels` -> `K`,
#' `max_children` -> `kmax`, `max_children_mss` -> `khigh`,
#' `collapse` -> `coll`, `new_medoid` -> `newmed`, `mss` -> `mss`,
#' `min_improvement` -> `impr`, `initial_order` -> `initord`,
#' `element_order` -> `ord`.
#'
#' Two values `hopach` documents are not offered, because neither works:
#' `d = "abseuclid"`, which `distancematrix()` has no branch for, and
#' `ord = "co"`, which errors for every input.
#'
#' On some datasets `hopach` collapses its tree all the way to a single
#' cluster and cannot split it further, which ends the run with an error.
#' Whether it happens depends on the data and on `dist`, so another `dist`
#' clusters the same data.
#'
#' @param dist Character \{"cosangle", "abscosangle", "euclid", "cor", "abscor"\}: Distance measure.
#' @param level_selection Character \{"best", "greedy"\}: Which level of the tree becomes the returned partition. "best": the level minimizing MSS; "greedy": the first level below which MSS increases.
#' @param max_levels Integer \[1, 15\]: Maximum number of levels in the tree.
#' @param max_children Integer \[2, 9\]: Maximum number of children at each node.
#' @param max_children_mss Integer \[2, 9\]: Maximum number of children at each node when computing MSS.
#' @param collapse Character \{"seq", "all"\}: How collapsing is performed at each level. "seq": collapse pairs sequentially from the closest while MSS decreases; "all": collapse any pair that decreases MSS.
#' @param new_medoid Character \{"medsil", "center", "nn", "uwnn"\}: How the medoid of a collapsed pair is chosen.
#' @param mss Character \{"med", "mean"\}: Split silhouette criterion; median or mean.
#' @param min_improvement Numeric \[0, 1\]: Relative improvement in MSS required to accept a collapse step.
#' @param initial_order Character \{"co", "clust"\}: How clusters in the first level are ordered.
#' @param element_order Character \{"own", "neighbor"\}: How elements are ordered within a cluster.
#'
#' @return `HOPACHConfig` object.
#'
#' @references
#' van der Laan MJ, Pollard KS (2003). A new algorithm for hybrid hierarchical
#' clustering with visualization and the bootstrap. \emph{Journal of
#' Statistical Planning and Inference}, 117(2), 275-303.
#' \doi{10.1016/S0378-3758(02)00388-9}
#'
#' @author EDG
#' @export
#' @examples
#' hopach_config <- setup_HOPACH(dist = "euclid", max_levels = 3L)
#' hopach_config
setup_HOPACH <- function(
  dist = "cosangle",
  level_selection = "best",
  max_levels = 15L,
  max_children = 9L,
  max_children_mss = 9L,
  collapse = "seq",
  new_medoid = "medsil",
  mss = "med",
  min_improvement = 0,
  initial_order = "co",
  element_order = "own"
) {
  max_levels <- clean_posint(max_levels)
  max_children <- clean_posint(max_children)
  max_children_mss <- clean_posint(max_children_mss)
  HOPACHConfig(
    dist = dist,
    level_selection = level_selection,
    max_levels = max_levels,
    max_children = max_children,
    max_children_mss = max_children_mss,
    collapse = collapse,
    new_medoid = new_medoid,
    mss = mss,
    min_improvement = min_improvement,
    initial_order = initial_order,
    element_order = element_order
  )
} # /rtemis::setup_HOPACH


# %% PAMConfig ----
#' @title PAMConfig
#'
#' @description
#' ClusteringConfig subclass for Partitioning Around Medoids (PAM).
#'
#' @author EDG
#' @keywords internal
#' @noRd
PAMConfig <- new_class(
  name = "PAMConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("PAM"),
    # The backend's bound is `k <= n_cases - 1`, which `data_bound = "n_cases"`
    # cannot state: it checks `<=` the dimension itself. The real bound is given
    # in the description instead, so the published note is not off by one.
    k = prop_integer(
      3L,
      min = 1L,
      description = paste0(
        "Number of clusters. Must be fewer than the number of cases."
      )
    ),
    dist = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance measure."
    ),
    stand = prop_boolean(
      FALSE,
      description = paste0(
        "Standardize each variable before computing dissimilarities, by ",
        "subtracting its mean and dividing by its mean absolute deviation."
      )
    ),
    do_swap = prop_boolean(
      TRUE,
      description = paste0(
        "Perform the swap phase. Skipping it is faster and gives only the ",
        "build phase's initial medoids."
      )
    ),
    variant = prop_string(
      "original",
      enum = c("original", "o_1", "o_2", "f_3", "f_4", "f_5", "faster"),
      description = paste0(
        "Algorithm variant. \"original\" is the published algorithm; the ",
        "others trade exactness for speed, \"faster\" most aggressively."
      )
    ),
    nstart = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = paste0(
        "Number of random starts. Unset uses the deterministic build phase ",
        "instead of drawing the initial medoids at random."
      )
    )
  )
) # /rtemis::PAMConfig


# %% setup_PAM ----
#' Setup PAMConfig
#'
#' Setup a `PAMConfig` object for Partitioning Around Medoids (PAM) clustering,
#' via the 'cluster' package.
#'
#' PAM is the medoid-based counterpart of k-means: each cluster is represented
#' by one of the cases rather than by a mean, which makes it usable with
#' non-Euclidean dissimilarities and less sensitive to outliers.
#'
#' Argument names are rtemis' own. They map to `cluster::pam()` as:
#' `dist` -> `metric`, `do_swap` -> `do.swap`; the rest keep their names.
#'
#' @param k Integer [1, Inf): Number of clusters. Must be fewer than the number of cases.
#' @param dist Character \{"euclidean", "manhattan"\}: Distance measure to use.
#' @param stand Logical: If TRUE, standardize each variable before computing dissimilarities.
#' @param do_swap Logical: If TRUE, perform the swap phase.
#' @param variant Character \{"original", "o_1", "o_2", "f_3", "f_4", "f_5", "faster"\}: Algorithm variant; "original" is the published algorithm, the others trade exactness for speed.
#' @param nstart Optional Integer [1, Inf): Number of random starts. NULL uses the deterministic build phase instead of drawing the initial medoids at random.
#'
#' @return `PAMConfig` object.
#'
#' @references
#' Kaufman L, Rousseeuw PJ (1990). \emph{Finding Groups in Data: An
#' Introduction to Cluster Analysis}. Wiley.
#' \doi{10.1002/9780470316801}
#'
#' @author EDG
#' @export
#' @examples
#' pam_config <- setup_PAM(k = 3L)
#' pam_config
setup_PAM <- function(
  k = 3L,
  dist = "euclidean",
  stand = FALSE,
  do_swap = TRUE,
  variant = "original",
  nstart = NULL
) {
  k <- clean_posint(k)
  if (!is.null(nstart)) {
    nstart <- clean_posint(nstart)
  }
  PAMConfig(
    k = k,
    dist = dist,
    stand = stand,
    do_swap = do_swap,
    variant = variant,
    nstart = nstart
  )
} # /rtemis::setup_PAM


# %% PAMKConfig ----
#' @title PAMKConfig
#'
#' @description
#' ClusteringConfig subclass for Partitioning Around Medoids with estimation of
#' the number of clusters (PAMK). PAMK selects `k` from `krange`, so this config
#' declares no `k`: the number of clusters chosen is reported by the resulting
#' `Clustering`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
PAMKConfig <- new_class(
  name = "PAMKConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("PAMK"),
    krange = prop_integer(
      2:10,
      min = 1L,
      vector = TRUE,
      unique_items = TRUE,
      contains_min = 2L,
      description = paste0(
        "Candidate numbers of clusters to compare. Neither criterion can ",
        "score a single cluster, so including 1 selects it only when a ",
        "Duda-Hart test finds no evidence for more, and a set holding only 1 ",
        "has nothing to choose between."
      )
    ),
    criterion = prop_string(
      "asw",
      enum = c("asw", "multiasw", "ch"),
      description = paste0(
        "Criterion used to choose the number of clusters: \"asw\" = average ",
        "silhouette width, \"multiasw\" = average silhouette width computed ",
        "on random subsets, \"ch\" = Calinski-Harabasz index."
      )
    ),
    use_pam = prop_boolean(
      TRUE,
      description = paste0(
        "Fit each candidate with PAM. Disabling it uses CLARA instead, which ",
        "subsamples and is intended for large datasets."
      )
    ),
    scaling = prop_boolean(
      FALSE,
      description = paste0(
        "Scale each variable by its root mean square, after centering, ",
        "before clustering."
      )
    ),
    alpha = prop_float(
      0.001,
      min = 0,
      max = 1,
      description = paste0(
        "Significance level of the Duda-Hart test, which decides between one ",
        "cluster and more. Only used when 1 is among the candidates."
      )
    ),
    n_subsets = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      applies_when = list(criterion = "multiasw"),
      description = paste0(
        "Number of random subsets the average silhouette width is computed on."
      )
    )
  ),
  # Neither criterion can score a single cluster, so a search over 1 alone has
  # nothing to choose between and the backend fails with "subscript out of
  # bounds".
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::PAMKConfig


# %% setup_PAMK ----
#' Setup PAMKConfig
#'
#' Setup a `PAMKConfig` object for Partitioning Around Medoids with estimation
#' of the number of clusters (PAMK), via the 'fpc' package.
#'
#' PAMK fits PAM (or CLARA) for every candidate in `krange` and keeps the one
#' scoring best under `criterion`. There is therefore no `k` to set, and the
#' number of clusters chosen is reported by the resulting `Clustering`.
#'
#' Argument names are rtemis' own. They map to `fpc::pamk()` as:
#' `use_pam` -> `usepam`, `n_subsets` -> `ns`; the rest keep their names.
#'
#' `fpc::pamk()` also accepts a per-variable numeric vector for `scaling`; only
#' the logical form is exposed here. Scale by column with [preprocess] instead,
#' where the decision is recorded and replayed with the data.
#'
#' @param krange Integer [1, Inf) vector: Candidate numbers of clusters to compare. Must include at least one value greater than 1.
#' @param criterion Character \{"asw", "multiasw", "ch"\}: Criterion used to choose the number of clusters.
#' @param use_pam Logical: If TRUE, fit each candidate with PAM; if FALSE, with CLARA.
#' @param scaling Logical: If TRUE, scale each variable by its root mean square after centering.
#' @param alpha Numeric \[0, 1\]: Significance level of the Duda-Hart test, used only when 1 is among the candidates.
#' @param n_subsets Optional Integer [1, Inf): Number of random subsets the average silhouette width is computed on. Applies when `criterion` is "multiasw".
#'
#' @return `PAMKConfig` object.
#'
#' @references
#' Kaufman L, Rousseeuw PJ (1990). \emph{Finding Groups in Data: An
#' Introduction to Cluster Analysis}. Wiley.
#' \doi{10.1002/9780470316801}
#'
#' @author EDG
#' @export
#' @examples
#' pamk_config <- setup_PAMK(krange = 2:5)
#' pamk_config
setup_PAMK <- function(
  krange = 2:10,
  criterion = "asw",
  use_pam = TRUE,
  scaling = FALSE,
  alpha = 0.001,
  n_subsets = NULL
) {
  krange <- clean_posint(krange)
  if (!is.null(n_subsets)) {
    n_subsets <- clean_posint(n_subsets)
  }
  PAMKConfig(
    krange = krange,
    criterion = criterion,
    use_pam = use_pam,
    scaling = scaling,
    alpha = alpha,
    n_subsets = n_subsets
  )
} # /rtemis::setup_PAMK


# %% GMMConfig ----
#' @title GMMConfig
#'
#' @description
#' ClusteringConfig subclass for Gaussian Mixture Model clustering.
#'
#' `k` is optional, which makes this the one clustering config that both
#' prescribes and discovers: left unset, the number of components is selected
#' by BIC over the candidate models; set, it is fixed and BIC selects only the
#' covariance parameterization.
#'
#' @author EDG
#' @keywords internal
#' @noRd
GMMConfig <- new_class(
  name = "GMMConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("GMM"),
    k = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = paste0(
        "Number of mixture components. Unset selects it by BIC over the ",
        "candidate models."
      )
    ),
    # The 14 multivariate parameterizations. Each is three letters for the
    # components' volume, shape and orientation: E equal across components,
    # V varying, I axis-aligned.
    model_names = prop_string(
      NULL,
      enum = c(
        "EII",
        "VII",
        "EEI",
        "VEI",
        "EVI",
        "VVI",
        "EEE",
        "VEE",
        "EVE",
        "VVE",
        "EEV",
        "VEV",
        "EVV",
        "VVV"
      ),
      nullable = TRUE,
      vector = TRUE,
      unique_items = TRUE,
      description = paste0(
        "Covariance parameterizations to consider. Each names the components' ",
        "volume, shape and orientation in that order: \"E\" equal across ",
        "components, \"V\" varying, \"I\" axis-aligned. Unset considers all ",
        "of them."
      )
    )
  )
) # /rtemis::GMMConfig


# %% setup_GMM ----
#' Setup GMMConfig
#'
#' Setup a `GMMConfig` object for Gaussian Mixture Model clustering, via the
#' 'mclust' package.
#'
#' A GMM is the model-based counterpart of the prototype methods: each cluster
#' is a Gaussian component with its own mean and covariance, so clusters may be
#' elongated, differently oriented and differently sized, and every case gets a
#' posterior probability for each component rather than only a label. The
#' result is therefore a soft clustering.
#'
#' Both the number of components and the covariance parameterization are
#' selected by BIC, which makes `k` optional: leave it unset to have the number
#' of components chosen, or set it to fix the number and let BIC choose only the
#' parameterization.
#'
#' Argument names are rtemis' own: `k` is `mclust::Mclust()`'s `G` and
#' `model_names` is its `modelNames`.
#'
#' @param k Optional Integer [1, Inf): Number of mixture components. Unset selects it by BIC.
#' @param model_names Optional Character \{"EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV"\} vector: Covariance parameterizations to consider. Unset considers all of them.
#'
#' @return `GMMConfig` object.
#'
#' @references
#' Scrucca L, Fop M, Murphy TB, Raftery AE (2016). mclust 5: Clustering,
#' Classification and Density Estimation Using Gaussian Finite Mixture Models.
#' \emph{The R Journal}, 8(1), 289-317.
#' \doi{10.32614/RJ-2016-021}
#'
#' @author EDG
#' @export
#' @examples
#' gmm_config <- setup_GMM(k = 3L)
#' gmm_config
setup_GMM <- function(k = NULL, model_names = NULL) {
  if (!is.null(k)) {
    k <- clean_posint(k)
  }
  GMMConfig(k = k, model_names = model_names)
} # /rtemis::setup_GMM


# %% SpectralConfig ----
#' @title SpectralConfig
#'
#' @description
#' ClusteringConfig subclass for spectral clustering.
#'
#' `kernel` fuses the backend's two kernel arguments into one setting. That is
#' not cosmetic: `kernlab::specc()` discards its `kernel` argument whenever
#' `kpar` is one of the strings "automatic" or "local", so the pair admits
#' combinations that name a kernel and then silently use a different one. One
#' enum cannot express those.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SpectralConfig <- new_class(
  name = "SpectralConfig",
  parent = ClusteringConfig,
  properties = list(
    algorithm = prop_algorithm("Spectral"),
    # The backend's upper bound is `k <= n_cases`, and its lower bound is 2:
    # one cluster leaves a single eigenvector and the embedding loses its
    # matrix shape. `data_bound = "n_cases"` states neither, so both are in the
    # description, as `PAMConfig` does for the same reason.
    k = prop_integer(
      3L,
      min = 2L,
      description = paste0(
        "Number of clusters. Must be at least 2 and no more than the number ",
        "of cases."
      )
    ),
    kernel = prop_string(
      "rbf",
      enum = c("rbf", "rbf_local", "laplace"),
      description = paste0(
        "Similarity measure the affinity matrix is built from: \"rbf\" = ",
        "Gaussian, with one width shared by every case; \"rbf_local\" = ",
        "Gaussian with a per-case width taken from the distance to its ",
        "seventh nearest neighbor, which lets one clustering hold groups of ",
        "differing density; \"laplace\" = exponential, which decays more ",
        "slowly than the Gaussian and so keeps more weight on distant pairs."
      )
    ),
    sigma = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      applies_when = list(kernel = c("rbf", "laplace")),
      description = paste0(
        "Inverse kernel width: larger values make the affinity fall off ",
        "faster with distance. Unset estimates it for \"rbf\" by searching a ",
        "grid of widths for the one whose embedding clusters most tightly, ",
        "and uses 1 for \"laplace\"."
      )
    ),
    sigma_sample_fraction = prop_float(
      NULL,
      exclusive_min = 0,
      max = 1,
      nullable = TRUE,
      applies_when = list(kernel = "rbf"),
      description = paste0(
        "Fraction of the cases the kernel width is estimated from. Lowering ",
        "it is the way to make the search affordable on many cases, since it ",
        "decomposes one affinity matrix per candidate width. Used only when ",
        "\"sigma\" is unset, which is what triggers the search. Unset uses ",
        "three quarters of them."
      )
    ),
    iterations = prop_integer(
      200L,
      min = 1L,
      description = paste0(
        "Maximum number of k-means iterations run on the spectral embedding."
      )
    ),
    nystrom = prop_boolean(
      FALSE,
      description = paste0(
        "Approximate the affinity matrix from a sample of the cases by the ",
        "Nystrom method, rather than decomposing it in full. The exact ",
        "decomposition is cubic in the number of cases; this trades some ",
        "accuracy for a cost set by the sample size instead. Cannot be ",
        "combined with the \"rbf_local\" kernel, whose per-case widths need ",
        "every pairwise distance."
      )
    ),
    nystrom_sample = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      applies_when = list(nystrom = TRUE),
      description = paste0(
        "Number of cases the approximation samples. Must be fewer than the ",
        "number of cases, and enough larger than the number of clusters for ",
        "the sample to be clustered on its own. Unset samples one sixth of ",
        "the cases."
      )
    )
  ),
  # Local scaling reads a per-case width off the full pairwise distance
  # matrix, which the Nystrom approximation never forms; the backend refuses
  # the combination outright.
  validator = function(self) {
    c(
      check_applies_when(self),
      if (identical(self@kernel, "rbf_local") && self@nystrom) {
        paste0(
          "@nystrom cannot be combined with @kernel \"rbf_local\": local ",
          "scaling needs every pairwise distance."
        )
      }
    )
  }
) # /rtemis::SpectralConfig


# %% setup_Spectral ----
#' Setup SpectralConfig
#'
#' Setup a `SpectralConfig` object for spectral clustering, via the 'kernlab'
#' package.
#'
#' Spectral clustering does not look for compact groups in the data itself. It
#' builds a similarity graph over the cases, embeds them in the leading
#' eigenvectors of that graph's normalized Laplacian, and runs k-means there.
#' Groups that are connected but not compact -- concentric rings, elongated
#' bands -- separate in that embedding while k-means on the raw features cannot
#' find them.
#'
#' The cost is cubic in the number of cases, since the whole affinity matrix is
#' decomposed. Set `nystrom` to approximate the decomposition from a sample
#' instead.
#'
#' `kernel` fuses `kernlab::specc()`'s `kernel` and `kpar` arguments, which are
#' not independent: `kpar = "automatic"` and `kpar = "local"` both replace
#' whatever `kernel` names with a Gaussian kernel of their own. The three
#' values here are the combinations that mean something, and each maps to one
#' pair. The remaining 'kernlab' kernels -- polynomial, linear, hyperbolic
#' tangent, Bessel, ANOVA, spline -- are not exposed: they are inner products
#' rather than distance-decaying similarities, so they can give an affinity
#' matrix with negative entries, which is not a similarity graph and which the
#' normalization step has no defined meaning for.
#'
#' Argument names are rtemis' own. They map to `kernlab::specc()` as: `k` ->
#' `centers`, `sigma_sample_fraction` -> `mod.sample`, `nystrom` ->
#' `nystrom.red`, `nystrom_sample` -> `nystrom.sample`; `iterations` keeps its
#' name, and `kernel` and `sigma` together set `kernel` and `kpar`.
#'
#' @param k Integer [2, Inf): Number of clusters. Must be no more than the number of cases.
#' @param kernel Character \{"rbf", "rbf_local", "laplace"\}: Similarity measure the affinity matrix is built from.
#' @param sigma Optional Numeric (0, Inf): Inverse kernel width. Unset estimates it for "rbf" and uses 1 for "laplace". Applies when `kernel` is "rbf" or "laplace".
#' @param sigma_sample_fraction Optional Numeric (0, 1\]: Fraction of the cases the kernel width is estimated from. Unset uses three quarters of them. Used only when `sigma` is unset. Applies when `kernel` is "rbf".
#' @param iterations Integer [1, Inf): Maximum number of k-means iterations run on the spectral embedding.
#' @param nystrom Logical: If TRUE, approximate the affinity matrix from a sample of the cases by the Nystrom method.
#' @param nystrom_sample Optional Integer [1, Inf): Number of cases the approximation samples. Unset samples one sixth of them. Applies when `nystrom` is TRUE.
#'
#' @return `SpectralConfig` object.
#'
#' @references
#' Ng AY, Jordan MI, Weiss Y (2001). On Spectral Clustering: Analysis and an
#' Algorithm. \emph{Advances in Neural Information Processing Systems}, 14,
#' 849-856.
#'
#' Zelnik-Manor L, Perona P (2004). Self-Tuning Spectral Clustering.
#' \emph{Advances in Neural Information Processing Systems}, 17, 1601-1608.
#'
#' @author EDG
#' @export
#' @examples
#' spectral_config <- setup_Spectral(k = 3L)
#' spectral_config
setup_Spectral <- function(
  k = 3L,
  kernel = "rbf",
  sigma = NULL,
  sigma_sample_fraction = NULL,
  iterations = 200L,
  nystrom = FALSE,
  nystrom_sample = NULL
) {
  k <- clean_posint(k)
  iterations <- clean_posint(iterations)
  if (!is.null(nystrom_sample)) {
    nystrom_sample <- clean_posint(nystrom_sample)
  }
  SpectralConfig(
    k = k,
    kernel = kernel,
    sigma = sigma,
    sigma_sample_fraction = sigma_sample_fraction,
    iterations = iterations,
    nystrom = nystrom,
    nystrom_sample = nystrom_sample
  )
} # /rtemis::setup_Spectral


# %% .list_to_ClusteringConfig ----
#' Convert a list to a ClusteringConfig object
#'
#' Internal function used to reconstruct a `ClusteringConfig` object from a named
#' list, such as the result of parsing a JSON config conforming to the
#' schema.rtemis.org clustering schema. The list must carry an `algorithm`
#' element; the remaining elements, its siblings, are passed to that
#' algorithm's `setup_*` function.
#'
#' @param x Named list with an `algorithm` element plus algorithm-specific
#'   parameters as its siblings, e.g. `list(algorithm = "DBSCAN", eps = 0.5)`.
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
  # One shape: `{algorithm, k, ...}`, which is what the published schema
  # declares. `.drop_meta_keys()` removes document metadata (e.g. `$schema`),
  # which is not a setup arg.
  label <- paste(algorithm, "clustering")
  check_no_settings_key(x, "config", "algorithm", label)
  params <- .drop_meta_keys(x)
  params[["algorithm"]] <- NULL
  setup_fn <- get_clust_setup_fn(algorithm)
  check_wire_keys(params, names(formals(setup_fn)), label)
  do.call(setup_fn, params)
} # /rtemis::.list_to_ClusteringConfig
