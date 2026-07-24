# 09_DecompositionConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# Architecture ----
# Mirrors 02_Hyperparameters.R: each `*Config` subclass declares its
# algorithm parameters with the `prop_*` factories, from which the S7
# validators, the `config` list, and the JSON Schema (S7_to_JSONSchema) are
# generated. The abstract `DecompositionConfig` superclass provides the
# computed `config` list (assembled from the subclass's own properties;
# assignment routes back to them) and carries the algorithm-agnostic
# `features` selection. Decomposition has no tuning, so every parameter is a
# fixed scalar (or, for `features`, a plain vector). Parameters whose R type
# cannot be expressed as a JSON type (tSNE `Y_init`, a matrix) are plain
# properties: stored and validated by class, but excluded from schemas.

# %% DecompositionConfig ----
#' DecompositionConfig
#'
#' @description
#' Abstract superclass for decomposition configs. Subclasses declare each
#' algorithm parameter as a property; this class contributes the computed
#' `config` list and the `features` selection.
#'
#' @field algorithm Character: Algorithm name (computed constant, overridden
#'   per subclass).
#' @field features Optional Character: Names of the feature columns to
#'   decompose. `NULL` means all numeric features.
#' @field config List: Algorithm-specific parameters (computed from the
#'   subclass's properties; assignment routes back and validates).
#'
#' @author EDG
#' @keywords internal
#' @noRd
DecompositionConfig <- new_class(
  name = "DecompositionConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    algorithm = class_character,
    features = NULL | class_character,
    config = new_property(
      class_list,
      getter = function(self) {
        own_prop_values(self, DecompositionConfig)
      },
      setter = function(self, value) {
        route_config_assignment(self, DecompositionConfig, value)
      }
    )
  )
) # /rtemis::DecompositionConfig


# %% serializable_props.DecompositionConfig ----
# Serialize as {algorithm, config[, features]} (the public shape); the
# per-algorithm properties are redundant with the computed `config`.
method(serializable_props, DecompositionConfig) <- function(x) {
  out <- list(
    algorithm = x@algorithm,
    config = config_prop_values(x, DecompositionConfig)
  )
  if (!is.null(x@features)) {
    out[["features"]] <- x@features
  }
  out
} # /rtemis::serializable_props.DecompositionConfig


# %% `$`.DecompositionConfig ----
# Make DecompositionConfig@config@name `$`-accessible ----
method(`$`, DecompositionConfig) <- function(x, name) {
  x@config[[name]]
}


# %% `.DollarNames`.DecompositionConfig ----
# `$`-autocomplete DecompositionConfig@config ----
method(`.DollarNames`, DecompositionConfig) <- function(x, pattern = "") {
  all_names <- names(x@config)
  grep(pattern, all_names, value = TRUE)
}


# %% `[`.DecompositionConfig ----
# Make props `[`-accessible ----
method(`[`, DecompositionConfig) <- function(x, name) {
  props(x)[[name]]
}


# %% `[[`.DecompositionConfig ----
# Make DecompositionConfig@config@name `[[`-accessible ----
method(`[[`, DecompositionConfig) <- function(x, name) {
  x@config[[name]]
}


# %% repr.DecompositionConfig ----
#' Show Method for DecompositionConfig
#'
#' @param object DecompositionConfig object.
#' @param pad Integer: Left side padding.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return character
#'
#' @author EDG
#' @noRd
method(repr, DecompositionConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  features <- x["features"]
  config <- x["config"]
  if (!is.null(features)) {
    config <- c(config, list(features = features))
  }
  paste0(
    repr_S7name(
      paste(x["algorithm"], "DecompositionConfig"),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(config, pad = pad, limit = -1L, output_type = output_type)
  )
} # /rtemis::repr.DecompositionConfig


# %% print.DecompositionConfig ----
#' Print Method for DecompositionConfig
#'
#' @param x DecompositionConfig object.
#' @param pad Integer: Left side padding.
#' @param ... Not used.
#'
#' @return DecompositionConfig object, invisibly.
#'
#' @author EDG
#' @noRd
method(print, DecompositionConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
}


# %% validate_decom_features ----
#' Validate a decomposition `features` selection
#'
#' Light validation performed at `setup_*` time, where the data are not yet
#' available: confirms the selection is a character vector of unique names with at
#' least two entries. The "columns exist and are numeric" check is deferred to
#' [train()], which has the data.
#'
#' @param features Optional Character: Feature column names to decompose, or `NULL`.
#'
#' @return `features`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_decom_features <- function(features) {
  if (is.null(features)) {
    return(invisible(NULL))
  }
  if (!is.character(features)) {
    rtemis.core::abort(
      "`features` must be a character vector of column names or `NULL`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (anyDuplicated(features)) {
    rtemis.core::abort(
      "`features` must not contain duplicate names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (length(features) < 2L) {
    rtemis.core::abort(
      "`features` must name at least 2 columns to decompose, but ",
      length(features),
      " given.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  invisible(features)
} # /rtemis::validate_decom_features


# %% PCAConfig ----
#' @title PCAConfig
#'
#' @description
#' DecompositionConfig subclass for Principal Component Analysis.
#'
#' @author EDG
#' @noRd
PCAConfig <- new_class(
  name = "PCAConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("PCA"),
    k = prop_integer(
      3L,
      min = 1L,
      description = "Number of components to extract."
    ),
    center = prop_boolean(TRUE, description = "Center the data."),
    scale = prop_boolean(TRUE, description = "Scale the data."),
    tol = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = "Magnitude tolerance below which components are omitted."
    )
  )
) # /rtemis::PCAConfig


# %% setup_PCA ----
#' Setup PCA config.
#'
#' @param k Integer [1, Inf): Number of components. (passed to `prcomp` `rank.`)
#' @param center Logical: If TRUE, center the data.
#' @param scale Logical: If TRUE, scale the data.
#' @param tol Optional Numeric [0, Inf): Tolerance.
#' @param features Optional Character: Names of the feature columns to decompose.
#' `NULL` decomposes all numeric features.
#'
#' @return PCAConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' pca_config <- setup_PCA(k = 3L)
#' pca_config
setup_PCA <- function(
  k = 3L,
  center = TRUE,
  scale = TRUE,
  tol = NULL,
  features = NULL
) {
  k <- clean_posint(k)
  validate_decom_features(features)
  PCAConfig(
    k = k,
    center = center,
    scale = scale,
    tol = tol,
    features = features
  )
} # /rtemis::setup_PCA


# %% ICAConfig ----
#' @title ICAConfig
#'
#' @description
#' DecompositionConfig subclass for Independent Component Analysis.
#'
#' @author EDG
#' @noRd
ICAConfig <- new_class(
  name = "ICAConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("ICA"),
    k = prop_integer(
      3L,
      min = 1L,
      description = "Number of components to extract."
    ),
    type = prop_string(
      "parallel",
      enum = c("parallel", "deflation"),
      description = "Component extraction scheme."
    ),
    fun = prop_string(
      "logcosh",
      enum = c("logcosh", "exp"),
      description = "Functional form of the approximation to neg-entropy."
    ),
    alpha = prop_float(
      1.0,
      min = 1,
      max = 2,
      description = "Used with `fun = \"logcosh\"`."
    ),
    row_norm = prop_boolean(
      TRUE,
      description = "Normalize rows of the input before ICA."
    ),
    maxit = prop_integer(
      100L,
      min = 1L,
      description = "Maximum number of iterations."
    ),
    tol = prop_float(1e-04, min = 0, description = "Convergence tolerance.")
  )
) # /rtemis::ICAConfig


# %% setup_ICA ----
#' @title setup_ICA
#'
#' @description
#' Setup ICA config.
#'
#' @param k Integer [1, Inf): Number of components.
#' @param type Character \{"parallel", "deflation"\}: Type of ICA.
#' @param fun Character \{"logcosh", "exp"\}: ICA function.
#' @param alpha Numeric \[1, 2\]: Used in approximation to neg-entropy with `fun = "logcosh"`.
#' @param row_norm Logical: If TRUE, normalize rows of `x` before ICA.
#' @param maxit Integer [1, Inf): Maximum number of iterations.
#' @param tol Numeric [0, Inf): Tolerance.
#' @param features Optional Character: Names of the feature columns to decompose.
#' `NULL` decomposes all numeric features.
#'
#' @return ICAConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' ica_config <- setup_ICA(k = 3L)
#' ica_config
setup_ICA <- function(
  k = 3L,
  type = "parallel",
  fun = "logcosh",
  alpha = 1.0,
  row_norm = TRUE,
  maxit = 100L,
  tol = 1e-04,
  features = NULL
) {
  k <- clean_posint(k)
  maxit <- clean_posint(maxit)
  validate_decom_features(features)
  ICAConfig(
    k = k,
    type = type,
    fun = fun,
    alpha = alpha,
    row_norm = row_norm,
    maxit = maxit,
    tol = tol,
    features = features
  )
} # /rtemis::setup_ICA


# %% NMFConfig ----
#' @title NMFConfig
#'
#' @description
#' DecompositionConfig subclass for Non-negative Matrix Factorization.
#'
#' @author EDG
#' @noRd
NMFConfig <- new_class(
  name = "NMFConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("NMF"),
    k = prop_integer(
      2L,
      min = 1L,
      description = "Number of components to extract."
    ),
    method = prop_string(
      "brunet",
      description = "NMF method (see `NMF::nmf`)."
    ),
    nrun = prop_integer(
      1L,
      min = 1L,
      description = "Number of runs to perform."
    )
  )
) # /rtemis::NMFConfig


# %% setup_NMF ----
#' Setup NMF config.
#'
#' @param k Integer [1, Inf): Number of components.
#' @param method Character: NMF method. See `NMF::nmf`.
#' @param nrun Integer [1, Inf): Number of runs to perform.
#' @param features Optional Character: Names of the feature columns to decompose.
#' `NULL` decomposes all numeric features.
#'
#' @return NMFConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' nmf_config <- setup_NMF(k = 3L)
#' nmf_config
setup_NMF <- function(
  k = 2L,
  method = "brunet",
  nrun = if (length(k) > 1L) 30L else 1L,
  features = NULL
) {
  k <- clean_posint(k)
  nrun <- clean_posint(nrun)
  validate_decom_features(features)
  NMFConfig(k = k, method = method, nrun = nrun, features = features)
} # /rtemis::setup_NMF


# %% UMAPConfig ----
#' @title UMAPConfig
#'
#' @description
#' DecompositionConfig subclass for Uniform Manifold Approximation and Projection.
#'
#' @author EDG
#' @noRd
UMAPConfig <- new_class(
  name = "UMAPConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("UMAP"),
    k = prop_integer(
      2L,
      min = 1L,
      description = "Number of components to extract."
    ),
    n_neighbors = prop_integer(
      15L,
      min = 1L,
      description = "Number of neighbors."
    ),
    init = prop_string(
      "spectral",
      description = "Initialization type (see `uwot::umap` `init`)."
    ),
    metric = prop_string(
      "euclidean",
      enum = c("euclidean", "cosine", "manhattan", "hamming", "categorical"),
      description = "Distance metric."
    ),
    n_epochs = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Number of epochs. NULL = algorithm default."
    ),
    learning_rate = prop_float(1.0, min = 0, description = "Learning rate."),
    scale = prop_boolean(TRUE, description = "Scale input data before UMAP.")
  )
) # /rtemis::UMAPConfig


# %% setup_UMAP ----
#' Setup UMAP config.
#'
#' @details
#' A high `n_neighbors` value may give error in some systems:
#' "Error in irlba::irlba(L, nv = n, nu = 0, maxit = iters) :
#'  function 'as_cholmod_sparse' not provided by package 'Matrix'"
#'
#' @param k Integer [1, Inf): Number of components.
#' @param n_neighbors Integer [1, Inf): Number of neighbors.
#' @param init Character: Initialization type. See `uwot::umap "init"`.
#' @param metric Character \{"euclidean", "cosine", "manhattan", "hamming", "categorical"\}: Distance metric.
#' @param n_epochs Optional Integer [1, Inf): Number of epochs.
#' @param learning_rate Numeric [0, Inf): Learning rate.
#' @param scale Logical: If TRUE, scale input data before doing UMAP.
#' @param features Optional Character: Names of the feature columns to decompose.
#' `NULL` decomposes all numeric features.
#'
#' @return UMAPConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' umap_config <- setup_UMAP(k = 3L)
#' umap_config
setup_UMAP <- function(
  k = 2L,
  n_neighbors = 15L,
  init = "spectral",
  metric = "euclidean",
  n_epochs = NULL,
  learning_rate = 1.0,
  scale = TRUE,
  features = NULL
) {
  k <- clean_posint(k)
  n_neighbors <- clean_posint(n_neighbors)
  n_epochs <- clean_posint(n_epochs)
  validate_decom_features(features)
  UMAPConfig(
    k = k,
    n_neighbors = n_neighbors,
    init = init,
    metric = metric,
    n_epochs = n_epochs,
    learning_rate = learning_rate,
    scale = scale,
    features = features
  )
} # /rtemis::setup_UMAP


# %% tSNEConfig ----
#' @title tSNEConfig
#'
#' @description
#' DecompositionConfig subclass for t-Distributed Stochastic Neighbor Embedding.
#' `Y_init` (an optional initial embedding matrix) is a plain property: it is
#' not JSON-expressible and is excluded from the generated schema.
#'
#' @author EDG
#' @noRd
tSNEConfig <- new_class(
  name = "tSNEConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("tSNE"),
    k = prop_integer(
      2L,
      min = 1L,
      description = "Number of components to extract."
    ),
    initial_dims = prop_integer(
      50L,
      min = 1L,
      description = "Initial dimensions."
    ),
    perplexity = prop_float(30, min = 0, description = "Perplexity."),
    theta = prop_float(
      0.5,
      min = 0,
      max = 1,
      description = "Speed/accuracy trade-off."
    ),
    check_duplicates = prop_boolean(
      TRUE,
      description = "Check for duplicates."
    ),
    pca = prop_boolean(TRUE, description = "Perform an initial PCA step."),
    partial_pca = prop_boolean(
      FALSE,
      description = "Use truncated PCA (irlba)."
    ),
    max_iter = prop_integer(
      1000L,
      min = 1L,
      description = "Maximum number of iterations."
    ),
    verbose = prop_boolean(FALSE, description = "Print progress."),
    is_distance = prop_boolean(
      FALSE,
      description = "Treat the input as a distance matrix."
    ),
    Y_init = prop_external(
      NULL | S7::new_S3_class("matrix"),
      data_dependent = TRUE
    ),
    pca_center = prop_boolean(
      TRUE,
      description = "Center before the PCA step."
    ),
    pca_scale = prop_boolean(FALSE, description = "Scale before the PCA step."),
    normalize = prop_boolean(TRUE, description = "Normalize the input."),
    stop_lying_iter = prop_integer(
      250L,
      min = 0L,
      description = "Iteration after which exaggeration stops."
    ),
    mom_switch_iter = prop_integer(
      250L,
      min = 0L,
      description = "Iteration at which momentum switches."
    ),
    momentum = prop_float(0.5, description = "Initial momentum."),
    final_momentum = prop_float(
      0.8,
      description = "Momentum used later in optimization."
    ),
    eta = prop_float(200, description = "Learning rate."),
    exaggeration_factor = prop_float(
      12,
      description = "Early-exaggeration factor."
    ),
    num_threads = prop_integer(
      1L,
      min = 0L,
      description = "Number of threads (0 = all cores)."
    )
  )
) # /rtemis::tSNEConfig


# %% .tsne_schema_extra ----
# Schema fragment for the tSNEConfig `Y_init` property (`NULL | matrix`), whose
# R type the prop_* factories do not express. Merged into the generated schema.
# See generate_schemas.R.
.tsne_schema_extra <- list(
  properties = list(
    Y_init = list(
      oneOf = list(
        list(type = "null"),
        list(
          type = "array",
          items = list(
            type = "array",
            items = list(type = "number"),
            minItems = 1L
          ),
          minItems = 1L
        )
      ),
      `$comment` = "Data-dependent: initial embedding matrix, rows = cases, columns = output dimensions.",
      description = "Optional initial Y (embedding) matrix. null = random initialization."
    )
  )
)


# %% setup_tSNE ----
#' Setup tSNE config.
#'
#' @details
#' Get more information on the config by running `?Rtsne::Rtsne`.
#'
#' @param k Integer [1, Inf): Number of components.
#' @param initial_dims Integer [1, Inf): Initial dimensions.
#' @param perplexity Numeric [0, Inf): Perplexity.
#' @param theta Numeric \[0, 1\]: Speed/accuracy trade-off.
#' @param check_duplicates Logical: If TRUE, check for duplicates.
#' @param pca Logical: If TRUE, perform PCA.
#' @param partial_pca Logical: If TRUE, perform partial PCA.
#' @param max_iter Integer [1, Inf): Maximum number of iterations.
#' @param verbose Logical: If TRUE, print messages.
#' @param is_distance Logical: If TRUE, `x` is a distance matrix.
#' @param Y_init Optional Matrix: Initial Y matrix.
#' @param pca_center Logical: If TRUE, center PCA.
#' @param pca_scale Logical: If TRUE, scale PCA.
#' @param normalize Logical: If TRUE, normalize.
#' @param stop_lying_iter Integer [0, Inf): Stop lying iterations.
#' @param mom_switch_iter Integer [0, Inf): Momentum switch iterations.
#' @param momentum Numeric: Momentum.
#' @param final_momentum Numeric: Final momentum.
#' @param eta Numeric: Eta.
#' @param exaggeration_factor Numeric: Exaggeration factor.
#' @param num_threads Integer [0, Inf): Number of threads.
#'
#' @return tSNEConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' tSNE_config <- setup_tSNE(k = 3L)
#' tSNE_config
setup_tSNE <- function(
  k = 2L,
  initial_dims = 50L,
  perplexity = 30,
  theta = 0.5,
  check_duplicates = TRUE,
  pca = TRUE,
  partial_pca = FALSE,
  max_iter = 1000L,
  verbose = getOption("verbose", FALSE),
  is_distance = FALSE,
  Y_init = NULL,
  pca_center = TRUE,
  pca_scale = FALSE,
  normalize = TRUE,
  stop_lying_iter = if (is.null(Y_init)) 250L else 0L,
  mom_switch_iter = if (is.null(Y_init)) 250L else 0L,
  momentum = 0.5,
  final_momentum = 0.8,
  eta = 200,
  exaggeration_factor = 12,
  num_threads = 1L
) {
  k <- clean_posint(k)
  initial_dims <- clean_posint(initial_dims)
  max_iter <- clean_posint(max_iter)
  stop_lying_iter <- clean_int(stop_lying_iter)
  mom_switch_iter <- clean_int(mom_switch_iter)
  num_threads <- clean_int(num_threads)
  tSNEConfig(
    k = k,
    initial_dims = initial_dims,
    perplexity = perplexity,
    theta = theta,
    check_duplicates = check_duplicates,
    pca = pca,
    partial_pca = partial_pca,
    max_iter = max_iter,
    verbose = verbose,
    is_distance = is_distance,
    Y_init = Y_init,
    pca_center = pca_center,
    pca_scale = pca_scale,
    normalize = normalize,
    stop_lying_iter = stop_lying_iter,
    mom_switch_iter = mom_switch_iter,
    momentum = momentum,
    final_momentum = final_momentum,
    eta = eta,
    exaggeration_factor = exaggeration_factor,
    num_threads = num_threads
  )
} # /rtemis::setup_tSNE


# %% IsomapConfig ----
#' @title IsomapConfig
#'
#' @description
#' DecompositionConfig subclass for Isomap.
#'
#' @author EDG
#' @noRd
IsomapConfig <- new_class(
  name = "IsomapConfig",
  parent = DecompositionConfig,
  properties = list(
    algorithm = prop_algorithm("Isomap"),
    k = prop_integer(
      2L,
      min = 1L,
      description = "Number of components to extract."
    ),
    dist_method = prop_string(
      "euclidean",
      enum = c("euclidean", "manhattan"),
      description = "Distance method."
    ),
    nsd = prop_integer(
      0L,
      min = 0L,
      description = "Number of shortest dissimilarities retained (0 = all)."
    ),
    path = prop_string(
      "shortest",
      enum = c("shortest", "extended"),
      description = "`path` argument for `vegan::isomap`."
    )
  )
) # /rtemis::IsomapConfig


# %% setup_Isomap ----
#' Setup Isomap config.
#'
#' @param k Integer [1, Inf): Number of components.
#' @param dist_method Character \{"euclidean", "manhattan"\}: Distance method.
#' @param nsd Integer [0, Inf): Number of shortest dissimilarities retained.
#' @param path Character \{"shortest", "extended"\}: Path argument for `vegan::isomap`.
#'
#' @return IsomapConfig object.
#'
#' @author EDG
#' @export
#' @examples
#' isomap_config <- setup_Isomap(k = 3L)
#' isomap_config
setup_Isomap <- function(
  k = 2L,
  dist_method = "euclidean",
  nsd = 0L,
  path = "shortest"
) {
  k <- clean_posint(k)
  nsd <- clean_int(nsd)
  IsomapConfig(k = k, dist_method = dist_method, nsd = nsd, path = path)
} # /rtemis::setup_Isomap


# %% List of Decomposition Algorithms that can be applied on new data ----
# These algorithms learn a transformation on the training data that can later be
# applied to new (validation / test / unseen) data via `apply_decomp()`.
# Non-parametric embeddings (tSNE, Isomap) have no out-of-sample extension and
# are therefore excluded.
decom_algorithms_applicable <- c("PCA", "ICA", "NMF", "UMAP")

# %% decom_can_apply ----
#' Check whether a decomposition algorithm can be applied on new data
#'
#' @param algorithm Character: Decomposition algorithm name.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decom_can_apply <- function(algorithm) {
  get_decom_name(algorithm) %in% decom_algorithms_applicable
} # /rtemis::decom_can_apply
