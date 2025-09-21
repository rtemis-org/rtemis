# S7_DecompositionParameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# DecompositionParameters ----
#' @title DecompositionParameters
#'
#' @description
#' Decomposition parameters class.
#'
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
#' @noRd
DecompositionParameters <- new_class(
  name = "DecompositionParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /DecompositionParameters

# Make DecompositionParameters@parameters `$`-accessible ----
method(`$`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete DecompositionParameters@parameters ----
method(`.DollarNames`, DecompositionParameters) <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Make props `[`-accessible ----
method(`[`, DecompositionParameters) <- function(x, name) {
  props(x)[[name]]
}

# Make DecompositionParameters@parameters `[[`-accessible ----
method(`[[`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# Show DecompositionParameters ----
#' Show Method for DecompositionParameters
#'
#' @param object DecompositionParameters object.
#' @param pad Integer: Left side padding.
#' @param output_type Character {"ansi", "html", "plain"}: Output type.
#'
#' @return character
#'
#' @author EDG
#' @noRd
method(repr, DecompositionParameters) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      paste(x["algorithm"], "DecompositionParameters"),
      pad = pad,
      output_type = output_type
    ),
    show_ls(x["parameters"], pad = pad, limit = -1L, output_type = output_type)
  )
} # /rtemis::show.DecompositionParameters

# Print DecompositionParameters ----
#' Print Method for DecompositionParameters
#'
#' @param x DecompositionParameters object.
#' @param pad Integer: Left side padding.
#' @param ... Not used.
#'
#' @return DecompositionParameters object, invisibly.
#'
#' @author EDG
#' @noRd
method(print, DecompositionParameters) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
}

# PCAParameters ----
#' @title PCAParameters
#'
#' @description
#' DecompositionParameters subclass for Principal Component Analysis.
#' Internal use only.
#'
#' @author EDG
#' @noRd
PCAParameters <- new_class(
  name = "PCAParameters",
  parent = DecompositionParameters,
  constructor = function(k, center, scale, tol) {
    k <- clean_posint(k)
    check_logical(center)
    check_logical(scale)
    check_float0pos(tol)
    new_object(
      DecompositionParameters(
        algorithm = "PCA",
        parameters = list(
          k = k,
          center = center,
          scale = scale,
          tol = tol
        )
      )
    )
  }
) # /rtemis::PCAParameters

# setup_PCA ----
#' Setup PCA parameters.
#'
#' @param k Integer: Number of components. (passed to `prcomp` `rank.`)
#' @param center Logical: If TRUE, center the data.
#' @param scale Logical: If TRUE, scale the data.
#' @param tol Numeric: Tolerance.
#'
#' @return PCAParameters object.
#'
#' @author EDG
#' @export
setup_PCA <- function(k = 3L, center = TRUE, scale = TRUE, tol = NULL) {
  k <- clean_posint(k)
  check_logical(center)
  check_logical(scale)
  check_float0pos(tol)
  PCAParameters(k, center, scale, tol)
} # /rtemis::setup_PCA

# ICAParameters ----
#' @title ICAParameters
#'
#' @description
#' DecompositionParameters subclass for Independent Component Analysis.
#' Internal use only.
#'
#' @author EDG
#' @noRd
ICAParameters <- new_class(
  name = "ICAParameters",
  parent = DecompositionParameters,
  constructor = function(k, type, fun, alpha, row_norm, maxit, tol) {
    new_object(
      DecompositionParameters(
        algorithm = "ICA",
        parameters = list(
          k = k,
          type = type,
          fun = fun,
          alpha = alpha,
          row_norm = row_norm,
          maxit = maxit,
          tol = tol
        )
      )
    )
  }
) # /rtemis::ICAParameters

# setup_ICA ----
#' @title setup_ICA
#'
#' @description
#' Setup ICA parameters.
#'
#' @param k Integer: Number of components.
#' @param type Character: Type of ICA: "parallel" or "deflation".
#' @param fun Character: ICA function: "logcosh", "exp".
#' @param alpha Numeric \[1, 2\]: Used in approximation to neg-entropy with `fun = "logcosh"`.
#' @param row_norm Logical: If TRUE, normalize rows of `x` before ICA.
#' @param maxit Integer: Maximum number of iterations.
#' @param tol Numeric: Tolerance.
#'
#' @return ICAParameters object.
#'
#' @author EDG
#' @export
setup_ICA <- function(
  k = 3L,
  type = c("parallel", "deflation"),
  fun = c("logcosh", "exp"),
  alpha = 1.0,
  row_norm = TRUE,
  maxit = 100L,
  tol = 1e-04
) {
  k <- clean_posint(k)
  type <- match.arg(type)
  fun <- match.arg(fun)
  stopifnot(alpha >= 1, alpha <= 2)
  check_inherits(row_norm, "logical")
  maxit <- clean_posint(maxit)
  check_inherits(tol, "numeric")
  ICAParameters(
    k = k,
    type = type,
    fun = fun,
    alpha = alpha,
    row_norm = row_norm,
    maxit = maxit,
    tol = tol
  )
} # /rtemis::setup_ICA

# NMFParameters ----
#' @title NMFParameters
#'
#' @description
#' DecompositionParameters subclass for Non-negative Matrix Factorization.
#' Internal use only.
#'
#' @author EDG
#' @noRd
NMFParameters <- new_class(
  name = "NMFParameters",
  parent = DecompositionParameters,
  constructor = function(k, method, nrun) {
    k <- clean_posint(k)
    check_inherits(method, "character")
    nrun <- clean_posint(nrun)
    new_object(
      DecompositionParameters(
        algorithm = "NMF",
        parameters = list(
          k = k,
          method = method,
          nrun = nrun
        )
      )
    )
  }
) # /rtemis::NMFParameters

# setup_NMF ----
#' Setup NMF parameters.
#'
#' @param k Integer: Number of components.
#' @param method Character: NMF method. See `NMF::nmf`.
#' @param nrun Integer: Number of runs to perform.
#'
#' @return NMFParameters object.
#'
#' @author EDG
#' @export
setup_NMF <- function(
  k = 2L,
  method = "brunet",
  nrun = if (length(k) > 1L) 30L else 1L
) {
  k <- clean_posint(k)
  check_inherits(method, "character")
  nrun <- clean_posint(nrun)
  NMFParameters(k, method, nrun)
} # /rtemis::setup_NMF

# UMAPParameters ----
#' @title UMAPParameters
#'
#' @description
#' DecompositionParameters subclass for Uniform Manifold Approximation and Projection.
#' Internal use only.
#'
#' @author EDG
#' @noRd
UMAPParameters <- new_class(
  name = "UMAPParameters",
  parent = DecompositionParameters,
  constructor = function(
    k,
    n_neighbors,
    init,
    metric,
    n_epochs,
    learning_rate,
    scale
  ) {
    k <- clean_posint(k)
    n_neighbors <- clean_posint(n_neighbors)
    check_inherits(init, "character")
    check_inherits(metric, "character")
    n_epochs <- clean_posint(n_epochs)
    check_float0pos(learning_rate)
    check_inherits(scale, "logical")
    new_object(
      DecompositionParameters(
        algorithm = "UMAP",
        parameters = list(
          k = k,
          n_neighbors = n_neighbors,
          init = init,
          metric = metric,
          n_epochs = n_epochs,
          learning_rate = learning_rate,
          scale = scale
        )
      )
    )
  }
) # /rtemis::UMAPParameters

# setup_UMAP ----
#' Setup UMAP parameters.
#'
#' @details
#' A high `n_neighbors` value may give error in some systems:
#' "Error in irlba::irlba(L, nv = n, nu = 0, maxit = iters) :
#'  function 'as_cholmod_sparse' not provided by package 'Matrix'"
#'
#' @param k Integer: Number of components.
#' @param n_neighbors Integer: Number of keighbors.
#' @param init Character: Initialization type. See `uwot::umap "init"`.
#' @param metric Character: Distance metric to use: "euclidean", "cosine",
#' "manhattan", "hamming", "categorical".
#' @param n_epochs Integer: Number of epochs.
#' @param learning_rate Float: Learning rate.
#' @param scale Logical: If TRUE, scale input data before doing UMAP.
#'
#' @return UMAPParameters object.
#'
#' @author EDG
#' @export
setup_UMAP <- function(
  k = 2L,
  n_neighbors = 15L,
  init = "spectral",
  metric = c("euclidean", "cosine", "manhattan", "hamming", "categorical"),
  n_epochs = NULL,
  learning_rate = 1.0,
  scale = TRUE
) {
  k <- clean_posint(k)
  n_neighbors <- clean_posint(n_neighbors)
  init <- match.arg(init)
  metric <- match.arg(metric)
  check_inherits(n_epochs, "integer")
  check_float0pos(learning_rate)
  check_inherits(scale, "logical")
  UMAPParameters(
    k = k,
    n_neighbors = n_neighbors,
    init = init,
    metric = metric,
    n_epochs = n_epochs,
    learning_rate = learning_rate,
    scale = scale
  )
} # /rtemis::setup_UMAP

# tSNEParameters ----
#' @title tSNEParameters
#'
#' @description
#' DecompositionParameters subclass for t-Distributed Stochastic Neighbor Embedding.
#'
#' @author EDG
#' @noRd
tSNEParameters <- new_class(
  name = "tSNEParameters",
  parent = DecompositionParameters,
  constructor = function(
    k = NULL,
    initial_dims = NULL,
    perplexity = NULL,
    theta = NULL,
    check_duplicates = NULL,
    pca = NULL,
    partial_pca = NULL,
    max_iter = NULL,
    verbose = NULL,
    is_distance = NULL,
    Y_init = NULL,
    pca_center = NULL,
    pca_scale = NULL,
    normalize = NULL,
    stop_lying_iter = NULL,
    mom_switch_iter = NULL,
    momentum = NULL,
    final_momentum = NULL,
    eta = NULL,
    exaggeration_factor = NULL,
    num_threads = NULL
  ) {
    k <- clean_posint(k)
    initial_dims <- clean_posint(initial_dims)
    check_logical(check_duplicates)
    check_logical(pca)
    check_logical(partial_pca)
    max_iter <- clean_posint(max_iter)
    check_logical(verbose)
    check_logical(is_distance)
    check_inherits(Y_init, "matrix")
    check_logical(pca_center)
    check_logical(pca_scale)
    check_logical(normalize)
    stop_lying_iter <- clean_posint(stop_lying_iter)
    mom_switch_iter <- clean_posint(mom_switch_iter)
    num_threads <- clean_posint(num_threads)
    new_object(
      DecompositionParameters(
        algorithm = "tSNE",
        parameters = list(
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
      )
    )
  }
) # /rtemis::tSNEParameters


# setup_tSNE ----
#' Setup tSNE parameters.
#'
#' @details
#' Get more information on the parameters by running `?Rtsne::Rtsne`.
#'
#' @param k Integer: Number of components.
#' @param initial_dims Integer: Initial dimensions.
#' @param perplexity Integer: Perplexity.
#' @param theta Float: Theta.
#' @param check_duplicates Logical: If TRUE, check for duplicates.
#' @param pca Logical: If TRUE, perform PCA.
#' @param partial_pca Logical: If TRUE, perform partial PCA.
#' @param max_iter Integer: Maximum number of iterations.
#' @param verbose Logical: If TRUE, print messages.
#' @param is_distance Logical: If TRUE, `x` is a distance matrix.
#' @param Y_init Matrix: Initial Y matrix.
#' @param pca_center Logical: If TRUE, center PCA.
#' @param pca_scale Logical: If TRUE, scale PCA.
#' @param normalize Logical: If TRUE, normalize.
#' @param stop_lying_iter Integer: Stop lying iterations.
#' @param mom_switch_iter Integer: Momentum switch iterations.
#' @param momentum Float: Momentum.
#' @param final_momentum Float: Final momentum.
#' @param eta Float: Eta.
#' @param exaggeration_factor Float: Exaggeration factor.
#' @param num_threads Integer: Number of threads.
#'
#' @return tSNEParameters object.
#'
#' @author EDG
#' @export
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
  stop_lying_iter = ifelse(is.null(Y_init), 250L, 0L),
  mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L),
  momentum = 0.5,
  final_momentum = 0.8,
  eta = 200,
  exaggeration_factor = 12,
  num_threads = 1L
) {
  tSNEParameters(
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


# IsomapParameters ----
#' @title IsomapParameters
#'
#' @description
#' DecompositionParameters subclass for Isomap.
#'
#' @author EDG
#' @noRd
IsomapParameters <- new_class(
  name = "IsomapParameters",
  parent = DecompositionParameters,
  constructor = function(
    k,
    dist_method = NULL,
    nsd = NULL,
    path = NULL
  ) {
    k <- clean_posint(k)
    check_inherits(dist_method, "character")
    nsd <- clean_int(nsd)
    check_inherits(path, "character")
    new_object(
      DecompositionParameters(
        algorithm = "Isomap",
        parameters = list(
          k = k,
          dist_method = dist_method,
          nsd = nsd,
          path = path
        )
      )
    )
  }
) # /rtemis::IsomapParameters


# setup_Isomap ----
#' Setup Isomap parameters.
#'
#' @param k Integer: Number of components.
#' @param dist_method Character: Distance method.
#' @param nsd Integer: Number of shortest dissimilarities retained.
#' @param path Character: Path argument for `vegan::isomap`.
#'
#' @return IsomapParameters object.
#'
#' @author EDG
#' @export
setup_Isomap <- function(
  k = 2L,
  dist_method = c("euclidean", "manhattan"),
  nsd = 0L,
  path = c("shortest", "extended")
) {
  k <- clean_posint(k)
  dist_method <- match.arg(dist_method)
  nsd <- clean_int(nsd)
  path <- match.arg(path)
  IsomapParameters(k, dist_method, nsd, path)
} # /rtemis::setup_Isomap
