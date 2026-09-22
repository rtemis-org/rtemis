# cluster_Spectral.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://rdrr.io/cran/kernlab/man/specc.html

# %% cluster_specc ----
#' Fit `kernlab::specc()` for one spectral variant
#'
#' The three variants differ only in how the kernel is specified, so the fit
#' is one function taking the backend's `kernel`/`kpar` pair and the width
#' search's sample fraction, with the variant supplying them.
#'
#' `specc()` is S4-dispatched, and a data.frame is a list, so it would reach the
#' `list` method -- which clusters the *columns* as strings. The matrix coercion
#' selects the matrix method, and it is lossless because
#' `check_unsupervised_data()` has rejected non-numeric columns.
#'
#' @param config `ClusteringConfig`: One of the spectral variants; `k`,
#'   `iterations` and (where the variant declares it) `nystrom` are read here.
#' @param x Data frame or matrix: Numeric features.
#' @param kernel_args Named list: The backend's `kernel` and `kpar` arguments.
#' @param mod_sample Numeric or NULL: `mod.sample`, passed only when set.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `specc` object.
#' @keywords internal
#' @noRd
cluster_specc <- function(config, x, kernel_args, mod_sample, verbosity) {
  # Dependencies ----
  check_dependencies("kernlab")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  nystrom <- if ("nystrom" %in% names(S7_class(config)@properties)) {
    prop(config, "nystrom")
  }
  args <- c(
    list(
      x = as.matrix(x),
      centers = config[["k"]],
      iterations = config[["iterations"]],
      nystrom.red = !is.null(nystrom)
    ),
    kernel_args
  )
  # Both have non-NULL backend defaults, so each is passed only when set rather
  # than forwarded as NULL, which the backend would take as the value.
  if (!is.null(mod_sample)) {
    args[["mod.sample"]] <- mod_sample
  }
  if (!is.null(nystrom) && !is.null(nystrom@sample)) {
    args[["nystrom.sample"]] <- nystrom@sample
  }
  clust <- do.call(kernlab::specc, args)
  check_inherits(clust, "specc")
  clust
} # /rtemis::cluster_specc


# %% cluster_.SpectralRBFConfig ----
#' Spectral clustering with a Gaussian kernel of one shared width
#'
#' `kpar = "automatic"` asks the backend to search for the width; when a width
#' is given it is passed as `kpar = list(sigma)`. The search's sample fraction
#' is read only on the search branch.
#'
#' @keywords internal
#' @noRd
method(cluster_, SpectralRBFConfig) <- function(config, x, verbosity = 1L) {
  check_is_S7(config, SpectralRBFConfig)
  sigma <- config[["sigma"]]
  if (is.null(sigma)) {
    cluster_specc(
      config,
      x,
      kernel_args = list(kernel = "rbfdot", kpar = "automatic"),
      mod_sample = config[["sigma_sample_fraction"]],
      verbosity = verbosity
    )
  } else {
    cluster_specc(
      config,
      x,
      kernel_args = list(kernel = "rbfdot", kpar = list(sigma = sigma)),
      mod_sample = NULL,
      verbosity = verbosity
    )
  }
} # /rtemis::cluster_.SpectralRBFConfig


# %% cluster_.SpectralLaplaceConfig ----
#' Spectral clustering with an exponential kernel
#'
#' An empty `kpar` is how the backend's own default width (1) is asked for: it
#' constructs the kernel with `do.call(kernel, kpar)`.
#'
#' @keywords internal
#' @noRd
method(cluster_, SpectralLaplaceConfig) <- function(config, x, verbosity = 1L) {
  check_is_S7(config, SpectralLaplaceConfig)
  sigma <- config[["sigma"]]
  cluster_specc(
    config,
    x,
    kernel_args = list(
      kernel = "laplacedot",
      kpar = if (is.null(sigma)) list() else list(sigma = sigma)
    ),
    mod_sample = NULL,
    verbosity = verbosity
  )
} # /rtemis::cluster_.SpectralLaplaceConfig


# %% cluster_.SpectralLocalConfig ----
#' Spectral clustering with a per-case kernel width
#'
#' `kpar = "local"` is the whole specification: the backend builds the kernel
#' itself from each case's seventh-nearest-neighbor distance and reads no
#' width, so `kernel` is deliberately not passed.
#'
#' @keywords internal
#' @noRd
method(cluster_, SpectralLocalConfig) <- function(config, x, verbosity = 1L) {
  check_is_S7(config, SpectralLocalConfig)
  cluster_specc(
    config,
    x,
    kernel_args = list(kpar = "local"),
    mod_sample = NULL,
    verbosity = verbosity
  )
} # /rtemis::cluster_.SpectralLocalConfig


# %% clustpredict_SpectralRBF ----
#' Cluster labels of a fitted `specc`
#'
#' The embedding is defined by an eigendecomposition of the training cases'
#' affinity matrix, and 'kernlab' offers no out-of-sample extension of it, so
#' new data is refused rather than silently ignored. One reading serves the
#' three variants, each under the name its algorithm resolves to.
#'
#' @keywords internal
#' @noRd
clustpredict_SpectralRBF <- function(clust, newdata = NULL) {
  if (!is.null(newdata)) {
    rtemis.core::abort(
      "Spectral clustering cannot assign new data to fitted clusters.",
      class = c("rtemis_unsupported_error", "rtemis_error")
    )
  }
  check_inherits(clust, "specc")
  # `specc` holds the k-means assignment over the embedding in its `.Data`
  # part, already 1:k. It carries the input's row names, which are not data.
  as.integer(clust@.Data)
} # /rtemis::clustpredict_SpectralRBF

# %% clustpredict_SpectralLaplace ----
#' @keywords internal
#' @noRd
clustpredict_SpectralLaplace <- clustpredict_SpectralRBF

# %% clustpredict_SpectralLocal ----
#' @keywords internal
#' @noRd
clustpredict_SpectralLocal <- clustpredict_SpectralRBF
