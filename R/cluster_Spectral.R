# cluster_Spectral.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://rdrr.io/cran/kernlab/man/specc.html

# %% specc_kernel_args ----
#' The `kernel`/`kpar` pair one rtemis kernel setting maps to
#'
#' `kernlab::specc()` takes the kernel as two arguments that are not
#' independent: `kpar = "automatic"` estimates a Gaussian width and `kpar =
#' "local"` builds a per-case one, and both discard whatever `kernel` names.
#' `SpectralConfig` therefore publishes one setting, and this is where it
#' becomes the pair. `mod.sample` is added by the caller, since only the
#' estimating branch reads it.
#'
#' @param kernel Character: `SpectralConfig@kernel`.
#' @param sigma Numeric or NULL: `SpectralConfig@sigma`.
#'
#' @return Named list: The `kernel` and `kpar` arguments to pass on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
specc_kernel_args <- function(kernel, sigma) {
  if (identical(kernel, "rbf_local")) {
    # `kpar = "local"` is the whole specification: it builds the kernel itself
    # and reads no width, so `kernel` is deliberately not passed.
    return(list(kpar = "local"))
  }
  if (identical(kernel, "rbf") && is.null(sigma)) {
    return(list(kernel = "rbfdot", kpar = "automatic"))
  }
  backend_kernel <- switch(kernel, rbf = "rbfdot", laplace = "laplacedot")
  # An empty `kpar` is how the backend's own default width is asked for: it
  # constructs the kernel with `do.call(kernel, kpar)`.
  list(
    kernel = backend_kernel,
    kpar = if (is.null(sigma)) list() else list(sigma = sigma)
  )
} # /rtemis::specc_kernel_args


# %% cluster_.SpectralConfig ----
#' Spectral clustering
#'
#' @keywords internal
#' @noRd
method(cluster_, SpectralConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, SpectralConfig)

  # Dependencies ----
  check_dependencies("kernlab")

  # Data ----
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Cluster ----
  msg("Clustering with", config@algorithm, "...", verbosity = verbosity)
  kernel <- config[["kernel"]]
  sigma <- config[["sigma"]]
  args <- c(
    list(
      # `specc()` is S4-dispatched, and a data.frame is a list, so it reaches
      # the `list` method -- which clusters the *columns* as strings and
      # returns a well-formed `specc` holding one label per column. The
      # coercion is what selects the matrix method, and it is lossless because
      # `check_unsupervised_data()` has rejected non-numeric columns.
      x = as.matrix(x),
      centers = config[["k"]],
      iterations = config[["iterations"]],
      nystrom.red = config[["nystrom"]]
    ),
    specc_kernel_args(kernel, sigma)
  )
  # Both are read only on one branch and have non-NULL backend defaults, so
  # each is passed only when set rather than forwarded as NULL, which the
  # backend would take as the value.
  if (identical(kernel, "rbf") && is.null(sigma)) {
    if (!is.null(config[["sigma_sample_fraction"]])) {
      args[["mod.sample"]] <- config[["sigma_sample_fraction"]]
    }
  }
  if (config[["nystrom"]] && !is.null(config[["nystrom_sample"]])) {
    args[["nystrom.sample"]] <- config[["nystrom_sample"]]
  }
  clust <- do.call(kernlab::specc, args)
  check_inherits(clust, "specc")
  clust
} # /rtemis::cluster_.SpectralConfig


# %% clustpredict_Spectral ----
#' clustpredict method for Spectral
#'
#' @author EDG
#' @keywords internal
#' @noRd
clustpredict_Spectral <- function(clust, newdata = NULL) {
  # The embedding is defined by an eigendecomposition of the training cases'
  # affinity matrix, and 'kernlab' offers no out-of-sample extension of it, so
  # new data is refused rather than silently ignored.
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
} # /rtemis::clustpredict_Spectral
