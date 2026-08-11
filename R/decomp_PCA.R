# decom_PCA.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% decomp_.PCAConfig ----
#' PCA Decomposition
#'
#' @keywords internal
#' @noRd
method(decomp_, PCAConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, PCAConfig)
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Decompose ----
  msg("Decomposing with", config@algorithm, "...", verbosity = verbosity)
  decom <- prcomp(
    x = x,
    center = config[["center"]],
    scale. = config[["scale"]],
    tol = config[["tol"]],
    rank. = config[["k"]]
  )
  check_inherits(decom, "prcomp")
  list(decom = decom, transformed = decom[["x"]])
} # /rtemis::decomp_.PCAConfig


# %% apply_decomp_.PCAConfig ----
#' Apply a fitted PCA decomposition to new data
#'
#' @param config `PCAConfig` object.
#' @param decom Fitted `prcomp` object.
#' @param new_data Tabular data: New data to project onto the principal components.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Matrix of principal component scores.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, PCAConfig) <- function(
  config,
  decom,
  new_data,
  verbosity = 1L
) {
  check_inherits(decom, "prcomp")
  # `predict.prcomp` re-applies the learned centering, scaling, and rotation.
  stats::predict(decom, newdata = as.data.frame(new_data))
} # /rtemis::apply_decomp_.PCAConfig


# %% reconstruct_.PCAConfig ----
#' Map principal components back to input space
#'
#' @details
#' Rotates the scores back through the loadings, then undoes the scaling and
#' centering `prcomp()` applied, so the result is in the units of the data as it
#' was handed to `decomp()`. `prcomp()` stores `center` and `scale` as `FALSE`
#' when it did not apply them.
#'
#' @param config `PCAConfig` object.
#' @param decom Fitted `prcomp` object.
#' @param transformed Numeric matrix: Component scores, cases by components.
#' @param x Tabular data: Unused; PCA's preprocessing is recoverable from the
#' fit.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix: Reconstruction in input units, cases by features.
#'
#' @keywords internal
#' @noRd
method(reconstruct_, PCAConfig) <- function(
  config,
  decom,
  transformed,
  x,
  verbosity = 1L
) {
  check_inherits(decom, "prcomp")
  reconstructed <- as.matrix(transformed) %*% t(decom[["rotation"]])
  if (!identical(decom[["scale"]], FALSE)) {
    reconstructed <- sweep(reconstructed, 2L, decom[["scale"]], FUN = "*")
  }
  if (!identical(decom[["center"]], FALSE)) {
    reconstructed <- sweep(reconstructed, 2L, decom[["center"]], FUN = "+")
  }
  reconstructed
} # /rtemis::reconstruct_.PCAConfig
