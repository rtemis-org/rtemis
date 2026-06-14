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
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
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
