# decom_ICA.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% ica_preprocess ----
#' Reproduce fastICA's internal preprocessing
#'
#' fastICA centers the columns of the data matrix and, when `row.norm` is set,
#' additionally standardizes each row -- subtracting that case's mean across
#' features and dividing by its standard deviation. The source matrix is
#' `S = X_preprocessed %*% K %*% W`, so applying a fit to new data has to
#' reproduce this exactly, and reconstructing has to undo it.
#'
#' The row statistics are returned because they cannot be recovered afterwards:
#' they are per-case, so they belong to the data being transformed rather than
#' to the fit, and `reconstruct_` needs them to get back to input units.
#'
#' @param xm Numeric matrix: Data, cases by features.
#' @param center Numeric vector: Column means learned at fit time.
#' @param row_norm Logical: Whether to standardize each case across features.
#'
#' @return List with `x`, the preprocessed matrix, and `row_center` and
#' `row_scale`, the per-case statistics or `NULL` when `row_norm` is FALSE.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ica_preprocess <- function(xm, center, row_norm) {
  xm <- sweep(xm, 2L, center, FUN = "-")
  if (!row_norm) {
    return(list(x = xm, row_center = NULL, row_scale = NULL))
  }
  row_center <- rowMeans(xm)
  row_scale <- apply(xm, 1L, stats::sd)
  # Both vectors have one element per case and `xm` is column-major, so these
  # recycle down each column, i.e. per row of `xm`.
  list(
    x = (xm - row_center) / row_scale,
    row_center = row_center,
    row_scale = row_scale
  )
} # /rtemis::ica_preprocess


# %% decomp_.ICAConfig ----
#' ICA Decomposition
#'
#' @keywords internal
#' @noRd
method(decomp_, ICAConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_dependencies("fastICA")
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Decompose ----
  msg("Decomposing with", config@algorithm, "...", verbosity = verbosity)
  xm <- as.matrix(x)
  decom <- fastICA::fastICA(
    X = xm,
    n.comp = config[["k"]],
    alg.typ = config[["type"]],
    fun = config[["fun"]],
    alpha = config[["alpha"]],
    method = "C",
    row.norm = config[["row_norm"]],
    maxit = config[["maxit"]],
    tol = config[["tol"]],
    verbose = verbosity > 0L
  )
  check_inherits(decom, "list")
  # Store the column means used by fastICA for centering so the same projection
  # (S = X_centered %*% K %*% W) can be applied to new data via apply_decomp().
  decom[["rtemis_center"]] <- colMeans(xm)
  transformed <- decom[["S"]]
  colnames(transformed) <- paste0("ICA_", seq_len(NCOL(transformed)))
  list(decom = decom, transformed = transformed)
} # /rtemis::decomp_.ICAConfig


# %% apply_decomp_.ICAConfig ----
#' Apply a fitted ICA decomposition to new data
#'
#' @details
#' fastICA computes the source matrix as `S = X %*% K %*% W`, where `X` is the
#' preprocessed data matrix. `ica_preprocess()` reproduces that preprocessing on
#' `new_data`, using the centering means stored at fit time.
#'
#' @param config `ICAConfig` object.
#' @param decom Fitted fastICA list (with `rtemis_center` appended).
#' @param new_data Tabular data: New data to project onto the components.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Matrix of independent component scores.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, ICAConfig) <- function(
  config,
  decom,
  new_data,
  verbosity = 1L
) {
  xm <- ica_preprocess(
    as.matrix(new_data),
    center = decom[["rtemis_center"]],
    row_norm = config[["row_norm"]]
  )[["x"]]
  transformed <- xm %*% decom[["K"]] %*% decom[["W"]]
  colnames(transformed) <- paste0("ICA_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::apply_decomp_.ICAConfig


# %% reconstruct_.ICAConfig ----
#' Map ICA components back to input space
#'
#' @details
#' fastICA's mixing matrix `A` inverts the unmixing, so `transformed %*% A` is
#' the preprocessed data matrix. Undoing the preprocessing then requires the
#' per-case statistics of the data being reconstructed, which is why `x` is
#' needed under `row_norm`: each case's mean and standard deviation across
#' features were divided out and are not carried by the components.
#'
#' @param config `ICAConfig` object.
#' @param decom Fitted fastICA list (with `rtemis_center` appended).
#' @param transformed Numeric matrix: Component scores, cases by components.
#' @param x Tabular data: The data being reconstructed, in input units.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix: Reconstruction in input units, cases by features.
#'
#' @keywords internal
#' @noRd
method(reconstruct_, ICAConfig) <- function(
  config,
  decom,
  transformed,
  x,
  verbosity = 1L
) {
  reconstructed <- as.matrix(transformed) %*% decom[["A"]]
  if (config[["row_norm"]]) {
    xm <- as.matrix(x)
    if (NROW(xm) != NROW(reconstructed)) {
      rtemis.core::abort(
        "`x` has ",
        NROW(xm),
        " case(s) but `transformed` has ",
        NROW(reconstructed),
        ". Under `row_norm`, reconstruction needs the per-case statistics of ",
        "the same cases the components describe.",
        class = c("rtemis_dim_error", "rtemis_data_error")
      )
    }
    row_stats <- ica_preprocess(
      xm,
      center = decom[["rtemis_center"]],
      row_norm = TRUE
    )
    reconstructed <- reconstructed *
      row_stats[["row_scale"]] +
      row_stats[["row_center"]]
  }
  sweep(reconstructed, 2L, decom[["rtemis_center"]], FUN = "+")
} # /rtemis::reconstruct_.ICAConfig
