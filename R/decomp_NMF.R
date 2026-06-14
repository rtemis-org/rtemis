# decom_NMF.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% decomp_.NMFConfig ----
#' Non-negative Matrix Factorization (NMF)
#'
#' Decomposes a data matrix into non-negative factors using NMF.
#'
#' @param x A numeric matrix or data frame to be decomposed.
#' @param config `NMFConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return A list containing the decomposition and transformed data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decomp_, NMFConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, NMFConfig)
  check_dependencies("NMF")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
  xm <- as.matrix(x)
  args <- list(x = t(xm), rank = config[["k"]], nrun = config[["nrun"]])
  decom <- do_call(NMF::nmf, args)
  check_inherits(decom, "NMFfit")
  basis <- NMF::basis(decom)
  transformed <- xm %*% basis
  colnames(transformed) <- paste0("NMF_", seq_len(NCOL(transformed)))
  list(decom = decom, transformed = transformed)
} # /rtemis::decomp_.NMFConfig


# %% apply_decomp_.NMFConfig ----
#' Apply a fitted NMF decomposition to new data
#'
#' @details
#' Projects `new_data` onto the learned non-negative basis: `new_data %*% basis`,
#' the same projection used to produce the training `transformed` matrix.
#'
#' @param config `NMFConfig` object.
#' @param decom Fitted `NMFfit` object.
#' @param new_data Tabular data: New data to project onto the basis.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Matrix of NMF component scores.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, NMFConfig) <- function(
  config,
  decom,
  new_data,
  verbosity = 1L
) {
  check_dependencies("NMF")
  check_inherits(decom, "NMFfit")
  basis <- NMF::basis(decom)
  transformed <- as.matrix(new_data) %*% basis
  colnames(transformed) <- paste0("NMF_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::apply_decomp_.NMFConfig
