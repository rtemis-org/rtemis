# decom_NMF.R
# ::rtemis::
# 2025 EDG rtemis.org

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
decom_NMF <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, NMFConfig)
  check_dependencies("NMF")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  xm <- as.matrix(x)
  args <- list(x = t(xm), rank = config[["k"]], nrun = config[["nrun"]])
  decom <- do_call(NMF::nmf, args)
  check_inherits(decom, "NMFfit")
  basis <- NMF::basis(decom)
  transformed <- xm %*% basis
  colnames(transformed) <- paste0("NMF_", seq_len(NCOL(transformed)))
  list(decom = decom, transformed = transformed)
} # /rtemis::decom_NMF
