# decom_NMF.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% nmf_basis ----
#' Extract and validate an NMF basis matrix
#'
#' `NMF::nmf()` returns different classes depending on `nrun` and its options
#' (a single fit, or a multi-run object wrapping the best one). `NMF::basis()`
#' is defined for all of them, so the basis matrix -- the only part of the fit
#' this package uses -- is what gets validated, rather than the class of the
#' object carrying it.
#'
#' @param decom Fitted NMF object, as returned by `NMF::nmf()`.
#'
#' @return Numeric matrix: NMF basis, features by components.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nmf_basis <- function(decom) {
  basis <- NMF::basis(decom)
  if (!is.matrix(basis) || !is.numeric(basis)) {
    rtemis.core::abort(
      "Could not extract a numeric basis matrix from the NMF fit.",
      class = "rtemis_type_error"
    )
  }
  basis
} # /rtemis::nmf_basis


# %% nmf_scores ----
#' Non-negative coefficients of data on a fitted NMF basis
#'
#' Solves `min ||t(xm) - basis %*% h||_F` subject to `h >= 0` and returns
#' `t(h)`: one row per case, one column per component.
#'
#' `NMF::nmf()` factorizes a features-by-cases matrix as `basis %*% coef`, so
#' `coef` is indexed by the cases it was fitted on and says nothing about a case
#' the fit never saw. `basis` is the only factor that generalizes, and
#' recovering a case's coefficients from it is the non-negative least squares
#' problem solved here by `NMF::fcnnls()`.
#'
#' Fitting and application both route through this one function, so that
#' applying a fitted decomposition to its own training data reproduces
#' `Decomposition@transformed` exactly. Note that `xm %*% basis` is not a
#' substitute: substituting the factorization gives
#' `xm %*% basis ~= t(h) %*% (t(basis) %*% basis)`, so it equals the
#' coefficients only when the basis columns are orthonormal, which NMF does not
#' constrain them to be.
#'
#' `pseudo = FALSE` keeps the solve inside base R: `NMF::fcnnls()` attaches
#' `corpcor` when `pseudo = TRUE`, and `corpcor` is only Suggested by `NMF`, so
#' the default errors wherever it is absent. That solve is a Gaussian
#' elimination on Gram submatrices of the basis, so it requires the basis to
#' have full column rank -- checked here, because a rank-deficient basis also
#' leaves the non-negative coefficients unidentified.
#'
#' @param basis Numeric matrix: NMF basis, features by components.
#' @param xm Numeric matrix: Data, cases by features.
#'
#' @return Numeric matrix: Non-negative scores, cases by components.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nmf_scores <- function(basis, xm) {
  if (NCOL(xm) != NROW(basis)) {
    rtemis.core::abort(
      "Data has ",
      NCOL(xm),
      " feature(s), but the NMF basis was learned on ",
      NROW(basis),
      ".",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  rank <- qr(basis)[["rank"]]
  if (rank < NCOL(basis)) {
    rtemis.core::abort(
      "The fitted NMF basis has rank ",
      rank,
      " for ",
      NCOL(basis),
      " components, so component scores are not identified. ",
      "Set `k` to at most ",
      rank,
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  transformed <- t(NMF::fcnnls(x = basis, y = t(xm), pseudo = FALSE)[["x"]])
  colnames(transformed) <- paste0("NMF_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::nmf_scores


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
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)

  # Decompose ----
  msg("Decomposing with", config@algorithm, "...", verbosity = verbosity)
  xm <- as.matrix(x)
  # `NMF::nmf()` takes features in rows and cases in columns.
  args <- list(
    x = t(xm),
    rank = config[["k"]],
    method = config[["method"]],
    nrun = config[["nrun"]]
  )
  decom <- do_call(NMF::nmf, args)
  list(decom = decom, transformed = nmf_scores(nmf_basis(decom), xm))
} # /rtemis::decomp_.NMFConfig


# %% apply_decomp_.NMFConfig ----
#' Apply a fitted NMF decomposition to new data
#'
#' @details
#' Solves for each new case's non-negative coefficients on the learned basis,
#' the same operation that produced the training `transformed` matrix.
#'
#' @param config `NMFConfig` object.
#' @param decom Fitted NMF object.
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
  nmf_scores(nmf_basis(decom), as.matrix(new_data))
} # /rtemis::apply_decomp_.NMFConfig
