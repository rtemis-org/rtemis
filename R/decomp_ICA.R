# decom_ICA.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% decomp_.ICAConfig ----
#' ICA Decomposition
#'
#' @keywords internal
#' @noRd
method(decomp_, ICAConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_dependencies("fastICA")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
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
#' centered (and, if `row_norm`, row-normalized) data matrix. We replicate that
#' pre-processing on `new_data` using the centering means stored at fit time.
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
  xm <- as.matrix(new_data)
  xm <- sweep(xm, 2L, decom[["rtemis_center"]], FUN = "-")
  if (config[["row_norm"]]) {
    xm <- xm / apply(xm, 1L, sd)
  }
  transformed <- xm %*% decom[["K"]] %*% decom[["W"]]
  colnames(transformed) <- paste0("ICA_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::apply_decomp_.ICAConfig
