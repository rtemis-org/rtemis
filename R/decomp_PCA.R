# decom_PCA.R
# ::rtemis::
# 2025 EDG rtemis.org

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
