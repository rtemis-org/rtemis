# decom_PCA.R
# ::rtemis::
# 2025 EDG rtemis.org

#' PCA Decomposition
#'
#' @keywords internal
#' @noRd
decom_PCA <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, PCAConfig)
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  decom <- prcomp(
    x = x,
    center = config[["center"]],
    scale. = config[["scale"]],
    tol = config[["tol"]],
    rank. = config[["k"]]
  )
  check_inherits(decom, "prcomp")
  list(decom = decom, transformed = decom[["x"]])
} # /rtemis::decom_PCA
