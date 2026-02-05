# decom_ICA.R
# ::rtemis::
# 2025 EDG rtemis.org

#' ICA Decomposition
#'
#' @keywords internal
#' @noRd
decom_ICA <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, ICAConfig)
  check_dependencies("fastICA")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
  decom <- fastICA::fastICA(
    X = as.matrix(x),
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
  list(decom = decom, transformed = decom[["S"]])
} # /rtemis::decom_ICA
