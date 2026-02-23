# decom_Isomap.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% decomp_.IsomapConfig ----
#' Isomap Decomposition
#'
#' @keywords internal
#' @noRd
method(decomp_, IsomapConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_dependencies("vegan")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
  dst <- vegan::vegdist(x = x, method = config[["dist_method"]])
  decom <- vegan::isomap(
    dist = dst,
    ndim = config[["k"]],
    k = config[["nsd"]],
    path = config[["path"]]
  )
  check_inherits(decom, "isomap")
  list(decom = decom, transformed = decom[["points"]])
} # /rtemis::decomp_.IsomapConfig
