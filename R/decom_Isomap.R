# decom_Isomap.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Isomap Decomposition
#'
#' @keywords internal
#' @noRd
decom_Isomap <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, IsomapConfig)
  check_dependencies("vegan")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  dst <- vegan::vegdist(x = x, method = config[["dist_method"]])
  decom <- vegan::isomap(
    dist = dst,
    ndim = config[["k"]],
    k = config[["nsd"]],
    path = config[["path"]]
  )
  check_inherits(decom, "isomap")
  list(decom = decom, transformed = decom[["points"]])
} # /rtemis::decom_Isomap
