# decom_tSNE.R
# ::rtemis::
# 2025 EDG rtemis.org

#' tSNE Decomposition
#'
#' @keywords internal
#' @noRd
decom_tSNE <- function(x, config, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, tSNEConfig)
  check_dependencies("Rtsne")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  args <- c(list(X = x, dims = config[["k"]]), config@config)
  args[["k"]] <- NULL
  decom <- do_call(
    Rtsne::Rtsne,
    args,
    error_pattern_suggestion = list(
      "Remove duplicates" = "Remove duplicates using `preprocess()"
    )
  )
  check_inherits(decom, "Rtsne")
  list(decom = decom, transformed = decom[["Y"]])
} # /rtemis::decom_tSNE
