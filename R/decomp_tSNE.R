# decom_tSNE.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% decomp_.tSNEConfig ----
#' tSNE Decomposition
#'
#' @keywords internal
#' @noRd
method(decomp_, tSNEConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, tSNEConfig)
  check_dependencies("Rtsne")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
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
} # /rtemis::decomp_.tSNEConfig
