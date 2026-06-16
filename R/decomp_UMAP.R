# decom_UMAP.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% decomp_.UMAPConfig ----
#' UMAP Decomposition
#'
#' @param x A numeric matrix or data frame to be decomposed.
#' @param config `UMAPConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return A list containing the decomposition and transformed data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decomp_, UMAPConfig) <- function(config, x, verbosity = 1L) {
  # Checks ----
  check_is_S7(config, UMAPConfig)
  check_dependencies("uwot")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  if (verbosity > 0L) {
    msg("Decomposing with", config@algorithm, "...")
  }
  args <- c(
    list(X = x, n_components = config[["k"]], ret_model = TRUE),
    config@config
  )
  args[["k"]] <- NULL
  decom <- do_call(
    uwot::umap,
    args,
    error_pattern_suggestion = list(
      "as_cholmod_sparse" = "Try installing packages 'Matrix' and 'irlba' from source."
    )
  )
  # ret_model = TRUE returns list
  check_inherits(decom, "list")
  list(decom = decom, transformed = decom[["embedding"]])
} # /rtemis::decomp_.UMAPConfig


# %% apply_decomp_.UMAPConfig ----
#' Apply a fitted UMAP decomposition to new data
#'
#' @details
#' Uses `uwot::umap_transform()`, which requires the model to have been fit with
#' `ret_model = TRUE` (as done in `decomp_.UMAPConfig`).
#'
#' @param config `UMAPConfig` object.
#' @param decom Fitted uwot UMAP model (list).
#' @param new_data Tabular data: New data to embed.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Matrix of UMAP embedding coordinates.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, UMAPConfig) <- function(
  config,
  decom,
  new_data,
  verbosity = 1L
) {
  check_dependencies("uwot")
  uwot::umap_transform(X = new_data, model = decom)
} # /rtemis::apply_decomp_.UMAPConfig
