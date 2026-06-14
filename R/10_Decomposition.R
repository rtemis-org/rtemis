# S7_Decomposition.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% Decomposition ----
#' Decomposition
#'
#' @description
#' Decomposition class.
#'
#' @field algorithm Character: Algorithm name.
#' @field decom Any: Decomposition object.
#' @field config List: Algorithm-specific config.
#' @field decom: Decomposition model.
#' @field transformed: transformedransformed data, i.e. either a projection or an embedding of the input data.
#'
#' @author EDG
#' @noRd
Decomposition <- new_class(
  name = "Decomposition",
  package = "rtemis",
  properties = list(
    algorithm = class_character,
    config = DecompositionConfig,
    decom = class_any,
    transformed = class_any
  )
) # /rtemis::Decomposition


# %% `$`.Decomposition ----
# Make Decomposition properties `$`-accessible
method(`$`, Decomposition) <- function(x, name) {
  prop_names <- names(props(x))
  if (name %in% prop_names) {
    prop(x, name)
  } else {
    cli::cli_abort(paste0(
      "No property named '",
      name,
      "' in Decomposition object."
    ))
  }
}


# %% `.DollarNames`.Decomposition ----
method(`.DollarNames`, Decomposition) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}


# %% `[[`.Decomposition ----
# Make Decomposition@transformed `[[`-accessible
method(`[[`, Decomposition) <- function(x, index) {
  props(x, "transformed")[[index]]
}


# %% repr.Decomposition ----
method(repr, Decomposition) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      paste(x@algorithm, "Decomposition"),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(props(x)[-1], pad = pad, output_type = output_type)
  )
} # /rtemis::repr.Decomposition


# %% print.Decomposition ----
method(print, Decomposition) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Decomposition


# %% apply_decomp.Decomposition ----
#' Apply Decomposition to New Data
#'
#' Apply a fitted decomposition to new data for algorithms that support this.
#'
#' @param decom Decomposition object.
#' @param new_data Tabular data (data.frame, data.table, or tibble): New data to which the
#'   decomposition will be applied.
#' @param verbosity Integer: Verbosity level
#'
#' @return Transformed data: a matrix of the learned components for `new_data`.
#' @author EDG
#' @export
apply_decomp <- function(decom, new_data, verbosity = 1L) {
  check_is_S7(decom, Decomposition)
  if (!decom@algorithm %in% decom_algorithms_applicable) {
    cli::cli_abort(c(
      "{.val {decom@algorithm}} decomposition cannot be applied on new data.",
      "i" = "Algorithms that support application on new data: {.val {decom_algorithms_applicable}}."
    ))
  }
  apply_decomp_(
    config = decom@config,
    decom = decom@decom,
    new_data = new_data,
    verbosity = verbosity
  )
} # /rtemis::apply_decomp


# %% .list_to_DecompositionConfig ----
#' Convert a list to a DecompositionConfig object
#'
#' Internal function used by `rtemis.server` and `SuperConfig` deserialization
#' to reconstruct a `DecompositionConfig` object from a named list. The list
#' must carry an `algorithm` element naming a decomposition algorithm that can
#' be applied on new data (see `decom_algorithms_applicable`); the remaining
#' elements are passed to that algorithm's `setup_*` function.
#'
#' @param x Named list with an `algorithm` element plus algorithm-specific
#'   parameters, e.g. `list(algorithm = "PCA", k = 3L)`.
#'
#' @return A `DecompositionConfig` object (an algorithm-specific subclass).
#'
#' @author EDG
#' @keywords internal
#' @export
.list_to_DecompositionConfig <- function(x) {
  algorithm <- x[["algorithm"]]
  if (is.null(algorithm)) {
    cli::cli_abort(
      "{.arg algorithm} is required to build a DecompositionConfig."
    )
  }
  if (!decom_can_apply(algorithm)) {
    cli::cli_abort(c(
      "Decomposition algorithm {.val {algorithm}} cannot be applied on new data.",
      "i" = "Supported algorithms: {.val {decom_algorithms_applicable}}."
    ))
  }
  # Normalize casing and drop `algorithm` before forwarding to the setup fn.
  algorithm <- get_decom_name(algorithm)
  # Params may arrive flat (UI / server: `list(algorithm, k, ...)`) or nested
  # under `config` (S7_to_list serialization of a DecompositionConfig, as used
  # by the TOML round-trip).
  params <- if (is.list(x[["config"]])) {
    x[["config"]]
  } else {
    x[setdiff(names(x), "algorithm")]
  }
  do.call(get_decom_setup_fn(algorithm), params)
} # /rtemis::.list_to_DecompositionConfig
