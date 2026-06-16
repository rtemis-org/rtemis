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
#' @details
#' When the fitted decomposition was learned on a subset of the features (i.e.
#' `decom@config@features` is not `NULL`), only those columns of `new_data` are
#' transformed; the remaining columns are returned unchanged, alongside the
#' learned components, in the layout `[kept features, components]`. When
#' `features` is `NULL` (the standalone default), all columns of `new_data` are
#' decomposed and only the components are returned.
#'
#' @return A data.frame of the learned components for `new_data`, preceded by any
#' feature columns that were not decomposed.
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
  new_data <- as.data.frame(new_data)
  features <- decom@config@features
  if (is.null(features)) {
    selected <- new_data
    kept <- NULL
  } else {
    missing_cols <- setdiff(features, names(new_data))
    if (length(missing_cols) > 0L) {
      cli::cli_abort(c(
        "New data is missing {length(missing_cols)} column{?s} required by the decomposition.",
        "x" = "Missing: {.val {missing_cols}}."
      ))
    }
    selected <- new_data[, features, drop = FALSE]
    kept <- new_data[, setdiff(names(new_data), features), drop = FALSE]
  }
  transformed <- as.data.frame(apply_decomp_(
    config = decom@config,
    decom = decom@decom,
    new_data = selected,
    verbosity = verbosity
  ))
  if (is.null(kept) || ncol(kept) == 0L) {
    transformed
  } else {
    cbind(kept, transformed)
  }
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
  # Params may arrive flat (UI / server: `list(algorithm, k, ..., features)`) or
  # nested under `config` (S7_to_list serialization of a DecompositionConfig, as
  # used by the TOML round-trip). In the nested shape `features` is a sibling of
  # `config`, so it is re-attached explicitly.
  params <- if (is.list(x[["config"]])) {
    c(
      x[["config"]],
      if (!is.null(x[["features"]])) list(features = x[["features"]])
    )
  } else {
    x[setdiff(names(x), "algorithm")]
  }
  # `features` may arrive from the wire as a list of scalars (a JSON array parsed
  # without vector simplification); flatten it to a character vector so the
  # strict `setup_*` check accepts it.
  if (!is.null(params[["features"]])) {
    params[["features"]] <- as.character(
      unlist(params[["features"]], use.names = FALSE)
    )
  }
  do.call(get_decom_setup_fn(algorithm), params)
} # /rtemis::.list_to_DecompositionConfig
