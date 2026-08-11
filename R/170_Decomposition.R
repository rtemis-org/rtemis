# 170_Decomposition.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% Decomposition ----
#' Decomposition
#'
#' @description
#' Decomposition class.
#'
#' @field algorithm Character: Algorithm name.
#' @field config DecompositionConfig: Algorithm-specific config.
#' @field decom Any: Fitted decomposition model.
#' @field transformed Any: Transformed data, i.e. either a projection or an
#' embedding of the input data.
#' @field decompose_config Optional DecomposeConfig: The run's input.
#' @field data_fingerprint Optional DataFingerprint: Identity of the data the
#' decomposition was fitted on.
#' @field metrics Optional DecompositionMetrics: Metrics for the data the
#' decomposition was fitted on.
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
    transformed = class_any,
    # The run's input. `@config` above is the *algorithm* config; this is the
    # whole call, and only it can say what the run was asked to do. Assigned by
    # `decomp()`.
    decompose_config = NULL | DecomposeConfig,
    # Identity of the data fitted, assigned by `decomp()`. Read by
    # `provenance_of()` for the run record, and by `decomp_metrics()` to tell
    # whether the `x` it was handed is that data.
    data_fingerprint = NULL | DataFingerprint,
    # Metrics for the training data, assigned by `decomp()`. Out-of-sample
    # metrics need a second data matrix and so belong to `decomp_metrics()`.
    metrics = NULL | DecompositionMetrics
  )
) # /rtemis::Decomposition


# %% `$`.Decomposition ----
# Make Decomposition properties `$`-accessible
method(`$`, Decomposition) <- function(x, name) {
  prop_names <- names(props(x))
  if (name %in% prop_names) {
    prop(x, name)
  } else {
    rtemis.core::abort(
      "No property named '",
      name,
      "' in Decomposition object.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
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


# %% split_decomp_features ----
#' Split data into the columns a decomposition was fitted on and the rest
#'
#' A fit made with `config@features` learned on those columns only, so anything
#' replaying it has to transform exactly them: transforming a different matrix
#' would silently produce components that are not the fitted ones.
#'
#' @param decom `Decomposition` object.
#' @param new_data Tabular data.
#'
#' @return List with `selected`, the columns to transform, and `kept`, the
#' columns to pass through (`NULL` when the fit used every column).
#'
#' @author EDG
#' @keywords internal
#' @noRd
split_decomp_features <- function(decom, new_data) {
  new_data <- as.data.frame(new_data)
  features <- decom@config@features
  if (is.null(features)) {
    return(list(selected = new_data, kept = NULL))
  }
  missing_cols <- setdiff(features, names(new_data))
  if (length(missing_cols) > 0L) {
    rtemis.core::abort(
      "New data is missing ",
      length(missing_cols),
      " column(s) required by the decomposition.\n",
      "Missing: ",
      paste(missing_cols, collapse = ", "),
      ".",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  list(
    selected = new_data[, features, drop = FALSE],
    kept = new_data[, setdiff(names(new_data), features), drop = FALSE]
  )
} # /rtemis::split_decomp_features


# %% decomp_matrix ----
#' The numeric matrix a decomposition operates on
#'
#' `config@features` selects the columns and `as.matrix()` puts them in the form
#' every `decomp_` method works in, so this is the data a fit actually sees.
#'
#' Everything that has to agree about "the data this decomposition saw" goes
#' through here -- the fingerprint `decomp()` stores, the fingerprint
#' `decomp_metrics()` compares against it, and the matrix the metrics are
#' computed from. The matrix form is what makes the two fingerprints comparable:
#' an "object" hash is taken over serialized bytes, and a data.frame and the
#' data.table it came from carry their attributes in a different order, so they
#' hash differently while being `identical()`. Reducing to a matrix drops that
#' difference along with the row names.
#'
#' @param decom `Decomposition` object.
#' @param data Tabular data.
#'
#' @return Numeric matrix, cases by the features `decom` operates on.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decomp_matrix <- function(decom, data) {
  as.matrix(split_decomp_features(decom, data)[["selected"]])
} # /rtemis::decomp_matrix


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
#' @examples
#' iris_pca <- decomp(exc(iris, "Species"), algorithm = "PCA")
#' apply_decomp(iris_pca, exc(iris, "Species"))
apply_decomp <- function(decom, new_data, verbosity = 1L) {
  check_is_S7(decom, Decomposition)
  if (!decom@algorithm %in% decom_algorithms_applicable) {
    rtemis.core::abort(
      "'",
      decom@algorithm,
      "' decomposition cannot be applied on new data.\n",
      "Algorithms that support application on new data: ",
      paste(decom_algorithms_applicable, collapse = ", "),
      ".",
      class = "rtemis_unsupported_error"
    )
  }
  split <- split_decomp_features(decom, new_data)
  selected <- split[["selected"]]
  kept <- split[["kept"]]
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


# %% reconstruct.Decomposition ----
#' Reconstruct Data from a Decomposition
#'
#' Encode data with a fitted decomposition and decode it back, giving the
#' rank-`k` approximation of the input in its original units and column layout.
#'
#' @details
#' The round trip [apply_decomp] leaves half-done. The reconstruction is `x`
#' with everything outside the first `k` components removed, i.e. a denoised
#' `x`. The residual, `x - reconstruct(decom, x)`, is what the decomposition
#' could not represent: `rowMeans(residual^2)` scores how anomalous each case
#' is, and `colMeans(residual^2)` shows which variables the decomposition fails
#' to capture.
#'
#' Available for algorithms whose `invertible` trait is TRUE; see
#' [decomposition_traits]. tSNE, UMAP and Isomap embed without an inverse, so
#' there is no map back for them at all.
#'
#' The reconstruction is in the units of the data as it was passed to [decomp],
#' with any centering or scaling the backend applied internally undone, so it
#' can be compared with `x` directly. Columns the fit did not decompose (when
#' `decom@config@features` is set) pass through unchanged and column order is
#' preserved, so the result lines up cell for cell with `x`.
#'
#' @param decom `Decomposition` object.
#' @param x Tabular data (data.frame, data.table, or tibble): Data to
#' reconstruct. Its columns must match those `decom` was fitted on.
#' @param verbosity Integer: Verbosity level.
#'
#' @return A data.frame with the same columns as `x`, holding the
#' reconstruction of the decomposed features.
#'
#' @author EDG
#' @export
#' @examples
#' x <- exc(iris, "Species")
#' iris_pca <- decomp(x, algorithm = "PCA", config = setup_PCA(k = 2L))
#' reconstructed <- reconstruct(iris_pca, x)
#' # What the two components could not represent, per case:
#' head(rowMeans((as.matrix(x) - as.matrix(reconstructed))^2))
reconstruct <- function(decom, x, verbosity = 1L) {
  check_is_S7(decom, Decomposition)
  traits <- decomposition_traits(decom@algorithm)
  if (!traits[["invertible"]]) {
    rtemis.core::abort(
      "'",
      decom@algorithm,
      "' has no inverse map, so its components cannot be reconstructed.\n",
      "Algorithms that support reconstruction: ",
      paste(decom_algorithms_invertible, collapse = ", "),
      ".",
      class = "rtemis_unsupported_error"
    )
  }
  # Reconstructing means encoding `x` first, so an algorithm that inverts but
  # cannot project new data could only reconstruct what it was fitted on. No
  # registered algorithm is in that quadrant; the check is here because one
  # would otherwise fail inside `apply_decomp_` with no explanation.
  if (!traits[["can_apply"]]) {
    rtemis.core::abort(
      "'",
      decom@algorithm,
      "' cannot be applied to data, so there is nothing to reconstruct from.",
      class = "rtemis_unsupported_error"
    )
  }
  split <- split_decomp_features(decom, x)
  selected <- split[["selected"]]
  transformed <- apply_decomp_(
    config = decom@config,
    decom = decom@decom,
    new_data = selected,
    verbosity = verbosity
  )
  reconstructed <- as.data.frame(reconstruct_(
    config = decom@config,
    decom = decom@decom,
    transformed = as.matrix(transformed),
    x = selected,
    verbosity = verbosity
  ))
  names(reconstructed) <- names(selected)
  out <- as.data.frame(x)
  out[, names(reconstructed)] <- reconstructed
  out
} # /rtemis::reconstruct


# %% .list_to_DecompositionConfig ----
#' Convert a list to a DecompositionConfig object
#'
#' Internal function used by `rtemis.server` and `SuperConfig` deserialization
#' to reconstruct a `DecompositionConfig` object from a named list. The list
#' must carry an `algorithm` element naming a decomposition algorithm that can
#' be applied on new data (see `decom_algorithms_applicable`); the elements of
#' `config`, along with `features`, are passed to that algorithm's `setup_*`
#' function.
#'
#' @param x Named list with an `algorithm` element, algorithm-specific
#'   parameters nested under `config`, and optionally `features`, e.g.
#'   `list(algorithm = "PCA", config = list(k = 3L))`.
#'
#' @return A `DecompositionConfig` object (an algorithm-specific subclass).
#'
#' @author EDG
#' @keywords internal
#' @export
#' @examples
#' .list_to_DecompositionConfig(list(algorithm = "PCA", config = list(k = 3L)))
.list_to_DecompositionConfig <- function(x) {
  algorithm <- x[["algorithm"]]
  if (is.null(algorithm)) {
    rtemis.core::abort(
      "`algorithm` is required to build a DecompositionConfig.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!decom_can_apply(algorithm)) {
    rtemis.core::abort(
      "Decomposition algorithm '",
      algorithm,
      "' cannot be applied on new data.\n",
      "Supported algorithms: ",
      paste(decom_algorithms_applicable, collapse = ", "),
      ".",
      class = "rtemis_unsupported_error"
    )
  }
  # Normalize casing and drop `algorithm` before forwarding to the setup fn.
  algorithm <- get_decom_name(algorithm)
  # One shape: `{algorithm, config, features?}`, which is what the published
  # schema declares -- a flat `{algorithm, k, ...}` is rejected by it, so
  # accepting one here would take input the contract does not. `features` is a
  # sibling of `config`, so it is re-attached explicitly. `.drop_meta_keys()`
  # removes document metadata (e.g. `$schema`), which is not a setup arg.
  check_wire_keys(
    x,
    c("algorithm", "config", "features"),
    "decomposition config"
  )
  params <- c(
    .drop_meta_keys(x[["config"]]),
    if (!is.null(x[["features"]])) list(features = x[["features"]])
  )
  # `features` may arrive from the wire as a list of scalars (a JSON array parsed
  # without vector simplification); flatten it to a character vector so the
  # strict `setup_*` check accepts it.
  if (!is.null(params[["features"]])) {
    params[["features"]] <- as.character(
      unlist(params[["features"]], use.names = FALSE)
    )
  }
  setup_fn <- get_decom_setup_fn(algorithm)
  check_wire_keys(
    params,
    names(formals(setup_fn)),
    paste(algorithm, "decomposition")
  )
  do.call(setup_fn, params)
} # /rtemis::.list_to_DecompositionConfig
