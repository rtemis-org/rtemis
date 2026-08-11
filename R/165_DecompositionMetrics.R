# 165_DecompositionMetrics.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% decomposition_metric_columns ----
# The decomposition metric set, declared once. Every column is nullable and
# every column is declared for every algorithm: which ones are *populated*
# follows from the algorithm's traits, via `applicable_metrics()`. Declaring
# them all is what keeps the table schema-expressible; a column that an
# algorithm cannot support is NA, not a missing column.
decomposition_metric_columns <- function() {
  list(
    explained_variance_ratio = prop_metric(
      max = 1,
      description = paste0(
        "Share of the data's variance the reconstruction accounts for, ",
        "1 - ||x - xhat||^2 / ||x - colMeans(x)||^2. The generalization of ",
        "PCA's cumulative explained variance to any invertible method."
      )
    ),
    relative_reconstruction_error = prop_metric(
      min = 0,
      description = paste0(
        "Frobenius norm of the reconstruction residual over that of the data, ",
        "||x - xhat|| / ||x||. Scale-free, so it compares across datasets and ",
        "algorithms."
      )
    ),
    reconstruction_rmse = prop_metric(
      min = 0,
      description = paste0(
        "Root mean squared reconstruction error, in the units of the data as ",
        "it was passed to `decomp()`."
      )
    ),
    max_abs_component_correlation = prop_metric(
      min = 0,
      max = 1,
      description = paste0(
        "Largest absolute correlation between two distinct components. ",
        "Approximately 0 for PCA by construction; a high value means the ",
        "components carry redundant information. NA for a single component."
      )
    ),
    effective_dimensionality = prop_metric(
      min = 1,
      description = paste0(
        "Participation ratio of the component variances, ",
        "sum(v)^2 / sum(v^2). Lies in [1, k]; a value well below k means some ",
        "components carry almost no variance and k is larger than the data ",
        "supports."
      )
    ),
    oos_explained_variance_ratio = prop_metric(
      max = 1,
      description = "Explained variance ratio on data the fit never saw."
    ),
    oos_relative_reconstruction_error = prop_metric(
      min = 0,
      description = "Relative reconstruction error on data the fit never saw."
    ),
    reconstruction_gap = prop_metric(
      description = paste0(
        "Out-of-sample minus in-sample relative reconstruction error. The ",
        "overfitting signal: approximately 0 when the decomposition ",
        "generalizes, positive when it has fitted the training cases."
      )
    )
  )
} # /rtemis::decomposition_metric_columns


# %% DecompositionMetrics ----
#' @title DecompositionMetrics
#'
#' @description
#' Metrics subclass for decompositions.
#'
#' The unprefixed columns describe the data the decomposition was fitted on;
#' the `oos_` columns describe the new data passed to `decomp_metrics()`, and
#' are NULL unless it was.
#'
#' @author EDG
#' @noRd
DecompositionMetrics <- new_class(
  name = "DecompositionMetrics",
  parent = Metrics,
  properties = list(
    metrics = prop_state(prop_table(
      columns = decomposition_metric_columns(),
      nullable = TRUE,
      description = "Decomposition metrics, one row."
    ))
  ),
  constructor = function(
    explained_variance_ratio = NA_real_,
    relative_reconstruction_error = NA_real_,
    reconstruction_rmse = NA_real_,
    max_abs_component_correlation = NA_real_,
    effective_dimensionality = NA_real_,
    oos_explained_variance_ratio = NA_real_,
    oos_relative_reconstruction_error = NA_real_,
    reconstruction_gap = NA_real_,
    sample = NULL
  ) {
    new_object(
      Metrics(sample = sample),
      metrics = data.frame(
        explained_variance_ratio = explained_variance_ratio,
        relative_reconstruction_error = relative_reconstruction_error,
        reconstruction_rmse = reconstruction_rmse,
        max_abs_component_correlation = max_abs_component_correlation,
        effective_dimensionality = effective_dimensionality,
        oos_explained_variance_ratio = oos_explained_variance_ratio,
        oos_relative_reconstruction_error = oos_relative_reconstruction_error,
        reconstruction_gap = reconstruction_gap
      )
    )
  }
) # /rtemis::DecompositionMetrics


# %% repr.DecompositionMetrics ----
method(repr, DecompositionMetrics) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  out <- if (!is.null(x@sample)) {
    repr_S7name(
      paste(x@sample, "Decomposition Metrics"),
      pad = pad,
      output_type = output_type
    )
  } else {
    repr_S7name("Decomposition Metrics", pad = pad, output_type = output_type)
  }
  # A metric that does not apply to this algorithm is NA for every case, and
  # printing a column of NAs states nothing. The stored table keeps them, so
  # the schema and the record stay complete either way.
  populated <- x@metrics[, !is.na(unlist(x@metrics)), drop = FALSE]
  paste0(
    out,
    repr_ls(
      label_metric_df(populated),
      print_class = FALSE,
      print_df = TRUE,
      pad = pad,
      output_type = output_type
    )
  )
} # /rtemis::repr.DecompositionMetrics


# %% print.DecompositionMetrics ----
method(print, DecompositionMetrics) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.DecompositionMetrics
