# decomp_metrics.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% decom_metrics ----
# One row per metric. `requires` names the trait columns of `decom_algorithms`
# that must all be TRUE for the metric to be defined, which is what makes
# applicability *derived* rather than tabulated: a new algorithm's row of traits
# determines its metrics with no edit here, and no metric-by-algorithm matrix
# can fall out of step with the code.
#
# `requires` is a list column because a metric can name any number of traits,
# including none.
decom_metrics <- data.frame(
  name = c(
    "explained_variance_ratio",
    "relative_reconstruction_error",
    "reconstruction_rmse",
    "max_abs_component_correlation",
    "effective_dimensionality",
    "oos_explained_variance_ratio",
    "oos_relative_reconstruction_error",
    "reconstruction_gap"
  ),
  group = c(
    "reconstruction",
    "reconstruction",
    "reconstruction",
    "components",
    "components",
    "generalization",
    "generalization",
    "generalization"
  ),
  requires = I(list(
    "invertible",
    "invertible",
    "invertible",
    character(),
    character(),
    c("can_apply", "invertible"),
    c("can_apply", "invertible"),
    c("can_apply", "invertible")
  )),
  # Whether the metric needs data the fit never saw. The ones that do are the
  # only reason `decomp_metrics()` exists beside the set `decomp()` computes.
  needs_new_data = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE
  ),
  higher_is_better = c(
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    NA,
    TRUE,
    FALSE,
    FALSE
  ),
  range = c(
    "(-Inf, 1]",
    "[0, Inf)",
    "[0, Inf)",
    "[0, 1]",
    "[1, k]",
    "(-Inf, 1]",
    "[0, Inf)",
    "(-Inf, Inf)"
  ),
  stringsAsFactors = FALSE
)


# %% applicable_metrics ----
#' Which Decomposition Metrics Apply to Which Algorithm
#'
#' Derived from [decomposition_traits] and the metric registry: a metric applies
#' to an algorithm when every trait it requires is TRUE for that algorithm.
#'
#' @details
#' Nothing here is maintained per algorithm. `reconstruction_rmse` requires
#' `invertible`, so it is defined for PCA, ICA and NMF and undefined for tSNE,
#' UMAP and Isomap; `max_abs_component_correlation` requires nothing, so it is
#' defined everywhere. An algorithm added to `decom_algorithms` with a correct
#' traits row inherits the right metric set without any edit to the metrics
#' code.
#'
#' @param algorithm Optional Character: Name of a decomposition algorithm,
#' matched case-insensitively. `NULL` returns the full derived matrix.
#'
#' @return data.frame. With `algorithm`, the registry rows that apply to it:
#' one row per metric, with its group, requirements, whether it needs new data,
#' direction and range. With `NULL`, one row per metric and one logical column
#' per algorithm.
#'
#' @author EDG
#' @export
#' @examples
#' # Every metric, and which algorithms support it
#' applicable_metrics()
#' # What can be computed for a UMAP embedding, which does not invert
#' applicable_metrics("UMAP")
applicable_metrics <- function(algorithm = NULL) {
  if (!is.null(algorithm)) {
    return(decom_metrics[metric_applies(decomposition_traits(algorithm)), ])
  }
  matrix_columns <- lapply(
    decom_algorithms[["name"]],
    function(alg) metric_applies(decomposition_traits(alg))
  )
  names(matrix_columns) <- decom_algorithms[["name"]]
  data.frame(
    name = decom_metrics[["name"]],
    matrix_columns,
    stringsAsFactors = FALSE
  )
} # /rtemis::applicable_metrics


# %% metric_applies ----
#' Which metrics a single algorithm's traits satisfy
#'
#' @param traits data.frame: One row of [decomposition_traits].
#'
#' @return Logical vector, one element per row of `decom_metrics`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
metric_applies <- function(traits) {
  vapply(
    decom_metrics[["requires"]],
    function(required) all(unlist(traits[required], use.names = FALSE)),
    logical(1L)
  )
} # /rtemis::metric_applies


# %% reconstruction_scores ----
#' Reconstruction metrics from one residual
#'
#' @param x Numeric matrix: Data in input units.
#' @param reconstructed Numeric matrix: Reconstruction, in the same units.
#'
#' @return Named list of `explained_variance_ratio`,
#' `relative_reconstruction_error` and `reconstruction_rmse`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
reconstruction_scores <- function(x, reconstructed) {
  residual <- x - reconstructed
  residual_norm <- norm(residual, "F")
  # The denominator of the explained variance ratio is the total sum of squares
  # about the column means, which is what an intercept-only "decomposition"
  # would leave. Zero when every column is constant, and the ratio is then
  # undefined rather than infinite.
  centered_norm <- norm(sweep(x, 2L, colMeans(x), FUN = "-"), "F")
  data_norm <- norm(x, "F")
  list(
    explained_variance_ratio = if (centered_norm > 0) {
      1 - (residual_norm / centered_norm)^2
    } else {
      NA_real_
    },
    relative_reconstruction_error = if (data_norm > 0) {
      residual_norm / data_norm
    } else {
      NA_real_
    },
    reconstruction_rmse = sqrt(mean(residual^2))
  )
} # /rtemis::reconstruction_scores


# %% component_scores ----
#' Metrics computable from the components alone
#'
#' @param transformed Numeric matrix: Components, cases by components.
#'
#' @return Named list of `max_abs_component_correlation` and
#' `effective_dimensionality`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
component_scores <- function(transformed) {
  variances <- apply(transformed, 2L, stats::var)
  list(
    max_abs_component_correlation = if (NCOL(transformed) > 1L) {
      correlations <- abs(stats::cor(transformed))
      max(correlations[upper.tri(correlations)])
    } else {
      # With one component there is no pair to correlate. NA is the honest
      # answer; 0 would claim the components are uncorrelated.
      NA_real_
    },
    effective_dimensionality = if (sum(variances^2) > 0) {
      sum(variances)^2 / sum(variances^2)
    } else {
      NA_real_
    }
  )
} # /rtemis::component_scores


# %% compute_decomposition_metrics ----
#' Compute the metrics an algorithm's traits support
#'
#' @param decom `Decomposition` object.
#' @param x Tabular data: The data the decomposition was fitted on.
#' @param new_data Optional tabular data: Data the fit never saw.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `DecompositionMetrics` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compute_decomposition_metrics <- function(
  decom,
  x,
  new_data = NULL,
  verbosity = 1L
) {
  traits <- decomposition_traits(decom@algorithm)
  applicable <- decom_metrics[["name"]][metric_applies(traits)]
  scores <- list()
  xm <- as.matrix(x)
  transformed <- as.matrix(decom@transformed)

  if (traits[["invertible"]]) {
    scores <- c(
      scores,
      reconstruction_scores(
        xm,
        reconstruct_(
          config = decom@config,
          decom = decom@decom,
          transformed = transformed,
          x = xm,
          verbosity = verbosity - 1L
        )
      )
    )
  }
  scores <- c(scores, component_scores(transformed))

  if (!is.null(new_data)) {
    if (!all(c("oos_explained_variance_ratio") %in% applicable)) {
      rtemis.core::abort(
        "'",
        decom@algorithm,
        "' cannot be applied to new data and inverted, so out-of-sample ",
        "metrics are undefined. Pass `new_data = NULL`.",
        class = "rtemis_unsupported_error"
      )
    }
    new_xm <- as.matrix(as.data.frame(new_data))
    new_transformed <- as.matrix(apply_decomp_(
      config = decom@config,
      decom = decom@decom,
      new_data = new_xm,
      verbosity = verbosity - 1L
    ))
    oos <- reconstruction_scores(
      new_xm,
      reconstruct_(
        config = decom@config,
        decom = decom@decom,
        transformed = new_transformed,
        x = new_xm,
        verbosity = verbosity - 1L
      )
    )
    scores[["oos_explained_variance_ratio"]] <- oos[[
      "explained_variance_ratio"
    ]]
    scores[["oos_relative_reconstruction_error"]] <- oos[[
      "relative_reconstruction_error"
    ]]
    scores[["reconstruction_gap"]] <- oos[["relative_reconstruction_error"]] -
      scores[["relative_reconstruction_error"]]
  }

  # With `new_data` the object describes two samples at once -- the unprefixed
  # columns the fitted data, the `oos_` columns the new data -- so no single
  # sample label is true of it.
  sample <- if (is.null(new_data)) "Training" else NULL
  do.call(DecompositionMetrics, c(scores, list(sample = sample)))
} # /rtemis::compute_decomposition_metrics


# %% decomp_metrics ----
#' Decomposition Metrics
#'
#' Quantify a fitted decomposition: how much of the data its components
#' reconstruct, how redundant they are, and -- given data the fit never saw --
#' whether it generalizes.
#'
#' @details
#' Which metrics are computed follows from the algorithm's traits; see
#' [applicable_metrics]. A metric its algorithm cannot support is `NA`.
#'
#' `decomp()` already stores this on its result for the data it was fitted on,
#' so calling this without `new_data` recomputes what `decom@metrics` holds. The
#' reason to call it is `new_data`: `reconstruction_gap` is the difference
#' between out-of-sample and in-sample relative reconstruction error, and it is
#' the signal that a decomposition has fitted its training cases rather than
#' their structure.
#'
#' Reconstruction is measured in the units of the data as it was passed to
#' `decomp()`, so the numbers are comparable across algorithms and across
#' configurations of one algorithm. One caveat: ICA with `row_norm = TRUE`
#' standardizes each case across features, and inverting that needs each case's
#' own mean and standard deviation, taken from the data being reconstructed.
#' Its reconstruction therefore draws on information the components do not
#' carry, and its error is not comparable with an algorithm that reconstructs
#' from components alone.
#'
#' @param decom `Decomposition` object.
#' @param x Tabular data (data.frame, data.table, or tibble): The data `decom`
#' was fitted on.
#' @param new_data Optional tabular data: Data the fit never saw. Its columns
#' must match those `decom` was fitted on.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `DecompositionMetrics` object.
#'
#' @author EDG
#' @export
#' @examples
#' x <- exc(iris, "Species")
#' iris_pca <- decomp(x, algorithm = "PCA", verbosity = 0L)
#' decomp_metrics(iris_pca, x, verbosity = 0L)
decomp_metrics <- function(decom, x, new_data = NULL, verbosity = 1L) {
  check_is_S7(decom, Decomposition)
  compute_decomposition_metrics(
    decom = decom,
    x = x,
    new_data = new_data,
    verbosity = verbosity
  )
} # /rtemis::decomp_metrics
