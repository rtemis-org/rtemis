# plot_varimp.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% plot_varimp ----
#' Plot a Model's Variable Importance
#'
#' Draw importance recorded by a rtemis `Supervised` or `SupervisedRes` object
#' using [rtemis.draw::draw_varimp()].
#'
#' @details
#' Named measures and variable identities are preserved. Resampled records
#' retain their fold IDs before summarizing. The first measure in the first
#' available fold is selected when `measure` is omitted; columns missing in
#' other folds are unavailable scores. The full fold list is passed to the
#' renderer, including folds with no importance result.
#'
#' CART's `importance` and LightGBM's `Gain`, `Cover`, and `Frequency` come
#' from sparse split summaries: omitted variables in a reporting fold have
#' zero importance. These measures use `absent = "zero"`. Other measures use
#' `absent = "missing"`; pass `absent` explicitly to declare another contract.
#' Explicit NA values and wholly unavailable folds are always kept missing.
#'
#' Use `type = "boxplot"` for resampled fold distributions with all contributing
#' scores overlaid. Summary bars remain the default. Both views share selection,
#' ranking, and the producer's missing-score contract. Ordinary models have no
#' fold distribution and require the bar view.
#'
#' Models without any importance records produce an informative error.
#' The default title names the algorithm; `title = NULL` omits it.
#' See [rtemis.draw::draw_varimp()] for selection, ranking, and missing-score semantics.
#'
#' @param x `rtemis::Supervised` or `rtemis::SupervisedRes`: Fitted model result.
#' @param ... Additional arguments to [rtemis.draw::draw_varimp()], including `measure`,
#'   `type`, `top_n`, `rank_by`, `decreasing`, `bar_width`, `summary`, `absent`,
#'   and `title`. Fold IDs come from
#'   the model and cannot be overridden.
#' @return htmlwidget: ECharts importance bars or fold distributions.
#' @export
#' @examplesIf interactive()
#' model <- train(
#'   mtcars[, c("wt", "hp", "mpg")],
#'   hyperparameters = setup_GLM(), verbosity = 0L
#' )
#' plot_varimp(model)
plot_varimp <- new_generic("plot_varimp", "x")


# %% varimp_plot_data ----
#' Extract model importance without aggregating folds
#' @inheritParams plot_varimp
#' @return Named list: Plain `data` records and optional complete `folds` IDs.
#' @keywords internal
#' @noRd
varimp_plot_data <- new_generic("varimp_plot_data", "x")


# %% extract_varimp_plot_data ----
#' Extract ordinary or resampled model importance
#'
#' Registered on ordinary and resampled result classes. The resampled class stores
#' importance in `resample_ids` order; list names are not used to reorder it.
#'
#' @inheritParams plot_varimp
#' @return Named list: Plain `data` records and optional complete `folds` IDs.
#' @keywords internal
#' @noRd
extract_varimp_plot_data <- function(x) {
  importance <- x@varimp
  if (
    is.null(importance) ||
      (is.list(importance) && all(vapply(importance, is.null, logical(1L))))
  ) {
    abort(
      "No variable importance is available. Use a model that records importance.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!is.list(importance)) {
    return(list(data = rtemis.draw::varimp_data(importance@data), folds = NULL))
  }
  folds <- x@resample_ids
  if (length(folds) != length(importance)) {
    abort(
      "Supply one importance result per resample ID, keeping NULL for unavailable folds.",
      class = c("rtemis_dim_error", "rtemis_input_error")
    )
  }
  parts <- lapply(seq_along(importance), function(i) {
    value <- importance[[i]]
    if (is.null(value)) {
      return(NULL)
    }
    data <- rtemis.draw::varimp_data(value@data)
    if ("fold" %in% names(data)) {
      abort(
        "Reserve `fold` for resample IDs; rename the importance measure `fold`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    data[["fold"]] <- rep(folds[[i]], nrow(data))
    data
  })
  parts <- Filter(Negate(is.null), parts)
  # A missing measure column is unavailable, rather than another fold's first
  # measure. Keep the union so named selection is consistent across folds.
  columns <- unique(unlist(lapply(parts, names), use.names = FALSE))
  parts <- lapply(parts, function(data) {
    for (column in setdiff(columns, names(data))) {
      data[[column]] <- NA_real_
    }
    data[columns]
  })
  list(data = rtemis.draw::varimp_data(do.call(rbind, parts)), folds = folds)
}


# %% draw_model_varimp ----
#' Render a model's importance records
#' @inheritParams plot_varimp
#' @param measure Optional Character: Named importance measure.
#' @param absent Optional Character: Override the producer's omission contract.
#' @param title Optional Character: Chart title.
#' @return htmlwidget: ECharts importance bars or fold distributions.
#' @keywords internal
#' @noRd
draw_model_varimp <- function(
  x,
  measure = NULL,
  absent = NULL,
  title = paste(x@algorithm, "variable importance"),
  ...
) {
  records <- varimp_plot_data(x)
  measure <- measure %||%
    setdiff(names(records[["data"]]), c("variable", "fold"))[[1L]]
  check_character_scalar(measure)
  check_enum(measure, setdiff(names(records[["data"]]), c("variable", "fold")))
  # These specific measures sum split contributions; their native producers
  # emit only credited variables. Unknown measures get no zero assumption.
  sparse_measures <- switch(
    x@algorithm,
    CART = "importance",
    LightGBM = c("Gain", "Cover", "Frequency"),
    character()
  )
  absent <- absent %||% if (measure %in% sparse_measures) "zero" else "missing"
  rtemis.draw::draw_varimp(
    records[["data"]],
    measure = measure,
    absent = absent,
    folds = records[["folds"]],
    title = title,
    ...
  )
}
