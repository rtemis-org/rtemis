# plot_varimp.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% plot_varimp ----
#' Plot a Model's Variable Importance
#'
#' Draw importance recorded by a rtemis `Supervised` or `SupervisedRes` object,
#' or computed from a `SHAP` explanation by [get_varimp], using
#' [rtemis.draw::draw_varimp()].
#'
#' @details
#' Named measures and variable identities are preserved. Resampled records
#' retain their fold IDs before summarizing. The first measure in the first
#' available fold is selected when `measure` is omitted; columns missing in
#' other folds are unavailable scores. The full fold list is passed to the
#' renderer, including folds with no importance result.
#'
#' Every producer reports a value for each predictor it scores, including
#' zeros, so an NA value or a wholly unavailable fold is an unavailable score
#' and is excluded from the summary. [varimp_ranking()] returns the summary and
#' ranking drawn here.
#'
#' Use `type = "boxplot"` for resampled fold distributions with all contributing
#' scores overlaid. Summary bars remain the default. Both views share selection,
#' ranking, and missing-score handling. Ordinary models have no
#' fold distribution and require the bar view.
#'
#' A `SHAP` explanation is drawn from the mean absolute contribution of each
#' feature, with one measure per class for a multiclass outcome.
#'
#' Models without any importance records produce an informative error.
#' The default title names the algorithm; `title = NULL` omits it.
#' See [rtemis.draw::draw_varimp()] for selection, ranking, and missing-score semantics.
#'
#' @param x `rtemis::Supervised`, `rtemis::SupervisedRes` or `rtemis::SHAP`:
#'   Fitted model result or explanation.
#' @param ... Additional arguments to [rtemis.draw::draw_varimp()], including `measure`,
#'   `type`, `top_n`, `rank_by`, `decreasing`, `bar_width`, `summary`, and
#'   `title`. Fold IDs come from
#'   the model and cannot be overridden.
#' @return htmlwidget: ECharts importance bars or fold distributions.
#' @export
#' @examplesIf interactive()
#' model <- train(
#'   mtcars[, c("wt", "hp", "mpg")],
#'   hyperparameters = setup_GLM(), verbosity = 0L
#' )
#' plot_varimp(model)
#' features <- mtcars[, c("wt", "hp")]
#' plot_varimp(explain(model, features, background = features, verbosity = 0L))
plot_varimp <- new_generic("plot_varimp", "x")


# %% varimp_plot_data ----
#' Extract model importance without aggregating folds
#' @inheritParams plot_varimp
#' @return Named list: Plain `data` records and optional complete `folds` IDs.
#' @keywords internal
#' @noRd
varimp_plot_data <- new_generic("varimp_plot_data", "x")


# %% varimp_results ----
#' Importance results a plot draws
#'
#' @param x `Supervised`, `SupervisedRes` or `SHAP` object.
#'
#' @return `VariableImportance`, a list of them (one per resample), or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
varimp_results <- function(x) {
  if (S7_inherits(x, SHAP)) {
    return(get_varimp(x))
  }
  x@varimp
} # /rtemis::varimp_results


# %% extract_varimp_plot_data ----
#' Extract ordinary or resampled model importance
#'
#' Registered on ordinary and resampled result classes and on `SHAP`. The
#' resampled class stores importance in `resample_ids` order; list names are not
#' used to reorder it.
#'
#' @inheritParams plot_varimp
#' @return Named list: Plain `data` records and optional complete `folds` IDs.
#' @keywords internal
#' @noRd
extract_varimp_plot_data <- function(x) {
  importance <- varimp_results(x)
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
    return(list(
      data = rtemis.draw::varimp_data(varimp_table(importance)),
      folds = NULL
    ))
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
    data <- rtemis.draw::varimp_data(varimp_table(value))
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


# %% varimp_measure_descriptors ----
#' The descriptors of one measure in a model's importance
#'
#' @param x `Supervised`, `SupervisedRes` or `SHAP` object.
#' @param column Character: The measure's column in the wide table.
#'
#' @return List of `ImportanceMeasure` objects, one per importance result that
#'   has the measure.
#'
#' @author EDG
#' @keywords internal
#' @noRd
varimp_measure_descriptors <- function(x, column) {
  results <- varimp_results(x)
  if (S7_inherits(results, VariableImportance)) {
    results <- list(results)
  }
  results <- Filter(Negate(is.null), results)
  unlist(
    lapply(results, function(vi) {
      columns <- varimp_column_names(names(vi@measures))
      unname(vi@measures[names(columns)[columns == column]])
    }),
    recursive = FALSE
  )
} # /rtemis::varimp_measure_descriptors


# %% VARIMP_RANKING ----
# How each measure direction ranks bars: by signed value or magnitude, and
# whether larger first.
VARIMP_RANKING <- list(
  larger = list(rank_by = "signed", decreasing = TRUE),
  absolute = list(rank_by = "magnitude", decreasing = TRUE),
  smaller = list(rank_by = "signed", decreasing = FALSE),
  none = list(rank_by = "signed", decreasing = TRUE)
)


# %% varimp_measure_direction ----
#' The ranking direction of one measure in a model's importance
#'
#' @param x `Supervised`, `SupervisedRes` or `SHAP` object.
#' @param measure Character: The measure's column in the wide table.
#'
#' @return Character: The `direction` every result with the measure declares,
#'   or "absolute" when no result describes it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
varimp_measure_direction <- function(x, measure) {
  descriptors <- varimp_measure_descriptors(x, measure)
  directions <- unique(vapply(
    descriptors,
    function(m) m@direction,
    character(1L)
  ))
  kinds <- unique(vapply(descriptors, function(m) m@kind, character(1L)))
  if (length(directions) > 1L || length(kinds) > 1L) {
    abort(
      "Measure '",
      measure,
      "' has different kinds or directions across resamples, so its values ",
      "cannot be summarized together. Choose another measure.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (length(directions) == 1L) directions else "absolute"
} # /rtemis::varimp_measure_direction


# %% draw_model_varimp ----
#' Render a model's importance records
#'
#' Bars are ranked as the measure's `direction` declares -- larger values
#' first, larger magnitudes first, or smaller values first -- unless `rank_by`
#' or `decreasing` is given. A measure that does not rank predictors (a
#' spread) is drawn in decreasing order and titled as descriptive.
#'
#' @inheritParams plot_varimp
#' @param measure Optional Character: Named importance measure.
#' @param title Optional Character: Chart title.
#' @return htmlwidget: ECharts importance bars or fold distributions.
#' @keywords internal
#' @noRd
draw_model_varimp <- function(
  x,
  measure = NULL,
  title = paste(x@algorithm, "variable importance"),
  ...
) {
  records <- varimp_plot_data(x)
  measure <- measure %||%
    setdiff(names(records[["data"]]), c("variable", "fold"))[[1L]]
  check_character_scalar(measure)
  check_enum(measure, setdiff(names(records[["data"]]), c("variable", "fold")))
  direction <- varimp_measure_direction(x, measure)
  dots <- list(...)
  ranking <- VARIMP_RANKING[[direction]]
  for (nm in names(ranking)) {
    dots[[nm]] <- dots[[nm]] %||% ranking[[nm]]
  }
  if (missing(title) && direction == "none") {
    title <- paste0(x@algorithm, " ", measure, " (descriptive, not a ranking)")
  }
  do.call(
    rtemis.draw::draw_varimp,
    c(
      list(
        records[["data"]],
        measure = measure,
        folds = records[["folds"]],
        title = title
      ),
      dots
    )
  )
}
