# plot_pr.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Plot Classification Precision-Recall Curves
#'
#' S7 generic for ordinary and resampled rtemis classification objects using
#' ECharts. Sample selection, resample handling, and the positive class follow
#' [plot_roc()].
#'
#' Curves are step functions of recall, and the legend reports average
#' precision, which is the area under the drawn curve. A dotted baseline marks
#' the prevalence of the positive class, the expected precision of a classifier
#' that scores independently of the label. Precision and recall are defined on
#' the positive class, so `positive` changes a binary curve.
#'
#' `variant = "aggregate"` pools aligned prediction rows, retaining repeated
#' observations. `"per_resample"` draws individual curves with the mean and
#' sample SD of fold average precision. Missing probability folds remain
#' undefined curves and are disclosed; they are not zero scores. All curves use
#' the same named classes and binary positive class, defaulting to the first
#' fold's second outcome level. Multiclass models have one one-vs-rest curve per
#' class.
#'
#' @param x rtemis classification object: Ordinary or resampled result.
#' @param ... Method arguments, including `what` (Character: `"all"` or unique
#'   sample names), `variant` (Character: `"aggregate"` or `"per_resample"`),
#'   `positive` (Optional Character: binary positive class), and `labelify`
#'   (Logical: capitalize sample labels), followed by additional named
#'   arguments to [rtemis.draw::draw_pr()].
#' @return An ECharts htmlwidget.
#' @export
#' @examplesIf interactive() && requireNamespace("rpart", quietly = TRUE)
#' model <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
#' plot_pr(model)
plot_pr <- new_generic("plot_pr", "x")


#' Extract portable precision-recall records from a model
#' @inheritParams plot_pr
#' @return Data frame of class, split, fold, recall, precision, average
#'   precision, prevalence, and omitted counts.
#' @keywords internal
#' @noRd
pr_plot_data <- new_generic("pr_plot_data", "x")


#' Normalize classification prediction samples before computing PR curves
#' @inheritParams plot_pr
#' @return Data frame of selected precision-recall records.
#' @keywords internal
#' @noRd
extract_pr_plot_data <- function(
  x,
  what = "all",
  variant = "aggregate",
  positive = NULL,
  labelify = TRUE
) {
  extract_curve_plot_data(
    x,
    rtemis.draw::pr_vertices,
    what,
    variant,
    positive,
    labelify
  )
}


#' Draw precision-recall records extracted from a classification result
#' @inheritParams plot_pr
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_model_pr <- function(
  x,
  what = "all",
  variant = "aggregate",
  positive = NULL,
  labelify = TRUE,
  ...
) {
  draw_model_curves(
    rtemis.draw::draw_pr,
    pr_plot_data(x, what, variant, positive, labelify),
    variant,
    list(...)
  )
}


# %% pr_table ----
#' Precision-Recall Curves of a Classification Model
#'
#' The precision-recall curves of a fitted classification model for each sample
#' it stores predicted probabilities for: the data [plot_pr()] draws, as a table.
#'
#' @details
#' Binary models have one curve, for the positive class; multiclass models have
#' one one-vs-rest curve per class. For a resampled model, `variant =
#' "aggregate"` pools the predictions of every outer resample into one curve
#' per sample and class, and `variant = "per_resample"` gives one curve per
#' resample, labeled in the `resample` column. A resample whose probabilities
#' are missing has an undefined curve, with `ap` NA. `omitted` counts the cases
#' whose probability or label is missing.
#'
#' `ap`, the average precision, and `prevalence`, the fraction of the positive
#' class, are computed from every case, so `max_points` leaves them unchanged.
#'
#' @param x `Classification` or `ClassificationRes` object.
#' @param sample Optional Character \{"training", "validation", "test"\}: The
#'   sample whose curves to return. `NULL` returns every sample with predicted
#'   probabilities.
#' @param variant Character \{"aggregate", "per_resample"\}: For a resampled
#'   model, pooled curves or one curve per resample. An ordinary model has
#'   aggregate curves only.
#' @param positive Optional Character: Positive class of a binary model. `NULL`
#'   uses the second level of the outcome.
#' @param max_points Optional Integer \[2, Inf): Cap on the number of vertices
#'   per curve. `NULL` keeps every vertex.
#'
#' @return data.table with columns `sample`; `resample`, for `variant =
#'   "per_resample"`; `class`; `recall`; `precision`; `ap` and `prevalence`,
#'   constant within a curve; and `omitted`.
#'
#' @author EDG
#' @export
#' @examplesIf requireNamespace("rpart", quietly = TRUE)
#' mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
#' pr_table(mod)
pr_table <- function(
  x,
  sample = NULL,
  variant = "aggregate",
  positive = NULL,
  max_points = NULL
) {
  curve_table(
    x,
    pr_plot_data,
    c("recall", "precision", "ap", "prevalence"),
    sample,
    variant,
    positive,
    max_points
  )
} # /rtemis::pr_table
