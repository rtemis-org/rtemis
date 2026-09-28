# plot_metric.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Plot Stored Metrics Across Model Resamples
#'
#' S7 generic for `SupervisedRes` results, including
#' regression and classification. Uses recorded per-resample metrics in
#' `resample_ids` order; list names do not change that producer contract.
#' The default metric is `balanced_accuracy` for classification and `rsq` for
#' regression. Classification selects overall metrics, not per-class scores.
#'
#' All available training and test reports are selected by default. An
#' explicitly requested unavailable report is an error. Missing metric values
#' remain missing; a metric absent from every selected report is an error.
#' No predictions or aggregate means are used to reconstruct fold values.
#'
#' @param x rtemis::SupervisedRes: Resampled fitted model.
#' @param ... Method arguments, including `what` (Character: `"all"` or unique
#'   `"training"` and/or `"test"` samples) and `metric` (Optional Character:
#'   stored metric name), followed by additional arguments to [rtemis.draw::draw_metric()].
#' @return htmlwidget: Metric distributions with all observations by default.
#' @export
#' @examplesIf interactive()
#' model <- train(mtcars[, c("wt", "mpg")],
#'   hyperparameters = setup_GLM(),
#'   outer_resampling_config = setup_KFold(n_resamples = 3L),
#'   execution_config = setup_SerialExecution(),
#'   verbosity = 0L
#' )
#' plot_metric(model)
plot_metric <- new_generic("plot_metric", "x")

#' Extract stored model metric records
#' @inheritParams plot_metric
#' @return Data frame: Fold, split, metric, and value records.
#' @keywords internal
#' @noRd
metric_plot_data <- new_generic("metric_plot_data", "x")

#' Extract per-resample scores in the producer's documented order
#' @inheritParams plot_metric
#' @return Data frame: Selected metric records, retaining unavailable scores.
#' @keywords internal
#' @noRd
extract_metric_plot_data <- function(x, what = "all", metric = NULL) {
  all_samples <- identical(what, "all")
  if (
    !is.character(what) ||
      !length(what) ||
      anyNA(what) ||
      anyDuplicated(what) ||
      (!all_samples && any(!what %in% c("training", "test")))
  ) {
    abort(
      "Select 'all' or unique 'training' and 'test' sample names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (all_samples) {
    what <- c("training", "test")
  }
  classification <- identical(x@type, "Classification")
  metric <- metric %||% if (classification) "balanced_accuracy" else "rsq"
  check_character_scalar(metric)
  ids <- x@resample_ids
  if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    abort(
      "Supply unique nonempty resample_ids for stored metrics.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  pieces <- list()
  found <- FALSE
  available <- character()
  for (sample in what) {
    report <- prop(x, paste0("metrics_", sample))
    if (is.null(report)) {
      if (all_samples) {
        next
      }
      abort(
        "Sample '",
        sample,
        "' has no metric report; select an available sample.",
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    folds <- report@res_metrics
    if (length(folds) != length(ids)) {
      abort(
        "Keep one metric report per resample_id in model order.",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
    values <- vapply(
      folds,
      function(fold) {
        if (is.null(fold)) {
          return(NA_real_)
        }
        table <- fold@metrics
        if (classification) {
          table <- table[["overall"]]
        }
        available <<- union(available, names(table))
        if (!metric %in% names(table)) {
          return(NA_real_)
        }
        found <<- TRUE
        value <- table[[metric]]
        if (length(value) != 1L) {
          abort(
            "Store one metric value per resample; use an overall scalar metric.",
            class = c("rtemis_length_error", "rtemis_input_error")
          )
        }
        plot_metric_value(value)
        as.numeric(value)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
    pieces[[length(pieces) + 1L]] <- data.frame(
      fold = ids,
      split = labelify(sample),
      metric = metric,
      value = values
    )
  }
  if (!found) {
    abort(
      "Select a stored metric from the available reports: ",
      paste(available, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  do.call(rbind, pieces)
}

#' Draw stored model metric distributions
#' @inheritParams plot_metric
#' @return htmlwidget: Metric distributions.
#' @keywords internal
#' @noRd
draw_model_metric <- function(x, what = "all", metric = NULL, ...) {
  rtemis.draw::draw_metric(
    metric_plot_data(x, what = what, metric = metric),
    ...
  )
}


# %% plot_metric_value ----
#' Validate a stored scalar metric for plotting
#' @param x Numeric scalar: A finite or missing stored metric.
#' @return Numeric scalar, retaining missing values.
#' @keywords internal
#' @noRd
plot_metric_value <- new_generic("plot_metric_value", "x")
method(plot_metric_value, class_any) <- function(x) {
  if (is.logical(x) && length(x) == 1L && is.na(x)) {
    x <- NA_real_
  }
  if (
    !is.numeric(x) ||
      is.complex(x) ||
      length(x) != 1L ||
      !is.null(dim(x)) ||
      any(is.infinite(x))
  ) {
    abort(
      "Store one finite numeric metric or NA per result.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  as.numeric(x)
}
