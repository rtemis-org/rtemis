# plot_confusion.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Extract stored confusion records from an optional metrics object
#'
#' Shared with the web interface: stored reference/predicted/n counts remain
#' the source of truth, including the producer's class order.
#' @inheritParams rtemis.draw::confusion_input
#' @return Data frame of frequency records.
#' @keywords internal
#' @noRd
extract_confusion_metrics <- function(x, y = NULL, classes = NULL) {
  if (!is.null(y)) {
    abort(
      "Omit `y` when drawing an existing confusion metrics object.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  x@confusion_long
}


#' Extract confusion frequencies for classification samples
#' @param x `rtemis::Classification` or `rtemis::ClassificationRes`: Model result.
#' @param what Optional Character: Selected samples, or all available samples.
#' @param labelify Logical: Capitalize sample labels.
#' @return Data frame of reference/predicted/n records with panel labels.
#' @keywords internal
#' @noRd
classification_plot_data <- new_generic("classification_plot_data", "x")

#' Extract stored counts or count ordinary label pairs
#'
#' Counts stored by rtemis take precedence over reconstructing predictions.
#' Ordinary results without stored counts use paired labels instead. Resampled
#' result classes require stored metrics, so their pooled records are used directly.
#' @inheritParams classification_plot_data
#' @return Data frame of frequency records with panel labels.
#' @keywords internal
#' @noRd
extract_classification_plot_data <- function(x, what = NULL, labelify = TRUE) {
  check_logical_scalar(labelify)
  properties <- names(S7_class(x)@properties)
  resampled <- "models" %in% properties
  allowed <- c("training", "validation", "test")
  allowed <- allowed[paste0("metrics_", allowed) %in% properties]
  available <- vapply(
    allowed,
    function(sample) {
      !is.null(prop(x, paste0("metrics_", sample))) ||
        !is.null(prop(x, paste0("y_", sample))) ||
        !is.null(prop(x, paste0("predicted_", sample)))
    },
    logical(1)
  )
  if (is.null(what)) {
    what <- if (resampled) "all" else tail(allowed[available], 1L)
  }
  all_samples <- identical(what, "all")
  if (
    !is.character(what) ||
      !length(what) ||
      anyNA(what) ||
      anyDuplicated(what) ||
      (!all_samples && any(!what %in% allowed))
  ) {
    abort(
      "Select unique `what` samples from training, validation, test, or 'all' as available.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (all_samples) {
    what <- allowed[available]
  }
  parts <- lapply(what, function(sample) {
    metrics <- prop(x, paste0("metrics_", sample))
    if (!is.null(metrics)) {
      data <- rtemis.draw::confusion_input(metrics)
    } else {
      observed <- prop(x, paste0("y_", sample))
      predicted <- prop(x, paste0("predicted_", sample))
      if (is.null(observed) || is.null(predicted)) {
        abort(
          "Sample '",
          sample,
          "' needs stored counts or both reference and predicted labels.",
          class = c("rtemis_null_input", "rtemis_input_error")
        )
      }
      # Resampled result classes require stored metrics. This fallback serves
      # ordinary results whose optional metrics have not been computed.
      data <- rtemis.draw::confusion_input(observed, predicted)
    }
    data[["panel"]] <- if (labelify) {
      paste0(toupper(substr(sample, 1L, 1L)), substring(sample, 2L))
    } else {
      sample
    }
    data
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    abort(
      "Select a classification result containing confusion counts or predictions.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  data <- do.call(rbind, parts)
  rownames(data) <- NULL
  data
}


#' Draw classification true/predicted summaries
#' @inheritParams classification_plot_data
#' @param ... Additional named settings for [rtemis.draw::draw_confusion()].
#' @return An ECharts htmlwidget with one or more confusion panels.
#' @keywords internal
#' @noRd
draw_classification_predictions <- function(
  x,
  what = NULL,
  labelify = TRUE,
  ...
) {
  rtemis.draw::draw_confusion(classification_plot_data(x, what, labelify), ...)
}
