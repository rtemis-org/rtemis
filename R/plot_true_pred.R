# plot_true_pred.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% plot_true_pred ----
#' Plot a Model's True Versus Predicted Values
#'
#' S7 generic for ECharts plots of rtemis regression and classification results.
#'
#' @details
#' `what = "all"` selects available training, validation, and test pairs for
#' a regression model, or training and test pairs for a resampled model.
#' An explicitly requested missing sample is an error. Resampled observations
#' are pooled in fold order, keeping repeated observations. Pair lengths and
#' names are checked within each fold before pooling.
#'
#' Rendering is provided by [rtemis.draw::draw_fit()]. The API uses draw argument names,
#' including `se`, `square`, `equal_axes`, and `title`, and a draw [rtemis.draw::Theme].
#' Supported fits are `"glm"`, `"gam"`, or `NULL`.
#'
#' Classification uses [rtemis.draw::draw_confusion()] with stored confusion frequencies
#' when available, preserving the producer's class order. Otherwise it counts
#' aligned reference/predicted pairs. An omitted `what` selects test, validation,
#' then training for an ordinary classifier, or all available samples for a
#' resampled classifier. Multiple samples share one widget and one export.
#' Balanced accuracy in this view is macro recall; it is not the legacy
#' multiclass average of one-versus-rest balanced accuracies. See
#' [rtemis.draw::ConfusionConfig] for zero-denominator and pooled-count semantics.
#'
#' @param x rtemis regression or classification object: Ordinary or resampled result.
#' @param ... Method arguments, including `what` (Optional Character: `"all"`
#'   or a vector of `"training"`, `"validation"`, `"test"` sample names) and
#'   `labelify` (Logical: capitalize sample names in the legend or panel titles),
#'   followed by additional arguments to [rtemis.draw::draw_fit()] or [rtemis.draw::draw_confusion()].
#'   Resampled results do not have validation pairs.
#'
#' @return htmlwidget: ECharts drawing.
#' @export
#' @examplesIf interactive()
#' model <- train(
#'   mtcars[, c("wt", "hp", "mpg")],
#'   hyperparameters = setup_GLM(), verbosity = 0L
#' )
#' plot_true_pred(model)
plot_true_pred <- new_generic("plot_true_pred", "x")


# %% regression_plot_data ----
#' Extract paired observations from a regression result
#'
#' Ordinary and resampled model classes share this data extraction boundary.
#'
#' @inheritParams plot_true_pred
#' @return Data frame with `true`, `predicted`, and `sample` columns.
#' @keywords internal
#' @noRd
regression_plot_data <- new_generic("regression_plot_data", "x")


# %% extract_regression_plot_data ----
#' Extract aligned regression samples
#'
#' @inheritParams plot_true_pred
#' @return Data frame with `true`, `predicted`, and `sample` columns.
#' @keywords internal
#' @noRd
extract_regression_plot_data <- function(x, what = "all", labelify = TRUE) {
  check_logical_scalar(labelify)
  allowed <- c("training", "validation", "test")
  allowed <- allowed[
    paste0("predicted_", allowed) %in% names(S7::S7_class(x)@properties)
  ]
  all_samples <- identical(what, "all")
  if (
    !is.character(what) ||
      !length(what) ||
      anyNA(what) ||
      anyDuplicated(what) ||
      (!all_samples && any(!what %in% allowed))
  ) {
    abort(
      "Set `what` to 'all' or unique sample names from: ",
      paste(allowed, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (all_samples) {
    what <- allowed
  }
  parts <- lapply(what, function(sample) {
    observed <- prop(x, paste0("y_", sample))
    predicted <- prop(x, paste0("predicted_", sample))
    if (is.null(observed) && is.null(predicted) && all_samples) {
      return(NULL)
    }
    if (
      all_samples &&
        is.list(observed) &&
        is.list(predicted) &&
        length(observed) == length(predicted) &&
        all(vapply(c(observed, predicted), is.null, logical(1L)))
    ) {
      return(NULL)
    }
    if (is.null(observed) || is.null(predicted)) {
      abort(
        "Sample '",
        sample,
        "' needs both true and predicted values; select an available sample.",
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    # List elements are folds. Normalize each pair before replacing its fold
    # label with the sample label, so missing values never shift other folds.
    data <- rtemis.draw::true_pred_data(observed, predicted)
    data[["sample"]] <- if (labelify) {
      paste0(toupper(substr(sample, 1L, 1L)), substring(sample, 2L))
    } else {
      sample
    }
    data
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    abort(
      "No true/predicted pairs are available; plot a result containing predictions.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  data <- do.call(rbind, parts)
  rownames(data) <- NULL
  data
}


# %% draw_regression_predictions ----
#' Render paired regression samples
#'
#' Shared implementation registered for ordinary and resampled regression
#' results. Statistical fitting and rendering are delegated to [rtemis.draw::draw_fit()].
#'
#' @inheritParams plot_true_pred
#' @return htmlwidget: ECharts drawing.
#' @keywords internal
#' @noRd
draw_regression_predictions <- function(x, what = "all", labelify = TRUE, ...) {
  data <- regression_plot_data(x, what = what, labelify = labelify)
  rtemis.draw::draw_fit(
    data[["true"]],
    data[["predicted"]],
    group = data[["sample"]],
    ...
  )
}
