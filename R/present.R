# present.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Describe and Plot Fitted Models
#'
#' Ordinary regression
#' presents true/predicted values; ordinary classification presents ROC curves
#' or confusion panels. Resampled models present stored metric distributions.
#' Descriptions use `describe()` unless `verbosity = 0`.
#'
#' A nonempty homogeneous list compares models of the same task and sampling
#' kind. Ordinary models use grouped metric bars. Resampled models use one
#' boxplot panel per sample with common value limits and aligned model labels.
#' Fold counts may differ; fold scores are neither paired nor pooled across
#' models. Missing scores stay missing. Wholly unavailable samples are omitted
#' under `what = "all"`; explicit unavailable samples are errors.
#'
#' Comparison labels use `model_names`, complete list names, or algorithm
#' names, made unique in that order. The existing rtemis data-fingerprint
#' mismatch notice is retained, including when descriptions are disabled.
#'
#' @param x rtemis model or List: Fitted ordinary/resampled models.
#' @param ... Arguments for the selected presentation: `what`, `verbosity`, and
#'   drawing arguments. Classification accepts `type = "roc"` or `"confusion"`.
#'   Lists accept `metric`, `model_names`, `what`, `ylim`, `boxpoints`,
#'   `quartiles`, `whisker`, `palette`, `theme`, `width`, `height`, and `filename`.
#'   Single models forward drawing arguments to their plot_* method. Comparison
#'   lists use training/test samples; individual ordinary models also support
#'   validation. Explicit `ylim` is a finite increasing pair for comparisons.
#' @return htmlwidget: One chart or one complete [rtemis.draw::draw_panels()] figure.
#' @export
#' @examplesIf interactive()
#' model <- train(mtcars[, c("wt", "mpg")],
#'   hyperparameters = setup_GLM(), verbosity = 0L)
#' present(model, verbosity = 0L)
present <- new_generic("present", "x")

#' Describe and plot one regression model
#' @inheritParams present
#' @param what Character: Samples to draw.
#' @param verbosity Integer `[0, Inf)`: Description verbosity.
#' @return htmlwidget: True/predicted visualization.
#' @keywords internal
#' @noRd
present_regression <- function(x, what = "all", verbosity = 1L, ...) {
  present_description(x, verbosity)
  plot_true_pred(x, what = what, ...)
}

#' Describe and plot one classification model
#' @inheritParams present_regression
#' @param type Character: ROC or confusion view, case insensitive.
#' @return htmlwidget: Classification visualization.
#' @keywords internal
#' @noRd
present_classification <- function(
  x,
  what = "all",
  type = "roc",
  verbosity = 1L,
  ...
) {
  check_character_scalar(type)
  type <- match_arg(tolower(type), c("roc", "confusion"))
  present_description(x, verbosity)
  if (type == "roc") {
    plot_roc(x, what = what, ...)
  } else {
    plot_true_pred(x, what = what, ...)
  }
}

#' Describe and plot one resampled model
#' @inheritParams present_regression
#' @param metric Optional Character: Stored metric name.
#' @return htmlwidget: Metric distributions.
#' @keywords internal
#' @noRd
present_resampled <- function(
  x,
  what = "all",
  metric = NULL,
  verbosity = 1L,
  ...
) {
  present_description(x, verbosity)
  plot_metric(x, what = what, metric = metric, ...)
}

#' Print an optional model description
#' @inheritParams present_regression
#' @param metric Optional Character: Metric used in a list description.
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
present_description <- new_generic("present_description", "x")
method(present_description, class_any) <- function(
  x,
  verbosity = 1L,
  metric = NULL
) {
  check_integer_scalar(verbosity)
  if (verbosity < 0L) {
    abort(
      "Set `verbosity` to a nonnegative integer.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (verbosity > 0L) {
    if (is.list(x)) {
      describe(x, metric = metric, verbosity = verbosity)
    } else {
      describe(x, verbosity = verbosity)
    }
  }
  invisible(NULL)
}

#' Extract metric records from a homogeneous model comparison
#' @inheritParams present
#' @param metric Optional Character: Stored metric name.
#' @param model_names Optional Character: Model labels.
#' @param what Character: All available or selected training/test samples.
#' @return List: Long data, model labels, metric, and sampling kind.
#' @keywords internal
#' @noRd
comparison_data <- new_generic("comparison_data", "x")
method(comparison_data, class_list) <- function(
  x,
  metric = NULL,
  model_names = NULL,
  what = "all"
) {
  if (!length(x)) {
    abort(
      "Supply a nonempty list of fitted rtemis models.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  ordinary_class <- Supervised
  resampled_class <- SupervisedRes
  ordinary <- vapply(x, S7_inherits, logical(1), ordinary_class)
  resampled <- vapply(x, S7_inherits, logical(1), resampled_class)
  if (!(all(ordinary) || all(resampled))) {
    abort(
      "Compare either ordinary models or resampled models in one list.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  tasks <- vapply(x, function(m) m@type, "")
  if (
    length(unique(tasks)) != 1L ||
      !tasks[[1L]] %in% c("Regression", "Classification")
  ) {
    abort(
      "Compare models with the same regression or classification task.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
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
  metric <- metric %||%
    if (tasks[[1L]] == "Classification") "balanced_accuracy" else "rsq"
  check_character_scalar(metric)
  if (is.null(model_names)) {
    model_names <- names(x)
    if (
      is.null(model_names) || anyNA(model_names) || any(!nzchar(model_names))
    ) {
      model_names <- vapply(x, function(m) m@algorithm, "")
    }
  }
  if (
    !is.character(model_names) ||
      length(model_names) != length(x) ||
      anyNA(model_names) ||
      any(!nzchar(model_names))
  ) {
    abort(
      "Supply one nonempty model name per model.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  model_names <- make.unique(model_names, sep = "_")
  parts <- lapply(seq_along(x), function(i) {
    model <- x[[i]]
    if (all(resampled)) {
      # Each producer owns its fold IDs; different models need not share folds.
      records <- metric_plot_data(model, what = what, metric = metric)
    } else {
      found <- FALSE
      records <- do.call(
        rbind,
        lapply(what, function(sample) {
          report <- prop(model, paste0("metrics_", sample))
          table <- if (is.null(report)) NULL else report@metrics
          if (tasks[[1L]] == "Classification") {
            table <- table[["overall"]]
          }
          value <- NA_real_
          if (metric %in% names(table)) {
            found <<- TRUE
            value <- table[[metric]]
            if (length(value) != 1L) {
              abort(
                "Use a scalar overall comparison metric.",
                class = c("rtemis_length_error", "rtemis_input_error")
              )
            }
            value <- plot_metric_value(value)
          }
          data.frame(
            fold = "model",
            split = labelify(sample),
            metric = metric,
            value = value
          )
        })
      )
      if (!found) {
        abort(
          "Metric '",
          metric,
          "' is absent from model '",
          model_names[[i]],
          "'; select a stored metric.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    records[["model"]] <- model_names[[i]]
    records
  })
  data <- do.call(rbind, parts)
  available <- vapply(
    labelify(what),
    function(sample) any(is.finite(data[["value"]][data[["split"]] == sample])),
    logical(1)
  )
  if (!all_samples && any(!available)) {
    abort(
      "An explicitly selected sample has no available metric values; select an available sample.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!any(available)) {
    abort(
      "Supply at least one available metric value for comparison.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  splits <- labelify(what)[available]
  data <- data[data[["split"]] %in% splits, , drop = FALSE]
  list(
    data = data,
    models = model_names,
    splits = splits,
    metric = metric,
    resampled = all(resampled)
  )
}

#' Draw a model comparison from extracted metric records
#' @param x List: Output of comparison_data().
#' @param ylim Optional Numeric: Shared finite increasing limits.
#' @param boxpoints,quartiles,whisker,palette See [rtemis.draw::draw_boxplot()].
#' @param theme,width,height,filename See [rtemis.draw::draw()].
#' @return htmlwidget: Bars or a complete panel figure.
#' @keywords internal
#' @noRd
comparison_drawing <- new_generic("comparison_drawing", "x")
method(comparison_drawing, class_list) <- function(
  x,
  ylim = NULL,
  boxpoints = "all",
  quartiles = "linear",
  whisker = 1.5,
  palette = NULL,
  theme = NULL,
  width = 900,
  height = NULL,
  filename = NULL
) {
  data <- x[["data"]]
  if (
    !is.null(ylim) &&
      (!is.numeric(ylim) ||
        is.complex(ylim) ||
        length(ylim) != 2L ||
        any(!is.finite(ylim)) ||
        ylim[[1]] >= ylim[[2]])
  ) {
    abort(
      "Set `ylim` to two finite increasing numbers.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!x[["resampled"]]) {
    values <- lapply(x[["splits"]], function(split) {
      rows <- data[data[["split"]] == split, , drop = FALSE]
      rows[["value"]][match(x[["models"]], rows[["model"]])]
    })
    names(values) <- x[["splits"]]
    chart_data <- data.frame(model = x[["models"]], values, check.names = FALSE)
    opt <- rtemis.draw::compile(
      rtemis.draw::setup_BarConfig(
        x = "model",
        y = names(values),
        palette = palette,
        ylab = labelify(x[["metric"]])
      ),
      data = chart_data
    )
    missing <- sum(is.na(unlist(values)))
    if (missing) {
      msg(missing, "missing metric value(s) omitted")
    }
    if (!is.null(ylim)) {
      opt@y_axis@min <- ylim[[1]]
      opt@y_axis@max <- ylim[[2]]
    }
    return(rtemis.draw::draw(
      opt,
      theme = theme,
      width = width,
      height = height,
      filename = filename
    ))
  }
  limits <- ylim
  if (is.null(limits)) {
    limits <- range(data[["value"]], na.rm = TRUE)
    span <- diff(limits)
    if (span == 0) {
      span <- max(abs(limits[[1L]]) * .1, 1)
    }
    limits <- limits + c(-.05, .05) * span
  }
  panels <- lapply(x[["splits"]], function(split) {
    rows <- data[data[["split"]] == split, , drop = FALSE]
    # A missing report keeps the model's category; do not drop its identity.
    absent <- setdiff(x[["models"]], rows[["model"]])
    for (name in absent) {
      rows <- rbind(
        rows,
        data.frame(
          fold = "unavailable",
          split = split,
          metric = x[["metric"]],
          value = NA_real_,
          model = name
        )
      )
    }
    rows <- rows[order(match(rows[["model"]], x[["models"]])), , drop = FALSE]
    opt <- rtemis.draw::compile(
      rtemis.draw::setup_BoxplotConfig(
        x = "value",
        group = "model",
        observation = "fold",
        boxpoints = boxpoints,
        quartiles = quartiles,
        whisker = whisker,
        palette = palette,
        title = split,
        ylab = labelify(x[["metric"]])
      ),
      data = rows
    )
    opt@y_axis@min <- limits[[1]]
    opt@y_axis@max <- limits[[2]]
    rtemis.draw::draw(opt, theme = theme)
  })
  rtemis.draw::draw_panels(
    panels,
    width = width,
    height = height,
    filename = filename
  )
}

#' Describe and compare a list of fitted models
#' @inheritParams comparison_data
#' @inheritParams comparison_drawing
#' @inheritParams present_regression
#' @return htmlwidget: Model comparison.
#' @keywords internal
#' @noRd
method(present, class_list) <- function(
  x,
  metric = NULL,
  model_names = NULL,
  what = "all",
  ylim = NULL,
  boxpoints = "all",
  quartiles = "linear",
  whisker = 1.5,
  palette = NULL,
  theme = NULL,
  width = 900,
  height = NULL,
  filename = NULL,
  verbosity = 1L
) {
  records <- comparison_data(x, metric, model_names, what)
  names(x) <- records[["models"]]
  # This producer-owned notice compares only available fingerprints and remains
  # a notice (not an R warning) for legitimate comparisons across input data.
  warn_fingerprint_mismatch(x)
  present_description(x, verbosity, records[["metric"]])
  comparison_drawing(
    records,
    ylim = ylim,
    boxpoints = boxpoints,
    quartiles = quartiles,
    whisker = whisker,
    palette = palette,
    theme = theme,
    width = width,
    height = height,
    filename = filename
  )
}
