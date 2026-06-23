# S7_Metrics.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% conf_table() ----
# Helper to place positive class in first row and column of confusion matrix,
# while train expects positive class to be second factor level.
conf_table <- function(true, pred, binclasspos = 2L) {
  # If binary and binclasspos is 2L, reorder factor levels
  if (length(levels(true)) == 2L && binclasspos == 2L) {
    true <- factor(true, levels = rev(levels(true)))
    pred <- factor(pred, levels = rev(levels(pred)))
  }
  table(true, pred)
}


# %% label_metrics() ----
# Prettify metric names for printing (e.g. balanced_accuracy -> "Balanced
# Accuracy", auc -> "AUC", rsq -> "R^2"). labelify() uppercases the acronyms
# via its capitalize_strings defaults; R-squared gets a Unicode superscript two.
# Stored field names stay lowercase.
CAP_METRICS <- c("mae", "mse", "rmse")
label_metrics <- function(x) {
  sub("^Rsq$", "R\u00b2", labelify(x, capitalize_strings = CAP_METRICS))
}

# Apply label_metrics() to a metric data.frame's row and column names.
label_metric_df <- function(df) {
  colnames(df) <- label_metrics(colnames(df))
  rownames(df) <- label_metrics(rownames(df))
  df
}


# %% Metrics ----
#' Metrics
#'
#' @description
#' Superclass for Metrics metrics.
#'
#' @field sample Character: Sample name.
#' @field metrics List or data.frame: Metrics.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Metrics <- new_class(
  name = "Metrics",
  package = "rtemis",
  properties = list(
    sample = class_character | NULL,
    metrics = class_list | class_data.frame
  )
) # /rtemis::Metrics


# %% `$`.Metrics ----
# Make Metrics@metrics `$`-accessible
method(`$`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}


# %% `.DollarNames`.Metrics ----
# `$`-autocomplete Metrics@metrics
method(`.DollarNames`, Metrics) <- function(x, pattern = "") {
  all_names <- names(x@metrics)
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.Metrics ----
# Make Metrics@metrics `[[`-accessible
method(`[[`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}


# %% RegressionMetrics ----
#' @title RegressionMetrics
#'
#' @description
#' Metrics subclass for regression models.
#'
#' @author EDG
#' @noRd
RegressionMetrics <- new_class(
  name = "RegressionMetrics",
  parent = Metrics,
  # properties = list(
  #   MAE = class_numeric,
  #   MSE = class_numeric,
  #   RMSE = class_numeric,
  #   Rsq = class_numeric
  # ),
  constructor = function(mae, mse, rmse, rsq, sample = NULL) {
    new_object(
      Metrics(
        sample = sample,
        metrics = data.frame(
          mae = mae,
          mse = mse,
          rmse = rmse,
          rsq = rsq
        )
      )
    )
  }
) # /rtemis::RegressionMetrics


# %% repr.RegressionMetrics ----
# Show RegressionMetrics ----
method(repr, RegressionMetrics) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  out <- if (!is.null(x@sample)) {
    repr_S7name(
      paste(x@sample, "Regression Metrics"),
      pad = pad,
      output_type = output_type
    )
  } else {
    repr_S7name("Regression Metrics", pad = pad, output_type = output_type)
  }
  out <- paste0(
    out,
    repr_ls(
      label_metric_df(x@metrics),
      print_class = FALSE,
      print_df = TRUE,
      pad = pad,
      output_type = output_type
    )
  )
  out
} # /rtemis::repr.RegressionMetrics


# %% print.RegressionMetrics ----
method(print, RegressionMetrics) <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.RegressionMetrics


# %% ClassificationMetrics ----
#' @title ClassificationMetrics
#'
#' @description
#' Metrics subclass for classification models.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClassificationMetrics <- new_class(
  name = "ClassificationMetrics",
  parent = Metrics,
  properties = list(
    confusion_matrix = class_table
  ),
  constructor = function(
    confusion_matrix,
    overall,
    class,
    positive_class,
    sample = NULL
  ) {
    new_object(
      confusion_matrix = confusion_matrix,
      Metrics(
        sample = sample,
        metrics = list(
          overall = overall,
          class = class,
          positive_class = positive_class
        )
      )
    )
  }
) # /rtemis::ClassificationMetrics


# %% repr.ClassificationMetrics ----
method(repr, ClassificationMetrics) <- function(
  x,
  decimal_places = 3L,
  pad = 0L,
  output_type = NULL,
  ...
) {
  output_type <- get_output_type(output_type)

  if (!is.null(x@sample)) {
    out <- repr_S7name(
      paste(x@sample, "Classification Metrics"),
      pad = pad,
      output_type = output_type
    )
  } else {
    out <- repr_S7name(
      "Classification Metrics",
      pad = pad,
      output_type = output_type
    )
  }
  # Confusion Matrix
  # suggestion: document 17 and 9
  tblpad <- 17L -
    max(nchar(colnames(x@confusion_matrix)), 9L) +
    pad
  out <- paste0(
    out,
    show_table(x@confusion_matrix, pad = tblpad, output_type = output_type)
  )
  out <- paste0(
    out,
    "\n",
    show_df(
      label_metric_df(x@metrics[["overall"]]),
      pad = pad,
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2L,
      output_type = output_type
    )
  )

  if (is.na(x@metrics[["positive_class"]])) {
    out <- paste0(
      out,
      show_df(
        label_metric_df(x@metrics[["class"]]),
        pad = pad,
        transpose = TRUE,
        ddSci_dp = decimal_places,
        justify = "left",
        spacing = 2,
        output_type = output_type
      )
    )
  } else {
    out <- paste0(
      out,
      "\n     Positive Class ",
      fmt(
        x@metrics[["positive_class"]],
        col = highlight_col,
        bold = TRUE,
        output_type = output_type
      ),
      "\n"
    )
  }
  out
} # /rtemis::repr.ClassificationMetrics


# %% print.ClassificationMetrics ----
method(print, ClassificationMetrics) <- function(
  x,
  decimal_places = 3,
  pad = 0L,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(
    x,
    decimal_places = decimal_places,
    pad = pad,
    output_type = output_type
  ))
  invisible(x)
} # /rtemis::print.ClassificationMetrics


# %% MetricsRes ----
#' @title MetricsRes
#'
#' @description
#' Superclass for MetricsRes metrics.
#'
#' @field sample Character: Sample name.
#'
#' @author EDG
#' @noRd
MetricsRes <- new_class(
  name = "MetricsRes",
  properties = list(
    sample = class_character | NULL,
    res_metrics = class_list,
    mean_metrics = class_data.frame,
    sd_metrics = class_data.frame
  )
) # /rtemis::MetricsRes


# %% repr.MetricsRes ----
method(repr, MetricsRes) <- function(
  x,
  decimal_places = 3L,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  type <- if (S7_inherits(x, RegressionMetricsRes)) {
    "Regression"
  } else {
    "Classification"
  }
  out <- repr_S7name(
    paste("Resampled", type, x@sample, "Metrics"),
    pad = pad,
    output_type = output_type
  )
  # Confusion Matrix
  if (type == "Classification") {
    tblpad <- 17L -
      max(nchar(colnames(x@confusion_matrix)), 9L) +
      pad
    out <- paste0(
      out,
      strrep(" ", pad),
      italic(
        "Aggregate Confusion Matrix across resamples.\n",
        output_type = output_type
      ),
      show_table(x@confusion_matrix, pad = tblpad, output_type = output_type),
      "\n"
    )
  }
  out <- paste0(out, strrep(" ", pad))
  out <- paste0(
    out,
    italic("Showing mean (sd) across resamples.\n", output_type = output_type)
  )
  # Create list with mean_metrics (sd_metrics)
  metricsl <- lapply(seq_along(x@mean_metrics), function(i) {
    paste0(
      ddSci(x@mean_metrics[[i]], decimal_places),
      gray(
        paste0(" (", ddSci(x@sd_metrics[[i]], decimal_places), ")"),
        output_type = output_type
      )
    )
  })
  names(metricsl) <- label_metrics(names(x@mean_metrics))
  out <- paste0(
    out,
    repr_ls(
      metricsl,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad,
      output_type = output_type
    )
  )
  out
} # /rtemis::repr.MetricsRes


# %% print.MetricsRes ----
method(print, MetricsRes) <- function(
  x,
  decimal_places = 3L,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, decimal_places, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.MetricsRes


# %% RegressionMetricsRes ----
#' @author EDG
#' @noRd
RegressionMetricsRes <- new_class(
  name = "RegressionMetricsRes",
  parent = MetricsRes,
  constructor = function(sample, res_metrics) {
    new_object(
      MetricsRes(
        sample = sample,
        res_metrics = res_metrics,
        mean_metrics = vec2df(
          colMeans(do.call(rbind, lapply(res_metrics, function(x) x@metrics)))
        ),
        sd_metrics = vec2df(
          sapply(do.call(rbind, lapply(res_metrics, function(x) x@metrics)), sd)
        )
      )
    )
  }
) # /rtemis::RegressionMetricsRes


#' @author EDG
#' @noRd
ClassificationMetricsRes <- new_class(
  name = "ClassificationMetricsRes",
  parent = MetricsRes,
  properties = list(
    confusion_matrix = class_table
  ),
  constructor = function(sample, confusion_matrix, res_metrics) {
    new_object(
      confusion_matrix = confusion_matrix,
      MetricsRes(
        sample = sample,
        res_metrics = res_metrics,
        mean_metrics = vec2df(
          colMeans(do.call(
            rbind,
            lapply(res_metrics, function(x) x@metrics[["overall"]])
          ))
        ),
        sd_metrics = vec2df(
          sapply(
            do.call(
              rbind,
              lapply(res_metrics, function(x) x@metrics[["overall"]])
            ),
            sd
          )
        )
      )
    )
  }
) # /rtemis::ClassificationMetricsRes


# %% repr.CalibratedClassification ----
#' @param x `ClassificationMetrics` before calibration.
#' @param x_cal `ClassificationMetrics` after calibration.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
repr_CalibratedClassificationMetrics <- function(
  x,
  x_cal,
  decimal_places = 2L,
  pad = 2L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)

  if (!is.null(x@sample)) {
    out <- repr_S7name(
      paste(x@sample, "Classification Metrics (Pre => Post Calibration)"),
      pad = pad,
      output_type = output_type
    )
  } else {
    out <- repr_S7name(
      "Classification Metrics (Pre => Post Calibration)",
      pad = pad,
      output_type = output_type
    )
  }

  # Confusion Matrix: Pre=>Post
  prepost_cm <- paste_tables(
    x@confusion_matrix,
    x_cal@confusion_matrix,
    sep = " => "
  )
  tblpad <- 17L -
    max(nchar(colnames(prepost_cm)), 9L) +
    pad
  out <- paste0(
    out,
    show_table(prepost_cm, pad = tblpad, output_type = output_type)
  )

  # Overall metrics: Pre=>Post
  # Note: decimal formatting handled by paste_dfs with decimal_places parameter
  out <- paste0(
    out,
    "\n",
    show_df(
      label_metric_df(paste_dfs(
        x@metrics[["overall"]],
        x_cal@metrics[["overall"]],
        sep = " => ",
        decimal_places = decimal_places
      )),
      pad = pad,
      transpose = TRUE,
      ddSci_dp = NULL,
      justify = "left",
      spacing = 2L,
      output_type = output_type
    )
  )

  # Class metrics: Pre=>Post (for multiclass) or Positive Class (for binary)
  if (is.na(x@metrics[["positive_class"]])) {
    out <- paste0(
      out,
      show_df(
        label_metric_df(paste_dfs(
          x@metrics[["class"]],
          x_cal@metrics[["class"]],
          decimal_places = decimal_places
        )),
        pad = pad,
        transpose = TRUE,
        ddSci_dp = NULL,
        justify = "left",
        spacing = 2,
        output_type = output_type
      )
    )
  } else {
    out <- paste0(
      out,
      "\n     Positive Class ",
      fmt(
        x@metrics[["positive_class"]],
        col = highlight_col,
        bold = TRUE,
        output_type = output_type
      ),
      "\n"
    )
  }
  out
} # /rtemis::repr_CalibratedClassification


# %% repr.CalibratedClassificationResMetrics ----
#' @param x `ClassificationMetricsRes` before calibration.
#' @param x_cal `ClassificationMetricsRes` after calibration.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
repr_CalibratedClassificationResMetrics <- function(
  x,
  x_cal,
  decimal_places = 2L,
  pad = 2L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)

  out <- repr_S7name(
    paste(
      "Resampled Classification",
      x@sample,
      "Metrics (Pre => Post Calibration)"
    ),
    pad = pad,
    output_type = output_type
  )
  out <- paste0(out, strrep(" ", pad))
  out <- paste0(
    out,
    italic(
      "Showing mean (sd) across resamples, Pre => Post calibration.\n",
      output_type = output_type
    )
  )

  # Create pre and post formatted strings: mean (sd)
  pre_strings <- lapply(seq_along(x@mean_metrics), function(i) {
    paste0(
      ddSci(x@mean_metrics[[i]], decimal_places),
      gray(
        paste0(" (", ddSci(x@sd_metrics[[i]], decimal_places), ")"),
        output_type = output_type
      )
    )
  })
  names(pre_strings) <- names(x@mean_metrics)

  post_strings <- lapply(seq_along(x_cal@mean_metrics), function(i) {
    paste0(
      ddSci(x_cal@mean_metrics[[i]], decimal_places),
      gray(
        paste0(" (", ddSci(x_cal@sd_metrics[[i]], decimal_places), ")"),
        output_type = output_type
      )
    )
  })
  names(post_strings) <- names(x_cal@mean_metrics)

  # Combine pre=>post
  prepost_strings <- lapply(seq_along(pre_strings), function(i) {
    paste(pre_strings[[i]], post_strings[[i]], sep = " => ")
  })
  names(prepost_strings) <- names(pre_strings)

  out <- paste0(
    out,
    repr_ls(
      prepost_strings,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad,
      output_type = output_type
    )
  )
  out
} # /rtemis::repr_CalibratedClassificationResMetrics
