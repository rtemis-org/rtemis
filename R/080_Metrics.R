# 080_Metrics.R
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


# %% confusion_to_long() ----
# A confusion matrix in row-oriented form: one row per cell, with the two
# outcome levels as declared columns. `as.data.frame()` on a `table` yields
# exactly this long form; the columns are renamed and retyped by position so a
# `table` from any source converts cleanly.
confusion_to_long <- function(cm) {
  df <- as.data.frame(cm, stringsAsFactors = FALSE)
  data.frame(
    reference = as.character(df[[1L]]),
    predicted = as.character(df[[2L]]),
    n = as.integer(df[[3L]]),
    row.names = NULL
  )
}


# %% long_to_confusion() ----
# The wide `table` view of the long form, which is the stored one. Level order
# is preserved by construction: the long form enumerates cells with `reference`
# varying fastest, which is also the order `matrix()` fills, so taking each
# column's distinct values in order recovers the levels as they were.
long_to_confusion <- function(long) {
  reference <- unique(long[["reference"]])
  predicted <- unique(long[["predicted"]])
  as.table(matrix(
    long[["n"]],
    nrow = length(reference),
    ncol = length(predicted),
    dimnames = list(Reference = reference, Predicted = predicted)
  ))
}


# %% prop_confusion_long ----
#' The stored confusion matrix: one row per cell
#'
#' A `table`'s column names are the outcome levels, so they are data rather than
#' a declarable column set and no schema can describe them. The long form turns
#' them into two declared columns and loses nothing, so it is what the class
#' stores and publishes; `confusion_matrix` is the wide view derived from it.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_confusion_long <- function() {
  prop_state(prop_table(
    # Every cell is populated for every case pair, so unlike a metric none of
    # these is nullable. Their defaults are unused, a column spec describing a
    # cell rather than a value.
    columns = list(
      reference = prop_string("", description = "True outcome level."),
      predicted = prop_string("", description = "Predicted outcome level."),
      n = prop_integer(0L, min = 0L, description = "Number of cases.")
    ),
    nullable = TRUE,
    description = "Confusion matrix, one row per cell."
  ))
} # /rtemis::prop_confusion_long


# %% Row labels for printing ----
# The stored tables carry no row names: `overall` has a single row and `class`
# names its rows in a `level` column, neither of which has a row-oriented JSON
# form. Both are restored on a copy for display only.
overall_df_for_print <- function(df) {
  rownames(df) <- "overall"
  df
}

class_df_for_print <- function(df) {
  rownames(df) <- df[["level"]]
  df[["level"]] <- NULL
  df
}


# %% label_metrics() ----
# Prettify metric names for printing (e.g. balanced_accuracy -> "Balanced
# Accuracy", auc -> "AUC", rsq -> "R^2"). labelify() uppercases the acronyms
# via its capitalize_strings defaults; R-squared gets a Unicode superscript two.
# Stored field names stay lowercase.
CAP_METRICS <- c("mae", "mse", "rmse", "oos")
label_metrics <- function(x) {
  sub("^Rsq$", "R\u00b2", labelify(x, capitalize_strings = CAP_METRICS))
}

# Apply label_metrics() to a metric data.frame's row and column names.
label_metric_df <- function(df) {
  colnames(df) <- label_metrics(colnames(df))
  rownames(df) <- label_metrics(rownames(df))
  df
}


# %% prop_metric ----
#' A metric cell: a number that may be missing
#'
#' Every metric column is nullable. A metric can be genuinely undefined for a
#' sample -- F1 when precision and recall are both zero, sensitivity for a class
#' with no cases, AUC when its backend fails -- and NA is the honest answer
#' rather than a reason to reject the whole object. Distinct from a column being
#' *absent*, which says the metric was not computed for this task at all.
#'
#' @param min Numeric, optional: Lower bound.
#' @param max Numeric, optional: Upper bound.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_metric <- function(min = NULL, max = NULL, description = "") {
  prop_float(
    NULL,
    min = min,
    max = max,
    nullable = TRUE,
    description = description
  )
} # /rtemis::prop_metric


# %% prop_rate ----
#' A metric cell bounded to \[0, 1\]
#'
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_rate <- function(description = "") {
  prop_metric(min = 0, max = 1, description = description)
} # /rtemis::prop_rate


# %% regression_metric_columns ----
# The regression metric set, declared once: a sample's metrics and their mean
# and standard deviation across resamples all carry these columns.
regression_metric_columns <- function() {
  list(
    mae = prop_metric(min = 0),
    mse = prop_metric(min = 0),
    rmse = prop_metric(min = 0),
    rsq = prop_metric(max = 1)
  )
}


# %% classification_overall_columns ----
# Which columns are present depends on the task: multiclass gets the three
# always-present ones, binary adds the positive class's rates, and the
# probability-based pair needs predicted probabilities. Every column is
# declared; only the invariant ones are required.
classification_overall_columns <- function() {
  list(
    balanced_accuracy = prop_rate(),
    f1 = prop_rate(),
    accuracy = prop_rate(),
    sensitivity = prop_rate(),
    specificity = prop_rate(),
    ppv = prop_rate(),
    npv = prop_rate(),
    auc = prop_rate(),
    brier_score = prop_rate()
  )
}

CLASSIFICATION_OVERALL_REQUIRED <- c("balanced_accuracy", "f1", "accuracy")


# %% METRICS_SAMPLES ----
# Which sample a metrics object describes. A closed set: rtemis names the
# samples itself, so a value outside it is a typo rather than a new case.
METRICS_SAMPLES <- c(
  "Training",
  "Validation",
  "Test",
  "Calibrated Training",
  "Calibrated Validation",
  "Calibrated Test"
)


# %% prop_metrics_sample ----
#' The sample a metrics object describes
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_metrics_sample <- function() {
  prop_state(prop_string(
    NULL,
    enum = METRICS_SAMPLES,
    nullable = TRUE,
    description = "Sample the metrics describe."
  ))
} # /rtemis::prop_metrics_sample


# %% Metrics ----
#' Metrics
#'
#' @description
#' Superclass for Metrics metrics.
#'
#' Holds only what every metrics class shares. `metrics` is declared by each
#' concrete subclass, whose payload differs in both shape and columns: a
#' regression sample is one table, a classification sample is a struct of two
#' tables and a scalar. Declaring an untyped union here as well would put a
#' second, weaker contract above the typed ones.
#'
#' @field sample Character: Sample name.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Metrics <- new_class(
  name = "Metrics",
  package = "rtemis",
  properties = list(
    sample = prop_metrics_sample()
  )
) # /rtemis::Metrics


# %% metric_names ----
# Everything `$` and `[[` resolve: the object's own properties first, then the
# members of `@metrics`. Reaching only into `@metrics` would leave the fields
# that are properties in their own right -- the confusion matrix and its long
# view -- unreachable by the accessor that reaches everything else.
metric_names <- function(x) {
  union(prop_names(x), names(x@metrics))
}


# %% metric_get ----
metric_get <- function(x, name) {
  if (name %in% prop_names(x)) {
    return(prop(x, name))
  }
  x@metrics[[name]]
}


# %% `$`.Metrics ----
method(`$`, Metrics) <- function(x, name) {
  metric_get(x, name)
}


# %% `.DollarNames`.Metrics ----
method(`.DollarNames`, Metrics) <- function(x, pattern = "") {
  grep(pattern, metric_names(x), value = TRUE)
}


# %% `[[`.Metrics ----
method(`[[`, Metrics) <- function(x, name) {
  metric_get(x, name)
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
  properties = list(
    # A one-row table. Every metric is nullable: a metric can be genuinely
    # undefined for a sample (R-squared when the outcome has no variance), and
    # NA is the honest answer rather than a reason to reject the object.
    metrics = prop_state(prop_table(
      columns = regression_metric_columns(),
      nullable = TRUE,
      description = "Regression metrics, one row."
    ))
  ),
  constructor = function(mae, mse, rmse, rsq, sample = NULL) {
    new_object(
      Metrics(sample = sample),
      metrics = data.frame(
        mae = mae,
        mse = mse,
        rmse = rmse,
        rsq = rsq
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
  output_type = NULL,
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
    confusion_long = prop_confusion_long(),
    # The wide `table` R users and the plotting code expect, rebuilt from the
    # stored long form. Recoverable from a published field, so it is a view
    # rather than a second stored representation that could disagree.
    confusion_matrix = prop_computed(new_property(
      class = class_table,
      getter = function(self) long_to_confusion(self@confusion_long)
    )),
    metrics = prop_state(prop_struct(
      members = list(
        overall = prop_table(
          columns = classification_overall_columns(),
          required = CLASSIFICATION_OVERALL_REQUIRED,
          nullable = TRUE,
          description = "Overall metrics, one row."
        ),
        class = prop_table(
          columns = list(
            level = prop_string(NULL, nullable = TRUE),
            sensitivity = prop_rate(),
            specificity = prop_rate(),
            balanced_accuracy = prop_rate(),
            ppv = prop_rate(),
            npv = prop_rate(),
            f1 = prop_rate()
          ),
          nullable = TRUE,
          description = "Per-class metrics, one row per outcome level."
        ),
        # Absent unless the outcome is binary, which is why it is not required.
        positive_class = prop_string(
          NULL,
          nullable = TRUE,
          description = "Outcome level treated as positive."
        )
      ),
      required = c("overall", "class"),
      nullable = TRUE,
      description = "Classification metrics."
    ))
  ),
  constructor = function(
    confusion_matrix,
    overall,
    class,
    positive_class,
    sample = NULL
  ) {
    new_object(
      Metrics(sample = sample),
      confusion_long = confusion_to_long(confusion_matrix),
      metrics = list(
        overall = overall,
        class = class,
        positive_class = positive_class
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
      label_metric_df(overall_df_for_print(x@metrics[["overall"]])),
      pad = pad,
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2L,
      output_type = output_type
    )
  )

  if (is.null(x@metrics[["positive_class"]])) {
    out <- paste0(
      out,
      show_df(
        label_metric_df(class_df_for_print(x@metrics[["class"]])),
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
  output_type = NULL,
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
#' As with `Metrics`, only the shared field is declared here: the per-resample
#' metrics and their mean and standard deviation all carry the concrete task's
#' columns, so each subclass declares its own.
#'
#' @field sample Character: Sample name.
#'
#' @author EDG
#' @noRd
MetricsRes <- new_class(
  name = "MetricsRes",
  properties = list(
    sample = prop_metrics_sample()
  )
) # /rtemis::MetricsRes


# %% prop_res_metrics ----
#' Per-resample metrics
#'
#' A list holding one metrics object per resample. Declared without a
#' `PropertySpec`: its elements are S7 objects with schemas of their own, which
#' the generator wires up as an array of `$ref`s.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_res_metrics <- function() {
  new_property(class_list)
} # /rtemis::prop_res_metrics


# %% prop_mean_metrics ----
#' Mean of a metric across resamples
#'
#' @param columns Named list of column specs: The metric columns being
#'   averaged, so the mean carries the same bounds as the values.
#' @param required Character, optional: Names of the always-present columns.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_mean_metrics <- function(columns, required = NULL) {
  prop_state(prop_table(
    columns = columns,
    required = required,
    nullable = TRUE,
    description = "Mean of each metric across resamples, one row."
  ))
} # /rtemis::prop_mean_metrics


# %% prop_sd_metrics ----
#' Standard deviation of a metric across resamples
#'
#' The same columns as `prop_mean_metrics()`, bounded only below: a dispersion is
#' non-negative but is not confined to the range of the values it summarizes --
#' R-squared is bounded above by 1 and unbounded below, so its standard
#' deviation across resamples can exceed 1.
#'
#' @param columns Named list of column specs: The metric columns summarized.
#' @param required Character, optional: Names of the always-present columns.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_sd_metrics <- function(columns, required = NULL) {
  prop_state(prop_table(
    columns = lapply(columns, function(...) prop_metric(min = 0)),
    required = required,
    nullable = TRUE,
    description = "Standard deviation of each metric across resamples, one row."
  ))
} # /rtemis::prop_sd_metrics


# %% repr.MetricsRes ----
method(repr, MetricsRes) <- function(
  x,
  decimal_places = 3L,
  pad = 0L,
  output_type = NULL
) {
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
  properties = list(
    res_metrics = prop_res_metrics(),
    mean_metrics = prop_mean_metrics(regression_metric_columns()),
    sd_metrics = prop_sd_metrics(regression_metric_columns())
  ),
  constructor = function(sample, res_metrics) {
    stacked <- do.call(rbind, lapply(res_metrics, function(x) x@metrics))
    new_object(
      MetricsRes(sample = sample),
      res_metrics = res_metrics,
      mean_metrics = vec2df(colMeans(stacked)),
      sd_metrics = vec2df(sapply(stacked, sd))
    )
  }
) # /rtemis::RegressionMetricsRes


#' @author EDG
#' @noRd
ClassificationMetricsRes <- new_class(
  name = "ClassificationMetricsRes",
  parent = MetricsRes,
  properties = list(
    confusion_long = prop_confusion_long(),
    confusion_matrix = prop_computed(new_property(
      class = class_table,
      getter = function(self) long_to_confusion(self@confusion_long)
    )),
    res_metrics = prop_res_metrics(),
    # Aggregated from each resample's `overall` table, so those are its columns.
    mean_metrics = prop_mean_metrics(
      classification_overall_columns(),
      required = CLASSIFICATION_OVERALL_REQUIRED
    ),
    sd_metrics = prop_sd_metrics(
      classification_overall_columns(),
      required = CLASSIFICATION_OVERALL_REQUIRED
    )
  ),
  constructor = function(sample, confusion_matrix, res_metrics) {
    stacked <- do.call(
      rbind,
      lapply(res_metrics, function(x) x@metrics[["overall"]])
    )
    new_object(
      MetricsRes(sample = sample),
      confusion_long = confusion_to_long(confusion_matrix),
      res_metrics = res_metrics,
      mean_metrics = vec2df(colMeans(stacked)),
      sd_metrics = vec2df(sapply(stacked, sd))
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
  if (is.null(x@metrics[["positive_class"]])) {
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
