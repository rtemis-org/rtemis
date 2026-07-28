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


# %% prop_metric ----
#' A metric cell: a number that may be missing
#'
#' Every metric column is nullable. A metric can be genuinely undefined for a
#' sample — F1 when precision and recall are both zero, sensitivity for a class
#' with no cases, AUC when its backend fails — and NA is the honest answer
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
#' A metric cell bounded to [0, 1]
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
    sample = NULL | class_character,
    metrics = class_list | class_data.frame
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
    metrics = prop_table(
      columns = list(
        mae = prop_metric(min = 0),
        mse = prop_metric(min = 0),
        rmse = prop_metric(min = 0),
        rsq = prop_metric(max = 1)
      ),
      nullable = TRUE,
      description = "Regression metrics, one row."
    )
  ),
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
    confusion_matrix = class_table,
    # A derived view of `confusion_matrix` in the row-oriented form a data frame
    # reader expects. A `table`'s column names are the outcome levels, so they
    # are data, not a declarable column set; the long form turns them into two
    # declared columns and loses nothing.
    confusion_long = prop_computed(new_property(
      class = class_data.frame,
      getter = function(self) confusion_to_long(self@confusion_matrix)
    )),
    metrics = prop_struct(
      members = list(
        # Which columns `overall` carries depends on the task: multiclass gets
        # the three always-present ones, binary adds the positive class's rates,
        # and the probability-based pair needs predicted probabilities. Every
        # column is declared; only the invariant ones are required.
        overall = prop_table(
          columns = list(
            balanced_accuracy = prop_rate(),
            f1 = prop_rate(),
            accuracy = prop_rate(),
            sensitivity = prop_rate(),
            specificity = prop_rate(),
            ppv = prop_rate(),
            npv = prop_rate(),
            auc = prop_rate(),
            brier_score = prop_rate()
          ),
          required = c("balanced_accuracy", "f1", "accuracy"),
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
    )
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


# %% to_json.ClassificationMetrics ----
#' @name to_json
#' @keywords internal
#' @noRd
method(to_json, ClassificationMetrics) <- function(x, ...) {
  # The default method walks every property, which would hand `jsonlite` the
  # raw `confusion_matrix`: a `table` has no `asJSON` method, and its column
  # names are outcome levels rather than a declarable set. `confusion_long`
  # carries the same counts in row-oriented form, so the wide table is omitted
  # here rather than converted downstream by each consumer in turn.
  list(
    .class = S7_class(x)@name,
    sample = x@sample,
    metrics = .to_json_value(x@metrics),
    confusion_long = x@confusion_long
  )
} # /rtemis::to_json.ClassificationMetrics


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
#' @field sample Character: Sample name.
#'
#' @author EDG
#' @noRd
MetricsRes <- new_class(
  name = "MetricsRes",
  properties = list(
    sample = NULL | class_character,
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
    confusion_matrix = class_table,
    confusion_long = prop_computed(new_property(
      class = class_data.frame,
      getter = function(self) confusion_to_long(self@confusion_matrix)
    ))
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


# %% to_json.ClassificationMetricsRes ----
#' @name to_json
#' @keywords internal
#' @noRd
method(to_json, ClassificationMetricsRes) <- function(x, ...) {
  # Same reason as the single-fit method: the aggregate confusion matrix is a
  # `table` and travels in its long form.
  list(
    .class = S7_class(x)@name,
    sample = x@sample,
    res_metrics = .to_json_value(x@res_metrics),
    mean_metrics = x@mean_metrics,
    sd_metrics = x@sd_metrics,
    confusion_long = x@confusion_long
  )
} # /rtemis::to_json.ClassificationMetricsRes


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
