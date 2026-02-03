# S7_Metrics.R
# ::rtemis::
# 2025- EDG rtemis.org

# Metrics ----
#' @title Metrics
#'
#' @description
#' Superclass for Metrics metrics.
#'
#' @field sample Character: Sample name.
#' @field metrics List or data.frame: Metrics.
#'
#' @author EDG
#' @noRd
Metrics <- new_class(
  name = "Metrics",
  properties = list(
    sample = class_character,
    metrics = class_list | class_data.frame
  )
) # /rtemis::Metrics

# Make Metrics@metrics `$`-accessible
method(`$`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}

# `$`-autocomplete Metrics@metrics
method(`.DollarNames`, Metrics) <- function(x, pattern = "") {
  all_names <- names(x@metrics)
  grep(pattern, all_names, value = TRUE)
}

# Make Metrics@metrics `[[`-accessible
method(`[[`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}


# RegressionMetrics ----
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
  constructor = function(MAE, MSE, RMSE, Rsq, sample = character()) {
    new_object(
      Metrics(
        sample = sample,
        metrics = data.frame(
          MAE = MAE,
          MSE = MSE,
          RMSE = RMSE,
          Rsq = Rsq
        )
      )
    )
  }
) # /rtemis::RegressionMetrics

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
      x@metrics,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad + 2L,
      output_type = output_type
    )
  )
  out
} # /rtemis::show.RegressionMetrics


# Print RegressionMetrics ----
method(print, RegressionMetrics) <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.RegressionMetrics


# ClassificationMetrics ----
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
  constructor = function(
    Confusion_Matrix,
    Overall,
    Class,
    Positive_Class,
    sample = character()
  ) {
    new_object(
      Metrics(
        sample = sample,
        metrics = list(
          Confusion_Matrix = Confusion_Matrix,
          Overall = Overall,
          Class = Class,
          Positive_Class = Positive_Class
        )
      )
    )
  }
) # /rtemis::ClassificationMetrics


# Show ClassificationMetrics ----
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
    max(nchar(colnames(x@metrics[["Confusion_Matrix"]])), 9L) +
    pad
  out <- paste0(
    out,
    show_table(x[["Confusion_Matrix"]], pad = tblpad, output_type = output_type)
  )
  out <- paste0(
    out,
    "\n",
    show_df(
      x@metrics[["Overall"]],
      pad = pad,
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2L,
      output_type = output_type
    )
  )

  if (is.na(x@metrics[["Positive_Class"]])) {
    out <- paste0(
      out,
      show_df(
        x@metrics[["Class"]],
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
        x@metrics[["Positive_Class"]],
        col = highlight_col,
        bold = TRUE,
        output_type = output_type
      ),
      "\n"
    )
  }
  out
} # /rtemis::show.ClassificationMetrics


# Print ClassificationMetrics ----
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


# MetricsRes ----
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


# Show MetricsRes ----
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
  out <- paste0(out, strrep(" ", pad))
  out <- paste0(
    out,
    italic("  Showing mean (sd) across resamples.\n", output_type = output_type)
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
  names(metricsl) <- names(x@mean_metrics)
  out <- paste0(
    out,
    repr_ls(
      metricsl,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad + 2L,
      output_type = output_type
    )
  )
  out
} # /rtemis::show.MetricsRes


# Print MetricsRes ----
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


# RegressionMetricsRes ----

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
  constructor = function(sample, res_metrics) {
    new_object(
      MetricsRes(
        sample = sample,
        res_metrics = res_metrics,
        mean_metrics = vec2df(
          colMeans(do.call(
            rbind,
            lapply(res_metrics, function(x) x@metrics[["Overall"]])
          ))
        ),
        sd_metrics = vec2df(
          sapply(
            do.call(
              rbind,
              lapply(res_metrics, function(x) x@metrics[["Overall"]])
            ),
            sd
          )
        )
      )
    )
  }
) # /rtemis::ClassificationMetricsRes
