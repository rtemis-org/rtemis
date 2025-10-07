# S7_init.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# S7 generics: https://rconsortium.github.io/S7/articles/generics-methods.html

#' String representation
#'
#' @details
#' Exported as internal function for use by other rtemis packages.
#'
#' @keywords internal
#' @export
repr <- new_generic("repr", "x")
# Standard error of the fit.
se <- new_generic("se", "x")
# Short description for inline printing.
desc <- new_generic("desc", "x")
# Alt description for inline printing.
desc_alt <- new_generic("desc_alt", "x")
# Get metrics
get_metric <- new_generic("get_metric", "x")
# check hyperparameters given training data
validate_hyperparameters <- new_generic("validate_hyperparameters", "x")

#' Plot Metric
#'
#' @description
#' Plot metric for Supervised or SupervisedRes objects.
#'
#' @param x Supervised or SupervisedRes object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return plotly object
#'
#' @author EDG
#' @export
plot_metric <- new_generic("plot_metric", "x")


#' Plot ROC curve
#'
#' @description
#' This generic is used to plot the ROC curve for a model.
#'
#' @param x Classification or ClassificationRes object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object containing the ROC curve.
#'
#' @author EDG
#' @export
plot_roc <- new_generic("plot_roc", "x")


#' Plot Variable Importance
#'
#' @description
#' Plot Variable Importance for Supervised objects.
#'
#' @param x Supervised or SupervisedRes object.
# @param theme Theme object.
# @param filename Character: Filename to save the plot to. If NULL, the plot is not saved.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return plotly object or invisible NULL if no variable importance is available.
#'
#' @author EDG
#' @export
plot_varimp <- new_generic("plot_varimp", "x")


#' Plot True vs. Predicted Values
#'
#' @description
#' Plot True vs. Predicted Values for Supervised objects.
#' For classification, it plots a confusion matrix.
#' For regression, it plots a scatter plot of true vs. predicted values.
#'
#' @param x Supervised or SupervisedRes object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return plotly object.
#'
#' @author EDG
#' @export
plot_true_pred <- new_generic("plot_true_pred", "x")


#' Manhattan plot
#'
#' @description
#' Draw a Manhattan plot for MassGLM objects created with [massGLM].
#'
#' @param x MassGLM object.
#' @param ... Additional arguments passed to the plotting function.
plot_manhattan <- new_generic("plot_manhattan", "x")

#' Describe rtemis object
#'
#' @description
#' This generic is used to provide a description of an rtemis object in plain language.
#'
#' @param x `Supervised` or `SupervisedRes` object or list of such objects.
#' @param ... Not used.
#'
#' @return A character string describing the object.
#'
#' @author EDG
#' @export
describe <- new_generic("describe", "x")

#' Present rtemis object
#'
#' @description
#' This generic is used to present an rtemis object using plots and text.
#'
#' @param x `Supervised` or `SupervisedRes` object or list of such objects.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object along with console output.
#'
#' @author EDG
#' @export
present <- new_generic("present", "x")

# Get parameters that need tuning.
get_params_need_tuning <- new_generic("get_params_need_tuning", "x")
# Get parameters.
get_params <- new_generic("get_params", c("x", "param_names"))
# Extract rules from a model.
extract_rules <- new_generic("extract_rules", "x")

# S3 Classes ----
class_data.table <- new_S3_class("data.table")
class_lgb.Booster <- new_S3_class("lgb.Booster")

#' Custom S7 validators
#'
#' @description
#' A collection of custom S7 validators used in rtemis.
#'
#' @keywords internal
#' @noRd
scalar_dbl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (!is.double(value)) {
        "must be double."
      }
    }
  }
) # /scalar_dbl

#' @keywords internal
#' @noRd
scalar_dbl_01excl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (value <= 0 || value >= 1) {
        "must be between > 0 and < 1."
      }
    }
  }
) # /scalar_dbl_01excl

#' @keywords internal
#' @noRd
scalar_dbl_01incl <- S7::new_property(
  class = S7::class_double | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar double."
      } else if (value < 0 || value > 1) {
        "must be between >= 0 and <= 1."
      }
    }
  }
) # /scalar_dbl_01incl

#' @keywords internal
#' @noRd
scalar_int <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a scalar integer."
      }
    }
  }
) # /scalar_int

#' @keywords internal
#' @noRd
scalar_int_pos <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a positive integer scalar."
      } else if (value < 0) {
        "must be >= 0."
      }
    }
  }
) # /scalar_int_pos


#' @keywords internal
#' @noRd
scalar_int_12 <- S7::new_property(
  class = S7::class_integer | NULL,
  validator = function(value) {
    if (!is.null(value)) {
      if (length(value) != 1) {
        "must be a positive integer scalar."
      } else if (!value %in% 1:2) {
        "must be 1 or 2."
      }
    }
  }
) # /scalar_int_12

# data.frame data.table compatibility ----
#' Select columns by character or numeric vector.
#'
#' @param x data.frame or similar.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @keywords internal
#' @noRd
.. <- identity
inc <- new_generic("inc", "x")
exc <- new_generic("exc", c("x", "idx"))
method(inc, class_data.frame) <- function(x, idx) {
  x[, idx, drop = FALSE]
}
# may cause R CMD check note, consider defining `..` or using the `with = FALSE` approach instead.
method(inc, class_data.table) <- function(x, idx) {
  x[, ..idx]
}

method(exc, list(class_data.frame, class_character)) <- function(x, idx) {
  x[, -which(names(x) %in% idx), drop = FALSE]
}
method(exc, list(class_data.frame, class_integer)) <- function(x, idx) {
  x[, -idx, drop = FALSE]
}
method(exc, list(class_data.frame, class_double)) <- function(x, idx) {
  idx <- clean_int(idx)
  x[, -idx, drop = FALSE]
}
method(
  exc,
  list(class_data.table, class_character | class_integer)
) <- function(x, idx) {
  x[, !..idx]
}
method(exc, list(class_data.table, class_double)) <- function(x, idx) {
  idx <- clean_int(idx)
  x[, !idx]
}

#' Get the name of the last column
#'
#' @param x data.frame or similar.
#'
#' @return Name of the last column.
#'
#' @keywords internal
#' @noRd
outcome_name <- new_generic("outcome_name", "x")
method(outcome_name, class_data.frame) <- function(x) {
  names(x)[NCOL(x)]
}

#' Get last column as a vector
#'
#' @param x data.frame or similar.
#' @param ... Not used.
#'
#' @return Vector containing the last column of `x`.
#'
#' @author EDG
#' @export
outcome <- new_generic("outcome", "x")
method(outcome, class_data.frame) <- function(x) {
  x[[NCOL(x)]]
}

#' Get features (all columns except the last one)
#'
#' @param x data.frame or similar.
#' @param ... Not used.
#'
#' @return object same as input, after removing the last column.
#'
#' @author EDG
#' @export
features <- new_generic("features", "x")
method(features, class_data.frame) <- function(x) {
  stopifnot(NCOL(x) > 1)
  x[, 1:(NCOL(x) - 1), drop = FALSE]
}

feature_names <- new_generic("feature_names", "x")
method(feature_names, class_data.frame) <- function(x) {
  names(x)[1:(NCOL(x) - 1)]
}

#' Get factor names
#'
#' @param x data.frame or similar.
#'
#' @return Character vector of factor names.
#'
#' @keywords internal
#' @noRd
get_factor_names <- new_generic("get_factor_names", "x")
method(get_factor_names, class_data.frame) <- function(x) {
  names(x)[sapply(x, is.factor)]
}

#' Get output type
#'
#' Get output type for printing text.
#'
#' @param output_type Character vector of output types.
#' @param filename Character: Filename for output.
#'
#' @details
#' Exported as internal function for use by other rtemis packages.
#'
#' @return Character with selected output type.
#'
#' @author EDG
#'
#' @export
#' @keywords internal
#' @noRd

get_output_type <- function(
  output_type = c("ansi", "html", "plain"),
  filename = NULL
) {
  if (!is.null(filename)) {
    return("plain")
  }

  if (is.null(output_type)) {
    if (interactive()) {
      return("ansi")
    } else {
      return("plain")
    }
  }

  match.arg(output_type)
} # /rtemis::get_output_type
