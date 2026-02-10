# S7_init.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# S7 generics: https://rconsortium.github.io/S7/articles/generics-methods.html

# --- S3 Classes for S7 ----------------------------------------------------------------------------
class_data.table <- new_S3_class("data.table")
class_lgb.Booster <- new_S3_class("lgb.Booster")


# --- Generics -------------------------------------------------------------------------------------

#' String representation
#'
#' @return Character string representation of the object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr <- new_generic("repr", "x")

# Standard error of the fit.
se <- new_generic("se", "x")

#' Short description for inline printing.
#' This is like `repr` for single-line descriptions.
#'
#' @author EDG
#' @keywords internal
#' @noRd
desc <- new_generic("desc", "x")


#' Get metric
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_metric <- new_generic("get_metric", "x")


#' Check hyperparameters given training data
#'
#' @param x data.frame or similar: Training data.
#' @param hyperparameters `Hyperparameters` to check.
#'
validate_hyperparameters <- new_generic(
  "validate_hyperparameters",
  "x",
  function(x, hyperparameters) {
    S7_dispatch()
  }
) # /rtemis::validate_hyperparameters


#' Plot Metric
#'
#' @description
#' Plot metric for `SupervisedRes` objects.
#'
#' @param x `SupervisedRes` object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return plotly object
#'
#' @author EDG
#' @keywords internal
#' @noRd
plot_metric <- new_generic("plot_metric", "x")


#' Plot ROC curve
#'
#' @description
#' This generic is used to plot the ROC curve for a model.
#'
#' @param x `Classification` or `ClassificationRes` object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object containing the ROC curve.
#'
#' @author EDG
#' @export
#'
#' @examples
#' ir <- iris[51:150, ]
#' ir[["Species"]] <- factor(ir[["Species"]])
#' species_glm <- train(ir, algorithm = "GLM")
#' plot_roc(species_glm)
plot_roc <- new_generic("plot_roc", "x")


#' Plot Variable Importance
#'
#' @description
#' Plot Variable Importance for Supervised objects.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' This method calls [draw_varimp] internally.
#' If you pass an integer to the `plot_top` argument, the method will plot this many top features.
#' If you pass a number between 0 and 1 to the `plot_top` argument, the method will plot this
#' fraction of top features.
#'
#' @return plotly object or invisible NULL if no variable importance is available.
#'
#' @author EDG
#' @export
#'
#' @seealso [draw_varimp], which is called by this method
#'
#' @examples
#' ir <- set_outcome(iris, "Sepal.Length")
#' seplen_cart <- train(ir, algorithm = "CART")
#' plot_varimp(seplen_cart)
#' # Plot horizontally
#' plot_varimp(seplen_cart, orientation = "h")
#' plot_varimp(seplen_cart, orientation = "h", plot_top = 3L)
#' plot_varimp(seplen_cart, orientation = "h", plot_top = 0.5)
plot_varimp <- new_generic("plot_varimp", "x")


#' Plot True vs. Predicted Values
#'
#' @description
#' Plot True vs. Predicted Values for Supervised objects.
#' For classification, it plots a confusion matrix.
#' For regression, it plots a scatter plot of true vs. predicted values.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return plotly object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- set_outcome(iris, "Sepal.Length")
#' sepallength_glm <- train(x, algorithm = "GLM")
#' plot_true_pred(sepallength_glm)
plot_true_pred <- new_generic("plot_true_pred", "x")


#' Manhattan plot
#'
#' @description
#' Draw a Manhattan plot for `MassGLM` objects created with [massGLM].
#'
#' @param x `MassGLM` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return plotly object.
#'
#' @author EDG
#' @export
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
#'
#' @examples
#' species_lightrf <- train(iris, algorithm = "lightrf")
#' describe(species_lightrf)
describe <- new_generic("describe", "x")


#' Present rtemis object
#'
#' @description
#' This generic is used to present an rtemis object by printing to console and drawing plots.
#'
#' @param x `Supervised` or `SupervisedRes` object or list of such objects.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A plotly object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' ir <- set_outcome(iris, "Sepal.Length")
#' seplen_lightrf <- train(ir, algorithm = "lightrf")
#' present(seplen_lightrf)
present <- new_generic("present", "x")


#' Get hyperparameters that need tuning.
#'
#' @return Character vector of hyperparameter names that need tuning.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_hyperparams_need_tuning <- new_generic("get_hyperparams_need_tuning", "x")


#' Get hyperparameters.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_hyperparams <- new_generic("get_hyperparams", c("x", "param_names"))


#' Extract rules from a model.
#'
#' @author EDG
#' @keywords internal
#' @noRd
extract_rules <- new_generic("extract_rules", "x")


#' @name get_factor_levels
#'
#' @title
#' Get factor levels from data.frame or similar
#'
#' @usage
#' get_factor_levels(x)
#'
#' @param x data.frame or similar.
#'
#' @return Named list of factor levels. Names correspond to column names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_factor_levels <- new_generic(
  "get_factor_levels",
  "x",
  function(x) S7_dispatch()
)

method(get_factor_levels, class_data.frame) <- function(x) {
  factor_index <- which(sapply(x, is.factor))
  lapply(x[, factor_index, drop = FALSE], levels)
}

method(get_factor_levels, class_data.table) <- function(x) {
  factor_index <- which(sapply(x, is.factor))
  lapply(x[, factor_index, with = FALSE], levels)
}


#' Convert to TOML
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_toml <- new_generic("to_toml", "x")


#' Read from TOML
#'
#' @author EDG
#' @keywords internal
#' @noRd
from_toml <- new_generic("from_toml", "x")


#' Select (include) columns by character or numeric vector.
#'
#' @param x data.frame or similar.
#' @param idx Character or numeric vector: Column names or indices to include.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @author EDG
#' @export
#'
#' @examples
#' inc(iris, c(3, 4)) |> head()
#' inc(iris, c("Sepal.Length", "Species")) |> head()
inc <- new_generic("inc", "x", function(x, idx) {
  S7_dispatch()
})


#' Exclude columns by character or numeric vector.
#'
#' @param x data.frame or similar.
#' @param idx Character or numeric vector: Column names or indices to exclude.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @author EDG
#' @export
#'
#' @examples
#' exc(iris, "Species") |> head()
#' exc(iris, c(1, 3)) |> head()
exc <- new_generic("exc", c("x", "idx"), function(x, idx) {
  S7_dispatch()
})

method(inc, class_data.frame) <- function(x, idx) {
  x[, idx, drop = FALSE]
}

method(inc, class_data.table) <- function(x, idx) {
  x[, .SD, .SDcols = idx]
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
  x[, .SD, .SDcols = -idx]
}

method(exc, list(class_data.table, class_double)) <- function(x, idx) {
  idx <- clean_int(idx)
  x[, .SD, .SDcols = -idx]
}


#' Get the name of the last column
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x data.frame or similar.
#'
#' @return Name of the last column.
#'
#' @author EDG
#' @export
#'
#' @examples
#' outcome_name(iris)
outcome_name <- new_generic("outcome_name", "x", function(x) {
  S7_dispatch()
})

method(outcome_name, class_data.frame) <- function(x) {
  names(x)[NCOL(x)]
} # /rtemis::outcome_name


#' Get the outcome as a vector
#'
#' Returns the last column of `x`, which is by convention the outcome variable.
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x data.frame or similar.
#'
#' @return Vector containing the last column of `x`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' outcome(iris)
outcome <- new_generic("outcome", "x", function(x) {
  S7_dispatch()
}) # /rtemis::outcome

method(outcome, class_data.frame) <- function(x) {
  x[[NCOL(x)]]
}


#' Get features
#'
#' Returns all columns except the last one
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x data.frame or similar.
#'
#' @return object same as input, after removing the last column.
#'
#' @author EDG
#' @export
#'
#' @examples
#' features(iris) |> head()
features <- new_generic("features", "x", function(x) {
  S7_dispatch()
}) # /rtemis::features

method(features, class_data.frame) <- function(x) {
  stopifnot(NCOL(x) > 1)
  x[, 1:(NCOL(x) - 1), drop = FALSE]
}


#' Get feature names
#'
#' Returns all column names except the last one
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x data.frame or similar.
#'
#' @return Character vector of feature names.
#'
#' @author EDG
#' @export
#'
#' @examples
#' feature_names(iris)
feature_names <- new_generic("feature_names", "x", function(x) {
  S7_dispatch()
}) # /rtemis::feature_names

method(feature_names, class_data.frame) <- function(x) {
  names(x)[1:(NCOL(x) - 1)]
}


#' Check factor levels
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_factor_levels <- new_generic("check_factor_levels", c("x"))


#' Get factor names
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x data.frame or similar.
#'
#' @return Character vector of factor names.
#'
#' @author EDG
#' @export
#'
#' @examples
#' get_factor_names(iris)
get_factor_names <- new_generic("get_factor_names", "x", function(x) {
  S7_dispatch()
}) # /rtemis::get_factor_names

method(get_factor_names, class_data.frame) <- function(x) {
  names(x)[sapply(x, is.factor)]
}


#' Calibrate `Classification` & `ClassificationRes` Models
#'
#' @description
#' Generic function to calibrate binary classification models.
#'
#' @param x `Classification` or `ClassificationRes` object to calibrate.
#' @param algorithm Character: Algorithm to use to train calibration model.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to specific methods.
#'
#' @section Method-specific parameters:
#'
#' **For `Classification` objects:**
#' * `predicted_probabilities`: Numeric vector of predicted probabilities
#' * `true_labels`: Factor of true class labels
#'
#' **For `ClassificationRes` objects:**
#' * `resampler_config`: `ResamplerConfig` object for calibration training
#' * `train_verbosity`: Integer controlling calibration model training output
#'
#' @details
#' The goal of calibration is to adjust the predicted probabilities of a binary classification
#' model so that they better reflect the true probabilities (i.e. empirical risk) of the positive
#' class.
#'
#' @return Calibrated model object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # --- Calibrate Classification ---
#' dat <- iris[51:150, ]
#' res <- resample(dat)
#' dat$Species <- factor(dat$Species)
#' dat_train <- dat[res[[1]], ]
#' dat_test <- dat[-res[[1]], ]
#'
#' # Train LightRF on a training/test split
#' mod_c_lightrf <- train(
#'   x = dat_train,
#'   dat_test = dat_test,
#'   hyperparameters = setup_LightRF(nrounds = 20L)
#' )
#'
#' # Calibrate the `Classification` by defining `predicted_probabilities` and `true_labels`,
#' # in this case using the training data, but it could be a separate calibration dataset.
#' mod_c_lightrf_cal <- calibrate(
#'   mod_c_lightrf,
#'   predicted_probabilities = mod_c_lightrf$predicted_prob_training,
#'   true_labels = mod_c_lightrf$y_training
#')
#' mod_c_lightrf_cal
#'
#' # --- Calibrate ClassificationRes ---
#'
#' # Train LightRF with cross-validation
#' resmod_c_lightrf <- train(
#'  x = dat,
#'  hyperparameters = setup_LightRF(nrounds = 20L),
#'  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
#' )
#'
#' # Calibrate the `ClassificationRes` using the same resampling configuration as used for training.
#' resmod_c_lightrf_cal <- calibrate(resmod_c_lightrf)
#' resmod_c_lightrf_cal
calibrate <- new_generic(
  "calibrate",
  ("x"),
  function(
    x,
    algorithm = "isotonic",
    hyperparameters = NULL,
    verbosity = 1L,
    ...
  ) {
    S7_dispatch()
  }
) # /rtemis::calibrate


# --- Custom S7 validators -------------------------------------------------------------------------

#' Scalar double
#'
#' @author EDG
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
) # /rtemis::scalar_dbl


#' Scalar double between 0 and 1, exclusive
#'
#' @author EDG
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
) # /rtemis::scalar_dbl_01excl


#' Scalar double between 0 and 1, inclusive
#'
#' @author EDG
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
) # /rtemis::scalar_dbl_01incl


#' Scalar integer
#'
#' @author EDG
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
) # /rtemis::scalar_int


#' Scalar positive integer
#'
#' @author EDG
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
) # /rtemis::scalar_int_pos


#' Get preprocessed data from `Preprocessor`.
#'
#' Returns the preprocessed data from a `Preprocessor` object.
#'
#' @param x `Preprocessor`: A `Preprocessor` object.
#'
#' @return data.frame: The preprocessed data.
#'
#' @export
preprocessed <- new_generic("preprocessed", "x", function(x) {
  S7_dispatch()
}) # /rtemis::preprocessed


# --- Internal functions ---------------------------------------------------------------------------
#' Get output type
#'
#' Get output type for printing text.
#'
#' @param output_type Character vector of output types.
#' @param filename Character: Filename for output.
#'
#' @return Character with selected output type.
#'
#' @author EDG
#'
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
