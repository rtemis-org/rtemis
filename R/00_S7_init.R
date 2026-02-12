# S7_init.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# S7 generics: https://rconsortium.github.io/S7/articles/generics-methods.html

# --- S3 Classes for S7 ----------------------------------------------------------------------------
class_data.table <- new_S3_class("data.table")
class_lgb.Booster <- new_S3_class("lgb.Booster")
class_tabular <- new_union(class_data.frame, class_data.table)

# --- Generics -------------------------------------------------------------------------------------
# %% preprocess ----
#' @name
#' preprocess
#'
#' @title
#' Preprocess Data
#'
#' @description
#' Preprocess data for analysis and visualization.
#'
#' @details
#' Methods are provided for preprocessing training set data, which accepts a `PreprocessorConfig`
#' object, and for preprocessing validation and test set data, which accept a `Preprocessor`
#' object.
#'
#' @return `Preprocessor` object.
#'
#' @author EDG
#' @rdname preprocess
#' @export
#'
#' @examples
#' # Setup a `Preprocessor`: this outputs a `PreprocessorConfig` object.
#' prp <- setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE)
#'
#' # Includes a long list of parameters
#' prp
#'
#' # Resample iris to get train and test data
#' res <- resample(iris, setup_Resampler(seed = 2026))
#' iris_train <- iris[res[[1]], ]
#' iris_test <- iris[-res[[1]], ]
#'
#' # Preprocess training data
#' iris_pre <- preprocess(iris_train, prp)
#'
#' # Access preprocessd training data with `preprocessed()`
#' preprocessed(iris_pre)
#'
#' # Apply the same preprocessing to test data
#' # In this case, the scale and center values from training data will be used.
#' # Note how `preprocess()` accepts either a `PreprocessorConfig` or `Preprocessor` object for
#' # this reason.
#' iris_test_pre <- preprocess(iris_test, iris_pre)
#'
#' # Access preprocessed test data
#' preprocessed(iris_test_pre)
preprocess <- new_generic("preprocess", c("x", "config"))


# %% train ----
#' @name
#' train
#'
#' @title
#' Train Supervised Learning Models
#'
#' @description
#' Preprocess, tune, train, and test supervised learning models in a single call
#' using nested resampling.
#'
#' @details
#' **Online documentation**
#' See [rdocs.rtemis.org/train](https://rdocs.rtemis.org/train) for detailed documentation.
#'
#' **Binary Classification**
#' For binary classification, the outcome should be a factor where the 2nd level
#' corresponds to the positive class.
#'
#' **Resampling**
#' Note that you should not use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and test sets of the inner resamples, leading to underestimated
#' test error.
#'
#' **Reproducibility**
#' If using ***outer resampling***, you can set a seed when defining `outer_resampling_config`, e.g.
#' ```r
#' outer_resampling_config = setup_Resampler(n_resamples = 10L, type = "KFold", seed = 2026L)
#' ```
#' If using ***tuning with inner resampling***, you can set a seed when defining `tuner_config`,
#' e.g.
#' ```r
#' tuner_config = setup_GridSearch(
#'   resampler_config = setup_Resampler(n_resamples = 5L, type = "KFold", seed = 2027L)
#' )
#' ```
#'
#' **Parallelization**
#' There are three levels of parallelization that may be used during training:
#'
#' 1. Algorithm training (e.g. a parallelized learner like LightGBM)
#' 2. Tuning (inner resampling, where multiple resamples can be processed in parallel)
#' 3. Outer resampling (where multiple outer resamples can be processed in parallel)
#'
#' The `train()` function and its sub-functions will automatically manage parallelization depending
#' on:
#' - The number of workers specified by the user using `n_workers`
#' - Whether the training algorithm supports parallelization itself
#' - Whether hyperparameter tuning is needed
#'
#' @author EDG
#' @export
# Examples are provided in the method-specific documentation.
train <- new_generic("train", "x")

#' String representation
#'
#' @param x rtemis object.
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
#' @examplesIf interactive()
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


# %% to_toml
#' Convert to TOML
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_toml <- new_generic("to_toml", "x")


# %% to_yaml
#' Convert to YAML
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_yaml <- new_generic("to_yaml", "x")


# %% write_toml

#' @name
#' write_toml
#'
#' @title
#' Write to TOML file
#'
#' @author EDG
#' @export
write_toml <- new_generic(
  "write_toml",
  "x",
  function(x, file, overwrite = FALSE, verbosity = 1L) {
    S7_dispatch()
  }
) # /rtemis::write_toml


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
#' # Train GLM on a training/test split
#' mod_c_glm <- train(
#'   x = dat_train,
#'   dat_test = dat_test,
#'   algorithm = "glm"
#' )
#'
#' # Calibrate the `Classification` by defining `predicted_probabilities` and `true_labels`,
#' # in this case using the training data, but it could be a separate calibration dataset.
#' mod_c_glm_cal <- calibrate(
#'   mod_c_glm,
#'   predicted_probabilities = mod_c_glm$predicted_prob_training,
#'   true_labels = mod_c_glm$y_training
#' )
#' mod_c_glm_cal
#'
#' # --- Calibrate ClassificationRes ---
#'
#' # Train GLM with cross-validation
#' resmod_c_glm <- train(
#'   x = dat,
#'   algorithm = "glm",
#'   outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
#' )
#'
#' # Calibrate the `ClassificationRes` using the same resampling configuration as used for training.
#' resmod_c_glm_cal <- calibrate(resmod_c_glm)
#' resmod_c_glm_cal
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
  # with = FALSE slightly more performance than using .SD
  lapply(x[, factor_index, with = FALSE], levels)
}


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
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
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


# %% S7_to_list ----
S7_to_list <- function(x) {
  if (S7_inherits(x)) {
    x <- props(x)
  }
  if (is.list(x)) {
    x <- lapply(x, S7_to_list)
  }
  x
} # /rtemis::S7_to_list


# %% toml_to_list ----

# %% toml_empty_to_null ----
toml_empty_to_null <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (length(x) == 0L) {
    return(NULL)
  }
  if (is.null(names(x))) {
    scalar_types <- vapply(
      x,
      function(el) {
        is.atomic(el) && length(el) == 1L && !is.null(el)
      },
      logical(1)
    )
    if (all(scalar_types)) {
      return(unlist(x, use.names = FALSE))
    }
  }
  lapply(x, toml_empty_to_null)
} # /rtemis::toml_empty_to_null

# %% write_lines

#' Write lines to file
#'
#' Normalizes path, check if directory exists, creates it if necessary,
#' writes lines to file, and checks if file was created successfully.
#'
#' @param x Character: Text to write to file.
#' @param file Character: Path to output file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Invisible NULL. Called for side effect of writing to file.
#'
#' @author EDG
#' @keywords internal
#' @noRd
write_lines <- function(x, file, overwrite = FALSE, verbosity = 1L) {
  # Normalize path
  file <- normalizePath(file, mustWork = FALSE)
  # Check if file exists
  if (file.exists(file)) {
    if (overwrite) {
      if (verbosity >= 1L) {
        msg(fmt(
          paste("Overwriting existing file:", file),
          col = rtemis_colors[["orange"]]
        ))
      }
    } else {
      cli::cli_abort(
        "File already exists: {file}. Set `overwrite = TRUE` to overwrite."
      )
    }
  }
  # Get directory name
  dir <- dirname(file)
  # Check if directory exists, create it if not
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (!dir.exists(dir)) {
      cli::cli_abort("Failed to create directory: {dir}")
    } else {
      if (verbosity >= 1L) {
        msg(checkmark(), "Created directory:", dir)
      }
    }
  }
  # Write lines to file
  writeLines(x, con = file)
  # Check if file was created successfully
  if (!file.exists(file)) {
    cli::cli_abort("Failed to create file: {file}")
  } else {
    if (verbosity >= 1L) {
      msg(checkmark(), "Created file:", file)
    }
  }
  invisible(NULL)
} # /rtemis::write_lines


# %% toml_meta ----
#' @name
#' toml_meta
#'
#' @title
#' Write TOML metadata
#'
#' @description
#' Creates named list which will become first TOML table in the following format:
#'
#' ```toml
#' [_meta]
#' package = "rtemis"
#' package_version = "0.4.2"
#' schema_version = "1.0"
#' object_type = "SuperConfig"
#' created_at = 2026-2-11T22:45:00Z
#' ```
#' @param x Object to create metadata for. Class name will be included in metadata.
#' @param schema_version Character: Version of the schema to include in metadata.
#'
#' @return Named list containing metadata.
#'
#' @author EDG
#' @keywords internal
#' @noRd
toml_meta <- function(x, schema_version = "1.0") {
  list(
    `_meta` = list(
      package = "rtemis",
      package_version = as.character(packageVersion("rtemis")),
      schema_version = schema_version,
      object_type = S7_class(x)@name,
      created_at = format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    )
  )
} # /rtemis::toml_meta


# %% toml_with_meta ----
#' Create TOML string with metadata
#'
#' Creates a TOML string with an inline metadata table followed by the TOML representation of the
#' object.
#'
#' @param x Object to convert to TOML. Class name will be included in metadata.
#'
#'
#' @return Character string containing TOML representation of the object, with metadata included as
#' an inline table at the top.
#'
#' @author EDG
#' @keywords internal
#' @noRd
toml_with_meta <- function(x, payload, schema_version = "1.0") {
  meta_block <- toml::write_toml(
    toml_meta(x, schema_version = schema_version)
  )
  meta_lines <- strsplit(meta_block, "\n", fixed = TRUE)[[1]]
  meta_lines <- meta_lines[meta_lines != "" & meta_lines != "[_meta]"]
  meta_inline <- paste0(
    "_meta = { ",
    paste(meta_lines, collapse = ", "),
    " }"
  )
  payload_str <- toml::write_toml(payload)
  paste(meta_inline, payload_str, sep = "\n\n")
} # /rtemis::toml_with_meta
