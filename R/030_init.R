# 030_init.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# S7 generics: https://rconsortium.github.io/S7/articles/generics-methods.html

# %% --- S3 Classes for S7 ----------------------------------------------------------------------------
class_table <- new_S3_class("table")
class_matrix <- new_S3_class("matrix")
class_POSIXct <- new_S3_class("POSIXct")
class_data.table <- new_S3_class("data.table")
class_lgb.Booster <- new_S3_class("lgb.Booster")
# All internal methods should support data.frame, data.table, tbl_df
class_tabular <- new_union(class_data.frame, class_data.table)
# Supervised learning model classes
class_glm <- new_S3_class("glm")
class_gam <- new_S3_class("gam")
class_glmnet <- new_S3_class("glmnet")
class_cv.glmnet <- new_S3_class("cv.glmnet")
class_stepfun <- new_S3_class("stepfun") # Isotonic regression
class_rpart <- new_S3_class("rpart")
class_ranger <- new_S3_class("ranger")
class_svm <- new_S3_class("svm")
class_tabnet_fit <- new_S3_class("tabnet_fit")
class_spls <- new_S3_class("spls")
class_splsda <- new_S3_class("splsda")
class_train.kknn <- new_S3_class("train.kknn")
class_bartmodel <- new_S3_class("bartmodel")


# %% --- Generics -------------------------------------------------------------------------------------
# A generic declaring formals beyond its dispatch argument(s) calls
# `force_supplied()` before `S7_dispatch()`. See there for why.

# %% force_supplied ----
#' Force the arguments the caller supplied
#'
#' Called from an S7 generic's body, immediately before `S7_dispatch()`.
#'
#' @details
#' S7 inlines every named formal of a generic into the method call as a
#' promise. An argument whose first force happens inside the method, and which
#' raises there, leaves that promise flagged under evaluation -- and anything
#' that later walks the stack and touches it reports "promise already under
#' evaluation" instead of the real error. Capturing a backtrace does exactly
#' that, so every caught error in a testthat run reaches it.
#'
#' Forcing here raises in the generic's frame instead, where the error names
#' the argument that failed.
#'
#' Only the arguments named in the call are forced, read off `match.call()`.
#' Forcing the rest would evaluate defaults nothing asked for
#' (`setup_ExecutionConfig()` for a `train_` method that ignores it) and would
#' turn an omitted required formal into "argument "x" is missing, with no
#' default" before the method can say anything better. It is not about
#' preserving method defaults: S7 requires a method's defaults to match its
#' generic's and warns when they differ.
#'
#' This reads the calling frame, so it is only meaningful called directly from
#' the generic.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
force_supplied <- function() {
  frame <- sys.parent()
  fn <- sys.function(frame)
  supplied <- setdiff(names(match.call(fn, sys.call(frame))), "")
  env <- sys.frame(frame)
  for (nm in intersect(supplied, names(formals(fn)))) {
    get(nm, envir = env, inherits = FALSE)
  }
  invisible(NULL)
} # /rtemis::force_supplied


# %% repr ----

# %% get_varimp ----
#' Get variable importance
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return `VariableImportance` object or list of `VariableImportance` objects.
#'
#' @author EDG
#' @export
#' @examples
#' mod <- train(iris, hyperparameters = setup_LightRF())
#' get_varimp(mod)
get_varimp <- new_generic("get_varimp", "x")


# %% inspect ----
#' Inspect rtemis object
#'
#' @param x R object to inspect.
#'
#' @return Called for side effect of printing information to console; returns character string
#' invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' inspect(iris)
inspect <- new_generic("inspect", "x", function(x) {
  S7_dispatch()
}) # /rtemis::inspect


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
#' `preprocess()` preprocesses training data and learns any data-dependent values (e.g. scale
#' centers and coefficients, one-hot levels). Optional `dat_validation` and `dat_test` data are
#' preprocessed using the values learned from the training data. To apply a trained
#' `Preprocessor` to new data, use [apply_preprocessor].
#'
#' For interactive use, `config` may be omitted and [setup_Preprocessor] arguments passed
#' directly instead, e.g. `preprocess(x, scale = TRUE, center = TRUE)`. At least one
#' preprocessing parameter must be specified: `preprocess(x)` is an error.
#'
#' @param x data.frame or data.table: Training set data to preprocess.
#' @param config `PreprocessorConfig`: Preprocessing configuration created by
#' [setup_Preprocessor]. May be omitted, in which case [setup_Preprocessor] arguments are
#' passed directly via `...`.
#' @param dat_validation Optional data.frame or data.table: Validation set data. Preprocessed
#' using the values learned from the training set data.
#' @param dat_test Optional data.frame or data.table: Test set data. Preprocessed using the
#' values learned from the training set data.
#' @param verbosity Integer: Verbosity level.
#' @param ... [setup_Preprocessor] arguments: Only used when `config` is not provided.
#'
#' @return `Preprocessor` object.
#'
#' @author EDG
#' @seealso [apply_preprocessor], [setup_Preprocessor]
#' @rdname preprocess
#' @export
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
#' # Alternatively, for interactive use, pass `setup_Preprocessor()` arguments directly
#' iris_pre <- preprocess(iris_train, remove_duplicates = TRUE, scale = TRUE, center = TRUE)
#'
#' # Access preprocessed training data with `preprocessed()`
#' preprocessed(iris_pre)
#'
#' # Apply the same preprocessing to test data with `apply_preprocessor()`,
#' # which returns the preprocessed data directly.
#' # The scale and center values learned from the training data will be used.
#' iris_test_pre <- apply_preprocessor(iris_pre, iris_test)
preprocess <- new_generic(
  "preprocess",
  c("x", "config"),
  function(
    x,
    config,
    dat_validation = NULL,
    dat_test = NULL,
    verbosity = 1L,
    ...
  ) {
    force_supplied()
    S7_dispatch()
  }
)


# %% train_ ----
#' Generic for training supervised learning models
#'
#' @description
#' Internal S7 generic that dispatches algorithm-specific training based on
#' `Hyperparameters` class. Called by `train()`.
#'
#' @param hyperparameters `Hyperparameters` object: Algorithm-specific hyperparameters.
#' @param x tabular data: Training set.
#' @param weights Optional Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Validation set for algorithms that support early stopping.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list:
#'   * `model` -- the algorithm-specific fitted model object.
#'   * `preprocessor` -- Optional `Preprocessor`: algorithm-level preprocessing
#'     (e.g. factor-to-integer for LightGBM), re-applied before predicting.
#'   * `hyperparameters` -- Optional `Hyperparameters`: returned **only** by a
#'     method that resolved values into it (LightGBM's `objective` from the
#'     outcome type, GLMNET's `lambda` from `cv.glmnet`). R copies the object
#'     into the method, so without returning it the caller keeps the unresolved
#'     one and the fitted model reports NULL for settings it demonstrably used.
#'     `train()` adopts it when present.
#'
#' @author EDG
#' @keywords internal
#' @noRd
train_ <- new_generic(
  "train_",
  "hyperparameters",
  function(
    hyperparameters,
    x,
    weights = NULL,
    dat_validation = NULL,
    execution_config = setup_ExecutionConfig(),
    verbosity = 1L
  ) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::train_


# %% predict_super ----
#' Predict from supervised learning model (internal)
#'
#' @description
#' Internal S7 generic that dispatches algorithm-specific prediction based on
#' model class.
#'
#' @param model Fitted model object.
#' @param newdata tabular data: New data for prediction.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#' @param ... Additional arguments (not currently used).
#'
#' @return Predictions (class probabilities for classification, numeric for regression).
#'
#' @author EDG
#' @keywords internal
#' @noRd
predict_super <- new_generic(
  "predict_super",
  "model",
  function(model, newdata, type = NULL, verbosity = 0L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::predict_super


# %% varimp_super ----
#' Get variable importance (internal)
#'
#' @description
#' Internal S7 generic that dispatches algorithm-specific variable importance
#' extraction based on model class.
#'
#' @param object Fitted model object.
#'
#' @return Numeric vector of variable importance scores (named by feature).
#'
#' @author EDG
#' @keywords internal
#' @noRd
varimp_super <- new_generic(
  "varimp_super",
  "model",
  function(model, ...) {
    S7_dispatch()
  }
) # /rtemis::varimp_super


# %% se_super ----
#' Get standard errors of predictions (internal)
#'
#' @description
#' Internal S7 generic for extracting standard errors from regression models.
#'
#' @param object Fitted model object.
#' @param newdata tabular data: New data for prediction.
#'
#' @return Numeric vector of standard errors.
#'
#' @author EDG
#' @keywords internal
#' @noRd
se_super <- new_generic(
  "se_super",
  "model",
  function(model, newdata) {
    force_supplied()
    S7_dispatch()
  }
)


# %% se ----
#' Standard error of the fit
#'
#' Computed on demand from the fitted model rather than stored: only linear and
#' additive models produce standard errors at all, so storing three per-case
#' vectors on every regression result carried a value that two of thirteen
#' algorithms populate.
#'
#' @param x `Supervised` object.
#' @param newdata tabular data: Data to compute standard errors for.
#' @param ... Additional arguments passed to methods.
#'
#' @return Numeric vector of standard errors, or NULL when the algorithm has
#'   none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
se <- new_generic("se", "x", function(x, newdata, ...) {
  force_supplied()
  S7_dispatch()
})


# %% decomp_ ----
#' Generic for decomposition
#'
#' @author EDG
#' @keywords internal
#' @noRd
decomp_ <- new_generic(
  "decomp_",
  "config",
  function(config, x, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::decomp_


# %% apply_decomp_ ----
#' Generic for applying a fitted decomposition to new data
#'
#' Dispatches on the `DecompositionConfig` subclass. Implemented only for
#' algorithms listed in `decom_algorithms_applicable`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
apply_decomp_ <- new_generic(
  "apply_decomp_",
  "config",
  function(config, decom, new_data, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::apply_decomp_


# %% cluster_ ----
#' Generic for clustering
#'
#' @author EDG
#' @keywords internal
#' @noRd
cluster_ <- new_generic(
  "cluster_",
  "config",
  function(config, x, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::cluster_


# %% desc ----
#' Short description for inline printing.
#' This is like `repr` for single-line descriptions.
#'
#' @author EDG
#' @keywords internal
#' @noRd
desc <- new_generic("desc", "x")


# %% get_metric ----
#' Get metric
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_metric <- new_generic("get_metric", "x")


# %% validate_hyperparameters ----
#' Check hyperparameters given training data
#'
#' @description
#' Internal S7 generic for algorithm-specific hyperparameter constraints that
#' can only be checked once the data is known - e.g. Ranger's `mtry` cannot
#' exceed the number of features. Bounds, types, and enums are enforced by the
#' `prop_*` validators at construction; this generic covers only what depends
#' on `x`.
#'
#' Called by [train] before any tuning or resampling, so an invalid search
#' space fails fast rather than surfacing as per-grid-cell failures that
#' `on_error = "continue"` would swallow, and again immediately before
#' `train_()` on the resolved hyperparameters, where the feature count reflects
#' any preprocessing and decomposition.
#'
#' Tunable hyperparameters hold a *vector* of search values at the first call
#' site, so methods must validate every element (`any(...)`, not `>`).
#'
#' The default method (on `Hyperparameters`, in 070_Hyperparameters.R) checks
#' every property declaring a `data_bound`, so most algorithms need no method of
#' their own. Write one only for a constraint the `data_bound` vocabulary cannot
#' express, and call `check_data_bounds()` from it so the declarative checks
#' still run.
#'
#' @param hyperparameters `Hyperparameters`: Hyperparameters to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly. Throws if a constraint is violated.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_hyperparameters <- new_generic(
  "validate_hyperparameters",
  "hyperparameters",
  function(hyperparameters, x) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::validate_hyperparameters


# %% plot_metric ----
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


# %% plot_roc ----
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
#' @examples
#' ir <- iris[51:150, ]
#' ir[["Species"]] <- factor(ir[["Species"]])
#' species_glm <- train(ir, hyperparameters = setup_GLM())
#' plot_roc(species_glm)
plot_roc <- new_generic("plot_roc", "x")


# %% plot_varimp ----
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
#' @examplesIf interactive()
#' ir <- set_outcome(iris, "Sepal.Length")
#' seplen_cart <- train(ir, hyperparameters = setup_CART())
#' plot_varimp(seplen_cart)
#' # Plot horizontally
#' plot_varimp(seplen_cart, orientation = "h")
#' plot_varimp(seplen_cart, orientation = "h", plot_top = 3L)
#' plot_varimp(seplen_cart, orientation = "h", plot_top = 0.5)
#'
#' @seealso [draw_varimp], which is called by this method
plot_varimp <- new_generic("plot_varimp", "x")


# %% plot_true_pred ----
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
#' @examples
#' x <- set_outcome(iris, "Sepal.Length")
#' sepallength_glm <- train(x, hyperparameters = setup_GLM())
#' plot_true_pred(sepallength_glm)
plot_true_pred <- new_generic("plot_true_pred", "x")


# %% plot_manhattan ----
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
# example included in `plot_manhattan.MassGLM` method.
plot_manhattan <- new_generic("plot_manhattan", "x")


# %% describe ----
#' Describe object
#'
#' @param x R object to describe. See method documentation for supported classes.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to methods.
#'
#' @return Character, invisibly.
#'
#' @details
#' Extra arguments for `factor` method:
#' - `max_n`: Integer: Return counts for up to this many levels.
#' - `return_ordered`: Logical: If TRUE, return levels ordered by count, otherwise return in level order.
#' - `verbosity`: Integer: Verbosity level.
#'
#' @author EDG
#' @export
#' @examples
#' # --- For `Supervised` objects ---
#' species_lightrf <- train(iris, hyperparameters = setup_LightRF())
#' describe(species_lightrf)
#'
#' # --- For `SupervisedRes` objects ---
#' mod <- train(iris, hyperparameters = setup_CART(), outer_resampling_config = setup_Resampler())
#' describe(mod)
#'
#' # --- For factors ---
#' # Small number of levels
#' describe(iris[["Species"]])
#'
#' # Large number of levels: show top n by count
#' x <- factor(sample(letters, 1000, TRUE))
#' describe(x)
#' describe(x, 3)
#' describe(x, 3, return_ordered = FALSE)
describe <- new_generic("describe", "x", function(x, verbosity = 1L, ...) {
  force_supplied()
  S7_dispatch()
})


# %% present ----
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
#' @examplesIf interactive()
#' ir <- set_outcome(iris, "Sepal.Length")
#' seplen_lightrf <- train(ir, hyperparameters = setup_LightRF())
#' present(seplen_lightrf)
present <- new_generic("present", "x")


# %% get_hyperparams_need_tuning ----
#' Get hyperparameters that need tuning.
#'
#' @return Character vector of hyperparameter names that need tuning.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_hyperparams_need_tuning <- new_generic("get_hyperparams_need_tuning", "x")


# %% get_hyperparams ----
#' Get hyperparameters.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_hyperparams <- new_generic("get_hyperparams", c("x", "param_names"))


# %% extract_rules ----
#' Extract rules from a model.
#'
#' @author EDG
#' @keywords internal
#' @noRd
extract_rules <- new_generic("extract_rules", "x")


# %% get_factor_levels ----
#' @name get_factor_levels
#'
#' @title
#' Get factor levels from data.frame or similar
#'
#' @usage
#' get_factor_levels(x)
#'
#' @param x tabular data.
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


# %% to_html ----
#' Convert to HTML
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_html <- new_generic("to_html", "x")


# %% to_json ----
#' Convert to JSON-serializable list
#'
#' Convert an rtemis S7 object to a named list suitable for
#' `jsonlite::toJSON(auto_unbox = TRUE)`. Used by the rtemislive backend
#' to send structured results to the browser frontend without scraping
#' R console output.
#'
#' Each output list includes a `.class` field equal to the most specific
#' S7 class name, allowing the frontend to dispatch to a class-specific
#' renderer.
#'
#' The default method walks the class's *published* properties (see
#' `prop_published()`), recursing into S7-typed properties and passing through
#' primitive properties as-is. A computed view or an R-only value is omitted:
#' the first is recoverable from what is published, and the second has no wire
#' form at all, so emitting either would put a value on the wire that no schema
#' declares. Per-class methods override where the default isn't appropriate
#' (e.g. where some props should be excluded for size or relevance reasons).
#'
#' @param x rtemis S7 object.
#' @param ... Additional arguments passed to method.
#'
#' @return Named list. Pass through `jsonlite::toJSON(auto_unbox = TRUE)`
#' for serialization.
#'
#' @author EDG
#' @keywords internal
#' @export
#' @examples
#' to_json(check_data(iris))
to_json <- new_generic("to_json", "x")


# %% to_json default ----
#' @name to_json
#' @keywords internal
#' @noRd
method(to_json, S7_object) <- function(x, ...) {
  # Read one property at a time rather than `props(x)`, so an omitted computed
  # property's getter is not evaluated only to be discarded.
  nms <- published_prop_names(S7_class(x))
  body <- lapply(nms, function(nm) .to_json_value(prop(x, nm)))
  names(body) <- nms
  c(list(.class = S7_class(x)@name), body)
} # /rtemis::to_json.S7_object


#' Recursively convert a value to a JSON-serializable form
#'
#' Handles the common composite shapes encountered when walking S7 props:
#' nested S7 objects (recurse via the generic), lists that may *contain*
#' S7 objects (recurse element-wise), and primitives / data.frames
#' (pass through -- jsonlite supports them natively).
#'
#' @param v Value from an S7 property.
#'
#' @return JSON-serializable value.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.to_json_value <- function(v) {
  if (is.null(v)) {
    return(NULL)
  }
  if (S7_inherits(v)) {
    return(to_json(v))
  }
  # data.frame / data.table are list-like but jsonlite handles them natively.
  if (is.list(v) && !is.data.frame(v)) {
    return(lapply(v, .to_json_value))
  }
  v
} # /rtemis::.to_json_value


# %% inc ----
#' Select (include) columns by character or numeric vector.
#'
#' @param x tabular data.
#' @param idx Character or numeric vector: Column names or indices to include.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @author EDG
#' @export
#' @examples
#' inc(iris, c(3, 4)) |> head()
#' inc(iris, c("Sepal.Length", "Species")) |> head()
inc <- new_generic("inc", "x", function(x, idx) {
  force_supplied()
  S7_dispatch()
})


# %% exc ----
#' Exclude columns by character or numeric vector.
#'
#' @param x tabular data.
#' @param idx Character or numeric vector: Column names or indices to exclude.
#'
#' @return data.frame, tibble, or data.table.
#'
#' @author EDG
#' @export
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


# %% outcome_name ----
#' Get the name of the last column
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x tabular data.
#'
#' @return Name of the last column.
#'
#' @author EDG
#' @export
#' @examples
#' outcome_name(iris)
outcome_name <- new_generic("outcome_name", "x", function(x) {
  S7_dispatch()
})

method(outcome_name, class_data.frame) <- function(x) {
  names(x)[NCOL(x)]
} # /rtemis::outcome_name


# %% outcome ----
#' Get the outcome as a vector
#'
#' Returns the last column of `x`, which is by convention the outcome variable.
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x tabular data.
#'
#' @return Vector containing the last column of `x`.
#'
#' @author EDG
#' @export
#' @examples
#' outcome(iris)
outcome <- new_generic("outcome", "x", function(x) {
  S7_dispatch()
}) # /rtemis::outcome

method(outcome, class_data.frame) <- function(x) {
  x[[NCOL(x)]]
}


# %% features ----
#' Get features from tabular data
#'
#' Returns all columns except the last one.
#'
#' @details
#' This can be applied to tabular datasets used for supervised learning in \pkg{rtemis},
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x tabular data: Input data to get features from.
#'
#' @return Object of the same class as the input, after removing the last column.
#'
#' @author EDG
#' @export
#' @examples
#' features(iris) |> head()
features <- new_generic("features", "x", function(x) {
  S7_dispatch()
}) # /rtemis::features

method(features, class_data.frame) <- function(x) {
  if (NCOL(x) < 2) {
    rtemis.core::abort(
      "Input must have at least 2 columns.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  x[, -NCOL(x), drop = FALSE]
}

method(features, class_data.table) <- function(x) {
  if (NCOL(x) < 2) {
    rtemis.core::abort(
      "Input must have at least 2 columns.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  x[, -NCOL(x), with = FALSE]
} # /rtemis::features.class_data.table


# %% numeric_features ----
#' Get numeric features from tabular data
#'
#' Returns the numeric columns among the features (all columns except the last).
#'
#' @details
#' Mirrors [features()]: by \pkg{rtemis} convention the last column is the outcome
#' variable and all other columns are features. This drops the outcome, then keeps
#' the numeric features (both double and integer). Useful, for example, to feed only
#' the continuous features to a decomposition: `decomp(numeric_features(iris), ...)`.
#'
#' @param x tabular data: Input data to get numeric features from.
#'
#' @return Object of the same class as the input, containing only the numeric
#' feature columns.
#'
#' @author EDG
#' @export
#' @examples
#' numeric_features(iris) |> head()
numeric_features <- new_generic("numeric_features", "x", function(x) {
  S7_dispatch()
}) # /rtemis::numeric_features

method(numeric_features, class_data.frame) <- function(x) {
  feat <- features(x)
  feat[, vapply(feat, is.numeric, logical(1L)), drop = FALSE]
}

method(numeric_features, class_data.table) <- function(x) {
  feat <- features(x)
  feat[, vapply(feat, is.numeric, logical(1L)), with = FALSE]
} # /rtemis::numeric_features.class_data.table


# %% feature_names ----
#' Get feature names
#'
#' Returns all column names except the last one
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x tabular data.
#'
#' @return Character vector of feature names.
#'
#' @author EDG
#' @export
#' @examples
#' feature_names(iris)
feature_names <- new_generic("feature_names", "x", function(x) {
  S7_dispatch()
}) # /rtemis::feature_names

method(feature_names, class_data.frame) <- function(x) {
  if (NCOL(x) < 2) {
    rtemis.core::abort(
      "Input must have at least 2 columns.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  names(x)[-NCOL(x)]
} # /rtemis::feature_names.class_data.frame


# %% check_factor_levels ----
#' Check factor levels
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_factor_levels <- new_generic("check_factor_levels", c("x"))


# %% get_factor_names ----
#' Get factor names
#'
#' @details
#' This applied to tabular datasets used for supervised learning in rtemis,
#' where, by convention, the last column is the outcome variable and all other columns
#' are features.
#'
#' @param x tabular data.
#'
#' @return Character vector of factor names.
#'
#' @author EDG
#' @export
#' @examples
#' get_factor_names(iris)
get_factor_names <- new_generic("get_factor_names", "x", function(x) {
  S7_dispatch()
}) # /rtemis::get_factor_names

method(get_factor_names, class_data.frame) <- function(x) {
  names(x)[sapply(x, is.factor)]
}


# %% calibrate ----
#' Calibrate `Classification` & `ClassificationRes` Models
#'
#' @description
#' Generic function to calibrate binary classification models.
#'
#' @param x `Classification` or `ClassificationRes` object to calibrate.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' Defines the algorithm used to train the calibration model.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to specific methods.
#'
#' @section Method-specific parameters:
#'
#' **For `Classification` objects:**
#' * `predicted_probabilities`: Numeric vector of the positive class's
#'   predicted probabilities, one per case
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
#'   hyperparameters = setup_GLM()
#' )
#'
#' # Calibrate the `Classification` by defining `predicted_probabilities` and `true_labels`,
#' # in this case using the training data, but it could be a separate calibration dataset.
#' mod_c_glm_cal <- calibrate(
#'   mod_c_glm,
#'   predicted_probabilities = mod_c_glm$predicted_prob_training[, 1L],
#'   true_labels = mod_c_glm$y_training
#' )
#' mod_c_glm_cal
#'
#' # --- Calibrate ClassificationRes ---
#'
#' # Train GLM with cross-validation
#' resmod_c_glm <- train(
#'   x = dat,
#'   hyperparameters = setup_GLM(),
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
    hyperparameters = setup_Isotonic(),
    verbosity = 1L,
    ...
  ) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::calibrate


# %% freeze ----
#' Freeze Hyperparameters
#'
#' @param x `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
freeze <- new_generic("freeze", "x")


# %% lock ----
#' Lock Hyperparameters
#'
#' @param x `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lock <- new_generic("lock", "x")


# %% needs_tuning ----
#' needs_tuning
#'
#' @keywords internal
#' @noRd
needs_tuning <- new_generic("needs_tuning", "x")


# %% get_factor_levels ----
#' @name get_factor_levels
#'
#' @title
#' Get factor levels from data.frame or similar
#'
#' @usage
#' get_factor_levels(x)
#'
#' @param x tabular data.
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


# %% is_tuned ----
is_tuned <- new_generic("is_tuned", "x")


# %% get_tuned_status ----
get_tuned_status <- new_generic("get_tuned_status", "x")


# %% one_hot ----
one_hot <- new_generic("one_hot", "x")


# --- Custom S7 validators -------------------------------------------------------------------------
# %% preprocessed ----
#' Get preprocessed data from `Preprocessor`.
#'
#' Returns the preprocessed data from a `Preprocessor` object.
#'
#' @param x `Preprocessor`: A `Preprocessor` object.
#'
#' @return data.frame: The preprocessed data.
#'
#' @export
#' @examples
#' prp <- preprocess(iris, setup_Preprocessor(scale = TRUE, center = TRUE))
#' preprocessed(prp)
preprocessed <- new_generic("preprocessed", "x", function(x) {
  S7_dispatch()
}) # /rtemis::preprocessed


# --- Internal functions ---------------------------------------------------------------------------

# %% serializable_props ----
#' Properties of an S7 object to serialize
#'
#' The default keeps every property `prop_serialized()` admits, so a flat config
#' drops the same fields a config family does rather than emitting whatever it
#' happens to hold. Config-family classes (`Hyperparameters`,
#' `DecompositionConfig`, `ClusteringConfig`) override this to return their
#' canonical public shape (`algorithm` + the computed parameter list + any base
#' fields), so the per-algorithm properties they declare -- redundant with the
#' computed list -- are not duplicated into the serialized output. See methods
#' in the respective class files.
#'
#' @param x S7 object.
#'
#' @return Named list of properties to serialize.
#'
#' @author EDG
#' @keywords internal
#' @noRd
serializable_props <- new_generic("serializable_props", "x")

method(serializable_props, S7_object) <- function(x) {
  values <- props(x)
  declared <- S7_class(x)@properties
  keep <- vapply(
    names(values),
    function(nm) {
      # A property this object holds but does not declare cannot be judged;
      # keep it rather than silently dropping data.
      is.null(declared[[nm]]) || prop_serialized(declared[[nm]])
    },
    logical(1L)
  )
  values <- values[keep]
  for (nm in names(values)) {
    if (!is.null(declared[[nm]])) {
      values[[nm]] <- wire_value(values[[nm]], declared[[nm]])
    }
  }
  values
} # /rtemis::serializable_props.S7_object


# %% S7_to_list ----
S7_to_list <- function(x) {
  if (S7_inherits(x)) {
    x <- serializable_props(x)
  }
  if (is.list(x)) {
    x <- lapply(x, S7_to_list)
  }
  x
} # /rtemis::S7_to_list


# %% write_lines ----
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
      rtemis.core::abort(
        "File already exists: ",
        file,
        ". Set `overwrite = TRUE` to overwrite.",
        class = c("rtemis_file_exists", "rtemis_io_error")
      )
    }
  }
  # Get directory name
  dir <- dirname(file)
  # Check if directory exists, create it if not
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (!dir.exists(dir)) {
      rtemis.core::abort(
        "Failed to create directory: ",
        dir,
        class = "rtemis_io_error"
      )
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
    rtemis.core::abort(
      "Failed to create file: ",
      file,
      class = "rtemis_io_error"
    )
  } else {
    if (verbosity >= 1L) {
      msg(checkmark(), "Created file:", file)
    }
  }
  invisible(NULL)
} # /rtemis::write_lines


# %% coming up in rtemis.core
collapse_head <- function(x, maxlength = 6L, format_fn = identity) {
  if (maxlength == -1L || length(x) <= maxlength) {
    paste(format_fn(x), collapse = ", ")
  } else {
    paste0(
      paste(
        format_fn(utils::head(as.vector(x), n = maxlength)),
        collapse = ", "
      ),
      ", ..."
    )
  }
}


# %% repr, S7 ----
# generic for S7 objects, when no more specific method is defined.
method(repr, S7_object) <- function(x, limit = -1L, output_type = NULL, ...) {
  paste0(
    repr_S7name(x, output_type = output_type),
    "\n",
    repr_ls(props(x), limit = limit, output_type = output_type, ...)
  )
} # /rtemis::repr.S7_object


method(print, S7_object) <- function(x, ...) {
  cat(repr(x, ...), "\n")
  invisible(x)
} # /rtemis::print.S7_object
