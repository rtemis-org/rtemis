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
class_hal9001 <- new_S3_class("hal9001")
class_earth <- new_S3_class("earth")
class_lmtree <- new_S3_class("lmtree")
class_glmtree <- new_S3_class("glmtree")


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
#' (`setup_FutureExecution()` for a `train_` method that ignores it) and would
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
#' The preprocessed data comes back in the structure it was given -- data.frame,
#' data.table or tibble -- and the object passed in is never modified. It carries
#' no row names: a case identifier that matters belongs in a column, where it can
#' be selected, joined, validated and serialized.
#'
#' @param x Tabular data, i.e. data.frame, data.table, or tbl_df (tibble):
#' Training set data to preprocess.
#' @param config `PreprocessorConfig`: Preprocessing configuration created by
#' [setup_Preprocessor]. May be omitted, in which case [setup_Preprocessor] arguments are
#' passed directly via `...`.
#' @param dat_validation Optional tabular data: Validation set data. Preprocessed
#' using the values learned from the training set data.
#' @param dat_test Optional tabular data: Test set data. Preprocessed using the
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
#' res <- resample(iris, setup_KFold(seed = 2026))
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
    execution_config = setup_FutureExecution(),
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
#' @param execution_config Optional `ExecutionConfig`: Where and with what the
#' prediction runs. A method whose backend can use threads or a device resolves
#' them with `algorithm_threads()` and `training_device()`; the rest have
#' nothing to resolve. NULL means the host's defaults. It describes the
#' prediction's machine and workload, never the ones the model was trained on.
#'
#' @return Predictions (class probabilities for classification, numeric for regression).
#'
#' @author EDG
#' @keywords internal
#' @noRd
predict_super <- new_generic(
  "predict_super",
  "model",
  function(
    model,
    newdata,
    type = NULL,
    execution_config = NULL,
    verbosity = 0L
  ) {
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
#' Computed on demand from the fitted model, since only three of the twenty-four
#' algorithms provide it.
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


# %% quantile_super ----
#' Predict conditional quantiles (internal)
#'
#' @description
#' Internal S7 generic dispatching on the fitted backend's class, for the
#' backends that can answer a quantile query from a model already fitted.
#'
#' A method exists only where **one** fitted object answers **every** level: a
#' quantile regression forest stores the training outcomes at its terminal
#' nodes, so it does; a gradient booster trained on the `quantile` objective
#' targets the one level it was fitted for, so a pair of levels is a pair of
#' models and belongs behind `train()` instead.
#'
#' The contract. Returns an `n x length(quantiles)` numeric matrix, columns in
#' the order `quantiles` were given. A backend that was fitted without whatever
#' it needs to answer -- Ranger without `quantreg = TRUE` -- aborts naming the
#' setting.
#'
#' @param model Fitted model object.
#' @param newdata tabular data: Cases to predict, already transformed.
#' @param quantiles Numeric (0, 1): Levels to predict, in increasing order.
#'
#' @return Numeric matrix, one row per case and one column per level.
#'
#' @author EDG
#' @keywords internal
#' @noRd
quantile_super <- new_generic(
  "quantile_super",
  "model",
  function(model, newdata, quantiles) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::quantile_super


# %% explain_super ----
#' Compute per-case contributions (internal)
#'
#' Internal S7 generic dispatching on the fitted backend's class.
#'
#' Unlike `varimp_super()`, the method does **not** choose what to compute: the
#' estimator is resolved from the algorithm's row in `explanation_methods()` and
#' passed in. One backend class can back two algorithms warranting different
#' estimators -- LinearSVM and RadialSVM are both `e1071::svm` -- so the fitted
#' object cannot decide.
#'
#' The contract. Contributions are indexed by the columns of `newdata` **as
#' passed in**, so a method that builds its own design matrix -- `glm()` and
#' `glmnet()` expand factors through `model.matrix()` rather than through a
#' `Preprocessor` -- undoes that expansion itself. Where the expansion happened
#' upstream in a `Preprocessor`, `newdata` arrives already encoded and
#' `shap_aggregate()` undoes it.
#'
#' \describe{
#'   \item{`phi`}{Unnamed list of `n x p` numeric matrices, one per output;
#'     `explain()` labels them by class. One entry for regression and for
#'     binary classification.}
#'   \item{`baseline`}{Named numeric, parallel to `phi`: `E[f(x)]`.}
#'   \item{`predicted`}{`n x k` matrix on `scale`, which `phi` and `baseline`
#'     must reconstruct.}
#'   \item{`exact`}{TRUE if these are exact Shapley values; FALSE for an
#'     estimate.}
#' }
#'
#' @param model Fitted model object.
#' @param newdata tabular data: Cases to explain, already transformed.
#' @param background Optional tabular data: Reference cases, already
#' transformed. A method needing one aborts when it is NULL.
#' @param estimator Character: Resolved estimator to compute.
#' @param perturbation Character: Resolved value function.
#' @param scale Character: Scale the contributions are additive on.
#' @param type Character: "Regression" or "Classification".
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
explain_super <- new_generic(
  "explain_super",
  "model",
  function(
    model,
    newdata,
    background,
    estimator,
    perturbation,
    scale,
    type,
    verbosity = 0L
  ) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::explain_super


# %% explain ----
#' Explain a prediction
#'
#' Per-case explanation of a fitted model: why it predicted what it did, for
#' each case in `newdata`. Where `get_varimp()` ranks features across a whole
#' model, this attributes one prediction -- a feature can rank third overall and
#' be the entire reason for one case.
#'
#' @details
#' **`background` is what the contributions are measured against**, and most
#' estimators cannot work without one. A contribution says how far a feature
#' moved this case's prediction away from `E[f(x)]`, and that expectation is a
#' property of the background data, and the model does not store the data it
#' was trained on. Pass the training features, or a
#' representative sample of them. The exceptions are the estimators that take
#' their baseline from the model itself, such as the LightGBM family's; those
#' ignore it, and everything else aborts without it.
#'
#' Two explanations of one model against different backgrounds are not
#' comparable, and nothing in the returned numbers says so -- which is why the
#' result carries a fingerprint of the background it used.
#'
#' **`config` chooses the method**, built by [setup_SHAP]:
#' `setup_SHAP(perturbation = "conditional")` picks the value function,
#' `setup_SHAP(estimator = "kernel")` overrides the estimator the algorithm
#' would otherwise get. [explanation_methods] reports which estimator applies to
#' which algorithm, whether it is exact, and why.
#'
#' A `SupervisedRes` additionally takes `type`: `"avg"` (the default) averages
#' the resamples' explanations, which decomposes the averaged prediction
#' exactly, and `"all"` returns one explanation per resample.
#'
#' The contributions plus the baseline reconstruct the prediction exactly, on
#' the scale they were computed on. For classification that scale is the
#' model's margin, **not** the probability [stats::predict()] returns:
#' probability is a nonlinear transform of the margin, so contributions do not
#' sum to it.
#'
#' Read the result with [shap_case] for one case, [shap_long] across cases,
#' [shap_by_level] for the levels of a categorical, and `get_varimp()` for
#' `mean(|phi|)` per feature.
#'
#' A SHAP value is not a causal effect. It attributes a *prediction* under the
#' model's own logic; intervening on a feature in the world does not move the
#' outcome by its attribution. Correlated features split credit between
#' themselves, and how they split it depends on the value function -- so a small
#' attribution does not show a feature to be unimportant, and this is not a
#' feature-selection criterion.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param newdata tabular data: Cases to explain. Predictors only, in training
#' order, as [stats::predict()] requires.
#' @param background Optional tabular data: Cases the contributions are measured
#' against, in the same shape as `newdata`. Required by every estimator that
#' needs `E[x]` or a reference set, which is most of them; those abort with a
#' message naming it when it is missing.
#' @param config Optional `ExplanationConfig` object: Built by [setup_SHAP].
#' NULL uses `setup_SHAP()`, which resolves the estimator per algorithm.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to methods, such as `type` for a
#' `SupervisedRes`.
#'
#' @return `SHAP` object, or a named list of them for a `SupervisedRes` with
#' `type = "all"`.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(age = rnorm(100), bmi = rnorm(100))
#' x[["y"]] <- x[["age"]] * 2 + rnorm(100, sd = 0.3)
#' features <- x[, c("age", "bmi")]
#' mod <- train(x, hyperparameters = setup_GLM(), verbosity = 0L)
#'
#' # `background` is what the contributions are measured against.
#' contributions <- explain(
#'   mod,
#'   features[1:5, ],
#'   background = features,
#'   verbosity = 0L
#' )
#' contributions
#'
#' # Contributions plus the baseline reconstruct the prediction, exactly:
#' rowSums(contributions@phi[["outcome"]]) + contributions@baseline[, 1L]
#' predict(mod, features[1:5, ], verbosity = 0L)
#'
#' # One case, ordered the way a waterfall reads:
#' shap_case(contributions, features[1:5, ], case = 1L)
#'
#' # Which features mattered across these cases:
#' get_varimp(contributions)
#'
#' # `config` chooses the method; here, a subsampled background.
#' explain(
#'   mod,
#'   features[1:5, ],
#'   background = features,
#'   config = setup_SHAP(background_n = 50L),
#'   verbosity = 0L
#' )
explain <- new_generic(
  "explain",
  "x",
  function(x, newdata, background = NULL, config = NULL, verbosity = 1L, ...) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::explain


# %% conformal ----
#' Conformal prediction regions
#'
#' @description
#' A prediction interval for a regression or a set of labels for a
#' classification, covering the truth with probability at least `1 - alpha`
#' under exchangeability alone -- in finite samples, for any model, including a
#' misspecified one.
#'
#' @details
#' **The guarantee is marginal.** `P(Y in C(X)) >= 1 - alpha` averages over the
#' draw of both the calibration set and the test case. It is *not* conditional:
#' 90% marginal coverage is compatible with 99% coverage on easy cases and 60%
#' on a hard subgroup, and averaged over everyone is not the same as guaranteed
#' for anyone. It is also void, silently, under distribution shift --
#' exchangeability is the whole assumption.
#'
#' **Width is the quality measure; coverage is the correctness check.** A
#' useless model attains valid coverage by returning intervals wide enough to be
#' uninformative, so read the two together. The region reports its widths (or
#' set sizes); `conformal_metrics()` scores coverage against outcomes the region
#' did not see.
#'
#' **Which data calibrates.** `calibration` is a tabular dataset holding the
#' predictors and the outcome, in training shape. Left NULL, a model trained
#' with `dat_test` calibrates on that split, whose residuals it already stores:
#' `train()` never fits, tunes or early-stops on `dat_test`. Nothing else is
#' assumed clean -- a validation split may have driven early stopping, and a
#' model chosen by its test metric has used that split for selection, which
#' nothing in the object records.
#'
#' Conformal calibration is unrelated to the probability calibration
#' [calibrate] performs, and the two must not share rows. Where rtemis can see
#' that they do, it refuses.
#'
#' **Which method.** `config` NULL takes the method the object supports: split
#' conformal for a `Supervised`, CV+ for a `SupervisedRes`. See
#' [setup_SplitConformal], [setup_CVPlus] and [setup_CQR].
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param newdata tabular data: Cases to bound. Predictors only, in training
#' order, as [stats::predict()] requires.
#' @param calibration Optional tabular data: Calibration cases, predictors and
#' outcome, in the shape `train()` was given. NULL uses the model's stored test
#' split where it has one.
#' @param config Optional `ConformalConfig` object: Built by
#' [setup_SplitConformal], [setup_CVPlus] or [setup_CQR]. NULL takes the method
#' the object supports.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments passed to methods.
#'
#' @return `PredictionInterval` object for a regression, `PredictionSet` for a
#' classification.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(age = rnorm(300), bmi = rnorm(300))
#' x[["y"]] <- x[["age"]] * 2 + rnorm(300, sd = 0.3)
#' # Three splits: one fits, one calibrates, one scores what the other two did.
#' mod <- train(
#'   x[1:200, ],
#'   dat_test = x[201:250, ],
#'   hyperparameters = setup_GLM(),
#'   verbosity = 0L
#' )
#'
#' # The stored test split calibrates; 90% intervals for five fresh cases.
#' region <- conformal(mod, x[1:5, c("age", "bmi")], verbosity = 0L)
#' region
#' region@lower
#' region@upper
#'
#' # Coverage, against outcomes nothing in the pipeline has seen.
#' held_out <- x[251:300, ]
#' conformal_metrics(
#'   conformal(mod, held_out[, c("age", "bmi")], verbosity = 0L),
#'   held_out[["y"]]
#' )
conformal <- new_generic(
  "conformal",
  "x",
  function(x, newdata, calibration = NULL, config = NULL, verbosity = 1L, ...) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::conformal


# %% decomp_ ----
#' Generic for decomposition
#'
#' `execution_config` is where and with what the fit runs, as for `train_()`.
#' A method whose backend threads reads its count with `algorithm_threads()`;
#' which algorithms do is the `threaded` trait in `decom_algorithms`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decomp_ <- new_generic(
  "decomp_",
  "config",
  function(config, x, execution_config = NULL, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::decomp_


# %% apply_decomp_ ----
#' Generic for applying a fitted decomposition to new data
#'
#' Dispatches on the `DecompositionConfig` subclass. Implemented only for
#' algorithms listed in `decom_algorithms_applicable`. `execution_config`
#' describes where the fit is applied, as for `predict_super()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
apply_decomp_ <- new_generic(
  "apply_decomp_",
  "config",
  function(config, decom, new_data, execution_config = NULL, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::apply_decomp_


# %% reconstruct_ ----
#' Generic for mapping components back to input space
#'
#' Dispatches on the `DecompositionConfig` subclass. Implemented only for
#' algorithms whose `invertible` trait is TRUE in `decom_algorithms`.
#'
#' @details
#' The reconstruction must be in the units of the data as it was handed to
#' `decomp()`, so each method undoes whatever centering, scaling or
#' normalization its backend applied internally. Reconstruction error measured
#' in a backend's internal space is not comparable between two configurations
#' of the same algorithm, let alone between algorithms.
#'
#' `x` is the data being reconstructed, in input units. Methods need it only
#' when the backend's preprocessing is per-case and therefore not recoverable
#' from the components -- ICA's `row_norm`. It is required, since a caller
#' computing reconstruction error already holds `x`, and reconstruction against
#' other cases would produce errors nothing downstream could detect.
#'
#' `execution_config` describes where the reconstruction runs, as for
#' `apply_decomp_()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
reconstruct_ <- new_generic(
  "reconstruct_",
  "config",
  function(
    config,
    decom,
    transformed,
    x,
    execution_config = NULL,
    verbosity = 1L
  ) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::reconstruct_


# %% cluster_ ----
#' Generic for clustering
#'
#' `execution_config` is where and with what the fit runs, as for `train_()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cluster_ <- new_generic(
  "cluster_",
  "config",
  function(config, x, execution_config = NULL, verbosity = 1L) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::cluster_


# %% cluster_membership ----
#' Generic for extracting a clustering's membership matrix
#'
#' Dispatches on the *config* class, which `cluster()` holds. The default
#' method returns NULL, so an algorithm that fits no membership matrix needs no
#' method and `cluster()` builds a `HardClustering` for it. An algorithm that
#' does fit one returns it with **column j corresponding to cluster label j**;
#' `SoftClustering`'s validator checks what it can of that, but permuting the
#' backend's columns into rtemis' order is the method's responsibility.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cluster_membership <- new_generic(
  "cluster_membership",
  "config",
  function(config, clust) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::cluster_membership


# %% cluster_k ----
#' Generic for the number of clusters a fit produced
#'
#' Only consulted when the config does not prescribe `k`. Returns the number of
#' **fitted** clusters -- excluding a noise label, and including a cluster that
#' won no case -- which is not in general the number of distinct labels.
#'
#' The default method aborts: label counting is correct only where a backend's
#' non-noise labels enumerate its fitted clusters, so an algorithm that
#' discovers `k` defines a method stating where its count comes from.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cluster_k <- new_generic(
  "cluster_k",
  "config",
  function(config, clust) {
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::cluster_k


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
#' space fails before any grid cell runs, and again immediately before
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


# %% get_learning_curve ----
#' Learning curve of a fitted model
#'
#' @description
#' The loss recorded at every step of training, as data:
#' one row per step, with the training and validation loss where the algorithm
#' records them.
#'
#' Returns NULL for an algorithm that records no learning curve, so it is safe
#' to call on any model.
#'
#' @param x `Supervised` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return data.frame with columns `iteration`, `loss_training` and
#' `loss_validation`, carrying attributes `unit` and `selected`; or NULL.
#'
#' @author EDG
#' @export
#' @examplesIf interactive()
#' dat <- set_outcome(iris[, 1:4], "Sepal.Length")
#' mod <- train(
#'   dat[1:100, ],
#'   dat_validation = dat[101:150, ],
#'   hyperparameters = setup_LINAD(max_leaves = 12L)
#' )
#' get_learning_curve(mod)
#'
#' @seealso [plot_learning], which draws it
get_learning_curve <- new_generic("get_learning_curve", "x")


# %% learning_curve_super ----
#' Learning curve of a fitted model object
#'
#' The per-algorithm half of [get_learning_curve]: dispatches on the fitted
#' model class and returns the curve in one shape whatever the algorithm's own
#' unit of progress is. A missing method means the algorithm records no curve,
#' which `get_learning_curve()` reports as NULL.
#'
#' @param model Fitted model object.
#'
#' @return data.frame with `iteration`, `loss_training` and `loss_validation`,
#' carrying `unit` and `selected` attributes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
learning_curve_super <- new_generic(
  "learning_curve_super",
  "model",
  function(model) {
    force_supplied()
    S7_dispatch()
  }
)


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
#' mod <- train(iris, hyperparameters = setup_CART(), outer_resampling_config = setup_KFold())
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


# %% review ----
#' Review a trained supervised model
#'
#' @description
#' Assess a trained model: whether its evaluation can be trusted given the
#' sample size and the number of predictors, whether it performs better than a
#' baseline that ignores the predictors, and whether it overfits. The review
#' is deterministic: it draws no random numbers.
#'
#' @details
#' The review holds every value it rests on, so a reader can judge for
#' themselves: `@sample` (sample sizes, input predictors and the columns the
#' learner received), `@class_counts`, `@performance` (one row per metric:
#' training, test, training minus test, the spread over resamples, and the
#' test interval or pooled value), `@baseline` (one row per comparison with a
#' reference that ignores the predictors, naming the reference and the method)
#' and `@tuning` (tuned hyperparameters against their search range). Its
#' findings state what it observes. Printing shows a summary: one row per
#' metric -- mean (SD) over resamples for a resampled model -- the baseline
#' comparisons, the findings and the limitations.
#'
#' **Single split.** The test cases are independent of the fitted model, so
#' the review computes confidence intervals and two-sided comparisons at
#' `confidence_level`. Accuracy is compared with always predicting the most
#' common training class by the exact McNemar test, which pairs the two
#' predictors' results case by case. Balanced accuracy and AUC are compared
#' with their chance levels (1/K, 0.5) through their intervals:
#' per-class binomial variances for balanced accuracy, the DeLong method for
#' AUC. The Brier score, MSE and MAE are compared with a constant baseline
#' (training proportion, training mean) through a paired t interval of the
#' per-case loss reduction; the skill score is reported as a point estimate.
#' An interval that cannot be computed -- too few cases, a class absent from
#' the test set, an AUC of 0 or 1 -- is left unset and no verdict is made from
#' it. A training value of the headline metric (balanced accuracy, or mean
#' squared error for regression) outside its test interval is reported as a
#' diagnostic sign of possible overfitting; it is not a test of the
#' train-test difference.
#'
#' **Resampled models.** Resamples share training cases, so their test
#' results are dependent, and the review makes no interval or test from them.
#' Every metric is reported as its mean and standard deviation over the outer
#' resamples, each baseline is fit to the training data of its resample, and
#' the review counts the resamples in which the model beat its baseline. When
#' every case is tested once, as with k-fold resampling, pooled out-of-sample
#' values are reported as descriptions for metrics that average over cases;
#' AUC is not pooled, since it would rank scores from different fitted models
#' together.
#'
#' A tuned hyperparameter selected at the edge of the values searched is
#' reported, since a better value may lie beyond it.
#'
#' The cases-per-predictor check counts the columns the learner received,
#' after preprocessing and decomposition. Its threshold is a rule of thumb from
#' logistic regression (events per variable; see References), reported for
#' context.
#'
#' Preprocessing, decomposition and tuning inside `train()` are fitted on the
#' training cases of each split and only applied to its test cases. Steps
#' taken before the data is passed to `train()` -- selecting predictors or
#' transforming cases using all the data, or choosing among models by their
#' test performance -- can bias evaluation, and the fitted model cannot show
#' whether they happened. Performance metrics also cannot establish whether a
#' model is useful. Every review states both.
#'
#' @param x `Supervised` or `SupervisedRes` object: A trained model, as returned
#'   by [train].
#' @param confidence_level Optional Numeric (0, 1): Confidence level of every
#'   interval. NULL uses 0.95.
#' @param min_cases_per_predictor Optional Numeric (0, Inf): Training cases per
#'   learner column -- minority-class cases for classification -- below which
#'   the review notes a shortfall. NULL uses 10, an events-per-variable rule of
#'   thumb from logistic regression (see References).
#' @param ... Not used.
#'
#' @return `SupervisedReview` object, whose tables are data.frames.
#'
#' @references
#' Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits
#' illustrated in the case of the binomial. Biometrika, 26(4), 404-413.
#'
#' DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under
#' two or more correlated receiver operating characteristic curves: a
#' nonparametric approach. Biometrics, 44(3), 837-845.
#'
#' McNemar Q (1947). Note on the sampling error of the difference between
#' correlated proportions or percentages. Psychometrika, 12(2), 153-157.
#'
#' Peduzzi P, Concato J, Kemper E, Holford TR, Feinstein AR (1996). A
#' simulation study of the number of events per variable in logistic regression
#' analysis. Journal of Clinical Epidemiology, 49(12), 1373-1379.
#'
#' @author EDG
#' @export
#'
#' @examples
#' idx <- c(1:40, 51:90, 101:140)
#' mod <- train(
#'   iris[idx, ],
#'   dat_test = iris[-idx, ],
#'   hyperparameters = setup_CART(),
#'   verbosity = 0L
#' )
#' review(mod)
#'
#' # Resampled
#' mod_res <- train(
#'   iris,
#'   hyperparameters = setup_CART(),
#'   outer_resampling_config = setup_KFold(5L),
#'   verbosity = 0L
#' )
#' review(mod_res)
review <- new_generic(
  "review",
  "x",
  function(
    x,
    confidence_level = NULL,
    min_cases_per_predictor = NULL,
    ...
  ) {
    force_supplied()
    S7_dispatch()
  }
)


# %% ai_review ----
#' Write an assessment of a model review with a language model
#'
#' @description
#' Ask a language model to write a summary, an evaluation, next steps and
#' caveats from a [review] of a trained supervised model. Every statement cites
#' the codes of the review findings it rests on, and the result keeps the
#' review and a record of how the text was produced.
#'
#' @details
#' The model receives the review as JSON and, if given, `context`: the
#' question the model addresses, the costs of different errors, how its
#' predictions will be used. It never receives the data. Without context, the
#' assessment states that usefulness cannot be judged.
#'
#' The model answers in a declared structure whose codes are restricted to the
#' review's findings, and the answer is checked: an answer that does not match
#' the structure, or cites a code that is not a finding of the review, is an
#' error. The returned object records the model, the provider, the
#' temperature, the full prompt, a SHA-256 hash of the review sent and the
#' time, so the assessment can be audited and, as far as the model allows,
#' reproduced.
#'
#' The written assessment is an interpretation of the review, which remains
#' the evidence; read them together.
#'
#' Requires the `rtemis.llm` package and access to a model.
#'
#' @param x `SupervisedReview`, `Supervised` or `SupervisedRes` object: A
#'   review, or a trained model to review with default settings.
#' @param llm `rtemis.llm` `LLM` or `Agent` object: The model to write the
#'   assessment, for example from `rtemis.llm::create_Ollama()` or
#'   `rtemis.llm::create_Anthropic()`.
#' @param context Optional Character: Domain context for the assessment.
#' @param temperature Optional Numeric [0, Inf): Sampling temperature for this
#'   call. NULL uses the model's own setting.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used.
#'
#' @return `AISupervisedReview` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires a running Ollama server with the model pulled.
#' mod <- train(
#'   iris,
#'   hyperparameters = setup_CART(),
#'   outer_resampling_config = setup_KFold(5L),
#'   verbosity = 0L
#' )
#' llm <- rtemis.llm::create_Ollama(model_name = "gemma4:e4b")
#' ai_review(
#'   mod,
#'   llm = llm,
#'   context = "Teaching example; no decisions depend on the predictions."
#' )
#' }
ai_review <- new_generic(
  "ai_review",
  "x",
  function(
    x,
    llm,
    context = NULL,
    temperature = NULL,
    verbosity = 1L,
    ...
  ) {
    force_supplied()
    S7_dispatch()
  }
)

# %% get_hyperparams_need_tuning ----
#' Get hyperparameters that need tuning.
#'
#' @return Character vector of hyperparameter names that need tuning.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_hyperparams_need_tuning <- new_generic("get_hyperparams_need_tuning", "x")


# %% tuning_grid ----
#' Tuning grid of a Hyperparameters object
#'
#' @description
#' The set of hyperparameter combinations a grid search would fit, one per row.
#' Derived from the object, so it can be inspected before `train()` is called.
#'
#' @details
#' The grid is the cross product of the search values, reduced by two rules:
#'
#' * A hyperparameter that declares it applies only under certain values of
#'   another is set to `NA` -- meaning "left unset for this fit" -- in the rows
#'   that do not meet them. `reduce_basis` applies only at `smoothness_orders`
#'   of 0, so a search over both drops it from the higher-order rows.
#' * Rows that become identical once that is done are collapsed, so a
#'   combination is never fit, scored, and ranked twice.
#'
#' `tune_GridSearch()` fits exactly these rows, so what this prints is what will
#' run.
#'
#' @param x `Hyperparameters` object.
#'
#' @return data.frame with one row per combination and one column per searched
#'   hyperparameter, or NULL if nothing needs tuning.
#'
#' @author EDG
#' @export
#' @examples
#' # reduce_basis applies only at smoothness order 0, so the six combinations
#' # of the cross product reduce to four.
#' tuning_grid(
#'   setup_HAL(
#'     smoothness_orders = tune_over(0L, 1L, 2L),
#'     reduce_basis = tune_over(0.1, 0.5)
#'   )
#' )
tuning_grid <- new_generic("tuning_grid", "x", function(x) {
  S7_dispatch()
})


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
#' The output carries only what the object's schema declares. Every results
#' schema is `additionalProperties: false`, so a key the schema does not name
#' makes the document invalid against the contract it is published under.
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
  # Read one property at a time, so an omitted computed property's getter is
  # never evaluated.
  nms <- published_prop_names(S7_class(x))
  body <- lapply(nms, function(nm) .to_json_value(prop(x, nm)))
  names(body) <- nms
  body
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
#' @param hyperparameters Optional `Hyperparameters` object: Setup using one of
#' `setup_*` functions. Defines the algorithm used to train the calibration
#' model. NULL uses [setup_Isotonic].
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
#' @section Choosing a calibrator:
#'
#' A calibration map must be monotonic non-decreasing. A map that reorders
#' scores changes the ranking of the predictions, and so changes AUC; a
#' non-decreasing one cannot. [available_calibration] lists the algorithms
#' that carry that guarantee. Any other `Hyperparameters` object is accepted
#' and trained like any other model, but nothing then constrains the map.
#'
#' * [setup_Isotonic] (the default) fits isotonic regression. The map is a step
#'   function, so it merges nearby scores into ties, which moves AUC slightly
#'   in either direction. Fitted probabilities are held at least `1 / (2 * n)`
#'   away from 0 and 1, `n` being the number of calibration cases, so a block
#'   of uniformly labelled cases does not assert certainty.
#' * [setup_MonotonicHAL] fits a monotonic Highly Adaptive Lasso on the logit
#'   scale. At its default `smoothness_orders = 1` the map is continuous and
#'   strictly increasing, so it preserves AUC exactly -- but the constraint
#'   that achieves this also makes the map convex on the logit scale, which
#'   costs accuracy whenever the correction needed is concave.
#'   `smoothness_orders = 0` with `penalized = FALSE` lifts that restriction,
#'   at the cost of ties and a markedly slower fit.
#'
#' Isotonic is the default because no monotonic lasso configuration is
#' uniformly better and every one of them is several times slower.
#' `data-raw/benchmark_calibrators.R` reproduces the comparison.
#'
#' The calibrator that ran is recorded on the returned object's `@calibrator`
#' property, is shown by `print()`, and is serialized by `to_json()`.
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
#'   outer_resampling_config = setup_KFold(n_resamples = 3L)
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
    hyperparameters = NULL,
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


# %% training_device ----
#' The compute device an algorithm will train on, or NULL
#'
#' Answered before training starts, so `train()` can name it in its resources
#' line, and again by the algorithm when it fits. Free of side effects beyond
#' an error for a device the machine lacks.
#'
#' `requested` is the execution config's `DeviceConfig`, or NULL for automatic
#' selection. A method returns the device type it will use: the requested one if
#' the algorithm can use it, the CPU if it cannot, or its automatic choice.
#' NULL means the CPU by definition -- every algorithm without a method.
#'
#' @keywords internal
#' @noRd
training_device <- new_generic(
  "training_device",
  "x",
  function(x, requested = NULL) {
    force_supplied()
    S7_dispatch()
  }
)


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
#' Reports keep every published property, including observed state. Other
#' objects keep every property `prop_serialized()` admits, so a flat config
#' drops the same fields a config family does. Config-family classes (`Hyperparameters`,
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
  artifact <- attr(S7_class(x), "rtemis_artifact_schema")
  publication <- schema_publication(S7_class(x))
  observed <- !is.null(publication) && publication@kind == "report"
  keep <- vapply(
    names(values),
    function(nm) {
      # Reconstructed classes serialize the document they were read from,
      # including report state and family discriminators.
      if (!is.null(artifact)) {
        return(nm %in% names(artifact[["properties"]]))
      }
      # A property this object holds but does not declare cannot be judged, so
      # it is kept.
      is.null(declared[[nm]]) ||
        if (observed) {
          prop_published(declared[[nm]])
        } else {
          prop_serialized(declared[[nm]])
        }
    },
    logical(1L)
  )
  values <- values[artifact_present_names(x, names(values)[keep])]
  for (nm in names(values)) {
    if (!is.null(declared[[nm]])) {
      values[nm] <- list(wire_value(values[[nm]], declared[[nm]]))
    }
  }
  values
} # /rtemis::serializable_props.S7_object


# %% S7_to_list ----
S7_to_list <- function(x) {
  if (S7_inherits(x)) {
    x <- serializable_props(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, S7_to_list)
  }
  x
} # /rtemis::S7_to_list


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
