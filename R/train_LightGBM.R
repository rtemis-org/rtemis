# train_LightGBM.R
# ::rtemis::
# 2025- EDG rtemis.org

# LightGBM parameters
# https://lightgbm.readthedocs.io/en/latest/Parameters.html

# %% train_.LightGBMHyperparameters ----
#' Gradient Boosting with LightGBM
#'
#' @param hyperparameters `LightGBMHyperparameters` object: make using [setup_LightGBM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Validation set for early stopping.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LightGBMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # When nrounds is unset (NULL), train up to max_nrounds using early
  # stopping. (During tuning, the best iteration is captured as best_iter.)
  if (is.null(hyperparameters[["nrounds"]])) {
    hyperparameters@nrounds <- hyperparameters[["max_nrounds"]]
  }

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)

  ## Objective ----
  if (type == "Classification") {
    nclasses <- nlevels(outcome(x))
  } else {
    nclasses <- 1L
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@objective <- if (type == "Regression") {
      "regression"
    } else {
      if (nclasses == 2L) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }

  ## Preprocess & create lgb.Datasets ----
  lgb_data <- prepare_lgb_data(
    x = x,
    dat_validation = dat_validation,
    type = type,
    weights = weights,
    verbosity = verbosity
  )
  x <- lgb_data[["train_data"]]
  dat_validation <- lgb_data[["valid_data"]]
  prp <- lgb_data[["preprocessor"]]

  # Train ----
  params <- hyperparameters@hyperparameters
  params[["nrounds"]] <- params[["max_nrounds"]] <- params[[
    "early_stopping_rounds"
  ]] <- params[["force_nrounds"]] <- params[["ifw"]] <- NULL

  # num_class is required for multiclass classification only, must be 1 or unset for regression & binary classification
  if (nclasses > 2L) {
    params[["num_class"]] <- nclasses
  }
  # Set n threads
  params[["num_threads"]] <- prop(hyperparameters, "n_workers")

  model <- lightgbm::lgb.train(
    params = params,
    data = x,
    nrounds = hyperparameters[["nrounds"]],
    valids = if (!is.null(dat_validation)) {
      list(training = x, validation = dat_validation)
    } else {
      list(training = x)
    },
    early_stopping_rounds = hyperparameters[["early_stopping_rounds"]],
    verbose = verbosity - 1L
  )
  check_inherits(model, "lgb.Booster")
  # `hyperparameters` is returned because this method resolved values into
  # it (R copied the caller's object, so the caller cannot see them).
  # `train()` adopts them, and the fitted model reports what it used.
  list(model = model, preprocessor = prp, hyperparameters = hyperparameters)
} # /rtemis::train_.LightGBMHyperparameters


# %% predict_super.class_lgb.Booster ----
#' Predict from LightGBM model
#'
#' @param model lgb.Booster object.
#' @param newdata tabular data: Data to predict on. Will have been preprocessed by
#' `predict.Supervised` before calling this method if algorithm-specific preprocessing was performed during training.
#' @param type Character: Type of supervised learning.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_lgb.Booster) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(model, "lgb.Booster")
  check_inherits(newdata, "data.frame")

  # Algorithm-specific preprocessing (factor2integer) is applied by
  # predict.Supervised before calling this method. See R/train.R and R/200_Supervised.R

  # Predict ----
  predict(model, newdata = as.matrix(newdata))
} # /rtemis::predict_super.lgb.Booster


# %% varimp_super.class_lgb.Booster ----
#' Get variable importance from LightGBM model
#'
#' @param model lgb.Booster object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_lgb.Booster) <- function(model) {
  check_inherits(model, "lgb.Booster")
  vi <- lightgbm::lgb.importance(model, percentage = TRUE) # -> data.table
  names(vi)[1] <- "variable"
  VariableImportance(vi)
} # /rtemis::varimp_super.lgb.Booster


# %% explain_super.class_lgb.Booster ----
#' TreeSHAP contributions from a LightGBM booster
#'
#' `predict(type = "contrib")` returns `n x (p + 1)` per class, the trailing
#' column being the baseline. The contributions sum to the **raw margin**, not
#' to what `predict()` returns: for binary and multiclass objectives `predict()`
#' applies the link, and comparing contributions against a probability is the
#' error the additivity test exists to catch.
#'
#' Multiclass is laid out as class-major blocks of `p + 1` columns, so class `k`
#' occupies `(k - 1) * (p + 1) + 1` through `k * (p + 1)`.
#'
#' The booster's own contributions are path-dependent -- coalitions are weighted
#' by the training coverage recorded in the trees -- which is a conditional
#' value function, and it takes no background. An interventional answer needs
#' one and is not what this returns, so it is refused rather than silently
#' relabeled.
#'
#' @param model `lgb.Booster` object.
#' @param newdata tabular data: Cases to explain, already transformed.
#' @param background Optional tabular data: Unused by the path-dependent
#' estimator.
#' @param estimator Character: Resolved estimator.
#' @param perturbation Character: Resolved value function.
#' @param scale Character: Scale the contributions are additive on.
#' @param type Character: "Regression" or "Classification".
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @keywords internal
#' @noRd
method(explain_super, class_lgb.Booster) <- function(
  model,
  newdata,
  background,
  estimator,
  perturbation,
  scale,
  type,
  verbosity = 0L
) {
  check_inherits(model, "lgb.Booster")
  check_inherits(newdata, "data.frame")
  if (!identical(estimator, "TreeSHAP")) {
    rtemis.core::abort(
      "LightGBM's explain_super() computes TreeSHAP, not ",
      estimator,
      ".",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (!identical(perturbation, "conditional")) {
    rtemis.core::abort(
      "Interventional TreeSHAP is not implemented for LightGBM: the booster's ",
      "own contributions are path-dependent, which is a conditional value ",
      "function.\n",
      "Use `setup_SHAP(perturbation = \"conditional\")`, or ",
      "`setup_SHAP(estimator = \"kernel\", perturbation = \"interventional\")`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  features <- as.matrix(newdata)
  contrib <- predict(model, newdata = features, type = "contrib")
  # The margin, which is what the contributions decompose.
  margin <- predict(model, newdata = features, type = "raw")

  n_features <- NCOL(features)
  block <- n_features + 1L
  n_classes <- NCOL(contrib) / block
  if (n_classes != round(n_classes)) {
    rtemis.core::abort(
      "LightGBM returned ",
      NCOL(contrib),
      " contribution columns, which is not a multiple of ",
      block,
      ".",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  n_classes <- as.integer(n_classes)
  # `predict(type = "raw")` is a bare vector for one output and a matrix for
  # several; the object stores one column per class either way.
  margin <- matrix(margin, ncol = n_classes)

  phi <- vector("list", n_classes)
  baseline <- numeric(n_classes)
  for (k in seq_len(n_classes)) {
    columns <- ((k - 1L) * block + 1L):(k * block)
    values <- contrib[, columns, drop = FALSE]
    contributions <- values[, seq_len(n_features), drop = FALSE]
    colnames(contributions) <- colnames(features)
    phi[[k]] <- contributions
    # Constant across cases by construction: it is the model's own expected
    # value, so the first row carries it.
    baseline[[k]] <- values[1L, block]
  }
  list(
    phi = phi,
    baseline = baseline,
    predicted = margin,
    exact = TRUE
  )
} # /rtemis::explain_super.lgb.Booster
