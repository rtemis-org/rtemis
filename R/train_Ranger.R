# train_Ranger.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# https://imbs-hl.github.io/ranger/reference/ranger.html

# %% train_.RangerHyperparameters ----
#' Random Forest using Ranger
#'
#' @param hyperparameters `RangerHyperparameters`: Hyperparameters for Ranger.
#' @param x tabular data: Training data.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data: Validation data (currently unused).
#' @param verbosity Integer: Verbosity level.
#'
#' @return `ranger` model object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, RangerHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("ranger")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Data-dependent constraints (mtry, case_weights, class_weights,
  # always_split_variables) are declared via `data_bound` on the properties and
  # checked by train() via validate_hyperparameters(), before tuning and again
  # before this call.

  # Data ----
  check_supervised(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)

  # Train ----
  model <- ranger::ranger(
    formula = NULL,
    x = features(x),
    y = outcome(x),
    num.trees = hyperparameters@hyperparameters[["num_trees"]],
    mtry = hyperparameters@hyperparameters[["mtry"]],
    importance = hyperparameters@hyperparameters[["importance"]],
    write.forest = hyperparameters@hyperparameters[["write_forest"]],
    probability = type == "Classification",
    min.node.size = hyperparameters@hyperparameters[["min_node_size"]],
    min.bucket = hyperparameters@hyperparameters[["min_bucket"]],
    max.depth = hyperparameters@hyperparameters[["max_depth"]],
    replace = hyperparameters@hyperparameters[["replace"]],
    sample.fraction = hyperparameters@hyperparameters[["sample_fraction"]],
    case.weights = weights,
    splitrule = hyperparameters@hyperparameters[["splitrule"]],
    num.random.splits = hyperparameters@hyperparameters[["num_random_splits"]],
    alpha = hyperparameters@hyperparameters[["alpha"]],
    minprop = hyperparameters@hyperparameters[["minprop"]],
    poisson.tau = hyperparameters@hyperparameters[["poisson_tau"]],
    split.select.weights = hyperparameters@hyperparameters[[
      "split_select_weights"
    ]],
    always.split.variables = hyperparameters@hyperparameters[[
      "always_split_variables"
    ]],
    respect.unordered.factors = hyperparameters@hyperparameters[[
      "respect_unordered_factors"
    ]],
    scale.permutation.importance = hyperparameters@hyperparameters[[
      "scale_permutation_importance"
    ]],
    local.importance = hyperparameters@hyperparameters[["local_importance"]],
    regularization.factor = hyperparameters@hyperparameters[[
      "regularization_factor"
    ]],
    regularization.usedepth = hyperparameters@hyperparameters[[
      "regularization_usedepth"
    ]],
    keep.inbag = hyperparameters@hyperparameters[["keep_inbag"]],
    inbag = hyperparameters@hyperparameters[["inbag"]],
    holdout = hyperparameters@hyperparameters[["holdout"]],
    quantreg = hyperparameters@hyperparameters[["quantreg"]],
    time.interest = hyperparameters@hyperparameters[["time_interest"]],
    oob.error = hyperparameters@hyperparameters[["oob_error"]],
    num.threads = prop(hyperparameters, "n_workers"),
    save.memory = hyperparameters@hyperparameters[["save_memory"]],
    verbose = verbosity > 0L,
    node.stats = hyperparameters@hyperparameters[["node_stats"]],
    seed = hyperparameters@hyperparameters[["seed"]],
    na.action = hyperparameters@hyperparameters[["na_action"]]
  )
  check_inherits(model, "ranger")
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.RangerHyperparameters

#' Predict from Ranger model
#'
#' @param model `ranger` model object.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Character: Prediction type.
#' @param verbosity Integer: Verbosity level.
#' @param ranger_type Character: Ranger prediction type.
#' @param ... Additional arguments passed to ranger predict.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_ranger) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(model, "ranger")
  check_inherits(newdata, "data.frame")

  # Predict ----
  # `ranger::predict()` draws its C++ seed from the R stream when none is
  # given, so an unwrapped call advances the caller's RNG by one -- and a
  # construction that predicts once per fold advances it by the fold count.
  # Preserved rather than seeded: ranger sees the same state it would have, so
  # nothing about the prediction changes.
  predicted <- with_preserved_rng(
    predict(
      model,
      data = newdata,
      type = "response",
      verbose = verbosity > 0L
    )
  )[["predictions"]]
  if (type == "Classification" && NCOL(predicted) == 2L) {
    # In binary classification, ranger returns matrix with 2 columns
    # with probabilities for each class
    predicted <- predicted[, 2L]
  }
  predicted
} # /rtemis::predict_super.class_ranger


# %% quantile_super.class_ranger ----
#' Predict quantiles from a Ranger model
#'
#' A quantile regression forest keeps the training outcomes reaching each
#' terminal node, so one fitted forest answers every level and CQR needs no
#' second fit. That store is what `quantreg = TRUE` builds; without it the
#' forest holds node means and cannot answer at all, which is reported as the
#' training setting it is rather than as a backend error.
#'
#' `keep_inbag` is not required here. It records which cases each tree was
#' grown on, which quantiles of the *training* data need and quantiles of new
#' data do not.
#'
#' @param model `ranger` model object.
#' @param newdata tabular data: Cases to predict.
#' @param quantiles Numeric (0, 1): Levels to predict, in increasing order.
#'
#' @return Numeric matrix, one row per case and one column per level.
#'
#' @keywords internal
#' @noRd
method(quantile_super, class_ranger) <- function(model, newdata, quantiles) {
  check_inherits(model, "ranger")
  check_inherits(newdata, "data.frame")
  if (is.null(model[["random.node.values"]])) {
    rtemis.core::abort(
      "This Ranger forest was not trained for quantile prediction, so it ",
      "holds node means rather than the outcomes a quantile is read from.\n",
      "Retrain with `setup_Ranger(quantreg = TRUE)`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  predicted <- with_preserved_rng(
    predict(
      model,
      data = newdata,
      type = "quantiles",
      quantiles = quantiles
    )
  )[["predictions"]]
  # One level comes back as a bare vector; the caller reads columns either way.
  matrix(predicted, nrow = nrow(newdata), ncol = length(quantiles))
} # /rtemis::quantile_super.class_ranger


# %% varimp_super.class_ranger ----
#' Get variable importance from Ranger model
#'
#' @param model `ranger` model object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_ranger) <- function(model) {
  check_inherits(model, "ranger")
  vi <- ranger::importance(model)
  VariableImportance(
    data.table(
      variable = names(vi),
      importance = unname(vi)
    )
  )
} # /rtemis::varimp_super.class_ranger
