# train_LINADForest.R
# ::rtemis::
# 2026- EDG rtemis.org

# LINADForest is implemented in this package rather than wrapped from a backend.
# The tree engine lives in `R/linad.R`, the bagging layer in `R/linad_forest.R`;
# this file is the rtemis interface to them.

# %% make_linadforest_tree_runner ----
#' Build the per-tree body
#'
#' Returns the closure `progress_plapply()` dispatches once per tree.
#'
#' Built by a factory rather than inline in `train_()` because serializing a
#' closure walks its enclosing environments: a body defined in `train_()`'s
#' frame would ship that entire frame to every worker. This frame holds only
#' what a tree needs.
#'
#' @param x data.frame: Features, unencoded.
#' @param y Numeric vector: Outcome; `{-1, +1}` for classification.
#' @param case_weights Numeric vector: Case weights.
#' @param type Character: "Regression" or "Classification".
#' @param y_levels Optional Character: Outcome levels for a classification.
#' @param settings List: `linadforest_settings()` output.
#' @param bags List: One integer vector of row indices per tree.
#'
#' @return Function of `(b)` returning one `linadforest_tree()` result.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_linadforest_tree_runner <- function(
  x,
  y,
  case_weights,
  type,
  y_levels,
  settings,
  bags
) {
  force(x)
  force(y)
  force(case_weights)
  force(type)
  force(y_levels)
  force(settings)
  force(bags)
  function(b) {
    linadforest_tree(
      x = x,
      y = y,
      case_weights = case_weights,
      type = type,
      y_levels = y_levels,
      settings = settings,
      bag = bags[[b]]
    )
  }
} # /rtemis::make_linadforest_tree_runner


# %% train_.LINADForestHyperparameters ----
#' Train a LINADForest
#'
#' Train a bagged ensemble of Linear Additive Trees. Each tree is grown on a
#' bootstrap sample of the cases, restricted to a random subset of the features,
#' and chooses its own number of leaves on the cases its bootstrap left out.
#'
#' LINADForest needs no encoding preprocessor, for the reason [setup_LINAD]
#' gives: splits are searched and routed on the features as given, and each tree
#' builds a reference-coded design matrix internally.
#'
#' The forest builds its own validation sets from the bootstrap, so it takes no
#' `dat_validation` and is deliberately absent from `early_stopping_algs`.
#'
#' Missing values are not supported: every case contributes to every node model
#' through the soft weights, so there is no node at which an absent value can be
#' ignored.
#'
#' The "train_*" functions train a single model.
#' Use [train] for tuning and testing using nested cross-validation.
#'
#' @param hyperparameters `LINADForestHyperparameters` object: make using
#' [setup_LINADForest].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used -- the forest selects
#' each tree's size on that tree's out-of-bag cases.
#' @param execution_config `ExecutionConfig` object: Supplies the master seed
#' and the workers the trees are dispatched over.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `LINADForest` and a NULL preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LINADForestHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(x = x, allow_missing = FALSE, verbosity = verbosity)
  type <- supervised_type(x)
  features <- as.data.frame(features(x))
  outcome_values <- outcome(x)
  y_levels <- NULL
  if (type == "Classification") {
    if (nlevels(outcome_values) > 2L) {
      rtemis.core::abort(
        "LINADForest does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    y_levels <- levels(outcome_values)
    # +1 on the second level, so 1 / (1 + exp(-2F)) is the probability of the
    # positive class with no sign correction anywhere downstream.
    y <- ifelse(outcome_values == y_levels[[2L]], 1, -1)
  } else {
    y <- as.numeric(outcome_values)
  }
  settings <- linadforest_settings(hyperparameters, ncol(features))

  if (is.null(weights)) {
    weights <- rep(1, NROW(features))
  }
  check_case_weights(weights, NROW(features))
  # Rescaled to average 1 so `lambda`, which multiplies the weight total, means
  # the same thing whether or not inverse frequency weighting is on. Each tree
  # rescales its own bag again, for the same reason.
  weights <- weights / mean(weights)

  # Train ----
  n_trees <- settings[["n_trees"]]
  bags <- linadforest_bags(y, n_trees, seed = execution_config@seed)
  grown <- progress_plapply(
    seq_len(n_trees),
    make_linadforest_tree_runner(
      x = features,
      y = y,
      case_weights = weights,
      type = type,
      y_levels = y_levels,
      settings = settings,
      bags = bags
    ),
    backend = execution_config@backend,
    n_workers = hyperparameters@n_workers,
    future_plan = resolve_future_plan(
      execution_config@backend,
      execution_config@future_plan
    ),
    # One independent RNG substream per tree, assigned by tree index, so the
    # feature sampling is the same sequentially and in parallel at any worker
    # count.
    seeds = rng_substreams(execution_config@seed, n_trees),
    label = "Trees",
    # A forest quietly missing trees is worse than one that failed, so a tree
    # failure stops the run rather than being aggregated over.
    stop_on_error = TRUE,
    verbosity = verbosity
  )

  # Out-of-bag ----
  n <- nrow(features)
  oob_prediction <- linadforest_oob_prediction(
    lapply(grown, `[[`, "oob"),
    lapply(grown, `[[`, "oob_prediction"),
    n
  )
  oob_metrics <- linadforest_oob_metrics(oob_prediction, outcome_values, type)
  # How many times each case appears in each bag: what the infinitesimal
  # jackknife reads, and the only training-set-sized state the model keeps.
  bag_counts <- vapply(bags, function(bag) tabulate(bag, n), integer(n))

  trees <- lapply(grown, `[[`, "tree")
  model <- LINADForest(
    trees = trees,
    bag_counts = bag_counts,
    oob_prediction = oob_prediction,
    oob_metrics = oob_metrics,
    xnames = names(features),
    xlev = lapply(Filter(is.factor, features), levels),
    type = type,
    y_levels = y_levels
  )
  if (verbosity > 0L) {
    info(
      "Grew ",
      length(trees),
      ngettext(length(trees), " tree", " trees"),
      " of ",
      ddSci(mean(vapply(trees, function(tree) tree@n_leaves, integer(1L)))),
      " leaves on average."
    )
  }

  check_is_S7(model, LINADForest)
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.LINADForestHyperparameters


# %% predict_super.LINADForest ----
#' Predict from a LINADForest
#'
#' The mean over trees: of the fitted values for a regression, and of the
#' probabilities for a classification. Averaging probabilities rather than the
#' additive-scale values each tree was grown against is the bagging convention,
#' and it keeps this method returning what its contract says it returns.
#'
#' Each tree subsets `newdata` to the features it holds, so `mtry_tree` needs no
#' code here at all.
#'
#' @param model `LINADForest` object.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector: fitted values, or the probability of the positive
#' class.
#'
#' @keywords internal
#' @noRd
method(predict_super, LINADForest) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  newdata <- as.data.frame(newdata)[, model@xnames, drop = FALSE]
  rowMeans(linadforest_tree_predictions(model@trees, newdata))
} # /rtemis::predict_super.LINADForest


# %% varimp_super.LINADForest ----
#' Variable importance from a LINADForest
#'
#' Each tree's two measures, averaged over the forest. A feature a tree did not
#' hold contributes zero to that tree, which is what makes the average read as
#' "the effect this feature had on the forest" rather than "on the trees that
#' happened to draw it" -- the second would rate a rarely drawn feature by the
#' few trees it reached.
#'
#' \describe{
#'   \item{`importance`}{The feature's linear effect, averaged over trees.}
#'   \item{`split_gain`}{The feature's partitioning effect, averaged over trees.}
#' }
#'
#' See `varimp_super.LinearAdditiveTree` for what each measures within one tree.
#'
#' @param model `LINADForest` object.
#'
#' @return `VariableImportance` object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, LINADForest) <- function(model, ...) {
  n_trees <- length(model@trees)
  importance <- numeric(length(model@xnames))
  split_gain <- importance
  names(importance) <- model@xnames
  names(split_gain) <- model@xnames
  for (tree in model@trees) {
    per_tree <- varimp_super(tree)@data
    at <- match(per_tree[["variable"]], model@xnames)
    importance[at] <- importance[at] + per_tree[["importance"]]
    split_gain[at] <- split_gain[at] + per_tree[["split_gain"]]
  }
  VariableImportance(
    data.table(
      variable = model@xnames,
      importance = unname(importance) / n_trees,
      split_gain = unname(split_gain) / n_trees
    )
  )
} # /rtemis::varimp_super.LINADForest


# %% learning_curve_super.LINADForest ----
#' Learning curves of every tree in a LINADForest
#'
#' Each tree selects its own size on its own out-of-bag cases, so a forest has
#' one curve per tree rather than one curve. The rows carry a `tree` column and
#' the spread of selected sizes is the interesting part.
#'
#' @param model `LINADForest` object.
#'
#' @return data.frame with a `tree` column added to the usual shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(learning_curve_super, LINADForest) <- function(model) {
  curves <- lapply(seq_along(model@trees), function(index) {
    curve <- learning_curve_super(model@trees[[index]])
    cbind(tree = index, curve)
  })
  out <- do.call(rbind, curves)
  attr(out, "unit") <- "leaves"
  # One number for a forest of selections: the typical tree's size.
  attr(out, "selected") <- as.integer(round(stats::median(
    vapply(model@trees, function(tree) tree@n_leaves, integer(1L))
  )))
  out
} # /rtemis::learning_curve_super.LINADForest


# %% se_super.LINADForest ----
#' Standard errors of a LINADForest's predictions
#'
#' The infinitesimal jackknife of Wager, Hastie and Efron (2014), computed by
#' `linadforest_jackknife()`. The spread of predictions across trees is a
#' dispersion rather than a standard error of the fit, so it is not what is
#' reported here.
#'
#' The estimator is a covariance across bags, so it is NA for a single-tree
#' forest and noisy for a small one -- the Monte-Carlo correction is the term
#' that grows as the tree count falls.
#'
#' @param model `LINADForest` object.
#' @param newdata tabular data: Data to compute standard errors for.
#'
#' @keywords internal
#' @noRd
method(se_super, LINADForest) <- function(model, newdata) {
  newdata <- as.data.frame(newdata)[, model@xnames, drop = FALSE]
  linadforest_jackknife(
    linadforest_tree_predictions(model@trees, newdata),
    model@bag_counts
  )
} # /rtemis::se_super.LINADForest
