# linad_forest.R
# ::rtemis::
# 2026- EDG rtemis.org

# The bagging layer over the tree engine in `R/linad.R`. Everything here decides
# which cases and which features a tree sees, and how the trees are combined;
# nothing here knows how a tree is grown.

# %% LINADFOREST_MIN_OOB ----
# Fewest out-of-bag cases a tree needs before its size is chosen on them. A
# handful of cases makes a validation curve whose minimum is noise, and the
# whole tree is then selected by it, so below this the tree keeps every leaf.
LINADFOREST_MIN_OOB <- 10L


# %% LINADFOREST_JACKKNIFE_BLOCK ----
# Rows of new data the jackknife scores at a time. Its covariance step forms an
# `n_new x n_train` matrix, so an unblocked implementation allocates the product
# of the two sample sizes.
LINADFOREST_JACKKNIFE_BLOCK <- 512L


# %% linadforest_settings ----
#' Resolve forest hyperparameters into the values the engine runs on
#'
#' The tree-level settings are LINAD's own. The two `mtry` values are resolved
#' from the data here, and deliberately in one expression each: NULL means every
#' feature today, and replacing that with a rule of `p` -- once the synthetic
#' sweep says which rule -- is then a one-line change rather than a redesign.
#'
#' `mtry_split` samples from the features the tree holds, not from every feature
#' in the data, so it is capped by `mtry_tree` rather than by `p`.
#'
#' @param hyperparameters `LINADForestHyperparameters` object.
#' @param n_features Integer: Number of features in the training data.
#'
#' @return Named list of engine settings.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_settings <- function(hyperparameters, n_features) {
  settings <- linad_settings(hyperparameters)
  settings[["n_trees"]] <- hyperparameters[["n_trees"]]
  mtry_tree <- hyperparameters[["mtry_tree"]] %||% n_features
  settings[["mtry_tree"]] <- min(mtry_tree, n_features)
  mtry_split <- hyperparameters[["mtry_split"]] %||% settings[["mtry_tree"]]
  settings[["mtry_split"]] <- min(mtry_split, settings[["mtry_tree"]])
  settings
} # /rtemis::linadforest_settings


# %% linadforest_bags ----
#' Bootstrap samples, one per tree
#'
#' Drawn through the package's own resampler rather than a bare `sample()`, so
#' the bags are produced by the same machinery, and are described by the same
#' vocabulary, as every other resampling in rtemis.
#'
#' @param y Vector: The outcome, used for its length alone.
#' @param n_trees Integer: Number of bootstrap samples.
#' @param seed Optional Integer: Seed for the draw.
#'
#' @return List of `n_trees` integer vectors of row indices, with repeats.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_bags <- function(y, n_trees, seed = NULL) {
  resampler <- resample(
    y,
    config = setup_Resampler(
      n_resamples = n_trees,
      type = "Bootstrap",
      seed = seed,
      verbosity = 0L
    ),
    verbosity = 0L
  )
  resampler@resamples
} # /rtemis::linadforest_bags


# %% linadforest_tree ----
#' Grow one tree of a forest
#'
#' Draws the tree's feature subset, grows it on its bag, and selects its number
#' of leaves on the cases the bag left out -- the validation set a bootstrap
#' produces for free, and the reason a forest needs no tuning to size its trees.
#'
#' The tree's factor levels are the **training** levels, not the bag's, so a
#' level the bag happens to miss still has a column and prediction on new data
#' carrying it cannot fail.
#'
#' @param x data.frame: Features, unencoded, all cases and all columns.
#' @param y Numeric vector: Outcome; `{-1, +1}` for classification.
#' @param case_weights Numeric vector: Case weights over all cases.
#' @param type Character: "Regression" or "Classification".
#' @param y_levels Optional Character: Outcome levels for a classification.
#' @param settings List: `linadforest_settings()` output.
#' @param bag Integer vector: Row indices of the tree's bootstrap sample.
#'
#' @return List with the fitted `LinearAdditiveTree`, the out-of-bag rows, and
#' this tree's prediction of them.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_tree <- function(
  x,
  y,
  case_weights,
  type,
  y_levels,
  settings,
  bag
) {
  n_features <- ncol(x)
  columns <- if (settings[["mtry_tree"]] < n_features) {
    sort(sample.int(n_features, settings[["mtry_tree"]]))
  } else {
    seq_len(n_features)
  }
  features <- x[bag, columns, drop = FALSE]
  weights <- case_weights[bag]
  weights <- weights / mean(weights)
  xlev <- lapply(Filter(is.factor, features), levels)
  xm <- linad_design_matrix(features, xlev)
  oob <- setdiff(seq_len(nrow(x)), bag)
  # A tree's own out-of-bag cases are its validation set, so `patience` needs
  # no held-out data of its own here -- and the growth it saves multiplies by
  # the tree count.
  validation <- if (
    !is.null(settings[["patience"]]) && length(oob) >= LINADFOREST_MIN_OOB
  ) {
    oob_features <- x[oob, columns, drop = FALSE]
    list(
      x = oob_features,
      xm = linad_design_matrix(oob_features, xlev),
      y = y[oob]
    )
  }
  fitted <- linad_fit(
    x = features,
    xm = xm,
    y = y[bag],
    case_weights = weights,
    type = type,
    settings = settings,
    validation = validation,
    verbosity = 0L
  )
  tree <- LinearAdditiveTree(
    frame = fitted[["frame"]],
    coefficients = fitted[["coefficients"]],
    steps = fitted[["steps"]],
    n_leaves = as.integer(fitted[["n_leaves"]]),
    xnames = names(features),
    xlev = xlev,
    design_assign = as.integer(attr(xm, "assign")),
    design_names = colnames(xm),
    design_scale = linad_scaling(xm)[["scale"]],
    type = type,
    y_levels = y_levels,
    settings = fitted[["settings"]],
    leaf_curve = NULL,
    training_curve = NULL
  )

  tree@training_curve <- linad_size_curve(tree, features, xm, y[bag], type)
  if (
    !settings[["force_max_leaves"]] &&
      length(tree@steps) > 1L &&
      length(oob) >= LINADFOREST_MIN_OOB
  ) {
    oob_features <- x[oob, columns, drop = FALSE]
    oob_y <- y[oob]
    selection <- linad_select_leaves(
      tree,
      oob_features,
      linad_design_matrix(oob_features, xlev),
      oob_y,
      type,
      smooth = settings[["smooth_validation_curve"]]
    )
    tree@n_leaves <- as.integer(selection[["n_leaves"]])
    tree@leaf_curve <- selection[["curve"]]
  }
  # The frame's leaf flags describe the fully grown tree; at the selected size
  # the terminal set is `steps[[n_leaves]]`, so they are brought into agreement
  # here rather than left to mislead every later reader.
  tree@frame[["is_leaf"]] <- tree@frame[["node"]] %in%
    tree@steps[[tree@n_leaves]]

  list(
    tree = tree,
    oob = oob,
    oob_prediction = if (length(oob) > 0L) {
      predict_super(tree, x[oob, , drop = FALSE])
    } else {
      numeric(0)
    }
  )
} # /rtemis::linadforest_tree


# %% linadforest_oob_prediction ----
#' Aggregate the out-of-bag predictions of every tree
#'
#' A case is predicted by the trees that did not hold it, which is the forest's
#' error estimate with no resampling and no held-out data. A case in every bag
#' -- possible, if unlikely, at small `n_trees` -- has no such trees and is NA
#' rather than silently predicted by trees that saw it.
#'
#' @param oob List: One integer vector of row indices per tree.
#' @param predictions List: One numeric vector per tree, aligned with `oob`.
#' @param n Integer: Number of training cases.
#'
#' @return Numeric vector of length `n`, NA where no tree left the case out.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_oob_prediction <- function(oob, predictions, n) {
  total <- numeric(n)
  count <- integer(n)
  for (b in seq_along(oob)) {
    rows <- oob[[b]]
    if (length(rows) == 0L) {
      next
    }
    total[rows] <- total[rows] + predictions[[b]]
    count[rows] <- count[rows] + 1L
  }
  out <- rep(NA_real_, n)
  covered <- count > 0L
  out[covered] <- total[covered] / count[covered]
  out
} # /rtemis::linadforest_oob_prediction


# %% linadforest_oob_metrics ----
#' Score the out-of-bag predictions
#'
#' @param oob_prediction Numeric vector: `linadforest_oob_prediction()` output.
#' @param outcome Vector: The training outcome, as the user supplied it.
#' @param type Character: "Regression" or "Classification".
#'
#' @return `Metrics` object, or NULL if no case had an out-of-bag prediction.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_oob_metrics <- function(oob_prediction, outcome, type) {
  covered <- !is.na(oob_prediction)
  if (!any(covered)) {
    return(NULL)
  }
  predicted <- oob_prediction[covered]
  # `METRICS_SAMPLES` is a closed set with no out-of-bag member, and of the six
  # it has, "Validation" is what these cases are: held-out data on which a
  # modeling choice -- each tree's number of leaves -- was made. Labelling them
  # "Training" would name the rows correctly and invite exactly the in-sample
  # reading the estimate exists to avoid.
  if (type == "Classification") {
    levels_y <- levels(outcome)
    classification_metrics(
      true_labels = outcome[covered],
      predicted_labels = prob2categorical(predicted, levels_y),
      predicted_prob = prob_matrix(predicted, levels_y),
      sample = "Validation",
      verbosity = 0L
    )
  } else {
    regression_metrics(
      true = as.numeric(outcome)[covered],
      predicted = predicted,
      sample = "Validation"
    )
  }
} # /rtemis::linadforest_oob_metrics


# %% linadforest_tree_predictions ----
#' Every tree's prediction of every new case
#'
#' @param trees List: `LinearAdditiveTree` objects.
#' @param newdata data.frame: Data to predict on.
#'
#' @return Numeric matrix, cases x trees.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_tree_predictions <- function(trees, newdata) {
  # Shaped explicitly: `vapply()` drops to a vector for a single case, and the
  # aggregation and the jackknife both read this as cases x trees.
  matrix(
    vapply(
      trees,
      function(tree) predict_super(tree, newdata),
      numeric(NROW(newdata))
    ),
    nrow = NROW(newdata),
    ncol = length(trees)
  )
} # /rtemis::linadforest_tree_predictions


# %% linadforest_jackknife ----
#' Infinitesimal-jackknife standard errors of a bagged prediction
#'
#' The spread of predictions across trees is a dispersion, not a standard error
#' of the fit, so this is the estimator of Wager, Hastie and Efron (2014) --
#' the same one `ranger` reaches through `se.method = "infjack"`:
#'
#' ```
#' V_IJ   = sum_i cov_b(N_bi, t_b(x))^2
#' V_IJ_U = V_IJ - (n / B^2) * sum_b (t_b(x) - t_bar(x))^2
#' ```
#'
#' `N_bi` counts case `i` in bag `b`, which is why the fitted forest stores the
#' bag counts. The Monte-Carlo correction can overshoot at finite `B`, taking
#' the variance below zero; it is clamped, as `ranger` clamps it.
#'
#' The covariance step pairs every new case with every training case, so it runs
#' in blocks of new cases rather than allocating that product whole.
#'
#' @param predictions Numeric matrix: New cases x trees.
#' @param bag_counts Integer matrix: Training cases x trees.
#'
#' @return Numeric vector: One standard error per new case.
#'
#' @references
#' Wager S, Hastie T, Efron B (2014). Confidence intervals for random forests:
#' the jackknife and the infinitesimal jackknife. \emph{Journal of Machine
#' Learning Research}, 15, 1625-1651.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linadforest_jackknife <- function(predictions, bag_counts) {
  n_trees <- ncol(predictions)
  n_train <- nrow(bag_counts)
  n_new <- nrow(predictions)
  if (n_trees < 2L) {
    return(rep(NA_real_, n_new))
  }
  counts_centered <- bag_counts - rowMeans(bag_counts)
  variance <- numeric(n_new)
  starts <- seq.int(1L, n_new, by = LINADFOREST_JACKKNIFE_BLOCK)
  for (start in starts) {
    rows <- seq.int(start, min(start + LINADFOREST_JACKKNIFE_BLOCK - 1L, n_new))
    block <- predictions[rows, , drop = FALSE]
    block <- block - rowMeans(block)
    covariance <- tcrossprod(block, counts_centered) / n_trees
    variance[rows] <- rowSums(covariance^2) -
      (n_train / n_trees^2) * rowSums(block^2)
  }
  sqrt(pmax(variance, 0))
} # /rtemis::linadforest_jackknife
