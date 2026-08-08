# train_MetaLearner.R
# ::rtemis::
# 2026- EDG rtemis.org

# Machinery shared by the meta learners. The two training implementations live
# beside the fitted classes they build, in `train_StackedLearner.R` and
# `train_ConditionalSuperLearner.R`.

# %% expand_library ----
#' Turn a meta learner's base learners into the library it trains
#'
#' A base learner holding more than one value for a tunable hyperparameter
#' becomes one library entry per combination, which is how the SuperLearner
#' literature handles a search space: the candidates are separate library members
#' and the ensemble's own cross-validation chooses between them, so no inner
#' tuning is needed.
#'
#' A learner that tunes *itself* is left alone. GLMNET with `lambda` unset has a
#' one-row grid -- there is no search space to expand, only a value `cv.glmnet`
#' resolves internally -- so it passes through and `train()` handles it. That
#' distinction is what `NROW(grid) < 2L` tests.
#'
#' @param hyperparameters `MetaLearnerHyperparameters` object.
#'
#' @return Named list with `learners` (the library) and `origin` (a character
#'   vector mapping each entry back to the base learner it came from, which is
#'   how a feature group follows its learner through expansion).
#'
#' @author EDG
#' @keywords internal
#' @noRd
expand_library <- function(hyperparameters) {
  learners <- hyperparameters@base_learners
  if (!hyperparameters[["expand_search_spaces"]]) {
    return(list(
      learners = learners,
      origin = stats::setNames(names(learners), names(learners))
    ))
  }
  out <- list()
  origin <- character()
  for (nm in names(learners)) {
    learner <- learners[[nm]]
    grid <- tuning_grid(learner)
    if (is.null(grid) || NROW(grid) < 2L) {
      out[[nm]] <- learner
      origin[[nm]] <- nm
      next
    }
    for (i in seq_len(NROW(grid))) {
      entry <- paste0(nm, "_", i)
      out[[entry]] <- update(learner, as.list(grid[i, , drop = FALSE]))
      origin[[entry]] <- nm
    }
  }
  list(learners = out, origin = origin)
} # /rtemis::expand_library


# %% meta_entry_features ----
#' The features each library entry sees
#'
#' Always fully resolved, one character vector per entry, so that nothing
#' downstream has to re-derive "all of them" from a frame whose shape differs
#' between training (outcome present) and prediction (features only).
#'
#' Only modality stacking declares groups; for the others every entry gets the
#' full feature set. Expansion can turn one base learner into several entries,
#' and they all inherit its group -- hence `origin`.
#'
#' @param hyperparameters `MetaLearnerHyperparameters` object.
#' @param origin Named character: entry name to base learner name, from
#'   `expand_library()`.
#' @param feature_names Character: Every feature in the training data.
#'
#' @return Named list of character vectors, one per entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_entry_features <- function(hyperparameters, origin, feature_names) {
  groups <- if (prop_exists(hyperparameters, "feature_groups")) {
    hyperparameters@feature_groups
  } else {
    NULL
  }
  out <- lapply(names(origin), function(entry) {
    if (is.null(groups)) feature_names else groups[[origin[[entry]]]]
  })
  names(out) <- names(origin)
  out
} # /rtemis::meta_entry_features


# %% meta_outcome ----
#' Check a meta learner's outcome and return it as a number
#'
#' Every meta learner needs the outcome on a numeric scale: a squared error, a
#' level-one regression, a Brier score. Classification is coded 0/1 on the second
#' factor level, matching rtemis's positive-class convention, and multiclass is
#' rejected here rather than inside one of the V*K nested fits.
#'
#' @param x tabular data: Training set.
#' @param algorithm Character: Name used in the error message.
#'
#' @return Named list with `type`, `y` (the outcome), `y_numeric` and
#'   `y_levels` (NULL for regression).
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_outcome <- function(x, algorithm) {
  type <- supervised_type(x)
  y <- outcome(x)
  if (type == "Classification") {
    if (nlevels(y) > 2L) {
      rtemis.core::abort(
        algorithm,
        " does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    return(list(
      type = type,
      y = y,
      y_numeric = as.numeric(y) - 1,
      y_levels = levels(y)
    ))
  }
  list(
    type = type,
    y = y,
    y_numeric = as.numeric(y),
    y_levels = NULL
  )
} # /rtemis::meta_outcome


# %% meta_subset ----
#' One library entry's view of the training data
#'
#' Rows for a resample, columns for a feature group, and the outcome last, which
#' is the layout every rtemis learner expects.
#'
#' @param x data.frame: Training set, outcome last.
#' @param rows Integer vector: Rows to keep.
#' @param columns Character: Features to keep.
#'
#' @return data.frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_subset <- function(x, rows, columns) {
  outcome_nm <- names(x)[NCOL(x)]
  x[rows, c(columns, outcome_nm), drop = FALSE]
} # /rtemis::meta_subset


# %% meta_features ----
#' One library entry's view of the features, with no outcome
#'
#' `columns` is always explicit, so this reads the same whether `x` carries an
#' outcome column (training) or not (prediction).
#'
#' @param x data.frame: Data holding at least `columns`.
#' @param rows Integer vector: Rows to keep.
#' @param columns Character: Features to keep.
#'
#' @return data.frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_features <- function(x, rows, columns) {
  x[rows, columns, drop = FALSE]
} # /rtemis::meta_features


# %% meta_fit ----
#' Fit one library entry
#'
#' A nested `train()` rather than a direct `train_()`: the entry may still need
#' tuning (a GLMNET resolving `lambda` by internal cross-validation), and
#' `train()` is what handles that, along with the entry's own internal
#' preprocessing.
#'
#' @param learner `Hyperparameters` object: The entry.
#' @param dat data.frame: Training data, outcome last.
#' @param weights Optional Numeric: Case weights for these rows.
#' @param execution_config `ExecutionConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Supervised` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_fit <- function(learner, dat, weights, execution_config, verbosity) {
  train(
    x = dat,
    weights = weights,
    hyperparameters = learner,
    outer_resampling_config = NULL,
    execution_config = execution_config,
    verbosity = verbosity
  )
} # /rtemis::meta_fit


# %% meta_predict ----
#' One model's predictions as a single number per case
#'
#' Regression gives the fitted value; binary classification gives the probability
#' of the second outcome level, which is the one number the level-one data, the
#' Brier score and the oracle's loss are all defined on.
#'
#' @param model `Supervised` object.
#' @param newdata data.frame: Features, in the model's own column order.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_predict <- function(model, newdata, verbosity = 0L) {
  predicted <- predict(model, newdata, verbosity = verbosity)
  if (model@type == "Classification") {
    positive_prob(predicted)
  } else {
    as.numeric(predicted)
  }
} # /rtemis::meta_predict


# %% meta_holdout_rows ----
#' The rows each resample holds out, checked to be a partition
#'
#' `validate_hyperparameters()` restricts the resampler to the partitioning kinds
#' before training starts. This re-checks the resamples themselves, because that
#' check reads the config while this reads what `resample()` actually produced:
#' a level-one matrix with an unfilled cell is a silent NA in whatever is fitted
#' on it.
#'
#' @param resampler `Resampler` object.
#' @param n_cases Integer: Number of cases.
#'
#' @return List of integer vectors, one per resample.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_holdout_rows <- function(resampler, n_cases) {
  all_rows <- seq_len(n_cases)
  holdout <- lapply(resampler@resamples, function(train_rows) {
    all_rows[-train_rows]
  })
  covered <- unlist(holdout, use.names = FALSE)
  if (!setequal(covered, all_rows) || anyDuplicated(covered) > 0L) {
    rtemis.core::abort(
      "The inner resampler must hold out each case exactly once; it held out ",
      length(covered),
      " of ",
      n_cases,
      " cases.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  holdout
} # /rtemis::meta_holdout_rows


# %% meta_cv_risk ----
#' Cross-validated risk of each library entry
#'
#' Mean squared error between an entry's cross-validated predictions and the
#' outcome. For binary classification the predictions are probabilities of the
#' second level and the outcome is 0/1, which makes this the Brier score.
#'
#' @param level_one Numeric matrix: cases x library entries.
#' @param y_numeric Numeric vector: Outcome on the same scale.
#' @param weights Optional Numeric: Case weights.
#'
#' @return Named numeric vector, one risk per entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
meta_cv_risk <- function(level_one, y_numeric, weights = NULL) {
  risk <- apply(level_one, 2L, function(predicted) {
    squared_error <- (predicted - y_numeric)^2
    if (is.null(weights)) {
      mean(squared_error)
    } else {
      stats::weighted.mean(squared_error, weights)
    }
  })
  stats::setNames(as.numeric(risk), colnames(level_one))
} # /rtemis::meta_cv_risk
