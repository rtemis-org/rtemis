# train_StackedLearner.R
# ::rtemis::
# 2026- EDG rtemis.org

# Reference: van der Laan, Polley & Hubbard, "Super Learner", Statistical
# Applications in Genetics and Molecular Biology 6(1), 2007.

# %% level_one_matrix ----
#' Predictions of a fitted library, one column per entry
#'
#' The level-one data: what the meta learner is fitted on at train time and
#' applied to at predict time. Built by one function so the two cannot disagree
#' about column order, which would silently permute the ensemble weights.
#'
#' @param base_models Named list of `Supervised` objects.
#' @param entry_features Named list of character vectors: Features each entry
#'   sees, one per entry.
#' @param newdata data.frame: Features.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix: cases x entries, columns named by entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
level_one_matrix <- function(
  base_models,
  entry_features,
  newdata,
  verbosity = 0L
) {
  rows <- seq_len(NROW(newdata))
  columns <- lapply(names(base_models), function(entry) {
    meta_predict(
      base_models[[entry]],
      meta_features(newdata, rows, entry_features[[entry]]),
      verbosity = verbosity
    )
  })
  out <- do.call(cbind, columns)
  colnames(out) <- names(base_models)
  out
} # /rtemis::level_one_matrix


# %% stacked_weights ----
#' Ensemble weight of each library entry, where one is defined
#'
#' A weight exists only when the meta learner produces one: NNLS gives a
#' non-negative coefficient per entry, which (normalized) is the convex
#' combination the SuperLearner is defined by. A tree or forest meta learner
#' combines the entries without weighting them, and reporting its variable
#' importances as weights would be a different quantity under the same name --
#' so those read NA and `cv_risk` carries the ranking instead.
#'
#' @param meta_model `Supervised` object, or NULL for a discrete ensemble.
#' @param entries Character: Library entry names, in level-one column order.
#' @param discrete_winner Character or NULL: The kept entry, for a discrete
#'   ensemble.
#'
#' @return Named numeric vector, one value per entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
stacked_weights <- function(meta_model, entries, discrete_winner = NULL) {
  if (!is.null(discrete_winner)) {
    return(stats::setNames(as.numeric(entries == discrete_winner), entries))
  }
  if (!is.null(meta_model) && S7_inherits(meta_model@model, NNLS)) {
    coefficients <- meta_model@model@coefficients
    return(stats::setNames(unname(coefficients[entries]), entries))
  }
  stats::setNames(rep(NA_real_, length(entries)), entries)
} # /rtemis::stacked_weights


# %% train_.StackedLearnerHyperparameters ----
#' Train a stacked meta learner
#'
#' Serves every stacking meta learner -- SuperLearner and ModalityStacking --
#' which differ only in whether each base learner sees its own group of features.
#'
#' Each library entry is fitted on every training fold of
#' `inner_resampling_config` and predicts that fold's held-out cases, giving one
#' cross-validated prediction per case per entry. The meta learner is fitted on
#' that matrix against the outcome, every entry is refitted on the whole training
#' set, and a prediction is the meta learner applied to the refitted entries'
#' predictions.
#'
#' @param hyperparameters `StackedLearnerHyperparameters` object: make using
#' [setup_SuperLearner] or [setup_ModalityStacking].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Not used.
#' @param execution_config `ExecutionConfig` object: Passed to every nested fit.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `StackedLearner` model and a NULL preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, StackedLearnerHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(x = x, allow_missing = TRUE, verbosity = verbosity)
  outcome_info <- meta_outcome(x, hyperparameters@algorithm)
  n_cases <- NROW(x)

  # Library ----
  expanded <- expand_library(hyperparameters)
  learners <- expanded[["learners"]]
  entries <- names(learners)
  entry_features <- meta_entry_features(
    hyperparameters,
    expanded[["origin"]],
    names(x)[-NCOL(x)]
  )
  msg0(
    "Stacking ",
    highlight(length(entries)),
    ngettext(length(entries), " library entry", " library entries"),
    ": ",
    paste(entries, collapse = ", "),
    ".",
    verbosity = verbosity
  )

  # Cross-fit ----
  # Every entry predicts each case from a fit that never saw it, so the meta
  # learner is fitted on honest predictions rather than in-sample ones.
  resampler <- resample(
    x,
    config = hyperparameters@inner_resampling_config,
    verbosity = verbosity - 1L
  )
  holdout <- meta_holdout_rows(resampler, n_cases)
  n_folds <- length(holdout)
  level_one <- matrix(
    NA_real_,
    nrow = n_cases,
    ncol = length(entries),
    dimnames = list(NULL, entries)
  )
  crossfit_fold <- function(v) {
    fold_node <- node_enter(
      "crossfit_fold",
      label = paste0(v, "/", n_folds),
      meta = list(fold = v)
    )
    train_rows <- resampler[[v]]
    test_rows <- holdout[[v]]
    out <- vapply(
      entries,
      function(entry) {
        columns <- entry_features[[entry]]
        fold_model <- meta_fit(
          learners[[entry]],
          meta_subset(x, train_rows, columns),
          weights = if (!is.null(weights)) weights[train_rows],
          execution_config = execution_config,
          verbosity = verbosity - 2L
        )
        meta_predict(
          fold_model,
          meta_features(x, test_rows, columns),
          verbosity = verbosity - 2L
        )
      },
      numeric(length(test_rows))
    )
    node_exit(fold_node, status = "ok")
    # `vapply` over a single held-out case returns a bare vector.
    matrix(out, nrow = length(test_rows), dimnames = list(NULL, entries))
  }
  fold_predictions <- progress_lapply(
    seq_len(n_folds),
    crossfit_fold,
    label = "Cross-fitting library",
    kind = "crossfit",
    verbosity = verbosity
  )
  for (v in seq_len(n_folds)) {
    level_one[holdout[[v]], ] <- fold_predictions[[v]]
  }

  # Cross-validated risk ----
  cv_risk <- meta_cv_risk(level_one, outcome_info[["y_numeric"]], weights)

  # Meta learner ----
  # Fitted on the level-one data, which carries the entries' names as its
  # columns, so the meta model's own `xnames` are the library.
  discrete_winner <- NULL
  meta_model <- NULL
  if (hyperparameters[["discrete"]]) {
    discrete_winner <- entries[[which.min(cv_risk)]]
    msg0(
      "Discrete: keeping ",
      highlight(discrete_winner),
      ".",
      verbosity = verbosity
    )
  } else {
    dat_meta <- as.data.table(level_one)
    set(dat_meta, j = outcome_name(x), value = outcome_info[["y"]])
    meta_node <- node_enter(
      "meta_learner",
      label = hyperparameters@meta_learner@algorithm
    )
    meta_model <- meta_fit(
      hyperparameters@meta_learner,
      dat_meta,
      weights = weights,
      execution_config = execution_config,
      verbosity = verbosity - 1L
    )
    node_exit(meta_node, status = "ok")
  }

  # Refit the library ----
  base_models <- lapply(entries, function(entry) {
    meta_fit(
      learners[[entry]],
      meta_subset(x, seq_len(n_cases), entry_features[[entry]]),
      weights = weights,
      execution_config = execution_config,
      verbosity = verbosity - 2L
    )
  })
  names(base_models) <- entries

  # StackedLearner ----
  model <- StackedLearner(
    base_models = base_models,
    meta_model = meta_model,
    level_one_training = level_one,
    resampler = resampler,
    cv_risk = data.table(
      learner = entries,
      cv_risk = unname(cv_risk[entries]),
      weight = unname(stacked_weights(meta_model, entries, discrete_winner))
    ),
    discrete_winner = discrete_winner,
    entry_features = entry_features,
    y_levels = outcome_info[["y_levels"]],
    xnames = names(x)[-NCOL(x)],
    type = outcome_info[["type"]]
  )
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.StackedLearnerHyperparameters


# %% predict_super.StackedLearner ----
#' Predict from a StackedLearner model
#'
#' @param model `StackedLearner` object.
#' @param newdata data.frame or similar: Data to predict on.
#' @param type Not used; the model carries its own.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Numeric vector: fitted values, or probabilities of the second outcome
#' level for classification.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(predict_super, StackedLearner) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(newdata, "data.frame")
  level_one <- level_one_matrix(
    model@base_models,
    model@entry_features,
    newdata,
    verbosity = verbosity - 1L
  )
  if (!is.null(model@discrete_winner)) {
    return(as.numeric(level_one[, model@discrete_winner]))
  }
  meta_predict(
    model@meta_model,
    as.data.table(level_one),
    verbosity = verbosity - 1L
  )
} # /rtemis::predict_super.StackedLearner


# %% varimp_super.StackedLearner ----
#' Get library weights and risks from a StackedLearner model
#'
#' The `variable` column names **library entries**, not features: what a stacked
#' ensemble attributes its fit to is its base learners. `LightRuleFit` names
#' rules in the same column for the same reason.
#'
#' Two measures. `weight` is the ensemble weight and is the default plotted
#' measure; it is NA throughout when the meta learner defines no weights (see
#' `stacked_weights()`), and the column is then omitted so `cv_risk` takes its
#' place. `cv_risk` is the cross-validated mean squared error of each entry on
#' its own, for which **lower is better** -- the reverse of how a variable
#' importance bar usually reads.
#'
#' @param model `StackedLearner` object.
#'
#' @return `VariableImportance` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(varimp_super, StackedLearner) <- function(model) {
  vi <- data.table(
    variable = model@cv_risk[["learner"]],
    weight = model@cv_risk[["weight"]],
    cv_risk = model@cv_risk[["cv_risk"]]
  )
  if (all(is.na(vi[["weight"]]))) {
    vi[["weight"]] <- NULL
  }
  VariableImportance(vi)
} # /rtemis::varimp_super.StackedLearner
