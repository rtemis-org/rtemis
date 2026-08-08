# train_ConditionalSuperLearner.R
# ::rtemis::
# 2026- EDG rtemis.org

# Reference: Valdes, Interian, Gennatas & Van der Laan, "Conditional Super
# Learner", IEEE Transactions on Pattern Analysis and Machine Intelligence,
# 2022. https://doi.org/10.1109/TPAMI.2021.3131976 (arXiv:1912.06675)
#
# The model is `sum_k 1{o(x) = k} F_k(x)`: an oracle `o` routes each case to one
# of K experts. Oracle and experts are fitted by alternating (paper Algorithm 1).

# %% csl_case_loss ----
#' Per-case loss of one prediction vector
#'
#' @param y_numeric Numeric vector: Outcome; 0/1 on the second level for
#'   classification.
#' @param predicted Numeric vector: Predictions on the same scale.
#' @param loss Character \{"squared_error", "log_loss"\}: Which loss.
#'
#' @return Numeric vector, one loss per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
csl_case_loss <- function(y_numeric, predicted, loss) {
  if (loss == "squared_error") {
    return((y_numeric - predicted)^2)
  }
  # Log loss is unbounded at the endpoints, and a single confident mistake would
  # otherwise dominate every extended weight in the problem.
  eps <- .Machine[["double.eps"]]
  bounded <- pmin(pmax(predicted, eps), 1 - eps)
  -(y_numeric * log(bounded) + (1 - y_numeric) * log1p(-bounded))
} # /rtemis::csl_case_loss


# %% csl_extended_weights ----
#' Weights of the extended dataset the oracle is fitted on
#'
#' Paper Definition 1 and Lemma 2.1: minimizing the total loss over the oracle is
#' equivalent to minimizing weighted misclassification on a dataset holding each
#' case once per expert, with weights `[ONE_K - DIAG_K]^-1 l_i`. That inverse is
#' `J/(K-1) - I`, so expert `k`'s weight on case `i` is
#' `sum_j l_ij / (K-1) - l_ik`: high where the *other* experts do badly.
#'
#' **Negative weights are clamped to zero.** For K >= 3 the transform goes
#' negative wherever one expert is worse than the average of the rest, and a
#' negative weight would ask the oracle to *prefer* misclassifying as that
#' expert, which Equation 7 does not intend -- most backends reject it outright.
#' The clamp is local and order-preserving, and it cannot silence the expert that
#' matters: `l_min <= sum(l)/K <= sum(l)/(K-1)`, so the best expert for a case
#' always keeps a non-negative weight, and a case's row can only go all-zero when
#' every expert is exactly right about it.
#'
#' @param loss Numeric matrix: cases x experts, per-case loss of each expert.
#'
#' @return Numeric matrix of the same shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
csl_extended_weights <- function(loss) {
  n_experts <- NCOL(loss)
  if (n_experts < 2L) {
    rtemis.core::abort(
      "The extended weights are undefined for fewer than 2 experts.",
      class = c("rtemis_dim_error", "rtemis_input_error")
    )
  }
  weights <- rowSums(loss) / (n_experts - 1L) - loss
  weights[weights < 0] <- 0
  weights
} # /rtemis::csl_extended_weights


# %% csl_extended_data ----
#' The extended dataset: each case once per expert, labelled by expert
#'
#' @param feat data.frame: Features.
#' @param experts Character: Expert names, in the column order of the weights.
#' @param label Character: Name for the outcome column.
#'
#' @return data.frame with `feat` stacked K times and the expert label last.
#'
#' @author EDG
#' @keywords internal
#' @noRd
csl_extended_data <- function(feat, experts, label) {
  out <- feat[rep(seq_len(NROW(feat)), times = length(experts)), , drop = FALSE]
  rownames(out) <- NULL
  # `each = n` so the blocks line up with `as.vector()` of the cases x experts
  # weight matrix, which runs down its columns.
  out[[label]] <- factor(
    rep(experts, each = NROW(feat)),
    levels = experts
  )
  out
} # /rtemis::csl_extended_data


# %% csl_oracle_assign ----
#' Which expert the oracle routes each case to
#'
#' The oracle is a `Supervised` classification model, so `predict()` returns
#' probabilities; the routing needs the label.
#'
#' @param oracle `Supervised` object: The fitted oracle.
#' @param newdata data.frame: Features.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character vector of expert names, one per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
csl_oracle_assign <- function(oracle, newdata, verbosity = 0L) {
  probabilities <- predict(oracle, newdata, verbosity = verbosity)
  as.character(prob2categorical(
    probabilities,
    levels = levels(oracle@y_training),
    binclasspos = oracle@binclasspos
  ))
} # /rtemis::csl_oracle_assign


# %% csl_initial_regions ----
#' The rows each expert starts from, within one set of training rows
#'
#' "full" gives every expert the whole training fold, so the first oracle sees
#' cross-validated losses of fully-trained experts. "random" partitions the fold,
#' which is the k-means-style initialization the paper's framing suggests and
#' gives the alternation a different local optimum to find.
#'
#' @param train_rows Integer vector: Rows available.
#' @param experts Character: Expert names.
#' @param init Character \{"full", "random"\}: Which initialization.
#'
#' @return Named list of integer vectors, one per expert.
#'
#' @author EDG
#' @keywords internal
#' @noRd
csl_initial_regions <- function(train_rows, experts, init) {
  if (init == "full") {
    return(stats::setNames(
      rep(list(train_rows), length(experts)),
      experts
    ))
  }
  assigned <- sample(experts, length(train_rows), replace = TRUE)
  out <- lapply(experts, function(expert) train_rows[assigned == expert])
  names(out) <- experts
  out
} # /rtemis::csl_initial_regions


# %% train_.ConditionalSuperLearnerHyperparameters ----
#' Train a Conditional SuperLearner
#'
#' Implements Algorithm 1 of Valdes et al. (2022): initialize the experts on each
#' cross-validation fold, then alternate between fitting the oracle on the
#' extended dataset and refitting each expert on the region the oracle assigned
#' it, finishing with a refit of every expert over the whole training sample.
#'
#' @param hyperparameters `ConditionalSuperLearnerHyperparameters` object: make
#' using [setup_ConditionalSuperLearner].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Not used.
#' @param execution_config `ExecutionConfig` object: Passed to every nested fit.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `ConditionalSuperLearner` model and a NULL
#' preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, ConditionalSuperLearnerHyperparameters) <- function(
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
  # Indexing below is by row number and column name throughout, which a
  # data.table's `[` reads differently.
  x <- as.data.frame(x)
  outcome_info <- meta_outcome(x, hyperparameters@algorithm)
  type <- outcome_info[["type"]]
  y_numeric <- outcome_info[["y_numeric"]]
  n_cases <- NROW(x)
  feature_names <- names(x)[-NCOL(x)]

  loss <- hyperparameters[["loss"]] %||% "squared_error"
  if (loss == "log_loss" && type == "Regression") {
    rtemis.core::abort(
      "`loss = \"log_loss\"` needs probabilities, so it applies to classification only.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Experts ----
  expanded <- expand_library(hyperparameters)
  learners <- expanded[["learners"]]
  experts <- names(learners)
  n_experts <- length(experts)
  entry_features <- meta_entry_features(
    hyperparameters,
    expanded[["origin"]],
    feature_names
  )
  n_iterations <- hyperparameters[["n_iterations"]]
  min_region_size <- hyperparameters[["min_region_size"]]
  # The oracle's outcome column joins the features, so it must not collide with
  # one of them.
  oracle_label <- utils::tail(make.unique(c(feature_names, "expert")), 1L)
  msg0(
    "Conditioning over ",
    highlight(n_experts),
    ngettext(n_experts, " expert", " experts"),
    ": ",
    paste(experts, collapse = ", "),
    ".",
    verbosity = verbosity
  )

  # Cross-validation folds ----
  resampler <- resample(
    x,
    config = hyperparameters@inner_resampling_config,
    verbosity = verbosity - 1L
  )
  holdout <- meta_holdout_rows(resampler, n_cases)
  n_folds <- length(holdout)

  # Fit every expert on the rows assigned to it within each fold, then predict
  # that fold's held-out cases. This is both the initialization and the body of
  # the loop's refit step, so the cross-validated predictions are produced the
  # same way at every iteration.
  cv_predict <- function(regions, previous) {
    predictions <- previous
    kept <- previous
    fold_models <- vector("list", n_folds)
    for (v in seq_len(n_folds)) {
      test_rows <- holdout[[v]]
      models <- vector("list", n_experts)
      names(models) <- experts
      for (expert in experts) {
        rows <- regions[[v]][[expert]]
        if (length(rows) < min_region_size) {
          # Too small to fit: the expert keeps whatever it last predicted here.
          next
        }
        fold_model <- meta_fit(
          learners[[expert]],
          meta_subset(x, rows, entry_features[[expert]]),
          weights = if (!is.null(weights)) weights[rows],
          execution_config = execution_config,
          verbosity = verbosity - 2L
        )
        models[[expert]] <- fold_model
        predictions[test_rows, expert] <- meta_predict(
          fold_model,
          meta_features(x, test_rows, entry_features[[expert]]),
          verbosity = verbosity - 2L
        )
      }
      fold_models[[v]] <- models
    }
    list(predictions = predictions, models = fold_models)
  }

  # Initialize ----
  init_regions <- lapply(seq_len(n_folds), function(v) {
    csl_initial_regions(
      resampler[[v]],
      experts,
      hyperparameters[["init"]]
    )
  })
  cv_predicted <- matrix(
    NA_real_,
    nrow = n_cases,
    ncol = n_experts,
    dimnames = list(NULL, experts)
  )
  cv_predicted <- cv_predict(init_regions, cv_predicted)[["predictions"]]
  if (anyNA(cv_predicted)) {
    rtemis.core::abort(
      "Initialization left some experts with no cross-validated prediction; ",
      "either `min_region_size` is larger than a fold, or `init = \"random\"` ",
      "gave an expert too few cases.",
      class = c("rtemis_runtime_error", "rtemis_error")
    )
  }

  # Alternate ----
  oracle <- NULL
  assignments <- NULL
  iteration_loss <- numeric(n_iterations)
  region_sizes <- matrix(
    NA_integer_,
    nrow = n_iterations,
    ncol = n_experts,
    dimnames = list(NULL, experts)
  )
  for (iteration in seq_len(n_iterations)) {
    iteration_node <- node_enter(
      "csl_iteration",
      label = paste0(iteration, "/", n_iterations),
      meta = list(iteration = iteration)
    )
    case_loss <- vapply(
      experts,
      function(expert) {
        csl_case_loss(y_numeric, cv_predicted[, expert], loss)
      },
      numeric(n_cases)
    )
    extended_weights <- as.vector(csl_extended_weights(case_loss))
    if (!is.null(weights)) {
      # A case that counts twice counts twice for the oracle too.
      extended_weights <- extended_weights * rep(weights, times = n_experts)
    }
    if (sum(extended_weights) <= 0) {
      rtemis.core::abort(
        "Every expert has the same loss on every case, so the oracle has ",
        "nothing to separate. Check that the experts differ.",
        class = c("rtemis_runtime_error", "rtemis_error")
      )
    }
    oracle <- meta_fit(
      hyperparameters@meta_learner,
      csl_extended_data(
        x[, feature_names, drop = FALSE],
        experts,
        oracle_label
      ),
      weights = extended_weights,
      execution_config = execution_config,
      verbosity = verbosity - 2L
    )
    assignments <- csl_oracle_assign(
      oracle,
      x[, feature_names, drop = FALSE],
      verbosity = verbosity - 2L
    )
    # The paper's objective, read off the cross-validated losses the oracle was
    # just fitted on, so a run can be checked against its monotone-decrease claim.
    iteration_loss[[iteration]] <- mean(
      case_loss[cbind(seq_len(n_cases), match(assignments, experts))]
    )
    region_sizes[iteration, ] <- vapply(
      experts,
      function(expert) sum(assignments == expert),
      integer(1L)
    )
    msg0(
      "Iteration ",
      highlight(paste0(iteration, "/", n_iterations)),
      ": loss ",
      highlight(format(iteration_loss[[iteration]], digits = 4L)),
      "; regions ",
      paste(experts, region_sizes[iteration, ], sep = ": ", collapse = ", "),
      ".",
      verbosity = verbosity
    )
    node_exit(iteration_node, status = "ok")
    if (iteration == n_iterations) {
      break
    }
    # Refit each expert on its region, within each fold's training rows.
    refit_regions <- lapply(seq_len(n_folds), function(v) {
      train_rows <- resampler[[v]]
      out <- lapply(experts, function(expert) {
        train_rows[assignments[train_rows] == expert]
      })
      names(out) <- experts
      out
    })
    cv_predicted <- cv_predict(refit_regions, cv_predicted)[["predictions"]]
  }

  # Final refit ----
  # Each expert on the region the final oracle gave it, over the whole training
  # sample. An expert whose region is too small still needs a usable fit -- the
  # oracle may route new data to it -- so it falls back to the full sample.
  final_regions <- lapply(experts, function(expert) {
    rows <- which(assignments == expert)
    if (length(rows) < min_region_size) seq_len(n_cases) else rows
  })
  names(final_regions) <- experts
  starved <- experts[vapply(
    experts,
    function(expert) sum(assignments == expert) < min_region_size,
    logical(1L)
  )]
  if (length(starved) == n_experts) {
    rtemis.core::abort(
      "Every expert's region held fewer than ",
      min_region_size,
      " cases, so the oracle partitioned nothing. Lower `min_region_size` or ",
      "use fewer experts.",
      class = c("rtemis_runtime_error", "rtemis_error")
    )
  }
  if (length(starved) > 0L) {
    rtemis.core::warn(
      "Region too small to fit, so fitted on the whole training set instead: ",
      paste(starved, collapse = ", "),
      "."
    )
  }
  expert_models <- lapply(experts, function(expert) {
    meta_fit(
      learners[[expert]],
      meta_subset(x, final_regions[[expert]], entry_features[[expert]]),
      weights = if (!is.null(weights)) weights[final_regions[[expert]]],
      execution_config = execution_config,
      verbosity = verbosity - 2L
    )
  })
  names(expert_models) <- experts

  # ConditionalSuperLearner ----
  model <- ConditionalSuperLearner(
    experts = expert_models,
    oracle = oracle,
    resampler = resampler,
    assignments = factor(assignments, levels = experts),
    cv_loss = vapply(
      experts,
      function(expert) {
        csl_case_loss(y_numeric, cv_predicted[, expert], loss)
      },
      numeric(n_cases)
    ),
    iteration_loss = iteration_loss,
    region_sizes = region_sizes,
    entry_features = entry_features,
    loss = loss,
    y_levels = outcome_info[["y_levels"]],
    xnames = feature_names,
    type = type
  )
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.ConditionalSuperLearnerHyperparameters


# %% predict_super.ConditionalSuperLearner ----
#' Predict from a Conditional SuperLearner
#'
#' The oracle routes each case to one expert and that expert predicts it, which
#' is what makes the model conditional: no case sees a combination.
#'
#' @param model `ConditionalSuperLearner` object.
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
method(predict_super, ConditionalSuperLearner) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(newdata, "data.frame")
  newdata <- as.data.frame(newdata)
  assignments <- csl_oracle_assign(
    model@oracle,
    newdata[, model@xnames, drop = FALSE],
    verbosity = verbosity - 1L
  )
  predicted <- numeric(NROW(newdata))
  for (expert in unique(assignments)) {
    rows <- which(assignments == expert)
    predicted[rows] <- meta_predict(
      model@experts[[expert]],
      meta_features(newdata, rows, model@entry_features[[expert]]),
      verbosity = verbosity - 1L
    )
  }
  predicted
} # /rtemis::predict_super.ConditionalSuperLearner


# %% varimp_super.ConditionalSuperLearner ----
#' Get the oracle's variable importance from a Conditional SuperLearner
#'
#' The oracle's importance answers the question the model is for: which
#' covariates decide *which* model applies. Unlike a stacked ensemble's weights
#' this is a genuine feature importance, over the same features the experts see.
#'
#' NULL when the chosen oracle reports no importance of its own.
#'
#' @param model `ConditionalSuperLearner` object.
#'
#' @return `VariableImportance` object, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(varimp_super, ConditionalSuperLearner) <- function(model) {
  model@oracle@varimp
} # /rtemis::varimp_super.ConditionalSuperLearner
