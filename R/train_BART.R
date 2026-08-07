# train_BART.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Chipman, George & McCulloch (2010) <doi:10.1214/09-AOAS285>
# - https://stochtree.ai/R_docs/pkgdown/reference/bart.html

# %% bart_is_binary ----
#' Was a BART model sampled with a binary outcome model?
#'
#' The sampled outcome model is the only thing that distinguishes a
#' classification fit from a regression one, and it is the only thing
#' `se_super()` receives.
#'
#' @param model `bartmodel` model.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
bart_is_binary <- function(model) {
  identical(
    model[["model_params"]][["outcome_model"]][["outcome"]],
    "binary"
  )
} # /rtemis::bart_is_binary


# %% bart_posterior ----
#' Posterior draws of the mean function from a BART model
#'
#' One column per retained MCMC sample. Binary fits are read on the
#' probability scale, so the draws are directly comparable across outcome
#' types.
#'
#' @param model `bartmodel` model.
#' @param newdata tabular data: Data to predict on.
#'
#' @return Numeric matrix with one row per case and one column per retained
#' sample.
#'
#' @author EDG
#' @keywords internal
#' @noRd
bart_posterior <- function(model, newdata) {
  predict(
    model,
    X = as.data.frame(newdata),
    type = "posterior",
    terms = "y_hat",
    scale = if (bart_is_binary(model)) "probability" else "linear"
  )
} # /rtemis::bart_posterior


# %% train_.BARTHyperparameters ----
#' Train a Bayesian Additive Regression Trees model
#'
#' Train a BART model using `stochtree::bart` for both regression and binary
#' classification.
#'
#' BART does not work in the presence of missing values. Factors are expanded
#' by the backend, so no encoding is needed beforehand.
#'
#' @param hyperparameters `BARTHyperparameters` object: make using [setup_BART].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights. Scale the residual variance;
#' not supported under the "cloglog" link.
#' @param dat_validation Optional tabular data: Not used for BART.
#' @param execution_config `ExecutionConfig` object: Not used for BART, which
#' threads internally using `n_workers`.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Named list with `model` (object of class `bartmodel`) and
#' `preprocessor`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, BARTHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("stochtree")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )
  type <- supervised_type(x)

  # Outcome ----
  # `link` names the binary outcome model's link, so it reaches the sampler
  # only for classification; a continuous outcome is always identity-linked.
  # A probit or cloglog link needs the outcome coded 0/1, and coding the second
  # factor level as 1 makes the backend's P(y == 1) the second-level
  # probability rtemis expects back from `predict_super()`.
  is_cloglog <- FALSE
  if (type == "Classification") {
    y <- outcome(x)
    if (nlevels(y) > 2L) {
      rtemis.core::abort(
        "BART does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    link <- hyperparameters[["link"]]
    is_cloglog <- identical(link, "cloglog")
    y_train <- as.integer(y) - 1L
    outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = link)
  } else {
    y_train <- outcome(x)
    outcome_model <- stochtree::OutcomeModel(
      outcome = "continuous",
      link = "identity"
    )
  }

  # Checks ----
  if (!is.null(weights) && is_cloglog) {
    rtemis.core::abort(
      "Case weights are not supported by stochtree under the \"cloglog\" link. Set `link = \"probit\"`, or set `ifw = FALSE` in the hyperparameters and do not pass `weights`.",
      class = "rtemis_unsupported_error"
    )
  }

  # Train ----
  variance_forest_num_trees <- hyperparameters[["variance_forest_num_trees"]]
  # The sampler holds the global error variance fixed under a non-identity
  # link and under a heteroskedastic model, and the leaf scale fixed under
  # cloglog. Declaring that up front keeps it from warning about ignoring a
  # request rtemis never intended to make.
  sample_sigma2_global <- type == "Regression" &&
    variance_forest_num_trees == 0L
  max_depth <- hyperparameters[["max_depth"]]
  model <- stochtree::bart(
    X_train = as.data.frame(features(x)),
    y_train = y_train,
    observation_weights = weights,
    num_gfr = hyperparameters[["num_gfr"]],
    num_burnin = hyperparameters[["num_burnin"]],
    num_mcmc = hyperparameters[["num_mcmc"]],
    general_params = list(
      cutpoint_grid_size = hyperparameters[["cutpoint_grid_size"]],
      standardize = hyperparameters[["standardize"]],
      sample_sigma2_global = sample_sigma2_global,
      random_seed = hyperparameters[["seed"]],
      keep_every = hyperparameters[["keep_every"]],
      num_chains = hyperparameters[["num_chains"]],
      num_threads = prop(hyperparameters, "n_workers"),
      outcome_model = outcome_model,
      verbose = verbosity > 1L
    ),
    mean_forest_params = list(
      num_trees = hyperparameters[["num_trees"]],
      alpha = hyperparameters[["alpha"]],
      beta = hyperparameters[["beta"]],
      min_samples_leaf = hyperparameters[["min_samples_leaf"]],
      # The backend spells "no depth limit" as -1.
      max_depth = if (is.null(max_depth)) -1L else max_depth,
      sample_sigma2_leaf = if (is_cloglog) FALSE else NULL,
      num_features_subsample = hyperparameters[["num_features_subsample"]]
    ),
    variance_forest_params = list(num_trees = variance_forest_num_trees)
  )
  check_inherits(model, "bartmodel")
  # `train_set_metadata` maps each column of the backend's internal design
  # matrix back to a feature by position but not by name, and `varimp_super()`
  # receives the model alone, so the names are stashed here.
  model[["rtemis_xnames"]] <- names(features(x))
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.BARTHyperparameters


# %% predict_super.class_bartmodel ----
#' Predict from BART model
#'
#' The posterior mean of the retained MCMC samples, on the probability scale
#' for classification.
#'
#' @param model `bartmodel` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_bartmodel) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  # `scale = "probability"` returns P(y == 1), which training coded as the
  # second factor level -- the probability rtemis expects for binary outcomes.
  predict(
    model,
    X = as.data.frame(newdata),
    type = "mean",
    terms = "y_hat",
    scale = if (identical(type, "Classification")) "probability" else "linear"
  )
} # /rtemis::predict_super.class_bartmodel


# %% se_super.class_bartmodel ----
#' Get standard errors from BART model
#'
#' The posterior standard deviation of the mean function at each case: the
#' spread of the retained MCMC draws that `predict_super()` averages.
#'
#' A sampler configured to retain a single draw has no spread to report.
#' `sd()` of one value is `NA`, which would present a degenerate configuration
#' as a failed computation, so the zero spread is returned directly.
#'
#' @param model `bartmodel` model.
#' @param newdata tabular data: Data to compute standard errors for.
#'
#' @keywords internal
#' @noRd
method(se_super, class_bartmodel) <- function(model, newdata) {
  posterior <- bart_posterior(model, newdata)
  if (NCOL(posterior) < 2L) {
    return(rep(0, NROW(posterior)))
  }
  apply(posterior, 1L, sd)
} # /rtemis::se_super.class_bartmodel


# %% varimp_super.class_bartmodel ----
#' Get variable importance from BART model
#'
#' Two measures, both derived from the one quantity `stochtree` records:
#' how many splitting rules use each feature. The backend exposes it per tree,
#' per posterior draw, and aggregated; the per-draw form is read here because
#' it supports both measures.
#'
#' - `importance`: the **variable inclusion proportion**, the standard BART
#'   importance measure. Within each retained draw, the share of that draw's
#'   splitting rules that use the feature; reported as the mean across draws.
#'   A proportion rather than the raw count `stochtree` returns, because a
#'   count scales with `num_mcmc` and `keep_every`, so two runs differing only
#'   in sampler budget would report importances differing by that factor.
#' - `inclusion_sd`: the standard deviation of that proportion across draws.
#'   BART's importance is a posterior quantity, so it has a spread as well as
#'   a center: a feature the sampler uses consistently is separable from one
#'   whose apparent importance rests on a handful of draws, and the two are
#'   indistinguishable from the mean alone.
#'
#' The backend counts splits per column of its internal design matrix, so a
#' factor -- which occupies several columns -- is summed back onto the feature
#' those columns came from before proportions are taken.
#'
#' @param model `bartmodel` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_bartmodel) <- function(model) {
  var_indices <- model[["train_set_metadata"]][["original_var_indices"]]
  xnames <- model[["rtemis_xnames"]]
  # The forest container is an R6 object, so its methods are bindings in an
  # environment: reaching one with `[[` gets the same function `$` would.
  mean_forests <- model[["mean_forests"]]
  forest_split_counts <- mean_forests[["get_forest_split_counts"]]
  num_samples <- mean_forests[["num_samples"]]
  n_features <- length(var_indices)
  # One column per retained draw. `forest_num` is 0-indexed.
  per_draw <- vapply(
    seq_len(num_samples()) - 1L,
    function(i) forest_split_counts(forest_num = i, num_features = n_features),
    numeric(n_features)
  )
  # Design columns -> features. Every feature contributes at least one design
  # column, so the grouping covers `seq_along(xnames)`; sort to be sure the
  # rows come back in feature order rather than in `rowsum()`'s label order.
  by_feature <- rowsum(per_draw, group = var_indices)
  by_feature <- by_feature[
    order(as.integer(rownames(by_feature))),
    ,
    drop = FALSE
  ]
  draw_totals <- colSums(by_feature)
  # A draw whose forest is all root splits on nothing and has no proportions
  # to contribute; dropping it keeps the others' mean well defined.
  by_feature <- by_feature[, draw_totals > 0, drop = FALSE]
  draw_totals <- draw_totals[draw_totals > 0]
  if (length(draw_totals) == 0L) {
    return(VariableImportance(
      data.table(
        variable = xnames,
        importance = rep(0, length(xnames)),
        inclusion_sd = rep(0, length(xnames))
      )
    ))
  }
  proportions <- sweep(by_feature, 2L, draw_totals, "/")
  VariableImportance(
    data.table(
      variable = xnames,
      importance = unname(rowMeans(proportions)),
      # A single draw has a mean but no spread.
      inclusion_sd = if (NCOL(proportions) > 1L) {
        unname(apply(proportions, 1L, sd))
      } else {
        rep(0, length(xnames))
      }
    )
  )
} # /rtemis::varimp_super.class_bartmodel
