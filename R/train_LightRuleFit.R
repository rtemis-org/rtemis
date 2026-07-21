# train_LightRuleFit.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% .rule_coefs ----
#' Extract rule coefficients from the GLMNET step of LightRuleFit
#'
#' `coef()` on a `glmnet`/`cv.glmnet` model returns a sparse matrix, except
#' for multinomial (multiclass) models, where it returns a *list* of sparse
#' matrices, one per outcome class. This normalizes both to a dense numeric
#' matrix of rule coefficients (rules in rows, one column per coefficient
#' set), with the intercept row dropped.
#'
#' @param model `glmnet` or `cv.glmnet` model.
#'
#' @return Numeric matrix: rules x coefficient sets. Columns are named by
#' outcome class for multiclass models, "Coefficient" otherwise.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.rule_coefs <- function(model) {
  coefs <- stats::coef(model)
  if (is.list(coefs)) {
    # Multinomial: one sparse matrix per class, each with the intercept first.
    out <- do.call(
      cbind,
      lapply(coefs, function(m) as.matrix(m)[-1L, 1L])
    )
    colnames(out) <- names(coefs)
  } else {
    out <- as.matrix(coefs)[-1L, 1L, drop = FALSE]
    colnames(out) <- "Coefficient"
  }
  out
} # /rtemis::.rule_coefs


# %% .rule_importance ----
#' Reduce a rule's coefficient set to a single importance value
#'
#' Single-coefficient models (binary classification, regression) have one
#' signed coefficient per rule, which is returned as-is: the sign is
#' meaningful (direction of the rule's effect) and drives the descending
#' ranking of rules.
#'
#' Multinomial (multiclass) models have one coefficient per outcome class per
#' rule; there is no single meaningful sign, so importance is the total
#' absolute influence across classes (L1 norm of the coefficient row). This
#' is direction-agnostic and, unlike picking the largest single-class
#' coefficient, does not arbitrarily privilege one class. The per-class
#' coefficients themselves are preserved separately (see `train_` and
#' `varimp_super`), so no information is lost.
#'
#' @param coef_matrix Numeric matrix: rules x coefficient sets, from
#' `.rule_coefs`.
#'
#' @return Numeric vector, one value per rule.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.rule_importance <- function(coef_matrix) {
  if (NCOL(coef_matrix) == 1L) {
    coef_matrix[, 1L]
  } else {
    rowSums(abs(coef_matrix))
  }
} # /rtemis::.rule_importance


# %% train_.LightRuleFitHyperparameters ----
#' Train a LightRuleFit model
#'
#' Train a LightRuleFit model using LightGBM and GLMNET.
#'
#' @param hyperparameters `LightRuleFitHyperparameters` object: make using [setup_LightRuleFit].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation tabular data: Validation set.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LightRuleFitHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm", "glmnet", "matrixStats", "gsubfn")

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
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  nclasses <- if (type == "Classification") nlevels(x[[ncol(x)]]) else 1L

  # IFW for LightGBM ----
  # See setup_LightRuleFit: You can choose to use IFW for both steps with `ifw = TRUE` OR control each steps individually using `ifw_lightgbm` and `ifw_glmnet`.
  lightgbm_weights <- if (hyperparameters[["ifw_lightgbm"]]) {
    ifw(x[[ncol(x)]], verbosity = verbosity)
  } else {
    weights
  }

  # Train Gradient Boosting using LightGBM ----
  # LightRuleFit_lightgbm_params names the LightGBM hyperparameters forwarded
  # from LightRuleFit to the LightGBM step.
  lgbm_parameters <- update(
    setup_LightGBM(),
    get_hyperparams(hyperparameters, LightRuleFit_lightgbm_params)
  )
  lgbm_parameters@ifw <- hyperparameters[["ifw_lightgbm"]]
  mod_lgbm <- train(
    x = x,
    dat_validation = dat_validation,
    weights = lightgbm_weights,
    hyperparameters = lgbm_parameters,
    # tuner_config = tuner_config, # ? add tuner_config to LightRuleFitHyperparameters
    outer_resampling_config = NULL,
    execution_config = execution_config,
    verbosity = verbosity
  )

  # Extract Rules from Boosted Trees ----
  lgbm_rules <- extract_rules(
    mod_lgbm@model,
    n_iter = NULL,
    xnames = names(x),
    factor_levels = get_factor_levels(x)
  )

  # Match cases x rules ----
  cases_by_rules <- match_cases_by_rules(x, lgbm_rules, verbosity = verbosity)

  # IFW for LASSO ----
  glmnet_weights <- if (hyperparameters[["ifw_glmnet"]]) {
    ifw(x[[ncol(x)]], verbosity = verbosity)
  } else {
    weights
  }

  # LASSO: Select Rules ----
  lasso_hyperparameters <- setup_GLMNET(
    alpha = hyperparameters[["alpha"]],
    lambda = hyperparameters[["lambda"]]
  )
  dat_rules <- data.frame(cases_by_rules, y = x[[ncol(x)]])
  colnames(dat_rules)[ncol(dat_rules)] <- colnames(x)[ncol(x)]
  mod_glmnet <- train(
    dat_rules,
    hyperparameters = lasso_hyperparameters,
    weights = glmnet_weights,
    execution_config = execution_config,
    verbosity = verbosity
  )

  # Rule coefficients ----
  # Multiclass models carry one coefficient per class per rule: rules are
  # selected if nonzero for any class, and reduced to a single importance
  # value for ranking (see `.rule_importance`). The per-class coefficients
  # are kept verbatim as extra columns of `rules_coefs`.
  coef_matrix <- .rule_coefs(mod_glmnet@model)
  rules_coefs <- data.frame(
    Rule = lgbm_rules,
    Coefficient = .rule_importance(coef_matrix)
  )
  if (NCOL(coef_matrix) > 1L) {
    rules_coefs <- cbind(rules_coefs, as.data.frame(coef_matrix))
  }
  nonzero_index <- which(rowSums(abs(coef_matrix)) > 0)
  rules_selected <- lgbm_rules[nonzero_index]
  cases_by_rules_selected <- cases_by_rules[, nonzero_index]
  Ncases_by_rules <- matrixStats::colSums2(cases_by_rules_selected)

  # Empirical risk ----
  if (type == "Classification" && nclasses == 2) {
    x <- as.data.table(x)
    empirical_risk <- vector("numeric", length(rules_selected))
    for (i in seq_along(rules_selected)) {
      match <- x[eval(parse(text = rules_selected[i])), ]
      freq <- table(match[[ncol(match)]])
      empirical_risk[i] <- freq[mod_glmnet@binclasspos] / sum(freq)
    }
  } else {
    empirical_risk <- NULL
  }

  # Format Rules ----
  # => Check format_LightRuleFit_rules' use of gsubfn::gsubfn
  rules_selected_formatted <- format_LightRuleFit_rules(
    rules_selected,
    decimal_places = 2
  )
  # appease R CMD check
  Coefficient <- NULL
  rules_selected_formatted_coefs <- data.table(
    Rule_ID = seq(rules_selected_formatted),
    Rule = rules_selected_formatted,
    N_Cases = Ncases_by_rules,
    Coefficient = rules_coefs[["Coefficient"]][nonzero_index]
  )
  # For multiclass, `Coefficient` is the aggregate importance used for
  # ranking; break out the signed per-class coefficients so the listing is
  # complete. (setorder below reorders these columns in lockstep.)
  if (NCOL(coef_matrix) > 1L) {
    rules_selected_formatted_coefs <- cbind(
      rules_selected_formatted_coefs,
      as.data.table(coef_matrix[nonzero_index, , drop = FALSE])
    )
  }
  if (type == "Classification" && nclasses == 2) {
    # appease R CMD check
    Empirical_Risk <- NULL
    rules_selected_formatted_coefs[, Empirical_Risk := empirical_risk]
  }
  setorder(rules_selected_formatted_coefs, -Coefficient)

  # LightRuleFit ----
  model <- LightRuleFit(
    model_lightgbm = mod_lgbm,
    model_glmnet = mod_glmnet,
    rules = lgbm_rules,
    rules_coefs = rules_coefs,
    rules_index = nonzero_index,
    rules_selected = rules_selected,
    rules_selected_formatted = rules_selected_formatted,
    rules_selected_formatted_coefs = rules_selected_formatted_coefs,
    y_levels = levels(x[[ncol(x)]]),
    xnames = names(x)[-ncol(x)],
    complexity_metrics = data.frame(
      n_rules_total = length(lgbm_rules),
      n_nonzero_rules = length(nonzero_index)
    )
  )
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.LightRuleFitHyperparameters


# %% predict_super.LightRuleFitHyperparameters ----
#' Predict from LightRuleFit LightGBM model
#'
#' @param model LightRuleFit object trained using `train_LightRuleFit`.
#' @param newdata data.frame or similar: Data to predict on.
#'
#' @keywords internal
#' @noRd
method(predict_super, LightRuleFit) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  check_inherits(newdata, "data.frame")

  rules <- model@rules
  cases_by_rules <- match_cases_by_rules(newdata, rules, verbosity = verbosity)
  datm <- data.matrix(cases_by_rules)
  if (model@model_lightgbm@type == "Classification") {
    predicted <- predict(
      model@model_glmnet@model,
      newx = datm,
      type = "response"
    )
    if (length(model@y_levels) == 2) {
      predicted[, 1]
    } else {
      predicted
    }
  } else {
    as.numeric(predict(model@model_glmnet@model, newx = datm))
  }
} # /rtemis::predict_super.LightRuleFit


# %% varimp_super.LightRuleFit ----
#' Get variable importance from LightRuleFit model
#'
#' @param model LightRuleFit object trained using `train_LightRuleFit`.
#'
#' @keywords internal
#' @noRd
method(varimp_super, LightRuleFit) <- function(model) {
  # Column 2 (the default plotted measure) is the per-rule importance: the
  # signed coefficient for single-coefficient models, the total absolute
  # influence for multiclass (see `.rule_importance`). For multiclass, the
  # signed per-class coefficients are appended as extra named columns, so
  # `plot_varimp(measure = "<class>")` shows a single class.
  coef_matrix <- .rule_coefs(model@model_glmnet@model)
  vi <- data.table(
    variable = rownames(coef_matrix),
    Coefficient = .rule_importance(coef_matrix)
  )
  if (NCOL(coef_matrix) > 1L) {
    vi <- cbind(vi, as.data.table(coef_matrix))
  }
  VariableImportance(vi)
} # /rtemis::varimp_super.LightRuleFit
