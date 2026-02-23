# train_LightRuleFit.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% train_super.LightRuleFitHyperparameters ----
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
method(train_super, LightRuleFitHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm", "glmnet", "matrixStats", "gsubfn")

  # Checks ----
  check_is_S7(hyperparameters, LightRuleFitHyperparameters)

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
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
  # LightRuleFit_tunable includes the names of all LightGBM hyperparameters used by LightRuleFit.
  lgbm_parameters <- update(
    setup_LightGBM(),
    get_hyperparams(hyperparameters, LightRuleFit_lightgbm_params)
  )
  lgbm_parameters@hyperparameters[["ifw"]] <- hyperparameters[["ifw_lightgbm"]]
  mod_lgbm <- train(
    x = x,
    dat_validation = dat_validation,
    weights = lightgbm_weights,
    hyperparameters = lgbm_parameters,
    # tuner_config = tuner_config, # ? add tuner_config to LightRuleFitHyperparameters
    outer_resampling_config = NULL,
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
    verbosity = verbosity
  )

  # Rule coefficients ----
  rules_coefs <- data.matrix(coef(mod_glmnet@model))
  # Need special handling for multiclass support starting here
  intercept_coef <- rules_coefs[1, , drop = FALSE]
  colnames(intercept_coef) <- "Coefficient"
  rules_coefs <- data.frame(Rule = lgbm_rules, Coefficient = rules_coefs[-1, 1])
  nonzero_index <- which(abs(rules_coefs[["Coefficient"]]) > 0)
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
} # /rtemis::train_super.LightRuleFitHyperparameters


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
  verbosity = 1L
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
  coef(model@model_glmnet@model)
} # /rtemis::varimp_super.LightRuleFit
