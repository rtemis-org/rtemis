# train_GAM.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_.GAMHyperparameters ----
#' Train a GAM model
#'
#' Train a GAM model using `GAM`.
#'
#' GAM does not work in the presence of missing values.
#'
#' @param hyperparameters `GAMHyperparameters` object: make using [setup_GAM].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used for GAM.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, GAMHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("mgcv")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    cli::cli_abort("Hyperparameters must be fixed - use train() instead.")
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = FALSE,
    verbosity = verbosity
  )

  type <- supervised_type(x)
  n_classes <- if (type == "Classification") {
    nlevels(x[, ncol(x)])
  } else {
    NA
  }

  # Formula ----
  # use s(x, k = k) for all numeric predictors
  index_numeric <- which(sapply(features(x), is.numeric))
  spline_features <- if (length(index_numeric) > 0) {
    paste0(
      "s(",
      colnames(x)[index_numeric],
      ", k = ",
      hyperparameters[["k"]],
      ")",
      collapse = " + "
    )
  } else {
    ""
  }
  index_factor <- which(sapply(features(x), is.factor))
  categorical_features <- if (length(index_factor) > 0) {
    paste0(
      colnames(x)[index_factor],
      collapse = " + "
    )
  } else {
    ""
  }
  formula <- as.formula(
    gsub(
      " \\+ $",
      "",
      paste(
        outcome_name(x),
        "~",
        gsub(
          "^ \\+ ",
          "",
          paste(spline_features, categorical_features, sep = " + ")
        )
      )
    )
  )

  # Train ----
  family <- if (type == "Regression") {
    gaussian()
  } else if (type == "Classification") {
    if (n_classes == 2) {
      binomial()
    } else {
      mgcv::multinom()
    }
  }

  model <- mgcv::gam(
    formula = formula,
    family = family,
    data = x,
    weights = weights
  )
  check_inherits(model, "gam")
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.GAMHyperparameters


# %% predict_super.class_gam ----
#' Predict from GAM model
#'
#' @param model GAM model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#'
#' @keywords internal
#' @noRd
method(predict_super, class_gam) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  out <- predict(object = model, newdata = newdata, type = "response")
  if (model[["family"]][["family"]] == "binomial") {
    # mgvc::predict.gam returns an array of 1 dimension that causes errors during type-checking.
    out <- as.numeric(out)
  }
  out
} # /rtemis::predict_super.gam


# %% varimp_super.class_gam ----
#' Get variable importance from GAM model
#'
#' Variable importance for GAM is estimated as the variance of each predictor's partial effect,
#' obtained via predict(model, type = "terms"). This measures each smooth term's contribution to
#' the variance of the fitted values. Values are normalized to sum to one, representing each
#' predictor's proportion of total predicted variance. This approach is computationally efficient
#' (no refitting required) and analogous to importance measures in tree-based methods. It assumes
#' approximate uncorrelatedness of partial effects, which penalized smooths tend to satisfy. For
#' models with high concurvity, consider hierarchical partitioning of R² (e.g. via the gam.hp
#' package) as an alternative.
#'
#' @param model mgcv gam model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_gam) <- function(
  model,
  type = c("partial_effect", "F-test")
) {
  peff <- predict(model, type = "terms")
  vi <- apply(peff, 2, var)
  npeff <- vi / sum(vi) # normalized importance
  VariableImportance(
    data.table(
      variable = names(npeff),
      Partial_Effect_Variance = unname(npeff)
    )
  )
} # /rtemis::varimp_super.gam


# %% se_super.class_gam ----
#' Get Standard Errors from GAM model
#'
#' @param model mgcv gam model.
#' @param newdata tabular data: Data to predict on.
#'
#' @keywords internal
#' @noRd
method(se_super, class_gam) <- function(model, newdata) {
  predict(model, newdata = newdata, se.fit = TRUE)[["se.fit"]]
} # /rtemis::se_super.gam
