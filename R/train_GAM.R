# train_GAM.R
# ::rtemis::
# 2025 EDG rtemis.org

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
#' @param dat_validation tabular data or NULL: Not used for GAM.
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
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("mgcv")

  # Checks ----
  check_is_S7(hyperparameters, GAMHyperparameters)

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
  type = NULL
) {
  out <- predict(object = model, newdata = newdata, type = "response")
  if (model[["family"]][["family"]] == "binomial") {
    # mgvc::predict.gam returns an array of 1 dimension that causes errors during type-checking.
    out <- as.numeric(out)
  }
  out
} # /rtemis::predict_super.gam


# %% varimp_super.class_gam ----
#' Get coefficients from GAM model
#'
#' @param model mgcv gam model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_gam) <- function(
  model,
  type = c("p-value", "edf", "coefficients")
) {
  type <- match.arg(type)
  if (type == "p-value") {
    # Get parametric and smooth term p-values
    summary_ <- summary(model)
    # Exclude intercept
    -log10(c(
      summary_[["s.table"]][, "p-value"],
      summary_[["p.table"]][, ncol(summary_[["p.table"]])][-1]
    ))
  } else if (type == "edf") {
    summary(model)[["s.table"]][, "edf"]
  } else if (type == "coefficients") {
    coef(model)
  }
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
