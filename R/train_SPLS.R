# train_SPLS.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Chun & Keles (2010) <doi:10.1111/j.1467-9868.2009.00723.x>
# - https://cran.r-project.org/package=spls

# %% spls_design_matrix ----
#' Design matrix passed to spls
#'
#' `spls` takes a numeric matrix, so factors are one-hot encoded. The encoder
#' is returned alongside the matrix so `train_` can hand it back as the
#' algorithm-internal preprocessor and have it re-applied at predict time.
#'
#' @param x tabular data: Features only, outcome already removed.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with elements `xm` (numeric matrix) and `preprocessor`
#' (`Preprocessor` or NULL when there was nothing to encode).
#'
#' @author EDG
#' @keywords internal
#' @noRd
spls_design_matrix <- function(x, verbosity = 0L) {
  if (any(vapply(x, is.factor, logical(1L)))) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(one_hot = TRUE),
      verbosity = verbosity
    )
    x <- preprocessed(prp)
  } else {
    prp <- NULL
  }
  list(xm = as.matrix(x), preprocessor = prp)
} # /rtemis::spls_design_matrix


# %% spls_scores ----
#' Project new data onto a fitted splsda model's latent components
#'
#' `predict.splsda` only returns probabilities for the binary logistic case; it
#' returns hard labels for `lda` and for multiclass. Reproducing the projection
#' here gives the inner classifier the same scores it was fit on, so its own
#' `predict` method can be asked for probabilities in every case.
#'
#' Mirrors the transform `splsda` applies when fitting: subset to the selected
#' features, center and scale with the training statistics, then apply the
#' projection matrix. `normx` is all ones when the model was fit with
#' `scale_x = FALSE`, so the same expression covers both.
#'
#' @param model `splsda` model.
#' @param newx Numeric matrix: Data to project.
#'
#' @return data.frame of latent component scores, named as in the fitted model.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spls_scores <- function(model, newx) {
  selected <- model[["A"]]
  scores <- scale(
    newx[, selected, drop = FALSE],
    model[["meanx"]][selected],
    model[["normx"]][selected]
  ) %*%
    model[["W"]]
  scores <- as.data.frame(scores)
  colnames(scores) <- colnames(model[["T"]])
  scores
} # /rtemis::spls_scores


# %% train_.SPLSHyperparameters ----
#' Train a Sparse Partial Least Squares model
#'
#' Train a Sparse PLS model using `spls::spls` for regression and
#' `spls::splsda` for classification.
#'
#' SPLS does not work in the presence of missing values, and takes a numeric
#' matrix, so factors are one-hot encoded first.
#'
#' @param hyperparameters `SPLSHyperparameters` object: make using [setup_SPLS].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights. Not supported by `spls`.
#' @param dat_validation Optional tabular data: Not used for SPLS.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `spls` (Regression) or `splsda` (Classification).
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, SPLSHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("spls")

  # Checks ----
  if (!is.null(weights)) {
    rtemis.core::abort(
      "Case weights are not supported by spls. Set `ifw = FALSE` in the hyperparameters and do not pass `weights`.",
      class = "rtemis_unsupported_error"
    )
  }

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

  # Preprocess ----
  y <- outcome(x)
  design <- spls_design_matrix(features(x), verbosity = verbosity)

  # Train ----
  if (type == "Regression") {
    model <- spls::spls(
      x = design[["xm"]],
      y = y,
      K = hyperparameters[["k"]],
      eta = hyperparameters[["eta"]],
      kappa = hyperparameters[["kappa"]],
      select = hyperparameters[["select"]],
      fit = hyperparameters[["fit"]],
      scale.x = hyperparameters[["scale_x"]],
      scale.y = hyperparameters[["scale_y"]],
      eps = hyperparameters[["eps"]],
      maxstep = hyperparameters[["maxstep"]],
      trace = verbosity > 1L
    )
    check_inherits(model, "spls")
  } else {
    # splsda fixes `select`, `scale.y` and `trace` internally and forwards its
    # `...` to both spls() and the inner classifier, so nothing else is passed.
    model <- spls::splsda(
      x = design[["xm"]],
      y = y,
      K = hyperparameters[["k"]],
      eta = hyperparameters[["eta"]],
      kappa = hyperparameters[["kappa"]],
      classifier = hyperparameters[["classifier"]],
      scale.x = hyperparameters[["scale_x"]]
    )
    check_inherits(model, "splsda")
  }
  list(model = model, preprocessor = design[["preprocessor"]])
} # /rtemis::train_.SPLSHyperparameters


# %% predict_super.class_spls ----
#' Predict from SPLS model
#'
#' @param model `spls` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning. Always "Regression" here.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_spls) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  predict(model, newx = as.matrix(newdata), type = "fit")[, 1L]
} # /rtemis::predict_super.class_spls


# %% predict_super.class_splsda ----
#' Predict from SPLSDA model
#'
#' @param model `splsda` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning. Always "Classification" here.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_splsda) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  scores <- spls_scores(model, as.matrix(newdata))
  predicted_prob <- if (model[["classifier"]] == "lda") {
    predict(model[["class.fit"]], newdata = scores)[["posterior"]]
  } else if (model[["ngroups"]] > 2L) {
    predict(model[["class.fit"]], newdata = scores, type = "probs")
  } else {
    # splsda codes the second level as 1, so the binomial glm already returns
    # the probability of the second level.
    as.numeric(
      predict(model[["class.fit"]], newdata = scores, type = "response")
    )
  }
  if (NCOL(predicted_prob) == 2L) {
    # lda returns one column per class; rtemis expects the second level only.
    predicted_prob <- predicted_prob[, 2L]
  }
  predicted_prob
} # /rtemis::predict_super.class_splsda


# %% varimp_super.class_spls ----
#' Get coefficients from SPLS model
#'
#' Features not selected by the sparse fit have coefficient zero.
#'
#' @param model `spls` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_spls) <- function(model) {
  coefs <- coef(model)
  VariableImportance(
    data.table(
      variable = rownames(coefs),
      Coefficient = unname(coefs[, 1L])
    )
  )
} # /rtemis::varimp_super.class_spls


# %% varimp_super.class_splsda ----
#' Get coefficients from SPLSDA model
#'
#' The sparse PLS coefficients on the coded outcome, which is a single column
#' for binary classification and one per class for multiclass. The latter is
#' not yet supported as `VariableImportance`.
#'
#' @param model `splsda` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_splsda) <- function(model) {
  coefs <- model[["spls.fit"]][["betahat"]]
  if (NCOL(coefs) > 1L) {
    return(NULL)
  }
  VariableImportance(
    data.table(
      variable = rownames(coefs),
      Coefficient = unname(coefs[, 1L])
    )
  )
} # /rtemis::varimp_super.class_splsda
