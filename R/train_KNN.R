# train_KNN.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Hechenbichler & Schliep (2004) <doi:10.5282/ubm/epub.1769>
# - https://cran.r-project.org/package=kknn

# %% knn_design_data ----
#' Training frame passed to kknn
#'
#' `kknn` codes factors with its own `contr.dummy`, which `model.matrix()`
#' resolves by name off the search path and therefore cannot find while the
#' package is merely loaded. One-hot encoding here produces the same full set of
#' indicator columns, and the encoder is returned alongside so `train_` can hand
#' it back as the algorithm-internal preprocessor and have it re-applied at
#' predict time.
#'
#' @param x tabular data: Features only, outcome already removed.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with elements `x` (data.frame of numeric features) and
#' `preprocessor` (`Preprocessor` or NULL when there was nothing to encode).
#'
#' @author EDG
#' @keywords internal
#' @noRd
knn_design_data <- function(x, verbosity = 0L) {
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
  list(x = as.data.frame(x), preprocessor = prp)
} # /rtemis::knn_design_data


# %% validate_hyperparameters.KNNHyperparameters ----
#' Validate KNN Hyperparameters
#'
#' `train.kknn` selects among its candidate `ks` by leave-one-out
#' cross-validation, so every fit sees one case fewer than the training set and
#' `k` must be strictly less than the number of cases. The `data_bound`
#' vocabulary only expresses `<=`, hence the explicit check; the call to
#' `check_data_bounds()` keeps the declarative checks running for any property
#' that later declares one.
#'
#' @param hyperparameters `KNNHyperparameters`: Hyperparameters to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(validate_hyperparameters, KNNHyperparameters) <- function(
  hyperparameters,
  x
) {
  check_data_bounds(hyperparameters, x)
  # At the first call site `k` may still hold the whole search space, so every
  # value the tuner could pick is checked, not just a scalar.
  k <- domain_values(hyperparameters[["k"]])
  n_cases <- NROW(x)
  if (any(k >= n_cases)) {
    rtemis.core::abort(
      "`k` must be less than the number of training cases (",
      n_cases,
      "); got ",
      paste(unique(k[k >= n_cases]), collapse = ", "),
      ".",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.KNNHyperparameters


# %% train_.KNNHyperparameters ----
#' Train a k-Nearest Neighbors model
#'
#' Train a KNN model using `kknn::train.kknn` for both regression and
#' classification.
#'
#' KNN is a lazy learner: the fitted object carries the training set and all
#' work happens at predict time. It does not work in the presence of missing
#' values, and factors are one-hot encoded first; see `knn_design_data()`.
#'
#' @param hyperparameters `KNNHyperparameters` object: make using [setup_KNN].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights. Not supported by `kknn`.
#' @param dat_validation Optional tabular data: Not used for KNN.
#' @param execution_config `ExecutionConfig` object: Not used for KNN.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Named list with `model` (object of class `train.kknn`) and
#' `preprocessor` (the one-hot encoder, re-applied at predict time).
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, KNNHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("kknn")

  # Checks ----
  # Both routes to a weighted fit are rejected. `ifw` is checked in its own
  # right rather than only through the `weights` it produces: a caller that
  # reaches `train_()` directly, or a future path that resolves weights
  # elsewhere, would otherwise fit an unweighted model while reporting
  # `ifw = TRUE`.
  if (isTRUE(hyperparameters[["ifw"]])) {
    rtemis.core::abort(
      "Inverse Frequency Weighting is not supported by kknn, which takes no case weights. Set `ifw = FALSE` in the hyperparameters.",
      class = "rtemis_unsupported_error"
    )
  }
  if (!is.null(weights)) {
    rtemis.core::abort(
      "Case weights are not supported by kknn. Set `ifw = FALSE` in the hyperparameters and do not pass `weights`.",
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

  # Preprocess ----
  outcome_name <- names(x)[NCOL(x)]
  design <- knn_design_data(features(x), verbosity = verbosity)
  dat <- design[["x"]]
  dat[[outcome_name]] <- outcome(x)

  # Train ----
  model <- kknn::train.kknn(
    formula = as.formula(make_formula(dat)),
    data = dat,
    ks = hyperparameters[["k"]],
    distance = hyperparameters[["distance"]],
    kernel = hyperparameters[["kernel"]],
    scale = hyperparameters[["scale"]]
  )
  check_inherits(model, "train.kknn")
  # `train.kknn` does not record `scale`, and `predict.train.kknn` re-fits with
  # its own default of TRUE regardless of what training used. `predict_super()`
  # therefore calls `kknn::kknn()` directly with the value stashed here, so that
  # prediction uses the same geometry as the fit.
  model[["rtemis_scale"]] <- hyperparameters[["scale"]]
  list(model = model, preprocessor = design[["preprocessor"]])
} # /rtemis::train_.KNNHyperparameters


# %% predict_super.class_train.kknn ----
#' Predict from KNN model
#'
#' Refits the neighbor search against the stored training set rather than
#' calling `predict.train.kknn()`, which would drop the `scale` setting.
#'
#' @param model `train.kknn` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_train.kknn) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  fitted <- kknn::kknn(
    formula = formula(model[["terms"]]),
    train = model[["data"]],
    test = as.data.frame(newdata),
    k = model[["best.parameters"]][["k"]],
    distance = model[["distance"]],
    kernel = model[["best.parameters"]][["kernel"]],
    scale = model[["rtemis_scale"]]
  )
  if (identical(type, "Classification")) {
    # Columns follow the outcome's factor levels; rtemis expects the second
    # level's probability in the binary case and the full matrix otherwise.
    predicted_prob <- fitted[["prob"]]
    if (NCOL(predicted_prob) == 2L) {
      predicted_prob <- predicted_prob[, 2L]
    }
    return(predicted_prob)
  }
  fitted[["fitted.values"]]
} # /rtemis::predict_super.class_train.kknn


# %% varimp_super.class_train.kknn ----
#' Get variable importance from KNN model
#'
#' `kknn` provides no measure of variable importance.
#'
#' @param model `train.kknn` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_train.kknn) <- function(model) {
  NULL
} # /rtemis::varimp_super.class_train.kknn
