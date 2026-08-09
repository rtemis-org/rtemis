# train_MARS.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Friedman (1991) <doi:10.1214/aos/1176347963>
# - https://cran.r-project.org/package=earth

# %% mars_design_matrix ----
#' Design matrix passed to earth
#'
#' `earth` expands factors itself, but only through `model.matrix()` with the
#' contrasts it finds on the search path. Reached as `earth::earth()` the
#' package is loaded and not attached, so a multiclass outcome dies looking up
#' `contr.earth.response`, and a feature level absent from a prediction set
#' produces a design matrix narrower than the fitted model expects. Encoding
#' here avoids both: the encoder is returned so `train_` can hand it back as
#' the algorithm-internal preprocessor and have it re-applied at predict time.
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
mars_design_matrix <- function(x, verbosity = 0L) {
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
  list(xm = mars_matrix(x), preprocessor = prp)
} # /rtemis::mars_design_matrix


# %% mars_matrix ----
#' Coerce already-numeric features to the matrix earth takes
#'
#' `earth` only skips its `model.matrix()` path when the input is a double
#' matrix, so an integer column would otherwise route back through the
#' contrasts machinery this wrapper exists to avoid.
#'
#' @param x tabular data: Numeric features.
#'
#' @return Numeric matrix.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mars_matrix <- function(x) {
  xm <- as.matrix(x)
  storage.mode(xm) <- "double"
  xm
} # /rtemis::mars_matrix


# %% mars_response ----
#' Response passed to earth
#'
#' Built here rather than left to `earth` so that no factor reaches the
#' backend's contrasts lookup. Regression passes the outcome through; binary
#' classification codes the second level as 1, which is the level rtemis
#' predicts probabilities for; multiclass becomes a 0/1 indicator matrix, one
#' column per level, which is what `contr.earth.response` would have produced.
#'
#' @param y Outcome vector.
#' @param type Character: "Regression" or "Classification".
#'
#' @return Numeric vector (regression, binary classification) or numeric matrix
#' with one column per class (multiclass classification).
#'
#' @author EDG
#' @keywords internal
#' @noRd
mars_response <- function(y, type) {
  if (type == "Regression") {
    return(as.double(y))
  }
  if (nlevels(y) == 2L) {
    return(as.double(as.integer(y) - 1L))
  }
  ym <- matrix(
    0,
    nrow = length(y),
    ncol = nlevels(y),
    dimnames = list(NULL, levels(y))
  )
  ym[cbind(seq_along(y), as.integer(y))] <- 1
  ym
} # /rtemis::mars_response


# %% validate_hyperparameters.MARSHyperparameters ----
#' Validate MARS hyperparameters against the training data
#'
#' `earth` prunes a multi-column response with `"backward"` or `"none"` only,
#' and the outcome's class count is not something the `data_bound` vocabulary
#' can gate an enum on. Everything else is declarative and handled by
#' `check_data_bounds()`.
#'
#' @param hyperparameters `MARSHyperparameters`: Hyperparameters to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(validate_hyperparameters, MARSHyperparameters) <- function(
  hyperparameters,
  x
) {
  check_data_bounds(hyperparameters, x)
  pmethod <- domain_values(hyperparameters[["pmethod"]])
  if (
    supervised_type(x) == "Classification" &&
      nlevels(outcome(x)) > 2L &&
      !pmethod %in% c("backward", "none")
  ) {
    rtemis.core::abort(
      "Multiclass classification fits one response column per class, and earth prunes a multi-column response with \"backward\" or \"none\" only. Got `pmethod = \"",
      pmethod,
      "\"` with ",
      nlevels(outcome(x)),
      " classes.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.MARSHyperparameters


# %% train_.MARSHyperparameters ----
#' Train a MARS model
#'
#' Train Multivariate Adaptive Regression Splines using `earth`.
#'
#' MARS does not work in the presence of missing values, and factors are
#' one-hot encoded before they reach the backend.
#'
#' Classification fits a binomial GLM on the MARS basis so that `predict()`
#' returns probabilities; without it `earth` regresses on the coded outcome and
#' returns values outside \[0, 1\].
#'
#' @param hyperparameters `MARSHyperparameters` object: make using [setup_MARS].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used for MARS.
#' @param execution_config `ExecutionConfig` object: Not used for MARS.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with elements `model` (object of class `earth`) and
#' `preprocessor`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, MARSHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("earth")

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
  design <- mars_design_matrix(features(x), verbosity = verbosity)
  y <- mars_response(outcome(x), type)

  # Train ----
  # `glm` is read only on the classification path: passing it for regression
  # would refit the basis coefficients with a family the outcome does not have.
  args <- list(
    x = design[["xm"]],
    y = y,
    weights = weights,
    glm = if (type == "Classification") list(family = "binomial") else NULL,
    degree = hyperparameters[["degree"]],
    penalty = hyperparameters[["penalty"]],
    nk = hyperparameters[["nk"]],
    nprune = hyperparameters[["nprune"]],
    thresh = hyperparameters[["thresh"]],
    minspan = hyperparameters[["minspan"]],
    endspan = hyperparameters[["endspan"]],
    newvar.penalty = hyperparameters[["newvar_penalty"]],
    fast.k = hyperparameters[["fast_k"]],
    fast.beta = hyperparameters[["fast_beta"]],
    pmethod = hyperparameters[["pmethod"]],
    nfold = hyperparameters[["nfold"]],
    ncross = hyperparameters[["ncross"]],
    stratify = hyperparameters[["stratify"]],
    trace = if (verbosity > 1L) 1 else 0
  )
  # `penalty` and `nk` default to expressions over `degree` and the design
  # matrix width, evaluated inside earth. Passing NULL would override those
  # defaults rather than request them, so an unset argument is dropped.
  args <- args[!vapply(args, is.null, logical(1L))]
  model <- do.call(earth::earth, args)
  check_inherits(model, "earth")
  list(model = model, preprocessor = design[["preprocessor"]])
} # /rtemis::train_.MARSHyperparameters


# %% predict_super.class_earth ----
#' Predict from MARS model
#'
#' Classification models carry one binomial GLM per response column, so a
#' binary fit already returns the probability of the second outcome level. The
#' per-class GLMs of a multiclass fit are independent and their probabilities
#' do not sum to 1, so they are normalized here.
#'
#' @param model `earth` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#' @param verbosity Integer: If > 0, print messages.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_earth) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  newx <- mars_matrix(newdata)
  if (type == "Classification") {
    predicted_prob <- predict(model, newdata = newx, type = "response")
    if (NCOL(predicted_prob) == 1L) {
      # Binary: the response was coded with the second level as 1.
      return(predicted_prob[, 1L])
    }
    row_sums <- rowSums(predicted_prob)
    # A row whose per-class GLMs all underflowed to zero carries no ranking to
    # preserve; making it uniform keeps NaN out of the downstream metrics.
    degenerate <- row_sums == 0
    if (any(degenerate)) {
      predicted_prob[degenerate, ] <- 1
      row_sums[degenerate] <- NCOL(predicted_prob)
    }
    return(predicted_prob / row_sums)
  }
  predict(model, newdata = newx, type = "response")[, 1L]
} # /rtemis::predict_super.class_earth


# %% varimp_super.class_earth ----
#' Get variable importance from MARS model
#'
#' `earth::evimp()` reports the three criteria described in the "Three
#' Criteria" chapter of the earth vignette, all of them accumulated over the
#' subsets the pruning pass evaluated:
#'
#' - `importance`: the GCV criterion, the drop in generalized cross-validation
#'   error attributable to the feature, scaled by earth so the top feature is
#'   100. The headline measure: GCV is what MARS itself optimizes, and it
#'   charges each feature for the model complexity it adds.
#' - `rss`: the same accumulation over residual sum of squares, so unpenalized.
#'   It ranks a feature that buys its fit with many terms higher than
#'   `importance` does, and the two disagreeing is the signal worth reading.
#' - `subset_proportion`: the fraction of pruning subsets that retain the
#'   feature, in \[0, 1\]. A consistency measure rather than a magnitude one.
#'   Reported as a proportion because earth's own count scales with the number
#'   of terms in the model, which makes the raw value incomparable across a
#'   grid search over `nk` or `nprune`.
#'
#' There is one row per design-matrix column, so a one-hot encoded factor
#' contributes one row per level rather than one per feature. Features the
#' pruned model dropped are kept with importance zero.
#'
#' @param model `earth` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_earth) <- function(model) {
  # `trim = FALSE` keeps unused features, so the table covers every predictor.
  vi <- unclass(earth::evimp(model, trim = FALSE))
  # Subsets evaluated by the pruning pass: every retained term but the first.
  n_subsets <- length(model[["selected.terms"]]) - 1L
  subset_proportion <- if (n_subsets > 0L) {
    vi[, "nsubsets"] / n_subsets
  } else {
    # An intercept-only model evaluated no subsets, so no feature can be in one.
    rep(0, nrow(vi))
  }
  VariableImportance(
    data.table(
      # `col` indexes the design matrix, so it recovers the name without
      # parsing evimp's "-unused" row-name suffix.
      variable = colnames(model[["dirs"]])[vi[, "col"]],
      importance = unname(vi[, "gcv"]),
      rss = unname(vi[, "rss"]),
      subset_proportion = unname(subset_proportion)
    )
  )
} # /rtemis::varimp_super.class_earth
