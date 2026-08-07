# train_HAL.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Benkeser & van der Laan (2016) <doi:10.1109/DSAA.2016.93>
# - https://cran.r-project.org/package=hal9001

# %% Constants ----
# Projected basis size past which training reports a warning. The fit is still
# attempted; `max_basis` is the count that stops it.
HAL_BASIS_WARN <- 1e6


# %% hal_design_matrix ----
#' Design matrix passed to fit_hal
#'
#' `hal9001` takes a numeric matrix, so factors are one-hot encoded. The
#' encoder is returned alongside the matrix so `train_` can hand it back as the
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
hal_design_matrix <- function(x, verbosity = 0L) {
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
} # /rtemis::hal_design_matrix


# %% hal_num_knots ----
#' Knots per interaction degree the backend would generate
#'
#' Mirrors the knot generator `hal9001::fit_hal` applies when `num_knots` is
#' left unset, so the basis size can be projected before the basis is built.
#' The halving per degree and the two base counts are the backend's.
#'
#' @param max_degree Integer: Highest interaction degree.
#' @param smoothness_orders Integer: Smoothness of the basis functions.
#'
#' @return Numeric vector of length `max_degree`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hal_num_knots <- function(max_degree, smoothness_orders) {
  base <- if (all(smoothness_orders > 0L)) 50 else 200
  round(base / 2^(seq_len(max_degree) - 1L))
} # /rtemis::hal_num_knots


# %% hal_projected_basis ----
#' Upper bound on the number of basis functions a HAL fit would enumerate
#'
#' Each interaction degree contributes one basis function per distinct knot
#' combination of each feature subset of that size. A subset of `d` features
#' yields at most `n_cases` distinct combinations, and at most `knots^d` once
#' the features have been binned to `knots` values, so the bound per degree is
#' `C(n_features, d) * min(n_cases, knots^d)`.
#'
#' The bound is what the guardrail acts on, so it is deliberately an upper
#' bound: measured against the backend it runs 0-30% above the realized count.
#'
#' @param n_cases Integer: Number of training cases.
#' @param n_features Integer: Number of columns of the design matrix.
#' @param max_degree Integer: Highest interaction degree.
#' @param smoothness_orders Integer: Smoothness of the basis functions.
#' @param num_knots Optional integer vector: Knots per degree. NULL projects
#' the backend's generated knots.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hal_projected_basis <- function(
  n_cases,
  n_features,
  max_degree,
  smoothness_orders,
  num_knots = NULL
) {
  # The backend caps the degree at the width of the design matrix.
  max_degree <- min(n_features, max_degree)
  if (is.null(num_knots)) {
    num_knots <- hal_num_knots(max_degree, smoothness_orders)
  }
  sum(vapply(
    seq_len(max_degree),
    function(d) {
      # A degree past the end of the knot vector reuses its smallest entry,
      # as the backend does.
      knots <- if (length(num_knots) < d) min(num_knots) else num_knots[d]
      choose(n_features, d) * min(as.double(n_cases), as.double(knots)^d)
    },
    numeric(1L)
  ))
} # /rtemis::hal_projected_basis


# %% hal_check_basis_size ----
#' Report and bound the projected basis size of a HAL fit
#'
#' The basis is enumerated before any model is fit, and an over-large basis
#' does not fail -- it runs until it exhausts memory or patience. Projecting
#' the count first turns that into a message, a warning, or a corrective abort.
#'
#' @param n_cases Integer: Number of training cases.
#' @param n_features Integer: Number of columns of the design matrix.
#' @param hyperparameters `HALHyperparameters` object.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return The projected count, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hal_check_basis_size <- function(
  n_cases,
  n_features,
  hyperparameters,
  verbosity = 1L
) {
  max_degree <- hyperparameters[["max_degree"]]
  num_knots <- hyperparameters[["num_knots"]]
  projected <- hal_projected_basis(
    n_cases = n_cases,
    n_features = n_features,
    max_degree = max_degree,
    smoothness_orders = hyperparameters[["smoothness_orders"]],
    num_knots = num_knots
  )
  max_basis <- hyperparameters[["max_basis"]]
  if (projected > max_basis) {
    rtemis.core::abort(
      "HAL would enumerate up to ",
      format(projected, big.mark = ",", scientific = FALSE),
      " basis functions from ",
      n_cases,
      " cases and ",
      n_features,
      " design-matrix columns, above `max_basis` (",
      format(max_basis, big.mark = ",", scientific = FALSE),
      "). Reduce it by lowering `max_degree` (currently ",
      max_degree,
      "), by setting `num_knots` to fewer knots per degree",
      if (is.null(num_knots)) " (currently the backend's own)" else "",
      ", or by setting `reduce_basis` with `smoothness_orders = 0`. Raise `max_basis` to fit it anyway.",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  if (projected > HAL_BASIS_WARN) {
    warn(
      "HAL will enumerate up to ",
      format(projected, big.mark = ",", scientific = FALSE),
      " basis functions: expect a long fit. Lower `max_degree` or set `num_knots` to reduce it.",
      verbosity = verbosity
    )
  } else if (verbosity > 0L) {
    info(
      "HAL basis: up to ",
      format(projected, big.mark = ",", scientific = FALSE),
      " basis functions over ",
      n_features,
      " design-matrix columns at max_degree ",
      max_degree,
      "."
    )
  }
  invisible(projected)
} # /rtemis::hal_check_basis_size


# %% hal_fit ----
#' Fit a model with the hal9001 backend
#'
#' The single call site for `hal9001::fit_hal`, shared by `HAL` and
#' `MonotonicHAL`. Everything the two algorithms disagree on -- the interaction
#' degree, the family, and whether a shape constraint is imposed through
#' `formula` -- arrives as an argument, so neither carries its own copy of the
#' backend's argument handling.
#'
#' `fit_control` is forwarded to glmnet, which takes a fold assignment but no
#' seed, so the seed reaches the internal cross-validation as `foldid`. The
#' backend generates its own from the ambient RNG when none is supplied.
#'
#' @param x Numeric matrix: Design matrix.
#' @param y Numeric vector: Outcome, coded 0/1 for the binomial family.
#' @param family Character: glmnet family.
#' @param max_degree Integer: Highest interaction degree.
#' @param smoothness_orders Integer: Smoothness of the basis functions.
#' @param num_knots Optional integer vector: Knots per degree.
#' @param reduce_basis Optional numeric: Minimum non-zero proportion of a kept
#' basis function.
#' @param formula Optional character: HAL formula, used to impose shape
#' constraints. NULL fits the unconstrained basis.
#' @param weights Optional numeric vector: Case weights.
#' @param cv_select Logical: Select lambda by the backend's internal
#' cross-validation. FALSE returns the whole lambda path, which
#' `predict_super()` cannot use, so it is never passed FALSE.
#' @param use_min Logical: Select `lambda.min` rather than `lambda.1se`.
#' @param nfolds Integer: Folds of the internal cross-validation.
#' @param seed Optional integer: Seed for the fold assignment.
#'
#' @return Object of class `hal9001`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hal_fit <- function(
  x,
  y,
  family,
  max_degree,
  smoothness_orders,
  num_knots = NULL,
  reduce_basis = NULL,
  formula = NULL,
  weights = NULL,
  cv_select = TRUE,
  use_min = TRUE,
  nfolds = 10L,
  seed = NULL
) {
  fit_control <- list(
    cv_select = cv_select,
    use_min = use_min,
    nfolds = nfolds
  )
  # The seed is scoped to the fold draw: seeding is an implementation detail of
  # this fit, so it must not change the RNG the caller sees afterwards.
  if (!is.null(seed)) {
    fit_control[["foldid"]] <- with_seed(
      seed,
      sample(rep_len(seq_len(nfolds), NROW(x)))
    )
  }
  args <- list(
    X = x,
    Y = y,
    formula = formula,
    max_degree = max_degree,
    smoothness_orders = smoothness_orders,
    num_knots = num_knots,
    reduce_basis = reduce_basis,
    family = family,
    weights = weights,
    fit_control = fit_control
  )
  # `num_knots` is the one argument whose backend default is not NULL: unset,
  # it is filled by the backend's knot generator, which only runs when the
  # argument is absent. Passing NULL through would instead place a knot at
  # every observed value and enumerate the largest basis available.
  args <- args[!vapply(args, is.null, logical(1L))]
  model <- do.call(hal9001::fit_hal, args)
  check_inherits(model, "hal9001")
  model
} # /rtemis::hal_fit


# %% train_.HALHyperparameters ----
#' Train a Highly Adaptive Lasso model
#'
#' Train a HAL model using `hal9001::fit_hal` for regression (gaussian family)
#' and binary classification (binomial family).
#'
#' HAL does not work in the presence of missing values, and takes a numeric
#' matrix, so factors are one-hot encoded first. `hal9001` has no multinomial
#' family, so multiclass classification is not supported.
#'
#' @param hyperparameters `HALHyperparameters` object: make using [setup_HAL].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights, passed to the lasso.
#' @param dat_validation Optional tabular data: Not used for HAL.
#' @param execution_config `ExecutionConfig` object: Not used for HAL.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Named list with `model` (object of class `hal9001`) and
#' `preprocessor` (the one-hot encoder, re-applied at predict time).
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, HALHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("hal9001")

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
  # The binomial family needs the outcome coded 0/1, and coding the second
  # factor level as 1 makes the backend's P(y == 1) the second-level
  # probability rtemis expects back from `predict_super()`. A factor reaches
  # the backend's own prediction-bound calculation, which cannot take one.
  if (type == "Classification") {
    y <- outcome(x)
    if (nlevels(y) > 2L) {
      rtemis.core::abort(
        "HAL does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    y_train <- as.integer(y) - 1L
    family <- "binomial"
  } else {
    y_train <- outcome(x)
    family <- "gaussian"
  }

  # Design matrix ----
  design <- hal_design_matrix(features(x), verbosity = verbosity)
  xm <- design[["xm"]]
  if (!is.numeric(xm)) {
    rtemis.core::abort(
      "HAL needs an all-numeric design matrix. Convert non-numeric features to factors so they can be one-hot encoded, or drop them.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  # Basis size ----
  hal_check_basis_size(
    n_cases = NROW(xm),
    n_features = NCOL(xm),
    hyperparameters = hyperparameters,
    verbosity = verbosity
  )

  # Train ----
  model <- hal_fit(
    x = xm,
    y = y_train,
    family = family,
    max_degree = hyperparameters[["max_degree"]],
    smoothness_orders = hyperparameters[["smoothness_orders"]],
    num_knots = hyperparameters[["num_knots"]],
    reduce_basis = hyperparameters[["reduce_basis"]],
    weights = weights,
    cv_select = hyperparameters[["cv_select"]],
    use_min = hyperparameters[["use_min"]],
    nfolds = hyperparameters[["nfolds"]],
    seed = hyperparameters[["seed"]]
  )
  list(model = model, preprocessor = design[["preprocessor"]])
} # /rtemis::train_.HALHyperparameters


# %% predict_super.class_hal9001 ----
#' Predict from HAL model
#'
#' @param model `hal9001` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_hal9001) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  # A binomial fit returns P(y == 1), which training coded as the second
  # factor level -- the probability rtemis expects for binary outcomes. Both
  # families return a plain vector, because lambda is a single selected value.
  predict(model, new_data = as.matrix(newdata))
} # /rtemis::predict_super.class_hal9001


# %% varimp_super.class_hal9001 ----
#' Get variable importance from HAL model
#'
#' `hal9001` has no importance measure of its own. Both measures here are
#' derived from the lasso coefficients, aggregated over the basis functions
#' that involve each feature: `basis_list` records, per basis function, which
#' design-matrix columns it is built from, and the coefficients align with it
#' one-to-one after the intercept.
#'
#' - `importance`: the sum of the absolute values of the non-zero coefficients
#'   of those basis functions. A feature entering many selected terms and one
#'   entering a single large term both register.
#' - `max_coefficient`: the largest single absolute coefficient among them,
#'   separating a feature carried by one strong term from one carried by many
#'   weak ones.
#'
#' Both read coefficients on the scale of the basis functions. At
#' `smoothness_orders = 0` those are indicators, so the values are unit-free
#' and comparable across features; at higher orders each basis function carries
#' the units of its feature.
#'
#' @param model `hal9001` model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_hal9001) <- function(model) {
  xnames <- model[["X_colnames"]]
  basis_list <- model[["basis_list"]]
  # Row 1 is the intercept. Any unpenalized covariates would follow the basis
  # coefficients; rtemis never passes them, so the basis is the leading block.
  coefs <- as.numeric(model[["coefs"]])[-1L][seq_along(basis_list)]
  importance <- setNames(numeric(length(xnames)), xnames)
  max_coefficient <- importance
  for (i in which(coefs != 0)) {
    cols <- unique(basis_list[[i]][["cols"]])
    abs_coef <- abs(coefs[i])
    importance[cols] <- importance[cols] + abs_coef
    max_coefficient[cols] <- pmax(max_coefficient[cols], abs_coef)
  }
  VariableImportance(
    data.table(
      variable = xnames,
      importance = unname(importance),
      max_coefficient = unname(max_coefficient)
    )
  )
} # /rtemis::varimp_super.class_hal9001
