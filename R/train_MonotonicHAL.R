# train_MonotonicHAL.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# - Benkeser & van der Laan (2016) <doi:10.1109/DSAA.2016.93>
# - https://cran.r-project.org/package=hal9001

# %% Constants ----
# The interaction degree is fixed: the algorithm is additive by definition, and
# the calibration case it exists for has a single feature.
MONOTONIC_HAL_MAX_DEGREE <- 1L

# Fewest cases glmnet wants in a cross-validation fold before it stops grouping
# them, and the fewest folds worth running.
MONOTONIC_HAL_MIN_FOLD_SIZE <- 3L
MONOTONIC_HAL_MIN_FOLDS <- 3L


# %% monotonic_hal_nfolds ----
#' Folds the internal cross-validation can actually support
#'
#' The calibration sets this algorithm is built for are often small -- a single
#' resample's test partition -- and `nfolds` cases split ten ways leaves folds
#' glmnet refuses to group, which it reports once per fit. Sizing the fold
#' count to the data keeps the requested value whenever it fits and backs off
#' only when it does not.
#'
#' @param nfolds Integer: Requested number of folds.
#' @param n_cases Integer: Number of training cases.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
monotonic_hal_nfolds <- function(nfolds, n_cases) {
  supported <- n_cases %/% MONOTONIC_HAL_MIN_FOLD_SIZE
  max(MONOTONIC_HAL_MIN_FOLDS, min(nfolds, supported))
} # /rtemis::monotonic_hal_nfolds


# %% monotonic_hal_formula ----
#' HAL formula imposing a monotonic non-decreasing fit
#'
#' `hal9001` takes shape constraints through its formula interface rather than
#' through an argument of `fit_hal`. `h(., monotone = "i")` expands to one term
#' per feature and constrains those terms' coefficients to be non-negative;
#' since every basis function `hal9001` generates is itself non-decreasing in
#' its feature -- indicators at `smoothness_orders = 0`, hinges at 1 -- a
#' non-negative combination of them is non-decreasing, and so is the fitted
#' probability, the link being increasing.
#'
#' The formula is built as a character string because that is the form
#' `fit_hal` documents and parses internally. Building a `formula` object here
#' instead would require `h()` to be resolvable from the calling frame, which
#' it is not: `hal9001` is a suggested package reached by `::`, so it is loaded
#' but never attached.
#'
#' `pf` is glmnet's penalty factor for the generated terms. `pf = 0` removes
#' the lasso penalty, leaving the non-parametric maximum likelihood estimate
#' over the monotonic class.
#'
#' @param penalized Logical: If FALSE, set `pf = 0` to drop the lasso penalty.
#'
#' @return Character string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
monotonic_hal_formula <- function(penalized = TRUE) {
  if (penalized) {
    "~ h(., monotone = 'i')"
  } else {
    "~ h(., monotone = 'i', pf = 0)"
  }
} # /rtemis::monotonic_hal_formula


# %% train_.MonotonicHALHyperparameters ----
#' Train a monotonic Highly Adaptive Lasso model
#'
#' Train a shape-constrained HAL model using `hal9001::fit_hal` for regression
#' (gaussian family) and binary classification (binomial family). The fit is
#' additive and monotonic non-decreasing in every feature.
#'
#' HAL does not work in the presence of missing values, and takes a numeric
#' matrix, so factors are one-hot encoded first. `hal9001` has no multinomial
#' family, so multiclass classification is not supported.
#'
#' The basis-size guardrail `train_HAL` applies is not needed here: at
#' interaction degree 1 the basis grows linearly in the number of features
#' rather than combinatorially.
#'
#' @param hyperparameters `MonotonicHALHyperparameters` object: make using
#' [setup_MonotonicHAL].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights, passed to the lasso.
#' @param dat_validation Optional tabular data: Not used for MonotonicHAL.
#' @param execution_config `ExecutionConfig` object: Not used for MonotonicHAL.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return Object of class `hal9001`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, MonotonicHALHyperparameters) <- function(
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
  # probability rtemis expects back from `predict_super()`. It also fixes the
  # direction of the monotonic constraint: the fitted probability increases
  # with the feature, which for calibration is the required direction.
  if (type == "Classification") {
    y <- outcome(x)
    if (nlevels(y) > 2L) {
      rtemis.core::abort(
        "MonotonicHAL does not support multiclass classification.",
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
      "MonotonicHAL needs an all-numeric design matrix. Convert non-numeric features to factors so they can be one-hot encoded, or drop them.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  # Lambda selection ----
  nfolds <- monotonic_hal_nfolds(
    hyperparameters[["nfolds"]],
    n_cases = NROW(xm)
  )
  if (nfolds != hyperparameters[["nfolds"]] && verbosity > 0L) {
    info(
      "MonotonicHAL: selecting lambda over ",
      nfolds,
      " folds rather than ",
      hyperparameters[["nfolds"]],
      "; ",
      NROW(xm),
      " cases do not support more."
    )
  }

  # Train ----
  if (verbosity > 0L) {
    info(
      "MonotonicHAL: additive monotonic non-decreasing fit over ",
      NCOL(xm),
      " design-matrix column",
      if (NCOL(xm) == 1L) "" else "s",
      " at smoothness order ",
      hyperparameters[["smoothness_orders"]],
      "."
    )
  }
  model <- hal_fit(
    x = xm,
    y = y_train,
    family = family,
    max_degree = MONOTONIC_HAL_MAX_DEGREE,
    smoothness_orders = hyperparameters[["smoothness_orders"]],
    num_knots = hyperparameters[["num_knots"]],
    reduce_basis = hyperparameters[["reduce_basis"]],
    formula = monotonic_hal_formula(hyperparameters[["penalized"]]),
    weights = weights,
    cv_select = hyperparameters[["cv_select"]],
    use_min = hyperparameters[["use_min"]],
    nfolds = nfolds,
    seed = hyperparameters[["seed"]]
  )
  # `predict_super` and `varimp_super` dispatch on `class_hal9001`, which
  # `train_HAL.R` registers; the fitted class is the same.
  list(model = model, preprocessor = design[["preprocessor"]])
} # /rtemis::train_.MonotonicHALHyperparameters
