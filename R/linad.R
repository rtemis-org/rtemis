# linad.R
# ::rtemis::
# 2026- EDG rtemis.org

# The LINAD engine: a decision tree whose leaves carry linear models, grown by
# stagewise gradient descent so that a leaf's coefficients are the accumulated
# sum of every model on its root-to-leaf path.
#
# Reference: Gennatas, Friedman, Eaton, Simone, Ungar, Xing, Valdes,
# "The Linear Additive Tree". Equation and algorithm numbers below cite that
# manuscript.
#
# Everything here is plain numeric code over matrices, deliberately: the three
# primitives `linad_gram()`, `linad_solve()` and `linad_stump()` are the only
# places that touch the data in bulk, so a compiled kernel can replace them
# without the growth loop above them changing.

# %% LINAD_NUGGET ----
# Relative ridge added to every Gram diagonal before factorization. A leaf with
# a constant column is ordinary rather than exceptional, so the solve has to
# survive rank deficiency; at 1e-9 of the mean diagonal this moves a
# well-conditioned answer by far less than the convergence tolerance of the
# iterative solvers it replaces.
LINAD_NUGGET <- 1e-9


# %% LINAD_WEIGHT_TOLERANCE ----
# A case whose soft weight has decayed below this fraction of a node's largest
# weight is dropped from that node's leaf-model fit. Under `gamma`, weight
# decays as `gamma^depth`, so this is what stops a deep node from carrying the
# whole dataset for the sake of contributions no double can represent. At
# `gamma = 0` it excludes exactly the node's non-members, which is what makes
# the hard partition a special case rather than a separate code path.
LINAD_WEIGHT_TOLERANCE <- 1e-8


# %% LINAD_BASELINE_MAX ----
# Largest margin `linad_baseline()` will return for a single-class outcome, where
# the minimizing constant is unbounded.
LINAD_BASELINE_MAX <- 10


# %% linad_gram ----
#' Weighted Gram matrix and cross-product
#'
#' `G = X'WX` and `Xty = X'Wy` over the rows in `idx`, the sufficient statistics
#' for every weighted least-squares fit LINAD performs.
#'
#' Subsetting rows rather than zero-weighting them is what keeps a deep node
#' cheap: a leaf holding 30 of 1000 cases costs 30 rows, not 1000.
#'
#' @param xm Numeric matrix: Intercept-augmented design matrix.
#' @param y Numeric vector: Target, length `nrow(xm)`.
#' @param w Numeric vector: Case weights, length `nrow(xm)`.
#' @param idx Optional Integer vector: Rows to include. NULL uses every row.
#'
#' @return List with `G`, `Xty`, `sw` (sum of weights) and `syy` (weighted sum
#' of squared `y`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_gram <- function(xm, y, w, idx = NULL) {
  if (!is.null(idx)) {
    xm <- xm[idx, , drop = FALSE]
    y <- y[idx]
    w <- w[idx]
  }
  xw <- xm * w
  list(
    G = crossprod(xw, xm),
    Xty = drop(crossprod(xw, y)),
    sw = sum(w),
    syy = sum(w * y * y)
  )
} # /rtemis::linad_gram


# %% linad_chol_solve ----
#' Solve a penalized normal-equation system by Cholesky
#'
#' The intercept, when there is one, is column 1 and is never penalized.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param Xty Numeric vector: `X'Wy`.
#' @param penalty Numeric scalar: Ridge penalty already scaled to the weight
#' total, so it is comparable across nodes of different size.
#' @param intercept Logical: If TRUE, column 1 is an intercept and is left
#' unpenalized. FALSE for a centered, intercept-free design, where every column
#' is a slope and all of them are penalized.
#'
#' @return Numeric vector of coefficients, or NULL if the system is singular.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_chol_solve <- function(G, Xty, penalty, intercept = TRUE) {
  diag(G) <- diag(G) + linad_ridge_diagonal(G, penalty, intercept)
  chol_factor <- tryCatch(chol(G), error = function(e) NULL)
  if (is.null(chol_factor)) {
    return(NULL)
  }
  drop(backsolve(chol_factor, backsolve(chol_factor, Xty, transpose = TRUE)))
} # /rtemis::linad_chol_solve


# %% linad_ridge_diagonal ----
#' The diagonal a penalized solve adds to a Gram
#'
#' A nugget proportional to the trace keeps a rank-deficient node solvable (a
#' constant column inside a leaf is ordinary, not an error) without perceptibly
#' moving a well-conditioned solve. The intercept is never penalized: it carries
#' the level.
#'
#' Both the solve and the degrees-of-freedom count read the penalty from here,
#' so the two cannot describe different models.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param penalty Numeric: Ridge penalty.
#' @param intercept Logical: Whether column 1 is the intercept.
#'
#' @return Numeric vector of length `ncol(G)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_ridge_diagonal <- function(G, penalty, intercept = TRUE) {
  d <- ncol(G)
  # `penalty` is a scalar for a whole-design solve and one value per column when
  # forward selection penalizes an active set, so the length is set explicitly.
  ridge <- rep_len(penalty, d)
  if (intercept) {
    ridge[[1L]] <- 0
  }
  ridge + LINAD_NUGGET * max(mean(diag(G)), .Machine[["double.eps"]])
} # /rtemis::linad_ridge_diagonal


# %% linad_ridge_edf ----
#' Effective degrees of freedom of a penalized solve
#'
#' `tr((G + D)^-1 G)`, written as `d - sum_j D_jj [(G + D)^-1]_jj` so one
#' Cholesky answers it. Shrinkage means a ridge fit spends fewer parameters
#' than it has coefficients, and its nonzero count -- always the full width,
#' since ridge shrinks but never zeroes -- would charge it for parameters it is
#' not using. At 117 features and lambda 0.6 that difference decides whether a
#' node can ever keep a linear model.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param penalty Numeric: Ridge penalty.
#' @param intercept Logical: Whether column 1 is the intercept.
#'
#' @return Numeric scalar in `[0, ncol(G)]`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_ridge_edf <- function(G, penalty, intercept = TRUE) {
  d <- ncol(G)
  ridge <- linad_ridge_diagonal(G, penalty, intercept)
  diag(G) <- diag(G) + ridge
  chol_factor <- tryCatch(chol(G), error = function(e) NULL)
  if (is.null(chol_factor)) {
    return(d)
  }
  min(max(d - sum(ridge * diag(chol2inv(chol_factor))), 0), d)
} # /rtemis::linad_ridge_edf


# %% linad_forward ----
#' Forward stepwise selection from a Gram matrix
#'
#' Adds `nvmax` features one at a time, each time the one that reduces the
#' weighted residual sum of squares most. The reduction from adding feature `j`
#' to an active set `A` is `g_j^2 / s_j`, where `g` is the gradient
#' `Xty - G[, A] b_A` and `s_j` the Schur complement
#' `G[j, j] - G[j, A] G[A, A]^-1 G[A, j]` -- both readable straight off the
#' Gram, so no candidate is ever refit.
#'
#' With `intercept = TRUE` column 1 is always active and is not counted against
#' `nvmax`. With FALSE the search starts from nothing, which on a design centered
#' by its weighted means selects the same features and the same coefficients --
#' the gain `g_j^2 / s_j` is invariant to a column rescaling and to the level.
#'
#' `nvmax` is a term count, not an upper bound: the search takes exactly that
#' many steps unless it runs out of features that improve the fit. That is the
#' behavior the published experiments were run under.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param Xty Numeric vector: `X'Wy`.
#' @param nvmax Integer: Number of features to select beside the intercept.
#' @param intercept Logical: If TRUE, column 1 is an always-active intercept.
#'
#' @return Numeric vector of coefficients, zero outside the selected set, or
#' NULL if even the starting system is singular.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_forward <- function(
  G,
  Xty,
  nvmax,
  intercept = TRUE,
  penalty = 0,
  syy = NULL,
  sample_weight = NULL,
  stop_rule = "none"
) {
  d <- ncol(G)
  coefficients <- rep(0, d)
  active <- if (intercept) 1L else integer(0)
  diagonal <- diag(G)
  nugget <- LINAD_NUGGET * max(mean(diagonal), .Machine[["double.eps"]])
  n_steps <- min(as.integer(nvmax), d - length(active))
  # A per-term cost is what makes `nvmax` a ceiling rather than a quota. Ridge
  # shrinks but never zeroes, so a penalized gain alone would still spend the
  # whole budget: every added term reduces the residual sum of squares by
  # something positive.
  cost <- switch(
    stop_rule,
    none = NULL,
    aic = 2,
    bic = if (is.null(sample_weight) || sample_weight <= 1) {
      NULL
    } else {
      log(sample_weight)
    },
    NULL
  )
  testing <- !is.null(cost) && !is.null(syy) && !is.null(sample_weight)
  penalties <- function(index) {
    # The intercept is never penalized: it carries the level, and shrinking it
    # would pull the fit toward zero rather than toward the mean.
    out <- rep(penalty, length(index))
    if (intercept) {
      out[index == 1L] <- 0
    }
    out
  }
  solve_active <- function(index) {
    if (length(index) == 0L) {
      return(numeric(0))
    }
    linad_chol_solve(
      G[index, index, drop = FALSE],
      Xty[index],
      penalties(index),
      intercept = intercept
    )
  }
  residual_sum_squares <- function(index, solved) {
    if (is.null(syy)) {
      return(NA_real_)
    }
    if (length(index) == 0L) {
      return(syy)
    }
    linad_gram_loss(
      G[index, index, drop = FALSE],
      Xty[index],
      syy,
      solved
    )
  }

  current <- NULL
  for (step in seq_len(n_steps + 1L)) {
    solved <- solve_active(active)
    if (is.null(solved)) {
      return(NULL)
    }
    coefficients[] <- 0
    coefficients[active] <- solved
    current <- residual_sum_squares(active, solved)
    if (step > n_steps) {
      break
    }
    gradient <- if (length(active) > 0L) {
      Xty - drop(G[, active, drop = FALSE] %*% solved)
    } else {
      Xty
    }
    candidates <- setdiff(seq_len(d), active)
    chol_active <- if (length(active) > 0L) {
      chol(
        G[active, active, drop = FALSE] +
          diag(nugget + penalties(active), length(active))
      )
    } else {
      NULL
    }
    # Selection under the objective the fit uses: the same penalty enters the
    # Schur complement, so a feature that only looks good unpenalized does not
    # win the search.
    # Every candidate's Schur complement from one triangular solve over their
    # columns together, rather than one solve each: the matrices are the size of
    # the active set, so per-call overhead otherwise dwarfs the arithmetic.
    schur <- if (is.null(chol_active)) {
      diagonal[candidates]
    } else {
      projected <- backsolve(
        chol_active,
        G[active, candidates, drop = FALSE],
        transpose = TRUE
      )
      diagonal[candidates] - colSums(projected^2)
    }
    schur <- schur + penalty
    gain <- gradient[candidates]^2 / schur
    gain[!is.finite(schur) | schur <= nugget] <- -Inf
    best <- which.max(gain)
    if (length(best) == 0L || !is.finite(gain[[best]]) || gain[[best]] <= 0) {
      break
    }
    proposed <- c(active, candidates[[best]])
    if (testing) {
      trial <- solve_active(proposed)
      if (is.null(trial)) {
        break
      }
      improved <- residual_sum_squares(proposed, trial)
      # The Gaussian information criterion, in the form that needs only the two
      # residual sums of squares: a term is kept when the deviance it buys
      # exceeds what it costs.
      if (
        !is.finite(improved) ||
          improved <= 0 ||
          !is.finite(current) ||
          current <= 0 ||
          sample_weight * log(current / improved) <= cost
      ) {
        break
      }
    }
    active <- proposed
  }
  coefficients
} # /rtemis::linad_forward


# %% linad_baseline ----
#' The constant that alone minimizes the loss
#'
#' Equation 3's `F_0 = argmin_c sum L(y_i, c)`: the weighted mean for squared
#' error, and half the log odds for the exponential-family logistic loss on
#' `{-1, +1}`, which is where `sum w y / (1 + exp(2yc))` vanishes.
#'
#' This is what `root_learning_rate` shrinks the root model *towards*. Shrinking
#' it towards zero instead would mean shrinking towards predicting nothing, which
#' for an outcome centered anywhere but the origin is worse than useless.
#'
#' @param y Numeric vector: Outcome; `{-1, +1}` for classification.
#' @param w Numeric vector: Case weights.
#' @param type Character: "Regression" or "Classification".
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_baseline <- function(y, w, type) {
  if (!identical(type, "Classification")) {
    return(stats::weighted.mean(y, w))
  }
  positive <- sum(w[y > 0])
  negative <- sum(w[y < 0])
  if (positive <= 0 || negative <= 0) {
    # One class only: the minimizing constant is unbounded, so stop at the
    # largest margin the line search is allowed to take anyway.
    return(sign(positive - negative) * LINAD_BASELINE_MAX)
  }
  0.5 * log(positive / negative)
} # /rtemis::linad_baseline


# %% linad_constant ----
#' The node's constant, by the manuscript's closed forms
#'
#' Equation 19 for squared error, with the `sum(p)` denominator the manuscript
#' drops -- a weighted *mean* of the residual, not a weighted sum. Equation 20
#' for classification, where the loss has no closed form and the constant is one
#' Newton step, the ratio of the weighted first to the weighted second
#' derivative.
#'
#' `constant_rule = "least_squares"` takes the weighted mean in both cases
#' instead, which is what an intercept-only least-squares fit returns. The two
#' are indistinguishable on the data tried so far -- mean holdout AUC 0.8714
#' against 0.8712 over 288 classification fits -- so which is better is a
#' question for the full rerun rather than for reading, and both ship.
#'
#' This is the tree's own contribution at the node. Everything the linear model
#' adds is a slope, so the two parts of the fit stay separable all the way to
#' the plot.
#'
#' @param r Numeric vector: Negative gradient, the target the node is fitting.
#' @param derivatives List: `linad_gradient()` output at the parent's function
#' value, needed for Eq 20 and ignored for Eq 19.
#' @param w Numeric vector: Weights.
#' @param idx Integer vector: Rows this node fits on.
#' @param type Character: "Regression" or "Classification".
#' @param max_step Numeric: Bound on the absolute constant, as for the line
#' search: a node whose second derivative nearly vanishes would otherwise take
#' an unbounded step.
#' @param rule Character: "closed_form" for Eqs 19/20, or "least_squares" for
#' the weighted mean of the residual in both outcome types. They coincide for a
#' regression, so this only bites for a classification.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_constant <- function(
  r,
  derivatives,
  w,
  idx,
  type,
  max_step,
  rule = "closed_form"
) {
  total <- sum(w[idx])
  if (total <= 0) {
    return(0)
  }
  # Eq 19, and the whole of the `least_squares` rule: the weighted mean of the
  # residual is also what an intercept-only least-squares fit returns, so the
  # two rules coincide for a regression and differ only for a classification.
  if (!identical(type, "Classification") || identical(rule, "least_squares")) {
    return(sum(w[idx] * r[idx]) / total)
  }
  # Eq 20.
  numerator <- sum(w[idx] * derivatives[["g"]][idx])
  denominator <- sum(w[idx] * derivatives[["h"]][idx])
  if (!is.finite(denominator) || denominator <= 0) {
    return(sign(-numerator) * min(max_step, LINAD_BASELINE_MAX))
  }
  step <- -numerator / denominator
  if (!is.finite(step)) {
    return(0)
  }
  sign(step) * min(abs(step), max_step)
} # /rtemis::linad_constant


# %% linad_node_test ----
#' Whether a node's slopes earn their coefficients
#'
#' The constant is nested in the linear model, so on the node's own cases the
#' slopes always reduce the residual sum of squares and the comparison is
#' meaningless without a cost. This is the Gaussian information criterion in
#' the form that needs only the two sums of squares, charging `n_terms` at the
#' rule's per-term rate.
#'
#' Both split searches and the commit call this, so a candidate is scored by
#' the model the node will actually receive.
#'
#' @param rss_constant Numeric: Weighted residual sum of squares of the
#' constant-only fit.
#' @param rss_linear Numeric: The same after the slopes.
#' @param sw Numeric: Total weight carried by the fit.
#' @param n_terms Numeric: Parameters the slopes spend -- their count where a
#' fit zeroes what it does not use, and the effective degrees of freedom where
#' it shrinks instead.
#' @param rule Character: "none", "aic" or "bic".
#'
#' @return Logical: TRUE to keep the slopes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_node_test <- function(rss_constant, rss_linear, sw, n_terms, rule) {
  if (identical(rule, "none") || n_terms <= 0) {
    return(TRUE)
  }
  per_term <- switch(rule, aic = 2, bic = log(sw), NA_real_)
  if (!is.finite(per_term) || per_term <= 0) {
    return(TRUE)
  }
  if (!is.finite(rss_constant) || rss_constant <= 0 || !is.finite(rss_linear)) {
    return(FALSE)
  }
  sw * log(rss_constant / max(rss_linear, .Machine[["double.xmin"]])) >
    per_term * n_terms
} # /rtemis::linad_node_test


# %% linad_solve ----
#' Fit one leaf model, as a constant plus slopes
#'
#' The single entry point for every linear model LINAD fits, at the root and at
#' every node. `node_model` names both the fitting procedure and its
#' regularization, as one flat choice -- the schema's `applies_when` gates
#' cannot chain, and it matches the manuscript's own `lin.type` vocabulary.
#'
#' The update is split in two. The **constant** is the node's own value by
#' Eq 19/20; the **slopes** are then fitted without an intercept to what the
#' constant leaves behind, on a design centered by the node's *weighted* column
#' means. That centering is what makes the split exact: on a centered design the
#' intercept-free fit returns the same coefficients as fitting an intercept
#' jointly, for ridge at any `lambda` and for forward selection. Without it the
#' slopes absorb part of the level and the fit is worse.
#'
#' Separating them is the point. The constant is what the *tree* contributes at
#' this node and accumulates into `node_value`; the slopes are what the *linear
#' model* adds. `"constant"` simply fits no slopes, which is what reduces LINAD
#' to the Additive Tree -- the manuscript notes the software "allows to skip
#' fitting of the linear models".
#'
#' @param xm Numeric matrix: Intercept-augmented design matrix.
#' @param y Numeric vector: Target -- the pseudo-residual, except at the root.
#' @param w Numeric vector: Case weights.
#' @param idx Integer vector: Rows this node fits on.
#' @param node_model Character: "forward", "ridge", "elasticnet" or "constant".
#' @param lambda Numeric: Ridge/elastic-net penalty.
#' @param alpha Numeric: Elastic-net mixing.
#' @param nvmax Integer: Forward-selection term count.
#' @param derivatives List: `linad_gradient()` output, for Eq 20.
#' @param type Character: "Regression" or "Classification".
#' @param max_step Numeric: Bound on the constant.
#' @param constant_rule Character: Which rule computes the constant.
#' @param node_test Character: Cost a node's slopes must earn over the constant
#' alone, as `linad_node_test()` applies it.
#'
#' @return List with `coefficients` (length `ncol(xm)`, column 1 the effective
#' intercept in the node's own coordinates) and `constant` (the node's value),
#' or NULL when the node cannot be fit and the caller should treat the update as
#' zero.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_solve <- function(
  xm,
  y,
  w,
  idx,
  node_model,
  lambda = 0.05,
  forward_stop = "none",
  alpha = 1,
  nvmax = 3L,
  derivatives = NULL,
  type = "Regression",
  max_step = 1000,
  constant_rule = "closed_form",
  node_test = "none"
) {
  if (length(idx) == 0L || sum(w[idx]) <= 0) {
    return(NULL)
  }
  constant <- linad_constant(
    y,
    derivatives,
    w,
    idx,
    type,
    max_step,
    constant_rule
  )
  d <- ncol(xm)
  if (identical(node_model, "constant")) {
    coefficients <- rep(0, d)
    coefficients[[1L]] <- constant
    return(list(coefficients = coefficients, constant = constant))
  }

  # Slopes, fitted to what the constant leaves, on a design centered by this
  # node's weighted means. Column 1 is the intercept and is dropped: the
  # constant already carries the level.
  slope_columns <- seq.int(2L, d)
  center <- drop(crossprod(w[idx], xm[idx, slope_columns, drop = FALSE])) /
    sum(w[idx])
  centered <- sweep(xm[, slope_columns, drop = FALSE], 2L, center, "-")
  residual <- y - constant

  slopes <- if (identical(node_model, "elasticnet")) {
    linad_glmnet_slopes(centered, residual, w, idx, lambda, alpha)
  } else {
    gram <- linad_gram(cbind(centered), residual, w, idx)
    if (identical(node_model, "forward")) {
      linad_forward(
        gram[["G"]],
        gram[["Xty"]],
        nvmax,
        intercept = FALSE,
        penalty = lambda * gram[["sw"]],
        syy = gram[["syy"]],
        sample_weight = gram[["sw"]],
        stop_rule = forward_stop
      )
    } else {
      # "ridge". Scaling by the weight total matches glmnet's objective, which
      # divides the residual sum of squares by it, so one `lambda` means the
      # same thing in a node of 30 cases and a node of 300.
      linad_chol_solve(
        gram[["G"]],
        gram[["Xty"]],
        lambda * gram[["sw"]],
        intercept = FALSE
      )
    }
  }
  if (is.null(slopes) || !all(is.finite(slopes))) {
    slopes <- rep(0, length(slope_columns))
  }
  n_terms <- if (identical(node_model, "ridge")) {
    linad_ridge_edf(gram[["G"]], lambda * gram[["sw"]], intercept = FALSE)
  } else {
    sum(slopes != 0)
  }
  if (!identical(node_test, "none") && any(slopes != 0)) {
    explained <- drop(centered[idx, , drop = FALSE] %*% slopes)
    keep <- linad_node_test(
      sum(w[idx] * residual[idx]^2),
      sum(w[idx] * (residual[idx] - explained)^2),
      sum(w[idx]),
      n_terms,
      node_test
    )
    if (!keep) {
      slopes <- rep(0, length(slope_columns))
    }
  }
  # Back to the node's coordinates: the centered fit's level is `constant`, so
  # the effective intercept absorbs what centering removed.
  coefficients <- c(constant - sum(center * slopes), slopes)
  list(coefficients = coefficients, constant = constant)
} # /rtemis::linad_solve


# %% linad_glmnet_slopes ----
#' Elastic-net slopes on a centered, intercept-free design
#'
#' The one leaf model not fitted natively. Coordinate descent overtakes a
#' Cholesky solve once the design is wide, and an L1 penalty has no closed form,
#' so this path earns its dependency; the other three do not.
#'
#' `intercept = FALSE`, because the node's constant has already been removed from
#' the target and the design is centered on the node's weighted means.
#'
#' @param centered Numeric matrix: Design without an intercept column, centered.
#' @param residual Numeric vector: Target after the constant is removed.
#' @param w Numeric vector: Weights.
#' @param idx Integer vector: Rows this node fits on.
#' @param lambda Numeric: Penalty.
#' @param alpha Numeric: Mixing between ridge (0) and lasso (1).
#'
#' @return Numeric vector of slopes, or NULL if the fit failed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_glmnet_slopes <- function(centered, residual, w, idx, lambda, alpha) {
  check_dependencies("glmnet")
  if (ncol(centered) == 0L) {
    return(NULL)
  }
  fit <- tryCatch(
    glmnet::glmnet(
      centered[idx, , drop = FALSE],
      residual[idx],
      weights = w[idx],
      alpha = alpha,
      lambda = lambda,
      standardize = FALSE,
      intercept = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  # `coef()` keeps a leading intercept slot even when none was fitted.
  drop(as.matrix(stats::coef(fit, s = lambda)))[-1L]
} # /rtemis::linad_glmnet_slopes


# %% linad_loss ----
#' Per-case loss
#'
#' Regression is half squared error, so its gradient is the plain residual.
#' Classification is the exponential-family logistic loss on `y` in `{-1, +1}`,
#' the parameterization the Additive Tree line of work uses.
#'
#' @param y Numeric vector: Outcome, `{-1, +1}` for classification.
#' @param f Numeric vector: Current function value.
#' @param type Character: "Regression" or "Classification".
#'
#' @return Numeric vector of per-case losses.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_loss <- function(y, f, type) {
  if (identical(type, "Classification")) {
    # log(1 + exp(-2yF)), written so a large negative margin does not overflow.
    margin <- -2 * y * f
    ifelse(margin > 30, margin, log1p(exp(pmin(margin, 30))))
  } else {
    0.5 * (y - f)^2
  }
} # /rtemis::linad_loss


# %% linad_gradient ----
#' First and second derivatives of the loss with respect to the function value
#'
#' Taken with respect to the **accumulated** `f`, which is what Eqs 13 and 26
#' specify; Algorithm 1 lines 20 and 25 write the node's own local model
#' instead, and the two coincide only at the root.
#'
#' @param y Numeric vector: Outcome.
#' @param f Numeric vector: Current function value.
#' @param type Character: "Regression" or "Classification".
#'
#' @return List with `g` (gradient) and `h` (second derivative).
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_gradient <- function(y, f, type) {
  if (identical(type, "Classification")) {
    g <- -2 * y / (1 + exp(pmin(2 * y * f, 30)))
    list(g = g, h = abs(g) * (2 - abs(g)))
  } else {
    list(g = f - y, h = rep(1, length(y)))
  }
} # /rtemis::linad_gradient


# %% linad_cut_positions ----
#' Thin a feature's admissible split positions down to a fixed number
#'
#' Both searches need a way to consider `k` cut points rather than every
#' distinct value: the stump because scoring every value is wasted once the
#' feature is finely resolved, the exhaustive search because each candidate
#' costs a solve. Taking evenly spaced order statistics of the admissible
#' positions gives equal-frequency bins -- each candidate cut has roughly the
#' same number of cases on either side of it, which is what a histogram split
#' means here.
#'
#' `"frequency"` spaces the cuts evenly through the *cases*, so each candidate
#' has roughly the same number either side. `"width"` spaces them evenly through
#' the feature's *range*, which is the literal histogram and what a fixed grid
#' over the axis gives; on a skewed feature most of its bins can be near-empty.
#' The two differ only where a feature is not uniform, which is most of them.
#'
#' @param breaks Integer vector: Positions in sorted order after which the
#' feature's value changes.
#' @param n_cuts Integer: How many to keep.
#' @param sorted Optional Numeric vector: The feature's values in sorted order.
#' Required for `type = "width"`, which places cuts by value rather than by rank.
#' @param type Character: "frequency" or "width".
#'
#' @return Integer vector, sorted and unique, possibly shorter than `n_cuts`
#' when the feature has fewer distinct values than that.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_cut_positions <- function(
  breaks,
  n_cuts,
  sorted = NULL,
  type = "frequency"
) {
  if (length(breaks) <= n_cuts) {
    return(breaks)
  }
  if (identical(type, "width") && !is.null(sorted)) {
    values <- sorted[breaks]
    targets <- seq(
      values[[1L]],
      values[[length(values)]],
      length.out = n_cuts + 2L
    )[-c(1L, n_cuts + 2L)]
    # The admissible position whose value sits closest to each evenly spaced
    # target. A feature with a gap contributes one position for every target
    # that falls in it, which `unique()` then collapses -- so an equal-width
    # search on a skewed feature really does try fewer cuts.
    chosen <- vapply(
      targets,
      function(target) breaks[[which.min(abs(values - target))]],
      integer(1L)
    )
    return(sort(unique(chosen)))
  }
  probabilities <- seq(0, 1, length.out = n_cuts + 2L)[-c(1L, n_cuts + 2L)]
  sort(unique(stats::quantile(
    breaks,
    probs = probabilities,
    names = FALSE,
    type = 1L
  )))
} # /rtemis::linad_cut_positions


# %% linad_context ----
#' Precompute everything about the features that does not change between nodes
#'
#' Sort orders and the positions where a feature's sorted values change are
#' properties of the training data, not of a node, so they are computed once and
#' reused at every split search. This is the single largest reason the split
#' search is not delegated to `rpart`, which re-sorts on every call.
#'
#' Splits are searched on the **original** features, so a factor splits on a set
#' of levels and the rule reads naturally, while the leaf models use the encoded
#' design matrix. The two indexings are kept apart deliberately: conflating them
#' is what made the legacy optimized search name the wrong feature whenever a
#' factor preceded the split feature.
#'
#' @param x data.frame: Features, before encoding.
#' @param n_bins Optional Integer: Discretize each numeric feature into this
#' many bins and consider only bin boundaries as candidate splits. NULL
#' considers every distinct value.
#' @param bin_type Character: "frequency" or "width".
#'
#' @return List describing the numeric and factor features.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_context <- function(x, n_bins = NULL, bin_type = "frequency") {
  n <- nrow(x)
  is_factor <- vapply(x, is.factor, logical(1L))
  numeric_names <- names(x)[!is_factor]
  factor_names <- names(x)[is_factor]
  numeric_matrix <- if (length(numeric_names) > 0L) {
    as.matrix(as.data.frame(lapply(x[numeric_names], as.numeric)))
  } else {
    matrix(numeric(0), nrow = n, ncol = 0L)
  }
  numeric_order <- vector("list", length(numeric_names))
  numeric_breaks <- vector("list", length(numeric_names))
  for (j in seq_along(numeric_names)) {
    o <- order(numeric_matrix[, j])
    sorted <- numeric_matrix[o, j]
    numeric_order[[j]] <- o
    # Positions after which the value changes: the only places a split can go.
    admissible <- if (n > 1L) {
      which(sorted[-n] < sorted[-1L])
    } else {
      integer(0)
    }
    numeric_breaks[[j]] <- if (is.null(n_bins)) {
      admissible
    } else {
      linad_cut_positions(admissible, n_bins - 1L, sorted, bin_type)
    }
  }
  list(
    n = n,
    numeric_matrix = numeric_matrix,
    numeric_names = numeric_names,
    numeric_order = numeric_order,
    numeric_breaks = numeric_breaks,
    factor_codes = lapply(x[factor_names], as.integer),
    factor_levels = lapply(x[factor_names], levels),
    factor_names = factor_names
  )
} # /rtemis::linad_context


# %% linad_scan_features ----
#' Resolve which features a split search scans
#'
#' A restriction arrives as a list of two integer vectors, one indexing
#' `context$numeric_names` and one `context$factor_names`, rather than as a
#' narrowed context. The context holds each feature's sort order and break
#' positions, and rebuilding it per node is precisely the cost it exists to
#' avoid.
#'
#' @param context List: `linad_context()` output.
#' @param features Optional List: `numeric` and `factor` integer vectors.
#'
#' @return List of two integer vectors, named `numeric` and `factor`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_scan_features <- function(context, features = NULL) {
  if (is.null(features)) {
    list(
      numeric = seq_along(context[["numeric_names"]]),
      factor = seq_along(context[["factor_names"]])
    )
  } else {
    features
  }
} # /rtemis::linad_scan_features


# %% linad_slope_gain ----
#' What a side's slope in the split variable explains
#'
#' The sum of squares a weighted least-squares slope removes beyond the level,
#' `Sxy_c^2 / Sxx_c` on centered sufficient statistics. A side whose split
#' variable is constant has no slope to fit and explains nothing extra.
#'
#' @param sw Numeric: Sum of weights.
#' @param sx Numeric: Weighted sum of the split variable.
#' @param sxx Numeric: Weighted sum of its square.
#' @param sxy Numeric: Weighted sum of its product with the residual.
#' @param sy Numeric: Weighted sum of the residual.
#'
#' @return Numeric: Explained sum of squares, one per input element.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_slope_gain <- function(sw, sx, sxx, sxy, sy) {
  sxx_c <- sxx - sx * sx / sw
  sxy_c <- sxy - sx * sy / sw
  ifelse(
    sxx_c > .Machine[["double.eps"]] * pmax(1, abs(sxx)),
    sxy_c^2 / sxx_c,
    0
  )
} # /rtemis::linad_slope_gain


# %% linad_stump ----
#' Best weighted squared-error split over every feature
#'
#' The decision stump of Algorithm 1 line 21, fit to the negative gradient with
#' the node's weights. Minimizing the weighted residual sum of squares over a
#' binary partition is the same as maximizing
#' `SwrL^2 / SwL + SwrR^2 / SwR`, which one pass of cumulative sums evaluates at
#' every candidate cut at once.
#'
#' A factor's levels are ordered by their weighted mean residual and then
#' scanned the same way, which is exact for squared error rather than a
#' heuristic over the `2^(k-1) - 1` partitions.
#'
#' `min_cases_child` counts the node's own members, not all rows: under soft
#' weighting every case carries some weight everywhere, so a row count would not
#' mean what a user asking for a minimum leaf size means.
#'
#' @param context List: `linad_context()` output.
#' @param r Numeric vector: Negative gradient, length n.
#' @param w Numeric vector: Node weights, length n.
#' @param member Logical vector: Node membership, length n.
#' @param min_cases_child Integer: Minimum members either side of the split.
#' @param features Optional List: Which features to scan, as
#' `linad_scan_features()` describes. NULL scans every feature.
#' @param criterion Character: What each side's fit explains -- its level
#' ("mean") or its level and its slope in the split variable ("linear").
#'
#' @return List describing the split, or NULL when no admissible split exists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_stump <- function(
  context,
  r,
  w,
  member,
  min_cases_child,
  features = NULL,
  criterion = "mean"
) {
  n <- context[["n"]]
  wr <- w * r
  membership <- as.numeric(member)
  best <- list(gain = -Inf)
  scan <- linad_scan_features(context, features)
  slopes <- identical(criterion, "linear")

  # Numeric features ----
  for (j in scan[["numeric"]]) {
    breaks <- context[["numeric_breaks"]][[j]]
    if (length(breaks) == 0L) {
      next
    }
    o <- context[["numeric_order"]][[j]]
    cum_w <- cumsum(w[o])
    cum_wr <- cumsum(wr[o])
    cum_member <- cumsum(membership[o])
    total_w <- cum_w[[n]]
    total_wr <- cum_wr[[n]]
    total_member <- cum_member[[n]]
    left_w <- cum_w[breaks]
    right_w <- total_w - left_w
    admissible <- cum_member[breaks] >= min_cases_child &
      (total_member - cum_member[breaks]) >= min_cases_child &
      left_w > 0 &
      right_w > 0
    if (!any(admissible)) {
      next
    }
    left_wr <- cum_wr[breaks]
    gain <- rep(-Inf, length(breaks))
    gain[admissible] <- left_wr[admissible]^2 /
      left_w[admissible] +
      (total_wr - left_wr[admissible])^2 / right_w[admissible]
    if (slopes) {
      xo <- context[["numeric_matrix"]][o, j]
      cum_wx <- cumsum(w[o] * xo)
      cum_wxx <- cumsum(w[o] * xo * xo)
      cum_wxr <- cumsum(wr[o] * xo)
      gain[admissible] <- gain[admissible] +
        linad_slope_gain(
          left_w[admissible],
          cum_wx[breaks][admissible],
          cum_wxx[breaks][admissible],
          cum_wxr[breaks][admissible],
          left_wr[admissible]
        ) +
        linad_slope_gain(
          right_w[admissible],
          cum_wx[[n]] - cum_wx[breaks][admissible],
          cum_wxx[[n]] - cum_wxx[breaks][admissible],
          cum_wxr[[n]] - cum_wxr[breaks][admissible],
          total_wr - left_wr[admissible]
        )
    }
    k <- which.max(gain)
    if (gain[[k]] > best[["gain"]]) {
      position <- breaks[[k]]
      sorted <- context[["numeric_matrix"]][o, j]
      best <- list(
        gain = gain[[k]],
        feature = context[["numeric_names"]][[j]],
        kind = "numeric",
        value = (sorted[[position]] + sorted[[position + 1L]]) / 2,
        levels = NULL,
        column = j
      )
    }
  }

  # Factor features ----
  for (j in scan[["factor"]]) {
    codes <- context[["factor_codes"]][[j]]
    levels_j <- context[["factor_levels"]][[j]]
    n_levels <- length(levels_j)
    if (n_levels < 2L) {
      next
    }
    level_w <- as.vector(rowsum(w, codes, reorder = TRUE))
    level_wr <- as.vector(rowsum(wr, codes, reorder = TRUE))
    level_member <- as.vector(rowsum(membership, codes, reorder = TRUE))
    present <- sort(unique(codes))
    keep <- level_w > 0
    if (sum(keep) < 2L) {
      next
    }
    # Ordering levels by weighted mean residual makes the best contiguous split
    # of that ordering the best of all level partitions, for squared error.
    ranking <- order(level_wr[keep] / level_w[keep])
    ordered_levels <- present[keep][ranking]
    cum_w <- cumsum(level_w[keep][ranking])
    cum_wr <- cumsum(level_wr[keep][ranking])
    cum_member <- cumsum(level_member[keep][ranking])
    m <- length(ordered_levels)
    total_w <- cum_w[[m]]
    total_wr <- cum_wr[[m]]
    total_member <- cum_member[[m]]
    cuts <- seq_len(m - 1L)
    left_w <- cum_w[cuts]
    right_w <- total_w - left_w
    admissible <- cum_member[cuts] >= min_cases_child &
      (total_member - cum_member[cuts]) >= min_cases_child &
      left_w > 0 &
      right_w > 0
    if (!any(admissible)) {
      next
    }
    left_wr <- cum_wr[cuts]
    gain <- rep(-Inf, length(cuts))
    gain[admissible] <- left_wr[admissible]^2 /
      left_w[admissible] +
      (total_wr - left_wr[admissible])^2 / right_w[admissible]
    k <- which.max(gain)
    if (gain[[k]] > best[["gain"]]) {
      best <- list(
        gain = gain[[k]],
        feature = context[["factor_names"]][[j]],
        kind = "factor",
        value = NA_real_,
        levels = levels_j[ordered_levels[seq_len(k)]],
        column = j
      )
    }
  }

  if (!is.finite(best[["gain"]])) {
    return(NULL)
  }
  best
} # /rtemis::linad_stump


# %% linad_goes_left ----
#' Which cases take the left branch at one split
#'
#' The single definition of the routing rule, used while growing, while
#' predicting, and while rebuilding a tree from its frame -- so training and
#' inference cannot drift apart.
#'
#' @param split List: A `linad_stump()` result, or a frame row carrying
#' `feature`, `kind`, `value` and `levels`.
#' @param x data.frame: Cases.
#'
#' @return Logical vector, one per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_goes_left <- function(split, x) {
  values <- x[[split[["feature"]]]]
  if (identical(split[["kind"]], "factor")) {
    as.character(values) %in% split[["levels"]]
  } else {
    as.numeric(values) < split[["value"]]
  }
} # /rtemis::linad_goes_left


# %% linad_line_search ----
#' Newton step size for one functional update
#'
#' Equation 27. One Newton step in `rho` for the update `f + rho * v`, over the
#' rows the update actually touches -- cases outside the node are unaffected, so
#' they cannot inform the step.
#'
#' For squared error the step is the exact minimizer, since the objective is
#' quadratic in `rho`.
#'
#' @param y Numeric vector: Outcome.
#' @param f Numeric vector: Current function value.
#' @param v Numeric vector: Update direction.
#' @param w Numeric vector: Case weights.
#' @param idx Integer vector: Rows the update applies to.
#' @param type Character: "Regression" or "Classification".
#' @param max_step Numeric: Bound on the absolute step.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_line_search <- function(y, f, v, w, idx, type, max_step) {
  derivatives <- linad_gradient(y[idx], f[idx], type)
  numerator <- sum(w[idx] * derivatives[["g"]] * v[idx])
  denominator <- sum(w[idx] * derivatives[["h"]] * v[idx]^2)
  if (!is.finite(denominator) || denominator <= 0) {
    return(0)
  }
  step <- -numerator / denominator
  if (!is.finite(step)) {
    return(0)
  }
  sign(step) * min(abs(step), max_step)
} # /rtemis::linad_line_search


# %% linad_child ----
#' Build one child of a node being expanded
#'
#' Algorithm 1 lines 23-28 and 32-34. The child's coefficients are the parent's
#' plus the shrunk, line-searched update, which is what makes a leaf's
#' coefficients the accumulated sum along its path.
#'
#' There is no separate stump constant. The manuscript's stump supplies the
#' partition only (Algorithm 1 line 21) and the leaf model, which carries its own
#' intercept, is the whole update; the legacy implementation added both and then
#' shrank them separately, which is where its fitted values and its stored
#' coefficients came apart for classification.
#'
#' @param state List: Fit state.
#' @param node List: Parent node.
#' @param r Numeric vector: Negative gradient at the parent's function value.
#' @param derivatives List: `linad_gradient()` at the parent's function value.
#' @param idx Integer vector: The child's member rows.
#' @param weights Numeric vector: The child's weights, after Eq 29.
#'
#' @return List with the child's `index`, `weights`, `coef`, `depth`, the
#' un-shrunk update direction `v` and its coefficients, and the `constant` that
#' accumulates into `node_value`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_child <- function(state, node, r, derivatives, idx, weights) {
  fit <- NULL
  # A node below the linear model's floor is left to its parent. Splits already
  # respect that floor, so this fires only for a node the caller placed there.
  model <- if (
    identical(state[["node_model"]], "constant") ||
      length(idx) >= state[["min_cases_node_model"]]
  ) {
    state[["node_model"]]
  } else {
    NULL
  }
  # A residual with no variation is still fitted: the update is that constant
  # with zero slopes, which is what the solve returns. Degeneracy is handled
  # below, where a NULL or non-finite fit falls back to a zero update.
  if (!is.null(model) && length(idx) > 0L) {
    # Every case with weight left, not just the node's own. This is what makes
    # `gamma` a hyperparameter rather than a formality: at 0 the set below is
    # exactly the node's cases and the fit is the hard-partition one, and as it
    # rises each leaf model is pulled toward what the rest of the data supports.
    #
    # Rows whose weight has decayed past `LINAD_WEIGHT_TOLERANCE` are dropped.
    # They contribute nothing a double can represent, and skipping them is what
    # keeps a deep node's fit proportional to its own size.
    active <- which(weights > LINAD_WEIGHT_TOLERANCE * max(weights))
    fit <- linad_solve(
      state[["xm"]],
      r,
      weights,
      active,
      model,
      state[["lambda"]],
      state[["alpha"]],
      state[["nvmax"]],
      forward_stop = state[["forward_stop"]],
      derivatives = derivatives,
      type = state[["type"]],
      max_step = state[["line_search_max"]],
      constant_rule = state[["constant_rule"]],
      node_test = state[["node_test"]]
    )
  }
  if (is.null(fit) || !all(is.finite(fit[["coefficients"]]))) {
    fit <- list(coefficients = rep(0, ncol(state[["xm"]])), constant = 0)
  }
  list(
    index = idx,
    weights = weights,
    depth = node[["depth"]] + 1L,
    update = fit[["coefficients"]],
    constant = fit[["constant"]],
    v = drop(state[["xm"]] %*% fit[["coefficients"]])
  )
} # /rtemis::linad_child


# %% linad_node_loss ----
#' Loss over a set of cases
#'
#' Case weights, not the soft membership weights: the criterion should respond
#' to inverse-frequency weighting, and it must be additive across the two halves
#' of a split for the loss reduction of Algorithm 1 line 9 to mean anything.
#'
#' @param state List: Fit state.
#' @param coefficients Numeric vector: The node's accumulated coefficients.
#' @param idx Integer vector: Rows to score.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_node_loss <- function(state, coefficients, idx) {
  if (length(idx) == 0L) {
    return(0)
  }
  f <- drop(state[["xm"]][idx, , drop = FALSE] %*% coefficients)
  sum(
    state[["case_weights"]][idx] *
      linad_loss(state[["y"]][idx], f, state[["type"]])
  )
} # /rtemis::linad_node_loss


# %% linad_sample_features ----
#' Draw the feature sample one split search scans
#'
#' `mtry` features without replacement from those the tree holds, returned in
#' the two-vector form `linad_scan_features()` reads. NULL, or an `mtry` that
#' reaches every feature, returns NULL so the search takes its unrestricted
#' path unchanged.
#'
#' @param context List: `linad_context()` output.
#' @param mtry Optional Integer: Features to sample.
#'
#' @return List of two integer vectors, or NULL for no restriction.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_sample_features <- function(context, mtry = NULL) {
  n_numeric <- length(context[["numeric_names"]])
  n_factor <- length(context[["factor_names"]])
  total <- n_numeric + n_factor
  if (is.null(mtry) || mtry >= total) {
    return(NULL)
  }
  drawn <- sample.int(total, mtry)
  list(
    numeric = drawn[drawn <= n_numeric],
    factor = drawn[drawn > n_numeric] - n_numeric
  )
} # /rtemis::linad_sample_features


# %% linad_min_child_cases ----
#' Fewest cases a split may leave on a side
#'
#' The higher of `min_cases_leaf` and `min_cases_node_model`, so that a split
#' never creates a node too small to carry its own model: such a node takes a
#' zero update and predicts exactly what its parent did, spending a leaf to
#' change nothing. Both split searches use this, so the exhaustive search scores
#' a candidate only by child models the commit will fit.
#'
#' A constant node carries no model and is exempt, which is what keeps the CART
#' and Additive Tree reductions exact.
#'
#' @param state List: Fit state.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_min_child_cases <- function(state) {
  if (identical(state[["node_model"]], "constant")) {
    return(state[["min_cases_leaf"]])
  }
  max(state[["min_cases_leaf"]], state[["min_cases_node_model"]])
} # /rtemis::linad_min_child_cases


# %% linad_split_search ----
#' Dispatch to the configured split search
#'
#' @param state List: Fit state.
#' @param r Numeric vector: Negative gradient.
#' @param w Numeric vector: Node weights.
#' @param member Logical vector: Node membership.
#' @param features Optional List: Which features to scan, as
#' `linad_scan_features()` describes. NULL scans every feature.
#'
#' @return A split, or NULL when no admissible split exists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_split_search <- function(state, r, w, member, features) {
  if (identical(state[["split_search"]], "exhaustive")) {
    linad_sweep(state, r, w, member, features)
  } else {
    linad_stump(
      state[["context"]],
      r,
      w,
      member,
      linad_min_child_cases(state),
      features,
      criterion = state[["split_criterion"]]
    )
  }
} # /rtemis::linad_split_search


# %% linad_improves ----
#' Whether a proposal reduces the loss
#'
#' The growth loop's own admission test, named once so the `mtry_split` retry
#' and the loop cannot drift apart on what counts as progress.
#'
#' @param proposal Optional List: `linad_propose()` output.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_improves <- function(proposal) {
  !is.null(proposal) &&
    is.finite(proposal[["score"]]) &&
    proposal[["score"]] > 0
} # /rtemis::linad_improves


# %% linad_propose ----
#' Propose a split for one node, over the features it is given
#'
#' `ExpandNode` of Algorithm 1. Splits the node, fits a leaf model on each side,
#' line-searches the step, and scores the result -- but commits nothing. The
#' growth loop expands every frontier node speculatively and then commits only
#' the best proposal, which is the one-step lookahead of Figure 2 and the reason
#' a LINAD tree's split order is a global argmax rather than a traversal.
#'
#' @param state List: Fit state.
#' @param node List: The node to expand.
#' @param features Optional List: Which features the split search may scan, as
#' `linad_scan_features()` describes. NULL scans every feature.
#'
#' @return List describing the proposal, or NULL if the node cannot be split.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_propose <- function(state, node, features = NULL) {
  f <- drop(state[["xm"]] %*% node[["coef"]])
  derivatives <- linad_gradient(state[["y"]], f, state[["type"]])
  r <- -derivatives[["g"]]
  if (length(node[["index"]]) < state[["min_cases_split"]]) {
    return(NULL)
  }
  member <- logical(state[["n"]])
  member[node[["index"]]] <- TRUE
  split <- linad_split_search(state, r, node[["weights"]], member, features)
  if (is.null(split)) {
    return(NULL)
  }
  goes_left <- linad_goes_left(split, state[["x"]])
  index_left <- node[["index"]][goes_left[node[["index"]]]]
  index_right <- node[["index"]][!goes_left[node[["index"]]]]
  if (length(index_left) == 0L || length(index_right) == 0L) {
    return(NULL)
  }

  # Equation 29: a case keeps its weight in the branch it belongs to and has it
  # scaled by gamma in the other, so influence decays as gamma^depth rather than
  # vanishing at the first split.
  weights_left <- node[["weights"]]
  weights_left[!goes_left] <- weights_left[!goes_left] * state[["gamma"]]
  weights_right <- node[["weights"]]
  weights_right[goes_left] <- weights_right[goes_left] * state[["gamma"]]

  left <- linad_child(state, node, r, derivatives, index_left, weights_left)
  right <- linad_child(state, node, r, derivatives, index_right, weights_right)

  steps <- linad_steps(state, node, f, goes_left, left, right)
  coef_left <- node[["coef"]] +
    state[["learning_rate"]] * steps[[1L]] * left[["update"]]
  coef_right <- node[["coef"]] +
    state[["learning_rate"]] * steps[[2L]] * right[["update"]]
  # The tree's own value, accumulated exactly as the coefficients are, so it is
  # the model evaluated with the slopes zeroed.
  value_left <- node[["node_value"]] +
    state[["learning_rate"]] * steps[[1L]] * left[["constant"]]
  value_right <- node[["node_value"]] +
    state[["learning_rate"]] * steps[[2L]] * right[["constant"]]
  loss_left <- linad_node_loss(state, coef_left, index_left)
  loss_right <- linad_node_loss(state, coef_right, index_right)

  score <- if (identical(state[["node_selection"]], "global")) {
    # The legacy criterion: the loss over *every* case, with this node's own
    # model extrapolated to the cases it does not contain. It is not the tree's
    # actual loss -- those cases sit in other leaves -- which is why it differs
    # from the local form and why the two are an ablation rather than a
    # refactoring.
    after <- f
    after[index_left] <- drop(
      state[["xm"]][index_left, , drop = FALSE] %*% coef_left
    )
    after[index_right] <- drop(
      state[["xm"]][index_right, , drop = FALSE] %*% coef_right
    )
    all_rows <- seq_len(state[["n"]])
    sum(
      state[["case_weights"]] * linad_loss(state[["y"]], f, state[["type"]])
    ) -
      sum(
        state[["case_weights"]][all_rows] *
          linad_loss(state[["y"]], after, state[["type"]])
      )
  } else {
    # Algorithm 1 line 9, without its stray leading minus.
    node[["loss"]] - (loss_left + loss_right)
  }

  list(
    split = split,
    score = score,
    left = list(
      index = index_left,
      weights = weights_left,
      coef = coef_left,
      node_value = value_left,
      depth = left[["depth"]],
      loss = loss_left
    ),
    right = list(
      index = index_right,
      weights = weights_right,
      coef = coef_right,
      node_value = value_right,
      depth = right[["depth"]],
      loss = loss_right
    )
  )
} # /rtemis::linad_propose


# %% linad_expand ----
#' Propose a split for one node, over a sample of the features
#'
#' `ExpandNode` of Algorithm 1, with `mtry_split` drawn once per node.
#'
#' A node is expanded once and its proposal cached, so a feature sample that
#' finds nothing worth splitting on would close that node for good -- an
#' artifact of the caching rather than a property of the method, and invisible
#' in the output. One retry over every feature keeps `mtry_split` a choice of
#' *which* split is made and never of *whether* the node can split at all: a
#' node closes only where no feature improves the loss.
#'
#' The retry costs a second expansion, child model fits included, and only at
#' nodes whose sample found nothing -- terminal nodes, mostly, where the full
#' search is about to find nothing either.
#'
#' @param state List: Fit state.
#' @param node List: The node to expand.
#'
#' @return List describing the proposal, or NULL if the node cannot be split.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_expand <- function(state, node) {
  features <- linad_sample_features(
    state[["context"]],
    state[["mtry_split"]]
  )
  proposal <- linad_propose(state, node, features)
  if (!is.null(features) && !linad_improves(proposal)) {
    proposal <- linad_propose(state, node, NULL)
  }
  proposal
} # /rtemis::linad_expand


# %% linad_steps ----
#' Resolve the line-search step for a pair of children
#'
#' `line_search` decides the scope, which is one of the places the manuscript and
#' the implementation it was written from disagree. Equation 27 and Algorithm 1
#' line 30 estimate a single step over the whole expansion; the original code
#' estimates one per child. Both are available so the question can be settled on
#' data rather than by reading.
#'
#' **The step is estimated over every case, weighted by the soft membership
#' weights**, not over the node's own cases. That is Eq 15's expectation, and it
#' is the channel through which `gamma` reaches a regression fit: a node whose
#' cases suggest a large step is pulled back toward one the rest of the data can
#' also live with, in proportion to how much weight `gamma` leaves them. Scored
#' over the node's own cases instead, `gamma` would only ever change which split
#' is chosen, and would be very nearly inert.
#'
#' @param state List: Fit state.
#' @param node List: The node being expanded.
#' @param f Numeric vector: Parent's function value.
#' @param goes_left Logical vector: Which cases the split sends left.
#' @param left List: `linad_child()` output for the left child.
#' @param right List: `linad_child()` output for the right child.
#'
#' @return Numeric vector of length 2: the left and right steps.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_steps <- function(state, node, f, goes_left, left, right) {
  if (identical(state[["line_search"]], "none")) {
    return(c(1, 1))
  }
  all_rows <- seq_len(state[["n"]])
  if (identical(state[["line_search"]], "child")) {
    return(c(
      linad_line_search(
        state[["y"]],
        f,
        left[["v"]],
        left[["weights"]],
        all_rows,
        state[["type"]],
        state[["line_search_max"]]
      ),
      linad_line_search(
        state[["y"]],
        f,
        right[["v"]],
        right[["weights"]],
        all_rows,
        state[["type"]],
        state[["line_search_max"]]
      )
    ))
  }
  # "expansion": one step over both children's combined update direction, each
  # case taking the direction of the side the split sends it to.
  combined <- numeric(state[["n"]])
  combined[goes_left] <- left[["v"]][goes_left]
  combined[!goes_left] <- right[["v"]][!goes_left]
  step <- linad_line_search(
    state[["y"]],
    f,
    combined,
    node[["weights"]],
    all_rows,
    state[["type"]],
    state[["line_search_max"]]
  )
  c(step, step)
} # /rtemis::linad_steps


# %% linad_fit ----
#' Grow a linear additive tree
#'
#' Algorithm 1. The engine proper: takes numeric inputs and hyperparameter
#' values, returns a plain description of the tree. It knows nothing about S7,
#' preprocessing or outcome types beyond `type`, which is what makes it
#' testable on its own.
#'
#' A node's function value is never stored. It is always recomputed as
#' `xm %*% coef`, so the fitted values and the coefficients a prediction will
#' use are the same object by construction rather than by agreement -- the
#' legacy implementation kept them separately and they diverged for
#' classification.
#'
#' @param x data.frame: Features, unencoded, used for splits.
#' @param xm Numeric matrix: Intercept-augmented design matrix, used for models.
#' @param y Numeric vector: Outcome; `{-1, +1}` for classification.
#' @param case_weights Numeric vector: Case weights, rescaled to average 1.
#' @param type Character: "Regression" or "Classification".
#' @param settings List: Hyperparameter values, as named in `setup_LINAD`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `frame`, `coefficients`, `steps` and `n_leaves`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_fit <- function(x, xm, y, case_weights, type, settings, verbosity = 1L) {
  n <- nrow(xm)
  # Growth runs in standardized space so one `lambda` means the same thing at
  # every node; the finished coefficients are mapped back before they are
  # stored, so nothing downstream sees the standardized scale.
  scaling <- linad_scaling(xm)
  xm_scaled <- sweep(
    sweep(xm, 2L, scaling[["center"]], "-"),
    2L,
    scaling[["scale"]],
    "/"
  )
  state <- c(
    settings,
    list(
      x = x,
      xm = xm_scaled,
      y = y,
      n = n,
      case_weights = case_weights,
      type = type,
      context = linad_context(
        x,
        settings[["split_binning"]],
        settings[["split_bin_type"]]
      )
    )
  )

  # Root ----
  # Algorithm 1 line 3: a regularized linear model of y itself, on every case,
  # with uniform weights. Its intercept is the model's initialization, so there
  # is no separate init term to keep in step with the coefficients.
  # The root fits `y` itself by least squares, in both outcome types -- it is an
  # initialization, not a boosting step, so there is no gradient yet to fit. Its
  # constant is therefore Eq 3 for a regression (the weighted mean) and the
  # least-squares level for a classification, and `root_learning_rate` shrinks
  # only the slopes around it.
  #
  # That separation is the whole point: nothing special-cases the intercept any
  # more. Shrinking the slopes cannot disturb the level, which is what used to
  # drag a shrunk root towards predicting zero.
  # At a rate of 0 the root's slopes are discarded whatever they are, so the fit
  # is skipped rather than computed and thrown away.
  root_fit <- if (settings[["root_learning_rate"]] == 0) {
    NULL
  } else {
    linad_solve(
      xm_scaled,
      y,
      case_weights,
      seq_len(n),
      settings[["root_model"]],
      settings[["root_lambda"]],
      settings[["root_alpha"]],
      settings[["root_nvmax"]],
      forward_stop = settings[["forward_stop"]],
      derivatives = NULL,
      type = "Regression",
      max_step = settings[["line_search_max"]],
      node_test = settings[["node_test"]]
    )
  }
  root_constant <- if (is.null(root_fit)) {
    linad_baseline(y, case_weights, type)
  } else {
    root_fit[["constant"]]
  }
  root_slopes <- if (is.null(root_fit)) {
    rep(0, ncol(xm_scaled) - 1L)
  } else {
    root_fit[["coefficients"]][-1L]
  }
  if (!all(is.finite(root_slopes))) {
    root_slopes <- rep(0, ncol(xm_scaled) - 1L)
  }
  root_slopes <- settings[["root_learning_rate"]] * root_slopes
  root_center <- if (ncol(xm_scaled) > 1L) {
    drop(crossprod(case_weights, xm_scaled[, -1L, drop = FALSE])) /
      sum(case_weights)
  } else {
    numeric(0)
  }
  root_coef <- c(root_constant - sum(root_center * root_slopes), root_slopes)
  baseline <- root_constant

  nodes <- list(list(
    id = 1L,
    parent = NA_integer_,
    index = seq_len(n),
    weights = case_weights,
    coef = root_coef,
    node_value = baseline,
    depth = 0L,
    loss = 0,
    split = NULL,
    left = NA_integer_,
    right = NA_integer_,
    proposal = NULL
  ))
  nodes[[1L]][["loss"]] <- linad_node_loss(state, root_coef, seq_len(n))
  leaves <- 1L
  steps <- list(1L)

  if (settings[["max_leaves"]] > 1L) {
    nodes[[1L]][["proposal"]] <- linad_expand(state, nodes[[1L]])
    candidates <- if (is.null(nodes[[1L]][["proposal"]])) integer(0) else 1L

    # Tree-growing ----
    while (
      length(leaves) < settings[["max_leaves"]] && length(candidates) > 0L
    ) {
      scores <- vapply(
        candidates,
        function(id) nodes[[id]][["proposal"]][["score"]],
        numeric(1L)
      )
      best <- which.max(scores)
      if (!is.finite(scores[[best]]) || scores[[best]] <= 0) {
        # No frontier node can still reduce the loss. `linad_improves()` is the
        # same test, applied per proposal.
        break
      }
      chosen <- candidates[[best]]
      proposal <- nodes[[chosen]][["proposal"]]
      child_ids <- length(nodes) + seq_len(2L)
      for (side in seq_len(2L)) {
        child <- proposal[[c("left", "right")[[side]]]]
        nodes[[child_ids[[side]]]] <- list(
          id = child_ids[[side]],
          parent = chosen,
          index = child[["index"]],
          weights = child[["weights"]],
          coef = child[["coef"]],
          node_value = child[["node_value"]],
          depth = child[["depth"]],
          loss = child[["loss"]],
          split = NULL,
          left = NA_integer_,
          right = NA_integer_,
          proposal = NULL
        )
      }
      nodes[[chosen]][["split"]] <- proposal[["split"]]
      nodes[[chosen]][["left"]] <- child_ids[[1L]]
      nodes[[chosen]][["right"]] <- child_ids[[2L]]
      nodes[[chosen]][["proposal"]] <- NULL
      # An internal node's weights are only ever read while it sits on the
      # frontier, so dropping them here bounds memory by the frontier size
      # rather than by the node count.
      nodes[[chosen]][["weights"]] <- NULL
      leaves <- c(setdiff(leaves, chosen), child_ids)
      candidates <- setdiff(candidates, chosen)
      for (id in child_ids) {
        nodes[[id]][["proposal"]] <- linad_expand(state, nodes[[id]])
        if (!is.null(nodes[[id]][["proposal"]])) {
          candidates <- c(candidates, id)
        }
      }
      steps[[length(leaves)]] <- leaves
      if (verbosity > 1L) {
        dbg(
          "LINAD: ",
          length(leaves),
          " leaves, split node ",
          chosen,
          " on ",
          proposal[["split"]][["feature"]]
        )
      }
    }
  }

  fitted <- linad_frame(nodes, leaves, steps, colnames(xm))
  fitted[["coefficients"]] <- linad_unscale(fitted[["coefficients"]], scaling)
  fitted
} # /rtemis::linad_fit


# %% linad_frame ----
#' Flatten a grown tree into a frame and a coefficient matrix
#'
#' One row per node and one coefficient row per node, in place of a nested
#' structure: routing is then a vectorized pass per internal node, and the whole
#' object serializes with no external references.
#'
#' @param nodes List: Grown nodes.
#' @param leaves Integer vector: Terminal node ids.
#' @param steps List: Terminal sets by tree size.
#' @param design_names Character vector: Design-matrix column names.
#'
#' @return List with `frame`, `coefficients`, `steps` and `n_leaves`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_frame <- function(nodes, leaves, steps, design_names) {
  n_nodes <- length(nodes)
  ids <- seq_len(n_nodes)
  split_of <- function(field, empty) {
    vapply(
      nodes,
      function(node) {
        if (is.null(node[["split"]])) empty else node[["split"]][[field]]
      },
      empty
    )
  }
  frame <- data.table::data.table(
    node = ids,
    parent = vapply(nodes, function(node) node[["parent"]], integer(1L)),
    left = vapply(nodes, function(node) node[["left"]], integer(1L)),
    right = vapply(nodes, function(node) node[["right"]], integer(1L)),
    is_leaf = ids %in% leaves,
    depth = vapply(nodes, function(node) node[["depth"]], integer(1L)),
    n = vapply(nodes, function(node) length(node[["index"]]), integer(1L)),
    # What the tree alone predicts here, with the slopes zeroed. The root's is
    # Eq 3's constant, and a child's is its parent's plus the shrunk,
    # line-searched constant of Eq 19/20.
    node_value = vapply(
      nodes,
      function(node) node[["node_value"]],
      numeric(1L)
    ),
    loss = vapply(nodes, function(node) node[["loss"]], numeric(1L)),
    split_feature = split_of("feature", NA_character_),
    split_kind = split_of("kind", NA_character_),
    split_value = split_of("value", NA_real_)
  )
  frame[["split_levels"]] <- lapply(nodes, function(node) {
    if (is.null(node[["split"]])) NULL else node[["split"]][["levels"]]
  })
  coefficients <- do.call(rbind, lapply(nodes, function(node) node[["coef"]]))
  colnames(coefficients) <- design_names
  rownames(coefficients) <- NULL
  list(
    frame = frame,
    coefficients = coefficients,
    steps = steps,
    n_leaves = length(leaves)
  )
} # /rtemis::linad_frame


# %% linad_check_tree ----
#' Structural invariants of a grown tree
#'
#' Everything a fitted `LinearAdditiveTree` must satisfy whatever the
#' hyperparameters, checked against the frame it already stores: that each split
#' partitions its node, that no leaf merely repeats its parent, that the numbers
#' are finite, that the leaf flags match the selected size, and that the
#' sequence of tree sizes is unbroken.
#'
#' Returns the violations rather than raising, so a caller can assert on it in a
#' test or report it across a benchmark. Cheap enough to run on every fit.
#'
#' These are properties an accuracy measurement cannot see: a tree that wastes
#' leaves is a worse number, not a visible fault.
#'
#' @param model `LinearAdditiveTree` object.
#'
#' @param min_cases_child Optional Integer: The floor a split had to leave on
#' each side. Supplied by a caller that knows the hyperparameters, since the
#' fitted tree does not carry them.
#'
#' @return Character vector of violations, empty when the tree is sound.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_check_tree <- function(model, min_cases_child = NULL) {
  problems <- character()
  frame <- model@frame
  node <- frame[["node"]]
  terminal <- model@steps[[model@n_leaves]]
  leaf_rows <- match(terminal, node)
  internal <- which(!is.na(frame[["left"]]))

  # No node smaller than the floor its split was required to leave. A rule that
  # moves that floor without saying so spends the leaf budget on nodes too small
  # to carry a model, which no accuracy measurement reliably shows.
  if (!is.null(min_cases_child)) {
    undersized <- node[frame[["n"]] < min_cases_child]
    if (length(undersized) > 0L) {
      problems <- c(
        problems,
        paste0(
          "node(s) ",
          paste(undersized, collapse = ", "),
          " hold fewer than the ",
          min_cases_child,
          " cases a split had to leave"
        )
      )
    }
  }

  # A split partitions its node: nothing is lost or double counted.
  for (row in internal) {
    children <- match(
      c(frame[["left"]][[row]], frame[["right"]][[row]]),
      node
    )
    if (anyNA(children)) {
      problems <- c(
        problems,
        paste0("node ", node[[row]], " names a child that is not in the frame")
      )
      next
    }
    if (frame[["n"]][[row]] != sum(frame[["n"]][children])) {
      problems <- c(
        problems,
        paste0(
          "node ",
          node[[row]],
          " holds ",
          frame[["n"]][[row]],
          " cases but its children hold ",
          sum(frame[["n"]][children])
        )
      )
    }
  }

  # A leaf that predicts exactly what its parent did spent one of `max_leaves`
  # to change nothing.
  for (i in seq_along(terminal)) {
    parent <- frame[["parent"]][[leaf_rows[[i]]]]
    if (is.na(parent)) {
      next
    }
    parent_row <- match(parent, node)
    same_coefficients <- isTRUE(all.equal(
      model@coefficients[leaf_rows[[i]], ],
      model@coefficients[parent_row, ]
    ))
    same_value <- isTRUE(all.equal(
      frame[["node_value"]][[leaf_rows[[i]]]],
      frame[["node_value"]][[parent_row]]
    ))
    if (same_coefficients && same_value) {
      problems <- c(
        problems,
        paste0(
          "leaf ",
          terminal[[i]],
          " (n = ",
          frame[["n"]][[leaf_rows[[i]]]],
          ") is identical to its parent ",
          parent
        )
      )
    }
  }

  # A model whose numbers are not numbers predicts nothing.
  if (!all(is.finite(model@coefficients))) {
    problems <- c(problems, "coefficients hold non-finite values")
  }
  if (any(!is.finite(frame[["loss"]]))) {
    problems <- c(problems, "frame holds a non-finite loss")
  }

  # The leaf flags describe the tree at its selected size, which is what every
  # later reader assumes.
  flagged <- node[frame[["is_leaf"]]]
  if (!setequal(flagged, terminal)) {
    problems <- c(
      problems,
      "is_leaf disagrees with the terminal set at the selected size"
    )
  }

  # Growing to k leaves passes through every smaller size, which is what makes
  # selecting a size on held-out data meaningful.
  sizes <- lengths(model@steps)
  if (!identical(sizes, seq_along(model@steps))) {
    problems <- c(problems, "the sequence of tree sizes is not 1, 2, ... k")
  }
  problems
} # /rtemis::linad_check_tree


# %% linad_route ----
#' Route cases to their terminal node
#'
#' Algorithm 2. Descends the frame one internal node at a time, splitting the
#' set of cases that reached it, rather than walking the tree once per case.
#' Instance weights and `gamma` play no part: soft weighting is a training
#' device, and prediction follows hard splits.
#'
#' @param frame data.table: The tree frame.
#' @param x data.frame: Cases, unencoded.
#' @param terminal Integer vector: Node ids to stop at, which is how a tree
#' grown to `max_leaves` is evaluated at a smaller size.
#'
#' @return Integer vector of terminal node ids, one per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_route <- function(frame, x, terminal) {
  current <- rep(1L, nrow(x))
  settled <- frame[["node"]] %in% terminal | is.na(frame[["left"]])
  repeat {
    pending <- which(!settled[current])
    if (length(pending) == 0L) {
      break
    }
    for (id in unique(current[pending])) {
      rows <- pending[current[pending] == id]
      goes_left <- linad_goes_left(
        list(
          feature = frame[["split_feature"]][[id]],
          kind = frame[["split_kind"]][[id]],
          value = frame[["split_value"]][[id]],
          levels = frame[["split_levels"]][[id]]
        ),
        x[rows, , drop = FALSE]
      )
      current[rows] <- ifelse(
        goes_left,
        frame[["left"]][[id]],
        frame[["right"]][[id]]
      )
    }
  }
  current
} # /rtemis::linad_route


# %% linad_raw_prediction ----
#' Function value for new cases
#'
#' The leaf's coefficients already carry the sum along its path, so prediction
#' is one dot product per case and the whole set is one vectorized pass.
#'
#' @param model `LinearAdditiveTree` object.
#' @param x data.frame: Cases, unencoded, for routing.
#' @param xm Numeric matrix: The same cases, encoded, for the leaf models.
#' @param n_leaves Optional Integer: Evaluate the tree at this size. NULL uses
#' the size selected at training.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_raw_prediction <- function(model, x, xm, n_leaves = NULL) {
  if (is.null(n_leaves)) {
    n_leaves <- model@n_leaves
  }
  leaf <- linad_route(model@frame, x, model@steps[[n_leaves]])
  rowSums(xm * model@coefficients[leaf, , drop = FALSE])
} # /rtemis::linad_raw_prediction


# %% linad_select_leaves ----
#' Choose the tree size that minimizes validation loss
#'
#' The manuscript's leaf-count selection: score the tree at every size it passed
#' through and take the argmin, "equivalent to choosing the number of trees in
#' gradient boosting based on validation error". Smoothing the curve first trades
#' a little optimism for a lot of stability when the validation set is small.
#'
#' @param model `LinearAdditiveTree` object.
#' @param x data.frame: Validation features, unencoded.
#' @param xm Numeric matrix: Validation features, encoded.
#' @param y Numeric vector: Validation outcome, on the training scale.
#' @param type Character: "Regression" or "Classification".
#' @param smooth Logical: Smooth the curve before taking the argmin.
#'
#' @return List with the selected `n_leaves` and the `curve` it was read from.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_select_leaves <- function(model, x, xm, y, type, smooth = FALSE) {
  sizes <- seq_along(model@steps)
  curve <- vapply(
    sizes,
    function(k) {
      f <- linad_raw_prediction(model, x, xm, n_leaves = k)
      mean(linad_loss(y, f, type))
    },
    numeric(1L)
  )
  scored <- curve
  if (smooth && length(sizes) >= 4L) {
    smoothed <- tryCatch(
      stats::loess(curve ~ sizes)[["fitted"]],
      error = function(e) NULL
    )
    if (!is.null(smoothed)) {
      scored <- smoothed
    }
  }
  list(n_leaves = sizes[[which.min(scored)]], curve = curve)
} # /rtemis::linad_select_leaves


# %% linad_design_matrix ----
#' Intercept-augmented design matrix for the leaf models
#'
#' Reference coding, not one-hot: an intercept plus a full set of level
#' indicators is rank deficient, which leaves the leaf coefficients
#' unidentifiable -- unacceptable for a model whose coefficients are the thing a
#' reader is meant to look at.
#'
#' LINAD is therefore one of the algorithms that keeps its features unencoded
#' through the pipeline and encodes here, rather than returning a `Preprocessor`
#' from `train_()`. It needs both forms at once: splits are searched and routed
#' on the original features, so a factor splits on a set of levels, while the
#' models need numbers. A one-hot `Preprocessor` would deliver only the encoded
#' frame to `predict_super()` and the level sets would have nothing to route on.
#'
#' Passing `xlev` forces the training levels, so the columns line up even when
#' new data happens not to contain one.
#'
#' @param x data.frame: Features.
#' @param xlev Optional named list: Factor levels recorded at training.
#'
#' @return Numeric matrix with `(Intercept)` as its first column.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_design_matrix <- function(x, xlev = NULL) {
  x <- as.data.frame(x)
  if (!is.null(xlev)) {
    for (feature in names(xlev)) {
      values <- factor(as.character(x[[feature]]), levels = xlev[[feature]])
      if (anyNA(values)) {
        rtemis.core::abort(
          "Feature '",
          feature,
          "' contains levels not seen during training: ",
          paste(
            setdiff(unique(as.character(x[[feature]])), xlev[[feature]]),
            collapse = ", "
          ),
          ".",
          class = c("rtemis_value_error", "rtemis_data_error")
        )
      }
      x[[feature]] <- values
    }
  }
  stats::model.matrix(~., data = x)
} # /rtemis::linad_design_matrix


# %% linad_scaling ----
#' Centering and scaling for the design matrix
#'
#' One `lambda` is shared by every node, so it only means the same thing at each
#' of them if the columns are on a common scale. Growth runs in standardized
#' space and `linad_unscale()` maps the finished coefficients back, so what the
#' model stores -- and what a reader sees -- is on the features' own scale.
#'
#' Forward selection needs none of this: its gain `g_j^2 / s_j` scales as
#' `c^2 / c^2` in a column rescaled by `c`, so the selected set is unchanged.
#' Ridge and the elastic net do need it.
#'
#' @param xm Numeric matrix: Design matrix, intercept first.
#'
#' @return List with `center` and `scale`, both length `ncol(xm)` and both
#' neutral at the intercept.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_scaling <- function(xm) {
  d <- ncol(xm)
  center <- rep(0, d)
  scale <- rep(1, d)
  if (d > 1L) {
    columns <- seq.int(2L, d)
    center[columns] <- colMeans(xm[, columns, drop = FALSE])
    spread <- apply(xm[, columns, drop = FALSE], 2L, stats::sd)
    spread[!is.finite(spread) | spread <= 0] <- 1
    scale[columns] <- spread
  }
  list(center = center, scale = scale)
} # /rtemis::linad_scaling


# %% linad_unscale ----
#' Map coefficients from standardized space back to the features' own scale
#'
#' For `z_j = (x_j - m_j) / s_j`, `sum_j b_j z_j + b_0` equals
#' `sum_j (b_j / s_j) x_j + (b_0 - sum_j b_j m_j / s_j)`. Applied row-wise, so
#' every node's coefficients come back together.
#'
#' @param coefficients Numeric matrix: One row per node, standardized space.
#' @param scaling List: `linad_scaling()` output.
#'
#' @return Numeric matrix of the same shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_unscale <- function(coefficients, scaling) {
  center <- scaling[["center"]]
  scale <- scaling[["scale"]]
  rescaled <- sweep(coefficients, 2L, scale, "/")
  shift <- drop(rescaled %*% center)
  rescaled[, 1L] <- coefficients[, 1L] - shift
  rescaled
} # /rtemis::linad_unscale


# %% linad_settings ----
#' Resolve hyperparameters into the values the engine runs on
#'
#' Optional hyperparameters are NULL until here. Two kinds of NULL are resolved:
#' a parameter left unset takes its default, and a `first_*` parameter left
#' unset inherits the node-level value -- so `setup_LINAD(node_model =
#' "constant")` gives a tree with constant nodes *including its root*, rather
#' than an Additive Tree with one stray linear model at the top.
#'
#' Resolved values are deliberately not written back into the `Hyperparameters`
#' object. A gated property must be NULL when its gate is shut, so writing
#' `nvmax` back into a ridge fit would produce an object its own validator
#' rejects.
#'
#' @param hyperparameters `LINADHyperparameters` object.
#'
#' @return Named list of engine settings.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_settings <- function(hyperparameters) {
  value_or <- function(name, fallback) {
    value <- hyperparameters[[name]]
    if (is.null(value)) fallback else value
  }
  node_model <- hyperparameters[["node_model"]]
  nvmax <- value_or("nvmax", 3L)
  lambda <- value_or("lambda", 0.05)
  alpha <- value_or("alpha", 1)
  list(
    max_leaves = hyperparameters[["max_leaves"]],
    force_max_leaves = hyperparameters[["force_max_leaves"]],
    smooth_validation_curve = value_or("smooth_validation_curve", FALSE),
    min_cases_split = hyperparameters[["min_cases_split"]],
    min_cases_leaf = hyperparameters[["min_cases_leaf"]],
    min_cases_node_model = value_or("min_cases_node_model", 10L),
    node_model = node_model,
    nvmax = nvmax,
    lambda = lambda,
    alpha = alpha,
    learning_rate = hyperparameters[["learning_rate"]],
    root_model = value_or("root_model", node_model),
    root_nvmax = value_or("root_nvmax", nvmax),
    root_lambda = value_or("root_lambda", lambda),
    root_alpha = value_or("root_alpha", alpha),
    root_learning_rate = hyperparameters[["root_learning_rate"]],
    forward_stop = value_or("forward_stop", "bic"),
    node_test = value_or("node_test", "none"),
    split_search = hyperparameters[["split_search"]],
    split_criterion = value_or("split_criterion", "mean"),
    # A single tree scans every feature at every split; `LINADForest` overrides
    # this in `linadforest_settings()`, which is the only caller that samples.
    mtry_split = NULL,
    split_binning = hyperparameters[["split_binning"]],
    split_bin_type = hyperparameters[["split_bin_type"]],
    n_cuts = value_or("n_cuts", 20L),
    gamma = hyperparameters[["gamma"]],
    line_search = hyperparameters[["line_search"]],
    line_search_max = hyperparameters[["line_search_max"]],
    node_selection = hyperparameters[["node_selection"]],
    constant_rule = hyperparameters[["constant_rule"]]
  )
} # /rtemis::linad_settings


# %% linad_gram_solve ----
#' Solve a leaf model from sufficient statistics alone
#'
#' The Gram path of `linad_solve()`, split out because the exhaustive split
#' search needs to fit thousands of candidate models without ever touching the
#' rows again.
#'
#' The elastic net has no Gram form, so a search under it is scored with a ridge
#' of the same `lambda`. The chosen split is then fitted with the real leaf
#' model, so this affects which split wins, not what is fitted at it.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param Xty Numeric vector: `X'Wy`.
#' @param sw Numeric: Sum of weights.
#' @param node_model Character: Leaf model name.
#' @param lambda Numeric: Ridge penalty.
#' @param nvmax Integer: Forward-selection term count.
#' @param node_test Character: Cost a side's slopes must earn over the constant
#' alone, as `linad_node_test()` applies it.
#'
#' @return Numeric vector of coefficients, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_gram_solve <- function(
  G,
  Xty,
  sw,
  node_model,
  lambda,
  nvmax,
  syy = NULL,
  forward_stop = "none",
  node_test = "none"
) {
  if (G[[1L, 1L]] <= 0) {
    return(NULL)
  }
  constant_only <- rep(0, ncol(G))
  constant_only[[1L]] <- Xty[[1L]] / G[[1L, 1L]]
  if (identical(node_model, "constant")) {
    return(constant_only)
  }
  coefficients <- if (identical(node_model, "forward")) {
    linad_forward(
      G,
      Xty,
      nvmax,
      penalty = lambda * sw,
      syy = syy,
      sample_weight = sw,
      stop_rule = forward_stop
    )
  } else {
    linad_chol_solve(G, Xty, lambda * sw)
  }
  if (identical(node_test, "none") || is.null(coefficients) || is.null(syy)) {
    return(coefficients)
  }
  n_terms <- if (identical(node_model, "forward")) {
    sum(coefficients[-1L] != 0)
  } else {
    linad_ridge_edf(G, lambda * sw) - 1
  }
  keep <- linad_node_test(
    linad_gram_loss(G, Xty, syy, constant_only),
    linad_gram_loss(G, Xty, syy, coefficients),
    sw,
    n_terms,
    node_test
  )
  if (keep) coefficients else constant_only
} # /rtemis::linad_gram_solve


# %% linad_gram_loss ----
#' Weighted residual sum of squares implied by a Gram and a coefficient vector
#'
#' `sum_i w_i (y_i - x_i'b)^2 = syy - 2 b'Xty + b'Gb`, so the loss of a
#' candidate split costs no data access at all.
#'
#' @param G Numeric matrix: `X'WX`.
#' @param Xty Numeric vector: `X'Wy`.
#' @param syy Numeric: `sum(w * y^2)`.
#' @param b Numeric vector: Coefficients.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_gram_loss <- function(G, Xty, syy, b) {
  syy - 2 * sum(b * Xty) + sum(b * drop(G %*% b))
} # /rtemis::linad_gram_loss


# %% linad_sweep ----
#' Split search scored by the loss after fitting both child models
#'
#' Section 5.1's second strategy: rather than taking the best squared-error
#' split of the gradient and fitting models afterwards, score every candidate
#' split by what the two child models actually achieve. The manuscript describes
#' it, notes it "significantly increases the computational demands", and leaves
#' it out of the experiments.
#'
#' It is affordable here because the sweep is incremental. Ordering the rows by
#' a feature once and accumulating `X'WX`, `X'Wy` and `sum(w y^2)` in blocks
#' between consecutive cut points yields every candidate's left-hand sufficient
#' statistics in a single pass; the right-hand ones are the node totals minus
#' them. Each candidate then costs one small solve instead of two fits over the
#' data, which takes the cost per feature from `O(k(n d^2 + d^3))` to
#' `O(n d^2 + k d^3)`.
#'
#' Soft weighting survives the same trick. Writing `G_L` and `G_R` for the two
#' hard-side Grams, the gamma-weighted ones are `G_L + gamma * G_R` and
#' `gamma * G_L + G_R`, both of which the sweep already has.
#'
#' @param state List: Fit state.
#' @param r Numeric vector: Negative gradient.
#' @param w Numeric vector: Node weights.
#' @param member Logical vector: Node membership.
#' @param features Optional List: Which features to scan, as
#' `linad_scan_features()` describes. NULL scans every feature.
#'
#' @return A split, shaped as `linad_stump()` returns one, with `gain` the
#' negated loss so the caller compares the two the same way.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_sweep <- function(state, r, w, member, features = NULL) {
  context <- state[["context"]]
  xm <- state[["xm"]]
  gamma <- state[["gamma"]]
  best <- list(gain = -Inf)
  scan <- linad_scan_features(context, features)
  min_cases_child <- linad_min_child_cases(state)

  # The node's own sufficient statistics, which every candidate's right-hand
  # side is read off by subtraction. They do not depend on the feature, so they
  # are formed once per node rather than once per feature.
  weights <- w
  total <- linad_gram(xm, r, weights, NULL)

  # One candidate, scored by the loss after fitting both child models. Every
  # search path below reaches a split through this, so a candidate is never
  # scored by a model the commit would not fit.
  score_split <- function(left, left_members, total_members) {
    if (
      left_members < min_cases_child ||
        (total_members - left_members) < min_cases_child
    ) {
      return(Inf)
    }
    right_G <- total[["G"]] - left[["G"]]
    right_Xty <- total[["Xty"]] - left[["Xty"]]
    right_syy <- total[["syy"]] - left[["syy"]]
    # A case carries `gamma` of its weight into the branch it does not belong
    # to, so each side's statistics are a mixture of the two hard sides -- both
    # of which the caller already holds. At `gamma = 0` the mixture collapses to
    # the hard sides themselves.
    mix <- gamma
    sides <- list(
      list(
        G = left[["G"]] + mix * right_G,
        Xty = left[["Xty"]] + mix * right_Xty,
        syy = left[["syy"]] + mix * right_syy
      ),
      list(
        G = mix * left[["G"]] + right_G,
        Xty = mix * left[["Xty"]] + right_Xty,
        syy = mix * left[["syy"]] + right_syy
      )
    )
    loss <- 0
    for (side in sides) {
      sw <- side[["G"]][[1L, 1L]]
      if (sw <= 0) {
        return(Inf)
      }
      b <- linad_gram_solve(
        side[["G"]],
        side[["Xty"]],
        sw,
        state[["node_model"]],
        state[["lambda"]],
        state[["nvmax"]],
        syy = side[["syy"]],
        forward_stop = state[["forward_stop"]],
        node_test = state[["node_test"]]
      )
      if (is.null(b) || !all(is.finite(b))) {
        return(Inf)
      }
      loss <- loss +
        linad_gram_loss(side[["G"]], side[["Xty"]], side[["syy"]], b)
    }
    loss
  }

  score_cuts <- function(order_index, cut_positions, member_counts) {
    # Sufficient statistics accumulated left-to-right in blocks, one block per
    # candidate cut, so each row is visited once per feature rather than once
    # per cut.
    d <- ncol(xm)
    running_G <- matrix(0, d, d)
    running_Xty <- numeric(d)
    running_syy <- 0
    from <- 1L
    losses <- rep(Inf, length(cut_positions))
    total_members <- member_counts[[length(member_counts)]]
    for (k in seq_along(cut_positions)) {
      rows <- order_index[seq.int(from, cut_positions[[k]])]
      from <- cut_positions[[k]] + 1L
      block <- linad_gram(xm, r, weights, rows)
      running_G <- running_G + block[["G"]]
      running_Xty <- running_Xty + block[["Xty"]]
      running_syy <- running_syy + block[["syy"]]
      losses[[k]] <- score_split(
        list(G = running_G, Xty = running_Xty, syy = running_syy),
        member_counts[[k]],
        total_members
      )
    }
    losses
  }

  # Numeric features ----
  for (j in scan[["numeric"]]) {
    order_index <- context[["numeric_order"]][[j]]
    sorted <- context[["numeric_matrix"]][order_index, j]
    breaks <- context[["numeric_breaks"]][[j]]
    if (length(breaks) == 0L) {
      next
    }
    # Cut points among the admissible ones, so the sweep costs `n_cuts`
    # solves per feature rather than one per distinct value. If `split_binning`
    # already coarsened the feature, this thins what is left, by the same rule.
    wanted <- linad_cut_positions(
      breaks,
      state[["n_cuts"]] - 1L,
      sorted,
      state[["split_bin_type"]]
    )
    wanted <- wanted[wanted >= 1L & wanted < context[["n"]]]
    if (length(wanted) == 0L) {
      next
    }
    member_counts <- cumsum(as.numeric(member)[order_index])[
      c(wanted, context[["n"]])
    ]
    losses <- score_cuts(order_index, wanted, member_counts)
    k <- which.min(losses)
    if (
      length(k) == 1L && is.finite(losses[[k]]) && -losses[[k]] > best[["gain"]]
    ) {
      position <- wanted[[k]]
      best <- list(
        gain = -losses[[k]],
        feature = context[["numeric_names"]][[j]],
        kind = "numeric",
        value = (sorted[[position]] + sorted[[position + 1L]]) / 2,
        levels = NULL,
        column = j
      )
    }
  }

  # Factor features ----
  for (j in scan[["factor"]]) {
    codes <- context[["factor_codes"]][[j]]
    levels_j <- context[["factor_levels"]][[j]]
    level_w <- as.vector(rowsum(weights, codes, reorder = TRUE))
    level_wr <- as.vector(rowsum(weights * r, codes, reorder = TRUE))
    present <- sort(unique(codes))
    keep <- level_w > 0
    if (sum(keep) < 2L) {
      next
    }
    present_levels <- present[keep]
    n_partitions <- 2^(length(present_levels) - 1L) - 1L
    # Ordering levels by mean residual makes the best contiguous split of that
    # ordering the best of all partitions -- for squared error of the *mean*.
    # This search scores by child linear models, for which the theorem does not
    # hold, so it enumerates every partition where that costs no more than a
    # numeric feature's candidates and falls back to the ordering above that.
    if (n_partitions <= state[["n_cuts"]]) {
      by_level <- lapply(
        present_levels,
        function(level) linad_gram(xm, r, weights, which(codes == level))
      )
      members_by_level <- vapply(
        present_levels,
        function(level) sum(member[codes == level]),
        numeric(1L)
      )
      total_members <- sum(members_by_level)
      # A mask below 2^(k-1) never holds the last level, so each partition and
      # its complement are enumerated once between them.
      for (mask in seq_len(n_partitions)) {
        left <- bitwAnd(mask, bitwShiftL(1L, seq_along(present_levels) - 1L)) >
          0L
        loss <- score_split(
          list(
            G = Reduce(`+`, lapply(by_level[left], `[[`, "G")),
            Xty = Reduce(`+`, lapply(by_level[left], `[[`, "Xty")),
            syy = sum(vapply(by_level[left], `[[`, numeric(1L), "syy"))
          ),
          sum(members_by_level[left]),
          total_members
        )
        if (is.finite(loss) && -loss > best[["gain"]]) {
          best <- list(
            gain = -loss,
            feature = context[["factor_names"]][[j]],
            kind = "factor",
            value = NA_real_,
            levels = levels_j[present_levels[left]],
            column = j
          )
        }
      }
      next
    }
    ranking <- order(level_wr[keep] / level_w[keep])
    ordered_levels <- present_levels[ranking]
    order_index <- order(match(codes, ordered_levels))
    sizes <- cumsum(as.vector(table(codes)[as.character(ordered_levels)]))
    cuts <- sizes[-length(sizes)]
    if (length(cuts) == 0L) {
      next
    }
    member_counts <- cumsum(as.numeric(member)[order_index])[
      c(cuts, context[["n"]])
    ]
    losses <- score_cuts(order_index, cuts, member_counts)
    k <- which.min(losses)
    if (
      length(k) == 1L && is.finite(losses[[k]]) && -losses[[k]] > best[["gain"]]
    ) {
      best <- list(
        gain = -losses[[k]],
        feature = context[["factor_names"]][[j]],
        kind = "factor",
        value = NA_real_,
        levels = levels_j[ordered_levels[seq_len(k)]],
        column = j
      )
    }
  }

  if (!is.finite(best[["gain"]])) {
    return(NULL)
  }
  best
} # /rtemis::linad_sweep
