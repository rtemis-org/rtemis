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
  d <- ncol(G)
  ridge <- rep(penalty, d)
  if (intercept) {
    ridge[[1L]] <- 0
  }
  # A nugget proportional to the trace keeps a rank-deficient node solvable
  # (a constant column inside a leaf is ordinary, not an error) without
  # perceptibly moving a well-conditioned solve.
  nugget <- LINAD_NUGGET * max(mean(diag(G)), .Machine[["double.eps"]])
  chol_factor <- tryCatch(
    chol(G + diag(ridge + nugget, d)),
    error = function(e) NULL
  )
  if (is.null(chol_factor)) {
    return(NULL)
  }
  drop(backsolve(chol_factor, backsolve(chol_factor, Xty, transpose = TRUE)))
} # /rtemis::linad_chol_solve


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
linad_forward <- function(G, Xty, nvmax, intercept = TRUE) {
  d <- ncol(G)
  coefficients <- rep(0, d)
  active <- if (intercept) 1L else integer(0)
  nugget <- LINAD_NUGGET * max(mean(diag(G)), .Machine[["double.eps"]])
  n_steps <- min(as.integer(nvmax), d - length(active))
  for (step in seq_len(n_steps + 1L)) {
    solved <- if (length(active) > 0L) {
      linad_chol_solve(
        G[active, active, drop = FALSE],
        Xty[active],
        0,
        intercept = intercept
      )
    } else {
      numeric(0)
    }
    if (is.null(solved)) {
      return(NULL)
    }
    coefficients[] <- 0
    coefficients[active] <- solved
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
      chol(G[active, active, drop = FALSE] + diag(nugget, length(active)))
    } else {
      NULL
    }
    gain <- vapply(
      candidates,
      function(j) {
        schur <- if (is.null(chol_active)) {
          G[[j, j]]
        } else {
          projected <- backsolve(chol_active, G[active, j], transpose = TRUE)
          G[[j, j]] - sum(projected^2)
        }
        if (schur <= nugget) {
          return(-Inf)
        }
        gradient[[j]]^2 / schur
      },
      numeric(1L)
    )
    best <- which.max(gain)
    if (length(best) == 0L || !is.finite(gain[[best]]) || gain[[best]] <= 0) {
      break
    }
    active <- c(active, candidates[[best]])
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
  alpha = 1,
  nvmax = 3L,
  derivatives = NULL,
  type = "Regression",
  max_step = 1000,
  constant_rule = "closed_form"
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
      linad_forward(gram[["G"]], gram[["Xty"]], nvmax, intercept = FALSE)
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
#'
#' @return List describing the split, or NULL when no admissible split exists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_stump <- function(context, r, w, member, min_cases_child) {
  n <- context[["n"]]
  wr <- w * r
  membership <- as.numeric(member)
  best <- list(gain = -Inf)

  # Numeric features ----
  for (j in seq_along(context[["numeric_names"]])) {
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
  for (j in seq_along(context[["factor_names"]])) {
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
  enough <- length(idx) >= state[["min_cases_node_model"]] ||
    identical(state[["node_model"]], "constant")
  if (enough && length(idx) > 0L && !is_constant(r[idx])) {
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
      state[["node_model"]],
      state[["lambda"]],
      state[["alpha"]],
      state[["nvmax"]],
      derivatives = derivatives,
      type = state[["type"]],
      max_step = state[["line_search_max"]],
      constant_rule = state[["constant_rule"]]
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


# %% linad_expand ----
#' Propose a split for one node
#'
#' `ExpandNode` of Algorithm 1. Splits the node, fits a leaf model on each side,
#' line-searches the step, and scores the result -- but commits nothing. The
#' growth loop expands every frontier node speculatively and then commits only
#' the best proposal, which is the one-step lookahead of Figure 2 and the reason
#' a LINAD tree's split order is a global argmax rather than a traversal.
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
  f <- drop(state[["xm"]] %*% node[["coef"]])
  derivatives <- linad_gradient(state[["y"]], f, state[["type"]])
  r <- -derivatives[["g"]]
  if (length(node[["index"]]) < state[["min_cases_split"]]) {
    return(NULL)
  }
  member <- logical(state[["n"]])
  member[node[["index"]]] <- TRUE
  split <- if (identical(state[["split_search"]], "exhaustive")) {
    linad_sweep(state, r, node[["weights"]], member)
  } else {
    linad_stump(
      state[["context"]],
      r,
      node[["weights"]],
      member,
      state[["min_cases_leaf"]]
    )
  }
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
      derivatives = NULL,
      type = "Regression",
      max_step = settings[["line_search_max"]]
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
        # No frontier node can still reduce the loss.
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
    split_search = hyperparameters[["split_search"]],
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
#'
#' @return Numeric vector of coefficients, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_gram_solve <- function(G, Xty, sw, node_model, lambda, nvmax) {
  if (identical(node_model, "constant")) {
    coefficients <- rep(0, ncol(G))
    if (G[[1L, 1L]] <= 0) {
      return(NULL)
    }
    coefficients[[1L]] <- Xty[[1L]] / G[[1L, 1L]]
    return(coefficients)
  }
  if (identical(node_model, "forward")) {
    return(linad_forward(G, Xty, nvmax))
  }
  linad_chol_solve(G, Xty, lambda * sw)
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
#'
#' @return A split, shaped as `linad_stump()` returns one, with `gain` the
#' negated loss so the caller compares the two the same way.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_sweep <- function(state, r, w, member) {
  context <- state[["context"]]
  xm <- state[["xm"]]
  gamma <- state[["gamma"]]
  best <- list(gain = -Inf)

  # The node's own sufficient statistics, which every candidate's right-hand
  # side is read off by subtraction. They do not depend on the feature, so they
  # are formed once per node rather than once per feature.
  weights <- w
  total <- linad_gram(xm, r, weights, NULL)

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
    for (k in seq_along(cut_positions)) {
      rows <- order_index[seq.int(from, cut_positions[[k]])]
      from <- cut_positions[[k]] + 1L
      block <- linad_gram(xm, r, weights, rows)
      running_G <- running_G + block[["G"]]
      running_Xty <- running_Xty + block[["Xty"]]
      running_syy <- running_syy + block[["syy"]]
      if (
        member_counts[[k]] < state[["min_cases_leaf"]] ||
          (member_counts[[length(member_counts)]] - member_counts[[k]]) <
            state[["min_cases_leaf"]]
      ) {
        next
      }
      right_G <- total[["G"]] - running_G
      right_Xty <- total[["Xty"]] - running_Xty
      right_syy <- total[["syy"]] - running_syy
      # A case carries `gamma` of its weight into the branch it does not belong
      # to, so each side's statistics are a mixture of the two hard sides --
      # both of which the sweep already holds. At `gamma = 0` the mixture
      # collapses to the hard sides themselves.
      mix <- gamma
      sides <- list(
        list(
          G = running_G + mix * right_G,
          Xty = running_Xty + mix * right_Xty,
          syy = running_syy + mix * right_syy
        ),
        list(
          G = mix * running_G + right_G,
          Xty = mix * running_Xty + right_Xty,
          syy = mix * running_syy + right_syy
        )
      )
      loss <- 0
      for (side in sides) {
        sw <- side[["G"]][[1L, 1L]]
        if (sw <= 0) {
          loss <- Inf
          break
        }
        b <- linad_gram_solve(
          side[["G"]],
          side[["Xty"]],
          sw,
          state[["node_model"]],
          state[["lambda"]],
          state[["nvmax"]]
        )
        if (is.null(b) || !all(is.finite(b))) {
          loss <- Inf
          break
        }
        loss <- loss +
          linad_gram_loss(side[["G"]], side[["Xty"]], side[["syy"]], b)
      }
      losses[[k]] <- loss
    }
    losses
  }

  # Numeric features ----
  for (j in seq_along(context[["numeric_names"]])) {
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
  for (j in seq_along(context[["factor_names"]])) {
    codes <- context[["factor_codes"]][[j]]
    levels_j <- context[["factor_levels"]][[j]]
    level_w <- as.vector(rowsum(weights, codes, reorder = TRUE))
    level_wr <- as.vector(rowsum(weights * r, codes, reorder = TRUE))
    present <- sort(unique(codes))
    keep <- level_w > 0
    if (sum(keep) < 2L) {
      next
    }
    ranking <- order(level_wr[keep] / level_w[keep])
    ordered_levels <- present[keep][ranking]
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
