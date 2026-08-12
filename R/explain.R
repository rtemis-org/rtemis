# explain.R
# ::rtemis::
# 2026- EDG rtemis.org

# Description
# Per-case explanation of a fitted model.
#
# `supervised_features()` carries data through up to three spaces -- the user's
# own columns, the model's (`xnames`), and the fitted backend's encoded columns
# -- and a backend returns contributions in the last of them. Carrying them back
# is a sum: the contribution of a factor is the sum of the contributions of the
# columns encoding it. That preserves additivity exactly; whether it also gives
# the factor's Shapley value as a single player is a second question, answered
# at `aggregate_one_hot()`.
#
# How far back it can be carried depends on what preprocessing ran, so the
# result is *verified against the target space* rather than inferred from which
# options were enabled. A hop that does not land exactly on its target is not
# taken, and the space actually reached is recorded on the explanation.

# %% one_hot_groups ----
#' Columns encoding each one-hot expanded feature
#'
#' Reads the `Preprocessor`'s own record of what it expanded rather than
#' parsing column names: `one_hot()` writes `paste0(feature, "_", level)`, and
#' the levels are what `one_hot_levels` holds.
#'
#' @param cols Character: Column names of the contribution matrix.
#' @param one_hot_levels Named list: Per-feature levels, as recorded by the
#' `Preprocessor`.
#'
#' @return Named list of integer column indices, one entry per feature whose
#' encoding is fully present in `cols`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
one_hot_groups <- function(cols, one_hot_levels) {
  if (is.null(one_hot_levels) || length(one_hot_levels) == 0L) {
    return(list())
  }
  groups <- list()
  claimed <- integer()
  for (feature in names(one_hot_levels)) {
    encoded <- paste0(feature, "_", one_hot_levels[[feature]])
    idx <- match(encoded, cols)
    # A map entry with no matching column is expected, not an error: the map is
    # learned on data that includes the outcome, and is applied to features
    # alone -- the same tolerance `one_hot()` itself has.
    if (anyNA(idx)) {
      next
    }
    # Two features whose encodings collide (`x` with level `a_b` against `x_a`
    # with level `b`) would have one column counted twice, silently inflating
    # both. The encoding cannot distinguish them, so neither can this.
    if (any(idx %in% claimed)) {
      rtemis.core::abort(
        "One-hot encoded column names are ambiguous: '",
        feature,
        "' shares an encoded column with another feature. ",
        "Contributions cannot be attributed unambiguously.",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
    claimed <- c(claimed, idx)
    groups[[feature]] <- idx
  }
  groups
} # /rtemis::one_hot_groups


# %% aggregate_one_hot ----
#' Sum one-hot expanded contributions back into their feature
#'
#' Each group collapses at the position of its first column, which is where
#' `one_hot()` expanded the factor, so the result is in the pre-expansion column
#' order.
#'
#' Two claims, which are not the same and are worth keeping apart. **Additivity
#' is preserved exactly**, always: the totals still reconstruct the prediction,
#' because summing a partition of the columns cannot change their sum. Whether a
#' group's total is also **the Shapley value of that feature as a single
#' player** is exact when the value function has no interaction among the
#' columns being merged -- true of every linear model with a marginal value
#' function, which is every path that reaches this today -- and an approximation
#' when there is one, as for a tree or a kernel estimator over one-hot columns.
#' The second claim needs re-examining before such a path is added.
#'
#' @param phi Numeric matrix: Contributions, one column per encoded feature.
#' @param one_hot_levels Named list: Per-feature levels.
#'
#' @return Numeric matrix with each expanded group summed into one column.
#'
#' @author EDG
#' @keywords internal
#' @noRd
aggregate_one_hot <- function(phi, one_hot_levels) {
  groups <- one_hot_groups(colnames(phi), one_hot_levels)
  if (length(groups) == 0L) {
    return(phi)
  }
  owner <- rep(NA_character_, ncol(phi))
  for (feature in names(groups)) {
    owner[groups[[feature]]] <- feature
  }
  # One output column per ungrouped column, and one per group at its first
  # position.
  keep <- which(is.na(owner) | !duplicated(owner))
  out_names <- ifelse(is.na(owner[keep]), colnames(phi)[keep], owner[keep])
  out <- matrix(
    0,
    nrow = nrow(phi),
    ncol = length(keep),
    dimnames = list(rownames(phi), out_names)
  )
  for (k in seq_along(keep)) {
    feature <- owner[[keep[[k]]]]
    out[, k] <- if (is.na(feature)) {
      phi[, keep[[k]]]
    } else {
      rowSums(phi[, groups[[feature]], drop = FALSE])
    }
  }
  out
} # /rtemis::aggregate_one_hot


# %% shap_aggregate ----
#' Carry contributions back toward the user's own columns
#'
#' Two hops, each taken only if it lands exactly on its target: the backend's
#' encoded columns to `xnames`, undoing the algorithm-internal preprocessor;
#' and `xnames` to the columns the user supplied, undoing theirs.
#'
#' A decomposition stops the second hop. PCA, ICA and NMF are linear and their
#' loadings could carry an attribution back to the inputs, but that is a
#' different operation from summing an expansion, and the manifold methods
#' admit none at all -- so components are where the attribution honestly ends.
#'
#' Verification rather than inference is deliberate. Preprocessing changes the
#' column set in several ways besides one-hot expansion -- constants and named
#' features are dropped, missingness indicators and date features are added --
#' and reasoning about which combination is invertible is a standing
#' opportunity to be wrong. Comparing the aggregated names against the space
#' being claimed cannot be.
#'
#' @param phi Numeric matrix: Contributions in the fitted backend's own column
#' space.
#' @param object `Supervised` object.
#' @param input_names Optional Character: Feature names as the user supplied
#' them. NULL leaves the second hop unverifiable, so it is not taken.
#'
#' @return List with `phi` (the aggregated matrix) and `space` (one of
#' "encoded", "model", "input").
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_aggregate <- function(phi, object, input_names = NULL) {
  internal <- object@preprocessor_internal
  at_model <- if (is.null(internal)) {
    phi
  } else {
    aggregate_one_hot(phi, internal@values[["one_hot_levels"]])
  }
  # The backend's columns must reduce to exactly the model's, in order. Failing
  # here means the internal preprocessor changed the column set in a way this
  # does not model, and the encoded values are then the only honest answer.
  if (!identical(colnames(at_model), object@xnames)) {
    return(list(phi = phi, space = "encoded"))
  }
  if (!is.null(object@decomposition) || is.null(input_names)) {
    return(list(phi = at_model, space = "model"))
  }
  user <- object@preprocessor
  at_input <- if (is.null(user)) {
    at_model
  } else {
    aggregate_one_hot(at_model, user@values[["one_hot_levels"]])
  }
  if (!identical(colnames(at_input), input_names)) {
    return(list(phi = at_model, space = "model"))
  }
  list(phi = at_input, space = "input")
} # /rtemis::shap_aggregate


# %% shap_require_background ----
#' Demand the background an estimator cannot do without
#'
#' Path-dependent estimators take their baseline from the model. Everything that
#' needs `E[x]` or a reference set needs data, and there is nowhere to get it
#' from: a fitted `Supervised` stores `y_training` and `xnames`, not the
#' training features.
#'
#' @param background Optional tabular data.
#' @param estimator Character: Estimator asking for it.
#'
#' @return `background`, invisibly, when present.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_require_background <- function(background, estimator) {
  if (is.null(background)) {
    rtemis.core::abort(
      estimator,
      " needs a background sample: contributions are deviations from ",
      "E[f(x)] over one, and a fitted model does not carry the data it was ",
      "trained on.\n",
      "Pass `background = ` the training features, or a representative sample ",
      "of them.",
      class = c("rtemis_missing_error", "rtemis_input_error")
    )
  }
  invisible(background)
} # /rtemis::shap_require_background


# %% check_shap_linear ----
#' Refuse an estimator or value function a linear backend does not compute
#'
#' LinearSHAP is interventional by construction: it uses each feature's marginal
#' mean and never the joint distribution. A conditional answer needs the
#' covariance and is a different estimator, so asking for one here is refused
#' rather than answered with the marginal numbers under a conditional label.
#'
#' @param estimator Character: Resolved estimator.
#' @param perturbation Character: Resolved value function.
#' @param algorithm Character: Algorithm name, for the message.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_shap_linear <- function(estimator, perturbation, algorithm) {
  if (!identical(estimator, "LinearSHAP")) {
    rtemis.core::abort(
      algorithm,
      "'s explain_super() computes LinearSHAP, not ",
      estimator,
      ".",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (!identical(perturbation, "interventional")) {
    rtemis.core::abort(
      "Conditional LinearSHAP is not implemented: ",
      "`phi_j = beta_j * (x_j - E[x_j])` uses each feature's marginal mean, ",
      "which is the interventional answer.\n",
      "Use `setup_SHAP(perturbation = \"interventional\")`, or ",
      "`setup_SHAP(estimator = \"kernel\", perturbation = \"conditional\")`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis::check_shap_linear


# %% collapse_by_term ----
#' Sum design-matrix contributions back into the terms they came from
#'
#' `model.matrix()` expands a factor into contrast columns, and records which
#' term each column came from in its `assign` attribute. Reading that is exact
#' and needs no assumption about the contrast coding -- treatment contrasts drop
#' the reference level into the intercept, which is why the number of columns is
#' not the number of levels and why parsing names would not do.
#'
#' @param phi Numeric matrix: Contributions, one column per design-matrix column.
#' @param assign Integer: `attr(model.matrix(...), "assign")`, aligned to
#' `phi`'s columns.
#' @param labels Character: Term labels, indexed by `assign`.
#'
#' @return Numeric matrix with one column per term, in `labels` order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
collapse_by_term <- function(phi, assign, labels) {
  out <- matrix(
    0,
    nrow = nrow(phi),
    ncol = length(labels),
    dimnames = list(rownames(phi), labels)
  )
  for (j in seq_along(labels)) {
    columns <- which(assign == j)
    if (length(columns) > 0L) {
      out[, j] <- rowSums(phi[, columns, drop = FALSE])
    }
  }
  out
} # /rtemis::collapse_by_term


# %% linear_shap ----
#' Exact Shapley values for a model linear in its features
#'
#' `phi_j = beta_j * (x_j - E[x_j])`, with the expectation taken over the
#' background. Closed form and exact: for a linear value function the Shapley
#' value of a feature is its coefficient times its deviation from the mean, and
#' no coalition needs to be enumerated.
#'
#' Interventional by construction. It uses the marginal mean of each feature and
#' never the joint distribution, so a correlated proxy receives no credit for
#' what its partner does.
#'
#' @param design Numeric matrix: Cases to explain, in the coefficients' space.
#' @param background Numeric matrix: Reference cases, same columns.
#' @param coefficients Numeric matrix: One column per output.
#' @param intercept Numeric: One per output.
#' @param margin Optional numeric matrix: The model's own linear predictor, for
#' verification.
#' @param label Character: Estimator name, for error messages.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linear_shap <- function(
  design,
  background,
  coefficients,
  intercept,
  margin = NULL,
  label = "LinearSHAP"
) {
  coefficients <- as.matrix(coefficients)
  # A coefficient a backend could not resolve -- an aliased or dropped column --
  # contributes nothing, which is what 0 says. Left as NA it would propagate
  # through every case's total.
  coefficients[is.na(coefficients)] <- 0
  if (nrow(coefficients) != ncol(design)) {
    rtemis.core::abort(
      label,
      ": ",
      nrow(coefficients),
      " coefficients for ",
      ncol(design),
      " columns.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  means <- colMeans(background)
  centered <- sweep(design, 2L, means, "-")
  n_outputs <- ncol(coefficients)
  phi <- vector("list", n_outputs)
  baseline <- numeric(n_outputs)
  predicted <- matrix(0, nrow = nrow(design), ncol = n_outputs)
  for (k in seq_len(n_outputs)) {
    beta <- coefficients[, k]
    phi[[k]] <- sweep(centered, 2L, beta, "*")
    colnames(phi[[k]]) <- colnames(design)
    baseline[[k]] <- intercept[[k]] + sum(beta * means)
    predicted[, k] <- as.numeric(design %*% beta) + intercept[[k]]
  }
  # The reconstruction is checked against the model's own linear predictor
  # rather than trusted. Coefficients can be read at the wrong regularization
  # path step, or in a different order than the design matrix, and either
  # produces contributions that are internally consistent and describe a
  # different model -- which no additivity check downstream could detect.
  if (!is.null(margin)) {
    deviation <- max(abs(predicted - as.matrix(margin)))
    if (!is.finite(deviation) || deviation > 1e-6) {
      rtemis.core::abort(
        label,
        ": reconstructed predictions differ from the model's own by ",
        format(deviation, digits = 3L),
        ". The coefficients do not describe the fitted model.",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
  }
  list(
    phi = phi,
    baseline = baseline,
    predicted = predicted,
    exact = TRUE
  )
} # /rtemis::linear_shap


# %% probe_linear_map ----
#' Recover an affine model's coefficients by probing it
#'
#' For a model that is affine in its features, `w_j = m(x + e_j) - m(x)` and
#' `b = m(x) - w'x` recover the map **exactly** -- this is a difference of an
#' affine function, not a numerical derivative, so there is no step size to
#' choose and no approximation error.
#'
#' Preferred over reading a backend's coefficients wherever the two can
#' disagree. `spls::coef()` and `e1071::coef()` both report coefficients in a
#' space the model scaled internally, so multiplying the *input* features by
#' them does not reproduce the model's own predictions -- for SPLS the two
#' differ by about 1%. Probing recovers the map `predict()` actually implements,
#' whatever it scaled on the way.
#'
#' Costs one prediction call on `p + 1` rows.
#'
#' @param margin_fn Function: Takes a data.frame of cases, returns the model's
#' linear predictor as a vector or a one-column-per-output matrix.
#' @param base Named numeric: Point to probe around.
#'
#' @return List with `coefficients` (`p x k`) and `intercept` (length `k`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
probe_linear_map <- function(margin_fn, base) {
  p <- length(base)
  # Row i must be `base + e_i`. Built by row: `base + diag(p)` recycles
  # column-major and would give `base[i] + e_i` instead, which is a different
  # point and silently wrong wherever `base` is not constant.
  probes <- rbind(
    base,
    matrix(base, nrow = p, ncol = p, byrow = TRUE) + diag(p)
  )
  colnames(probes) <- names(base)
  margin <- as.matrix(margin_fn(as.data.frame(probes)))
  n_outputs <- ncol(margin)
  coefficients <- margin[-1L, , drop = FALSE] -
    matrix(margin[1L, ], nrow = p, ncol = n_outputs, byrow = TRUE)
  rownames(coefficients) <- names(base)
  list(
    coefficients = coefficients,
    intercept = as.numeric(margin[1L, ] - crossprod(coefficients, base))
  )
} # /rtemis::probe_linear_map


# %% probed_linear_shap ----
#' LinearSHAP for a backend whose coefficients cannot be read directly
#'
#' Recovers the map by probing, then hands it to `linear_shap()`, which checks
#' the reconstruction against the model's own predictions. That check is what
#' makes probing safe: a model that is *not* affine reconstructs badly and is
#' refused, rather than being described by the tangent plane at one point.
#'
#' @param design tabular data: Cases to explain.
#' @param background tabular data: Reference cases.
#' @param margin_fn Function: The model's linear predictor.
#' @param label Character: Estimator name, for error messages.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
probed_linear_shap <- function(
  design,
  background,
  margin_fn,
  label = "LinearSHAP"
) {
  design <- as.matrix(as.data.frame(design))
  background <- as.matrix(as.data.frame(background))
  map <- probe_linear_map(margin_fn, colMeans(background))
  linear_shap(
    design = design,
    background = background,
    coefficients = map[["coefficients"]],
    intercept = map[["intercept"]],
    margin = as.matrix(margin_fn(as.data.frame(design))),
    label = label
  )
} # /rtemis::probed_linear_shap


# %% additive_terms_shap ----
#' Exact Shapley values for a model that is additive in its terms
#'
#' For `f(x) = b + sum_t g_t(x_t)` with each term reading one feature, the
#' interventional Shapley value of that feature is `g_t(x_t) - E[g_t]`. No
#' coalition needs enumerating: with no interaction between terms, a feature's
#' marginal contribution is the same whatever else is already in the coalition,
#' so the Shapley average collapses to that one difference.
#'
#' The expectation is taken over the supplied background, not over whatever the
#' backend centered its terms on when it was fitted, so that a baseline means
#' the same thing here as it does for every other estimator.
#'
#' A term reading more than one feature breaks the premise -- its value would
#' have to be split between them, which is a within-term Shapley problem rather
#' than a sum. That is what `margin` catches: an interaction leaves the terms
#' unable to reconstruct the model's own predictions.
#'
#' @param terms_new Numeric matrix: Per-term values for the cases to explain.
#' @param terms_background Numeric matrix: Per-term values for the background.
#' @param feature_of_term Character: Feature each column of the terms matrices
#' belongs to.
#' @param feature_names Character: Output columns, in order.
#' @param margin_new Numeric: The model's own linear predictor for the cases.
#' @param margin_background Numeric: The same for the background.
#' @param label Character: Estimator name, for error messages.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
additive_terms_shap <- function(
  terms_new,
  terms_background,
  feature_of_term,
  feature_names,
  margin_new,
  margin_background,
  label = "TermsSHAP"
) {
  centered <- sweep(terms_new, 2L, colMeans(terms_background), "-")
  phi <- matrix(
    0,
    nrow = nrow(centered),
    ncol = length(feature_names),
    dimnames = list(rownames(terms_new), feature_names)
  )
  for (j in seq_along(feature_names)) {
    columns <- which(feature_of_term == feature_names[[j]])
    if (length(columns) > 0L) {
      phi[, j] <- rowSums(centered[, columns, drop = FALSE])
    }
  }
  # E[f(x)] over the background, which is what the contributions deviate from.
  baseline <- mean(margin_background)
  predicted <- matrix(as.numeric(margin_new), ncol = 1L)
  deviation <- max(abs(rowSums(phi) + baseline - predicted[, 1L]))
  if (!is.finite(deviation) || deviation > 1e-6) {
    rtemis.core::abort(
      label,
      ": the model's terms do not reconstruct its predictions (off by ",
      format(deviation, digits = 3L),
      "). This fit is not additive in its features, so its terms are not ",
      "its Shapley values.\n",
      "Use `setup_SHAP(estimator = \"kernel\")`.",
      class = c("rtemis_unsupported_error", "rtemis_data_error")
    )
  }
  list(
    phi = list(phi),
    baseline = baseline,
    predicted = predicted,
    exact = TRUE
  )
} # /rtemis::additive_terms_shap


# %% terms_feature_map ----
#' Map each term of a formula-built model to the feature it reads
#'
#' A term label parses to the variables it mentions -- `s(age, k = 5)` to
#' `age` -- so the map is read from the model's own labels rather than by
#' matching names, which would confuse `s(ab)` with a feature called `a`.
#'
#' @param term_labels Character: Column names of a `type = "terms"` matrix.
#' @param feature_names Character: The model's features.
#' @param label Character: Estimator name, for error messages.
#'
#' @return Character: The feature each term reads, aligned to `term_labels`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
terms_feature_map <- function(term_labels, feature_names, label = "TermsSHAP") {
  mapped <- vapply(
    term_labels,
    function(term) {
      variables <- intersect(all.vars(str2lang(term)), feature_names)
      # Exactly one, or the term is not a single feature's and the additive
      # premise does not hold for it.
      if (length(variables) != 1L) {
        rtemis.core::abort(
          label,
          ": term '",
          term,
          "' reads ",
          length(variables),
          " features, so its value is not one feature's contribution.\n",
          "Use `setup_SHAP(estimator = \"kernel\")`.",
          class = c("rtemis_unsupported_error", "rtemis_data_error")
        )
      }
      variables
    },
    character(1L),
    USE.NAMES = FALSE
  )
  mapped
} # /rtemis::terms_feature_map


# %% check_shap_binary ----
#' Refuse a multiclass fit to an estimator that has one linear map per model
#'
#' @param type Character: "Regression" or "Classification".
#' @param n_classes Integer: Number of classes the model was fitted on.
#' @param algorithm Character: Algorithm name, for the message.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_shap_binary <- function(type, n_classes, algorithm) {
  if (identical(type, "Classification") && n_classes > 2L) {
    rtemis.core::abort(
      "LinearSHAP is not available for multiclass ",
      algorithm,
      ": the fitted model has no single linear map per class.\n",
      "Use `setup_SHAP(estimator = \"kernel\")`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis::check_shap_binary


# %% model_matrix_shap ----
#' LinearSHAP for a backend that builds its own design matrix
#'
#' `glm()` and `glmnet()` expand factors through `model.matrix()` rather than
#' through a `Preprocessor`, so the expansion is internal to them and is undone
#' here: contributions come back indexed by the columns of `newdata`, which is
#' what every `explain_super()` method returns.
#'
#' @param newdata tabular data: Cases to explain.
#' @param background tabular data: Reference cases.
#' @param formula_terms `terms` object or formula used to build the design.
#' @param coefficients Numeric matrix: One column per output, aligned to the
#' design matrix including its intercept column.
#' @param margin_fn Optional function: Given the design matrix for `newdata`
#' (intercept column dropped), returns the model's own linear predictor. Taken
#' as a callback because the design is built here, and a backend that predicts
#' from a matrix needs the same one.
#' @param label Character: Estimator name, for error messages.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
model_matrix_shap <- function(
  newdata,
  background,
  formula_terms,
  coefficients,
  margin_fn = NULL,
  label = "LinearSHAP"
) {
  # Built from the two frames stacked, so a factor level present in one and not
  # the other cannot produce two different column sets.
  combined <- rbind(as.data.frame(newdata), as.data.frame(background))
  design_all <- stats::model.matrix(formula_terms, combined)
  assign <- attr(design_all, "assign")
  labels <- colnames(newdata)
  n <- NROW(newdata)
  coefficients <- as.matrix(coefficients)
  intercept <- coefficients[1L, , drop = TRUE]
  betas <- coefficients[-1L, , drop = FALSE]
  design_all <- design_all[, -1L, drop = FALSE]
  assign <- assign[-1L]
  design <- design_all[seq_len(n), , drop = FALSE]

  computed <- linear_shap(
    design = design,
    background = design_all[-seq_len(n), , drop = FALSE],
    coefficients = betas,
    intercept = intercept,
    margin = if (is.null(margin_fn)) NULL else margin_fn(design),
    label = label
  )
  # The design-matrix values are kept, not discarded: they are the per-level
  # view, and `explain()` publishes them as `@phi_encoded`. For a contrast-coded
  # factor they are relative to the reference level, which has no column of its
  # own -- see `shap_by_level()` for the question they are usually asked to
  # answer, which they answer badly.
  computed[["phi_encoded"]] <- computed[["phi"]]
  computed[["phi"]] <- lapply(
    computed[["phi"]],
    function(phi) collapse_by_term(phi, assign, labels)
  )
  computed
} # /rtemis::model_matrix_shap


# %% resolve_shap_estimator ----
#' Resolve `estimator` against what the algorithm offers
#'
#' @param requested Character: `SHAPConfig@estimator`.
#' @param algorithm Character: Algorithm name.
#'
#' @return Character: One of `SHAP_RESOLVED_ESTIMATORS`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_shap_estimator <- function(requested, algorithm) {
  row <- explanation_methods(algorithm)
  default <- row[["estimator"]]
  if (requested == "auto") {
    return(default)
  }
  if (requested == "kernel") {
    return("KernelSHAP")
  }
  # "exact". A row whose `exact` is FALSE has no exact estimator to offer, and
  # asking for one explicitly is a request the package cannot honor -- silently
  # falling back to the kernel estimator would answer a different question than
  # the one asked.
  if (isFALSE(row[["exact"]])) {
    rtemis.core::abort(
      "No exact explanation estimator for ",
      algorithm,
      ".\n",
      "`explanation_methods(\"",
      algorithm,
      "\")` reports: ",
      row[["rationale"]],
      "\n",
      "Use `setup_SHAP(estimator = \"kernel\")`, or leave it \"auto\".",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  default
} # /rtemis::resolve_shap_estimator


# %% explain.Supervised ----
#' Explain `Supervised`
#'
#' Per-case contributions for `newdata`, computed on demand rather than stored,
#' for the reason `se()` gives: a quantity that is O(n x p), depends on
#' `newdata` and on a background sample, and that most users never ask for, is
#' computed when asked.
#'
#' `newdata` and `background` hold predictors only, in training order, as
#' `predict()` requires.
#'
#' @param x `Supervised` object.
#' @param newdata tabular data: Cases to explain.
#' @param background Optional tabular data: Reference cases the contributions
#' are relative to. Required by every estimator that needs `E[x]` or a reference
#' set; those abort with a message naming it when it is missing.
#' @param config Optional `ExplanationConfig` object: Defaults to `setup_SHAP()`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `SHAP` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(explain, Supervised) <- function(
  x,
  newdata,
  background = NULL,
  config = NULL,
  verbosity = 1L,
  ...
) {
  check_inherits(newdata, "data.frame")
  if (is.null(config)) {
    config <- setup_SHAP()
  }
  check_is_S7(config, ExplanationConfig)

  estimator <- resolve_shap_estimator(config@estimator, x@algorithm)
  perturbation <- if (is.null(config@perturbation)) {
    SHAP_ESTIMATOR_PERTURBATION[[estimator]]
  } else {
    config@perturbation
  }
  scale <- resolve_shap_scale(config@scale, x@type, estimator)

  # Both go through the same pipeline as `predict()`: contributions computed
  # from differently transformed features than the predictions they decompose
  # would be silently mismatched.
  features <- supervised_features(x, newdata, verbosity = 0L)
  bg <- shap_background(x, background, config, verbosity = verbosity)

  computed <- if (identical(estimator, "KernelSHAP")) {
    # Model-agnostic, so it does not dispatch: it reads a prediction function
    # rather than a backend.
    kernel_shap(
      object = x,
      features = features,
      background = bg,
      perturbation = perturbation,
      config = config,
      verbosity = verbosity
    )
  } else {
    # A missing method means the estimator is not written yet, which is a fact
    # about this build rather than about the model. S7's dispatch error names a
    # backend class the user never chose, so it is translated into the algorithm
    # they did.
    tryCatch(
      explain_super(
        model = x@model,
        newdata = features,
        background = bg,
        estimator = estimator,
        perturbation = perturbation,
        scale = scale,
        type = x@type,
        verbosity = verbosity
      ),
      S7_error_method_not_found = function(e) {
        rtemis.core::abort(
          estimator,
          " is not implemented for ",
          x@algorithm,
          " yet.\n",
          "Use `setup_SHAP(estimator = \"kernel\")`, which applies to every ",
          "algorithm.",
          class = c("rtemis_unsupported_error", "rtemis_input_error")
        )
      }
    )
  }
  # An estimator whose baseline is the same for every case returns one value per
  # output; the object stores a row per case either way, so consumers never
  # branch on which kind of estimator produced it.
  if (!is.matrix(computed[["baseline"]])) {
    computed[["baseline"]] <- matrix(
      computed[["baseline"]],
      nrow = nrow(computed[["predicted"]]),
      ncol = length(computed[["baseline"]]),
      byrow = TRUE
    )
  }
  computed <- name_shap_outputs(computed, x)

  aggregated <- lapply(
    computed[["phi"]],
    function(phi) shap_aggregate(phi, x, names(newdata))
  )
  space <- aggregated[[1L]][["space"]]
  phi <- lapply(aggregated, `[[`, "phi")
  # Labeled here rather than per estimator, so a case is identified the same way
  # whichever one ran -- one of them builds its matrices from a design matrix
  # that carries the labels and another from a backend's output that does not.
  case_labels <- rownames(as.data.frame(newdata))
  phi <- lapply(phi, function(contributions) {
    rownames(contributions) <- case_labels
    contributions
  })
  rownames(computed[["predicted"]]) <- case_labels
  rownames(computed[["baseline"]]) <- case_labels
  # The finest-grained view available, carried only where it says something the
  # reported matrices do not. A backend that expanded the features itself hands
  # its own; otherwise it is what the aggregation collapsed, and if nothing
  # expanded then the two are the same numbers.
  phi_encoded <- if (!is.null(computed[["phi_encoded"]])) {
    computed[["phi_encoded"]]
  } else if (identical(phi, computed[["phi"]])) {
    NULL
  } else {
    computed[["phi"]]
  }

  SHAP(
    algorithm = x@algorithm,
    config = config,
    space = space,
    feature_names = colnames(phi[[1L]]),
    data_fingerprint = data_fingerprint(newdata),
    background_fingerprint = if (is.null(background)) {
      NULL
    } else {
      data_fingerprint(background)
    },
    phi = phi,
    phi_encoded = phi_encoded,
    baseline = computed[["baseline"]],
    predicted = computed[["predicted"]],
    scale = scale,
    perturbation = perturbation,
    estimator = estimator,
    exact = computed[["exact"]]
  )
} # /rtemis::explain.Supervised


# %% name_shap_outputs ----
#' Label an `explain_super()` result by class
#'
#' A backend returns one block of contributions per output and does not know
#' what the outputs are called; the model does. Named by the same rule
#' `prob_matrix()` uses for predicted probabilities, so a `SHAP`'s classes and a
#' `predict()` matrix's columns agree.
#'
#' @param computed List: `explain_super()` result.
#' @param x `Supervised` object.
#'
#' @return `computed`, with `phi`, `baseline` and `predicted` named.
#'
#' @author EDG
#' @keywords internal
#' @noRd
name_shap_outputs <- function(computed, x) {
  n_outputs <- length(computed[["phi"]])
  labels <- if (identical(x@type, "Regression")) {
    # The outcome's own name is not stored on a fitted model, so the entry is
    # named for what it is rather than for the column it came from.
    "outcome"
  } else {
    levels <- levels(x@y_training)
    if (n_outputs == length(levels)) {
      levels
    } else if (n_outputs == 1L && length(levels) == 2L) {
      # Binary: one block, explained on the positive class. The negative class's
      # contributions are its exact negation, so storing them would be storing
      # the same numbers twice.
      levels[[x@binclasspos]]
    } else {
      rtemis.core::abort(
        "Backend returned ",
        n_outputs,
        " blocks of contributions for ",
        length(levels),
        " classes.",
        class = c("rtemis_dim_error", "rtemis_data_error")
      )
    }
  }
  names(computed[["phi"]]) <- labels
  colnames(computed[["baseline"]]) <- labels
  colnames(computed[["predicted"]]) <- labels
  computed
} # /rtemis::name_shap_outputs


# %% explain.SupervisedRes ----
#' Explain `SupervisedRes`
#'
#' Explains `newdata` under every resample's fitted model, against one shared
#' background.
#'
#' @details
#' **The fold average is a decomposition, not a summary.** Since
#' `sum(phi_k) + b_k = f_k(x)` holds for each fold, averaging both sides gives
#' `sum(phi_bar) + b_bar = f_bar(x)`: the averaged contributions decompose the
#' averaged prediction **exactly**. What makes that true is a single
#' `background` shared across folds, which is why one is taken here rather than
#' per model.
#'
#' `type` echoes [stats::predict()] on the same object deliberately -- same word,
#' same concept -- and `explain()` on a single `Supervised` likewise has no
#' `type`, as `predict()` does not.
#'
#' For a classification the folds' *margins* are averaged, which is not the mean
#' probability `predict(type = "avg")` returns; the two differ by the link, as
#' they do for a single model.
#'
#' @param x `SupervisedRes` object.
#' @param newdata tabular data: Cases to explain.
#' @param background Optional tabular data: Reference cases, shared by every
#' resample.
#' @param config Optional `ExplanationConfig` object: Defaults to `setup_SHAP()`.
#' @param type Character \{"avg", "all"\}: Average the resamples' explanations,
#' or return one per resample.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `SHAP` object, or a named list of them when `type` is "all".
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(explain, SupervisedRes) <- function(
  x,
  newdata,
  background = NULL,
  config = NULL,
  verbosity = 1L,
  type = c("avg", "all"),
  ...
) {
  type <- match_arg(type, c("avg", "all"))
  msg(
    "Explaining ",
    length(x@models),
    " resamples...",
    verbosity = verbosity
  )
  explanations <- lapply(x@models, function(model) {
    explain(
      model,
      newdata = newdata,
      background = background,
      config = config,
      verbosity = 0L
    )
  })
  if (identical(type, "all")) {
    return(explanations)
  }
  average_shap(explanations)
} # /rtemis::explain.SupervisedRes


# %% average_shap ----
#' Average explanations across resamples
#'
#' Exact rather than approximate: each fold's contributions decompose that
#' fold's prediction, so their mean decomposes the mean prediction.
#'
#' The folds must agree on everything that gives the numbers meaning -- the
#' feature space, the scale, the value function -- or their mean describes
#' nothing. Models fitted to different resamples of one dataset by one algorithm
#' always do; it is checked rather than assumed because the failure would be
#' silent.
#'
#' @param explanations List of `SHAP` objects.
#'
#' @return `SHAP` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
average_shap <- function(explanations) {
  first <- explanations[[1L]]
  for (field in c("space", "scale", "perturbation", "estimator")) {
    values <- unique(vapply(
      explanations,
      function(x) as.character(prop(x, field)),
      character(1L)
    ))
    if (length(values) > 1L) {
      rtemis.core::abort(
        "Resamples disagree on @",
        field,
        " (",
        paste(values, collapse = ", "),
        "), so their explanations cannot be averaged.",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
  }
  if (
    !identical(
      unique(lapply(explanations, function(x) x@feature_names)),
      list(first@feature_names)
    )
  ) {
    rtemis.core::abort(
      "Resamples disagree on which features they attribute to.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }

  mean_of <- function(extract) {
    Reduce(`+`, lapply(explanations, extract)) / length(explanations)
  }
  phi <- lapply(names(first@phi), function(label) {
    mean_of(function(x) x@phi[[label]])
  })
  names(phi) <- names(first@phi)

  SHAP(
    algorithm = first@algorithm,
    config = first@config,
    space = first@space,
    feature_names = first@feature_names,
    data_fingerprint = first@data_fingerprint,
    background_fingerprint = first@background_fingerprint,
    phi = phi,
    phi_encoded = NULL,
    baseline = mean_of(function(x) x@baseline),
    predicted = mean_of(function(x) x@predicted),
    scale = first@scale,
    perturbation = first@perturbation,
    estimator = first@estimator,
    # A mean of exact decompositions is exact for the mean prediction; a mean
    # involving an estimate is not.
    exact = all(vapply(explanations, function(x) x@exact, logical(1L)))
  )
} # /rtemis::average_shap


# %% resolve_shap_scale ----
#' Resolve `scale` against the kind of learning and the estimator
#'
#' Which scale is available is not a property of the outcome alone: it depends
#' on what the estimator can see.
#'
#' An exact estimator reads the model's own additive structure, which for a
#' classifier with a link is the margin. The kernel estimator sees only what
#' `predict()` returns, which is the probability -- and its contributions are
#' additive **on that**, exactly, because it decomposes the function it was
#' given rather than transforming a margin decomposition. So both scales are
#' honest; they are simply not comparable, which is why the resolved one is
#' recorded on every result.
#'
#' @param requested Optional Character: `SHAPConfig@scale`.
#' @param type Character: "Regression" or "Classification".
#' @param estimator Character: Resolved estimator.
#'
#' @return Character: One of `SHAP_SCALES`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_shap_scale <- function(requested, type, estimator) {
  # Regression has one scale -- the outcome's -- and "margin" is its name here.
  # Asking for probabilities of a quantity that is not one is a mistake worth
  # naming rather than ignoring.
  if (identical(type, "Regression")) {
    if (identical(requested, "probability")) {
      rtemis.core::abort(
        "`scale = \"probability\"` does not apply to a Regression model: ",
        "its contributions are on the outcome's own scale.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return("margin")
  }
  if (identical(estimator, "KernelSHAP")) {
    if (identical(requested, "margin")) {
      rtemis.core::abort(
        "KernelSHAP cannot explain a classification on the margin: it sees ",
        "only what `predict()` returns, which is the probability.\n",
        "Leave `scale` NULL, or use an exact estimator where the algorithm ",
        "has one -- `explanation_methods()` reports which do.",
        class = c("rtemis_unsupported_error", "rtemis_input_error")
      )
    }
    return("probability")
  }
  if (identical(requested, "probability")) {
    rtemis.core::abort(
      "`scale = \"probability\"` is not implemented for ",
      estimator,
      ".\n",
      "Its contributions are additive on the model's own scale, and ",
      "probability is a nonlinear transform of it, so rescaling them would ",
      "not sum to the predicted probability. `setup_SHAP(estimator = ",
      "\"kernel\")` decomposes the probability directly.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  "margin"
} # /rtemis::resolve_shap_scale


# %% supervised_outputs ----
#' The model's outputs on the scale the contributions decompose
#'
#' One column per output: the fitted value for a regression, the positive
#' class's probability for a binary outcome, and one column per class for a
#' multiclass one -- the shape `prob_matrix()` already normalizes predictions to,
#' so a `SHAP`'s blocks and a predicted probability matrix's columns agree.
#'
#' Predicts from *already transformed* features, because the kernel estimator
#' perturbs them: re-running the preprocessing pipeline on a perturbed row would
#' transform it a second time.
#'
#' @param object `Supervised` object.
#' @param features tabular data: Features, already transformed.
#'
#' @return Numeric matrix, one row per case and one column per output.
#'
#' @author EDG
#' @keywords internal
#' @noRd
supervised_outputs <- function(object, features) {
  predicted <- predict_super(
    model = object@model,
    newdata = as.data.frame(features),
    type = object@type,
    verbosity = 0L
  )
  if (identical(object@type, "Classification")) {
    return(prob_matrix(
      predicted,
      levels(object@y_training),
      object@binclasspos
    ))
  }
  matrix(as.numeric(predicted), ncol = 1L)
} # /rtemis::supervised_outputs


# %% kernel_shap ----
#' Model-agnostic Shapley values, via shapr
#'
#' The fallback that makes `explain()` answer for every algorithm. It is handled
#' here rather than as an `explain_super()` method because it is
#' model-agnostic: it needs a prediction function, not a backend, so dispatching
#' on the fitted object's class would be dispatching on something it does not
#' read. That also means an algorithm sharing a backend class with another
#' cannot shadow it.
#'
#' `perturbation` selects the value function directly: `"interventional"` is
#' shapr's `independence` approach, and a conditional one is whichever
#' `approach` was asked for. That is the whole reason this plan chose shapr --
#' the choice decision 1 says must never be implicit is a first-class argument.
#'
#' Not exact: shapr samples coalitions and Monte Carlo samples within them, so
#' `@exact` is FALSE even where the sampling happens to be exhaustive.
#'
#' @param object `Supervised` object.
#' @param features tabular data: Cases to explain, already transformed.
#' @param background tabular data: Reference cases, already transformed.
#' @param perturbation Character: Resolved value function.
#' @param config `SHAPConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kernel_shap <- function(
  object,
  features,
  background,
  perturbation,
  config,
  verbosity = 1L
) {
  check_dependencies("shapr")
  shap_require_background(background, "KernelSHAP")
  features <- as.data.frame(features)
  background <- as.data.frame(background)
  approach <- if (identical(perturbation, "interventional")) {
    # shapr's spelling of "do not condition", which is the interventional value
    # function. `@approach` is gated to conditional runs for this reason.
    "independence"
  } else if (is.null(config@approach)) {
    # Nonparametric and makes no distributional assumption, which suits data
    # that has already been through a preprocessor and may be anything.
    "empirical"
  } else {
    config@approach
  }

  predicted <- supervised_outputs(object, features)
  background_predicted <- supervised_outputs(object, background)
  n_outputs <- ncol(predicted)
  phi <- vector("list", n_outputs)
  baseline <- numeric(n_outputs)

  for (k in seq_len(n_outputs)) {
    baseline[[k]] <- mean(background_predicted[, k])
    estimated <- shapr::explain(
      # shapr only ever calls `predict_model`, so the model it is handed need
      # not be the fitted object -- and must not be, since the prediction has to
      # select one output and skip the preprocessing pipeline.
      model = structure(list(), class = "rtemis_supervised"),
      x_explain = features,
      x_train = background,
      approach = approach,
      phi0 = baseline[[k]],
      max_n_coalitions = config@n_coalitions,
      seed = config@seed,
      verbose = if (verbosity > 1L) "basic" else NULL,
      predict_model = function(x, newdata) {
        supervised_outputs(object, newdata)[, k]
      }
    )
    values <- estimated[["shapley_values_est"]]
    phi[[k]] <- as.matrix(
      data.table::setDF(data.table::copy(values))[,
        names(features),
        drop = FALSE
      ]
    )
    dimnames(phi[[k]]) <- list(NULL, names(features))
  }

  list(
    phi = phi,
    baseline = baseline,
    predicted = predicted,
    # Sampled, both over coalitions and within them.
    exact = FALSE
  )
} # /rtemis::kernel_shap


# %% get_varimp.SHAP ----
#' Variable importance from per-case contributions
#'
#' `mean(|phi|)` per feature: how much that feature moved the prediction on
#' average, in the outcome's own units.
#'
#' @details
#' A better global measure than most native ones, for three reasons. It is on
#' the scale of the outcome rather than of a splitting criterion, so the numbers
#' mean something. It is comparable across algorithms, because every estimator
#' here decomposes the same quantity. And it does not inherit the bias of
#' impurity-based importance toward high-cardinality features.
#'
#' It also gives an importance to algorithms that have none of their own -- a
#' torch network has no native measure, so `get_varimp()` on a fitted MLP
#' returns NULL, while `get_varimp(explain(model, newdata, background))` does
#' not. No torch-specific code was needed for that.
#'
#' One row per feature. Which *level* of a categorical drives its importance is
#' a different question, and [shap_by_level] answers it.
#'
#' The magnitude is what is averaged, so contributions that cancel across cases
#' do not vanish: a feature that pushes half the cases up and half down is
#' important, and a signed mean would call it irrelevant.
#'
#' @param x `SHAP` object.
#'
#' @return `VariableImportance` object with one measure per class.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(get_varimp, SHAP) <- function(x) {
  measures <- lapply(x@phi, function(contributions) {
    colMeans(abs(contributions))
  })
  # One measure column per class, named for it; a regression or binary outcome
  # has one, whose name is the outcome's or the positive class's.
  importance <- data.table(variable = x@feature_names)
  for (label in names(measures)) {
    importance[, (label) := unname(measures[[label]])]
  }
  VariableImportance(importance)
} # /rtemis::get_varimp.SHAP


# %% shap_case ----
#' One case's contributions, ordered by how much they moved the prediction
#'
#' The three things a per-case explanation is: where the prediction would have
#' started, what each feature did to it, and where it ended.
#'
#' @details
#' Shaped here rather than in a renderer, following `session_timeline()`: the
#' table is the shared input for rtemis.draw's chart and for rtemislive, so both
#' show the same thing, and it is useful on its own without either.
#'
#' Features are ordered by the magnitude of their contribution, which is the
#' order a waterfall reads in, and each carries the value it took for this case
#' so the row says "age = 62, +0.4" rather than only "+0.4".
#'
#' @param x `SHAP` object.
#' @param newdata tabular data: The cases `x` explains.
#' @param case Integer or Character: Which case, by position or row name.
#' @param class Optional Character: Which class's contributions. NULL takes the
#' first, which is the only one for a regression or binary outcome.
#'
#' @return List with `steps` (a data.table of `feature`, `value` and
#' `contribution`), `baseline`, `predicted`, `case` and `class`.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(age = rnorm(100), bmi = rnorm(100))
#' x[["y"]] <- x[["age"]] * 2 + rnorm(100, sd = 0.3)
#' mod <- train(x, hyperparameters = setup_GLM(), verbosity = 0L)
#' contributions <- explain(
#'   mod,
#'   x[, c("age", "bmi")],
#'   background = x[, c("age", "bmi")],
#'   verbosity = 0L
#' )
#' shap_case(contributions, x[, c("age", "bmi")], case = 1L)
shap_case <- function(x, newdata, case = 1L, class = NULL) {
  check_is_S7(x, SHAP)
  newdata <- shap_check_newdata(x, newdata)
  label <- shap_check_class(x, class)
  index <- if (is.character(case)) {
    match(case, rownames(newdata))
  } else {
    as.integer(case)
  }
  if (is.na(index) || index < 1L || index > NROW(newdata)) {
    rtemis.core::abort(
      "No such case: ",
      case,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  contributions <- x@phi[[label]][index, ]
  steps <- data.table(
    feature = x@feature_names,
    # As character, because a waterfall labels a factor's level and a numeric's
    # value in the same place.
    value = vapply(
      x@feature_names,
      function(feature) as.character(newdata[[feature]][[index]]),
      character(1L),
      USE.NAMES = FALSE
    ),
    contribution = unname(contributions)
  )
  # By magnitude: a waterfall reads largest-effect-first, whichever way the
  # effect went.
  steps <- steps[order(-abs(steps[["contribution"]]))]
  list(
    steps = steps,
    baseline = x@baseline[index, label],
    predicted = x@predicted[index, label],
    case = rownames(newdata)[[index]],
    class = label
  )
} # /rtemis::shap_case


# %% shap_long ----
#' Every contribution, one row per case per feature
#'
#' The long form a beeswarm reads: a point per case per feature, positioned by
#' its contribution and colored by the value the feature took.
#'
#' Shaped here rather than in a renderer, for the reason `shap_case()` gives.
#'
#' @param x `SHAP` object.
#' @param newdata tabular data: The cases `x` explains.
#' @param class Optional Character: Which class's contributions. NULL takes the
#' first.
#'
#' @return data.table with columns `case`, `feature`, `value`, `numeric_value`
#' and `contribution`. `numeric_value` is NA for a feature that is not numeric,
#' which is what a color scale needs to skip it.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(age = rnorm(100), bmi = rnorm(100))
#' x[["y"]] <- x[["age"]] * 2 + rnorm(100, sd = 0.3)
#' mod <- train(x, hyperparameters = setup_GLM(), verbosity = 0L)
#' contributions <- explain(
#'   mod,
#'   x[, c("age", "bmi")],
#'   background = x[, c("age", "bmi")],
#'   verbosity = 0L
#' )
#' head(shap_long(contributions, x[, c("age", "bmi")]))
shap_long <- function(x, newdata, class = NULL) {
  check_is_S7(x, SHAP)
  newdata <- shap_check_newdata(x, newdata)
  label <- shap_check_class(x, class)
  contributions <- x@phi[[label]]
  rows <- lapply(x@feature_names, function(feature) {
    values <- newdata[[feature]]
    data.table(
      case = rownames(newdata),
      feature = feature,
      value = as.character(values),
      numeric_value = if (is.numeric(values)) {
        as.numeric(values)
      } else {
        NA_real_
      },
      contribution = unname(contributions[, feature])
    )
  })
  data.table::rbindlist(rows)
} # /rtemis::shap_long


# %% shap_check_newdata ----
#' Verify the data handed in is the data explained
#'
#' Contributions lined up against the wrong rows produce a plausible table
#' describing nothing, so the fingerprint on the object is checked rather than
#' the caller trusted.
#'
#' @param x `SHAP` object.
#' @param newdata tabular data.
#'
#' @return `newdata` as a data.frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_check_newdata <- function(x, newdata) {
  check_inherits(newdata, "data.frame")
  if (
    !is.null(x@data_fingerprint) &&
      !identical(data_fingerprint(newdata)@hash, x@data_fingerprint@hash)
  ) {
    rtemis.core::abort(
      "`newdata` is not the data this explanation was computed on.\n",
      "Pass the same cases given to `explain()`, in the same order.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  as.data.frame(newdata)
} # /rtemis::shap_check_newdata


# %% shap_check_class ----
#' Resolve which class's contributions were asked for
#'
#' @param x `SHAP` object.
#' @param class Optional Character.
#'
#' @return Character: One name of `x@phi`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_check_class <- function(x, class) {
  if (is.null(class)) {
    return(names(x@phi)[[1L]])
  }
  if (!class %in% names(x@phi)) {
    rtemis.core::abort(
      "No contributions for class '",
      class,
      "'. This explanation holds: ",
      paste(names(x@phi), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  class
} # /rtemis::shap_check_class


# %% shap_by_level ----
#' Contributions of a categorical feature, by the level each case has
#'
#' What a per-feature contribution hides once it is averaged across cases: that
#' one level of a diagnosis drives the prediction and the others do nothing.
#'
#' @details
#' This is the right tool for "which level matters", and per-level contributions
#' are not. A case has exactly one level, so its per-feature contribution
#' **is** the contribution of the level it has -- there is nothing to split. What
#' averaging loses is only that cases at different levels were averaged together,
#' and grouping them is what restores it.
#'
#' Splitting a contribution across the columns of an encoding answers a
#' different and worse question. Those values include a term for every level the case does
#' *not* have; they depend on the encoding, since a contrast-coded factor's
#' reference level has no column at all and so cannot appear; and for a linear
#' model they are identical for every case at the same level. Grouping has none
#' of those problems and shows the reference level like any other.
#'
#' `newdata` must be the data the explanation was computed on, which is checked
#' against its fingerprint rather than assumed: contributions lined up against
#' the wrong rows would produce a plausible table describing nothing.
#'
#' @param x `SHAP` object.
#' @param newdata tabular data: The cases `x` explains.
#' @param features Optional Character: Features to summarize. NULL takes every
#' categorical feature in `newdata`.
#'
#' @return data.table with columns `class`, `feature`, `level`, `n`, `mean`,
#' `sd`, and `mean_abs`.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(
#'   age = rnorm(100),
#'   dx = factor(sample(c("none", "diabetes"), 100, TRUE))
#' )
#' x[["y"]] <- x[["age"]] + 3 * (x[["dx"]] == "diabetes") + rnorm(100, sd = 0.3)
#' mod <- train(x, hyperparameters = setup_GLM(), verbosity = 0L)
#' contributions <- explain(
#'   mod,
#'   x[, c("age", "dx")],
#'   background = x[, c("age", "dx")],
#'   verbosity = 0L
#' )
#' shap_by_level(contributions, x[, c("age", "dx")])
shap_by_level <- function(x, newdata, features = NULL) {
  check_is_S7(x, SHAP)
  newdata <- shap_check_newdata(x, newdata)
  categorical <- names(newdata)[
    vapply(
      newdata,
      function(column) is.factor(column) || is.character(column),
      logical(1L)
    )
  ]
  if (is.null(features)) {
    features <- intersect(x@feature_names, categorical)
  } else {
    missing <- setdiff(features, x@feature_names)
    if (length(missing) > 0L) {
      rtemis.core::abort(
        "Not a feature of this explanation: ",
        paste(missing, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (length(features) == 0L) {
    rtemis.core::abort(
      "No categorical features to summarize.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  rows <- list()
  for (label in names(x@phi)) {
    contributions <- x@phi[[label]]
    for (feature in features) {
      levels <- factor(newdata[[feature]])
      phi <- contributions[, feature]
      rows[[length(rows) + 1L]] <- data.table(
        class = label,
        feature = feature,
        level = levels(levels),
        n = as.integer(table(levels)),
        mean = as.numeric(tapply(phi, levels, mean)),
        # NA for a level with one case, which `sd()` cannot estimate; that is a
        # fact about the data rather than a failure.
        sd = as.numeric(tapply(phi, levels, stats::sd)),
        mean_abs = as.numeric(tapply(abs(phi), levels, mean))
      )
    }
  }
  data.table::rbindlist(rows)
} # /rtemis::shap_by_level


# %% shap_background ----
#' Put the background sample in the shape the backend was trained on
#'
#' @param x `Supervised` object.
#' @param background Optional tabular data.
#' @param config `SHAPConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Transformed background, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_background <- function(x, background, config, verbosity = 1L) {
  if (is.null(background)) {
    return(NULL)
  }
  check_inherits(background, "data.frame")
  # An attribution is relative to a background, and two explanations against
  # different backgrounds are not comparable. The model carries a fingerprint of
  # what it was trained on, so a background that is not that data can be said
  # out loud rather than left to be assumed.
  if (verbosity > 0L && !is.null(x@data_fingerprint)) {
    if (
      !identical(data_fingerprint(background)@hash, x@data_fingerprint@hash)
    ) {
      msg(
        "Background differs from the training data; contributions are relative to it.",
        verbosity = verbosity
      )
    }
  }
  features <- supervised_features(x, background, verbosity = 0L)
  n <- config@background_n
  if (!is.null(n) && n < NROW(features)) {
    if (!is.null(config@seed)) {
      set.seed(config@seed)
    }
    features <- features[sample.int(NROW(features), n), , drop = FALSE]
  }
  features
} # /rtemis::shap_background
