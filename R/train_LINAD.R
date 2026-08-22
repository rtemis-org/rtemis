# train_LINAD.R
# ::rtemis::
# 2026- EDG rtemis.org

# LINAD is implemented in this package rather than wrapped from a backend.
# The engine lives in `R/linad.R`; this file is the rtemis interface to it.

# %% train_.LINADHyperparameters ----
#' Train a Linear Additive Tree
#'
#' Train a LINAD model: a decision tree carrying a linear model at every node,
#' grown by stagewise gradient descent so that a leaf's coefficients are the
#' accumulated sum along its root-to-leaf path.
#'
#' LINAD needs no encoding preprocessor. It searches and routes splits on the
#' features as given, so a factor splits on a set of its levels, and builds a
#' reference-coded design matrix internally for the leaf models. Returning a
#' one-hot `Preprocessor` would deliver only the encoded frame to
#' `predict_super()`, leaving the level sets with nothing to route on.
#'
#' Missing values are not supported: every case contributes to every leaf model
#' through the soft weights, so there is no node at which an absent value can be
#' ignored.
#'
#' The "train_*" functions train a single model.
#' Use [train] for tuning and testing using nested cross-validation.
#'
#' @param hyperparameters `LINADHyperparameters` object: make using [setup_LINAD].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Used to select the number of
#' leaves, unless `force_max_leaves` is TRUE.
#' @param execution_config `ExecutionConfig` object: Not used.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `LinearAdditiveTree` and a NULL preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LINADHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  settings <- linad_settings(hyperparameters)

  # Dependencies ----
  # Only the elastic-net leaf model reaches outside the package.
  if (
    identical(settings[["node_model"]], "elasticnet") ||
      identical(settings[["root_model"]], "elasticnet")
  ) {
    check_dependencies("glmnet")
  }

  # Data ----
  check_supervised(x = x, allow_missing = FALSE, verbosity = verbosity)
  type <- supervised_type(x)
  features <- as.data.frame(features(x))
  outcome_values <- outcome(x)
  y_levels <- NULL
  if (type == "Classification") {
    if (nlevels(outcome_values) > 2L) {
      rtemis.core::abort(
        "LINAD does not support multiclass classification.",
        class = "rtemis_unsupported_error"
      )
    }
    y_levels <- levels(outcome_values)
    # +1 on the second level, so 1 / (1 + exp(-2F)) is the probability of the
    # positive class with no sign correction anywhere downstream.
    y <- ifelse(outcome_values == y_levels[[2L]], 1, -1)
  } else {
    y <- as.numeric(outcome_values)
  }
  if (is.null(weights)) {
    weights <- rep(1, NROW(features))
  }
  # Rescaled to average 1 so `lambda`, which multiplies the weight total, means
  # the same thing whether or not inverse frequency weighting is on.
  weights <- weights / mean(weights)

  # Train ----
  xlev <- lapply(
    Filter(is.factor, features),
    levels
  )
  xm <- linad_design_matrix(features, xlev)
  fitted <- linad_fit(
    x = features,
    xm = xm,
    y = y,
    case_weights = weights,
    type = type,
    settings = settings,
    verbosity = verbosity
  )
  model <- LinearAdditiveTree(
    frame = fitted[["frame"]],
    coefficients = fitted[["coefficients"]],
    steps = fitted[["steps"]],
    n_leaves = as.integer(fitted[["n_leaves"]]),
    xnames = names(features),
    xlev = xlev,
    design_assign = as.integer(attr(xm, "assign")),
    design_names = colnames(xm),
    design_scale = linad_scaling(xm)[["scale"]],
    type = type,
    y_levels = y_levels,
    leaf_curve = NULL
  )

  # Number of leaves ----
  # The manuscript's counterpart to choosing the number of trees in gradient
  # boosting: score the tree at every size it passed through, keep the argmin.
  if (!settings[["force_max_leaves"]] && length(model@steps) > 1L) {
    if (is.null(dat_validation)) {
      if (verbosity > 0L) {
        info(
          "No validation set: keeping all ",
          model@n_leaves,
          " leaves. Pass `dat_validation` to train(), or set ",
          "`force_max_leaves = TRUE` to say so explicitly."
        )
      }
    } else {
      validation_features <- as.data.frame(features(dat_validation))
      validation_outcome <- outcome(dat_validation)
      validation_y <- if (type == "Classification") {
        ifelse(validation_outcome == y_levels[[2L]], 1, -1)
      } else {
        as.numeric(validation_outcome)
      }
      selection <- linad_select_leaves(
        model,
        validation_features,
        linad_design_matrix(validation_features, xlev),
        validation_y,
        type,
        smooth = settings[["smooth_validation_curve"]]
      )
      model@n_leaves <- as.integer(selection[["n_leaves"]])
      model@leaf_curve <- selection[["curve"]]
      if (verbosity > 0L) {
        info(
          "Selected ",
          model@n_leaves,
          ngettext(model@n_leaves, " leaf", " leaves"),
          " of ",
          length(model@steps),
          " on the validation set."
        )
      }
    }
  }
  # The frame's leaf flags describe the fully grown tree; at the selected size
  # the terminal set is `steps[[n_leaves]]`, so they are brought back into
  # agreement here rather than left to mislead every later reader.
  model@frame[["is_leaf"]] <- model@frame[["node"]] %in%
    model@steps[[model@n_leaves]]

  check_is_S7(model, LinearAdditiveTree)
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.LINADHyperparameters


# %% predict_super.LinearAdditiveTree ----
#' Predict from a Linear Additive Tree
#'
#' @param model `LinearAdditiveTree` object.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector: fitted values, or the probability of the positive
#' class.
#'
#' @keywords internal
#' @noRd
method(predict_super, LinearAdditiveTree) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  newdata <- as.data.frame(newdata)[, model@xnames, drop = FALSE]
  raw <- linad_raw_prediction(
    model,
    newdata,
    linad_design_matrix(newdata, model@xlev)
  )
  if (identical(model@type, "Classification")) {
    # The exponential-family logistic link that pairs with the {-1, +1} loss the
    # tree was grown against. The second factor level is the positive one.
    1 / (1 + exp(-2 * raw))
  } else {
    raw
  }
} # /rtemis::predict_super.LinearAdditiveTree


# %% varimp_super.LinearAdditiveTree ----
#' Variable importance from a Linear Additive Tree
#'
#' LINAD does two separable things to a feature, so it reports two measures
#' rather than blending them:
#'
#' \describe{
#'   \item{`importance`}{The feature's linear effect. Each leaf's coefficient is
#'     multiplied by the feature's training standard deviation, which puts every
#'     feature on the outcome's scale rather than on its own units, and averaged
#'     over leaves weighted by the training cases each holds -- a coefficient in
#'     a leaf of three cases should not count like one in a leaf of three
#'     hundred. This is the default plotted measure.}
#'   \item{`split_gain`}{The feature's partitioning effect: the loss reduction
#'     summed over the internal nodes that split on it. A feature can carry a
#'     large linear effect and never be split on, or the reverse, and averaging
#'     the two would hide exactly that.}
#' }
#'
#' A factor's dummy columns are folded back into the feature they came from.
#' Both measures describe the tree at its selected size.
#'
#' @param model `LinearAdditiveTree` object.
#'
#' @return `VariableImportance` object.
#'
#' @keywords internal
#' @noRd
method(varimp_super, LinearAdditiveTree) <- function(model, ...) {
  terminal <- model@steps[[model@n_leaves]]
  frame <- model@frame
  leaf_rows <- match(terminal, frame[["node"]])
  cases <- frame[["n"]][leaf_rows]
  share <- if (sum(cases) > 0) {
    cases / sum(cases)
  } else {
    rep(
      1 / length(cases),
      length(cases)
    )
  }
  # Column 1 is the intercept, whose `assign` code is 0.
  columns <- which(model@design_assign > 0L)
  scaled <- abs(model@coefficients[leaf_rows, columns, drop = FALSE]) *
    rep(model@design_scale[columns], each = length(leaf_rows))
  per_column <- drop(crossprod(share, scaled))
  source_feature <- model@xnames[model@design_assign[columns]]
  importance <- vapply(
    model@xnames,
    function(feature) sum(per_column[source_feature == feature]),
    numeric(1L)
  )

  # Only splits the selected tree can reach: a node below a selected terminal is
  # never visited by prediction and its gain is not part of this model.
  reachable <- linad_selected_nodes(frame, terminal)
  internal <- reachable[
    !is.na(frame[["left"]][reachable]) &
      !(frame[["node"]][reachable] %in% terminal)
  ]
  gain <- rep(0, length(model@xnames))
  names(gain) <- model@xnames
  for (row in internal) {
    children <- match(
      c(frame[["left"]][[row]], frame[["right"]][[row]]),
      frame[["node"]]
    )
    reduction <- frame[["loss"]][[row]] - sum(frame[["loss"]][children])
    feature <- frame[["split_feature"]][[row]]
    if (!is.na(feature) && reduction > 0) {
      gain[[feature]] <- gain[[feature]] + reduction
    }
  }

  VariableImportance(
    data.table(
      variable = model@xnames,
      importance = unname(importance),
      split_gain = unname(gain)
    )
  )
} # /rtemis::varimp_super.LinearAdditiveTree
