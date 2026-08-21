# train_GLMTree.R
# ::rtemis::
# 2026- EDG rtemis.org

# Model-based recursive partitioning via `partykit`.
# Parameter docs: https://cran.r-project.org/package=partykit -> ?mob_control
#
# One algorithm, two fitted-model classes: `lmtree` for a regression and
# `glmtree` for a binary classification, as `train_SPLS.R` does. They share a
# `train_` and take a `predict_super` each, because only the second needs
# `type = "response"`.

# %% glmtree_formula ----
#' The two-part formula `partykit` takes
#'
#' `outcome ~ regressors | partitioning variables`. Both halves default to every
#' feature. Naming them separately is the same choice `partykit` expresses
#' through the formula, made declaratively so a config can carry it.
#'
#' @param x tabular data: Training set, outcome last.
#' @param regressors Optional Character: Features in each leaf's model.
#' @param partitioning_variables Optional Character: Features to split on.
#'
#' @return A formula.
#'
#' @author EDG
#' @keywords internal
#' @noRd
glmtree_formula <- function(
  x,
  regressors = NULL,
  partitioning_variables = NULL
) {
  outcome_name <- names(x)[[NCOL(x)]]
  features <- setdiff(names(x), outcome_name)
  left <- regressors %||% features
  right <- partitioning_variables %||% features
  stats::as.formula(
    paste(
      outcome_name,
      "~",
      paste(left, collapse = " + "),
      "|",
      paste(right, collapse = " + ")
    )
  )
} # /rtemis::glmtree_formula


# %% glmtree_control ----
#' The control settings, as arguments to splice into the fitting call
#'
#' A plain list rather than a `mob_control()` object: `lmtree()` and `glmtree()`
#' take no `control` argument and forward their `...` to `mob_control()`
#' themselves, so the settings have to arrive as named arguments.
#'
#' Two of them are NULL here and `Inf` there: rtemis spells "no limit" as NULL,
#' which a JSON config can carry and a schema can type, where `mob_control()`
#' spells it `Inf`. `minsize` is NULL in both, meaning the same thing, and is
#' passed through.
#'
#' @param hyperparameters `GLMTreeHyperparameters` object.
#'
#' @return Named list of `mob_control()` arguments.
#'
#' @author EDG
#' @keywords internal
#' @noRd
glmtree_control <- function(hyperparameters) {
  list(
    alpha = hyperparameters[["alpha"]],
    bonferroni = hyperparameters[["bonferroni"]],
    minsize = hyperparameters[["minsize"]],
    maxdepth = hyperparameters[["maxdepth"]] %||% Inf,
    mtry = hyperparameters[["mtry"]] %||% Inf,
    trim = hyperparameters[["trim"]],
    breakties = hyperparameters[["breakties"]],
    prune = hyperparameters[["prune"]],
    restart = hyperparameters[["restart"]],
    dfsplit = hyperparameters[["dfsplit"]],
    numsplit = hyperparameters[["numsplit"]],
    catsplit = hyperparameters[["catsplit"]],
    ordinal = hyperparameters[["ordinal"]],
    vcov = hyperparameters[["vcov"]],
    nrep = hyperparameters[["nrep"]]
  )
} # /rtemis::glmtree_control


# %% train_.GLMTreeHyperparameters ----
#' Train a GLMTree
#'
#' Model-based recursive partitioning: a tree carrying a generalized linear
#' model in each leaf, grown by testing whether a node's coefficients are
#' constant across each candidate partitioning variable.
#'
#' A regression is fitted by `partykit::lmtree()` and a binary classification by
#' `partykit::glmtree()` with a binomial family, so the algorithm produces two
#' fitted-model classes and each has its own `predict_super()`.
#'
#' The "train_*" functions train a single model.
#' Use [train] for tuning and testing using nested cross-validation.
#'
#' @param hyperparameters `GLMTreeHyperparameters` object: make using [setup_GLMTree].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used.
#' @param execution_config `ExecutionConfig` object: Not used.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return List with the fitted `lmtree` or `glmtree` and a NULL preprocessor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, GLMTreeHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("partykit")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(x = x, allow_missing = FALSE, verbosity = verbosity)
  type <- supervised_type(x)
  if (type == "Classification" && nlevels(outcome(x)) > 2L) {
    rtemis.core::abort(
      "GLMTree does not support multiclass classification.",
      class = "rtemis_unsupported_error"
    )
  }
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }

  # Train ----
  formula <- glmtree_formula(
    x,
    hyperparameters[["regressors"]],
    hyperparameters[["partitioning_variables"]]
  )
  x <- as.data.frame(x)
  arguments <- c(
    list(formula = formula, data = x, weights = weights),
    glmtree_control(hyperparameters)
  )
  model <- if (type == "Regression") {
    do.call(partykit::lmtree, arguments)
  } else {
    do.call(
      partykit::glmtree,
      c(arguments, list(family = stats::binomial))
    )
  }
  check_inherits(model, if (type == "Regression") "lmtree" else "glmtree")
  if (verbosity > 0L) {
    n_leaves <- partykit::width(model)
    info(
      "Grew a tree of ",
      n_leaves,
      ngettext(n_leaves, " leaf", " leaves"),
      "."
    )
  }
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.GLMTreeHyperparameters


# %% predict_super.class_lmtree ----
#' Predict from a GLMTree regression
#'
#' `type = "response"` is passed **explicitly**. `predict.modelparty()` declares
#' `type = "node"` as its default and returns node ids; a regression tree only
#' gets fitted values because `predict.lmtree()` overrides that. Relying on
#' which method dispatches would make node ids the silent answer if that ever
#' changed, and a vector of small integers is a plausible-looking prediction.
#'
#' @param model `lmtree` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector: fitted values.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_lmtree) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  as.numeric(stats::predict(
    model,
    newdata = as.data.frame(newdata),
    type = "response"
  ))
} # /rtemis::predict_super.class_lmtree


# %% predict_super.class_glmtree ----
#' Predict from a GLMTree classification
#'
#' `type = "response"` gives the probability of the **second** factor level,
#' which is rtemis' convention, so nothing is negated here. Verified rather than
#' assumed: `glmtree()` codes a factor outcome with the first level as the
#' reference, so the fitted probability is of the other one.
#'
#' @param model `glmtree` model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector: probability of the positive class.
#'
#' @keywords internal
#' @noRd
method(predict_super, class_glmtree) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  as.numeric(stats::predict(
    model,
    newdata = as.data.frame(newdata),
    type = "response"
  ))
} # /rtemis::predict_super.class_glmtree


# %% varimp_super.class_lmtree ----
#' Variable importance from a GLMTree regression
#'
#' `partykit` has a variable-importance measure for `cforest` and for
#' constant-fit trees, and none for a model-based tree: `varimp()` has no
#' `modelparty` method. Each leaf carries its own coefficients over the same
#' regressors, and there is no accepted way to reduce a set of per-subgroup
#' models to one number per feature -- a large coefficient in one leaf and its
#' negative in another is a real effect, and averaging reports nothing.
#'
#' NULL is therefore the honest answer, and `explain()` is the route to a
#' per-feature measure: `get_varimp(explain(mod, newdata))`.
#'
#' @param model `lmtree` model.
#'
#' @return NULL.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_lmtree) <- function(model, ...) {
  NULL
} # /rtemis::varimp_super.class_lmtree


# %% varimp_super.class_glmtree ----
#' Variable importance from a GLMTree classification
#'
#' NULL, for the reason `varimp_super.class_lmtree` gives.
#'
#' @param model `glmtree` model.
#'
#' @return NULL.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_glmtree) <- function(model, ...) {
  NULL
} # /rtemis::varimp_super.class_glmtree
