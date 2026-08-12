# 250_Explanation.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7

# Description
# `ExplanationConfig` and its subclasses configure per-case explanation; they
# are set by `setup_*()` and consumed by `explain()`. `Explanation` and its
# subclasses hold the result.
#
# Both hierarchies are tagged by `@type`, and they subclass on different axes:
# the result by *kind* of explanation, the config by *how* it is estimated. A
# result carrying no kind tag would make a second kind of explanation a
# breaking change for every reader, which is why `SHAP` sits under an abstract
# `Explanation` rather than standing alone.

# %% Constants ----
# Kinds of explanation. One today; the tag is what lets a second arrive without
# breaking a reader.
EXPLANATION_TYPES <- c("SHAP")

# What the user asks for. "auto" reads the algorithm's default from
# `explanation_methods()`; the concrete estimator that ran is recorded on the
# result, so asked and resolved are never confused.
SHAP_ESTIMATORS <- c("auto", "exact", "kernel")

# The concrete estimators, as recorded on a result. `TreeSHAP` covers CART as
# well as the LightGBM family: TreeSHAP over a single tree is a tree traversal
# and is exact, so it needs no name of its own. A decision *path* is a different
# kind of explanation rather than another way of estimating this one.
SHAP_RESOLVED_ESTIMATORS <- c(
  "TreeSHAP",
  "LinearSHAP",
  "KernelSHAP",
  "GAMTerms",
  "MARSBasis",
  "HALBasis",
  "Isotonic",
  # Delegating rather than computing: the expert that predicted a case explains
  # it, with whichever estimator that expert's own algorithm has.
  "ExpertSHAP"
)

# The value function each estimator computes when `perturbation` is left NULL.
#
# Per estimator rather than one package-wide default, because the exact tier is
# only free where the backend already computes it: LightGBM's
# `predict(type = "contrib")` is path-dependent, weighting by training coverage,
# and offers no background argument -- so an interventional default would make
# the flagship algorithm's default path the expensive one. The resolved value is
# recorded on every result, which is what keeps two algorithms' explanations
# from being compared as if they answered the same question.
SHAP_ESTIMATOR_PERTURBATION <- c(
  TreeSHAP = "conditional",
  LinearSHAP = "interventional",
  GAMTerms = "interventional",
  MARSBasis = "interventional",
  HALBasis = "interventional",
  Isotonic = "interventional",
  KernelSHAP = "interventional",
  # Forced on every expert, so one value function describes the whole result.
  # The experts a conditional SuperLearner routes to are simple by design, and
  # a simple model's exact estimator is interventional.
  ExpertSHAP = "interventional"
)

# The two value functions, which answer different questions and disagree
# whenever features are correlated. Neither is the correct one: the choice is
# the question being asked (Chen, Janizek, Lundberg and Lee, 2020).
SHAP_PERTURBATIONS <- c("interventional", "conditional")

# The scale contributions are additive on. Regression has only the outcome
# scale, which is "margin" here; classification adds the probability scale, on
# which contributions do NOT sum to the prediction.
SHAP_SCALES <- c("margin", "probability")

# Which feature space a contribution matrix is indexed by. `supervised_features()`
# passes through all three in order, and only the first two are ever reported:
# "encoded" values are reachable on the result rather than returned.
EXPLANATION_SPACES <- c("input", "model", "encoded")

# Exact TreeSHAP over a single tree enumerates every coalition of the features
# it splits on, so the work doubles with each one. Beyond this the kernel
# estimator, which samples, is the right tool and is what the refusal names.
CART_SHAP_MAX_FEATURES <- 14L

# Conditional-distribution estimators offered by `shapr`. `independence` is
# deliberately absent: it is shapr's spelling of the *interventional* value
# function, which `@perturbation` already says, and admitting both would give
# one decision two ways to be stated.
SHAP_APPROACHES <- c(
  "empirical",
  "gaussian",
  "copula",
  "ctree",
  "vaeac",
  "categorical",
  "timeseries"
)


# %% ExplanationConfig ----
#' ExplanationConfig
#'
#' @description
#' Superclass for per-case explanation configuration.
#'
#' @field type Character: Kind of explanation.
#'
#' @author EDG
#' @noRd
ExplanationConfig <- new_class(
  name = "ExplanationConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character
  )
) # /rtemis::ExplanationConfig


# %% serializable_props.ExplanationConfig ----
# Type-specific settings serialize as siblings of `type`, as for
# `ResamplerConfig`: the wire shape is one flat object per kind, not a nested
# `config`.
method(serializable_props, ExplanationConfig) <- function(x) {
  c(list(type = x@type), config_prop_values(x, ExplanationConfig))
} # /rtemis::serializable_props.ExplanationConfig


# %% `$`.ExplanationConfig ----
# Make S7 properties `$`-accessible
method(`$`, ExplanationConfig) <- function(x, name) {
  prop(x, name)
}


# %% `[[`.ExplanationConfig ----
# Make S7 properties `[[`-accessible
method(`[[`, ExplanationConfig) <- function(x, name) {
  prop(x, name)
}


# %% repr.ExplanationConfig ----
#' repr ExplanationConfig
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, ExplanationConfig) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x)[-1],
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.ExplanationConfig


# %% print.ExplanationConfig ----
#' Print ExplanationConfig
#'
#' @param x `ExplanationConfig` object.
#' @param pad Integer: Number of spaces to pad the output with.
#' @param output_type Character \{"ansi", "html", "plain"\}: Output type.
#'
#' @author EDG
#' @noRd
method(print, ExplanationConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.ExplanationConfig


# %% SHAPConfig ----
#' @title SHAPConfig
#'
#' @description
#' `ExplanationConfig` subclass for Shapley additive explanations.
#'
#' @details
#' One leaf rather than one per estimator. Every registered algorithm answers
#' `explain()`, with the exact estimator used wherever one exists, so the
#' estimator is normally resolved rather than chosen: making it the class would
#' force a user to name the very thing they were relying on the package to pick.
#' `@estimator` is the override, and the kernel-only settings are gated by
#' `applies_when` rather than split into a second class.
#'
#' @author EDG
#' @noRd
SHAPConfig <- new_class(
  name = "SHAPConfig",
  parent = ExplanationConfig,
  properties = list(
    type = prop_algorithm("SHAP"),
    estimator = prop_string(
      "auto",
      enum = SHAP_ESTIMATORS,
      description = "Which estimator to use. 'auto' reads the algorithm's default."
    ),
    perturbation = prop_string(
      NULL,
      enum = SHAP_PERTURBATIONS,
      nullable = TRUE,
      default_on_null = TRUE,
      description = "Value function. NULL = the resolved estimator's documented default."
    ),
    scale = prop_string(
      NULL,
      enum = SHAP_SCALES,
      nullable = TRUE,
      default_on_null = TRUE,
      description = "Scale contributions are additive on. NULL = set from outcome type."
    ),
    background_n = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Cases to subsample from the background. NULL = use all of it."
    ),
    # A coalition is a feature subset, and how many are evaluated is what
    # decides whether a kernel estimate is exact or sampled -- so it belongs
    # here, unlike a batching knob, which only changes how long it takes.
    # Passed to shapr as `max_n_coalitions`.
    n_coalitions = prop_integer(
      NULL,
      min = 2L,
      nullable = TRUE,
      applies_when = list(estimator = "kernel"),
      description = "Most feature subsets to evaluate. NULL lets the backend decide."
    ),
    approach = prop_string(
      NULL,
      enum = SHAP_APPROACHES,
      nullable = TRUE,
      applies_when = list(estimator = "kernel", perturbation = "conditional"),
      description = "Conditional-distribution estimator. NULL = the backend's default."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  ),
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::SHAPConfig


# %% desc.ExplanationConfig ----
method(desc, ExplanationConfig) <- function(x) {
  switch(
    x@type,
    SHAP = paste0(
      "Shapley additive explanations (",
      x@estimator,
      if (!is.null(x@perturbation)) paste0(", ", x@perturbation),
      ")"
    ),
    x@type
  )
} # /rtemis::desc.ExplanationConfig


# %% setup_SHAP ----
#' Setup SHAP Explanation
#'
#' Configure per-case Shapley additive explanations for `explain()`.
#'
#' @details
#' Three choices here are semantics rather than tuning, and each is recorded on
#' the returned explanation so that two of them can never be silently compared.
#'
#' **`perturbation` is the question being asked.** `"interventional"` breaks the
#' dependence between features, attributing to what the model *uses*: a feature
#' the model ignores gets zero even if it predicts the outcome perfectly.
#' `"conditional"` respects the joint distribution, attributing to what the
#' case's features *tell you*, so a correlated proxy for a used feature receives
#' credit. Neither is the correct one -- debugging a model wants the first,
#' reasoning about a subject wants the second.
#'
#' **`scale` decides whether the contributions add up.** They are additive on
#' the scale they are computed on. For regression that is the outcome and there
#' is nothing to choose. For classification the model's additive scale is the
#' margin, and probability is a nonlinear transform of it, so contributions on
#' the probability scale do **not** sum to the predicted probability.
#'
#' **`estimator` is normally best left alone.** `"auto"` uses the exact
#' estimator wherever the algorithm has one and the model-agnostic kernel
#' estimator otherwise; `explanation_methods()` reports which applies where.
#'
#' `approach` and `n_coalitions` apply only to the kernel estimator, and
#' `approach` only when `perturbation` is `"conditional"` -- the interventional
#' answer is not one of several conditional estimators, it is the absence of
#' conditioning.
#'
#' Setting either therefore requires `estimator = "kernel"` rather than
#' `"auto"`, even for an algorithm `"auto"` would send to the kernel estimator
#' anyway. Tuning an estimator is a decision to use it, and the alternative --
#' accepting settings that a resolved exact estimator would then ignore --
#' would drop them silently.
#'
#' @param estimator Character \{"auto", "exact", "kernel"\}: Which estimator to
#' use.
#' @param perturbation Optional Character \{"interventional", "conditional"\}:
#' Value function. NULL uses the resolved estimator's documented default.
#' @param scale Optional Character \{"margin", "probability"\}: Scale the
#' contributions are additive on. NULL sets it from the outcome type.
#' @param background_n Optional Integer [1, Inf): Cases to subsample from the
#' background data. NULL uses all of it.
#' @param n_coalitions Optional Integer [2, Inf): Most feature subsets the
#' kernel estimator evaluates. NULL lets the backend decide.
#' @param approach Optional Character \{"empirical", "gaussian", "copula",
#' "ctree", "vaeac", "categorical", "timeseries"\}: Conditional-distribution
#' estimator used by the kernel estimator.
#' @param seed Optional Integer [0, Inf): Random seed.
#'
#' @return `SHAPConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_SHAP()
#' setup_SHAP(perturbation = "conditional")
#' setup_SHAP(estimator = "kernel", perturbation = "conditional", approach = "ctree")
setup_SHAP <- function(
  estimator = "auto",
  perturbation = NULL,
  scale = NULL,
  background_n = NULL,
  n_coalitions = NULL,
  approach = NULL,
  seed = NULL
) {
  estimator <- match_arg(estimator, SHAP_ESTIMATORS)
  background_n <- clean_posint(background_n)
  n_coalitions <- clean_posint(n_coalitions)
  seed <- clean_int(seed)
  SHAPConfig(
    estimator = estimator,
    perturbation = perturbation,
    scale = scale,
    background_n = background_n,
    n_coalitions = n_coalitions,
    approach = approach,
    seed = seed
  )
} # /rtemis::setup_SHAP


# %% Explanation ----
#' Explanation
#'
#' @description
#' Superclass for per-case explanations of a fitted model.
#'
#' @details
#' What every kind of per-case explanation must state regardless of how it is
#' computed: which model it explains, which feature space its numbers are
#' indexed by, and which data it was computed against. The last is not
#' bookkeeping -- an attribution is relative to a background, two explanations
#' of one model against different backgrounds are not comparable, and nothing
#' in the returned numbers says so.
#'
#' @author EDG
#' @noRd
Explanation <- new_class(
  name = "Explanation",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character,
    # The learning algorithm explained, not the estimator that explained it.
    algorithm = class_character,
    config = ExplanationConfig,
    # Which of the three spaces `supervised_features()` passes through the
    # numbers are indexed by. Reported rather than assumed: how far back an
    # attribution can be carried depends on what preprocessing ran, so a claim
    # about the space has to be made per explanation.
    space = prop_string(
      NULL,
      enum = EXPLANATION_SPACES,
      nullable = TRUE,
      description = "Feature space the contributions are indexed by."
    ),
    feature_names = class_character,
    # Identity of the cases explained, and of the background they were
    # explained against. Both, because a mismatch in either makes two
    # explanations incomparable, and neither is visible in the numbers.
    data_fingerprint = NULL | DataFingerprint,
    background_fingerprint = NULL | DataFingerprint
  )
) # /rtemis::Explanation


# %% SHAP ----
#' SHAP
#'
#' @description
#' `Explanation` subclass holding Shapley additive contributions.
#'
#' @details
#' `@phi` is a named list of `n x p` numeric matrices, one per class. Regression
#' and binary classification are length-1 -- binary is explained on the positive
#' class, the negative being its exact negation -- so binary and multiclass
#' share one structure and no consumer has to special-case them. This is the
#' rule `prob_matrix()` already applies to predictions: two names for one
#' quantity must not have two shapes.
#'
#' `@phi_encoded` holds the same contributions in the fitted backend's own
#' column space, and is NULL when no encoding intervened and the two would be
#' identical.
#'
#' @author EDG
#' @noRd
SHAP <- new_class(
  name = "SHAP",
  parent = Explanation,
  properties = list(
    type = prop_algorithm("SHAP"),
    phi = class_list,
    phi_encoded = NULL | class_list,
    # `E[f(x)]` over the background: the quantity `phi` is a deviation *from*,
    # without which the contributions reconstruct nothing.
    #
    # One row per case, parallel to `predicted`, even though almost every
    # estimator's baseline is the same for every case. A model that routes each
    # case to a different sub-model has a different expected prediction per
    # case -- `ConditionalSuperLearner` is one -- and one shape for both is
    # worth more than the rows it repeats, for the reason `phi` is a list even
    # when it holds one matrix.
    baseline = class_matrix,
    # The predictions the contributions decompose, on `@scale`. Carried so the
    # object can check its own additivity and a waterfall can label its endpoint
    # without querying the model again.
    predicted = class_matrix,
    scale = prop_string(
      NULL,
      enum = SHAP_SCALES,
      nullable = TRUE,
      description = "Scale the contributions are additive on."
    ),
    perturbation = prop_string(
      NULL,
      enum = SHAP_PERTURBATIONS,
      nullable = TRUE,
      description = "Value function used, resolved."
    ),
    estimator = prop_string(
      NULL,
      enum = SHAP_RESOLVED_ESTIMATORS,
      nullable = TRUE,
      description = "Concrete estimator that ran."
    ),
    # Whether these are the Shapley values for the declared value function or an
    # estimate of them. A sampled kernel run is not exact; neither is a
    # fixed-weight decomposition of an ensemble whose weights vary with the
    # case. Both are legitimate answers, and the difference is not visible in
    # the numbers.
    exact = prop_boolean(
      FALSE,
      description = "TRUE if the values are exact for the declared value function."
    )
  ),
  validator = function(self) {
    if (length(self@phi) == 0L) {
      return("@phi must hold at least one contribution matrix.")
    }
    if (is.null(names(self@phi)) || !all(nzchar(names(self@phi)))) {
      return(
        "@phi must be named: one entry per class, or one for a regression outcome."
      )
    }
    if (!all(vapply(self@phi, is.matrix, logical(1L)))) {
      return("@phi entries must be matrices.")
    }
    if (!all(vapply(self@phi, is.numeric, logical(1L)))) {
      return("@phi entries must be numeric.")
    }
    dims <- lapply(self@phi, dim)
    if (length(unique(dims)) > 1L) {
      return("@phi entries must all have the same dimensions.")
    }
    if (ncol(self@phi[[1L]]) != length(self@feature_names)) {
      return(paste0(
        "@phi has ",
        ncol(self@phi[[1L]]),
        " columns but @feature_names names ",
        length(self@feature_names),
        "."
      ))
    }
    expected <- c(nrow(self@phi[[1L]]), length(self@phi))
    if (!identical(dim(self@predicted), expected)) {
      return(
        "@predicted must be a matrix of one row per case and one column per entry of @phi."
      )
    }
    if (!identical(dim(self@baseline), expected)) {
      return(
        "@baseline must be a matrix of one row per case and one column per entry of @phi."
      )
    }
    if (!identical(colnames(self@baseline), names(self@phi))) {
      return("@baseline columns must match @phi names, in order.")
    }
    NULL
  }
) # /rtemis::SHAP


# %% n_cases.SHAP ----
#' Number of cases a `SHAP` explains
#'
#' @param x `SHAP` object.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shap_n_cases <- function(x) {
  nrow(x@phi[[1L]])
} # /rtemis::shap_n_cases


# %% `$`.Explanation ----
# Make Explanation props `$`-accessible
method(`$`, Explanation) <- function(x, name) {
  prop(x, name)
}


# %% `.DollarNames`.Explanation ----
method(`.DollarNames`, Explanation) <- function(x, pattern = "") {
  grep(pattern, names(props(x)), value = TRUE)
}


# %% `[[`.SHAP ----
# Index the contributions by class, which is what a list keyed by class is for.
method(`[[`, SHAP) <- function(x, index) {
  x@phi[[index]]
}


# %% repr.SHAP ----
#' repr `SHAP`
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, SHAP) <- function(x, pad = 0L, output_type = NULL) {
  n_classes <- length(x@phi)
  paste0(
    repr_S7name("SHAP", pad = pad, output_type = output_type),
    strrep(" ", pad),
    fmt(
      x@estimator,
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    " contributions for ",
    fmt(
      shap_n_cases(x),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    ngettext(shap_n_cases(x), " case", " cases"),
    " x ",
    fmt(
      length(x@feature_names),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    ngettext(length(x@feature_names), " feature", " features"),
    if (n_classes > 1L) {
      paste0(
        " x ",
        fmt(
          n_classes,
          col = highlight_col,
          bold = TRUE,
          output_type = output_type
        ),
        " classes"
      )
    },
    ", on the ",
    fmt(x@scale, col = highlight_col, bold = TRUE, output_type = output_type),
    " scale\n"
  )
} # /rtemis::repr.SHAP


# %% print.SHAP ----
#' Print `SHAP`
#'
#' @param x `SHAP` object.
#' @param output_type Character \{"ansi", "html", "plain"\}: Output type.
#'
#' @author EDG
#' @noRd
method(print, SHAP) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.SHAP


# %% desc.SHAP ----
method(desc, SHAP) <- function(x) {
  paste0(
    x@estimator,
    " contributions for ",
    shap_n_cases(x),
    ngettext(shap_n_cases(x), " case", " cases"),
    ", ",
    x@perturbation,
    ", on the ",
    x@scale,
    " scale"
  )
} # /rtemis::desc.SHAP


# %% to_json.SHAP ----
#' to_json `SHAP`
#'
#' Publishes what the explanation *is* -- estimator, value function, scale,
#' space, the baselines, and the identities of the data and background -- but
#' not the contribution matrices themselves, which are `n x p x k` bulk data.
#' `Supervised` draws the same line for its prediction vectors, and for the same
#' reason: a control-plane response is not where a per-case matrix belongs.
#'
#' The mean absolute contribution per feature is published, being the summary a
#' consumer would otherwise compute from the whole matrix in order to rank
#' features.
#'
#' @param x `SHAP` object.
#'
#' @return Named list. Pass to `jsonlite::toJSON(auto_unbox = TRUE)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_json, SHAP) <- function(x, ...) {
  mean_abs <- lapply(x@phi, function(m) {
    stats::setNames(colMeans(abs(m)), x@feature_names)
  })
  out <- list(
    .class = S7_class(x)@name,
    type = x@type,
    algorithm = x@algorithm,
    description = desc(x),
    estimator = x@estimator,
    perturbation = x@perturbation,
    scale = x@scale,
    space = x@space,
    exact = x@exact,
    feature_names = x@feature_names,
    n_features = length(x@feature_names),
    n_cases = shap_n_cases(x),
    classes = names(x@phi),
    # The mean over cases, which is the baseline itself for every estimator
    # whose baseline does not vary -- almost all of them. The per-case matrix
    # stays on the object, being bulk like the contributions.
    baseline = as.list(colMeans(x@baseline)),
    mean_abs_contribution = lapply(mean_abs, as.list),
    config = .to_json_value(x@config),
    data_fingerprint = .to_json_value(x@data_fingerprint),
    background_fingerprint = .to_json_value(x@background_fingerprint)
  )
  Filter(Negate(is.null), out)
} # /rtemis::to_json.SHAP
