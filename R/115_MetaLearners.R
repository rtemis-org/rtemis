# 115_MetaLearners.R
# ::rtemis::
# 2026- EDG rtemis.org

# The meta learner `Hyperparameters` classes live here rather than in
# `070_Hyperparameters.R` because they declare a `ResamplerConfig` property, and
# files are sourced in load order: `100_Resampler.R` has not run yet at 070.
# Same reason `SuperConfig` sits at 190.

# %% name_base_learners ----
#' Give a meta learner's library unique names
#'
#' The names key everything downstream: the level-one columns, the feature
#' groups, the CV risk table, and the pairing of a record's entries against the
#' input's. A user may supply them, and unnamed entries fall back to their
#' algorithm with a numeric suffix where that collides.
#'
#' @param base_learners List of `Hyperparameters` objects.
#'
#' @return The list, named.
#'
#' @author EDG
#' @keywords internal
#' @noRd
name_base_learners <- function(base_learners) {
  if (!is.list(base_learners)) {
    base_learners <- list(base_learners)
  }
  supplied <- names(base_learners)
  if (is.null(supplied)) {
    supplied <- rep("", length(base_learners))
  }
  out <- character(length(base_learners))
  for (i in seq_along(base_learners)) {
    if (nzchar(supplied[[i]])) {
      out[[i]] <- supplied[[i]]
      next
    }
    learner <- base_learners[[i]]
    stem <- if (S7_inherits(learner, Hyperparameters)) {
      learner@algorithm
    } else {
      paste0("learner", i)
    }
    candidate <- stem
    suffix <- 1L
    while (candidate %in% out[seq_len(i - 1L)] || candidate %in% supplied[-i]) {
      suffix <- suffix + 1L
      candidate <- paste0(stem, "_", suffix)
    }
    out[[i]] <- candidate
  }
  names(base_learners) <- out
  base_learners
} # /rtemis::name_base_learners


# %% MetaLearnerHyperparameters ----
#' @title MetaLearnerHyperparameters
#'
#' @description
#' Abstract superclass for meta learners: algorithms whose model is built from
#' the fits of a library of base learners, combined by a meta learner. Not
#' registered as an algorithm and not constructible; the concrete subclasses
#' differ in how the meta learner is used. `StackedLearnerHyperparameters`
#' combines the base learners' predictions, `ConditionalSuperLearnerHyperparameters`
#' routes each case to one of them.
#'
#' `base_learners`, `meta_learner` and `inner_resampling_config` hold config
#' objects and so carry no `PropertySpec`: each publishes its own schema, which
#' the generator references.
#'
#' @author EDG
#' @keywords internal
#' @noRd
MetaLearnerHyperparameters <- new_class(
  name = "MetaLearnerHyperparameters",
  parent = Hyperparameters,
  abstract = TRUE,
  properties = list(
    base_learners = new_property(
      class_list,
      default = quote(name_base_learners(list(
        setup_GLM(),
        setup_GLMNET(),
        setup_Ranger()
      )))
    ),
    meta_learner = new_property(Hyperparameters, default = quote(setup_NNLS())),
    inner_resampling_config = new_property(
      ResamplerConfig,
      default = quote(setup_Resampler(n_resamples = 10L, type = "KFold"))
    ),
    expand_search_spaces = prop_boolean(
      TRUE,
      description = "Expand a base learner's search space into one library entry per grid combination, rather than tuning it by inner resampling."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    if (length(self@base_learners) < 2L) {
      return(
        "@base_learners must hold at least 2 learners; with one there is nothing to combine."
      )
    }
    nms <- names(self@base_learners)
    if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms) > 0L) {
      return("@base_learners must have unique, non-empty names.")
    }
    # The names become column names of the level-one data the meta learner is
    # trained on, and a formula-based learner there would silently see the
    # mangled form `make.names()` produces.
    if (!identical(make.names(nms), nms)) {
      return(paste0(
        "@base_learners names must be syntactically valid; these are not: ",
        paste(nms[make.names(nms) != nms], collapse = ", "),
        "."
      ))
    }
    is_hp <- vapply(
      self@base_learners,
      function(learner) S7_inherits(learner, Hyperparameters),
      logical(1L)
    )
    if (!all(is_hp)) {
      return(paste0(
        "@base_learners must hold `Hyperparameters` objects (make them with `setup_*()`); these do not: ",
        paste(nms[!is_hp], collapse = ", "),
        "."
      ))
    }
    NULL
  }
) # /rtemis::MetaLearnerHyperparameters


# %% repr.MetaLearnerHyperparameters ----
#' Repr MetaLearnerHyperparameters
#'
#' The default `Hyperparameters` repr prints `@hyperparameters` through
#' `repr_ls`, which would render each nested `Hyperparameters` object in full and
#' bury the settings that belong to the meta learner itself. This lists the
#' library by name and algorithm instead.
#'
#' @param x `MetaLearnerHyperparameters` object.
#' @param pad Integer: Left padding for printed output.
#' @param maxlength Integer: Maximum length of items to show before truncating.
#' @param limit Integer: Limit number of items to show. `-1` means no limit.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @author EDG
#' @noRd
method(repr, MetaLearnerHyperparameters) <- function(
  x,
  pad = 0L,
  maxlength = -1L,
  limit = -1L,
  output_type = NULL
) {
  # Derived for display only: the stored objects are never rewritten (a repr
  # that mutates would make printing an object change it).
  summarized <- x@hyperparameters
  summarized[["base_learners"]] <- vapply(
    x@base_learners,
    function(learner) learner@algorithm,
    character(1L)
  )
  summarized[["meta_learner"]] <- x@meta_learner@algorithm
  summarized[["inner_resampling_config"]] <- desc(x@inner_resampling_config)
  paste0(
    repr_S7name(
      paste0(x@algorithm, "Hyperparameters"),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(
      list(
        hyperparameters = summarized,
        tunable_hyperparameters = x@tunable_hyperparameters,
        fixed_hyperparameters = x@fixed_hyperparameters,
        tuned = x@tuned,
        resampled = x@resampled,
        n_workers = x@n_workers
      ),
      pad = pad,
      maxlength = maxlength,
      limit = limit,
      output_type = output_type
    )
  )
} # /rtemis::repr.MetaLearnerHyperparameters


# %% META_PARTITIONING_RESAMPLERS ----
# Resamplers that hold out every case exactly once. A meta learner pairs each
# case with base learners trained without it, so a case held out never has no
# cross-validated prediction, and one held out twice would have two.
META_PARTITIONING_RESAMPLERS <- c("KFold", "LOOCV")


# %% check_meta_hyperparameters ----
#' Data-dependent checks shared by every meta learner
#'
#' The declarative `data_bound` checks, plus the one constraint the vocabulary
#' cannot express: the inner resampler must partition the cases.
#'
#' A plain function rather than a method so a subclass with checks of its own can
#' run these first. S7's `super()` would do the same job at the cost of naming
#' the parent class at every call site.
#'
#' @param hyperparameters `MetaLearnerHyperparameters` object.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_meta_hyperparameters <- function(hyperparameters, x) {
  check_data_bounds(hyperparameters, x)
  resampler_type <- hyperparameters@inner_resampling_config@type
  if (!resampler_type %in% META_PARTITIONING_RESAMPLERS) {
    rtemis.core::abort(
      hyperparameters@algorithm,
      " needs each case held out exactly once, so `inner_resampling_config` ",
      "must be one of: ",
      paste(META_PARTITIONING_RESAMPLERS, collapse = ", "),
      "; got '",
      resampler_type,
      "'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(hyperparameters)
} # /rtemis::check_meta_hyperparameters


# %% validate_hyperparameters.MetaLearnerHyperparameters ----
#' @author EDG
#' @keywords internal
#' @noRd
method(
  validate_hyperparameters,
  MetaLearnerHyperparameters
) <- function(hyperparameters, x) {
  check_meta_hyperparameters(hyperparameters, x)
} # /rtemis::validate_hyperparameters.MetaLearnerHyperparameters


# %% StackedLearnerHyperparameters ----
#' @title StackedLearnerHyperparameters
#'
#' @description
#' Abstract superclass for the stacking meta learners, which combine the base
#' learners' cross-validated predictions with a meta learner fitted on them. Not
#' registered as an algorithm and not constructible: `SuperLearnerHyperparameters`
#' and `ModalityStackingHyperparameters` are the concrete subclasses, and this
#' class carries what they share, including `train_()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
StackedLearnerHyperparameters <- new_class(
  name = "StackedLearnerHyperparameters",
  parent = MetaLearnerHyperparameters,
  abstract = TRUE,
  properties = list(
    discrete = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Keep the single library entry with the lowest cross-validated risk rather than the weighted combination."
    )
  )
) # /rtemis::StackedLearnerHyperparameters


# %% SuperLearnerHyperparameters ----
#' @title SuperLearnerHyperparameters
#'
#' @description
#' Hyperparameters subclass for the SuperLearner.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SuperLearnerHyperparameters <- new_class(
  name = "SuperLearnerHyperparameters",
  parent = StackedLearnerHyperparameters,
  properties = list(
    algorithm = prop_algorithm("SuperLearner")
  )
) # /rtemis::SuperLearnerHyperparameters


# %% setup_SuperLearner ----
#' Setup SuperLearner Hyperparameters
#'
#' Setup hyperparameters for a SuperLearner: the cross-validated stacked
#' ensemble of van der Laan, Polley & Hubbard (2007).
#'
#' @details
#' Each base learner is fitted on every training fold of `inner_resampling_config`
#' and predicts that fold's held-out cases, producing one cross-validated
#' prediction per case per learner. The meta learner is fitted on those
#' predictions against the outcome, and its coefficients are the ensemble
#' weights. Every base learner is then refitted on the whole training set, so a
#' prediction is the meta learner applied to the base learners' predictions.
#'
#' **Search spaces become library entries.** A base learner holding more than one
#' value for a tunable hyperparameter is expanded into one library entry per
#' combination, and the ensemble weights choose between them -- the ensemble's
#' own cross-validation is the model selection, so no inner tuning is needed.
#' Set `expand_search_spaces = FALSE` to tune each such learner by inner
#' resampling within every fold instead, which is far more expensive. A learner
#' that tunes *itself* (GLMNET choosing `lambda` by `cv.glmnet`) is unaffected
#' either way.
#'
#' The cross-validated predictions and the resampler are kept on the fitted
#' model, which is what a cross-fitting estimator needs from it.
#'
#' Supports regression and binary classification.
#'
#' @param base_learners List of `Hyperparameters` objects: The library. Names
#' label the level-one predictions and the reported weights; unnamed entries are
#' named after their algorithm.
#' @param meta_learner `Hyperparameters` object: Learner fitted on the base
#' learners' cross-validated predictions. The default is non-negative least
#' squares normalized to sum to 1, i.e. a convex combination.
#' @param inner_resampling_config `ResamplerConfig` object: Cross-validation
#' scheme used to build the level-one predictions.
#' @param discrete (Tunable) Logical: If TRUE, keep the single lowest-risk
#' library entry rather than the weighted combination (the discrete
#' SuperLearner).
#' @param expand_search_spaces Logical: If TRUE, expand a base learner's search
#' space into one library entry per combination.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in
#' classification.
#'
#' @return SuperLearnerHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' superlearner_hyperparams <- setup_SuperLearner(
#'   base_learners = list(setup_GLM(), setup_CART())
#' )
#' superlearner_hyperparams
setup_SuperLearner <- function(
  base_learners = list(setup_GLM(), setup_GLMNET(), setup_Ranger()),
  meta_learner = setup_NNLS(),
  inner_resampling_config = setup_Resampler(n_resamples = 10L, type = "KFold"),
  # tunable
  discrete = FALSE,
  # fixed
  expand_search_spaces = TRUE,
  ifw = FALSE
) {
  SuperLearnerHyperparameters(
    base_learners = name_base_learners(base_learners),
    meta_learner = meta_learner,
    inner_resampling_config = inner_resampling_config,
    discrete = discrete,
    expand_search_spaces = expand_search_spaces,
    ifw = ifw
  )
} # /rtemis::setup_SuperLearner


# %% ModalityStackingHyperparameters ----
#' @title ModalityStackingHyperparameters
#'
#' @description
#' Hyperparameters subclass for modality stacking: a SuperLearner whose base
#' learners each see one group of features.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ModalityStackingHyperparameters <- new_class(
  name = "ModalityStackingHyperparameters",
  parent = StackedLearnerHyperparameters,
  properties = list(
    algorithm = prop_algorithm("ModalityStacking"),
    feature_groups = prop_map(
      prop_string(NULL, vector = TRUE, nullable = TRUE),
      nullable = TRUE,
      data_bound = "feature_names",
      data_dependent = TRUE,
      description = "Features each base learner sees, keyed by base learner name."
    )
  )
) # /rtemis::ModalityStackingHyperparameters


# %% validate_hyperparameters.ModalityStackingHyperparameters ----
#' Check modality stacking's feature groups against the training data
#'
#' `data_bound = "feature_names"` covers membership, but two rules it cannot
#' express are enforced here: that groups were supplied at all, and that they
#' name the same learners as the library. Neither can go in the class
#' `validator`, which fires at construction -- `setup_ModalityStacking()` has to
#' construct with no arguments, and a config is in any case a partial expression
#' of intent until it meets data.
#'
#' @param hyperparameters `ModalityStackingHyperparameters`: Hyperparameters to
#' check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(
  validate_hyperparameters,
  ModalityStackingHyperparameters
) <- function(hyperparameters, x) {
  check_meta_hyperparameters(hyperparameters, x)
  groups <- hyperparameters@feature_groups
  if (is.null(groups)) {
    rtemis.core::abort(
      "ModalityStacking requires `feature_groups`: one group of features per base learner.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (length(groups) < 2L) {
    rtemis.core::abort(
      "`feature_groups` must define at least 2 groups; with one there is nothing to stack.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  learners <- names(hyperparameters@base_learners)
  if (!setequal(names(groups), learners)) {
    rtemis.core::abort(
      "`feature_groups` must name the same learners as `base_learners`.\n",
      "Learners: ",
      paste(learners, collapse = ", "),
      ".\n",
      "Groups: ",
      paste(names(groups), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.ModalityStackingHyperparameters


# %% setup_ModalityStacking ----
#' Setup ModalityStacking Hyperparameters
#'
#' Setup hyperparameters for modality stacking: a SuperLearner whose base
#' learners each see one group of features.
#'
#' @details
#' With a wide `x` formed by concatenating modalities -- imaging, genomics, labs
#' -- and few cases, one model per modality can beat one model over everything,
#' and each modality can take the algorithm that suits it: SPLS for correlated
#' blocks, LASSO for sparse ones, gradient boosting for interactions. Training is
#' otherwise identical to [setup_SuperLearner]: cross-validated predictions from
#' each base learner, combined by the meta learner.
#'
#' `feature_groups` maps each base learner's name to the features it sees. Pass
#' a single `Hyperparameters` object as `base_learners` to use the same algorithm
#' on every group.
#'
#' Whether this beats one model over the concatenated modalities is the empirical
#' question it exists to answer, and two things decide it. Each base learner sees
#' only its own group, so what every *other* modality contributes to the outcome
#' is irreducible noise to it, which can hide a real signal in a weak modality
#' behind a strong one. And the default meta learner returns a convex
#' combination, which averages the modalities rather than adding them: where they
#' are complementary rather than competing, `meta_learner =
#' setup_NNLS(normalize = FALSE)` lets their contributions sum.
#'
#' `feature_groups` is checked against the training data at [train] time, not at
#' setup: it names columns, and there is no data to name them in yet.
#'
#' Supports regression and binary classification.
#'
#' @param feature_groups Optional List: Features each base learner sees, as a
#' named list of character vectors keyed by base learner name. Required by
#' [train].
#' @param base_learners List of `Hyperparameters` objects, or one to use for
#' every group: The library, named to match `feature_groups`.
#' @param meta_learner `Hyperparameters` object: Learner fitted on the base
#' learners' cross-validated predictions.
#' @param inner_resampling_config `ResamplerConfig` object: Cross-validation
#' scheme used to build the level-one predictions.
#' @param discrete (Tunable) Logical: If TRUE, keep the single lowest-risk
#' library entry rather than the weighted combination.
#' @param expand_search_spaces Logical: If TRUE, expand a base learner's search
#' space into one library entry per combination.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in
#' classification.
#'
#' @return ModalityStackingHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' modality_hyperparams <- setup_ModalityStacking(
#'   feature_groups = list(a = c("Sepal.Length", "Sepal.Width"), b = "Petal.Length"),
#'   base_learners = list(a = setup_GLM(), b = setup_CART())
#' )
#' modality_hyperparams
setup_ModalityStacking <- function(
  feature_groups = NULL,
  base_learners = list(setup_GLM(), setup_GLMNET(), setup_Ranger()),
  meta_learner = setup_NNLS(),
  inner_resampling_config = setup_Resampler(n_resamples = 10L, type = "KFold"),
  # tunable
  discrete = FALSE,
  # fixed
  expand_search_spaces = TRUE,
  ifw = FALSE
) {
  # One learner for every group is the common case; broadcast it before naming
  # so the names come from the groups rather than from a repeated algorithm.
  if (S7_inherits(base_learners, Hyperparameters)) {
    n_groups <- if (is.null(feature_groups)) 2L else length(feature_groups)
    base_learners <- stats::setNames(
      rep(list(base_learners), n_groups),
      names(feature_groups)
    )
  }
  ModalityStackingHyperparameters(
    feature_groups = feature_groups,
    base_learners = name_base_learners(base_learners),
    meta_learner = meta_learner,
    inner_resampling_config = inner_resampling_config,
    discrete = discrete,
    expand_search_spaces = expand_search_spaces,
    ifw = ifw
  )
} # /rtemis::setup_ModalityStacking


# %% ConditionalSuperLearnerHyperparameters ----
#' @title ConditionalSuperLearnerHyperparameters
#'
#' @description
#' Hyperparameters subclass for the Conditional SuperLearner. Unlike the stacking
#' meta learners, the meta learner here is a K-class classifier -- the "oracle" --
#' over the original covariates, so `discrete` and `feature_groups` have no
#' meaning and are not declared.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ConditionalSuperLearnerHyperparameters <- new_class(
  name = "ConditionalSuperLearnerHyperparameters",
  parent = MetaLearnerHyperparameters,
  properties = list(
    algorithm = prop_algorithm("ConditionalSuperLearner"),
    meta_learner = new_property(
      Hyperparameters,
      default = quote(setup_Ranger())
    ),
    n_iterations = prop_integer(
      4L,
      min = 1L,
      tunable = TRUE,
      description = "Alternations between fitting the oracle and refitting the experts."
    ),
    loss = prop_string(
      NULL,
      enum = c("squared_error", "log_loss"),
      nullable = TRUE,
      default_on_null = TRUE,
      description = "Per-case loss the oracle minimizes. NULL = squared error, which for classification is the Brier score."
    ),
    init = prop_string(
      "full",
      enum = c("full", "random"),
      description = "Region each expert starts from: the whole training fold, or a random partition of it."
    ),
    min_region_size = prop_integer(
      10L,
      min = 1L,
      description = "Fewest cases an expert's region may hold before the expert keeps its previous fit instead of being refitted."
    )
  )
) # /rtemis::ConditionalSuperLearnerHyperparameters


# %% validate_hyperparameters.ConditionalSuperLearnerHyperparameters ----
#' Check the Conditional SuperLearner's oracle
#'
#' One constraint beyond the shared ones: the oracle solves a K-class problem
#' whatever the outcome is, so with more than two experts it must be an algorithm
#' that handles more than two classes.
#'
#' @param hyperparameters `ConditionalSuperLearnerHyperparameters`: Hyperparameters
#' to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(
  validate_hyperparameters,
  ConditionalSuperLearnerHyperparameters
) <- function(hyperparameters, x) {
  check_meta_hyperparameters(hyperparameters, x)
  # The library size decides the oracle's class count, and it is known here:
  # `base_learners` is not expanded until `train_()`, but expansion only ever
  # grows it, so a library already past 2 settles the question.
  oracle <- hyperparameters@meta_learner@algorithm
  if (
    length(hyperparameters@base_learners) > 2L &&
      !oracle %in% supervised_multiclass
  ) {
    rtemis.core::abort(
      "The oracle must handle more than 2 classes when there are more than 2 base learners, and ",
      oracle,
      " does not. Multiclass algorithms: ",
      paste(supervised_multiclass, collapse = ", "),
      ".",
      class = "rtemis_unsupported_error"
    )
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.ConditionalSuperLearnerHyperparameters


# %% setup_ConditionalSuperLearner ----
#' Setup ConditionalSuperLearner Hyperparameters
#'
#' Setup hyperparameters for the Conditional SuperLearner, which selects the best
#' model from a library *conditional on the covariates* rather than combining
#' them.
#'
#' @details
#' The model is `sum_k 1\{o(x) = k\} F_k(x)`: an "oracle" `o` assigns each case to
#' one of the K "experts" `F_k`, and that expert predicts it. Oracle and experts
#' are fitted by alternating, in the manner of k-means: with the experts fixed,
#' the oracle minimizes the total loss, which reduces to a weighted K-class
#' classification over an extended dataset holding each case once per expert; with
#' the oracle fixed, each expert is refitted on the cases assigned to it. The
#' losses are cross-validated throughout, so an expert is never scored on a case
#' it was trained on.
#'
#' The oracle is a classifier over the original covariates whatever the outcome
#' is, so with more than two experts it must handle more than two classes. Its
#' variable importance -- which covariates decide *which model applies* -- is what
#' [get_varimp] returns for a fitted model, and a simple oracle such as
#' [setup_CART] makes the partition itself readable.
#'
#' Supports regression and binary classification.
#'
#' Reference: Valdes, Interian, Gennatas & van der Laan, "Conditional Super
#' Learner", IEEE Transactions on Pattern Analysis and Machine Intelligence
#' (2022). \doi{10.1109/TPAMI.2021.3131976}
#'
#' @param base_learners List of `Hyperparameters` objects: The experts. Names
#' label the regions and the reported region sizes; unnamed entries are named
#' after their algorithm.
#' @param meta_learner `Hyperparameters` object: The oracle, a classifier fitted
#' on the extended dataset.
#' @param inner_resampling_config `ResamplerConfig` object: Cross-validation
#' scheme, which must hold out every case exactly once.
#' @param n_iterations (Tunable) Integer [1, Inf): Alternations between fitting
#' the oracle and refitting the experts.
#' @param loss Optional Character \{"squared_error", "log_loss"\}: Per-case loss
#' the oracle minimizes.
#' @param init Character \{"full", "random"\}: Region each expert starts from.
#' @param min_region_size Integer [1, Inf): Fewest cases an expert's region may
#' hold before the expert keeps its previous fit instead of being refitted.
#' @param expand_search_spaces Logical: If TRUE, expand a base learner's search
#' space into one expert per combination.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in
#' classification.
#'
#' @return ConditionalSuperLearnerHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' csl_hyperparams <- setup_ConditionalSuperLearner(
#'   base_learners = list(setup_GLM(), setup_CART())
#' )
#' csl_hyperparams
setup_ConditionalSuperLearner <- function(
  base_learners = list(setup_GLM(), setup_GLMNET(), setup_Ranger()),
  meta_learner = setup_Ranger(),
  inner_resampling_config = setup_Resampler(n_resamples = 10L, type = "KFold"),
  # tunable
  n_iterations = 4L,
  # fixed
  loss = NULL,
  init = "full",
  min_region_size = 10L,
  expand_search_spaces = TRUE,
  ifw = FALSE
) {
  n_iterations <- clean_posint(n_iterations)
  min_region_size <- clean_posint(min_region_size)
  ConditionalSuperLearnerHyperparameters(
    base_learners = name_base_learners(base_learners),
    meta_learner = meta_learner,
    inner_resampling_config = inner_resampling_config,
    n_iterations = n_iterations,
    loss = loss,
    init = init,
    min_region_size = min_region_size,
    expand_search_spaces = expand_search_spaces,
    ifw = ifw
  )
} # /rtemis::setup_ConditionalSuperLearner
