# test_LightGBMParameters.R
# ::rtemis::
# 2026- EDG rtemis.org

# The rtemis/LightGBM parameter boundary, audited against the backend itself.
#
# rtemis wraps LightGBM in four algorithms and passes `hyperparameters@hyperparameters`
# straight to `lgb.train()`, so a LightGBM parameter is reachable if and only if
# some class declares it. Which ones are declared was, until this file, a
# decision living in nobody's head in particular.
#
# Every canonical LightGBM parameter must therefore be accounted for exactly
# once: declared, excluded with a reason, or listed as pending. The test fails
# on anything that is none of the three -- including a parameter a future
# LightGBM release adds, which is the point: the cutoff becomes an artifact that
# a reviewer can read and a version bump cannot silently move.
#
# `.PARAMETER_ALIASES()` is read from the installed 'lightgbm' rather than
# transcribed, so the audit describes the backend actually in use. It calls
# `LGBM_DumpParamAliases_R`, which dumps the compiled library's own parameter
# table -- every parameter, not only the aliased ones: 71 of its 141 entries
# have no alias at all. Checked against the published v4.7.0 parameter list,
# which it matches exactly in both directions. Reading the library rather than
# the docs is what makes a version bump visible here instead of silent. It is
# unexported; `getFromNamespace()` reaches it without a `:::` call in package
# code.
#
# **One spelling per parameter.** LightGBM accepts several names for one
# setting and silently keeps one when given two -- `bagging_fraction = 0.5`
# together with `subsample = 0.9` trains at 0.5, with nothing said. So a class
# may declare a parameter under exactly one of its names, and where rtemis has
# a free choice it takes LightGBM's canonical one.

# %% .lgb_aliases ----
.lgb_aliases <- function() {
  utils::getFromNamespace(".PARAMETER_ALIASES", "lightgbm")()
}


# %% .LIGHTGBM_CLASSES ----
# The four rtemis algorithms backed by `lgb.train()`.
.LIGHTGBM_CLASSES <- c(
  "LightCARTHyperparameters",
  "LightRFHyperparameters",
  "LightGBMHyperparameters",
  "LightRuleFitHyperparameters"
)


# %% .lgb_facing_props ----
# The properties of one class that reach LightGBM. All of them, except on
# LightRuleFit, whose second stage is GLMNET: its `alpha` and `lambda` are the
# elastic-net mixing parameter and penalty, and both happen to spell LightGBM
# parameters meaning entirely different things (the huber/quantile level, and
# L2 regularization). `train_LightRuleFit()` keeps them apart with an explicit
# allowlist; this audit reads the same constant, so the two cannot drift.
.lgb_facing_props <- function(class_name) {
  ns <- asNamespace("rtemis")
  props <- names(get(class_name, envir = ns)@properties)
  if (identical(class_name, "LightRuleFitHyperparameters")) {
    props <- setdiff(props, get("LightRuleFit_glmnet_params", envir = ns))
  }
  props
}


# %% .lgb_covered ----
# Canonical LightGBM parameters some class declares, resolved through the
# backend's own alias table: rtemis names several of them differently
# (`subsample` is `bagging_fraction`, `nrounds` is `num_iterations`).
.lgb_covered <- function() {
  unique(unlist(
    lapply(.LIGHTGBM_CLASSES, .lgb_class_covered),
    use.names = FALSE
  ))
}


# %% .lgb_class_covered ----
# The same, for one wrapper: what a user can actually reach from that
# algorithm's `setup_*`.
.lgb_class_covered <- function(class_name) {
  aliases <- .lgb_aliases()
  canonical <- vapply(
    .lgb_facing_props(class_name),
    function(nm) {
      for (key in names(aliases)) {
        if (nm == key || nm %in% aliases[[key]]) {
          return(key)
        }
      }
      NA_character_
    },
    character(1L),
    USE.NAMES = FALSE
  )
  unique(canonical[!is.na(canonical)])
}


# %% .LIGHTGBM_EXCLUDED ----
# Parameters rtemis does not declare and will not, each with what makes it
# unreachable rather than merely unwritten. Grouped by reason, because the
# reason is the reviewable part: a group is a claim about where the rtemis/
# LightGBM boundary sits, and a parameter in the wrong group is a bug in the
# boundary rather than in the list.
.LIGHTGBM_EXCLUDED <- list(
  # rtemis builds the `lgb.Dataset` from a data frame it already holds and
  # never hands LightGBM a path. Everything about parsing a file, naming a
  # column inside one, or writing one back is unreachable by construction.
  `reads or writes files` = c(
    "data",
    "valid",
    "header",
    "label_column",
    "weight_column",
    "group_column",
    "ignore_column",
    "categorical_feature",
    "two_round",
    "save_binary",
    "precise_float_parser",
    "parser_config_file",
    "forcedbins_filename",
    "forcedsplits_filename",
    "output_model",
    "input_model",
    "output_result",
    "convert_model",
    "convert_model_language",
    "config",
    "task",
    "snapshot_freq",
    "pre_partition",
    "max_bin_by_feature",
    "bin_construct_sample_cnt",
    "enable_bundle",
    "is_enable_sparse",
    "feature_pre_filter",
    "saved_feature_importance_type",
    "data_random_seed"
  ),
  # LightGBM's own distributed mode, which is a different execution model from
  # the one `ExecutionConfig` describes. rtemis parallelizes over folds, tuning
  # cells and threads within a worker, never over machines training one booster.
  `distributed training` = c(
    "num_machines",
    "local_listen_port",
    "time_out",
    "machine_list_filename",
    "machines"
  ),
  # Which GPU, not whether to use one: `device_type` is declared and selects
  # the device class. Picking a board among several is machine configuration,
  # and a config naming board 2 is not portable to the machine that has one.
  `selects a specific GPU` = c(
    "gpu_platform_id",
    "gpu_device_id",
    "gpu_use_dp",
    "num_gpu",
    "gpu_device_id_list"
  ),
  # Prediction-time settings, which belong to `predict()` and not to a fitted
  # model's hyperparameters. rtemis drives that path itself -- `explain()` asks
  # for `predict(type = "contrib")` where it needs contributions.
  `applies to predict, not to the fit` = c(
    "predict_contrib",
    "predict_leaf_index",
    "predict_raw_score",
    "predict_disable_shape_check",
    "num_iteration_predict",
    "start_iteration_predict",
    "pred_early_stop",
    "pred_early_stop_freq",
    "pred_early_stop_margin"
  ),
  # Learning-to-rank, which rtemis has no task for: `train()` fits a regression
  # or a classification, decided by the outcome, and there is no third case for
  # a query-grouped ranking objective to attach to.
  `learning-to-rank only` = c(
    "lambdarank_truncation_level",
    "lambdarank_norm",
    "lambdarank_position_bias_regularization",
    "label_gain",
    "eval_at",
    "bagging_by_query",
    "multi_error_top_k",
    "auc_mu_weights"
  ),
  # What LightGBM reports while training. rtemis scores its own runs through
  # `Metrics`, so a second, differently-defined set of numbers printed by the
  # backend would be a second answer to a question already answered.
  `backend metric reporting` = c(
    "metric",
    "metric_freq",
    "is_provide_training_metric",
    "first_metric_only"
  ),
  # Set by the pipeline itself. Declaring them would give one decision two
  # owners: `n_workers` resolves the thread count, the outcome's levels fix
  # `num_class`, `verbosity` is the run's, and the seed comes from
  # `ExecutionConfig`.
  `owned by the pipeline` = c(
    "num_threads",
    "num_class",
    "verbosity",
    "seed"
  )
)


# %% .LIGHTGBM_PENDING ----
# Reachable, meaningful, and not yet declared. This list is the work, and it is
# meant to shrink to nothing: an entry here is a parameter a user cannot set
# today and should be able to.
#
# Two entries are naming decisions rather than plain additions. LightGBM spells
# the huber and quantile level `alpha`, which on `LightRuleFitHyperparameters`
# already means the elastic-net mixing parameter, and on `GLMNETHyperparameters`
# means the same unrelated thing; it needs a distinct name. `boosting` is
# declared on LightRF as `boosting_type` and fixed to "rf" there, so adding it
# to LightGBM -- which is what makes DART and GOSS reachable at all -- has to
# settle on one spelling for both.
.LIGHTGBM_PENDING <- c(
  # Objective-specific, one per objective that takes a parameter. Without these
  # the objective is settable and its parameter is not, so `objective =
  # "quantile"` silently trains LightGBM's default 0.9 quantile.
  "alpha",
  "tweedie_variance_power",
  "fair_c",
  "poisson_max_delta_step",
  "sigmoid",
  "boost_from_average",
  "reg_sqrt",
  "is_unbalance",
  "scale_pos_weight",
  # The parameters of the boosting modes `boosting` selects. DART and GOSS are
  # unreachable from every wrapper: `boosting` itself is declared on LightRF
  # alone and fixed to "rf" there (see `.LIGHTGBM_ASYMMETRIC`).
  "data_sample_strategy",
  "drop_rate",
  "max_drop",
  "skip_drop",
  "uniform_drop",
  "xgboost_dart_mode",
  "drop_seed",
  "top_rate",
  "other_rate",
  # Core tree regularization. `min_gain_to_split` and `min_sum_hessian_in_leaf`
  # are first-rank knobs, and `min_data_in_leaf` is declared on LightCART alone
  # -- one backend parameter, reachable from one of four wrappers.
  "min_gain_to_split",
  "min_sum_hessian_in_leaf",
  "max_delta_step",
  "path_smooth",
  "extra_trees",
  "linear_lambda",
  "top_k",
  # Binning and missing-value handling.
  "max_bin",
  "min_data_in_bin",
  "use_missing",
  "zero_as_missing",
  # Categorical handling, beside the two thresholds already declared.
  "cat_l2",
  "cat_smooth",
  "max_cat_to_onehot",
  # Structural constraints. rtemis ships MonotonicHAL as a whole algorithm for
  # monotonicity while LightGBM's own constraints sit unreachable.
  "monotone_constraints",
  "monotone_constraints_method",
  "monotone_penalty",
  "interaction_constraints",
  "feature_contri",
  # Per-node and per-class sampling, beside the per-tree fractions declared.
  "feature_fraction_bynode",
  "pos_bagging_fraction",
  "neg_bagging_fraction",
  # Cost-efficient gradient boosting.
  "cegb_tradeoff",
  "cegb_penalty_split",
  "cegb_penalty_feature_lazy",
  "cegb_penalty_feature_coupled",
  # Reproducibility and the per-stage seeds, which decide whether two runs of
  # one config agree.
  "deterministic",
  "bagging_seed",
  "feature_fraction_seed",
  "extra_seed",
  "objective_seed",
  # Stopping, beside the round count already declared.
  "early_stopping_min_delta",
  # Execution knobs that change timing rather than the fit, but that a user
  # tuning a large run reaches for.
  "force_row_wise",
  "histogram_pool_size",
  "refit_decay_rate",
  # Quantized-gradient training, which does change the fit.
  "use_quantized_grad",
  "num_grad_quant_bins",
  "quant_train_renew_leaf",
  "stochastic_rounding"
)


# %% .LIGHTGBM_ASYMMETRIC ----
# Parameters some of the four wrappers declare and others do not. One backend,
# four wrappers, and a user reasonably expects a knob reachable from one to be
# reachable from the others unless the algorithm makes it meaningless.
#
# `intended` is a claim that the wrappers omitting it cannot use it: a single
# LightCART tree has no boosting rounds, no learning rate and no bagging, and
# LightRF fixes `boosting` to "rf" by definition. `pending` is the rest -- the
# same parameter, meaningful in both places, reachable from one.
#
# Keyed by canonical name; the value names the wrappers that declare it.
.LIGHTGBM_ASYMMETRIC <- list(
  num_iterations = "intended: a LightCART fit is one tree",
  learning_rate = "intended: no shrinkage to apply to a single LightCART tree",
  early_stopping_round = "intended: nothing to stop early in one tree, and LightRuleFit fixes its own round count",
  bagging_fraction = "intended: LightCART fits one tree on all of the data",
  bagging_freq = "intended: as bagging_fraction",
  boosting = "pending: declared on LightRF as boosting_type and fixed to \"rf\"; LightGBM cannot reach DART or GOSS",
  min_data_in_leaf = "pending: declared on LightCART alone, and it is core regularization for all four",
  device_type = "pending: LightCART and LightRuleFit run on the CPU only, for no reason in the backend",
  feature_fraction = "pending: LightCART and LightRuleFit cannot sample features",
  force_col_wise = "pending: a performance choice the other two cannot make",
  linear_tree = "pending: LightRuleFit cannot fit linear leaves",
  max_cat_threshold = "pending: LightRuleFit cannot set the categorical split limit",
  min_data_per_group = "pending: LightRuleFit cannot set the categorical group minimum",
  tree_learner = "pending: LightCART and LightRuleFit cannot choose the tree learner"
)


# %% .LIGHTGBM_SHADOWED_NAMES ----
# Properties that spell a LightGBM parameter while meaning something else, and
# what keeps the two apart. Not a clash today and one refactor away from being
# one: the separation is a hand-maintained allowlist, and `train_LightGBM()`
# builds `params` from the whole property list, so a class following that
# pattern would hand LightGBM the wrong quantity under the right name.
.LIGHTGBM_SHADOWED_NAMES <- list(
  LightRuleFitHyperparameters = c(
    alpha = paste0(
      "GLMNET's elastic-net mixing parameter for the rule-fitting stage. ",
      "LightGBM's `alpha` is the huber and quantile level."
    ),
    lambda = paste0(
      "GLMNET's penalty for the rule-fitting stage. `lambda` is a LightGBM ",
      "alias of `lambda_l2`, which this class also declares."
    )
  )
)


test_that("no class ships two names for one LightGBM parameter", {
  # The failure this prevents is silent: given `bagging_fraction = 0.5` and
  # `subsample = 0.9`, LightGBM trains at 0.5 and says nothing. No exceptions --
  # a second spelling reaching `params` is a defect, not a thing to record.
  skip_if_not_installed("lightgbm")
  aliases <- .lgb_aliases()
  for (class_name in .LIGHTGBM_CLASSES) {
    canonical <- vapply(
      .lgb_facing_props(class_name),
      function(nm) {
        for (key in names(aliases)) {
          if (nm == key || nm %in% aliases[[key]]) {
            return(key)
          }
        }
        NA_character_
      },
      character(1L)
    )
    counts <- table(canonical[!is.na(canonical)])
    expect_identical(
      sort(names(counts)[counts > 1L]),
      character(),
      info = paste0(
        class_name,
        ": two properties name one LightGBM parameter; rename one."
      )
    )
  }
})


test_that("a property shadowing a LightGBM name is recorded as such", {
  # The names that would clash if the allowlist keeping them out of `params`
  # ever stopped: declared on a LightGBM-backed class, spelling a LightGBM
  # parameter, meaning something else.
  skip_if_not_installed("lightgbm")
  aliases <- .lgb_aliases()
  spellings <- unique(c(names(aliases), unlist(aliases, use.names = FALSE)))
  for (class_name in .LIGHTGBM_CLASSES) {
    all_props <- names(
      get(class_name, envir = asNamespace("rtemis"))@properties
    )
    shadowed <- setdiff(
      intersect(all_props, spellings),
      .lgb_facing_props(class_name)
    )
    # `character()` rather than NULL for a class with no entry, so both
    # comparisons below are between vectors of one type.
    recorded <- as.character(names(.LIGHTGBM_SHADOWED_NAMES[[class_name]]))
    expect_identical(
      sort(setdiff(shadowed, recorded)),
      character(),
      info = paste0(
        class_name,
        ": property spells a LightGBM parameter but is not one. Rename it, or ",
        "record it in .LIGHTGBM_SHADOWED_NAMES with what keeps it out of ",
        "`params`."
      )
    )
    # And the converse, so a record cannot outlive the rename that retires it.
    expect_identical(sort(setdiff(recorded, shadowed)), character())
  }
})


# %% .LGB_TRAIN_ARGUMENTS ----
# Properties consumed as R-level arguments of `lgb.train()` rather than passed
# inside `params`: `train_LightGBM()` and `train_LightRF()` strip both before
# the call and hand them over by name. `lgb.train()`'s own formals are
# `nrounds` and `early_stopping_rounds`, so these spellings are the R API's,
# not aliases chosen over a canonical name -- and being outside `params` they
# cannot collide with anything in it.
.LGB_TRAIN_ARGUMENTS <- c("nrounds", "early_stopping_rounds")


test_that("a declared parameter uses LightGBM's canonical name", {
  # Where rtemis has a free choice it takes the backend's own name, so one
  # parameter has one spelling across the docs, the schema and the config.
  # `Ranger` already works this way -- `sample_fraction` is ranger's name, not
  # a house synonym -- and the entries below are where the LightGBM family does
  # not, recorded so the list shrinks rather than grows.
  skip_if_not_installed("lightgbm")
  legacy <- c(
    # These three do reach `params`, under an alias.
    "subsample",
    "subsample_freq",
    "boosting_type",
    # GLMNET's, on the two-stage class; not a LightGBM name at all.
    "lambda",
    "alpha"
  )
  aliases <- .lgb_aliases()
  non_canonical <- character()
  for (class_name in .LIGHTGBM_CLASSES) {
    for (nm in .lgb_facing_props(class_name)) {
      for (key in names(aliases)) {
        if (nm != key && nm %in% aliases[[key]]) {
          non_canonical <- c(non_canonical, nm)
        }
      }
    }
  }
  expect_identical(
    sort(unique(setdiff(non_canonical, c(legacy, .LGB_TRAIN_ARGUMENTS)))),
    character(),
    info = "declares a LightGBM parameter under an alias: use the canonical name"
  )
  # The R-argument names are the R API's, so they must keep matching it rather
  # than drifting toward the params spelling: renaming `nrounds` to
  # `num_iterations` would leave `lgb.train()` called by position or by a name
  # it does not have.
  expect_true(all(
    .LGB_TRAIN_ARGUMENTS %in% names(formals(lightgbm::lgb.train))
  ))
})


test_that("every parameter declared by some wrapper is declared by all, or classified", {
  # One backend behind four algorithms, so an asymmetry is a decision and has
  # to read as one. This is the check that would have caught `min_data_in_leaf`
  # reaching one wrapper of four.
  skip_if_not_installed("lightgbm")
  coverage <- lapply(.LIGHTGBM_CLASSES, .lgb_class_covered)
  names(coverage) <- .LIGHTGBM_CLASSES
  everywhere <- Reduce(intersect, coverage)
  somewhere <- unique(unlist(coverage, use.names = FALSE))
  expect_identical(
    sort(setdiff(setdiff(somewhere, everywhere), names(.LIGHTGBM_ASYMMETRIC))),
    character(),
    info = paste0(
      "declared by some wrappers and not others: declare it on the rest, or ",
      "classify it in .LIGHTGBM_ASYMMETRIC as intended (the algorithm cannot ",
      "use it) or pending."
    )
  )
  # And the converse, so the classification cannot outlive the asymmetry.
  expect_identical(
    sort(setdiff(names(.LIGHTGBM_ASYMMETRIC), setdiff(somewhere, everywhere))),
    character(),
    info = "no longer asymmetric: drop from .LIGHTGBM_ASYMMETRIC"
  )
  # Every entry states which it is, so none can be filed without a decision.
  expect_true(all(grepl("^(intended|pending):", unlist(.LIGHTGBM_ASYMMETRIC))))
})


test_that("every LightGBM parameter is declared, excluded or pending", {
  # The gate. A parameter in none of the three is unaccounted for, which is how
  # a backend upgrade that adds one gets noticed.
  skip_if_not_installed("lightgbm")
  canonical <- names(.lgb_aliases())
  accounted <- c(
    .lgb_covered(),
    unlist(.LIGHTGBM_EXCLUDED, use.names = FALSE),
    .LIGHTGBM_PENDING
  )
  expect_identical(
    sort(setdiff(canonical, accounted)),
    character(),
    info = paste0(
      "unaccounted LightGBM parameter(s): declare one, or add it to ",
      ".LIGHTGBM_EXCLUDED with the reason it is unreachable, or to ",
      ".LIGHTGBM_PENDING."
    )
  )
})


test_that("nothing is accounted for twice", {
  # A parameter cannot be both excluded and pending, and a pending one cannot
  # already be declared -- either would let the list say two things at once.
  skip_if_not_installed("lightgbm")
  covered <- .lgb_covered()
  excluded <- unlist(.LIGHTGBM_EXCLUDED, use.names = FALSE)
  expect_identical(sort(intersect(excluded, .LIGHTGBM_PENDING)), character())
  expect_identical(sort(intersect(covered, excluded)), character())
  expect_identical(sort(intersect(covered, .LIGHTGBM_PENDING)), character())
  expect_identical(anyDuplicated(excluded), 0L)
  expect_identical(anyDuplicated(.LIGHTGBM_PENDING), 0L)
})


test_that("every excluded and pending parameter is still a LightGBM parameter", {
  # The converse, so neither list can outlive what it describes: a parameter
  # LightGBM removes, or that rtemis declares after all, must be struck rather
  # than left standing as a false claim about the boundary.
  skip_if_not_installed("lightgbm")
  canonical <- names(.lgb_aliases())
  excluded <- unlist(.LIGHTGBM_EXCLUDED, use.names = FALSE)
  expect_identical(sort(setdiff(excluded, canonical)), character())
  expect_identical(sort(setdiff(.LIGHTGBM_PENDING, canonical)), character())
  # And a pending parameter that someone has since declared is done, not
  # pending.
  expect_identical(
    sort(intersect(.LIGHTGBM_PENDING, .lgb_covered())),
    character()
  )
})


test_that("the objective a run asked for is the objective it trained", {
  # The concrete cost of the pending list, pinned so the fix has a test to
  # turn green: `objective` is a free-form string, so "quantile" is accepted
  # while LightGBM's `alpha` -- its quantile level -- is not declared, and the
  # fit silently targets the backend default of 0.9.
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  expect_true("alpha" %in% .LIGHTGBM_PENDING)
  expect_false("alpha" %in% names(formals(setup_LightGBM)))

  set.seed(3L)
  n <- 400L
  x <- data.frame(a = stats::runif(n, -3, 3), b = stats::rnorm(n))
  x[["y"]] <- 2 * x[["a"]] + stats::rnorm(n, sd = 1)
  mod <- train(
    x,
    hyperparameters = setup_LightGBM(objective = "quantile"),
    verbosity = 0L
  )
  below <- mean(x[["y"]] < predict(mod, x[, c("a", "b")], verbosity = 0L))
  # The 0.9 quantile, which nobody asked for and nobody can change.
  expect_gt(below, 0.85)
})
