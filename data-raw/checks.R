# checks.R
# ::rtemis::
# 2026- EDG rtemis.org

# The data checks as data: what `validate_data.R` decides, written so that an
# implementation with no R can decide the same thing.
#
# This file is the **source**; `checks/v1/checks.json` is the artifact, and
# nothing but `generate_checks.R` reads what is here. The same split the
# `prop_*` factories have with JSON Schema, for the same reason: JSONLogic at
# the nesting depth these guards reach is not reviewable, and a rule nobody can
# read is a rule nobody checks.
#
# `R/validate_data.R` remains the reference implementation. These rules are
# derived from it and are checked against it by the fixture corpus; they do not
# replace it, and rtemis does not evaluate them.
#
# Three things about the shape, each of which cost a rewrite to discover:
#
# **Guards stand in for early returns.** The reference implementation
# short-circuits -- if `complete_cases` is set, the imputation and
# algorithm-tolerance checks never run. Flattened, that exclusivity has to be
# written down, because statement order is exactly what a declarative form
# loses. Every rule but `OUTCOME_MISSING` also guards on `outcome_resolvable`,
# which is the one early return that ends the whole pass.
#
# **One rule is one condition, one severity, one message.** Where the reference
# implementation appends a clause -- the sentence `DIM_P_GT_N` adds about the
# algorithm, the two `PREPROCESSOR_UNSUPPORTED` reasons joined by a semicolon
# -- that is two or three rules here with disjoint guards, not one rule with a
# conditional template. It costs more rules and buys a renderer that only ever
# substitutes.
#
# **`plain` is not here and never will be.** It is authored once per code in
# `DIAGNOSTIC_PLAIN` and looked up; several rules share a code precisely so
# that there is one plain-language explanation per *kind* of problem.

# The vocabularies the reference implementation states in R, spelled into the
# rule language from the same constants, so the two cannot drift.
.array <- function(x) paste0("['", paste(x, collapse = "', '"), "']")
.pointers <- function(ops) paste0("/preprocessor_config/", ops)


# %% Bindings ----
# Evaluated once per validation, in order. A binding exists where a quantity is
# read more than once -- `min_class_n` appears in a condition, in the evidence
# and in a fix value, and three copies of one expression must agree -- or where
# naming it is what makes the rule readable.
CHECKS_LET <- list(
  # A config that designates an outcome. The `hyperparameters` block is the
  # marker: a decomposition or clustering config has none, because those
  # methods model every column jointly.
  expr("supervised", "config.hyperparameters !== null", "boolean"),
  expr("n_columns", "count(profile.columns)", "number"),
  scan(
    "column_labels",
    "profile.columns",
    select = list(label = r"[concat("'", item.name, "'")]")
  ),
  expr("last_column", 'last(profile.columns, "name")', "any"),

  # Outcome resolution, and the early return it can trigger. `named_matches`
  # reads the *given* outcome, before resolution, which is what breaks the
  # circularity: whether the pass continues cannot depend on the name the
  # convention would have supplied.
  scan("named_matches", "profile.columns", where = "item.name === outcome"),
  expr(
    "outcome_resolvable",
    "outcome === null or count(named_matches) === 1",
    "boolean"
  ),
  expr(
    "outcome_name",
    "if outcome !== null then outcome
     else if supervised and n_columns >= 2 then last_column
     else null",
    "any"
  ),
  scan(
    "outcome_columns",
    "profile.columns",
    where = "item.name === outcome_name"
  ),
  expr("outcome_resolved", "count(outcome_columns) === 1", "boolean"),
  expr("outcome_dtype", 'first(outcome_columns, "dtype")', "any"),
  expr("outcome_n_distinct", 'first(outcome_columns, "n_distinct")', "any"),
  expr("outcome_n_missing", 'first(outcome_columns, "n_missing")', "any"),
  expr("outcome_is_categorical", "outcome_dtype === 'categorical'", "boolean"),

  # Predictors: every column that is not the outcome. With no outcome resolved
  # that is every column, which is what a decomposition config wants.
  scan("features", "profile.columns", where = "item.name !== outcome_name"),
  expr("n_features", "count(features)", "number"),

  # The algorithm the config names. A hyperparameter *set* is a search over one
  # algorithm and names one too, but its members sit under object keys, so the
  # wildcard pointer is what reaches them.
  scan_pointers("variant_algorithms", "/hyperparameters/variants/*/algorithm"),
  expr(
    "algorithm",
    'if config.hyperparameters.algorithm !== null
     then config.hyperparameters.algorithm
     else first(variant_algorithms, "value")',
    "any"
  ),
  scan("algorithm_traits", "traits", where = "item.name === algorithm"),
  # Three-valued, and the third value is the point: null where the answer
  # belongs to the algorithm's base learners rather than to the algorithm.
  # Tested with `=== false` and `=== true` rather than by truthiness, so no
  # port-variable semantics decide a severity.
  expr("allows_missing", 'first(algorithm_traits, "missing")', "any"),
  expr("handles_p_gt_n", 'first(algorithm_traits, "p_gt_n")', "any"),
  expr("does_classification", 'first(algorithm_traits, "class")', "any"),
  expr("does_regression", 'first(algorithm_traits, "reg")', "any"),

  # Preprocessing, read off the resolved config. A block that is absent reads
  # as null throughout, which is the same answer the reference implementation
  # gives for a NULL `PreprocessorConfig`.
  expr(
    "remove_constants",
    "config.preprocessor_config.remove_constants === true",
    "boolean"
  ),
  expr(
    "skip_missing",
    "if config.preprocessor_config === null then true
     else config.preprocessor_config.remove_constants_skip_missing === true",
    "boolean"
  ),
  expr("already_removed", "config.preprocessor_config.remove_features", "any"),
  expr(
    "already_removed_list",
    "if already_removed === null then [] else already_removed",
    "any"
  ),

  # `is_constant()` in the profile's terms, with no values to compare. Skipping
  # missing values, a column with at most one observed value never varies -- an
  # entirely missing one included. Not skipping them, a single gap makes the
  # comparison undecidable and so not constant, which is exactly one observed
  # value and nothing missing.
  scan(
    "constants",
    "features",
    where = "(if skip_missing
              then item.n_distinct <= 1
              else item.n_distinct === 1 and item.n_missing === 0)
             and not (item.name in already_removed_list)",
    select = list(
      name = "item.name",
      label = r"[concat("'", item.name, "'")]"
    )
  ),
  expr("n_constants", "count(constants)", "number"),
  expr("constant_names", 'pluck(constants, "name")', "array"),

  # Predictors `check_supervised()` refuses outright.
  scan(
    "unsupported_features",
    "features",
    where = paste0(
      "not (item.dtype in ",
      .array(SUPERVISED_FEATURE_DTYPES),
      ")"
    ),
    select = list(
      name = "item.name",
      dtype = "item.dtype",
      label = r"[concat("'", item.name, "' (", item.dtype, ")")]"
    )
  ),
  expr("n_unsupported_features", "count(unsupported_features)", "number"),

  # The width the learner is handed. A factor contributes one column per level
  # it takes -- `one_hot()` encodes every level and drops no reference -- while
  # everything else contributes one.
  scan(
    "feature_widths",
    "features",
    select = list(
      width = "if item.dtype === 'categorical' then item.n_distinct else 1"
    )
  ),
  expr("encoded_p", 'sum(feature_widths, "width")', "number"),
  scan(
    "wide_features",
    "features",
    where = "item.dtype === 'categorical' and item.n_distinct > 1"
  ),
  # A decomposition runs before the fit, so a pipeline extracting k components
  # hands the learner k features however wide the data was. Reporting the
  # encoded width there would state a number the model never sees.
  expr("decomposition_k", "config.decomposition_config.config.k", "any"),
  expr("decomposition_name", "config.decomposition_config.algorithm", "any"),
  expr(
    "effective_p",
    "if decomposition_k === null then encoded_p else decomposition_k",
    "any"
  ),

  # Preprocessing steps `train()` refuses, in the two families it refuses them
  # for. Both are guaranteed aborts, so a pre-flight that missed them would
  # pass a config `train()` rejects one line later.
  scan_pointers(
    "case_ops",
    .pointers(PREPROCESSOR_CASE_OPS),
    where = "item.value !== null and item.value !== false",
    select = list(name = "item.name", label = "concat('`', item.name, '`')")
  ),
  scan_pointers(
    "learned_drop_ops",
    .pointers(PREPROCESSOR_LEARNED_DROP_OPS),
    where = "item.value !== null and item.value !== false",
    select = list(name = "item.name", label = "concat('`', item.name, '`')")
  ),
  expr("n_case_ops", "count(case_ops)", "number"),
  expr("n_learned_drop_ops", "count(learned_drop_ops)", "number"),

  # Missingness. `runs_*` credits a step only where `train()` will actually run
  # it: a step it rejects resolves nothing, and the gaps it was meant to remove
  # are still there.
  expr("total_missing", 'sum(profile.columns, "n_missing")', "number"),
  expr("feature_missing", 'sum(features, "n_missing")', "number"),
  scan("missing_features", "features", where = "item.n_missing > 0"),
  expr("n_features_missing", "count(missing_features)", "number"),
  scan(
    "all_missing_columns",
    "profile.columns",
    where = "item.n_distinct === 0 and item.n_missing > 0",
    select = list(
      name = "item.name",
      label = r"[concat("'", item.name, "'")]"
    )
  ),
  expr(
    "runs_complete_cases",
    "config.preprocessor_config.complete_cases === true and not supervised",
    "boolean"
  ),
  expr(
    "runs_impute",
    "config.preprocessor_config.impute === true",
    "boolean"
  ),
  # The one place the rule set knows it cannot answer. Simulating
  # `remove_cases_thres` then `remove_features_thres` needs the joint
  # missingness pattern, which a bounded profile does not carry; `train()`
  # rejects both steps, so the gap is reachable only for a standalone
  # preprocessor config. See `unevaluable` below.
  expr(
    "thresholds_set",
    "config.preprocessor_config.remove_cases_thres !== null
     or config.preprocessor_config.remove_features_thres !== null",
    "boolean"
  ),
  expr("missing_evaluable", "supervised or not thresholds_set", "boolean"),

  # Both resamplers partition the same cases and fail the same way, so both are
  # checked, and each finding's fix patches the one it is about.
  scan_pointers(
    "resamplers",
    c("/outer_resampling_config", "/tuner_config/config/resampler_config"),
    where = "item.value !== null"
  ),
  scan(
    "outcome_levels",
    "profile.level_counts",
    where = "item.column === outcome_name"
  ),
  expr("n_outcome_levels", "count(outcome_levels)", "number"),
  expr("min_class_n", 'min(outcome_levels, "n")', "number"),
  scan("min_classes", "outcome_levels", where = "item.n === min_class_n"),
  # The earliest level holding the minimum, which is what `which.min()` picks
  # on a tie.
  expr("min_class_level", 'first(min_classes, "level")', "any"),
  # Long form, one record per level, matching the shape `profile/v1` settled
  # on: an expression language iterates arrays and cannot iterate a
  # name -> count map.
  scan(
    "outcome_class_counts",
    "outcome_levels",
    select = list(level = "item.level", n = "item.n")
  )
)


# %% Rules ----
# Twenty-nine, from eighteen `new_diagnostic()` call sites in
# `R/validate_data.R`, two of which carry a conditional severity. The rest of
# the expansion is message variation: where the reference implementation
# appends a clause or joins two, that is a separate rule here with a disjoint
# guard, so every message is a constant string with slots and every renderer
# only ever substitutes.
CHECKS_RULES <- list(
  # %% OUTCOME_MISSING ----
  # The one early return that ends the whole pass: with no outcome resolved,
  # every check below would report on a column that is not there. Every other
  # rule guards on `outcome_resolvable`, which is asserted rather than trusted.
  rule(
    id = "OUTCOME_MISSING/not-a-column",
    code = "OUTCOME_MISSING",
    condition = "outcome !== null and count(named_matches) === 0",
    severity = "error",
    evidence = list(
      outcome = "outcome",
      columns = 'pluck(profile.columns, "name")'
    ),
    slots = list(column_labels = 'pluck(column_labels, "label")'),
    message = "Outcome column '{outcome}' is not in the data. Columns are: {column_labels}."
  ),

  # %% OUTCOME_TYPE_MISMATCH ----
  rule(
    id = "OUTCOME_TYPE_MISMATCH/unusable-dtype",
    code = "OUTCOME_TYPE_MISMATCH",
    applies_when = "outcome_resolvable and outcome_resolved",
    condition = paste0(
      "not (outcome_dtype in ",
      .array(SUPERVISED_OUTCOME_DTYPES),
      ")"
    ),
    severity = "error",
    evidence = list(outcome = "outcome_name", outcome_dtype = "outcome_dtype"),
    message = "Outcome '{outcome}' holds {outcome_dtype} values; rtemis requires an integer, numeric, or factor outcome. Convert it to a factor to classify, or to a number to regress."
  ),
  # A warning, not an error: `train()` ignores `positive_class` on a numeric
  # outcome and completes, so stopping the run would reject a config that
  # works. What it costs is that the run answers the question the *data* poses
  # rather than the one the config states.
  rule(
    id = "OUTCOME_TYPE_MISMATCH/positive-class-on-numeric",
    code = "OUTCOME_TYPE_MISMATCH",
    applies_when = paste0(
      "outcome_resolvable and outcome_resolved and (outcome_dtype in ",
      .array(SUPERVISED_OUTCOME_DTYPES),
      ") and config.positive_class !== null"
    ),
    condition = "not outcome_is_categorical",
    severity = "warning",
    evidence = list(
      outcome = "outcome_name",
      outcome_dtype = "outcome_dtype",
      declared_task = "'Classification'",
      data_task = "'Regression'"
    ),
    slots = list(positive_class = "config.positive_class"),
    message = "Config declares classification (`positive_class` is '{positive_class}') but outcome '{outcome}' holds {outcome_dtype} values, which rtemis reads as regression."
  ),
  # An algorithm that performs one task says the same thing about itself. A
  # hyperparameter that merely *implies* a task -- an objective, a family -- is
  # deliberately not read: each would need its own per-algorithm mapping, and a
  # wrong reading of one would reject a config that runs.
  rule(
    id = "OUTCOME_TYPE_MISMATCH/algorithm-classifies-only",
    code = "OUTCOME_TYPE_MISMATCH",
    applies_when = paste0(
      "outcome_resolvable and outcome_resolved and (outcome_dtype in ",
      .array(SUPERVISED_OUTCOME_DTYPES),
      ") and config.positive_class === null",
      " and does_classification === true and does_regression !== true"
    ),
    condition = "not outcome_is_categorical",
    severity = "warning",
    evidence = list(
      outcome = "outcome_name",
      outcome_dtype = "outcome_dtype",
      declared_task = "'Classification'",
      data_task = "'Regression'"
    ),
    slots = list(algorithm = "algorithm"),
    message = "Config declares classification (algorithm {algorithm}) but outcome '{outcome}' holds {outcome_dtype} values, which rtemis reads as regression."
  ),
  rule(
    id = "OUTCOME_TYPE_MISMATCH/algorithm-regresses-only",
    code = "OUTCOME_TYPE_MISMATCH",
    applies_when = paste0(
      "outcome_resolvable and outcome_resolved and (outcome_dtype in ",
      .array(SUPERVISED_OUTCOME_DTYPES),
      ") and config.positive_class === null",
      " and does_regression === true and does_classification !== true"
    ),
    condition = "outcome_is_categorical",
    severity = "warning",
    evidence = list(
      outcome = "outcome_name",
      outcome_dtype = "outcome_dtype",
      declared_task = "'Regression'",
      data_task = "'Classification'"
    ),
    slots = list(algorithm = "algorithm"),
    message = "Config declares regression (algorithm {algorithm}) but outcome '{outcome}' holds {outcome_dtype} values, which rtemis reads as classification."
  ),

  # %% RESAMPLE_MIN_CLASS ----
  # The profile omits level counts above `PROFILE_MAX_LEVELS`, so the check
  # cannot run rather than passing. Saying so is the point: a silent skip is
  # indistinguishable from a clean result.
  rule(
    id = "RESAMPLE_MIN_CLASS/levels-not-carried",
    code = "RESAMPLE_MIN_CLASS",
    over = "resamplers",
    applies_when = "outcome_resolvable and outcome_resolved and outcome_is_categorical
                    and item.value.type in ['KFold', 'StratSub', 'StratBoot']",
    condition = "n_outcome_levels === 0",
    severity = "note",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_resamples = "item.value.n_resamples",
      n_levels = "outcome_n_distinct",
      max_levels = as.character(PROFILE_MAX_LEVELS)
    ),
    slots = list(outcome = "outcome_name"),
    message = "Outcome '{outcome}' has {n_levels} levels, more than the {max_levels} a profile carries counts for, so the class balance of the {type} resampler at `{resampler}` was not checked."
  ),
  # A stratified resampler puts each class into every part in the class's own
  # proportion, so a class with fewer cases than there are parts cannot reach
  # all of them. A plain bootstrap makes no promise about class balance, so
  # there is no count it fails to meet and it is not checked.
  rule(
    id = "RESAMPLE_MIN_CLASS/fewer-cases-than-resamples",
    code = "RESAMPLE_MIN_CLASS",
    over = "resamplers",
    applies_when = "outcome_resolvable and outcome_resolved and outcome_is_categorical
                    and item.value.type in ['KFold', 'StratSub', 'StratBoot']
                    and n_outcome_levels > 0",
    condition = "min_class_n < item.value.n_resamples",
    severity = "error",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_resamples = "item.value.n_resamples",
      min_class = "min_class_level",
      min_class_n = "min_class_n",
      class_counts = "outcome_class_counts"
    ),
    message = "Class '{min_class}' has {min_class_n} {min_class_n|case|cases} but the {type} resampler at `{resampler}` asks for {n_resamples} resamples.",
    # Fewer parts than the rarest class has cases is the one repair that needs
    # no judgment. Below two there is no fold count that works, so nothing is
    # offered rather than something that fails differently.
    fix = fix(
      when = "min_class_n >= 2",
      patch = list(op("replace", "{resampler}/n_resamples", "min_class_n"))
    )
  ),

  # %% RESAMPLE_N_ROWS ----
  rule(
    id = "RESAMPLE_N_ROWS/needs-two-rows",
    code = "RESAMPLE_N_ROWS",
    over = "resamplers",
    applies_when = "outcome_resolvable and item.value.type in ['LOOCV', 'Bootstrap']",
    condition = "profile.n_rows < 2",
    severity = "error",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_rows = "profile.n_rows"
    ),
    message = "The {type} resampler at `{resampler}` needs at least 2 rows; the data has {n_rows}."
  ),
  rule(
    id = "RESAMPLE_N_ROWS/more-folds-than-rows",
    code = "RESAMPLE_N_ROWS",
    over = "resamplers",
    applies_when = "outcome_resolvable and item.value.type === 'KFold'",
    condition = "profile.n_rows < item.value.n_resamples",
    severity = "error",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_rows = "profile.n_rows",
      n_resamples = "item.value.n_resamples"
    ),
    message = "The KFold resampler at `{resampler}` asks for {n_resamples} folds but the data has {n_rows} {n_rows|row|rows}.",
    # The largest fold count that both fits the data and leaves a usable test
    # fold: any k above `n / 2` gives folds of one case, which is what the
    # warning below is about, so repairing to the row count would only trade an
    # error for a warning. Below four rows no such k exists.
    fix = fix(
      when = "profile.n_rows >= 4",
      patch = list(
        op("replace", "{resampler}/n_resamples", "div_int(profile.n_rows, 2)")
      )
    )
  ),
  # Parts that *can* be made but hold a single case: a score computed on one
  # case carries no information, and averaging such scores does not create any.
  rule(
    id = "RESAMPLE_N_ROWS/thin-test-fold",
    code = "RESAMPLE_N_ROWS",
    over = "resamplers",
    let = list(n_test = "div_int(profile.n_rows, item.value.n_resamples)"),
    applies_when = "outcome_resolvable and item.value.type === 'KFold'
                    and profile.n_rows >= item.value.n_resamples",
    condition = "n_test < 2",
    severity = "warning",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_rows = "profile.n_rows",
      n_resamples = "item.value.n_resamples",
      n_test = "n_test"
    ),
    message = "The KFold resampler at `{resampler}` leaves {n_test} {n_test|case|cases} per test fold ({n_rows} rows over {n_resamples} folds)."
  ),
  # StratSub and StratBoot split by fraction rather than by count.
  # `round_half_even()` is the reference implementation's `round()`, tie rule
  # included: at 5 rows and train_p 0.9 the two rules disagree, and the
  # disagreement is a warning against an error.
  rule(
    id = "RESAMPLE_N_ROWS/empty-split-side",
    code = "RESAMPLE_N_ROWS",
    over = "resamplers",
    let = list(
      n_train = "round_half_even(profile.n_rows * item.value.train_p)",
      n_test = "profile.n_rows - round_half_even(profile.n_rows * item.value.train_p)"
    ),
    applies_when = "outcome_resolvable and item.value.type in ['StratSub', 'StratBoot']",
    condition = "n_test < 1 or n_train < 2",
    severity = "error",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_rows = "profile.n_rows",
      train_p = "item.value.train_p",
      n_train = "n_train",
      n_test = "n_test"
    ),
    message = "The {type} resampler at `{resampler}` splits {n_rows} {n_rows|row|rows} at train_p {train_p} into {n_train} training and {n_test} test; neither side can be empty and training needs at least 2."
  ),
  rule(
    id = "RESAMPLE_N_ROWS/single-test-case",
    code = "RESAMPLE_N_ROWS",
    over = "resamplers",
    let = list(
      n_train = "round_half_even(profile.n_rows * item.value.train_p)",
      n_test = "profile.n_rows - round_half_even(profile.n_rows * item.value.train_p)"
    ),
    applies_when = "outcome_resolvable and item.value.type in ['StratSub', 'StratBoot']
                    and n_test >= 1 and n_train >= 2",
    condition = "n_test < 2",
    severity = "warning",
    evidence = list(
      resampler = "item.pointer",
      type = "item.value.type",
      n_rows = "profile.n_rows",
      train_p = "item.value.train_p",
      n_train = "n_train",
      n_test = "n_test"
    ),
    message = "The {type} resampler at `{resampler}` leaves {n_test} test case per resample ({n_rows} rows at train_p {train_p})."
  ),

  # %% FEATURE_CONSTANT ----
  # A column that never varies cannot explain a column that does. rtemis trains
  # on it regardless -- so a warning, not an error -- but the coefficient or
  # split it produces is an artifact of the column being there.
  #
  # Two rules, one message: the repair differs by whether there is a
  # preprocessor block to patch into, and a rule carries one patch.
  rule(
    id = "FEATURE_CONSTANT/no-preprocessor-block",
    code = "FEATURE_CONSTANT",
    applies_when = "outcome_resolvable and not remove_constants
                    and config.preprocessor_config === null",
    condition = "n_constants > 0",
    severity = "warning",
    evidence = list(features = "constant_names", n_features = "n_constants"),
    slots = list(feature_labels = 'pluck(constants, "label")'),
    message = "{n_features} {n_features|predictor never varies|predictors never vary}: {feature_labels}.",
    # Two operations rather than one with an object value: an expression
    # language has no object constructor, so the block is created empty and
    # filled by the operation after it. RFC 6902 applies them in order.
    fix = fix(
      patch = list(
        op(
          "add",
          "/preprocessor_config",
          lit(stats::setNames(list(), character()))
        ),
        op("add", "/preprocessor_config/remove_features", "constant_names")
      )
    )
  ),
  rule(
    id = "FEATURE_CONSTANT/existing-preprocessor-block",
    code = "FEATURE_CONSTANT",
    applies_when = "outcome_resolvable and not remove_constants
                    and config.preprocessor_config !== null",
    condition = "n_constants > 0",
    severity = "warning",
    evidence = list(features = "constant_names", n_features = "n_constants"),
    slots = list(feature_labels = 'pluck(constants, "label")'),
    message = "{n_features} {n_features|predictor never varies|predictors never vary}: {feature_labels}.",
    # The union with what the config already removes is what keeps the patch
    # from discarding that list.
    fix = fix(
      patch = list(
        op(
          "add",
          "/preprocessor_config/remove_features",
          "merge(already_removed_list, constant_names)"
        )
      )
    )
  ),

  # %% PREPROCESSOR_UNSUPPORTED ----
  # Three rules, one per shape of the reason clause. No fix: the remedy is to
  # run the step before training, which is a change to the workflow rather than
  # a patch to the config.
  rule(
    id = "PREPROCESSOR_UNSUPPORTED/case-ops",
    code = "PREPROCESSOR_UNSUPPORTED",
    applies_when = "outcome_resolvable and supervised
                    and config.preprocessor_config !== null",
    condition = "n_case_ops > 0 and n_learned_drop_ops === 0",
    severity = "error",
    evidence = list(
      case_ops = 'pluck(case_ops, "name")',
      learned_drop_ops = 'pluck(learned_drop_ops, "name")'
    ),
    slots = list(case_labels = 'pluck(case_ops, "label")'),
    message = "`preprocessor_config` cannot run inside train(): {case_labels} {case_ops|removes cases|remove cases}, which a fitted preprocessor cannot replay at predict time. Do this before training, with preprocess() on the full dataset."
  ),
  rule(
    id = "PREPROCESSOR_UNSUPPORTED/learned-drop-ops",
    code = "PREPROCESSOR_UNSUPPORTED",
    applies_when = "outcome_resolvable and supervised
                    and config.preprocessor_config !== null",
    condition = "n_case_ops === 0 and n_learned_drop_ops > 0",
    severity = "error",
    evidence = list(
      case_ops = 'pluck(case_ops, "name")',
      learned_drop_ops = 'pluck(learned_drop_ops, "name")'
    ),
    slots = list(learned_labels = 'pluck(learned_drop_ops, "label")'),
    message = "`preprocessor_config` cannot run inside train(): {learned_labels} {learned_drop_ops|decides|decide} which columns to drop from the data, so each resample would train on a different feature set. Do this before training, with preprocess() on the full dataset."
  ),
  rule(
    id = "PREPROCESSOR_UNSUPPORTED/both-families",
    code = "PREPROCESSOR_UNSUPPORTED",
    applies_when = "outcome_resolvable and supervised
                    and config.preprocessor_config !== null",
    condition = "n_case_ops > 0 and n_learned_drop_ops > 0",
    severity = "error",
    evidence = list(
      case_ops = 'pluck(case_ops, "name")',
      learned_drop_ops = 'pluck(learned_drop_ops, "name")'
    ),
    slots = list(
      case_labels = 'pluck(case_ops, "label")',
      learned_labels = 'pluck(learned_drop_ops, "label")'
    ),
    message = "`preprocessor_config` cannot run inside train(): {case_labels} {case_ops|removes cases|remove cases}, which a fitted preprocessor cannot replay at predict time; {learned_labels} {learned_drop_ops|decides|decide} which columns to drop from the data, so each resample would train on a different feature set. Do this before training, with preprocess() on the full dataset."
  ),

  # %% FEATURE_TYPE_UNSUPPORTED ----
  # Unconditional, which looks over-strict until you check the order:
  # `check_supervised()` runs *before* `preprocess()` in `train()`, so
  # `character2factor` -- the setting that exists to convert exactly this
  # column -- never gets the chance.
  rule(
    id = "FEATURE_TYPE_UNSUPPORTED/not-numeric-or-factor",
    code = "FEATURE_TYPE_UNSUPPORTED",
    applies_when = "outcome_resolvable",
    condition = "n_unsupported_features > 0",
    severity = "error",
    evidence = list(
      features = 'pluck(unsupported_features, "name")',
      dtypes = 'pluck(unsupported_features, "dtype")',
      n_features = "n_unsupported_features"
    ),
    slots = list(feature_labels = 'pluck(unsupported_features, "label")'),
    message = "{n_features} {n_features|predictor is|predictors are} neither numeric nor a factor: {feature_labels}."
  ),

  # %% DIM_P_GT_N ----
  # Severity is the algorithm's answer, not a judgment: `p_gt_n` is false only
  # where the fit is an unregularized least squares and goes rank-deficient,
  # and those runs complete while producing aliased coefficients -- a warning
  # by the definition of the level. Everything else regularizes, selects, or
  # cannot be rank-deficient: a note.
  #
  # Six rules, from two message prefixes (encoded width, or the component count
  # a decomposition hands the learner instead) crossed with three closing
  # clauses.
  rule(
    id = "DIM_P_GT_N/encoded-rank-deficient",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k === null and handles_p_gt_n === false",
    condition = "effective_p > profile.n_rows",
    severity = "warning",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n"
    ),
    message = "{n_features} {n_features|predictor encodes|predictors encode} to {effective_p} columns, more than the {n_rows} {n_rows|row|rows} available. {algorithm} fits an unregularized least squares, so the fit is rank-deficient."
  ),
  rule(
    id = "DIM_P_GT_N/encoded-algorithm-fits",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k === null and handles_p_gt_n !== false
                    and algorithm !== null",
    condition = "effective_p > profile.n_rows",
    severity = "note",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n"
    ),
    message = "{n_features} {n_features|predictor encodes|predictors encode} to {effective_p} columns, more than the {n_rows} {n_rows|row|rows} available. {algorithm} fits in this regime."
  ),
  rule(
    id = "DIM_P_GT_N/encoded-no-algorithm",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k === null and algorithm === null",
    condition = "effective_p > profile.n_rows",
    severity = "note",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n"
    ),
    message = "{n_features} {n_features|predictor encodes|predictors encode} to {effective_p} columns, more than the {n_rows} {n_rows|row|rows} available."
  ),
  rule(
    id = "DIM_P_GT_N/components-rank-deficient",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k !== null and handles_p_gt_n === false",
    condition = "effective_p > profile.n_rows",
    severity = "warning",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n",
      decomposition = "decomposition_name",
      decomposition_k = "decomposition_k"
    ),
    message = "{decomposition} extracts {effective_p} {effective_p|component|components}, more than the {n_rows} {n_rows|row|rows} available. {algorithm} fits an unregularized least squares, so the fit is rank-deficient."
  ),
  rule(
    id = "DIM_P_GT_N/components-algorithm-fits",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k !== null and handles_p_gt_n !== false
                    and algorithm !== null",
    condition = "effective_p > profile.n_rows",
    severity = "note",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n",
      decomposition = "decomposition_name",
      decomposition_k = "decomposition_k"
    ),
    message = "{decomposition} extracts {effective_p} {effective_p|component|components}, more than the {n_rows} {n_rows|row|rows} available. {algorithm} fits in this regime."
  ),
  rule(
    id = "DIM_P_GT_N/components-no-algorithm",
    code = "DIM_P_GT_N",
    applies_when = "outcome_resolvable and n_features > 0
                    and decomposition_k !== null and algorithm === null",
    condition = "effective_p > profile.n_rows",
    severity = "note",
    evidence = list(
      n_features = "n_features",
      encoded_p = "encoded_p",
      effective_p = "effective_p",
      n_rows = "profile.n_rows",
      categorical_features = 'pluck(wide_features, "name")',
      algorithm = "algorithm",
      algorithm_handles_p_gt_n = "handles_p_gt_n",
      decomposition = "decomposition_name",
      decomposition_k = "decomposition_k"
    ),
    message = "{decomposition} extracts {effective_p} {effective_p|component|components}, more than the {n_rows} {n_rows|row|rows} available."
  ),

  # %% MISSING_INCOMPATIBLE ----
  # No preprocessing option addresses a missing outcome: `preprocess()`
  # transforms features, and `check_supervised()` rejects a training set whose
  # outcome has gaps. Always an error, and it fires alongside the rules below
  # rather than instead of them.
  rule(
    id = "MISSING_INCOMPATIBLE/outcome-has-gaps",
    code = "MISSING_INCOMPATIBLE",
    applies_when = "outcome_resolvable and outcome_resolved and total_missing > 0",
    condition = "outcome_n_missing > 0",
    severity = "error",
    evidence = list(
      outcome = "outcome_name",
      n_missing = "outcome_n_missing",
      n_rows = "profile.n_rows"
    ),
    message = "Outcome '{outcome}' has {n_missing} missing {n_missing|value|values}. Preprocessing transforms features, so no setting removes them: drop those rows before training."
  ),
  rule(
    id = "MISSING_INCOMPATIBLE/complete-cases-leaves-nothing",
    code = "MISSING_INCOMPATIBLE",
    applies_when = "outcome_resolvable and total_missing > 0 and runs_complete_cases",
    condition = "profile.n_complete_cases < 2",
    severity = "error",
    evidence = list(
      n_complete = "profile.n_complete_cases",
      n_rows = "profile.n_rows"
    ),
    message = "`complete_cases` keeps rows with no gaps, and only {n_complete} of {n_rows} {n_rows|row has|rows have} none."
  ),
  rule(
    id = "MISSING_INCOMPATIBLE/nothing-to-impute-from",
    code = "MISSING_INCOMPATIBLE",
    applies_when = "outcome_resolvable and total_missing > 0
                    and not runs_complete_cases and runs_impute",
    condition = "count(all_missing_columns) > 0",
    severity = "error",
    evidence = list(
      features = 'pluck(all_missing_columns, "name")',
      impute_type = "config.preprocessor_config.impute_type"
    ),
    slots = list(feature_labels = 'pluck(all_missing_columns, "label")'),
    message = "Imputation fills gaps from the values a column does have, and {feature_labels}{features| has none.| have none.} Drop {features|it|them} before imputing."
  ),
  # Whether leftover gaps are fatal is the algorithm's own answer: an error
  # where it refuses them, a warning where the answer is not the algorithm's to
  # give (a meta learner defers to its base learners) or where none is named.
  rule(
    id = "MISSING_INCOMPATIBLE/algorithm-refuses-gaps",
    code = "MISSING_INCOMPATIBLE",
    applies_when = "outcome_resolvable and total_missing > 0
                    and not runs_complete_cases and not runs_impute
                    and missing_evaluable and allows_missing === false",
    condition = "feature_missing > 0",
    severity = "error",
    evidence = list(
      n_missing = "feature_missing",
      n_features_missing = "n_features_missing",
      algorithm = "algorithm",
      algorithm_allows_missing = "allows_missing"
    ),
    message = "{algorithm} does not accept missing values, and nothing in this config removes them: {n_missing} missing {n_missing|value|values} across {n_features_missing} {n_features_missing|column|columns}. Impute, or drop the incomplete cases before training."
  ),
  rule(
    id = "MISSING_INCOMPATIBLE/tolerance-unknown",
    code = "MISSING_INCOMPATIBLE",
    applies_when = "outcome_resolvable and total_missing > 0
                    and not runs_complete_cases and not runs_impute
                    and missing_evaluable
                    and allows_missing !== false and allows_missing !== true",
    condition = "feature_missing > 0",
    severity = "warning",
    evidence = list(
      n_missing = "feature_missing",
      n_features_missing = "n_features_missing",
      algorithm = "algorithm",
      algorithm_allows_missing = "allows_missing"
    ),
    message = "{n_missing} missing {n_missing|value|values} across {n_features_missing} {n_features_missing|column|columns} reach the learner unchanged; not every algorithm accepts them."
  )
)


# %% Unevaluable ----
# Where the rule set knows it cannot answer, so that an evaluator reports
# "not fully validated" rather than "clean".
#
# One entry. `remove_cases_thres` and `remove_features_thres` drop what is
# missing *above a threshold*, and whether they resolve a dataset's gaps
# depends on the joint missingness pattern: the case step changes each
# feature's missing fraction, and carrying enough to compute that costs one
# entry per feature per missing-count bucket -- unbounded exactly on wide data,
# where it matters. `train()` rejects both steps, so this is reachable only for
# a standalone preprocessor config, and the reference implementation simulates
# them against the rows.
CHECKS_UNEVALUABLE <- list(
  list(
    id = "missing-after-thresholds",
    when = "not supervised and thresholds_set and total_missing > 0",
    affects = "MISSING_INCOMPATIBLE",
    reason = paste0(
      "Whether `remove_cases_thres` and `remove_features_thres` resolve this ",
      "dataset's gaps depends on the joint missingness pattern, which a ",
      "profile does not carry. Findings for MISSING_INCOMPATIBLE are ",
      "incomplete for this config."
    )
  )
)
