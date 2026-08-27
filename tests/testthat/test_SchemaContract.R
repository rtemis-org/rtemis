# test_SchemaContract.R
# ::rtemis::
# 2026- EDG rtemis.org

# The input-schema contract, checked from the package side.
#
#   A schema states what is true of the data. It never states what any
#   interface chooses to fill in.
#
# `data-raw/schema_contract.R` enforces this on the generated artifacts, but
# generation is a deliberate act and `data-raw/` is not in the built package.
# These tests check the same contract structurally, from what the package
# itself can see, so a violation surfaces on every `just test` rather than only
# on the next regeneration.
#
# The load-bearing one is "every setup_* formal has a default". That is *why*
# a config schema can require nothing: there is no argument a user must supply,
# so there is no key a document must carry.
#
# The last section audits the other direction: a rule written into an S7
# `validator` rather than declared on a property has no route into the schema
# at all, so every one must be mirrored into the registry or recorded.

# %% .contract_family ----
# Pair each of a family's leaf classes with the `setup_*` that builds it,
# tagged with the family base class the generator subtracts (NULL for a flat
# config, which has no family).
.contract_family <- function(base, ...) {
  lapply(list(...), function(pair) {
    list(cls = pair[[1L]], setup = pair[[2L]], base = base)
  })
}


# %% .contract_classes ----
# Every class `data-raw/schema_registry.R` publishes a config schema for, with
# the `setup_*` that builds it. Kept in step with the registry by
# "the class/setup mapping covers the registry" below, so an entry added there
# and not here fails rather than going untested.
.contract_classes <- c(
  .contract_family(
    DecompositionConfig,
    list(PCAConfig, "setup_PCA"),
    list(ICAConfig, "setup_ICA"),
    list(NMFConfig, "setup_NMF"),
    list(UMAPConfig, "setup_UMAP"),
    list(tSNEConfig, "setup_tSNE"),
    list(IsomapConfig, "setup_Isomap")
  ),
  .contract_family(
    ClusteringConfig,
    list(KMeansConfig, "setup_KMeans"),
    list(HardCLConfig, "setup_HardCL"),
    list(NeuralGasConfig, "setup_NeuralGas"),
    list(CMeansConfig, "setup_CMeans"),
    list(DBSCANConfig, "setup_DBSCAN")
  ),
  .contract_family(
    ResamplerConfig,
    list(KFoldConfig, "setup_Resampler"),
    list(StratSubConfig, "setup_Resampler"),
    list(StratBootConfig, "setup_Resampler"),
    list(BootstrapConfig, "setup_Resampler"),
    list(LOOCVConfig, "setup_Resampler"),
    list(CustomConfig, "setup_Resampler")
  ),
  .contract_family(TunerConfig, list(GridSearchConfig, "setup_GridSearch")),
  .contract_family(ExplanationConfig, list(SHAPConfig, "setup_SHAP")),
  .contract_family(
    ConformalConfig,
    list(SplitConformalConfig, "setup_SplitConformal"),
    list(CVPlusConfig, "setup_CVPlus"),
    list(CQRConfig, "setup_CQR")
  ),
  .contract_family(
    Hyperparameters,
    list(GLMHyperparameters, "setup_GLM"),
    list(GAMHyperparameters, "setup_GAM"),
    list(CARTHyperparameters, "setup_CART"),
    list(GLMNETHyperparameters, "setup_GLMNET"),
    list(GLMTreeHyperparameters, "setup_GLMTree"),
    list(LightCARTHyperparameters, "setup_LightCART"),
    list(LightRFHyperparameters, "setup_LightRF"),
    list(LightGBMHyperparameters, "setup_LightGBM"),
    list(LightRuleFitHyperparameters, "setup_LightRuleFit"),
    list(IsotonicHyperparameters, "setup_Isotonic"),
    list(LinearSVMHyperparameters, "setup_LinearSVM"),
    list(RadialSVMHyperparameters, "setup_RadialSVM"),
    list(MLPHyperparameters, "setup_MLP"),
    list(TabNetHyperparameters, "setup_TabNet"),
    list(RangerHyperparameters, "setup_Ranger"),
    list(SPLSHyperparameters, "setup_SPLS"),
    list(KNNHyperparameters, "setup_KNN"),
    list(MARSHyperparameters, "setup_MARS"),
    list(BARTHyperparameters, "setup_BART"),
    list(HALHyperparameters, "setup_HAL"),
    list(MonotonicHALHyperparameters, "setup_MonotonicHAL"),
    list(LINADHyperparameters, "setup_LINAD"),
    list(LINADForestHyperparameters, "setup_LINADForest"),
    list(NNLSHyperparameters, "setup_NNLS"),
    list(SuperLearnerHyperparameters, "setup_SuperLearner"),
    list(ModalityStackingHyperparameters, "setup_ModalityStacking"),
    list(
      ConditionalSuperLearnerHyperparameters,
      "setup_ConditionalSuperLearner"
    )
  ),
  .contract_family(
    NULL,
    list(ExecutionConfig, "setup_ExecutionConfig"),
    list(PreprocessorConfig, "setup_Preprocessor"),
    list(SuperConfig, "setup_SuperConfig"),
    list(DecomposeConfig, "setup_DecomposeConfig"),
    list(ClusterConfig, "setup_ClusterConfig")
  )
)


# %% .contract_no_setup ----
# Registered classes with no `setup_*` and no input form: the furniture of a
# record, the results classes describing what a run produced, and the findings
# `validate_config()` reports about a config. Nothing authors one, so none
# states a user's intent and none is a config.
.contract_no_setup <- c(
  "Provenance",
  "DataFingerprint",
  "RegressionMetrics",
  "ClassificationMetrics",
  "DecompositionMetrics",
  "RegressionMetricsRes",
  "ClassificationMetricsRes",
  "Diagnostic",
  "Diagnostics",
  "DataProfile"
)


# %% .contract_no_schema ----
# `setup_*` exports backing no published schema, and so outside the contract.
# `SuperConfigLive` holds in-memory tables rather than paths and does not
# serialize, so it has no document form and `dat_training` has nowhere to come
# from but the caller.
.contract_no_schema <- "setup_SuperConfigLive"


# %% .contract_refs ----
# Placeholder `$ref` targets for the properties whose type is another S7
# class. Those carry no `PropertySpec`, so `S7_to_JSONSchema()` requires each
# to be referenced or it aborts. Only the property's name and shape are under
# test, never the target, so the URL is derived rather than copied from
# `data-raw/schema_registry.R` -- which the built package cannot see.
.contract_refs <- function(cls, base = NULL) {
  props <- cls@properties
  if (!is.null(base)) {
    props <- props[own_prop_names(cls, base)]
  }
  needs_ref <- vapply(
    props,
    function(p) {
      !isTRUE(prop_role(p) %in% c("computed", "r_only")) && is.null(get_spec(p))
    },
    logical(1L)
  )
  nm <- names(props)[needs_ref]
  if (length(nm) == 0L) {
    return(NULL)
  }
  stats::setNames(
    paste0("https://schema.rtemis.org/", tolower(nm), "/v1/schema.json"),
    nm
  )
}


# %% .contract_schema ----
# The schema the generator would emit for one entry, in either kind.
.contract_schema <- function(entry, record = FALSE) {
  S7_to_JSONSchema(
    entry[["cls"]],
    id = paste0(
      "https://schema.rtemis.org/test/v1/",
      if (record) "record" else "schema",
      ".json"
    ),
    base = entry[["base"]],
    record = record,
    refs = .contract_refs(entry[["cls"]], entry[["base"]])
  )
}


# %% .constant_props ----
# Properties the schema pins to one value: the discriminator and the document's
# own `$schema`. A record does not require them -- the algorithm implies them
# and `prop_serialized()` keeps them out of what is written.
.constant_props <- function(schema) {
  names(Filter(function(p) "const" %in% names(p), schema[["properties"]]))
}


# %% .authored_props ----
# The properties a user may write: everything the schema declares, less the
# constants and less run state, which the schema declares `readOnly` because
# only a run can produce it (GLMNET's `lambda.min`, LightGBM's `best_iter`).
# Both kinds declare state; what a record adds is that it requires it.
.authored_props <- function(schema) {
  props <- schema[["properties"]]
  props <- Filter(function(p) !isTRUE(p[["readOnly"]]), props)
  setdiff(names(props), c("$schema", .constant_props(schema)))
}


test_that("every setup_* backing a schema takes no mandatory argument", {
  # The premise of the whole contract. A config schema requires nothing because
  # a user is never obliged to supply anything -- if that stops being true for
  # some argument, the schema and the API have silently diverged and a document
  # the schema accepts will fail at `do.call(setup_*, doc)`.
  for (nm in unique(vapply(.contract_classes, `[[`, character(1L), "setup"))) {
    fm <- formals(get(nm, envir = asNamespace("rtemis")))
    fm <- fm[names(fm) != "..."]
    mandatory <- names(fm)[vapply(
      fm,
      function(d) identical(d, quote(expr = )),
      logical(1L)
    )]
    expect_identical(
      mandatory,
      character(),
      info = paste0(nm, "() has mandatory formal(s)")
    )
  }
})


test_that("every setup_* export is classified", {
  # So a `setup_*` written years from now cannot escape the check above by
  # simply not being listed: it must be paired with the class it builds, or
  # declared to have no schema.
  setup_fns <- grep("^setup_", getNamespaceExports("rtemis"), value = TRUE)
  expect_gt(length(setup_fns), 0L)
  classified <- c(
    vapply(.contract_classes, `[[`, character(1L), "setup"),
    .contract_no_schema
  )
  expect_identical(
    sort(setdiff(setup_fns, classified)),
    character(),
    info = "unclassified setup_*: add to .contract_classes or .contract_no_schema"
  )
})


test_that("config schemas declare no required beyond a discriminator", {
  for (entry in .contract_classes) {
    schema <- .contract_schema(entry)
    stray <- setdiff(
      as.character(schema[["required"]]),
      c("$schema", "algorithm", "type")
    )
    expect_identical(stray, character(), info = entry[["cls"]]@name)
  }
})


test_that("config schemas emit no default keyword", {
  # Defaults are API policy and live in defaults/v1/defaults.json, versioned
  # independently. A schema is immutable once published, so a default in one
  # would pin an artifact to a package version.
  for (entry in .contract_classes) {
    schema <- .contract_schema(entry)
    defaulted <- names(Filter(
      function(p) "default" %in% names(p),
      schema[["properties"]]
    ))
    expect_identical(defaulted, character(), info = entry[["cls"]]@name)
  }
})


test_that("config schemas have no conditional demand for a key", {
  # A `then` may constrain a value but may not demand a key an implementation
  # could supply. The generator asserts this over the whole document; here it
  # is the class-level `allOf` that the registry contributes.
  for (entry in .contract_classes) {
    schema <- .contract_schema(entry)
    demanded <- unlist(lapply(
      schema[["allOf"]],
      function(clause) as.character(clause[["then"]][["required"]])
    ))
    expect_length(demanded, 0L)
  }
})


test_that("record schemas do require every non-constant property", {
  # The converse, so the two kinds cannot quietly converge: a record asserts
  # what ran, and nothing in it may fall back to a reader's defaults.
  for (entry in .contract_classes) {
    schema <- .contract_schema(entry, record = TRUE)
    declared <- setdiff(
      names(schema[["properties"]]),
      c("$schema", .constant_props(schema))
    )
    missing <- setdiff(declared, as.character(schema[["required"]]))
    expect_identical(missing, character(), info = entry[["cls"]]@name)
  }
})


test_that("every authored schema property is a setup_* formal", {
  # The "every document the schema accepts is accepted by
  # `do.call(setup_*, doc)`" direction, checked structurally rather than by
  # fuzzing: a schema that declares a field the setup function cannot take
  # publishes a document nothing can read.
  for (entry in .contract_classes) {
    schema <- .contract_schema(entry)
    formals_nm <- names(formals(get(
      entry[["setup"]],
      envir = asNamespace("rtemis")
    )))
    expect_identical(
      setdiff(.authored_props(schema), formals_nm),
      character(),
      info = paste0(entry[["cls"]]@name, " / ", entry[["setup"]])
    )
  }
})


test_that("no readOnly schema property is a setup_* formal", {
  # The converse, and what makes `readOnly` true rather than decorative: a
  # state field a `setup_*` accepts is one a user can write, so declaring it
  # read-only would misdescribe the document.
  #
  # Only where one `setup_*` builds one class. `setup_Resampler()` builds six,
  # so its formals are the union over all of them and say nothing about any
  # one: it takes `n_resamples` and discards it for LOOCV, whose count only
  # `resample()` can know.
  shared <- vapply(.contract_classes, `[[`, character(1L), "setup")
  shared <- names(Filter(function(n) n > 1L, table(shared)))
  for (entry in Filter(
    function(e) !e[["setup"]] %in% shared,
    .contract_classes
  )) {
    schema <- .contract_schema(entry)
    state <- names(Filter(
      function(p) isTRUE(p[["readOnly"]]),
      schema[["properties"]]
    ))
    formals_nm <- names(formals(get(
      entry[["setup"]],
      envir = asNamespace("rtemis")
    )))
    expect_identical(
      intersect(state, formals_nm),
      character(),
      info = paste0(entry[["cls"]]@name, " / ", entry[["setup"]])
    )
  }
})


# %% .contract_registry ----
# `data-raw/schema_registry.R` and its contract helpers, evaluated against the
# loaded namespace: the entries reference class objects, and `base_url` is
# supplied by `generate_schemas.R` rather than by the registry itself.
# `data-raw/` is absent from a built package, so a caller gets NULL there.
.contract_registry <- function() {
  registry <- testthat::test_path("..", "..", "data-raw", "schema_registry.R")
  contract <- testthat::test_path("..", "..", "data-raw", "schema_contract.R")
  if (!file.exists(registry) || !file.exists(contract)) {
    return(NULL)
  }
  env <- new.env(parent = asNamespace("rtemis"))
  env[["base_url"]] <- "https://schema.rtemis.org"
  sys.source(registry, envir = env)
  sys.source(contract, envir = env)
  env
}


# %% .registry_entries ----
# Every entry the registry publishes a schema for, family leaves and flat
# configs alike, in one flat list. Each carries the class it is generated from
# and, where the registry declares one, that class's `extra`.
.registry_entries <- function(env) {
  c(
    unlist(lapply(env[["families"]], `[[`, "algorithms"), recursive = FALSE),
    env[["flat_configs"]]
  )
}


test_that("the class/setup mapping covers the registry", {
  # `.contract_classes` is written out here rather than read from the registry,
  # so that the checks above still run in a built package. This is what keeps
  # the two in step.
  env <- .contract_registry()
  skip_if(is.null(env), "data-raw/ not available (built package)")

  leaves <- unlist(
    lapply(env[["families"]], `[[`, "algorithms"),
    recursive = FALSE
  )
  registered <- c(
    vapply(leaves, function(a) a[["cls"]]@name, character(1L)),
    vapply(env[["flat_configs"]], function(f) f[["cls"]]@name, character(1L))
  )
  mapped <- vapply(
    .contract_classes,
    function(e) e[["cls"]]@name,
    character(1L)
  )
  expect_identical(
    sort(setdiff(registered, c(mapped, .contract_no_setup))),
    character(),
    info = "registered but untested: add to .contract_classes"
  )
  expect_identical(
    sort(setdiff(mapped, registered)),
    character(),
    info = "tested but no longer registered: drop from .contract_classes"
  )

  # The base class the generator subtracts, which decides which properties a
  # leaf declares at all.
  for (family in env[["families"]]) {
    for (algo in family[["algorithms"]]) {
      entry <- Filter(
        function(e) identical(e[["cls"]]@name, algo[["cls"]]@name),
        .contract_classes
      )[[1L]]
      expect_identical(
        entry[["base"]]@name,
        family[["base_class"]]@name,
        info = algo[["cls"]]@name
      )
    }
  }
})


test_that("the registry declares no conditional demand for a key", {
  # Guards the artifacts directly, and is the check that would have caught a
  # `then = list(required = ...)` clause. `data-raw/` is absent from the built
  # package, so this runs from the source tree only.
  env <- .contract_registry()
  skip_if(is.null(env), "data-raw/ not available (built package)")

  entries <- .registry_entries(env)
  for (i in seq_along(entries)) {
    extra <- entries[[i]][["extra"]]
    if (is.null(extra)) {
      next
    }
    expect_identical(
      env[[".conditional_demands"]](extra),
      character(),
      info = paste0(
        entries[[i]][["cls"]]@name,
        ": a `then` may constrain a value but may not demand a key."
      )
    )
  }
})


# %% Class-validator audit -------------------------------------------------
# Property-level coverage needs no audit: a property with no role aborts
# `S7_to_JSONSchema()`, so nothing declared on a property can go unpublished. A
# hand-written `validator = function(self)` is not a property. It is enforced in
# R, absent from the schema, and there is nothing in either artifact to notice
# the difference -- so a config that `rtemis validate` accepts fails at
# `read_config()`, and every non-R client is a second-class one.
#
# `AGENTS.md`: "Hand-writing validation for a property usually means a factory
# argument was missed." What follows holds a registered class to that, and is
# the check that keeps working as the package grows: a validator added to a
# registered class fails here until it is mirrored into the class's `extra` or
# recorded below as a rule the schema language cannot carry.

# %% .validator_classes ----
# Every class in `cls`'s S7 ancestry that declares a validator, `S7_object`'s
# stock one excluded. The leaf's own validator is not the whole story:
# `MetaLearnerHyperparameters` sits between `SuperLearnerHyperparameters` and
# `Hyperparameters`, and the properties it constrains are exactly the ones the
# leaf's schema declares.
.validator_classes <- function(cls) {
  out <- list()
  while (inherits(cls, "S7_class")) {
    if (!identical(cls, S7::S7_object) && !is.null(cls@validator)) {
      out <- c(out, list(cls))
    }
    cls <- cls@parent
  }
  out
}


# %% .spec_driven_validator ----
# TRUE if a validator does nothing but call `check_applies_when()`. That reads
# the gate off each property's `applies_when` spec field, which
# `S7_to_JSONSchema()` emits into the same property's `x-rtemis` annotation --
# one declaration, published. It is a factory argument doing its job, so it is
# not hand-written validation and needs no `extra` to mirror it.
.spec_driven_validator <- function(fn) {
  expr <- body(fn)
  calls <- if (is.call(expr) && identical(expr[[1L]], quote(`{`))) {
    as.list(expr)[-1L]
  } else {
    list(expr)
  }
  length(calls) > 0L &&
    all(vapply(
      calls,
      function(e) is.call(e) && identical(e[[1L]], quote(check_applies_when)),
      logical(1L)
    ))
}


# %% .hand_written_validators ----
# Names of the classes in `cls`'s ancestry whose validator states a rule the
# property specs do not.
.hand_written_validators <- function(cls) {
  hand <- Filter(
    function(k) !.spec_driven_validator(k@validator),
    .validator_classes(cls)
  )
  vapply(hand, function(k) k@name, character(1L))
}


# %% .contract_validator_gaps ----
# Rules enforced in R and absent from the published schema, each with what
# stops it from being an `extra` clause. Not an exemption list: an entry is a
# claim that the schema language cannot carry the rule, and the test below
# checks in both directions, so a rule that becomes mirrorable -- or a
# validator that is removed -- fails here rather than lingering.
#
# Keyed by the class the validator is written on, which for the meta learners
# is an ancestor of three registered leaves.
.contract_validator_gaps <- c(
  GridSearchConfig = paste0(
    "`@randomize_p` must be set for a randomized search and unset for an ",
    "exhaustive one. The second half is a value constraint and could be ",
    "mirrored; the first is a `then` demanding a key, which the contract ",
    "bans outright -- and `randomize_p` has no default, so nothing can fill ",
    "it in. This is the case plan/validation-completeness.md item 3 exists ",
    "to resolve."
  ),
  MARSHyperparameters = paste0(
    "`@nfold` must be at least 2 when `@pmethod` is \"cv\". Both are plain ",
    "scalars, so an `if`/`then` over values expresses this exactly: ",
    "mirrorable, and not yet mirrored."
  ),
  LightRuleFitHyperparameters = paste0(
    "`@ifw` cannot be combined with `@ifw_lightgbm` or `@ifw_glmnet`. Both ",
    "of the latter are tunable, so each is published as a ",
    "scalar-or-`candidates` `oneOf` and the `then` would have to forbid TRUE ",
    "in either shape. The class also carries the GOSS rules below, inherited ",
    "from sharing LightGBM's sampling properties."
  ),
  LightGBMHyperparameters = paste0(
    "GOSS cannot be combined with bagging (`@data_sample_strategy` \"goss\" ",
    "with `@bagging_fraction` below 1), and `@top_rate` + `@other_rate` must ",
    "not exceed 1. JSON Schema compares a value against a constant, never ",
    "against a sibling property's value, so neither is expressible -- the ",
    "first needs `bagging_fraction < 1` and the second a sum across two ",
    "properties. Both are also tunable, so each is published as a ",
    "scalar-or-`candidates` `oneOf`, which a `then` would have to constrain ",
    "in both shapes."
  ),
  HALHyperparameters = paste0(
    "`@num_knots` must hold one value per interaction degree (`@max_degree`) ",
    "and be non-increasing across them. JSON Schema ties an array's length to ",
    "a constant, never to a sibling property's value, and cannot order an ",
    "array's elements at all."
  ),
  BARTHyperparameters = paste0(
    "`@num_chains` cannot exceed `@num_gfr`. JSON Schema compares a value ",
    "against a constant, never against another property."
  ),
  LINADHyperparameters = paste0(
    "`@global_features` must be a subset of `@linear_features`. Both are ",
    "arrays of feature names, and JSON Schema compares an element against a ",
    "constant or an enum, never against another property's contents -- there ",
    "is no vocabulary for one array containing another. `extra` carries only ",
    "`if`/`then` over `const`/`enum` values, so there is nothing to mirror."
  ),
  LINADForestHyperparameters = paste0(
    "The same `@global_features` subset-of `@linear_features` rule LINAD ",
    "carries, from sharing `linad_tree_props()`."
  ),
  MetaLearnerHyperparameters = paste0(
    "`@base_learners` must hold at least two uniquely and syntactically ",
    "named `Hyperparameters`. It is a hand-declared `new_property(class_list)` ",
    "published through the registry's `array_refs`, so it carries no ",
    "`PropertySpec` for `min_items` to come from, and its names are R list ",
    "names with no counterpart in the array the schema declares. Inherited by ",
    "the three registered meta learners."
  ),
  DataFingerprint = paste0(
    "`@hash`, `@encoding`, `@language` and `@data_structure` must be ",
    "non-empty; `@source` must be set when `@method` is \"file\"; and ",
    "`@column_names` must hold one value per column (`@n_cols`). The first ",
    "group is the missed factory argument AGENTS.md names -- `prop_string()` ",
    "has no `min_length` -- the second is a `then` demanding a key, and the ",
    "third ties an array's length to a sibling's value."
  )
)


test_that("a registered class's validator is mirrored in extra or recorded", {
  env <- .contract_registry()
  skip_if(is.null(env), "data-raw/ not available (built package)")

  for (entry in .registry_entries(env)) {
    carriers <- .hand_written_validators(entry[["cls"]])
    # An `extra` accounts for the class: it is the only place a cross-field
    # rule can be published, and where it mirrors some of a validator rather
    # than all of it the registry says which part and why -- `ExecutionConfig`
    # mirrors its `n_workers` rule and documents why `@future_plan` stays out.
    if (!is.null(entry[["extra"]])) {
      expect_true(
        "allOf" %in% names(entry[["extra"]]),
        info = paste0(
          entry[["cls"]]@name,
          ": `extra` carries class-level `allOf` rules and nothing else."
        )
      )
      next
    }
    expect_identical(
      setdiff(carriers, names(.contract_validator_gaps)),
      character(),
      info = paste0(
        entry[["cls"]]@name,
        ": validator enforces a rule the schema does not. Mirror it in the ",
        "registry entry's `extra`, move it onto the property's spec, or ",
        "record in `.contract_validator_gaps` what stops both."
      )
    )
  }
})


test_that("every recorded validator gap is still a gap", {
  # The converse, so the record cannot outlive what it describes: a validator
  # that is deleted, or rewritten in terms of its property specs, must be
  # struck from the list rather than left standing as a false claim about the
  # schema.
  env <- .contract_registry()
  skip_if(is.null(env), "data-raw/ not available (built package)")

  carriers <- unique(unlist(lapply(
    .registry_entries(env),
    function(entry) {
      if (is.null(entry[["extra"]])) {
        .hand_written_validators(entry[["cls"]])
      } else {
        character()
      }
    }
  )))
  expect_identical(
    sort(setdiff(names(.contract_validator_gaps), carriers)),
    character(),
    info = "recorded but no longer a gap: drop from .contract_validator_gaps"
  )
})


# %% Records satisfy the schemas generated from the same classes ------------
# This is the check the `rtemis` CLI used to run at write time, moved to where
# it belongs. A record and its schema are generated from one set of property
# declarations, so the two disagreeing is a bug in rtemis -- and a bug in rtemis
# is a test failure, not something to discover on a user's machine via whatever
# binary happens to be on their PATH. Run against freshly generated schemas, it
# also cannot go stale.
#
# It is `config_record()` that drifts: it decides which properties a record
# carries and must subtract the same family base the generator does. A class
# with an intermediate ancestor is where that goes wrong.
test_that("a record carries exactly the keys its record schema requires", {
  for (entry in .contract_classes) {
    cls <- entry[["cls"]]
    setup <- get(entry[["setup"]], envir = asNamespace("rtemis"))
    object <- setup()
    # A shared `setup_*` builds one variant of its family (`setup_Resampler()`
    # returns a KFoldConfig), so only the variant it actually builds is checked
    # here; the others are covered by the schema-shape tests above.
    if (!S7_inherits(object, cls)) {
      next
    }
    required <- as.character(.contract_schema(entry, record = TRUE)[[
      "required"
    ]])
    expect_setequal(names(config_record(object, object)), required)
  }
})


test_that("a meta learner's record nests one block per library entry", {
  # `base_learners` is a list of S7 objects, which `config_record()` has to
  # recognize as a third kind of property: not flat, not a single nested config.
  # Serialized flat it would carry no per-entry `origin`, which every `$ref`d
  # block requires.
  hyperparameters <- setup_SuperLearner(
    base_learners = list(setup_GLM(), setup_CART())
  )
  entries <- config_record(hyperparameters, hyperparameters)[["base_learners"]]
  # An *array* of `$ref`d blocks, in library order: a named list would
  # serialize as a JSON object, which the schema does not admit. Each entry
  # names itself with `algorithm`, which is where the R-side names come from.
  expect_null(names(entries))
  expect_identical(
    vapply(entries, `[[`, character(1L), "algorithm"),
    c("GLM", "CART")
  )
  for (entry in entries) {
    expect_named(entry, c("algorithm", "hyperparameters"))
    expect_true("origin" %in% names(entry[["hyperparameters"]]))
  }
})
