# test_SchemaContract.R
# ::rtemis::
# 2026- EDG rtemis.org

# The input-schema contract, checked from the package side.
#
#   A schema states what is true of the data. It never states what any
#   interface chooses to fill in.
#
# `rtemis.core::assert_config_contract()` enforces this on the generated artifacts, but
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
# at all, so every one must be replaced by a typed declaration.

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
# Every class `schema_catalog()` publishes a config schema for, with
# the `setup_*` that builds it. Kept in step with the catalog by
# "the class/setup mapping covers the catalog" below, so an entry added there
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
    list(DBSCANConfig, "setup_DBSCAN"),
    list(GMMConfig, "setup_GMM"),
    list(HOPACHConfig, "setup_HOPACH"),
    list(PAMConfig, "setup_PAM"),
    list(PAMKConfig, "setup_PAMK"),
    list(SpectralConfig, "setup_Spectral")
  ),
  .contract_family(
    ResamplerConfig,
    list(KFoldConfig, "setup_KFold"),
    list(StratSubConfig, "setup_StratSub"),
    list(StratBootConfig, "setup_StratBoot"),
    list(BootstrapConfig, "setup_Bootstrap"),
    list(LOOCVConfig, "setup_LOOCV"),
    list(CustomConfig, "setup_Custom")
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
    IngestConfig,
    list(DelimitedIngestConfig, "setup_DelimitedIngest"),
    list(ParquetIngestConfig, "setup_ParquetIngest"),
    list(XLSXIngestConfig, "setup_XLSXIngest"),
    list(RDSIngestConfig, "setup_RDSIngest"),
    list(DTAIngestConfig, "setup_DTAIngest"),
    list(ARFFIngestConfig, "setup_ARFFIngest")
  ),
  .contract_family(
    PartitionConfig,
    list(RandomPartitionConfig, "setup_RandomPartition"),
    list(TimePartitionConfig, "setup_TimePartition"),
    list(GroupPartitionConfig, "setup_GroupPartition"),
    list(PredefinedPartitionConfig, "setup_PredefinedPartition")
  ),
  .contract_family(
    ExecutionConfig,
    list(SerialExecutionConfig, "setup_SerialExecution"),
    list(FutureExecutionConfig, "setup_FutureExecution"),
    list(MiraiExecutionConfig, "setup_MiraiExecution")
  ),
  .contract_family(
    NULL,
    list(PreprocessorConfig, "setup_Preprocessor"),
    list(SupervisedPreprocessorConfig, "setup_SupervisedPreprocessor"),
    list(SuperConfigPaths, "setup_SuperConfig"),
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
  "DataRef",
  "RegressionMetrics",
  "ClassificationMetrics",
  "ClusteringMetrics",
  "DecompositionMetrics",
  "RegressionMetricsRes",
  "ClassificationMetricsRes",
  "Diagnostic",
  "Diagnostics",
  "DataProfile"
)


# %% .contract_no_schema ----
# `setup_*` exports backing no published schema, and so outside the contract.
# `setup_SuperConfigLive()` builds a `SuperConfigTabular`, which holds
# in-memory tables rather than paths and does not serialize, so it has no
# document form and `dat_training` has nowhere to come from but the caller.
.contract_no_schema <- "setup_SuperConfigLive"


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
    record = record
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
    # `formals()` on a zero-argument function (`setup_LOOCV()`) returns NULL
    # rather than an empty pairlist -- the strongest case of "no mandatory
    # argument", not an exception to it.
    if (is.null(fm)) {
      fm <- list()
    }
    fm <- fm[names(fm) != "..."]
    # `character(...)` rather than a bare subset: `names(list())` is NULL, so
    # zero remaining formals would otherwise compare NULL against
    # `character()` below and fail on identity rather than content.
    mandatory <- as.character(names(fm)[vapply(
      fm,
      function(d) identical(d, quote(expr = )),
      logical(1L)
    )])
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
  # is the class-level `allOf` that the catalog contributes.
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
  # Only where one `setup_*` builds one class. Every registered family now
  # has a dedicated constructor per leaf (resampler included, since
  # `setup_Resampler(type = )` was split into `setup_KFold()`/`setup_StratSub()`/
  # etc.), so this exclusion is defensive rather than load-bearing today: it
  # protects against a future family sharing one constructor across variants,
  # the way `setup_Resampler()` once did.
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
    # `as.character(...)`: `formals()` on a zero-argument function
    # (`setup_LOOCV()`) returns NULL, and `names(NULL)` stays NULL rather than
    # `character(0)`.
    formals_nm <- as.character(names(formals(get(
      entry[["setup"]],
      envir = asNamespace("rtemis")
    ))))
    expect_identical(
      intersect(state, formals_nm),
      character(),
      info = paste0(entry[["cls"]]@name, " / ", entry[["setup"]])
    )
  }
})


# %% .catalog_entries ----
.catalog_entries <- function(catalog) {
  c(
    unlist(lapply(catalog$families, `[[`, "algorithms"), recursive = FALSE),
    catalog$flat_configs
  )
}


test_that("catalog family discriminators agree with the record writer", {
  for (family in schema_catalog()$families) {
    for (leaf in family$algorithms) {
      expect_identical(family_base(leaf$cls), family$base_class)
      expect_identical(
        schema_publication(family_base(leaf$cls))@discriminator,
        family$discriminator
      )
      expect_identical(family_discriminator(leaf$cls), family$discriminator)
    }
  }
})


test_that("the independent class/setup mapping covers the catalog", {
  # `.contract_classes` is written out here rather than read from the catalog,
  # so that the checks above still run in a built package. This is what keeps
  # the two in step.
  env <- schema_catalog()

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


test_that("every generated class clause satisfies the config contract", {
  for (entry in .catalog_entries(schema_catalog())) {
    expect_no_error(rtemis.core::assert_config_contract(
      list(allOf = class_rule_clauses(entry$cls)),
      id = entry$cls@name
    ))
  }
})


# %% .validator_classes ----
# Every class in `cls`'s S7 ancestry that declares a validator, `S7_object`'s
# stock one excluded. The leaf's own validator is not the whole story:
# `MetaLearnerHyperparameters` sits between `SuperLearnerHyperparameters` and
# `Hyperparameters`, and the properties it constrains are exactly the ones the
# leaf's schema declares.
.validator_classes <- function(cls) {
  out <- list()
  while (inherits(cls, "S7_class")) {
    if (
      !identical(cls, S7::S7_object) && !is.null(schema_native_validator(cls))
    ) {
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
# a generated validator whose declaration is already published.
.spec_driven_validator <- function(fn) {
  if (identical(fn, check_applies_when)) {
    return(TRUE)
  }
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
    function(k) !.spec_driven_validator(schema_native_validator(k)),
    .validator_classes(cls)
  )
  vapply(hand, function(k) k@name, character(1L))
}


test_that("published classes have no opaque native validators", {
  carriers <- unique(unlist(lapply(
    .catalog_entries(schema_catalog()),
    function(entry) .hand_written_validators(entry$cls)
  )))
  expect_length(carriers, 0L)
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
    # A bare call has a default for every formal (the whole contract's
    # premise), but a default is not always a *usable* one: `setup_Custom()`
    # takes `resamples = NULL` and then rejects NULL, deliberately -- there is
    # no meaningful empty Custom resampler, only a config that has not been
    # told its resamples yet. That class's record shape is covered on its own
    # below, with real resamples.
    object <- tryCatch(setup(), error = function(e) NULL)
    if (is.null(object)) {
      next
    }
    # A shared `setup_*`, where one exists, builds one variant of its family,
    # so only the variant it actually builds is checked here; the others are
    # covered by the schema-shape tests above. Every resampler constructor is
    # now dedicated to its own leaf, so this guard no longer excludes any of
    # them.
    if (!S7_inherits(object, cls)) {
      next
    }
    required <- as.character(.contract_schema(entry, record = TRUE)[[
      "required"
    ]])
    expect_setequal(names(config_record(object, object)), required)
  }
})


test_that("a CustomConfig record carries exactly the keys its record schema requires", {
  # Not reachable via a bare `setup_Custom()` call (see above), so checked
  # directly with real resamples.
  entry <- Filter(
    function(e) identical(e[["cls"]]@name, "CustomConfig"),
    .contract_classes
  )[[1L]]
  object <- setup_Custom(resamples = list(1:3, 2:4))
  required <- as.character(.contract_schema(entry, record = TRUE)[["required"]])
  expect_setequal(names(config_record(object, object)), required)
})


test_that("a meta learner's record carries one block per library entry", {
  # `base_learners` is a list of S7 objects, which `config_record()` has to
  # recognize as a third kind of property: not a scalar, not a single nested
  # config. Merged into the parent's own fields it would carry no per-entry
  # `origin`, which every `$ref`d block requires.
  hyperparameters <- setup_SuperLearner(
    base_learners = list(setup_GLM(), setup_CART())
  )
  entries <- config_record(hyperparameters, hyperparameters)[["base_learners"]]
  # The map preserves library identity separately from algorithm identity.
  expect_identical(names(entries), c("GLM", "CART"))
  expect_identical(
    unname(vapply(entries, `[[`, character(1L), "algorithm")),
    c("GLM", "CART")
  )
  for (entry in entries) {
    # Flat, like every family block: the discriminator leads -- a dispatcher
    # keys its `if/then` on it, and a block that does not lead with it matches
    # no branch -- and the leaf's own settings follow as siblings.
    expect_identical(names(entry)[[1L]], "algorithm")
    # `origin` accounts for every setting the block carries. That is what makes
    # the entry readable on its own, and it is the half a flat *merge* into the
    # parent would lose.
    expect_setequal(
      names(entry[["origin"]]),
      setdiff(names(entry), c("algorithm", "origin"))
    )
  }
})
