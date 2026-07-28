# test_ConfigProps.R
# ::rtemis::
# 2026- EDG rtemis.org

# Shared invariants for the DecompositionConfig / ClusteringConfig families,
# migrated to the prop_* factories (see 09_DecompositionConfig.R,
# 07_ClusteringConfig.R). Mirrors the Hyperparameters drift/serialization
# guards.

# %% Decomposition ----
.decomp_classes <- list(
  PCA = PCAConfig,
  ICA = ICAConfig,
  NMF = NMFConfig,
  UMAP = UMAPConfig,
  tSNE = tSNEConfig,
  Isomap = IsomapConfig
)

test_that("setup_* decomposition defaults do not drift from property defaults", {
  for (nm in names(.decomp_classes)) {
    setup_defaults <- get(paste0("setup_", nm))()@config
    class_defaults <- .decomp_classes[[nm]]()@config
    expect_identical(setup_defaults, class_defaults, info = nm)
  }
})

test_that("decomposition configs expose a computed config and constant algorithm", {
  for (nm in names(.decomp_classes)) {
    cfg <- get(paste0("setup_", nm))()
    expect_s7_class(cfg, DecompositionConfig)
    expect_identical(cfg@algorithm, nm, info = nm)
    # `$` / `[[` route through the computed config list.
    expect_identical(cfg[["k"]], cfg@config[["k"]], info = nm)
    # algorithm is a computed constant.
    expect_error(cfg@algorithm <- "PCA")
  }
})

test_that("DecompositionConfig is abstract", {
  expect_error(DecompositionConfig())
})

test_that("decomposition config validators enforce bounds and enums", {
  expect_error(setup_PCA(k = 0L))
  expect_error(setup_ICA(type = "bogus"))
  expect_error(setup_ICA(alpha = 3)) # max 2
  expect_error(setup_UMAP(metric = "chebyshev"))
  expect_no_error(setup_tSNE(theta = 1)) # inclusive max
  expect_error(setup_tSNE(theta = 1.5))
})

test_that("decomposition config setter routes and rejects unknown keys", {
  cfg <- setup_PCA(k = 3L)
  cfg@config[["k"]] <- 5L
  expect_identical(cfg@k, 5L)
  expect_error(cfg@config[["k"]] <- 0L) # validated
  expect_error(cfg@config[["bogus"]] <- 1L, "Unknown")
})

test_that("DecompositionConfig generates its JSON Schema", {
  s <- S7_to_JSONSchema(
    PCAConfig,
    id = "https://schema.rtemis.org/decomposition/pca/v1/schema.json",
    base = DecompositionConfig
  )
  expect_setequal(names(s[["properties"]]), c("k", "center", "scale", "tol"))
  expect_identical(s[["properties"]][["k"]][["minimum"]], 1L)
  # tSNE `Y_init` is a matrix: an array of row arrays.
  s <- S7_to_JSONSchema(
    tSNEConfig,
    id = "https://example.org/x.json",
    base = DecompositionConfig
  )
  y <- s[["properties"]][["Y_init"]]
  expect_identical(as.character(y[["type"]]), c("array", "null"))
  expect_identical(y[["items"]][["type"]], "array")
  expect_identical(y[["items"]][["items"]][["type"]], "number")
})


# %% Clustering ----
.clust_classes <- list(
  KMeans = KMeansConfig,
  HardCL = HardCLConfig,
  NeuralGas = NeuralGasConfig,
  CMeans = CMeansConfig,
  DBSCAN = DBSCANConfig
)

test_that("setup_* clustering defaults do not drift from property defaults", {
  for (nm in names(.clust_classes)) {
    setup_defaults <- get(paste0("setup_", nm))()@config
    class_defaults <- .clust_classes[[nm]]()@config
    expect_identical(setup_defaults, class_defaults, info = nm)
  }
})

test_that("clustering configs expose a computed config and constant algorithm", {
  for (nm in names(.clust_classes)) {
    cfg <- get(paste0("setup_", nm))()
    expect_s7_class(cfg, ClusteringConfig)
    expect_identical(cfg@algorithm, nm, info = nm)
    expect_error(cfg@algorithm <- "KMeans")
  }
})

test_that("ClusteringConfig is abstract", {
  expect_error(ClusteringConfig())
})

test_that("clustering config validators enforce bounds and enums", {
  expect_error(setup_KMeans(dist = "chebyshev"))
  expect_error(setup_DBSCAN(eps = 0)) # exclusive min
  expect_error(setup_CMeans(m = 1)) # exclusive min
  expect_error(setup_DBSCAN(search = "bogus"))
})

test_that("CMeans weights broadcast and control is an open object", {
  cfg <- setup_CMeans(k = 3L)
  expect_true("control" %in% names(cfg@config))
  s <- S7_to_JSONSchema(
    CMeansConfig,
    id = "https://schema.rtemis.org/clustering/cmeans/v1/schema.json",
    base = ClusteringConfig
  )
  # `weights`: one number for every case, or a per-case vector.
  w <- s[["properties"]][["weights"]]
  expect_length(w[["oneOf"]], 2L)
  expect_identical(w[["oneOf"]][[1L]][["type"]], "number")
  expect_identical(w[["oneOf"]][[2L]][["type"]], "array")
  # `control`: an opaque pass-through to the backend.
  expect_identical(s[["properties"]][["control"]][["type"]], "object")
})


# %% Dispatcher generation ----
test_that("S7_dispatcher_JSONSchema derives enum, leaf refs, and allOf", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(PCAConfig, ICAConfig, tSNEConfig),
    id = "https://schema.rtemis.org/decomposition/v1/schema.json",
    payload = "config",
    title = "rtemis DecompositionConfig",
    instance_schema_url = "https://schema.rtemis.org/decomposition/v1/schema.json"
  )
  # algorithm enum derived from the classes' computed @algorithm.
  expect_identical(
    as.character(s[["properties"]][["algorithm"]][["enum"]]),
    c("PCA", "ICA", "tSNE")
  )
  # required = algorithm + payload; additionalProperties closed.
  expect_identical(as.character(s[["required"]]), c("algorithm", "config"))
  expect_false(s[["additionalProperties"]])
  # $schema const present when instance_schema_url is set.
  expect_identical(
    s[["properties"]][["$schema"]][["const"]],
    "https://schema.rtemis.org/decomposition/v1/schema.json"
  )
  # one allOf clause per class, dispatching the payload to the leaf URL by
  # lowercase algorithm slug.
  expect_length(s[["allOf"]], 3L)
  clause <- s[["allOf"]][[3L]]
  expect_identical(
    clause[["if"]][["properties"]][["algorithm"]][["const"]],
    "tSNE"
  )
  expect_identical(
    clause[["then"]][["properties"]][["config"]][["$ref"]],
    "https://schema.rtemis.org/decomposition/tsne/v1/schema.json"
  )
})

test_that("dispatcher supports a custom discriminator and top-level mode", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(KFoldConfig, LOOCVConfig),
    id = "https://schema.rtemis.org/resampler/v1/schema.json",
    discriminator = "type",
    payload = NULL
  )
  expect_identical(
    as.character(s[["properties"]][["type"]][["enum"]]),
    c("KFold", "LOOCV")
  )
  # Top-level mode composes the leaf into the object, so strictness comes
  # from unevaluatedProperties, not additionalProperties.
  expect_false(s[["unevaluatedProperties"]])
  expect_false("additionalProperties" %in% names(s))
  # No payload property, and `then` applies the leaf $ref to the whole object.
  expect_false("config" %in% names(s[["properties"]]))
  expect_identical(
    s[["allOf"]][[1L]][["then"]][["$ref"]],
    "https://schema.rtemis.org/resampler/kfold/v1/schema.json"
  )
  # Each `if` requires the discriminator: a properties-only `if` is vacuously
  # true when absent, which would apply every branch at once.
  expect_identical(
    as.character(s[["allOf"]][[1L]][["if"]][["required"]]),
    "type"
  )
  expect_identical(as.character(s[["required"]]), "type")
})

test_that("dispatcher rejects a non-constant or duplicated discriminator", {
  # DecompositionConfig subclasses key on `algorithm`, not `type`.
  expect_error(
    S7_dispatcher_JSONSchema(
      classes = list(PCAConfig),
      id = "https://x/d/v1/schema.json",
      discriminator = "features"
    )
  )
  expect_error(
    S7_dispatcher_JSONSchema(
      classes = list(PCAConfig, PCAConfig),
      id = "https://x/d/v1/schema.json"
    ),
    "Duplicate"
  )
})

test_that("S7_to_JSONSchema emits $refs for nested config properties", {
  s <- S7_to_JSONSchema(
    DecomposeConfig,
    id = "https://schema.rtemis.org/decompose/v1/schema.json",
    refs = c(
      decomposition_config = "https://schema.rtemis.org/decomposition/v1/schema.json"
    )
  )
  # `decomposition_config` accepts NULL, so the ref is wrapped in a oneOf.
  ref <- s[["properties"]][["decomposition_config"]]
  expect_length(ref[["oneOf"]], 2L)
  expect_identical(ref[["oneOf"]][[1L]][["type"]], "null")
  expect_identical(
    ref[["oneOf"]][[2L]][["$ref"]],
    "https://schema.rtemis.org/decomposition/v1/schema.json"
  )
  # A non-nullable nested config emits a bare $ref.
  sup <- S7_to_JSONSchema(
    SuperConfig,
    id = "https://schema.rtemis.org/supervised/v1/schema.json",
    refs = c(
      preprocessor_config = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      decomposition_config = "https://schema.rtemis.org/decomposition/v1/schema.json",
      hyperparameters = "https://schema.rtemis.org/hyperparameters/v1/schema.json",
      tuner_config = "https://schema.rtemis.org/tuner/v1/schema.json",
      outer_resampling_config = "https://schema.rtemis.org/resampler/v1/schema.json",
      execution_config = "https://schema.rtemis.org/execution/v1/schema.json"
    )
  )
  expect_identical(
    sup[["properties"]][["execution_config"]][["$ref"]],
    "https://schema.rtemis.org/execution/v1/schema.json"
  )
  # Property order follows the class declaration.
  expect_identical(
    names(s[["properties"]]),
    intersect(names(DecomposeConfig@properties), names(s[["properties"]]))
  )
  # `refs` must name real properties.
  expect_error(
    S7_to_JSONSchema(
      DecomposeConfig,
      id = "https://x/a.json",
      refs = c(nope = "u")
    ),
    "no such"
  )
})

test_that("closed = FALSE omits additionalProperties for composed leaves", {
  args <- list(
    KFoldConfig,
    id = "https://schema.rtemis.org/resampler/kfold/v1/schema.json",
    base = ResamplerConfig
  )
  expect_false(do.call(S7_to_JSONSchema, args)[["additionalProperties"]])
  expect_false(
    "additionalProperties" %in%
      names(do.call(S7_to_JSONSchema, c(args, list(closed = FALSE))))
  )
})

test_that("serialized configs carry only declared parameters", {
  # Constants, run state, and data-dependent values are re-derived on read, so
  # they are not written. Constants do appear in the schema (as `const`); the
  # others are what a portable config omits.
  h <- setup_LightRF(nrounds = 100L)
  hp <- serializable_props(h)
  expect_identical(names(hp), c("algorithm", "hyperparameters"))
  expect_length(h@constant_hyperparameters, 4L)
  expect_false(any(
    h@constant_hyperparameters %in% names(hp[["hyperparameters"]])
  ))
  # tSNE's Y_init (a matrix) has no JSON form.
  expect_false(
    "Y_init" %in% names(serializable_props(setup_tSNE())[["config"]])
  )
  # A resampler serializes type + n_resamples + its settings, but not id_strat.
  rs <- serializable_props(setup_Resampler(n_resamples = 5L, type = "KFold"))
  expect_true(all(c("type", "n_resamples") %in% names(rs)))
  expect_false("id_strat" %in% names(rs))
  # LOOCV leaves `n_resamples` unset.
  expect_null(
    serializable_props(setup_Resampler(type = "LOOCV"))[["n_resamples"]]
  )
  # A tuner keeps its nested resampler config (it serializes as its own schema).
  expect_true(
    "resampler_config" %in%
      names(serializable_props(setup_GridSearch())[["config"]])
  )
})

test_that("dispatcher generates the discriminator from a spec", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(PCAConfig, ICAConfig),
    id = "https://schema.rtemis.org/decomposition/v1/schema.json",
    payload = "config",
    discriminator_description = "Decomposition algorithm name."
  )
  algorithm <- s[["properties"]][["algorithm"]]
  expect_identical(algorithm[["type"]], "string")
  expect_identical(as.character(algorithm[["enum"]]), c("PCA", "ICA"))
  expect_identical(algorithm[["description"]], "Decomposition algorithm name.")
  # Annotated like every other generated property, so a consumer reads one
  # vocabulary rather than special-casing the discriminator.
  expect_identical(algorithm[["x-rtemis"]][["type"]], "string")
  # The factory default never reaches the schema.
  expect_false("default" %in% names(algorithm))
})


test_that("dispatcher emits the family base's shared properties", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(PCAConfig, ICAConfig),
    id = "https://schema.rtemis.org/decomposition/v1/schema.json",
    payload = "config",
    base = DecompositionConfig
  )
  # Generated from the same PropertySpec the R class enforces, so the two
  # cannot drift.
  spec <- get_spec(DecompositionConfig@properties[["features"]])
  expect_identical(s[["properties"]][["features"]], spec_to_schema(spec))
  expect_identical(
    as.character(s[["properties"]][["features"]][["type"]]),
    c("array", "null")
  )
  expect_identical(s[["properties"]][["features"]][["minItems"]], 2L)
  # `algorithm` is the discriminator: emitted from the variant enum, not from
  # the base's raw `class_character` declaration.
  expect_identical(
    as.character(s[["properties"]][["algorithm"]][["enum"]]),
    c("PCA", "ICA")
  )
})


test_that("a resampler declares n_resamples per type, not on the base", {
  # The five configurable types carry it as ordinary config with a default;
  # LOOCV derives it from the data, so it is state. That is what lets each leaf
  # state its own contract instead of the dispatcher carrying a
  # "required unless LOOCV" rule.
  kfold <- S7_to_JSONSchema(
    KFoldConfig,
    id = "https://schema.rtemis.org/resampler/kfold/v1/schema.json",
    base = ResamplerConfig,
    closed = FALSE
  )
  expect_identical(kfold[["properties"]][["n_resamples"]][["type"]], "integer")
  expect_null(kfold[["properties"]][["n_resamples"]][["readOnly"]])

  loocv <- S7_to_JSONSchema(
    LOOCVConfig,
    id = "https://schema.rtemis.org/resampler/loocv/v1/schema.json",
    base = ResamplerConfig,
    closed = FALSE
  )
  expect_true(loocv[["properties"]][["n_resamples"]][["readOnly"]])
  expect_identical(
    loocv[["properties"]][["n_resamples"]][["x-rtemis"]][["role"]],
    "state"
  )
  # The dispatcher no longer carries it, and no `if/then` branch requires it.
  disp <- S7_dispatcher_JSONSchema(
    classes = list(KFoldConfig, LOOCVConfig),
    id = "https://schema.rtemis.org/resampler/v1/schema.json",
    discriminator = "type",
    payload = NULL,
    base = ResamplerConfig
  )
  expect_identical(names(disp[["properties"]]), "type")
  expect_null(disp[["allOf"]][[1L]][["then"]][["required"]])
})


test_that("dispatcher skips spec-less base machinery and needs no base", {
  # Hyperparameters' base properties are all computed views or run state.
  s <- S7_dispatcher_JSONSchema(
    classes = list(CARTHyperparameters, GLMHyperparameters),
    id = "https://schema.rtemis.org/hyperparameters/v1/schema.json",
    payload = "hyperparameters",
    base = Hyperparameters
  )
  expect_identical(
    names(s[["properties"]]),
    c("algorithm", "hyperparameters")
  )
  # `base` is optional.
  bare <- S7_dispatcher_JSONSchema(
    classes = list(KMeansConfig),
    id = "https://schema.rtemis.org/clustering/v1/schema.json",
    payload = "config"
  )
  expect_identical(names(bare[["properties"]]), c("algorithm", "config"))
  expect_error(
    S7_dispatcher_JSONSchema(
      classes = list(KMeansConfig),
      id = "https://x/c/v1/schema.json",
      base = "ClusteringConfig"
    ),
    "must be an S7 class"
  )
})


test_that("dispatcher orders base properties after the payload", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(PCAConfig),
    id = "https://schema.rtemis.org/decomposition/v1/schema.json",
    payload = "config",
    base = DecompositionConfig,
    instance_schema_url = "https://schema.rtemis.org/decomposition/v1/schema.json"
  )
  expect_identical(
    names(s[["properties"]]),
    c("$schema", "algorithm", "config", "features")
  )
})


# %% Serialization ----
test_that("config families serialize to their canonical public shape", {
  # DecompositionConfig -> {algorithm, config[, features]}
  expect_identical(
    names(serializable_props(setup_PCA(k = 3L))),
    c("algorithm", "config")
  )
  expect_identical(
    names(serializable_props(setup_PCA(k = 3L, features = c("a", "b")))),
    c("algorithm", "config", "features")
  )
  # ClusteringConfig -> {algorithm, config}
  expect_identical(
    names(serializable_props(setup_KMeans(k = 3L))),
    c("algorithm", "config")
  )
  # The per-algorithm properties are NOT leaked as siblings of config.
  s7l <- S7_to_list(setup_DBSCAN(eps = 0.5))
  expect_identical(sort(names(s7l)), c("algorithm", "config"))
  expect_false("eps" %in% names(s7l))
})

# %% x-rtemis annotations ----
test_that("x-rtemis agrees with the standard keywords, for every property", {
  # The annotation block is a second description of the same property. It is
  # generated from the same spec, so it cannot drift by construction -- but a
  # bug in either emitter would let the two disagree, and a consumer trusting
  # the annotation over the keywords would then generate a wrong model.
  for (cls in spec_classes()) {
    for (nm in spec_prop_names(cls)) {
      schema <- spec_to_schema(get_spec(cls@properties[[nm]]))
      ann <- schema[["x-rtemis"]]
      label <- paste0(cls@name, "@", nm)
      expect_false(is.null(ann), info = label)
      container <- if (is.null(ann[["container"]])) {
        "none"
      } else {
        ann[["container"]]
      }
      # A container holds values; a tunable array holds search values.
      expect_false(
        isTRUE(ann[["tunable"]]) && isTRUE(ann[["broadcast"]]),
        info = label
      )
      types <- as.character(schema[["type"]])
      if (isTRUE(ann[["tunable"]]) || isTRUE(ann[["broadcast"]])) {
        expect_true("oneOf" %in% names(schema), info = label)
      } else if (container %in% c("array", "matrix")) {
        expect_true("array" %in% types, info = label)
      } else if (container == "map") {
        expect_true("object" %in% types, info = label)
        expect_true("additionalProperties" %in% names(schema), info = label)
      } else if (identical(ann[["role"]], "constant")) {
        # A constant asserts its value; it has no `type` keyword.
        expect_true("const" %in% names(schema), info = label)
      } else {
        expect_true(ann[["type"]] %in% types, info = label)
      }
    }
  }
})


test_that("x-rtemis is what separates a tunable from a broadcast", {
  # These two emit structurally identical standard keywords: `oneOf` over a
  # scalar and an array of that scalar. The tunable branch carries a prose
  # description, but prose is not a contract -- only the annotation says which
  # is a search space and which is a value applied to every element.
  strip <- function(branches) {
    lapply(branches, function(b) b[setdiff(names(b), "description")])
  }
  tunable <- spec_to_schema(get_spec(prop_float(1, tunable = TRUE)))
  broadcast <- spec_to_schema(
    get_spec(prop_float(1, vector = TRUE, broadcast = TRUE))
  )
  expect_identical(strip(tunable[["oneOf"]]), strip(broadcast[["oneOf"]]))
  expect_true(tunable[["x-rtemis"]][["tunable"]])
  expect_null(tunable[["x-rtemis"]][["container"]])
  expect_true(broadcast[["x-rtemis"]][["broadcast"]])
  expect_identical(broadcast[["x-rtemis"]][["container"]], "array")
})


test_that("x-rtemis carries what the keywords cannot express", {
  # `data_bound` exists only in prose in the standard keywords; a code
  # generator needs it structurally.
  mtry <- spec_to_schema(get_spec(
    prop_integer(NULL, min = 1L, nullable = TRUE, data_bound = "n_features")
  ))
  expect_identical(mtry[["x-rtemis"]][["data_bound"]], "n_features")
  # Data-dependence decides whether a value is written to a portable config.
  centers <- spec_to_schema(get_spec(
    prop_map(prop_float(0), nullable = TRUE, data_dependent = TRUE)
  ))
  expect_true(centers[["x-rtemis"]][["data_dependent"]])
  expect_identical(centers[["x-rtemis"]][["container"]], "map")
})


# %% Wire strictness ----
# Principle 2 of `plan/wire-vocabulary.md`: nothing is dropped silently. A key
# a config does not declare is a stale name, a typo, or a field from another
# variant — every one of which trains something other than what was asked for
# if it is quietly ignored.
test_that("every .list_to_* rejects a key its config does not declare", {
  reject <- function(expr) {
    err <- tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) e
    )
    expect_s3_class(err, "rtemis_value_error")
    conditionMessage(err)
  }
  reject(.list_to_PreprocessorConfig(list(scale = TRUE, bogus = 1)))
  reject(.list_to_ResamplerConfig(list(type = "KFold", bogus = 1)))
  reject(.list_to_Hyperparameters(list(
    algorithm = "CART",
    hyperparameters = list(bogus = 1)
  )))
  reject(.list_to_DecompositionConfig(list(algorithm = "PCA", bogus = 1)))
  reject(.list_to_ClusteringConfig(list(algorithm = "KMeans", bogus = 1)))
  reject(.list_to_TunerConfig(list(
    type = "GridSearch",
    config = list(bogus = 1)
  )))
  reject(.list_to_SuperConfig(list(bogus = 1)))
  reject(.list_to_DecomposeConfig(list(bogus = 1)))
  reject(.list_to_ClusterConfig(list(bogus = 1)))
})

test_that("a field from another variant is named, not dropped", {
  # `train_p` is a StratSub/StratBoot field. Silently ignoring it on a KFold
  # config is the case that motivated this: the user asked for a 75/25 split
  # and would have got tenfold CV without a word.
  msg <- tryCatch(
    .list_to_ResamplerConfig(list(
      type = "KFold",
      n_resamples = 5L,
      train_p = 0.75
    )),
    error = conditionMessage
  )
  expect_match(msg, "KFold resampler", fixed = TRUE)
  expect_match(msg, "train_p", fixed = TRUE)
  # No plausible near-miss, so no misleading suggestion.
  expect_no_match(msg, "did you mean", fixed = TRUE)
  # The same key on a variant that declares it is accepted.
  expect_s7_class(
    .list_to_ResamplerConfig(list(
      type = "StratSub",
      n_resamples = 5L,
      train_p = 0.75
    )),
    StratSubConfig
  )
})

test_that("a near-miss key is named with its likely intent", {
  # A typo: small edit distance.
  expect_match(
    tryCatch(
      .list_to_Hyperparameters(list(
        algorithm = "CART",
        hyperparameters = list(maxdept = 3L)
      )),
      error = conditionMessage
    ),
    "did you mean `maxdepth`",
    fixed = TRUE
  )
  # A rename by extension: `n` is 10 edits from `n_resamples`, so the prefix
  # rule is what catches the historical resampler rename.
  expect_match(
    tryCatch(
      .list_to_ResamplerConfig(list(type = "KFold", n = 3L)),
      error = conditionMessage
    ),
    "did you mean `n_resamples`",
    fixed = TRUE
  )
})

test_that("document metadata is allowed through the check", {
  # `$schema` identifies the document, not a field, so a config read straight
  # from disk must not trip the strictness.
  expect_s7_class(
    .list_to_PreprocessorConfig(list(
      `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      scale = TRUE
    )),
    PreprocessorConfig
  )
})
