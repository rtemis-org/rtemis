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
  # tSNE `Y_init` (a matrix) is `prop_external()`: part of the contract, but its
  # fragment must come from `extra`.
  expect_error(
    S7_to_JSONSchema(
      tSNEConfig,
      id = "https://example.org/x.json",
      base = DecompositionConfig
    ),
    "Y_init"
  )
  s <- S7_to_JSONSchema(
    tSNEConfig,
    id = "https://example.org/x.json",
    base = DecompositionConfig,
    extra = .tsne_schema_extra
  )
  expect_true("Y_init" %in% names(s[["properties"]]))
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

test_that("CMeans non-JSON params (weights, control) survive but stay out of schema", {
  cfg <- setup_CMeans(k = 3L)
  # control (list) is preserved as a config key.
  expect_true("control" %in% names(cfg@config))
  # weights + control are `prop_external()`: generation aborts unless `extra`
  # supplies their fragments.
  expect_error(
    S7_to_JSONSchema(
      CMeansConfig,
      id = "https://example.org/x.json",
      base = ClusteringConfig
    ),
    "weights"
  )
  s <- S7_to_JSONSchema(
    CMeansConfig,
    id = "https://schema.rtemis.org/clustering/cmeans/v1/schema.json",
    base = ClusteringConfig,
    extra = .cmeans_schema_extra
  )
  expect_true(all(c("weights", "control") %in% names(s[["properties"]])))
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
    payload = NULL,
    extra_properties = list(n = list(type = I(c("integer", "null"))))
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
    base = ResamplerConfig,
    extra = .resampler_id_strat_schema_extra
  )
  expect_false(do.call(S7_to_JSONSchema, args)[["additionalProperties"]])
  expect_false(
    "additionalProperties" %in%
      names(do.call(S7_to_JSONSchema, c(args, list(closed = FALSE))))
  )
})

test_that("serialized configs carry only declared parameters", {
  # Constants, run state, and non-JSON values are reconstructed on read, so
  # they are not written (and are correspondingly absent from the schemas).
  hp <- serializable_props(setup_LightRF(nrounds = 100L))
  expect_identical(names(hp), c("algorithm", "hyperparameters"))
  expect_false(any(
    names(LightRF_constants) %in% names(hp[["hyperparameters"]])
  ))
  # tSNE's Y_init (a matrix) has no JSON form.
  expect_false(
    "Y_init" %in% names(serializable_props(setup_tSNE())[["config"]])
  )
  # A resampler serializes type + n + its declared settings, but not id_strat.
  rs <- serializable_props(setup_Resampler(n_resamples = 5L, type = "KFold"))
  expect_true(all(c("type", "n") %in% names(rs)))
  expect_false("id_strat" %in% names(rs))
  # LOOCV leaves `n` unset.
  expect_null(serializable_props(setup_Resampler(type = "LOOCV"))[["n"]])
  # A tuner keeps its nested resampler config (it serializes as its own schema).
  expect_true(
    "resampler_config" %in%
      names(serializable_props(setup_GridSearch())[["config"]])
  )
})

test_that("S7_dispatcher_JSONSchema merges extra top-level properties", {
  s <- S7_dispatcher_JSONSchema(
    classes = list(KMeansConfig),
    id = "https://schema.rtemis.org/x/v1/schema.json",
    payload = "config",
    extra_properties = list(features = list(type = "array"))
  )
  expect_true("features" %in% names(s[["properties"]]))
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
