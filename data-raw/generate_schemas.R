# generate_schemas.R
# ::rtemis::
# 2026- EDG rtemis.org

# Single source of truth for the schema.rtemis.org algorithm-family schemas.
# Generates, per family, one leaf schema per algorithm (S7_to_JSONSchema) plus
# the `<family>/v1` dispatcher (S7_dispatcher_JSONSchema), and writes them to
# the schema repo in the uniform `<family>/v1` + `<family>/<algorithm>/v1`
# layout. Run with: Rscript data-raw/generate_schemas.R [SCHEMA_REPO]

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"

# Registry ------------------------------------------------------------------
# Per family: the base class, the payload field name, the dispatcher's title /
# descriptions / extra top-level properties, and the per-algorithm classes with
# a one-line description, any properties to `exclude` from generation (runtime /
# tuner-written state that is not a recipe input, or props whose R type the
# prop_* factories cannot express), and an optional `extra` supplying
# hand-written schema for the latter so they are still described.
families <- list(
  decomposition = list(
    base_class = DecompositionConfig,
    payload = "config",
    title = "rtemis DecompositionConfig",
    description = paste0(
      "Language-independent config for an rtemis decomposition (dimensionality ",
      "reduction). Mirrors the `DecompositionConfig` object: an algorithm name, ",
      "an algorithm-specific `config`, and an optional feature subset. The same ",
      "config drives rtemis (R), rtemis-py, and rtemislive to identical output."
    ),
    algorithm_description = "Decomposition algorithm name.",
    extra_properties = list(
      features = list(
        type = I(c("array", "null")),
        items = list(type = "string"),
        minItems = 2L,
        uniqueItems = TRUE,
        description = "Names of the feature columns to decompose. null = all numeric features."
      )
    ),
    algorithms = list(
      list(
        cls = PCAConfig,
        desc = "Principal Component Analysis. See setup_PCA."
      ),
      list(
        cls = ICAConfig,
        desc = "Independent Component Analysis. See setup_ICA."
      ),
      list(
        cls = NMFConfig,
        desc = "Non-negative Matrix Factorization. See setup_NMF."
      ),
      list(
        cls = UMAPConfig,
        desc = "Uniform Manifold Approximation and Projection. See setup_UMAP."
      ),
      list(
        cls = tSNEConfig,
        desc = "t-Distributed Stochastic Neighbor Embedding. See setup_tSNE.",
        exclude = "Y_init",
        extra = .tsne_schema_extra
      ),
      list(cls = IsomapConfig, desc = "Isomap. See setup_Isomap.")
    )
  ),
  clustering = list(
    base_class = ClusteringConfig,
    payload = "config",
    title = "rtemis ClusteringConfig",
    description = paste0(
      "Language-independent config for an rtemis clustering run. Mirrors the ",
      "`ClusteringConfig` object: an algorithm name and an algorithm-specific ",
      "`config`. The same config drives rtemis (R), rtemis-py, and rtemislive ",
      "to identical output."
    ),
    algorithm_description = "Clustering algorithm name.",
    algorithms = list(
      list(cls = KMeansConfig, desc = "K-means clustering. See setup_KMeans."),
      list(
        cls = HardCLConfig,
        desc = "Hard competitive learning. See setup_HardCL."
      ),
      list(
        cls = NeuralGasConfig,
        desc = "Neural Gas clustering. See setup_NeuralGas."
      ),
      list(
        cls = CMeansConfig,
        desc = "Fuzzy c-means clustering. See setup_CMeans.",
        exclude = c("weights", "control"),
        extra = .cmeans_schema_extra
      ),
      list(
        cls = DBSCANConfig,
        desc = "DBSCAN density-based clustering. See setup_DBSCAN.",
        exclude = "weights",
        extra = .dbscan_schema_extra
      )
    )
  ),
  # Top-level mode: a ResamplerConfig serializes its per-type fields as
  # siblings of `type` (not nested), so the leaves are open and the
  # dispatcher enforces strictness with `unevaluatedProperties`.
  resampler = list(
    base_class = ResamplerConfig,
    discriminator = "type",
    payload = NULL,
    title = "rtemis ResamplerConfig",
    description = paste0(
      "Language-independent config for an rtemis resampler. Mirrors the ",
      "`ResamplerConfig` object: a resampler type plus its type-specific ",
      "settings. The same config drives rtemis (R), rtemis-py, and ",
      "rtemislive to identical resamples."
    ),
    discriminator_description = "Resampler type.",
    extra_properties = list(
      n = list(
        type = I(c("integer", "null")),
        minimum = 1L,
        description = "Number of resamples. null for LOOCV, where it is determined by the data."
      )
    ),
    # `n` is required for every type except LOOCV (mirrors the R
    # `ResamplerConfig` validator), so it is conditionally required per variant.
    required_except = list(n = "LOOCV"),
    algorithms = list(
      list(
        cls = KFoldConfig,
        desc = "K-fold cross-validation. See setup_Resampler.",
        exclude = "id_strat",
        extra = .resampler_id_strat_schema_extra
      ),
      list(
        cls = StratSubConfig,
        desc = "Stratified subsampling. See setup_Resampler.",
        exclude = "id_strat",
        extra = .resampler_id_strat_schema_extra
      ),
      list(
        cls = StratBootConfig,
        desc = "Stratified bootstrap. See setup_Resampler.",
        exclude = "id_strat",
        extra = .resampler_id_strat_schema_extra
      ),
      list(
        cls = BootstrapConfig,
        desc = "Bootstrap resampling. See setup_Resampler.",
        exclude = "id_strat",
        extra = .resampler_id_strat_schema_extra
      ),
      list(
        cls = LOOCVConfig,
        desc = "Leave-one-out cross-validation. See setup_Resampler."
      ),
      list(
        cls = CustomConfig,
        desc = "Custom, user-supplied resamples. See setup_Resampler."
      )
    )
  ),
  tuner = list(
    base_class = TunerConfig,
    discriminator = "type",
    payload = "config",
    title = "rtemis TunerConfig",
    description = paste0(
      "Language-independent config for rtemis hyperparameter tuning. Mirrors ",
      "the `TunerConfig` object: a tuner type and a type-specific `config`."
    ),
    discriminator_description = "Tuner type.",
    algorithms = list(
      list(
        cls = GridSearchConfig,
        desc = "Grid search over hyperparameter combinations. See setup_GridSearch.",
        refs = c(
          resampler_config = "https://schema.rtemis.org/resampler/v1/schema.json"
        )
      )
    )
  ),
  hyperparameters = list(
    base_class = Hyperparameters,
    payload = "hyperparameters",
    title = "rtemis Hyperparameters",
    description = paste0(
      "Language-independent algorithm hyperparameters: an algorithm name and an ",
      "algorithm-specific `hyperparameters` object, validated per-algorithm ",
      "against schema.rtemis.org/hyperparameters/<algorithm>/v1. Mirrors the ",
      "`{algorithm, hyperparameters}` wire format consumed by ",
      "`.list_to_Hyperparameters`."
    ),
    algorithm_description = "Supervised-learning algorithm name (matches `setup_<algorithm>`).",
    algorithms = list(
      list(
        cls = GLMHyperparameters,
        desc = "GLM (generalized linear model). See `setup_GLM`."
      ),
      list(
        cls = GAMHyperparameters,
        desc = "GAM (generalized additive model). See `setup_GAM`."
      ),
      list(
        cls = CARTHyperparameters,
        desc = "CART decision tree (rpart). See `setup_CART`."
      ),
      list(
        cls = GLMNETHyperparameters,
        desc = "Elastic net (glmnet). See `setup_GLMNET`.",
        exclude = c("lambda.min", "lambda.1se")
      ),
      list(
        cls = LightCARTHyperparameters,
        desc = "Single LightGBM tree (CART mode). See `setup_LightCART`."
      ),
      list(
        cls = LightRFHyperparameters,
        desc = "LightGBM random forest. See `setup_LightRF`."
      ),
      list(
        cls = LightGBMHyperparameters,
        desc = "LightGBM gradient boosting. See `setup_LightGBM`.",
        exclude = c("nrounds", "best_iter")
      ),
      list(
        cls = LightRuleFitHyperparameters,
        desc = "LightRuleFit (LightGBM rules + GLMNET). See `setup_LightRuleFit`."
      ),
      list(
        cls = IsotonicHyperparameters,
        desc = "Isotonic regression. See `setup_Isotonic`."
      ),
      list(
        cls = LinearSVMHyperparameters,
        desc = "SVM with linear kernel (e1071). See `setup_LinearSVM`."
      ),
      list(
        cls = RadialSVMHyperparameters,
        desc = "SVM with radial kernel (e1071). See `setup_RadialSVM`."
      ),
      list(
        cls = TabNetHyperparameters,
        desc = "TabNet neural network. See `setup_TabNet`.",
        # `optimizer` / `lr_scheduler` accept an R function or a string; only
        # the serializable string form is schematized (see the extra).
        exclude = c("optimizer", "lr_scheduler"),
        extra = .tabnet_hyperparameters_schema_extra
      ),
      list(
        cls = RangerHyperparameters,
        desc = "Ranger random forest. See `setup_Ranger`.",
        # These three have union / list R types the prop_* factories cannot
        # express; their JSON Schema is supplied by hand and merged in.
        exclude = c(
          "split_select_weights",
          "respect_unordered_factors",
          "inbag"
        ),
        extra = .ranger_hyperparameters_schema_extra
      )
    )
  )
)

# Generation ----------------------------------------------------------------
for (family in names(families)) {
  fam <- families[[family]]
  base_props <- names(fam[["base_class"]]@properties)
  classes <- lapply(fam[["algorithms"]], `[[`, "cls")
  discriminator <- if (is.null(fam[["discriminator"]])) {
    "algorithm"
  } else {
    fam[["discriminator"]]
  }
  # `payload = NULL` (top-level mode) needs open leaves so the dispatcher's
  # `unevaluatedProperties` can account for them.
  top_level <- !("payload" %in% names(fam)) || is.null(fam[["payload"]])
  payload <- if (top_level) NULL else fam[["payload"]]

  # Leaves.
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    slug <- tolower(discriminator_value(cls, discriminator))
    id <- paste0(base_url, "/", family, "/", slug, "/v1/schema.json")
    schema <- S7_to_JSONSchema(
      cls,
      id = id,
      title = paste0("rtemis ", cls@name),
      description = algo[["desc"]],
      exclude = c(base_props, algo[["exclude"]]),
      # `extra` supplies hand-written schema for excluded props whose R type the
      # prop_* factories cannot express (e.g. Ranger union/list params). NULL
      # for algorithms that need none.
      extra = algo[["extra"]],
      refs = algo[["refs"]],
      closed = !top_level
    )
    dir <- file.path(schema_repo, family, slug, "v1")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    write_JSONSchema(
      schema,
      file.path(dir, "schema.json"),
      overwrite = TRUE,
      verbosity = 0L
    )
  }

  # Dispatcher.
  dispatcher_id <- paste0(base_url, "/", family, "/v1/schema.json")
  dispatcher <- S7_dispatcher_JSONSchema(
    classes = classes,
    id = dispatcher_id,
    discriminator = discriminator,
    payload = payload,
    title = fam[["title"]],
    description = fam[["description"]],
    discriminator_description = if (
      is.null(fam[["discriminator_description"]])
    ) {
      fam[["algorithm_description"]]
    } else {
      fam[["discriminator_description"]]
    },
    extra_properties = if (is.null(fam[["extra_properties"]])) {
      list()
    } else {
      fam[["extra_properties"]]
    },
    # Translate `required_except` (property -> variants to skip) into the
    # per-variant `required` form the dispatcher generator consumes.
    variant_required = local({
      # The dispatcher keys `variant_required` by the raw discriminator value.
      values <- vapply(
        classes,
        function(cls) discriminator_value(cls, discriminator),
        character(1L)
      )
      req <- list()
      for (prop_name in names(fam[["required_except"]])) {
        except <- fam[["required_except"]][[prop_name]]
        for (v in setdiff(values, except)) {
          req[[v]] <- c(req[[v]], prop_name)
        }
      }
      req
    }),
    instance_schema_url = dispatcher_id
  )
  write_JSONSchema(
    dispatcher,
    file.path(schema_repo, family, "v1", "schema.json"),
    overwrite = TRUE,
    verbosity = 0L
  )
  cat(sprintf(
    "%-16s %d leaves + dispatcher\n",
    family,
    length(fam[["algorithms"]])
  ))
}

# Flat configs --------------------------------------------------------------
# Single-object configs (no algorithm discriminator): one schema per class.
# `exclude` lists properties whose R type is not expressible via the prop_*
# factories; `extra` supplies their hand-written schema fragments so the
# generated schema still describes them.
.url <- function(family) paste0(base_url, "/", family, "/v1/schema.json")

flat_configs <- list(
  execution = list(
    cls = ExecutionConfig,
    title = "rtemis ExecutionConfig",
    description = paste0(
      "Language-independent config for rtemis execution: sequential, ",
      "parallel, or distributed. Mirrors the `ExecutionConfig` object / ",
      "`setup_ExecutionConfig` arguments."
    ),
    # Cross-field rules enforced by the R class validator, mirrored here.
    extra = list(
      allOf = list(
        list(
          `if` = list(
            properties = list(backend = list(const = "future")),
            required = I("backend")
          ),
          then = list(required = I("future_plan"))
        ),
        list(
          `if` = list(
            properties = list(backend = list(const = "none")),
            required = I("backend")
          ),
          then = list(properties = list(n_workers = list(const = 1L)))
        )
      )
    )
  ),
  supervised = list(
    cls = SuperConfig,
    title = "rtemis SuperConfig",
    description = paste0(
      "Language-independent config for an rtemis supervised-learning run. ",
      "Mirrors the `SuperConfig` object: data references, optional ",
      "preprocessing / decomposition, an algorithm with hyperparameters, ",
      "optional tuning and outer resampling, and execution settings. The ",
      "same config drives rtemis (R), rtemis-py, and rtemislive."
    ),
    refs = c(
      preprocessor_config = .url("preprocessor"),
      decomposition_config = .url("decomposition"),
      hyperparameters = .url("hyperparameters"),
      tuner_config = .url("tuner"),
      outer_resampling_config = .url("resampler"),
      execution_config = .url("execution")
    )
  ),
  decompose = list(
    cls = DecomposeConfig,
    title = "rtemis DecomposeConfig",
    description = paste0(
      "Language-independent config for an rtemis decomposition pipeline: a ",
      "data reference, a `DecompositionConfig`, and an output directory."
    ),
    refs = c(decomposition_config = .url("decomposition"))
  ),
  cluster = list(
    cls = ClusterConfig,
    title = "rtemis ClusterConfig",
    description = paste0(
      "Language-independent config for an rtemis clustering pipeline: a ",
      "data reference, a `ClusteringConfig`, and an output directory."
    ),
    refs = c(clustering_config = .url("clustering"))
  ),
  preprocessor = list(
    cls = PreprocessorConfig,
    title = "rtemis PreprocessorConfig",
    description = paste0(
      "Language-independent config for rtemis preprocessing. Mirrors the ",
      "`PreprocessorConfig` object / `setup_Preprocessor` arguments. The same ",
      "config drives rtemis (R), rtemis-py, and rtemislive to identical output."
    ),
    exclude = c(
      "impute_missRanger_params",
      "scale_centers",
      "scale_coefficients",
      "one_hot_levels"
    ),
    extra = .preprocessor_schema_extra
  )
)

for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  id <- paste0(base_url, "/", family, "/v1/schema.json")
  schema <- S7_to_JSONSchema(
    cfg[["cls"]],
    id = id,
    title = cfg[["title"]],
    description = cfg[["description"]],
    exclude = cfg[["exclude"]],
    extra = cfg[["extra"]],
    refs = cfg[["refs"]],
    instance_schema_url = id
  )
  dir <- file.path(schema_repo, family, "v1")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_JSONSchema(
    schema,
    file.path(dir, "schema.json"),
    overwrite = TRUE,
    verbosity = 0L
  )
  cat(sprintf("%-16s flat config schema\n", family))
}

# `supervised/v1` is now generated from `SuperConfig` (with `$ref`s to the
# family schemas), so the hand-authored hyperparameters `allOf` and its
# drift check are retired: the references cannot drift from the classes.
