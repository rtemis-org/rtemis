# schema_registry.R
# ::rtemis::
# 2026- EDG rtemis.org

# The registry both generators read: which S7 class backs each published
# schema, plus the titles, descriptions and hand-written fragments that are not
# derivable from the class. Sourced by `generate_schemas.R` (which emits the
# schemas) and `generate_defaults.R` (which emits their `setup_*` defaults), so
# the two cannot disagree about what exists.
#
# Requires the package to be loaded first: the entries reference class objects.

# Schema `$id` for a family's top-level schema, used by `refs` entries below.
.url <- function(family) paste0(base_url, "/", family, "/v1/schema.json")


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
        desc = "t-Distributed Stochastic Neighbor Embedding. See setup_tSNE."
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
        desc = "Fuzzy c-means clustering. See setup_CMeans."
      ),
      list(
        cls = DBSCANConfig,
        desc = "DBSCAN density-based clustering. See setup_DBSCAN."
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
      n_resamples = list(
        type = I(c("integer", "null")),
        minimum = 1L,
        description = "Number of resamples. null for LOOCV, where it is determined by the data."
      )
    ),
    # `n_resamples` is required for every type except LOOCV (mirrors the R
    # `ResamplerConfig` validator), so it is conditionally required per variant.
    required_except = list(n_resamples = "LOOCV"),
    algorithms = list(
      list(
        cls = KFoldConfig,
        desc = "K-fold cross-validation. See setup_Resampler."
      ),
      list(
        cls = StratSubConfig,
        desc = "Stratified subsampling. See setup_Resampler."
      ),
      list(
        cls = StratBootConfig,
        desc = "Stratified bootstrap. See setup_Resampler."
      ),
      list(
        cls = BootstrapConfig,
        desc = "Bootstrap resampling. See setup_Resampler."
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
        desc = "Elastic net (glmnet). See `setup_GLMNET`."
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
        desc = "LightGBM gradient boosting. See `setup_LightGBM`."
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
        desc = "TabNet neural network. See `setup_TabNet`."
      ),
      list(
        cls = RangerHyperparameters,
        desc = "Ranger random forest. See `setup_Ranger`."
      )
    )
  )
)


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
    )
  )
)
