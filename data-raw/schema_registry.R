# schema_registry.R
# ::rtemis::
# 2026- EDG rtemis.org

# The registry both generators read: which S7 class backs each published
# schema, plus the titles, descriptions and cross-field constraints that are not
# derivable from the class. Sourced by `generate_schemas.R` (which emits the
# schemas) and `generate_defaults.R` (which emits their `setup_*` defaults), so
# the two cannot disagree about what exists.
#
# No entry here restates a property: every one is generated from its
# `PropertySpec`. `extra` carries only class-level `allOf` rules, which are
# constraints between properties rather than declarations of them.
#
# Requires the package to be loaded first: the entries reference class objects.

# Schema `$id` for a family's top-level schema, used by `refs` entries below.
.url <- function(family) paste0(base_url, "/", family, "/v1/schema.json")

# Every meta learner holds a library of other learners and one learner to combine
# them, so its leaf references the hyperparameters family it is itself part of.
.meta_learner_refs <- c(
  meta_learner = .url("hyperparameters"),
  inner_resampling_config = .url("resampler")
)
.meta_learner_array_refs <- c(base_learners = .url("hyperparameters"))


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
        cls = GLMNETHyperparameters,
        desc = "Elastic net (glmnet). See `setup_GLMNET`."
      ),
      list(
        cls = SPLSHyperparameters,
        desc = "Sparse Partial Least Squares. See `setup_SPLS`."
      ),
      list(
        cls = MARSHyperparameters,
        desc = "Multivariate Adaptive Regression Splines (earth). See `setup_MARS`."
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
        cls = CARTHyperparameters,
        desc = "CART decision tree (rpart). See `setup_CART`."
      ),
      list(
        cls = RangerHyperparameters,
        desc = "Ranger random forest. See `setup_Ranger`."
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
        cls = BARTHyperparameters,
        desc = "Bayesian Additive Regression Trees (stochtree). See `setup_BART`."
      ),
      list(
        cls = IsotonicHyperparameters,
        desc = "Isotonic regression. See `setup_Isotonic`."
      ),
      list(
        cls = MLPHyperparameters,
        desc = "Multilayer perceptron (torch). See `setup_MLP`."
      ),
      list(
        cls = TabNetHyperparameters,
        desc = "TabNet neural network. See `setup_TabNet`."
      ),
      list(
        cls = KNNHyperparameters,
        desc = "k-Nearest Neighbors (kknn). See `setup_KNN`."
      ),
      list(
        cls = HALHyperparameters,
        desc = "Highly Adaptive Lasso (hal9001). See `setup_HAL`."
      ),
      list(
        cls = MonotonicHALHyperparameters,
        desc = "Monotonic Highly Adaptive Lasso (hal9001). See `setup_MonotonicHAL`."
      ),
      list(
        cls = NNLSHyperparameters,
        desc = "Non-negative least squares (nnls). See `setup_NNLS`."
      ),
      # The meta learners hold other hyperparameters, so their leaves reference
      # this same family -- a recursive schema, which is valid but which a form
      # builder must bound. See plan/superlearner.md gap 5.
      list(
        cls = SuperLearnerHyperparameters,
        desc = "SuperLearner: cross-validated stacked ensemble. See `setup_SuperLearner`.",
        refs = .meta_learner_refs,
        array_refs = .meta_learner_array_refs
      ),
      list(
        cls = ModalityStackingHyperparameters,
        desc = "ModalityStacking: stacked ensemble with one learner per feature group. See `setup_ModalityStacking`.",
        refs = .meta_learner_refs,
        array_refs = .meta_learner_array_refs
      ),
      list(
        cls = ConditionalSuperLearnerHyperparameters,
        desc = "Conditional SuperLearner: an oracle routes each case to one of a library of experts. See `setup_ConditionalSuperLearner`.",
        refs = .meta_learner_refs,
        array_refs = .meta_learner_array_refs
      )
    )
  )
)


flat_configs <- list(
  # Not a config: the provenance block of a record. It is generated here so it
  # is published like everything else, and `$ref`d by each record rather than
  # restated in all of them.
  provenance = list(
    cls = Provenance,
    title = "rtemis Provenance",
    description = paste0(
      "What produced a run record: package and language versions, platform, ",
      "timing, how the run ended, and a fingerprint of each dataset used. ",
      "Referenced by every `<family>/v1/record.json`."
    ),
    refs = c(
      data_training = .url("datafingerprint"),
      data_validation = .url("datafingerprint"),
      data_test = .url("datafingerprint")
    )
  ),
  datafingerprint = list(
    cls = DataFingerprint,
    title = "rtemis DataFingerprint",
    description = paste0(
      "Identity of one dataset: a content hash plus the structural facts that ",
      "make a mismatch diagnosable rather than merely detectable."
    )
  ),
  execution = list(
    cls = ExecutionConfig,
    title = "rtemis ExecutionConfig",
    description = paste0(
      "Language-independent config for rtemis execution: sequential, ",
      "parallel, or distributed. Mirrors the `ExecutionConfig` object / ",
      "`setup_ExecutionConfig` arguments."
    ),
    # Cross-field rules `setup_ExecutionConfig()` *rejects*, mirrored here. A
    # rule the class validator enforces but `setup_*` resolves does not belong:
    # the validator only ever sees post-`setup_*` values, while the schema sees
    # the document as authored. `@future_plan` is the case in point -- the class
    # requires it when `backend` is "future", but `setup_ExecutionConfig()`
    # fills a NULL one in from `getOption("future.plan", "mirai_multisession")`,
    # so requiring it here would reject configs that read and run fine (and
    # leave the CLI's form with an unsatisfiable field, since the default is
    # resolved at read time and so absent from the defaults artifact).
    extra = list(
      allOf = list(
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
  # Results classes. Not configs: they describe what a run produced, so every
  # property is `readOnly` and none has an input form.
  regressionmetrics = list(
    cls = RegressionMetrics,
    title = "rtemis RegressionMetrics",
    description = paste0(
      "Regression metrics for one sample: a single-row table of mean absolute ",
      "error, mean squared error, root mean squared error and R-squared. Every ",
      "cell is nullable, a metric being genuinely undefined for some samples."
    )
  ),
  classificationmetrics = list(
    cls = ClassificationMetrics,
    title = "rtemis ClassificationMetrics",
    description = paste0(
      "Classification metrics for one sample: the confusion matrix in long ",
      "form (one row per cell), overall metrics, and per-class metrics. Which ",
      "overall columns are present depends on the task, so only the invariant ",
      "ones are required."
    )
  ),
  decompositionmetrics = list(
    cls = DecompositionMetrics,
    title = "rtemis DecompositionMetrics",
    description = paste0(
      "Decomposition metrics: a single-row table of reconstruction quality, ",
      "component redundancy and effective dimensionality. Which cells are ",
      "populated follows from the algorithm's traits -- a metric its algorithm ",
      "cannot support is null -- so every column is declared for every ",
      "algorithm and every cell is nullable. Unprefixed columns describe the ",
      "data the decomposition was fitted on; `oos_` columns describe data the ",
      "fit never saw."
    )
  ),
  regressionmetricsres = list(
    cls = RegressionMetricsRes,
    title = "rtemis RegressionMetricsRes",
    description = paste0(
      "Regression metrics aggregated across resamples: each resample's ",
      "metrics, plus their mean and standard deviation."
    ),
    array_refs = c(res_metrics = .url("regressionmetrics"))
  ),
  classificationmetricsres = list(
    cls = ClassificationMetricsRes,
    title = "rtemis ClassificationMetricsRes",
    description = paste0(
      "Classification metrics aggregated across resamples: each resample's ",
      "metrics, the aggregate confusion matrix in long form, and the mean and ",
      "standard deviation of the overall metrics."
    ),
    array_refs = c(res_metrics = .url("classificationmetrics"))
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
