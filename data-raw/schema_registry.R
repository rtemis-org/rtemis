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
# Every family publishes one shape: the discriminator, any field the base
# declares for every variant, and the variant's own settings, all as siblings
# (`S7_dispatcher_JSONSchema()`). A document holding only the discriminator is
# that variant with every default.
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
    title = "rtemis DecompositionConfig",
    description = paste0(
      "Language-independent config for an rtemis decomposition (dimensionality ",
      "reduction). Mirrors the `DecompositionConfig` object: an algorithm name, ",
      "its settings, and an optional feature subset. The same config drives ",
      "rtemis (R), rtemis CLI/shell, and rtemislive to identical output."
    ),
    algorithm_description = "Decomposition algorithm name.",
    algorithms = list(
      list(
        cls = PCAConfig,
        desc = "Principal Component Analysis."
      ),
      list(
        cls = ICAConfig,
        desc = "Independent Component Analysis."
      ),
      list(
        cls = NMFConfig,
        desc = "Non-negative Matrix Factorization."
      ),
      list(
        cls = UMAPConfig,
        desc = "Uniform Manifold Approximation and Projection."
      ),
      list(
        cls = tSNEConfig,
        desc = "t-Distributed Stochastic Neighbor Embedding."
      ),
      list(cls = IsomapConfig, desc = "Isomap.")
    )
  ),
  clustering = list(
    base_class = ClusteringConfig,
    title = "rtemis ClusteringConfig",
    description = paste0(
      "Language-independent config for an rtemis clustering run. Mirrors the ",
      "`ClusteringConfig` object: an algorithm name and its settings. The same ",
      "config drives rtemis (R), rtemis CLI/shell, and rtemislive to identical ",
      "output."
    ),
    algorithm_description = "Clustering algorithm name.",
    algorithms = list(
      list(cls = KMeansConfig, desc = "K-means clustering."),
      list(
        cls = HardCLConfig,
        desc = "Hard competitive learning."
      ),
      list(
        cls = NeuralGasConfig,
        desc = "Neural Gas clustering."
      ),
      list(
        cls = CMeansConfig,
        desc = "Fuzzy c-means clustering."
      ),
      list(
        cls = DBSCANConfig,
        desc = "DBSCAN density-based clustering."
      )
    )
  ),
  # The first node of a pipeline: how a file becomes the typed Parquet
  # everything downstream reads. Published because the decisions it holds --
  # what counts as missing, whether labels are factors, which reader parsed the
  # file, and any types the user declares outright -- change what the run trains
  # on, and a record that could not report them could not account for its own
  # numbers.
  #
  # A family rather than one config: a delimited file has a separator, a
  # spreadsheet has a sheet number, a Parquet has neither. The base carries
  # settings of its own -- the declared types and the three post-read
  # operations apply whatever the file is -- which the dispatcher publishes
  # once, beside `format`.
  ingest = list(
    base_class = IngestConfig,
    discriminator = "format",
    title = "rtemis IngestConfig",
    description = paste0(
      "Language-independent config for reading a data file and normalizing it ",
      "to Parquet. A delimited file or a spreadsheet carries no usable type ",
      "information and Parquet does, so this is the one step that decides ",
      "types -- everything after it reads a declaration rather than inferring ",
      "one. `format` is what the file is, not a preference, so it has no ",
      "default and a config that disagrees with its file is an error."
    ),
    discriminator_description = "What the file is.",
    algorithms = list(
      list(
        cls = DelimitedIngestConfig,
        desc = "A delimited file (csv, tsv, ...)."
      ),
      list(
        cls = ParquetIngestConfig,
        desc = "A Parquet file, which declares its own types."
      ),
      list(
        cls = XLSXIngestConfig,
        desc = "A spreadsheet."
      ),
      list(
        cls = RDSIngestConfig,
        desc = "An RDS file."
      ),
      list(
        cls = DTAIngestConfig,
        desc = "A Stata file."
      ),
      list(
        cls = ARFFIngestConfig,
        desc = "An ARFF file."
      )
    )
  ),
  # `PartitionConfig`'s base carries nothing but the discriminator, so a
  # document is `method` plus the method's own fields.
  partition = list(
    base_class = PartitionConfig,
    discriminator = "method",
    title = "rtemis PartitionConfig",
    description = paste0(
      "Language-independent config for splitting a dataset into a training ",
      "set and a held-out test set. A first-class, auditable operation -- ",
      "the same reason `ingest` is one -- rather than a field on `SuperConfig`: ",
      "how a held-out set was produced is a decision the record must be able ",
      "to report."
    ),
    discriminator_description = "How the dataset is split.",
    algorithms = list(
      list(
        cls = RandomPartitionConfig,
        desc = "A uniformly random split."
      ),
      list(
        cls = TimePartitionConfig,
        desc = "A split by time order."
      ),
      list(
        cls = GroupPartitionConfig,
        desc = "A split that keeps each group of cases on one side."
      ),
      list(
        cls = PredefinedPartitionConfig,
        desc = "A split already recorded in the data itself."
      )
    )
  ),
  resampler = list(
    base_class = ResamplerConfig,
    discriminator = "type",
    title = "rtemis ResamplerConfig",
    description = paste0(
      "Language-independent config for an rtemis resampler. Mirrors the ",
      "`ResamplerConfig` object: a resampler type plus its type-specific ",
      "settings. The same config drives rtemis (R), rtemis CLI/shell, and ",
      "rtemislive to identical resamples."
    ),
    discriminator_description = "Resampler type.",
    algorithms = list(
      list(
        cls = KFoldConfig,
        desc = "K-fold cross-validation."
      ),
      list(
        cls = StratSubConfig,
        desc = "Stratified subsampling."
      ),
      list(
        cls = StratBootConfig,
        desc = "Stratified bootstrap."
      ),
      list(
        cls = BootstrapConfig,
        desc = "Bootstrap resampling."
      ),
      list(
        cls = LOOCVConfig,
        desc = "Leave-one-out cross-validation."
      ),
      list(
        cls = CustomConfig,
        desc = "Custom, user-supplied resamples."
      )
    )
  ),
  tuner = list(
    base_class = TunerConfig,
    discriminator = "type",
    title = "rtemis TunerConfig",
    description = paste0(
      "Language-independent config for rtemis hyperparameter tuning. Mirrors ",
      "the `TunerConfig` object: a tuner type and its settings."
    ),
    discriminator_description = "Tuner type.",
    algorithms = list(
      list(
        cls = GridSearchConfig,
        desc = "Grid search over hyperparameter combinations.",
        refs = c(
          resampler_config = "https://schema.rtemis.org/resampler/v1/schema.json"
        )
      )
    )
  ),
  explanation = list(
    base_class = ExplanationConfig,
    discriminator = "type",
    title = "rtemis ExplanationConfig",
    description = paste0(
      "Language-independent config for a per-case rtemis explanation. Mirrors ",
      "the `ExplanationConfig` object: a kind of explanation plus its ",
      "kind-specific settings. The kind is tagged rather than inferred, so a ",
      "second kind of explanation can be added without changing how the first ",
      "is read."
    ),
    discriminator_description = "Kind of explanation.",
    algorithms = list(
      list(
        cls = SHAPConfig,
        desc = "Shapley additive contributions."
      )
    )
  ),
  conformal = list(
    base_class = ConformalConfig,
    discriminator = "type",
    title = "rtemis ConformalConfig",
    description = paste0(
      "Language-independent config for an rtemis conformal prediction region. ",
      "Mirrors the `ConformalConfig` object: a construction plus its ",
      "construction-specific settings. The construction is tagged rather than ",
      "inferred, since which of them a run may use depends on the model it is ",
      "applied to, not on the document."
    ),
    discriminator_description = "Conformal construction.",
    algorithms = list(
      list(
        cls = SplitConformalConfig,
        desc = "Split conformal prediction."
      ),
      list(
        cls = CVPlusConfig,
        desc = "CV+, jackknife+ and cross-conformal."
      ),
      list(
        cls = CQRConfig,
        desc = "Conformalized quantile regression."
      )
    )
  ),
  hyperparameters = list(
    base_class = Hyperparameters,
    title = "rtemis Hyperparameters",
    description = paste0(
      "Language-independent algorithm hyperparameters: an algorithm name and ",
      "its hyperparameters, validated per-algorithm against ",
      "schema.rtemis.org/hyperparameters/<algorithm>/v1."
    ),
    algorithm_description = "Supervised-learning algorithm name.",
    algorithms = list(
      list(
        cls = GLMHyperparameters,
        desc = "GLM (generalized linear model)."
      ),
      list(
        cls = GAMHyperparameters,
        desc = "GAM (generalized additive model)."
      ),
      list(
        cls = GLMNETHyperparameters,
        desc = "Elastic net (glmnet)."
      ),
      list(
        cls = GLMTreeHyperparameters,
        desc = "Model-Based Recursive Partitioning: a tree with a GLM in each leaf."
      ),
      list(
        cls = SPLSHyperparameters,
        desc = "Sparse Partial Least Squares."
      ),
      list(
        cls = MARSHyperparameters,
        desc = "Multivariate Adaptive Regression Splines (earth)."
      ),
      list(
        cls = LinearSVMHyperparameters,
        desc = "SVM with linear kernel (e1071)."
      ),
      list(
        cls = RadialSVMHyperparameters,
        desc = "SVM with radial kernel (e1071)."
      ),
      list(
        cls = CARTHyperparameters,
        desc = "CART decision tree (rpart)."
      ),
      list(
        cls = RangerHyperparameters,
        desc = "Ranger random forest."
      ),
      list(
        cls = LightCARTHyperparameters,
        desc = "Single LightGBM tree (CART mode)."
      ),
      list(
        cls = LightRFHyperparameters,
        desc = "LightGBM random forest."
      ),
      list(
        cls = LightGBMHyperparameters,
        desc = "LightGBM gradient boosting."
      ),
      list(
        cls = LightRuleFitHyperparameters,
        desc = "LightRuleFit (LightGBM rules + GLMNET)."
      ),
      list(
        cls = BARTHyperparameters,
        desc = "Bayesian Additive Regression Trees (stochtree)."
      ),
      list(
        cls = IsotonicHyperparameters,
        desc = "Isotonic regression."
      ),
      list(
        cls = MLPHyperparameters,
        desc = "Multilayer perceptron (torch)."
      ),
      list(
        cls = TabNetHyperparameters,
        desc = "TabNet neural network."
      ),
      list(
        cls = KNNHyperparameters,
        desc = "k-Nearest Neighbors (kknn)."
      ),
      list(
        cls = HALHyperparameters,
        desc = "Highly Adaptive Lasso (hal9001)."
      ),
      list(
        cls = MonotonicHALHyperparameters,
        desc = "Monotonic Highly Adaptive Lasso (hal9001)."
      ),
      list(
        cls = LINADHyperparameters,
        desc = "Linear Additive Tree."
      ),
      list(
        cls = LINADForestHyperparameters,
        desc = "Bagged ensemble of Linear Additive Trees."
      ),
      list(
        cls = NNLSHyperparameters,
        desc = "Non-negative least squares (nnls)."
      ),
      # The meta learners hold other hyperparameters, so their leaves reference
      # this same family -- a recursive schema, which is valid but which a form
      # builder must bound. See plan/superlearner.md gap 5.
      list(
        cls = SuperLearnerHyperparameters,
        desc = "SuperLearner: cross-validated stacked ensemble.",
        refs = .meta_learner_refs,
        array_refs = .meta_learner_array_refs
      ),
      list(
        cls = ModalityStackingHyperparameters,
        desc = "ModalityStacking: stacked ensemble with one learner per feature group.",
        refs = .meta_learner_refs,
        array_refs = .meta_learner_array_refs
      ),
      list(
        cls = ConditionalSuperLearnerHyperparameters,
        desc = "Conditional SuperLearner: an oracle routes each case to one of a library of experts.",
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
  # Not a config either: a record's reference to a file written beside it.
  # Most of what a run produces this way is tabular -- the execution graph
  # today, the per-fold predictions and grid results later -- and belongs in a
  # columnar file any data tool reads. Some of it is not: the fitted object and
  # the log are already written beside the record. The reference is to a file.
  dataref = list(
    cls = DataRef,
    title = "rtemis DataRef",
    description = paste0(
      "A reference from a record to a file written beside it: where it is, ",
      "how it is encoded, how big it is, and the digest that ties it to the ",
      "record naming it."
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
      "parallel, or distributed."
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
    cls = SuperConfigPaths,
    title = "rtemis SuperConfig",
    description = paste0(
      "Language-independent config for an rtemis supervised-learning run. ",
      "Mirrors the `SuperConfig` object: data references, optional ",
      "preprocessing / decomposition, an algorithm with hyperparameters, ",
      "optional tuning and outer resampling, and execution settings. The ",
      "same config drives rtemis (R), rtemis CLI/shell, and rtemislive."
    ),
    refs = c(
      preprocessor_config = .url("supervisedpreprocessor"),
      decomposition_config = .url("decomposition"),
      tuner_config = .url("tuner"),
      outer_resampling_config = .url("resampler"),
      execution_config = .url("execution")
    ),
    # A run may name one configuration or a set of them to search over, so this
    # property admits either. See `variant_refs` in `S7_to_JSONSchema()`.
    variant_refs = c(hyperparameters = .url("hyperparameters"))
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
  # A description, not a config and not a record: measured facts about one
  # dataset, published so that any implementation can validate a config against
  # data without the data -- and without R.
  profile = list(
    cls = DataProfile,
    title = "rtemis DataProfile",
    description = paste0(
      "What one dataset is, in the facts a validator needs: dimensions, ",
      "columns with their types, distinct and missing counts, level counts ",
      "for low-cardinality categorical columns, and complete-case and ",
      "duplicate counts. Bounded by the number of columns rather than the ",
      "number of rows, so it travels where the data cannot."
    ),
    refs = c(fingerprint = .url("datafingerprint"))
  ),
  # Findings, not configs: what `validate_config()` reports about a config, so
  # every property is `readOnly` and none has an input form. `diagnostic` is a
  # family of its own so that `diagnostics` can `$ref` it once instead of
  # restating the finding's shape inside an array.
  diagnostic = list(
    cls = Diagnostic,
    title = "rtemis Diagnostic",
    description = paste0(
      "One finding from validating an rtemis config: a stable code, how much ",
      "it matters, the technical and plain-language accounts of it, the ",
      "numbers behind it, and -- where a deterministic one exists -- an RFC ",
      "6902 JSON Patch that fixes it."
    )
  ),
  diagnostics = list(
    cls = Diagnostics,
    title = "rtemis Diagnostics",
    description = paste0(
      "The findings for one rtemis config, in the order they were made. An ",
      "empty array means the config is clean: there is no separate validity ",
      "flag, an empty list of problems being the same statement."
    ),
    array_refs = c(diagnostics = .url("diagnostic"))
  ),
  preprocessor = list(
    cls = PreprocessorConfig,
    title = "rtemis PreprocessorConfig",
    description = paste0(
      "Language-independent config for rtemis preprocessing. The same ",
      "config drives rtemis (R), rtemis CLI/shell, and rtemislive to identical output."
    )
  ),
  supervisedpreprocessor = list(
    cls = SupervisedPreprocessorConfig,
    title = "rtemis SupervisedPreprocessorConfig",
    description = paste0(
      "Language-independent config for the preprocessing a supervised run can ",
      "fit: the preprocessing config without the ",
      "operations a fitted preprocessor cannot replay at predict time ",
      "(`complete_cases`, `remove_duplicates`, `remove_cases_thres`) or would ",
      "learn differently in every resample (`remove_features_thres`). Those ",
      "belong to `preprocessor`, applied to a dataset before training."
    )
  )
)
