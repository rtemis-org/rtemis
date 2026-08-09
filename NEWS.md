# rtemis news

## 1.3.4

**Breaking changes**

- Tuning is asked for explicitly: `setup_LightRF(max_depth = tune_over(3L, 4L, 5L))`. A bare vector is a value everywhere, so `max_depth = 3:5` is an error that names the replacement. Passing several values used to mean "search these", which held only while no hyperparameter was itself vector-valued - `hidden_units = c(48L, 24L)` is one architecture, and no rule about shape can tell that from two candidates. A search space is now its own type, `HyperparameterCandidates`, that the tuner, the tuning grid, the data-bound checks and the run record all read rather than infer. `tune_over()` takes one candidate per argument, or a single vector or list holding them, and needs at least two: a one-value search costs a full resampling pass and can only return the value it was given.
- On the wire a search space is tagged - `"max_depth": {"candidates": [3, 4, 5]}` - and a value is anything else. Every tunable property's published schema changes to match: the second `oneOf` branch is an object rather than an array, carrying `minItems: 2` and `additionalProperties: false`, so the two-candidate minimum is part of the contract rather than only an R check. The point is that `[3, 4, 5]` could not be read without knowing the property's declared type, and `{"candidates": [3, 4, 5]}` can - which also lets a domain that is sampled rather than enumerated arrive later as a sibling key instead of a new shape.
- Vector-valued hyperparameters may now be tuned at all: `@container` and `@tunable` were declared mutually exclusive, which is what would have kept a neural network's layer widths fixed. `@broadcast` and `@tunable` are exclusive instead, a narrower rule covering the one combination that cannot be written unambiguously.

- `preprocess(factor2integer = TRUE)` records the levels it coded against, and `apply_preprocessor()` codes new data against those rather than against whatever levels the new data happens to carry. `as.integer()` on a factor returns a position in *that factor's* levels, so a validation, test or `predict()` frame whose factor has fewer levels, or the same levels in a different order, was coded differently from training and the model scored garbage, silently. `prepare_lgb_data()` is the only caller, so this fixes a real **LightGBM** defect for any model with a factor feature scored on independently constructed `newdata`. Training output is unchanged - capturing the levels at fit changes nothing about the fit, and the entire benefit lands on the replay. A value whose level was not seen in training takes the single index above the known levels, so a consumer sizes the feature at `length(levels) + 1L` categories; `NA` stays `NA`.
- One-hot encoding is pinned the same way, and there the defect changed the *shape* of the design matrix rather than its values. `preprocess()` never recorded `one_hot_levels`, so every replay re-derived the columns from the data in front of it, and new data with a missing or reordered level produced a different number of columns, in a different order. **HAL**, **KNN**, **MARS**, **SPLS** and **SVM** each build a one-hot preprocessor at train time and replay it at `predict()`, and all five were exposed. A level unseen in training has no column to take, so its row is all-zero - the degradation that preserves the encoded width, and what `one_hot.data.table` already did.
- `one_hot()` looks its level map up by feature name. The map was previously read with a full-frame column index while it holds one entry per factor, which is correct only when the factors are the leading columns of the frame and raises `subscript out of bounds` otherwise. A map entry with no matching column is now ignored rather than rejected, which is what a preprocessor learned on data that includes the outcome and applied to the features alone requires.
- `factor2integer` codes are integer under both `factor2integer_startat0` settings; `TRUE`, the default, previously returned double. The codes themselves are unchanged. A category code indexes an embedding table or a LightGBM category, and a double cannot.
- `scale` and `center` skip the columns `factor2integer` coded. Standardizing a category code yields a fraction of an index that no consumer can read back, and `setup_Preprocessor(factor2integer = TRUE, scale = TRUE)` previously did exactly that. This changes what `preprocess()` returns for that combination, and `scale_centers` / `scale_coefficients` now cover only the genuine numeric features.

## 1.3.3

- New algorithm: **SuperLearner**, the cross-validated stacked ensemble of van der Laan, Polley & Hubbard, for regression and binary classification. `setup_SuperLearner()`; each base learner predicts every case from a fit that did not see it, and the meta learner is fitted on those predictions. A base learner holding a search space becomes one library entry per combination, so the ensemble's own cross-validation does the model selection and no inner tuning is needed. `discrete = TRUE` keeps the single lowest-risk entry. The cross-validated predictions and the resampler are kept on the fitted model, for a cross-fitting estimator to reuse.
- New algorithm: **ModalityStacking**, the same machinery with each base learner bound to one group of features, for a wide `x` built by concatenating modalities. `setup_ModalityStacking(feature_groups = )` makes "one wide model or one model per modality" a one-argument comparison.
- New algorithm: **ConditionalSuperLearner**, Valdes, Interian, Gennatas & van der Laan (2022), which selects a model from a library *conditional on the covariates* rather than combining them: an oracle routes each case to one of K experts. `setup_ConditionalSuperLearner()`; `n_iterations` tunable. `get_varimp()` returns the oracle's importance - which covariates decide *which model applies*. Multiclass aborts.
- New algorithm: **NNLS**, non-negative least squares via `nnls`, for regression and binary classification. `setup_NNLS()`; `normalize = TRUE` scales the coefficients to sum to 1, making the fit a convex combination. It is the default stacking meta learner, and a poor general-purpose learner - no intercept, and a sign constraint.
- A meta learner is a `Hyperparameters` subclass like any other algorithm, so it works wherever a supervised learner does: `train()`, outer resampling, `calibrate()`, `SuperConfig`, run records.
- `schema.rtemis.org` publishes all four leaf pairs - `hyperparameters/{nnls,superlearner,modalitystacking,conditionalsuperlearner}/v1` with their `record.json`. A meta learner's leaf references the `hyperparameters` union it is itself part of, so a config nests its library.
- New algorithm: **MARS**, Multivariate Adaptive Regression Splines via `earth`, for regression, binary and multiclass classification. `setup_MARS()`; `degree`, `nprune`, `penalty`, `nk` and the forward-pass controls tunable. Classification fits a binomial GLM on the MARS basis, so `predict()` returns probabilities rather than the raw fit. `get_varimp()` reports earth's three criteria - `importance` (the GCV criterion), `rss` (the same accumulation unpenalized), and `subset_proportion`, the fraction of pruning subsets that retain the feature. `pmethod = "cv"` selects the number of terms by cross-validation inside the fit and requires `nfold`; multiclass allows only `"backward"` or `"none"`. Missing values abort.
- `train(x, weights = )` accepts the numeric vector its documentation describes; it previously aborted.
- A `SuperConfig` or `SuperConfigLive` naming a `weights` column now resolves it: the values become the case weights and the column is dropped from the training, validation and test sets. It was previously passed on as a literal string.
- A run record carries every property of the algorithm that produced it, including those inherited from an intermediate class, and records a list of configs as one block per element.
- Reading a config no longer calls out to the `rtemis` CLI, and `options(rtemis.validate = )` / `options(rtemis.cli = )` are gone. `check_wire_keys()` and the `setup_*` functions enforce the same `PropertySpec`s the published schemas are generated from, so rtemis validates its own contract with no external tool and `train(outdir = )` never depends on what is on the `PATH`.

## 1.3.2

**Breaking changes**

- Isotonic calibration no longer returns probabilities of exactly 0 or 1. A block of uniformly labelled cases was previously fitted at the boundary, which asserts certainty and makes log loss infinite for a single case there whose label disagrees. Fitted values are now held at least `1 / (2 * n)` from each end, `n` being the number of calibration cases - the finest distinction that many cases can support. Only the saturated blocks move, so the map stays non-decreasing and rankings are unchanged. Regression fits are untouched.
- `calibrate()`'s `hyperparameters` argument defaults to `NULL`, which selects the default calibrator (`setup_Isotonic()`), rather than naming it in the signature.

- Hyperparameters that only apply under certain values of another are now declared as such rather than described in prose, and a grid search over them is **conditional**. `reduce_basis` applies only at `smoothness_orders` of 0, so `setup_HAL(smoothness_orders = c(0L, 1L, 2L), reduce_basis = c(0.1, 0.5))` - previously rejected - now searches the four combinations that differ rather than the six of the cross product: `reduce_basis` is left unset above order 0, and the combinations that duplicates makes identical are collapsed, so none is fit and ranked twice. A search no value of which puts the hyperparameter in effect is still an error.
- New `tuning_grid()` returns the combinations a grid search would fit, one per row, so a search can be inspected before `train()` runs it. `tune_GridSearch()` fits exactly those rows, and reports the reduction at `verbosity >= 1`.
- The published schemas carry the dependency in `x-rtemis.applies_when`, so a form can disable a hyperparameter that does not apply to the values already chosen rather than accept one the server would reject.
- New algorithm: **SPLS**, Sparse Partial Least Squares via `spls`, for regression, binary and multiclass classification. `setup_SPLS()`; `k`, `eta` and `kappa` tunable. Takes no case weights, so `ifw = TRUE` aborts.
- New algorithm: **KNN**, weighted k-Nearest Neighbors via `kknn`, for regression, binary and multiclass classification. `setup_KNN()`; `k`, `kernel` and `distance` tunable. Takes no case weights, so `ifw = TRUE` aborts; reports no variable importance, so `get_varimp()` returns `NULL`.
- New algorithm: **BART**, Bayesian Additive Regression Trees via `stochtree`, for regression and binary classification. `setup_BART()`; the mean-forest prior and the variance forest tunable. Being a sampler, it is the first non-linear model with standard errors - `se()` returns the posterior standard deviation of the mean function - and `get_varimp()` reports two measures, `importance` (variable inclusion proportion) and `inclusion_sd`. Multiclass aborts; so does `ifw = TRUE` under `link = "cloglog"`, which `stochtree` cannot weight.
- New algorithm: **MonotonicHAL**, a shape-constrained Highly Adaptive Lasso via `hal9001`, for regression and binary classification. `setup_MonotonicHAL()`; `smoothness_orders` and `reduce_basis` tunable. It is a separate algorithm from `HAL` rather than a set of defaults over it, because the three values that distinguish it are invariants and not choices: the interaction degree is 1, every basis function's coefficient is constrained non-negative so the fit is monotonic non-decreasing, and no basis-size guardrail is needed at degree 1. None is representable as a property, so no combination of arguments produces a non-monotonic fit. `penalized = FALSE` drops the lasso penalty, giving the non-parametric maximum likelihood estimate over the monotonic class. Multiclass aborts.
- Available as a calibrator via `calibrate(mod, hyperparameters = setup_MonotonicHAL())`. It is not the default: at `smoothness_orders = 1` the non-negativity constraint that makes the map monotonic also makes it convex on the logit scale, which costs expected calibration error and Brier score whenever the correction needed is concave, and every configuration is several times slower than isotonic regression. `data-raw/benchmark_calibrators.R` reproduces the comparison across three calibration-set sizes.
- New `available_calibration()`, beside `available_supervised()` and friends, listing the algorithms whose fit is constrained monotonic and which are therefore safe calibration maps. `calibrate()` still accepts any `Hyperparameters` object, since it trains one like any other model, but only these carry the guarantee.
- The calibrator that actually ran is recorded on `CalibratedClassification@calibrator` and `CalibratedClassificationRes@calibrator`, shown by `print()`, and serialized by `to_json()`, so a run is reproducible from its output alone. `to_json()` on a `CalibratedClassificationRes` now also carries the calibrated metrics.
- New algorithm: **HAL**, the Highly Adaptive Lasso via `hal9001`, for regression and binary classification. `setup_HAL()`; `max_degree`, `smoothness_orders` and `reduce_basis` tunable. `lambda` is selected by cross-validation inside the fit rather than by rtemis' tuner; `seed`, `nfolds` and `use_min` control that. `get_varimp()` reports `importance` (summed absolute coefficients per feature) and `max_coefficient`. Multiclass aborts.
- The cost of a HAL fit grows as `C(n_features, max_degree)`, and is quadratic in the number of cases; an over-large basis does not fail - it runs until it exhausts memory. `train()` therefore projects the basis size first, reports it at `verbosity >= 1`, warns past a million, and aborts past `max_basis` (five million, and raised deliberately) naming the levers that reduce it. `max_degree` defaults to 2.
- `plot_varimp(mod, measure = )` selects among the measures above; the first is the default.
- `schema.rtemis.org` publishes all four leaf pairs - `hyperparameters/{spls,knn,bart,hal}/v1` with their `record.json` - joining the `algorithm` enum of the `hyperparameters` union, so their configs validate and describe like any other algorithm's.

## 1.3.1

**Breaking changes**

- Predicted probabilities are always a matrix: one row per case, one column per class, binary carrying a single column labelled with the positive class. `predict()` on a `Classification` returns the same shape. Code taking one score per case should index it: `mod$predicted_prob_training[, 1L]`.
- `algorithm` is gone as both a config property and a function argument - `train()`, `calibrate()`, `setup_SuperConfig()` and `setup_SuperConfigLive()` no longer take it. Name the algorithm through its `setup_*()`: `train(iris, hyperparameters = setup_LightRF())`. `train(iris)` still defaults to Ranger; `decomp(algorithm = )` and `cluster(algorithm = )` are unchanged.
- Standard errors are computed on demand. `Regression@se_training` / `@se_validation` / `@se_test` and the `RegressionRes` equivalents are removed; use `se(mod, newdata)`, which returns `NULL` for an algorithm that has none. `to_json()` no longer reports `has_se`.
- The `.list_to_*()` reconstructors reject a key the target config does not declare, naming it and suggesting the nearest valid property - `n` reports "did you mean `n_resamples`?".
- `DecompositionConfig` and `ClusteringConfig` are reconstructed only from their canonical `{algorithm, config}` shape. Relatedly, `decomp(x, config)` now honors `config@features` instead of ignoring it.
- `n_resamples` is declared per resampler type: `{"type": "KFold"}` is now a valid config, and LOOCV rejects a supplied value, being run state.
- A classification result's `positive_class` is `NULL` rather than `NA` when the outcome is not binary.

**Run records**

- Every `train()`, `decomp()` and `cluster()` call given an `outdir` writes a `<prefix>.record.json` beside the saved model, stating what the run actually did: every field present and resolved, each with an `origin` (`user`, `default`, `derived`, `tuned`, or `unset`), plus a `provenance` block and a `DataFingerprint` of the data. `record(mod)` returns the same document as a list.
- A record's top level is what was asked for; its `folds` array is what ran, one entry per model fitted, each carrying its resolved config and - when tuning ran - the candidate grid, per-resample scores and the winner.
- Records state what the run *scored*: `metrics` and `metrics_sd` give each sample's headline row as a flat metric-to-value map, so `jq '.metrics.test.rsq'` answers "was this any good?" without R.
- `Supervised` and `SupervisedRes` gain `@config`, `Decomposition` `@decompose_config`, and `Clustering` `@cluster_config` - the input each run was given. `read_config()` rejects a record fed where a config is expected.
- A fitted model now reports the values its algorithm resolved at train time - LightGBM's `objective` and `nrounds`, LightRF's `feature_fraction`, GLMNET's `lambda` - where `mod@hyperparameters` previously reported `NULL`.
- `schema.rtemis.org` publishes a `record.json` beside every `schema.json`, and each record is validated against its schema as it is written.

**Metrics**

- `RegressionMetrics` and `ClassificationMetrics` declare their tables with typed, bounded columns, validated on construction: a rate outside `[0, 1]`, an undeclared column, or a missing one is rejected with a message naming the field. `MetricsRes` and its subclasses are typed likewise, covering per-resample values and their mean and standard deviation.
- Per-class metrics name their outcome level in a `level` column rather than in row names, so serialized metrics keep their labels.
- New `confusion_long` (`reference`, `predicted`, `n`) is the declared property and what serializes; `metrics@confusion_matrix` is unchanged as a labelled `table`. `$` and `[[` on a metrics object now reach its properties as well as its metrics.
- `classification_metrics(sample = )` and `regression_metrics(sample = )` default to `NULL` rather than `character()`, and accept only the sample names rtemis uses.
- `schema.rtemis.org` publishes `regressionmetrics/v1`, `classificationmetrics/v1` and their resampled counterparts.

**Declarations and schemas**

- `to_json()` emits exactly what the schemas declare, walking a class's published properties, so a computed view or an R-only value can no longer reach the wire undeclared.
- New `prop_factor()` declares a factor-valued property: a distinct R class, so a character vector assigned to a classification outcome is a type error; and `{levels, codes}` on the wire - the levels in order, and a 1-based index into them per case.
- New declaration axes: `min_items` and `unique_items` for array length and distinctness, `default_on_null` for "apply the default for this task type", and `prop_computed()` for a derived view, omitted from schemas and written configs.
- `data_dependent` is now a pure annotation and no longer suppresses serialization, so `id_strat`, tSNE's `Y_init` and DBSCAN's `weights` round-trip.
- `data_bound` gains `"numeric_feature_names"`, declared by `DecompositionConfig@features`; `check_data_bounds()` now works with any config object rather than only `Hyperparameters`.
- `setup_Preprocessor()` configs can be written and read: `preprocessor` joins the supported config families, with a new `.list_to_PreprocessorConfig()`.
- Results-class properties that were `class_any` are now typed: `@y_*` and `@predicted_*`, `@type`, and `SupervisedSession@started` / `@finished`.
- New `JSONSchema_to_S7()` builds a live S7 class from a schema generated by `S7_to_JSONSchema()`, completing the round trip with the same types, bounds, enums and validators.

**Validation**

- Hyperparameter constraints that depend on the training data are declared with `data_bound = ` rather than hand-written, and checked before any model is fit: `setup_CART(cost = )`, `setup_GLMNET(penalty_factor = , offset = )`, and `setup_Ranger(mtry = , case_weights = , class_weights = , always_split_variables = )`. An out-of-range value aborts once, naming the value and the dimension it must match, rather than surfacing as a run of failed tuning cells.
- New internal generic `validate_hyperparameters(hyperparameters, x)` runs those checks, called before any tuning and again immediately before the algorithm runs.
- `setup_LightRF(feature_fraction = )` defaults to `NULL`, meaning derive from the data - `sqrt(n_features)/n_features` for classification, `0.33` for regression.

## 1.3.0

- All configuration classes - execution, preprocessing, resampling, tuning, hyperparameters, clustering, decomposition, and the pipeline recipes - now declare their user-settable properties through the `prop_*` factories (`prop_boolean()`, `prop_integer()`, `prop_float()`, `prop_string()`), completing the rollout begun in the hyperparameter classes. Each such property carries a `PropertySpec` recording its type, bounds, enum, nullability, and description, so type checking, validation, and JSON Schema generation all derive from a single declaration instead of being written three times. Runtime and fitted-model properties (e.g. `Supervised@model`) keep their plain `class_*` declarations.
- Optional (nullable) properties now enforce that `NULL` is the only "unset" value: a nullable property is declared `NULL | <class>` so S7 prototypes it to `NULL` rather than the base class's empty vector, and a zero-length value reaching validation is rejected with a corrective message (`must not be empty (use NULL to leave it unset)`). This keeps every downstream `!is.null()` guard meaningful.
- Properties that are *not* user-settable configuration now say so at the declaration site, completing the picture above. `prop_state()` marks run state written during training or tuning (GLMNET's `lambda.min` / `lambda.1se`, LightGBM's `nrounds` / `best_iter`): never schematized, never serialized, re-derived on read. `prop_external()` marks a genuine config input whose R type the `prop_*` factories cannot express (tSNE's `Y_init`, Ranger's `inbag`, TabNet's `optimizer`, `id_strat`, the preprocessor's learned scaling values), optionally `data_dependent` when the value is tied to a particular dataset and so has no portable form. `prop_role()`, `role_prop_names()`, and `data_dependent_prop_names()` read them back.
- New `S7_to_JSONSchema()` generates a JSON Schema directly from an S7 class, `S7_dispatcher_JSONSchema()` composes leaf schemas into a discriminated-union dispatcher, and `write_JSONSchema()` serializes a schema to file. These generate the `supervised`, `hyperparameters`, `resampler`, `decompose`, and `cluster` schemas consumed by the rtemis CLI and rtemis.server.
- `S7_to_JSONSchema()` derives which properties take part from those roles instead of an `exclude` list: the `exclude` argument is replaced by `base`, naming the family base class whose inherited properties are machinery rather than config. A property declared `prop_external()` must have its schema fragment supplied via `extra`, which is now checked - previously a forgotten fragment silently dropped a key from the published contract. A property with no declared role remains an error. The generated schemas are unchanged.
- `setup_Preprocessor(impute_type = )` takes the full set of choices (`"missRanger"`, `"micePMM"`, `"meanMode"`) as its default and matches on them, as the other enumerated setup arguments do, so the choices are visible in the signature and to callers that introspect formals.

## 1.2.8

- New `session_timeline()` flattens a `SupervisedSession` execution graph into a timeline (Gantt) table -- one row per node in depth-first order with millisecond offsets, status, and tooltip text. It is the shared source for rtemis.draw's `plot()` method on `SupervisedSession` and rtemis.server's `job.result` `session` slice (rtemislive Timeline tab). `session_kind_colors()` (internal, exported) provides the matching fixed kind → color map so all renderers color steps identically.
- Progress reporting now uses `rtemis.core::progress_lapply()` (new in rtemis.core 0.4.0) instead of `cli::cli_progress_along()` in `train()` outer resampling, sequential `tune_GridSearch()`, and `massGLM()`. Nested runs render a single breadcrumb status line (`Outer resamples 2/5 › Tuning 7/30 ETA 0:41`) with a color-pulsing spinner, and emit structured `level = "progress"` envelopes through the rtemis.core message sink for `rtemis.server`. The **cli** dependency is dropped.
- Parallel tuning now reports progress through the same system: new `handler_rtemis()` bridges progressr `progression` conditions (relayed by future from workers) onto the rtemis progress renderer, and `tune_GridSearch()` wraps its future backend in `progressr::with_progress(handlers = handler_rtemis(...))` - previously, worker ticks were silent unless the user had activated progressr handlers themselves. The mirai backend polls task resolution and reports through the same renderer (replacing mirai's own cli collection bar).
- `train(dat)` defaults to ranger instead of throwing an error.
- `read` now uses preprocess to remove duplicates. renamed `make_unique` => `remove_duplicates`.
- Breaking change: New `apply_preprocessor(preprocessor, new_data)` applies a trained `Preprocessor` to new data and returns the preprocessed data directly, analogous to `predict()` for models. It replaces `preprocess(x, Preprocessor)`; `preprocess(x, config)` now accepts only a `PreprocessorConfig`.
- `preprocess()` now also accepts `setup_Preprocessor()` arguments directly for interactive use, e.g. `preprocess(x, scale = TRUE)`, creating the `PreprocessorConfig` internally. Calling `preprocess(x)` with no preprocessing parameters is an error.

## 1.2.7

- Added `DecomposeConfig` and `ClusterConfig` pipeline-recipe classes with `setup_DecomposeConfig()` / `setup_ClusterConfig()`, mirroring `SuperConfig`: they bundle a data path, the algorithm config (`DecompositionConfig` /
`ClusteringConfig`), and an output directory.
- `decomp()` now accepts `DecomposeConfig` objects.
- `cluster()` now accepts `ClusterConfig` objects.
- Added `outdir` arg to `decomp()` and `cluster()`
- Added `read_config()` & `write_config()` with support for the new `supervised`, `decompose`, `cluster` schemas.
- Switched from Makefile to justfile

## 1.2.6

- `SupervisedRes` now records `preprocessor_config` and `decomposition_config`; updated `repr`.
- Added early input validation: column-type check in `check_supervised`, new `check_numeric_or_factor()`, and a check that the requested decomposition exists.
- `decomp()` now reports the number of features and components.
- Exported `show_color_key()`.
- `repr` moved to `rtemis.core`; metric acronyms now capitalized in console output.
- Extracted `roc_curve()`.

## 1.2.5

- Adopted the `rtemis.core` condition system (`rtemis.core::abort()` / `warn()`); documentation now links to rtemis conditions.
- `read()` now errors if the file does not exist.
- Improved `sanitize_path()`.
- Initial `SupervisedSession` support.

## 1.2.4

- Added `nanoparquet` support for reading and writing data (added to Suggests).
- Added `default_n_workers()`, used in `.onAttach()`.
- Added `numeric_features()` generic and methods.
- Added `features` property to `DecompositionConfig` for use within `train()`.
- WASM-safe parallel-worker detection.
- Moved shared utilities to `rtemis.core`.

## 1.2.3

- Added `decomposition_config` support to `SuperConfig`, `SuperConfigLive`, and `train()`, with new `apply_decomp` methods where supported.
- Converted `Regression` and `Classification` metric field names to lower case.
- Added `description` field to `to_json()` output.
- Added `verbosity` argument to the `describe()` S7 generic and methods.

## 1.2.2

- Added `set_positive_class()`. Can be used directly by users. Used by `rtemis.server` and `rtemislive` to pass the positive case from the UI to `rtemis`.
- Added `positive_class` field to `SuperConfigLive`.
- Added aggregated confusion matrix to resampled classification results (`ClassificationMetricsRes`); moved `Confusion_Matrix` out of the metrics object.
- Added `progress` argument to `train()` to allow a callback for `rtemis.server`.
- Added `get_varimp()` method.
- Added `rtemis.core` to Imports.

## 1.2.1

- Added the package name to S7 class definitions and regenerated docs for roxygen2 8.0.0.
- Exported additional internals required by `rtemis.server`.

## 1.2.0

- Add `rtemis.server` support:
  - New `SuperConfigLive` S7 class for server-based training configuration.
  - New `set_msg_sink()`, `get_msg_sink()`, `with_msg_sink()` functions to capture and redirect rtemis console messages.
  - New `to_json()` S7 generic to convert rtemis objects to JSON-serializable lists.
- Add `verbosity` argument to `predict_super()`; remove `...`.
- Add `names()` S7 method for `Theme` objects.
- Updated to roxygen2 8.0.0

## 1.0.1

- Introduce `VariableImportance` S7 class to represent variable importance data, allowing for more than one measure of importance per model and update all relevant classes and methods.
- Calculate Partial_Effect_Variance as variable importance measure for GAM models
- Add `execution_config` argument to internal `train_` method and use it in LightRuleFit to propagate to LightGBM and GLMNET calls.

## 1.0.0 First CRAN release
