# rtemis news

## 1.3.2

- New algorithm: **SPLS**, Sparse Partial Least Squares via `spls`, for regression, binary and multiclass classification. `setup_SPLS()`; `k`, `eta` and `kappa` tunable. Takes no case weights, so `ifw = TRUE` aborts.
- New algorithm: **KNN**, weighted k-Nearest Neighbors via `kknn`, for regression, binary and multiclass classification. `setup_KNN()`; `k`, `kernel` and `distance` tunable. Takes no case weights, so `ifw = TRUE` aborts; reports no variable importance, so `get_varimp()` returns `NULL`.
- New algorithm: **BART**, Bayesian Additive Regression Trees via `stochtree`, for regression and binary classification. `setup_BART()`; the mean-forest prior and the variance forest tunable. Being a sampler, it is the first non-linear model with standard errors - `se()` returns the posterior standard deviation of the mean function - and `get_varimp()` reports two measures, `importance` (variable inclusion proportion) and `inclusion_sd`. Multiclass aborts; so does `ifw = TRUE` under `link = "cloglog"`, which `stochtree` cannot weight.
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
