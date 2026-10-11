# rtemis news

## 1.4.2

- `setup_PCoA()` configures principal coordinates analysis (classical multidimensional scaling) on a choice of dissimilarities; it applies to new data through Gower's out-of-sample formula, so it can be `train()`'s decomposition step, and does not reconstruct the input.
- `setup_MDS()` configures metric or nonmetric multidimensional scaling through 'vegan' with multiple starts; it cannot be applied to new data and does not reconstruct the input.
- `review()` assesses a trained supervised model, single-split or resampled: it reports sample sizes, every training and test metric, and comparisons with a baseline predictor, with confidence intervals and tests for a single split and a description of the variation between resamples for a resampled model, then notes small samples, many predictors, and signs of overfitting.
- `ai_review()` asks a language model, through 'rtemis.llm', to write a summary, evaluation, next steps and caveats from a `review()`, statements citing the review's finding codes, checked against the review; the result keeps the review and records the model, settings, prompt and a hash of the review.
- A fitted model records the value its backend chose for a hyperparameter left unset, so the model and its record state every value the fit used: for example `mtry` for ranger, `nk` for MARS, the loss and optimizer settings for MLP, and the engine settings for LINAD.
- LightRuleFit stops with a corrective error when its boosting stage makes no split.
- `setup_TabNet()` rejects `importance_sample_size` when `skip_importance` is TRUE.
- `setup_CART(cost =)` and `setup_Ranger(class_weights =)` reach their backends; class weights named by class are matched to the outcome's levels.
- `setup_Ranger()` drops `case_weights`, which ranger did not receive; per-case weights are passed with `train(weights =)`.
- `setup_GLMNET()` drops `offset`, which failed at prediction and under resampling.
- `setup_LINAD()` and `setup_LINADForest()` take `root_forward_stop` and `root_node_test`, the root model's stopping rule and slopes test; unset uses the node-level value.
- Variable importance (`get_varimp()`) holds one measure per name, each stating what it quantifies (split gain, permutation, coefficient, ...), the cases it was computed on, whether it is signed or depends on predictor units, how it ranks predictors, and a description for the fitted model; the measures replace the single table in `@data`, and `varimp_table()` combines them into one data.frame. `plot_varimp()` ranks bars by the measure's direction (a cross-validated risk smallest first). Linear SVM weights are oriented toward the positive class, and a forest fitted with `importance = "none"` has no importance.
- `writeup()` writes the Methods and Results sections for a trained supervised model from the model and its `review()`, every number traceable to the field it was read from, with references to R, rtemis, the fitting packages and the statistical methods, a main hyperparameter table (primary hyperparameters and every one tuned, specified or resolved during fitting; `include_hyperparameters` replaces the primary list) and a supplementary table of every hyperparameter with its value, how it was chosen and the values tried, and a list of what the model does not record.
- `write_text()` writes a `writeup()`, `review()` or `ai_review()` as Markdown.
- Stratified resampling (`setup_KFold()`, `setup_StratSub()`, `setup_StratBoot()`) stratifies a categorical variable by its levels; with more than four levels it grouped adjacent levels together.
- Metric labels print AUC, PPV and NPV in capitals.
- LightRF predictions are no longer pulled toward 0 when some trees cannot split, as happens on small samples: such trees now predict their sample's center. Training reports how many trees could not split.
- `setup_SuperConfig()` and `setup_SuperConfigLive()` reject a decomposition that cannot be applied to new data, as reading a supervised config already did.
- Shared results identify implementation-specific learner, execution, preprocessing, and resampler configs by schema and read their typed settings without inserting omitted defaults.
- `write_config()` and `write_record()` write numbers with 15 significant digits instead of rounding to four decimal places.
- Each parallel task receives only its own resample or bag; LINADForest and `bias_variance()` pass training data to local workers through shared memory.

### Supervised result validation

- Outcomes, predictions, and classification probabilities must have matching row counts within each sample.
- Categorical outcomes and predictions must preserve the training class levels and their order; probability matrices require one column for binary classification or one per class for multiclass classification.

## 1.4.1

- Model plotting and `present()` use rtemis.draw, including session timelines and SVG export; standalone Plotly `draw_*()` functions remain available.
- Require R >= 4.4.0 for the rtemis.draw backend.
- `fit_predict()` trains one regression model from an algorithm name and setup arguments (`params`) and predicts new data; rtemis.draw uses it for `fit = "<algorithm>"` and `fit_params` in scatter plots.

- TabNet and MLP train and predict with the algorithm workers resolved from `execution_config` (`n_workers_algorithm`) rather than every core.
- `train()` prints one resources line: the compute device in use (including an Apple silicon GPU), the execution backend, the worker ceiling, and each level's share.
- `decomp()`, `cluster()`, `setup_DecomposeConfig()` and `setup_ClusterConfig()` take `execution_config`: its seed seeds the fit, and a threaded algorithm (UMAP, tSNE) runs on its workers; `decomposition_traits()` gains `threaded`.
- `setup_tSNE(num_threads =)` is deprecated; tSNE takes its threads from the execution config.
- `setup_Autoencoder()` configures a torch autoencoder decomposition, denoising when `input_noise` or `input_dropout` is set; it applies to new data, so it can be `train()`'s decomposition step, and `reconstruct()` returns its reconstruction in the units of the data.
- `setup_VariationalAutoencoder()` configures a torch variational autoencoder decomposition; `beta` weighs the KL divergence (1 is the standard VAE, larger values a beta-VAE), and its components are the latent means.
- `decomp_metrics()` takes `execution_config`.
- NMF with `nrun` greater than 1 runs.
- Execution configs take `device`: `"cpu"`, `"cuda"`, `"mps"` (Apple silicon GPU) or `"opencl"`, or `setup_CUDA(ids =)` to name GPUs. Unset uses CUDA where available and the CPU otherwise; the Apple silicon GPU runs only when named. An algorithm that cannot use the requested device runs on the CPU and the resources line says so.
- `setup_LightGBM(device_type =)`, `setup_LightRF(device_type =)` and `setup_TabNet(device =)` are deprecated; set `device` in the execution config.
- `predict()`, `apply_decomp()` and `reconstruct()` take `execution_config`; predictions use its threads, or the host's default worker count, never the count a model was trained with.
- UMAP runs on the execution config's algorithm threads, which respect the two-core limit of a CRAN check, rather than half the hardware threads.
- `setup_SpectralRBF()`, `setup_SpectralLaplace()`, and `setup_SpectralLocal()` configure spectral clustering through 'kernlab', one per kernel; `setup_Nystrom()` enables the Nystrom approximation.
- `setup_ASWCriterion()`, `setup_CHCriterion()`, and `setup_MultiASWCriterion()` configure how `setup_PAMK()` selects the number of clusters.
- `setup_DBSCAN(approx =)` is a nonnegative number, the approximation factor of the neighbor search (0, the default, searches exactly); it was declared logical, and `TRUE` ran as an approximation factor of 1.
- `setup_ClusterConfig()` and `setup_DecomposeConfig()` no longer take `algorithm`; the clustering or decomposition config names it, and `cluster()` and `decomp()` reject an `algorithm` that disagrees with a supplied config.
- Clustering configs take `features`, the columns to cluster on.
- `cluster()` and `decomp()` use every numeric column when the config names no `features`, and record the columns used.
- Shared results identify implementation-specific configs by schema and read their typed settings without inserting omitted defaults.
- Implementation-specific schema paths include the language namespace; shared result paths remain unqualified.
- The defaults artifact format accepts producer-owned namespaces while retaining separate schema declarations and resolution policies.
- `write_result()` and `read_result()` support portable result JSON with optional Parquet payloads, lazy loading, and integrity checks.
- Resampled results retain successful fold identifiers and all requested training-row splits, including failed folds and bootstrap repetitions.
- `Supervised`, `Regression`, `Classification`, and their resampled counterparts publish shared result schemas with typed outcomes, predictions, metrics, and runtime-only fitted state.
- Categorical result values preserve class order and missing positions through explicit levels and codes.
- Matrix property declarations support numeric cell bounds and cell-level nullability.
- Typed array decoding preserves missing-cell positions and rejects invalid cell types and ragged matrices.
- Schema publication metadata identifies the generating language, contract scope, registry domain, and standalone parent class.
- `Implementation` and `Provenance` record package and language identities independently of schema authorship.
- `VariableImportance` publishes a shared report schema with named numeric measures and null values for unavailable results.
- `JSONSchema_to_S7()` reconstructs standalone inheritance and runtime-only properties from schema artifacts, and rejects a `parent` other than the one the schema declares.
- Reading a table or object with a field its schema does not declare fails with an error naming the field.

## 1.4.0

- `setup_SerialExecution()`, `setup_FutureExecution()`, and `setup_MiraiExecution()` configure their respective execution backends, replacing `setup_ExecutionConfig()`.
- `JSONSchema_to_S7()` reconstructs declaration defaults, typed references, and input policies from a supplied artifact graph.
- Integer property declarations support exclusive lower and upper bounds.
- `ClusteringMetrics` records clustering measures with a status explaining each missing value.
- `HardClustering` and `SoftClustering` distinguish hard assignments from results with membership probabilities.
- `setup_GMM()` configures Gaussian mixture models through 'mclust', with optional selection of the number of components.
- `setup_PAM()` and `setup_PAMK()` configure medoid clustering through 'cluster' and 'fpc'.
- `setup_HOPACH()` configures hierarchical clustering through 'hopach'.

## 1.3.9

- Config families publish flat documents with a discriminator and the selected variant's settings.
- A separate authoring artifact identifies properties reserved for the host.
- `partition()` creates auditable train/test splits with random, time, group, or predefined partition configurations.
- `setup_KFold()`, `setup_StratSub()`, `setup_StratBoot()`, `setup_Bootstrap()`, `setup_LOOCV()`, and `setup_Custom()` configure individual resampling methods. `setup_Resampler()` remains available with a deprecation warning naming its replacement.
- `SuperConfigPaths` and `SuperConfigTabular` share common supervised configuration properties.
- `is_wire_hyperparameters_set()` identifies a serialized set of named learner configurations.
- Supervised records reference session graphs stored as Parquet sidecars.
- `DataRef` records a sidecar's path, encoding, digest, size, and optional table dimensions.

## 1.3.8

- `ingest()` reads supported data formats using format-specific configurations and writes Parquet output with a record of the reading decisions.
- Ingestion configurations accept explicit column type declarations.
- A published profile fixture pairs Parquet datasets with their expected data profiles.
- Supervised configurations expose character-to-factor conversion for file input.
- `setup_SupervisedPreprocessor()` configures preprocessing that preserves cases and feature identity during supervised resampling.
- Published checks, algorithm traits, and a shared conformance corpus support validation across implementations.
- `train(preflight = TRUE)` checks the configuration against the data before training.
- `data_profile()` records dataset dimensions, column types, missingness, distinct values, and level counts.
- `validate_config()` returns structured findings for configuration and data checks, including corrective patches where available.
- `Diagnostic` and `Diagnostics` have published schemas.
- `check_data()` reports distinct value counts per column.

## 1.3.7

- `setup_LINAD()` configures the Linear Additive Tree, a decision tree with a linear model at every node whose leaf coefficients are the sum along the path.
- LINAD recovers a pure linear model at `max_leaves = 1` and a CART-equivalent tree with constant nodes at `gamma = 0`.
- `setup_LINAD(split_search = "exhaustive")` scores each split by the loss after fitting both child models.
- LINAD selects its number of leaves on `dat_validation`; `force_max_leaves = TRUE` keeps the full tree, and `patience` stops growth early.
- `setup_LINAD()` takes `gamma`, `root_learning_rate`, `constant_rule`, `line_search`, `node_model` \{"forward", "ridge", "elasticnet"\}, `node_test`, `split_criterion`, `split_binning`, and `split_bin_type`.
- `setup_LINAD(split_features = , linear_features = , global_features = )` restrict which features may split, which enter the node models, and which have one coefficient shared by every leaf.
- `get_varimp()` on LINAD reports `importance`, the linear effect, and `split_gain`, the partitioning effect.
- `draw_linad()` draws a fitted LINAD tree as an interactive hierarchy with per-node coefficient tables; `show_changes` marks global, changed, and inherited coefficients.
- `setup_LINADForest()` configures a bagged ensemble of Linear Additive Trees, each sized on its own out-of-bag cases, with `mtry_tree` and `mtry_split` feature sampling.
- `se()` on a LINADForest returns infinitesimal jackknife standard errors.
- A fitted LINADForest reports out-of-bag predictions and metrics.
- LINADForest parallelizes across trees with results independent of worker count and backend.
- `setup_GLMTree()` configures model-based recursive partitioning through 'partykit', with separate `regressors` and `partitioning_variables`.
- `plot_learning()` and `get_learning_curve()` show loss against training progress for algorithms that train in steps.
- `train()` accepts a named list of hyperparameter configurations for one algorithm and tunes across all of them; `mod@hyperparameters@variant` names the selected configuration.
- `tuning_grid()` on a list of configurations returns the union of their grids with a `.variant` column.
- Configs serialize a set of hyperparameter configurations as `{"variants": {...}}`.
- `bias_variance()` decomposes a learner's error into bias and variance over resamples at a fixed test set; `true_values` measures bias against a known true function.
- Case weights must have one finite, non-negative value per case and a positive mean.
- Volcano and Manhattan plots share one package-wide pair of colors for positive and negative effects.

## 1.3.6

- Configs store the paths they are given, without resolving them against the working directory or expanding `~`.
- `predict()` on a multiclass resampled classification returns an `n x k` probability matrix for `type = "avg"` and a list of them for `type = "all"`.
- `predict(type = "metrics")` on a resampled model aggregates per case.
- `predict()` on a `CalibratedClassificationRes` takes `type`.
- LightGBM hyperparameters use LightGBM's own names: `bagging_fraction`, `bagging_freq`, and `boosting` replace `subsample`, `subsample_freq`, and `boosting_type`.
- `setup_LightGBM(boosting = , data_sample_strategy = )` select DART and GOSS, with their parameters.
- `setup_LightGBM()` exposes LightGBM's tree regularization, binning, missing-value, categorical, constraint, sampling, quantization, and determinism parameters.
- `setup_LightGBM()` exposes objective parameters (`alpha`, `tweedie_variance_power`, `fair_c`, `poisson_max_delta_step`, `sigmoid`, `boost_from_average`, `reg_sqrt`), each applicable only under the objective that uses it.
- `setup_LightCART()`, `setup_LightRF()`, and `setup_LightRuleFit()` expose the LightGBM parameters that apply to each.
- `setup_LightRuleFit()` names its GLMNET-step parameters `alpha_glmnet` and `lambda_glmnet`.
- Printing a `Hyperparameters` object summarizes unset, tunable, and fixed hyperparameters.
- Property schemas carry `x-rtemis.group`, naming the declaration group of each hyperparameter.
- `conformal()` returns prediction intervals or label sets with finite-sample coverage for any supervised model.
- `setup_SplitConformal()`, `setup_CVPlus()`, and `setup_CQR()` configure split conformal, CV+, and conformalized quantile regression.
- Conformal classification sets take `score` \{"APS", "LAC"\}.
- `conformal_metrics()` reports coverage and interval width or set size.
- `predict()` on a Ranger model leaves the caller's RNG state unchanged.
- Execution configs take `shared_memory` \{"auto", "none", "always"\} to pass training data to local workers through 'mori'.
- Execution configs take `n_workers_outer`, `n_workers_tuning`, and `n_workers_algorithm` to assign workers per level.
- `setup_MiraiExecution()` takes `warm_workers` to load rtemis in each worker as the pool starts.
- Workers start once per `train()` call and are reused across dispatches; the execution graph times their startup in a `worker_pool` node.
- Grid search results are identical across backends and worker counts; tuning results change for algorithms that draw random numbers.
- 'parallelly' moves from Suggests to Imports; 'futurize' and 'future.apply' are no longer dependencies.
- `preprocess()` modifies a data.table by reference and runs substantially faster.
- `setup_Preprocessor(holidays = )` selects the holidays `add_holidays` flags.
- `preprocess()` creates date and holiday features before encoding and scaling them.
- `setup_Ranger(split_select_weights = )` accepts a single vector applied to every tree.
- `setup_Resampler(id_strat = )` takes a column name; `train()` groups resamples by it and excludes it from the features.
- `read()` reads Parquet files with Arrow `string_view` and `binary_view` columns.

## 1.3.5

- rtemis is licensed under BSD 3-clause, replacing GPL (>= 3).
- `train()` parallelizes outer resampling.
- Execution configs take `seed`, a master seed for computation separate from the resampling seed; an unset seed is drawn and recorded.
- Each outer fold runs on its own RNG substream, so results match across sequential and parallel runs; results for stochastic algorithms under outer resampling change.
- `train()` leaves the caller's RNG state unchanged when a seed is in effect.
- A parallel run records the same execution graph as a sequential one.
- `explain()` computes per-case Shapley contributions for every supervised algorithm, exactly where the model structure allows and by KernelSHAP through 'shapr' otherwise.
- `setup_SHAP()` configures the explanation estimator, perturbation, scale, background, and coalitions.
- `explanation_methods()` lists, per algorithm, the estimator `"auto"` selects and whether it is exact.
- `shap_case()`, `shap_long()`, `shap_by_level()`, and `get_varimp()` read an explanation.
- `explain()` on a `SupervisedRes` takes `type` \{"avg", "all"\}.
- Shapley contributions are reported per input column, summed over encoded columns.
- `reconstruct()` returns a decomposition's reconstruction of the data in input units, for PCA, ICA, and NMF.
- `decomp()` stores reconstruction and component metrics in `Decomposition@metrics` and the run record.
- `decomp_metrics()` computes out-of-sample decomposition metrics.
- `decomposition_traits()` lists each decomposition algorithm's properties; `available_decomposition(traits = TRUE)` prints them.
- `applicable_metrics()` lists which metrics apply to which decomposition algorithm.
- `Decomposition` and `Clustering` record a data fingerprint, reported in run records.
- NMF scores are each case's non-negative coefficients on the fitted basis; NMF results change.
- `setup_NMF(method = )` selects the `NMF::nmf()` method.
- ICA `apply_decomp()` reproduces the fitted components under `row_norm = TRUE`; ICA scores from `apply_decomp()` change.
- `fitted_config()` returns a `Preprocessor`'s config with its learned values filled in.
- 'htmltools' moves from Imports to Suggests, needed only by `draw_leaflet()`.
- 'openssl' replaces 'digest' as a dependency.
- `setup_MLP()` configures a multilayer perceptron through 'torch' for regression and classification.
- `setup_MLP()` takes explicit `hidden_units` or generates them from `shape`, `shape_layers`, and `shape_max_units`; `hidden_units` is tunable, one architecture per candidate.
- MLP encodes categorical features as learned embeddings or one-hot columns, and centers and scales numeric features.
- MLP stops early on `dat_validation` and keeps the best epoch's weights.
- A fitted MLP can be saved and reloaded.
- MLP uses CUDA when available and the CPU otherwise; `device = "mps"` is supported on request.
- Printed hyperparameters show search spaces inline, tagged `<tune>`.

## 1.3.4

- Tuning requires `tune_over()`: `setup_LightRF(max_depth = tune_over(3L, 4L, 5L))`. A bare vector is a value, and `max_depth = 3:5` is an error naming the replacement.
- Serialized search spaces are tagged objects, `{"candidates": [3, 4, 5]}`, with at least two candidates.
- Vector-valued hyperparameters can be tuned.
- `apply_preprocessor()` codes factors against the training levels for `factor2integer` and one-hot encoding, so predictions on new data use the training encoding.
- `factor2integer` codes are integer.
- `scale` and `center` skip columns coded by `factor2integer`.

## 1.3.3

- `setup_SuperLearner()` configures the cross-validated stacked ensemble of van der Laan, Polley & Hubbard for regression and binary classification.
- `setup_ModalityStacking()` stacks base learners, each bound to one group of features.
- `setup_ConditionalSuperLearner()` routes each case to one expert from a library through a learned oracle (Valdes, Interian, Gennatas & van der Laan, 2022).
- `setup_NNLS()` configures non-negative least squares through 'nnls', the default stacking meta learner.
- `setup_MARS()` configures Multivariate Adaptive Regression Splines through 'earth' for regression and classification.
- `train(weights = )` accepts a numeric vector.
- A `SuperConfig` `weights` column supplies case weights and is excluded from the features.
- `read_config()` validates configs without calling the rtemis CLI; `options(rtemis.validate = )` and `options(rtemis.cli = )` are removed.

## 1.3.2

- Isotonic calibration keeps probabilities at least `1 / (2 * n)` from 0 and 1.
- `calibrate(hyperparameters = NULL)` selects `setup_Isotonic()`.
- Grid search leaves unset any hyperparameter that does not apply to the rest of a combination, and collapses duplicate combinations.
- `tuning_grid()` returns the combinations a grid search would fit.
- Published schemas declare hyperparameter dependencies in `x-rtemis.applies_when`.
- `setup_SPLS()` configures Sparse Partial Least Squares through 'spls'.
- `setup_KNN()` configures weighted k-Nearest Neighbors through 'kknn'.
- `setup_BART()` configures Bayesian Additive Regression Trees through 'stochtree'; `se()` returns the posterior standard deviation of the mean function.
- `setup_HAL()` configures the Highly Adaptive Lasso through 'hal9001'.
- `train()` projects a HAL basis size before fitting, warns past one million, and aborts past `max_basis`.
- `setup_MonotonicHAL()` configures a monotonic non-decreasing Highly Adaptive Lasso, usable as a calibrator.
- `available_calibration()` lists the algorithms usable as calibration maps.
- `CalibratedClassification@calibrator` and `CalibratedClassificationRes@calibrator` record the calibrator used.
- `plot_varimp(measure = )` selects among a model's importance measures.

## 1.3.1

- Predicted probabilities are a matrix with one row per case and one column per class; binary outcomes carry a single column for the positive class.
- `train()`, `calibrate()`, `setup_SuperConfig()`, and `setup_SuperConfigLive()` no longer take `algorithm`; the `setup_*()` function names the algorithm.
- `se(mod, newdata)` computes standard errors on demand, replacing the `@se_*` properties of `Regression` and `RegressionRes`.
- The `.list_to_*()` reconstructors reject undeclared keys and suggest the nearest valid property.
- `decomp(x, config)` uses `config@features`.
- A classification result's `positive_class` is `NULL` when the outcome is not binary.
- `train()`, `decomp()`, and `cluster()` write a `<prefix>.record.json` run record to `outdir`, stating every resolved value with its origin, provenance, and a data fingerprint.
- `record()` returns a run record; `write_record()` writes one.
- Run records list each fitted model under `folds`, with its tuning results, and include headline metrics per sample.
- `Supervised`, `SupervisedRes`, `Decomposition`, and `Clustering` store the config they were given; `read_config()` rejects a run record.
- Fitted models report hyperparameter values resolved during training, such as LightGBM's `nrounds`.
- Published schemas include a `record.json` beside each config schema.
- `RegressionMetrics`, `ClassificationMetrics`, and their resampled counterparts have typed, validated tables and published schemas.
- Per-class metrics name their outcome level in a `level` column.
- Classification metrics carry `confusion_long`, a long-format confusion table.
- `to_json()` emits the properties the published schemas declare.
- Factor-valued properties serialize as `{levels, codes}`.
- `write_config()` and `read_config()` support preprocessor configs.
- `JSONSchema_to_S7()` builds an S7 class from a schema generated by `S7_to_JSONSchema()`.
- Hyperparameters bounded by the training data are checked before any model is fit.
- `setup_LightRF(feature_fraction = NULL)` derives the value from the data.

## 1.3.0

- Configuration classes declare typed, validated properties from which their JSON Schemas are generated.
- Optional config properties use `NULL` as the only unset value and reject zero-length values.
- `S7_to_JSONSchema()`, `S7_dispatcher_JSONSchema()`, and `write_JSONSchema()` generate and write JSON Schemas from S7 classes.
- `setup_Preprocessor(impute_type = )` lists its choices in the signature.

## 1.2.8

- `session_timeline()` flattens a `SupervisedSession` execution graph into a timeline table; `session_kind_colors()` provides a shared color map.
- Progress reporting uses `rtemis.core::progress_lapply()`, including for parallel tuning; 'cli' is no longer a dependency.
- `train(dat)` defaults to Ranger.
- `read()` removes duplicates through `preprocess()`; its `make_unique` argument is renamed `remove_duplicates`.
- `apply_preprocessor()` applies a trained `Preprocessor` to new data, replacing `preprocess(x, Preprocessor)`.
- `preprocess()` accepts `setup_Preprocessor()` arguments directly, e.g. `preprocess(x, scale = TRUE)`.

## 1.2.7

- Added `DecomposeConfig` and `ClusterConfig` pipeline-recipe classes with `setup_DecomposeConfig()` / `setup_ClusterConfig()`, mirroring `SuperConfig`: they bundle a data path, the algorithm config (`DecompositionConfig` / `ClusteringConfig`), and an output directory.
- `decomp()` now accepts `DecomposeConfig` objects.
- `cluster()` now accepts `ClusterConfig` objects.
- Added `outdir` arg to `decomp()` and `cluster()`.
- Added `read_config()` & `write_config()` with support for the new `supervised`, `decompose`, `cluster` schemas.

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

- Added the package name to S7 class definitions.
- Exported additional internals required by `rtemis.server`.

## 1.2.0

- Add `rtemis.server` support:
  - New `SuperConfigLive` S7 class for server-based training configuration.
  - New `set_msg_sink()`, `get_msg_sink()`, `with_msg_sink()` functions to capture and redirect rtemis console messages.
  - New `to_json()` S7 generic to convert rtemis objects to JSON-serializable lists.
- Add `verbosity` argument to `predict_super()`; remove `...`.
- Add `names()` S7 method for `Theme` objects.

## 1.0.1

- Introduce `VariableImportance` S7 class to represent variable importance data, allowing for more than one measure of importance per model and update all relevant classes and methods.
- Calculate Partial_Effect_Variance as variable importance measure for GAM models
- Add `execution_config` argument to internal `train_` method and use it in LightRuleFit to propagate to LightGBM and GLMNET calls.

## 1.0.0 First CRAN release
