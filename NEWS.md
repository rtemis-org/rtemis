# rtemis news

## 1.2.7

- Added `DecomposeConfig` and `ClusterConfig` pipeline-recipe classes with
  `setup_DecomposeConfig()` / `setup_ClusterConfig()`, mirroring `SuperConfig`:
  they bundle a data path, the algorithm config (`DecompositionConfig` /
  `ClusteringConfig`), and an output directory.
- `decomp()` now accepts `DecomposeConfig` objects.
- `cluster()` now accepts `ClusterConfig` objects.
- Added `outdir` arg to `decomp()` and `cluster()`
- Added `read_config()` & `write_config()` with support for the new `supervised`, `decompose`, `cluster` schemas.
- Switched from Makefile to justfile

## 1.2.2

- Added `decomposition_config` to `SuperConfig`, `SuperConfigLive`, `train()`.

## 1.2.1

- Added `set_positive_class()`. Can be used directly by users. Used by `rtemis.server` and `rtemislive` to pass positive case from UI to `rtemis`.
- Added `positive_class` field to `SuperConfigLive`.
- Added aggregated confusion matrix to `ClassificationMetricsRes`.
- Converted Regression and Classification metrics field names to lower case.
- Added `description` field to `to_json()` output.
- Added `verbosity` argument to `describe()` S7 generic and methods.

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