# rtemis news

## 1.2.8

- Progress reporting now uses `rtemis.core::progress_lapply()` (new in
  rtemis.core 0.4.0) instead of `cli::cli_progress_along()` in `train()`
  outer resampling, sequential `tune_GridSearch()`, and `massGLM()`. Nested
  runs render a single breadcrumb status line (`Outer resamples 2/5 › Tuning
  7/30 ETA 0:41`) with a color-pulsing spinner, and emit structured
  `level = "progress"` envelopes through the rtemis.core message sink for
  `rtemis.server`. The **cli** dependency is dropped.
- Parallel tuning now reports progress through the same system: new exported
  `handler_rtemis()` bridges progressr `progression` conditions (relayed by
  future from workers) onto the rtemis progress renderer, and
  `tune_GridSearch()` wraps its future backend in
  `progressr::with_progress(handlers = handler_rtemis(...))` - previously,
  worker ticks were silent unless the user had activated progressr handlers
  themselves. The mirai backend polls task resolution and reports through
  the same renderer (replacing mirai's own cli collection bar).

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
