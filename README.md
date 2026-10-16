[![CRAN status](https://www.r-pkg.org/badges/version/rtemis)](https://CRAN.R-project.org/package=rtemis)
[![r-universe](https://rtemis-org.r-universe.dev/badges/rtemis)](https://rtemis-org.r-universe.dev/rtemis)
[![R-CMD-check](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml)

# rtemis: Schema-Driven Machine Learning & Visualization.

[![rtemis cover](https://www.rtemis.org/rtemis-cover.avif)](https://docs.rtemis.org/r/ml/)

- **Schema-driven configuration**: every step of a workflow, from data ingestion and preprocessing to resampling, tuning, learning, and execution, is a typed [**S7**](https://github.com/RConsortium/S7) config built by a `setup_*()` function and published as a JSON Schema at [schema.rtemis.org](https://schema.rtemis.org). `write_config()` and `read_config()` move the same config between R, the `rtemis` CLI, and rtemislive.
- **Validated inputs**: type checks and validation run when a config is created and before any fitting starts, with corrective error messages.
- **Functional API**: `train()`, `cluster()`, and `decomp()` provide one-call supervised learning, clustering, and decomposition. A single `train()` call handles preprocessing, decomposition, hyperparameter tuning, stacking, and nested resampling.
- **Observable runs**: timestamped messages report each step, at the level set by `verbosity`.
- **Auditable outputs**: records state every resolved value and its origin; `review()` assesses a trained model, and `writeup()` drafts its Methods and Results with every number traceable to its source.
- **Algorithms**: a broad set of supervised learners, including native SuperLearner, RuleFit, and Linear Additive Trees, alongside clustering and decomposition methods, including torch autoencoders.
- **Explanation and uncertainty**: Shapley values (`explain()`), calibration, and conformal prediction.
- **Parallel execution** through future or mirai, and model visualization through [rtemis.draw](https://github.com/rtemis-org/draw).
- Accepts `data.frame`, `data.table`, and `tibble` inputs.

![rtemis console ascii](https://www.rtemis.org/rtemis-splash.webp)

## Installation

### From CRAN

```{r}
pak::pak("rtemis")
```

or

```r
install.packages("rtemis")
```

### Latest version from `r-universe`

```r
pak::repo_add(rtemis_universe = "https://rtemis-org.r-universe.dev")
pak::pak("rtemis")
```

or using `install.packages`:

```r
install.packages(
  'rtemis',
  repos = c('https://rtemis-org.r-universe.dev', 'https://cloud.r-project.org')
)
```

## Installation of dependencies

Every `rtemis` call that uses third-party packages includes a check for required dependencies and will print a message if any are missing.

## Transparent messaging

It is essential to maintain transparency of operations at all times.
`rtemis` functions often call multiple other functions, sometime recursively. The package uses a formatted messaging system to provide logging output which includes:

- Timestamp
- Message
- Origin (function name)

Most function include a `verbosity` argument to control the level of messaging output, with support for three levels:

- `0`: silent
- `1`: normal messaging
- `2`: detailed messaging for debugging

## `setup_` functions

Machine learning workflows involve multiple steps, each with their own configuration options.

It is essential that a) the user has complete control over each step, while maintaining an intuitive, user-friendly interface, and b) user input is validated early, before a potentially long-running operation is started.

The following `setup_` functions are available to configure each step of the workflow:

- Supervised Learning:  `setup_CART()`, `setup_GAM()`, etc.
- Tuning: `setup_GridSearch()`
- Clustering: `setup_CMeans()`, `setup_HardCL()`, etc.
- Decomposition: `setup_NMF()`, `setup_ICA()`, etc.
- Resampling: `setup_KFold()`, `setup_StratSub()`, etc.
- Preprocessing: `setup_Preprocessor()`

## Supervised Learning

The following will perform hyperparameter tuning and 10-fold cross-validation.  
It will train `(3*3*2*5 + 1) * 25 = 2275` models total.

```r
mod_lightgbm <- train(
  dat,
  hyperparameters = setup_LightGBM(
        num_leaves = tune_over(2^(1:3)),
        learning_rate = tune_over(.001, .005, .01),
        subsample = tune_over(.6, .9)
  ),
  outer_resampling_config = setup_StratSub(
    n_resamples = 25L
  )
)
```

## Clustering

```r
clust_cmeans <- cluster(
  dat,
  config = setup_CMeans(k = 4L)
)
```

## Decomposition

```r
decomp_ica <- decomp(
  dat,
  config = setup_ICA(k = 12L)
)
```

## Documentation

The documentation is available at [docs.rtemis.org/r/ml](https://docs.rtemis.org/r/ml), which includes
walkthroughs of main features and full API reference.

---

© 2016–2026 E.D. Gennatas. Licensed under [BSD 3-Clause](https://opensource.org/license/bsd-3-clause).

