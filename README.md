[![R-CMD-check](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml)

# rtemis: Advanced Machine Learning &amp; Visualization.

![rtemis v1.0.0 console ascii](https://www.rtemis.org/rtemis_1.0.0.jpg)

This is the new version of the rtemis R package and remains under active development.

The new version (0.99+) features:

- Backend: complete rewrite of the supervised and unsupervised learning backend using the new [**S7** class system](https://github.com/RConsortium/S7), replacing all previous use of R6 and S3 classes.
- API: **Functional user-facing API**, to maintain a consistent, user-friendly interface.
- Extended use of **`setup_()`** functions, to offer increased transparency of configuration options.
- Strict **type checking** and **condition validation** throughout to minimize user error and provide highly focused error messages & suggestions.
- Expanded transparent messaging through each step.

## Installation of rtemis

Using `pak` from GitHub:

```r
pak::pak("rtemis-org/rtemis")
```

Using `install.packages` from `r-universe`:

```r
install.packages(
  'rtemis',
  repos = c('https://rtemis-org.r-universe.dev', 'https://cloud.r-project.org')
)
```

Using `pak` from `r-universe`:

```r
pak::repo_add(myuniverse = "https://rtemis-org.r-universe.dev")
pak::pak("rtemis")
```

## Installation of dependencies

Every `rtemis` call that uses external packages includes a check for required dependencies and will print a message if any are missing.

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

## Text formatting

`rtemis` includes an automatic text formatting system, which supports:

- plain text output (for output to log files)
- ANSI colored output (for R console)
- HTML formatted output (for Quarto documents, shiny apps, etc.)

## `setup_` functions

Machine learning workflows involve multiple steps, each with their own configuration options.

It is essential that a) the user has complete control over each step, while maintaining an intuitive, user-friendly interface, and b) the user input is validated immediately and before a potentially long-running operation is started.

The following `setup_` functions are available to configure each step of the workflow:

- Supervised Learning:  `setup_CART()`, `setup_GAM()`, etc.
- Tuning: `setup_GridSearch()`
- Clustering: `setup_CMeans()`, `setup_HardCL()`, etc.
- Decomposition: `setup_NMF()`, `setup_ICA()`, etc.
- Resampling: `setup_Resampler()`
- Preprocessing: `setup_Preprocessor()`

## Supervised Learning

The following will perform hyperparameter tuning and 10-fold cross-validation.  
It will train `(3*3*2*5 + 1) * 25 = 2275` models total (!).

```r
mod <- train(
  dat,
  hyperparameters = setup_LightGBM(
        num_leaves = 2^(1:3),
        learning_rate = c(.001, .005, .01),
        subsample = c(.6, .9)
  ),
  outer_resampling_config = setup_Resampler(
    n_resamples = 25L,
    type = "StratSub"
  )
)
```

## Clustering

```r
clust <- cluster(
  dat,
  config = setup_CMeans(k = 4L)
)
```

## Decomposition

```r
decomp <- decompose(
  dat,
  config = setup_ICA(k = 12L)
)
```

## Changes from original implementation & Ongoing work

### Algorithms

The original version included a long list of algorithms for supervised and unsupervised learning for testing and experimentation, many of which were rarely used.
The initial release of the new version focuses on a smaller set of core algorithms, that will keep growing.

### Visualization

The original version included the `mplot3` family of visualization functions using base R graphics and the `dplot3` family using `plotly`.
The new release includes the `draw` family of functions, the evolution of the `dplot3` family.

## Documentation

The documentation is available at [rdocs.rtemis.org](https://rdocs.rtemis.org), which includes
walkthroughs of main features and full API reference.

[![rtemis cover](https://rdocs.rtemis.org/assets/rtemis-mlv-cover.webp)](https://rdocs.rtemis.org)

## Ongoing work

There is a lot more coming - both within this package and the other packages in the rtemis framework.

## rtemisalpha

The original, unmaintained version of rtemis remains available as `rtemisalpha` at [rtemis-org/rtemis-legacy](https://github.com/rtemis-org/rtemis-legacy).
