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

## Installation

Using `pak` from GitHub:

```r
pak::pkg_install("rtemis-org/rtemis")
```

Using `install.packages` from `r-universe`:

```r
install.packages(
  'rtemis',
  repos = c('https://egenn.r-universe.dev', 'https://cloud.r-project.org')
)
```

Using `pak` from `r-universe`:

```r
pak::repo_add(myuniverse = "https://egenn.r-universe.dev")
pak::pkg_install("rtemis")
```

## Changes & Ongoing work

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

## rtemisalpha

The original, unmaintained version of rtemis remains available as `rtemisalpha` [here](https://github.com/rtemis-org/rtemis-legacy).