# rtemis 1.0.0

## Major Features

* Advanced machine learning framework with unified API for supervised and unsupervised learning
* Comprehensive data visualization capabilities
* Uses S7 classes and methods with type checking and validation internally
* Efficient functional user-facing API
* Support for parallel processing via future framework
* Interactive visualizations with plotly and other htmlwidgets

## Dependencies

* Core dependencies: S7, data.table, future, htmltools, cli
* Extensive support for optional packages for specialized functionality
* Compatible with R >= 4.1.0

## Documentation

* Complete function documentation
* Comprehensive examples throughout
* Available at https://rdocs.rtemis.org

## S7 re-write

* The package was re-written to use S7 classes and methods throughout, instead of the previous R6-based implementation.
* The goal is to provide a robust, type-safe backend while maintaining a functional user-facing API.
* The original package featured ~80 supervised learning algorithms, many of which we never used. The new version begins with a core set of essential algorithms, with more planned for future releases.

---

This is the initial CRAN release of rtemis.
