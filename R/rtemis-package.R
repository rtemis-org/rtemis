# rtemis-package.R
# ::rtemis::
# 2015- EDG rtemis.org

#' \pkg{rtemis}: Advanced Machine Learning and Visualization
#'
#' @description
#' Advanced Machine Learning & Visualization made efficient, accessible, reproducible
#'
#' @section Online Documentation and Vignettes:
#' <https://rdocs.rtemis.org>
#'
#' @section System Setup:
#' There are some options you can define in your .Rprofile (usually found in your home directory),
#' so you do not have to define each time you execute a function.
#' \describe{
#'     \item{rtemis_theme}{General plotting theme; set to e.g. "whiteigrid" or "darkgraygrid"}
#'     \item{rtemis_font}{Font family to use in plots.}
#'     \item{rtemis_palette}{Name of default palette to use in plots. See options by running `get_palette()`}
#' }
#' @section Visualization:
#' Graphics are handled using the `draw` family, which produces interactive plots primarily using
#' `plotly` and other packages.
#'
#' @section Supervised Learning:
#' By convention, the last column of the data is the outcome variable, and all other columns are
#' predictors. Convenience function [set_outcome] can be used to move a specified column to the
#' end of the data.
#' Regression and Classification is performed using `train()`.
#' This function allows you to preprocess, train, tune, and test models on multiple resamples.
#' Use [available_supervised] to get a list of available algorithms
#'
#' @section Classification:
#' For training of binary classification models, the outcome should be provided as a factor,
#' with the *second* level of the factor being the 'positive' class.
#'
#' @section Clustering:
#' Clustering is performed using `cluster()`.
#' Use [available_clustering] to get a list of available algorithms.
#'
#' @section Decomposition:
#' Decomposition is performed using `decomp()`.
#' Use [available_decomposition] to get a list of available algorithms.
#'
#' @section Type Documentation:
#' Function documentation includes input type (e.g. "Character", "Integer",
#' "Float"/"Numeric", etc).
#' When applicable, value ranges are provided in interval notation. For example, Float: [0, 1)
#' means floats between 0 and 1 including 0, but excluding 1.
#' Categorical variables may include set of allowed values using curly braces.
#' For example, Character: \{"future", "mirai", "none"\}.
#'
#' @section Tabular Data:
#' \pkg{rtemis} internally uses methods for efficient handling of tabular data, with support for
#' `data.frame`, `data.table`, and `tibble`. If a function is documented as accepting
#' "tabular data", it should work with any of these data structures. If a function is documented
#' as accepting only one of these, then it should only be used with that structure.
#' For example, some optimized `data.table` operations that perform in-place modifications only
#' work with `data.table` objects.
#'
#' @name rtemis-package
#' @import stats methods graphics grDevices S7 data.table htmltools
#' @importFrom utils packageVersion sessionInfo getFromNamespace head tail
"_PACKAGE"

NULL
