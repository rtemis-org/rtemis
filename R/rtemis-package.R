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
#' @section System Setup:
#' There are some options you can define in your .Rprofile (usually found in your home directory),
#' so you do not have to define each time you execute a function.
#' \describe{
#'     \item{rt.theme}{General plotting theme; set to e.g. "whiteigrid" or "darkgraygrid"}
#'     \item{rt.palette}{Name of default palette to use in plots. See options by running `rtpalette()`}
#'     \item{rt.font}{Font family to use in plots.}
#'     \item{rt.cores}{Number of cores to use. By default, rtemis will use available cores reported by
#'     future::availableCores(). In shared systems, you should limit this as appropriate.}
#'     \item{future.plan}{Default plan to use for parallel processing.}
#' }
#' @section Visualization:
#' Graphics are handled using the `draw` family, which produces interactive plots using`plotly` and
#' other packages.
#' @section Supervised Learning:
#' Regression and Classification is performed using `train()`.
#' This function allows you to preprocess, train, tune, and test models on multiple resamples.
#' Run [available_supervised] to get a list of available algorithms
#' @section Clustering:
#' Clustering is performed using `cluster()`.
#' Run [available_clustering] to get a list of available algorithms.
#' @section Decomposition:
#' Decomposition is performed using `decomp()`.
#' Run [available_decomposition] to get a list of available algorithms.
#'
#' @section Notes:
#' Function documentation includes input type (e.g. "String", "Integer",
#' "Float"/"Numeric", etc) and
#' range in interval notation where applicable. For example, Float: [0, 1)"
#' means floats between 0 and 1 including 0, but excluding 1
#'
#' For all classification models, the outcome should be provided as a factor,
#' with the *second* level of the factor being the 'positive' class.
#'
#' @name rtemis-package
#' @import stats methods graphics grDevices S7 data.table htmltools
#' @importFrom utils packageVersion sessionInfo getFromNamespace head tail write.csv write.table zip
#' @importFrom cli cli_abort cli_warn cli_progress_along
"_PACKAGE"

NULL
