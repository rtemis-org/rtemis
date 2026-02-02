# decomp.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Perform Data Decomposition
#'
#' Perform linear or non-linear decomposition of numeric data.
#'
#' @details
#' See [rdocs.rtemis.org/decomp](https://rdocs.rtemis.org/decomp) for detailed documentation.
#'
#' @param x Matrix or data frame: Input data.
#' @param algorithm Character: Decomposition algorithm.
#' @param config DecompositionConfig: Algorithm-specific config.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Decomposition` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' iris_pca <- decomp(exc(iris, "Species"), algorithm = "PCA")
#' }
decomp <- function(x, algorithm = "ICA", config = NULL, verbosity = 1L) {
  # Checks ----
  if (is.null(config)) {
    config <- get_default_decomparams(algorithm)
  }
  check_is_S7(config, DecompositionConfig)

  # Intro ----
  start_time <- intro(verbosity = verbosity)

  # Data ----
  if (verbosity > 0L) {
    summarize_unsupervised(x)
  }

  # Decompose ----
  algorithm <- get_decom_name(algorithm)
  decom_fn <- get_decom_fn(algorithm)
  if (verbosity > 0L) {
    msg0("Decomposing with ", algorithm, "...")
  }
  decom <- do_call(
    fn = decom_fn,
    args = list(x = x, config = config)
  )

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Decomposition(
    algorithm = algorithm,
    config = config,
    decom = decom[["decom"]],
    transformed = decom[["transformed"]]
  )
} # /rtemis::decomp
