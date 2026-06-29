# decomp.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% decomp ----
#' Perform Data Decomposition
#'
#' Perform linear or non-linear decomposition of numeric data.
#'
#' @details
#' See [docs.rtemis.org/r](https://docs.rtemis.org/r/) for detailed documentation.
#'
#' @param x Matrix, data frame, or `DecomposeConfig` object: Input data, or a
#' `DecomposeConfig` recipe (from [setup_DecomposeConfig]) carrying the data
#' path, algorithm config, and output directory.
#' @param algorithm Character: Decomposition algorithm.
#' @param config DecompositionConfig: Algorithm-specific config.
#' @param outdir Character, optional: Output directory. If not NULL, the returned
#' `Decomposition` object is saved there as an `.rds` file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Decomposition` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' iris_pca <- decomp(exc(iris, "Species"), algorithm = "PCA")
decomp <- function(
  x,
  algorithm = "ICA",
  config = NULL,
  outdir = NULL,
  verbosity = 1L
) {
  # DecomposeConfig dispatch ----
  if (S7_inherits(x, DecomposeConfig)) {
    # `DecomposeConfig` is a recipe: `dat_path` may be unbound. Require it at
    # decomp time (the CLI sets it from its data argument before calling).
    if (is.null(x@dat_path)) {
      rtemis.core::abort(
        "This `DecomposeConfig` has no `dat_path`; set it before decomposing ",
        '(e.g. `config@dat_path <- "data.csv"`).',
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    # The algorithm label prefers an explicit top-level `algorithm`, falling back
    # to the one carried by `decomposition_config`, then the formal default.
    algorithm <- x@algorithm
    if (is.null(algorithm) && !is.null(x@decomposition_config)) {
      algorithm <- x@decomposition_config@algorithm
    }
    if (is.null(algorithm)) {
      algorithm <- "ICA"
    }
    return(decomp(
      x = read(x@dat_path),
      algorithm = algorithm,
      config = x@decomposition_config,
      outdir = x@outdir,
      verbosity = x@verbosity
    ))
  } # / decomp.DecomposeConfig

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
  msg0("Decomposing with ", algorithm, "...", verbosity = verbosity)

  # decomp_ -> list with elements 'decom' and 'transformed'
  decom <- decomp_(config = config, x = x, verbosity = verbosity)

  # Outro ----
  outro(start_time, verbosity = verbosity)
  out <- Decomposition(
    algorithm = algorithm,
    config = config,
    decom = decom[["decom"]],
    transformed = decom[["transformed"]]
  )

  # Write ----
  if (!is.null(outdir)) {
    rt_save(
      out,
      outdir = outdir,
      file_prefix = paste0("decomp_", algorithm),
      verbosity = verbosity
    )
  }
  out
} # /rtemis::decomp
