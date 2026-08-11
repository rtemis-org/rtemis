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
#' @param config DecompositionConfig: Algorithm-specific config. Its `features`
#' selects the columns of `x` to decompose; `NULL` decomposes all of them.
#' @param outdir Character, optional: Output directory. If not NULL, the returned
#' `Decomposition` object is saved there as an `.rds` file, alongside a run
#' record (`decomp_<algorithm>.record.json`) stating what the run resolved. See
#' [write_record].
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Decomposition` object.
#'
#' @author EDG
#' @export
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
        '(e.g. `x@dat_path <- "data.csv"`).',
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

  # Feature selection ----
  # `apply_decomp()` subsets new data by `config@features`, so the fit must use
  # exactly those columns or the replay transforms a different matrix.
  # `x` is features-only here: there is no outcome column to exclude.
  if (!is.null(config@features)) {
    x <- as.data.frame(x)
    check_data_bounds(config, x, has_outcome = FALSE)
    x <- x[, config@features, drop = FALSE]
  }

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
  decom <- decomp_(config = config, x = x, verbosity = verbosity - 1L)

  # Outro ----
  outro(start_time, verbosity = verbosity)
  out <- Decomposition(
    algorithm = algorithm,
    config = config,
    decom = decom[["decom"]],
    transformed = decom[["transformed"]]
  )

  # Data identity ----
  # Fingerprinted through `decomp_matrix()` rather than from `x` directly, so
  # that a later `decomp_metrics()` call reducing the caller's frame the same way
  # arrives at the same hash. Feeds the record's provenance block.
  out@data_fingerprint <- data_fingerprint(decomp_matrix(out, x))

  # Metrics ----
  # The set an algorithm's traits support, on the data just fitted. Bounded by
  # the cost of one reconstruction, O(n * p * k), so it does not change this
  # function's complexity. Out-of-sample metrics need a second data matrix and
  # are `decomp_metrics()`'s job.
  out@metrics <- compute_decomposition_metrics(
    decom = out,
    x = x,
    verbosity = verbosity
  )

  # The run's input recipe, so a record can say what was asked for. `dat_path`
  # stays unset for an in-memory call -- data identity is provenance's job.
  # `outdir` is omitted when unset so the config's own default applies; passing
  # NULL is rejected, and a record reporting the default with origin `default`
  # is the honest reading of "the caller did not choose one".
  input_args <- list(
    algorithm = algorithm,
    decomposition_config = config,
    verbosity = max(0L, verbosity)
  )
  if (!is.null(outdir)) {
    input_args[["outdir"]] <- outdir
  }
  out@decompose_config <- do.call(setup_DecomposeConfig, input_args)

  # Write ----
  if (!is.null(outdir)) {
    rt_save(
      out,
      outdir = outdir,
      file_prefix = paste0("decomp_", algorithm),
      verbosity = verbosity
    )
    write_record(
      out,
      file.path(outdir, paste0("decomp_", algorithm, ".record.json")),
      overwrite = TRUE,
      verbosity = verbosity
    )
  }
  out
} # /rtemis::decomp
