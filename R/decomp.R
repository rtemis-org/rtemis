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
#' @param algorithm Character: Decomposition algorithm. Not needed when `config`
#' is supplied, which names its own; an explicit `algorithm` that disagrees
#' with `config` is an error rather than a mislabeled run.
#' @param config DecompositionConfig: Algorithm-specific config. Its `features`
#' selects the columns of `x` to decompose; `NULL` selects every numeric column,
#' since a decomposition reads a numeric matrix. The returned object's config
#' carries the resolved names, and `apply_decomp()` replays that selection.
#' @param execution_config `ExecutionConfig` object: Execution settings, e.g.
#' [setup_FutureExecution] or [setup_SerialExecution]. A decomposition dispatches
#' no work to other processes: an algorithm whose `threaded` trait is TRUE (see
#' [decomposition_traits]) runs on `n_workers_algorithm` threads, or on the
#' config's worker count when that is unset. The config's `seed` seeds the fit.
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
  execution_config = setup_FutureExecution(),
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
    # The document names the algorithm in one place, its `decomposition_config`;
    # with none, the formal default below applies.
    return(decomp(
      x = read(x@dat_path),
      config = x@decomposition_config,
      execution_config = x@execution_config,
      outdir = x@outdir,
      verbosity = x@verbosity
    ))
  } # / decomp.DecomposeConfig

  # Checks ----
  # A supplied config names its algorithm; `algorithm` then serves only to catch
  # a caller naming a different one, which would otherwise run under the wrong
  # label with the wrong settings.
  if (is.null(config)) {
    config <- get_default_decomparams(algorithm)
  } else {
    check_is_S7(config, DecompositionConfig)
    if (!missing(algorithm) && get_decom_name(algorithm) != config@algorithm) {
      rtemis.core::abort(
        "`algorithm` is \"",
        algorithm,
        "\" but `config` is a ",
        config@algorithm,
        " config; pass one or the other.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    algorithm <- config@algorithm
  }
  check_is_S7(execution_config, ExecutionConfig)

  # Feature selection ----
  # `apply_decomp()` subsets new data by `config@features`, so the fit must use
  # exactly those columns or the replay transforms a different matrix.
  # `x` is features-only here: there is no outcome column to exclude. Unset
  # means every numeric column, resolved here and written back so the fit, the
  # record and the replay name the same columns -- what `train()` already does
  # for the decomposition step it runs.
  x <- as.data.frame(x)
  if (is.null(config@features)) {
    config@features <- resolve_unsupervised_features(x, "Decomposition")
  }
  # Against the frame as supplied, so that `features` is checked against every
  # column the caller has and a per-case setting against every row. Both
  # paths, because a bound that is not about the feature selection -- CMeans'
  # per-case `weights` -- is wrong just as often when the selection was left
  # to us.
  check_data_bounds(config, x, has_outcome = FALSE)
  x <- x[, config@features, drop = FALSE]

  # Intro ----
  start_time <- intro(verbosity = verbosity)

  # Data ----
  if (verbosity > 0L) {
    summarize_unsupervised(x)
  }

  # Resources ----
  # Nothing is dispatched, so the only level is the algorithm's: the threads a
  # threaded backend runs on. The named share wins; otherwise the whole worker
  # count, which is 1 under a CRAN check.
  algorithm <- get_decom_name(algorithm)
  n_workers <- execution_n_workers(execution_config)
  threaded <- decomposition_traits(algorithm)[["threaded"]]
  n_threads <- if (!threaded) {
    1L
  } else {
    execution_config@n_workers_algorithm %||% n_workers
  }
  msg_resources(
    backend = execution_backend_label(execution_config),
    n_workers = n_workers,
    workers = list(algorithm = n_threads),
    explicit = threaded && !is.null(execution_config@n_workers_algorithm),
    device = "CPU",
    verbosity = verbosity
  )

  # Decompose ----
  msg0("Decomposing with ", algorithm, "...", verbosity = verbosity)

  # decomp_ -> list with elements 'decom' and 'transformed'. Seeded from the
  # execution config, which records the seed, so the fit reproduces from its
  # record; the caller's random stream is restored afterwards.
  decom <- with_seed(
    execution_config@seed,
    decomp_(
      config = config,
      x = x,
      n_threads = n_threads,
      verbosity = verbosity - 1L
    )
  )

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
    decomposition_config = config,
    execution_config = execution_config,
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
