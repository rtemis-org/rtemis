# cluster.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% cluster ----
#' Perform Clustering
#'
#' Perform clustering on the rows (usually cases) of a dataset.
#'
#' @details
#' See [docs.rtemis.org/r](https://docs.rtemis.org/r/) for detailed documentation.
#'
#' @param x Matrix, data.frame, or `ClusterConfig` object: Data to cluster (rows
#' are cases to be clustered), or a `ClusterConfig` recipe (from
#' [setup_ClusterConfig]) carrying the data path, algorithm config, and output
#' directory.
#' @param algorithm Character: Clustering algorithm. Not needed when `config` is
#' supplied, which names its own; an explicit `algorithm` that disagrees with
#' `config` is an error rather than a mislabeled run.
#' @param config `ClusteringConfig`, optional: Algorithm-specific config from a
#' clustering `setup_*` function. Its `features` selects the columns to cluster
#' on; `NULL` selects every numeric column of `x`, since a clustering backend
#' reads numbers. The returned object's config carries the resolved names.
#' @param outdir Character, optional: Output directory. If not NULL, the returned
#' `Clustering` object is saved there as an `.rds` file, alongside a run record
#' (`cluster_<algorithm>.record.json`) stating what the run resolved. See
#' [write_record].
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Clustering` object.
#'
#' @author EDG
#' @export
#' @examples
#' iris_km <- cluster(exc(iris, "Species"), algorithm = "KMeans")
cluster <- function(
  x,
  algorithm = "KMeans",
  config = NULL,
  outdir = NULL,
  verbosity = 1L
) {
  # ClusterConfig dispatch ----
  if (S7_inherits(x, ClusterConfig)) {
    # `ClusterConfig` is a recipe: `dat_path` may be unbound. Require it at
    # cluster time (the CLI sets it from its data argument before calling).
    if (is.null(x@dat_path)) {
      rtemis.core::abort(
        "This `ClusterConfig` has no `dat_path`; set it before clustering ",
        '(e.g. `x@dat_path <- "data.csv"`).',
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    # The document names the algorithm in one place, its `clustering_config`;
    # with none, the formal default below applies.
    return(cluster(
      x = read(x@dat_path),
      config = x@clustering_config,
      outdir = x@outdir,
      verbosity = x@verbosity
    ))
  } # / cluster.ClusterConfig

  # Checks ----
  # A supplied config names its algorithm; `algorithm` then serves only to catch
  # a caller naming a different one, which would otherwise run under the wrong
  # label with the wrong settings.
  if (is.null(config)) {
    config <- get_default_clusterparams(algorithm)
  } else {
    check_is_S7(config, ClusteringConfig)
    if (!missing(algorithm) && get_clust_name(algorithm) != config@algorithm) {
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

  # Feature selection ----
  # The config's `features` is the record's account of which columns were
  # clustered, so the fit must use exactly those. Unset means every numeric
  # column, resolved here and written back to the config: a clustering backend
  # reads numbers, and handing it a date column because the caller did not
  # enumerate a hundred names is a run that fails on data any other interface
  # clusters without being asked. `train()` resolves its decomposition step the
  # same way, and `decomp()` now does too, so one rule covers all three.
  x <- as.data.frame(x)
  if (is.null(config@features)) {
    config@features <- resolve_unsupervised_features(x, "Clustering")
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

  # Cluster ----
  algorithm <- get_clust_name(algorithm)
  msg0(
    bold(paste0("Clustering with ", algorithm, "...")),
    verbosity = verbosity
  )
  clust <- cluster_(config = config, x = x, verbosity = verbosity)

  # Clusters ----
  clusters <- do_call(
    fn = get_clustpredict_fn(algorithm),
    args = list(clust = clust),
    verbosity = verbosity
  )

  # Soft algorithms carry a weight per cluster per case; the rest do not, and
  # that is what selects the result variant below.
  membership <- cluster_membership(config = config, clust = clust)

  if (!is.null(config[["k"]])) {
    # For algorithms where k is specified in config
    k <- config[["k"]]
  } else {
    # For algorithms where k is not prescribed but determined by the fit. Each
    # algorithm reports its own fitted count: distinct labels answer that for
    # some backends and not others, over-counting a noise sentinel and
    # under-counting a cluster that won no case.
    k <- cluster_k(config = config, clust = clust)
    if (verbosity > 0L) {
      msg0(paste0("Found ", highlight(k), " clusters."))
    }
  }

  # Outro ----
  outro(start_time, verbosity = verbosity)
  # `Clustering` is abstract: every result is one variant or the other, and
  # which one follows from whether the algorithm produced a membership matrix.
  out <- if (is.null(membership)) {
    HardClustering(
      algorithm = algorithm,
      clust = clust,
      k = k,
      clusters = clusters,
      config = config
    )
  } else {
    SoftClustering(
      algorithm = algorithm,
      clust = clust,
      k = k,
      clusters = clusters,
      config = config,
      membership = membership
    )
  }

  # Cheap measures only, computed inline as `decomp()` does. See
  # `compute_clustering_metrics()` on what is deliberately not here.
  out@metrics <- compute_clustering_metrics(out)

  # `cluster()` fits every column it is given, so the input frame is the data
  # the run used. Reduced to a matrix like `decomp()` does, for the same reason
  # and one more: an "object" hash sees the container, so without it the same
  # data clustered as a data.table and decomposed as a data.frame would record
  # two different `data_training` hashes, and comparing runs across a batch is
  # what a fingerprint is for. Lossless because every `cluster_` method has
  # already run `check_unsupervised_data()`, which rejects a non-numeric column;
  # a method that skipped it would let one through to be coerced silently here.
  out@data_fingerprint <- data_fingerprint(as.matrix(x))

  # See `decomp()`: the run's input, which `@config` alone cannot supply.
  # `outdir` is omitted when unset so the config's own default applies; passing
  # NULL is rejected, and a record reporting the default with origin `default`
  # is the honest reading of "the caller did not choose one".
  input_args <- list(
    clustering_config = config,
    verbosity = max(0L, verbosity)
  )
  if (!is.null(outdir)) {
    input_args[["outdir"]] <- outdir
  }
  out@cluster_config <- do.call(setup_ClusterConfig, input_args)

  # Write ----
  if (!is.null(outdir)) {
    rt_save(
      out,
      outdir = outdir,
      file_prefix = paste0("cluster_", algorithm),
      verbosity = verbosity
    )
    write_record(
      out,
      file.path(outdir, paste0("cluster_", algorithm, ".record.json")),
      overwrite = TRUE,
      verbosity = verbosity
    )
  }
  out
} # /rtemis::cluster
