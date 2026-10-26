# 135_ClusteringMetrics.R
# ::rtemis::
# 2026- EDG rtemis.org

# The clustering metric set, declared once. `clustering_metric_columns()` and
# `clustering_status_columns()` are both generated from `CLUSTERING_MEASURES`,
# so every measure has both a value column and a status column.
#
# A status sits beside the value because `NA` alone conflates a measure that
# was computed and came out undefined, one the result cannot support, one
# not requested, and one whose prerequisites failed. The status records
# which of these applies.

# %% CLUSTERING_MEASURE_STATUS ----
# Five states, because there are four distinct reasons for absence and one for
# presence. `undefined` is not a catch-all for bugs: an unexpected computation
# error is a failure, not a status.
CLUSTERING_MEASURE_STATUS <- c(
  "computed",
  "unsupported",
  "not_applicable",
  "not_requested",
  "undefined"
)


# %% CLUSTERING_MEASURES ----
# One entry per measure: how the value column is typed, and what it means.
CLUSTERING_MEASURES <- list(
  n_cases = list(
    kind = "count",
    description = "Number of cases the metrics describe."
  ),
  n_clusters = list(
    kind = "count",
    description = paste0(
      "Number of fitted clusters, excluding noise and including any cluster ",
      "that no case was assigned to."
    )
  ),
  noise_fraction = list(
    kind = "rate",
    description = paste0(
      "Fraction of cases left unassigned by an algorithm that labels noise. ",
      "Zero where every case is assigned."
    )
  ),
  mean_assignment_uncertainty = list(
    kind = "rate",
    description = paste0(
      "Mean of one minus each case's largest membership. Zero when every case ",
      "belongs wholly to one cluster; higher where cases sit between clusters."
    )
  ),
  mean_assignment_entropy = list(
    kind = "metric",
    min = 0,
    description = paste0(
      "Mean Shannon entropy of the per-case membership distribution, in nats, ",
      "taking zero times its log as zero. Zero when every assignment is ",
      "certain. A descriptive measure of concentration: a fuzzy membership is ",
      "not a calibrated probability, so values are not comparable across ",
      "models fitted under different assumptions."
    )
  ),
  mean_silhouette = list(
    kind = "metric",
    min = -1,
    max = 1,
    description = paste0(
      "Mean silhouette width over the cases not labeled noise, from the ",
      "Euclidean distances between cases over the clustered features. A ",
      "case's width is (b - a) / max(a, b), with a its mean distance to the ",
      "other cases of its cluster and b its smallest mean distance to the ",
      "cases of another cluster; a case alone in its cluster has width zero. ",
      "Higher values mean cases lie closer to their own cluster than to the ",
      "nearest other one. Its cost is quadratic in the number of cases, so it ",
      "is computed on request."
    )
  )
)


# %% clustering_metric_columns ----
# The value columns, from the shared declaration. Every column is nullable and
# declared for every algorithm: one that cannot be supported is null with a
# status saying so, never an absent column.
clustering_metric_columns <- function() {
  lapply(CLUSTERING_MEASURES, function(m) {
    switch(
      m[["kind"]],
      count = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = m[["description"]]
      ),
      rate = prop_metric(min = 0, max = 1, description = m[["description"]]),
      metric = prop_metric(
        min = m[["min"]],
        max = m[["max"]],
        description = m[["description"]]
      )
    )
  })
} # /rtemis::clustering_metric_columns


# %% clustering_status_columns ----
# One enum per measure, from the same declaration.
clustering_status_columns <- function() {
  lapply(names(CLUSTERING_MEASURES), function(nm) {
    prop_string(
      "unsupported",
      enum = CLUSTERING_MEASURE_STATUS,
      description = paste0("Why `", nm, "` holds the value it does.")
    )
  }) |>
    stats::setNames(names(CLUSTERING_MEASURES))
} # /rtemis::clustering_status_columns


# %% ClusteringMetrics ----
#' ClusteringMetrics
#'
#' @description
#' Metrics for one clustering run: a single-row table of values and a matching
#' row of statuses saying, for each measure, why it holds what it holds.
#'
#' @field metrics Table: One row of measure values.
#' @field status Table: One row of measure statuses.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClusteringMetrics <- schema_class(
  name = "ClusteringMetrics",
  parent = Metrics,
  properties = list(
    metrics = prop_state(prop_table(
      columns = clustering_metric_columns(),
      nullable = TRUE,
      min_items = 1L,
      max_items = 1L,
      description = "Clustering metrics, one row."
    )),
    status = prop_state(prop_table(
      columns = clustering_status_columns(),
      nullable = TRUE,
      min_items = 1L,
      max_items = 1L,
      description = "Why each metric holds the value it does, one row."
    ))
  ),
  constructor = function(..., status = NULL, sample = NULL) {
    values <- list(...)
    measures <- names(CLUSTERING_MEASURES)
    supplied <- values[intersect(names(values), measures)]
    # `status` states why a measure not supplied is absent. A named measure
    # must be one not supplied, and its status one of the absence states:
    # `computed` follows from supplying the value.
    if (!is.null(status)) {
      bad_names <- setdiff(names(status), setdiff(measures, names(supplied)))
      bad_states <- setdiff(
        unlist(status),
        setdiff(CLUSTERING_MEASURE_STATUS, "computed")
      )
      if (length(bad_names) > 0L || length(bad_states) > 0L) {
        rtemis.core::abort(
          "`status` must name measures not supplied, with one of ",
          paste(
            setdiff(CLUSTERING_MEASURE_STATUS, "computed"),
            collapse = ", "
          ),
          ".",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    # A measure not supplied and not named in `status` is unsupported by this
    # result, which is the commonest case: entropy on a hard clustering, noise
    # on an algorithm that labels none. The absent value is a *typed* NA, since
    # the table declares each column's type and a bare logical NA is not one of
    # them.
    metrics <- lapply(measures, function(nm) {
      if (!is.null(supplied[[nm]])) {
        return(supplied[[nm]])
      }
      if (identical(CLUSTERING_MEASURES[[nm]][["kind"]], "count")) {
        NA_integer_
      } else {
        NA_real_
      }
    })
    status <- lapply(measures, function(nm) {
      if (!is.null(supplied[[nm]])) {
        "computed"
      } else {
        status[[nm]] %||% "unsupported"
      }
    })
    new_object(
      Metrics(sample = sample),
      metrics = as.data.frame(
        stats::setNames(metrics, measures),
        stringsAsFactors = FALSE
      ),
      status = as.data.frame(
        stats::setNames(status, measures),
        stringsAsFactors = FALSE
      )
    )
  },
  rules = list(StatusValueRule(
    id = "clustering-metrics.status-value",
    values = "metrics",
    statuses = "status",
    computed_status = "computed",
    message = "must hold a value when its status is 'computed', and must be null for every other status."
  )),
  publication = SchemaPublication(
    role = "document",
    slug = "clusteringmetrics",
    title = "rtemis ClusteringMetrics",
    description = "Clustering metrics: a single-row table of measure values beside a row of statuses saying why each holds what it does. Every measure is declared for every algorithm, so one an algorithm cannot support is null with a status of \"unsupported\" rather than an absent column; null alone does not distinguish a measure that was computed and came out undefined from one not requested.",
    order = 9L,
    kind = "report"
  )
) # /rtemis::ClusteringMetrics


# %% repr.ClusteringMetrics ----
method(repr, ClusteringMetrics) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  title <- if (is.null(x@sample)) {
    "Clustering Metrics"
  } else {
    paste(x@sample, "Clustering Metrics")
  }
  out <- repr_S7name(title, pad = pad, output_type = output_type)
  status <- unlist(x@status)
  computed <- names(status)[status == "computed"]
  # The values a run computed, then each absent measure with the reason the
  # status table gives for it.
  out <- paste0(
    out,
    repr_ls(
      label_metric_df(x@metrics[, computed, drop = FALSE]),
      print_class = FALSE,
      print_df = TRUE,
      pad = pad,
      output_type = output_type
    )
  )
  absent <- status[status != "computed"]
  if (length(absent) > 0L) {
    out <- paste0(
      out,
      strrep(" ", pad),
      "Not computed: ",
      paste0(names(absent), " (", absent, ")", collapse = ", "),
      "\n"
    )
  }
  out
} # /rtemis::repr.ClusteringMetrics


# %% print.ClusteringMetrics ----
method(print, ClusteringMetrics) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.ClusteringMetrics

# %% compute_clustering_metrics ----
#' Metrics for a fitted clustering
#'
#' Every measure computed here without `data` is O(nk) over values already in
#' hand, so `cluster()` computes them inline and reports the mean silhouette as
#' `not_requested`. `clustering_metrics()` passes the data, which adds the
#' mean silhouette.
#'
#' @param x `Clustering` object.
#' @param data Optional data.frame: The clustered features of the data `x` was
#' fitted on, one row per case of `x@clusters`.
#'
#' @return `ClusteringMetrics` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compute_clustering_metrics <- function(x, data = NULL) {
  clusters <- x@clusters
  values <- list(
    n_cases = as.integer(length(clusters)),
    # Copied from `@k`, never recounted: the two must not be able to disagree,
    # and a fitted cluster that won no case is still a cluster.
    n_clusters = x@k,
    noise_fraction = mean(clusters == 0L)
  )
  if (S7_inherits(x, SoftClustering)) {
    m <- x@membership
    values[["mean_assignment_uncertainty"]] <- mean(1 - apply(m, 1L, max))
    # 0 log 0 = 0, which is the limit and the convention.
    values[["mean_assignment_entropy"]] <- mean(rowSums(
      ifelse(m > 0, -m * log(m), 0)
    ))
  }
  status <- list()
  if (is.null(data)) {
    status[["mean_silhouette"]] <- "not_requested"
  } else {
    width <- mean_silhouette(as.matrix(data), clusters)
    if (is.na(width)) {
      status[["mean_silhouette"]] <- "not_applicable"
    } else {
      values[["mean_silhouette"]] <- width
    }
  }
  do.call(
    ClusteringMetrics,
    c(values, list(status = status, sample = "Training"))
  )
} # /rtemis::compute_clustering_metrics


# %% mean_silhouette ----
#' Mean silhouette width
#'
#' Rousseeuw's silhouette width, averaged over the cases not labeled noise
#' (label 0), from Euclidean distances. Noise cases are excluded both as
#' cases and as members of other clusters. A case alone in its cluster has
#' width zero, and a case with `a = b = 0` has width zero.
#'
#' @param xm Numeric matrix: Cases by clustered features.
#' @param labels Integer vector: Cluster label per case, 0 for noise.
#'
#' @return Numeric scalar, or `NA_real_` when fewer than two clusters hold an
#' evaluated case.
#'
#' @references
#' Rousseeuw PJ (1987). Silhouettes: a graphical aid to the interpretation and
#' validation of cluster analysis. Journal of Computational and Applied
#' Mathematics, 20, 53-65.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mean_silhouette <- function(xm, labels) {
  keep <- labels != 0L
  xm <- xm[keep, , drop = FALSE]
  labels <- labels[keep]
  groups <- sort(unique(labels))
  if (length(groups) < 2L) {
    return(NA_real_)
  }
  d <- as.matrix(stats::dist(xm, method = "euclidean"))
  member <- match(labels, groups)
  sizes <- tabulate(member, nbins = length(groups))
  # Sum of distances from each case to the cases of each cluster.
  indicator <- matrix(0, nrow = length(member), ncol = length(groups))
  indicator[cbind(seq_along(member), member)] <- 1
  totals <- d %*% indicator
  own <- cbind(seq_along(member), member)
  a <- totals[own] / pmax(sizes[member] - 1L, 1L)
  means <- sweep(totals, 2L, sizes, "/")
  means[own] <- Inf
  b <- apply(means, 1L, min)
  width <- ifelse(pmax(a, b) == 0, 0, (b - a) / pmax(a, b))
  width[sizes[member] == 1L] <- 0
  mean(width)
} # /rtemis::mean_silhouette


# %% clustering_metrics ----
#' Clustering Metrics
#'
#' Quantify a fitted clustering on the data it was fitted on: how many cases
#' and clusters, the fraction labeled noise, how concentrated the memberships
#' of a soft clustering are, and how well separated its clusters are.
#'
#' @details
#' `cluster()` stores the measures that cost no more than one pass over the
#' assignments on its result, where [get_metrics()] returns them, and reports
#' the mean silhouette as not requested. This function computes the same
#' measures and the mean silhouette, whose cost is quadratic in the number of
#' cases. The silhouette uses Euclidean distances over the clustered features,
#' `clust@config@features`, and excludes cases labeled noise (label 0).
#'
#' Each measure carries a status: `computed`, `unsupported` (the clustering
#' does not have what the measure needs, such as a membership matrix), or
#' `not_applicable` (the silhouette when fewer than two clusters hold a case).
#'
#' `x` must be the data `clust` was fitted on, with one row per case, since the
#' measures pair each row with its fitted assignment. Data whose fingerprint
#' differs from the one `cluster()` recorded raises an error.
#'
#' @param clust `Clustering` object.
#' @param x Tabular data (data.frame, data.table, or tibble): The data `clust`
#' was fitted on.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `ClusteringMetrics` object.
#'
#' @author EDG
#' @export
#' @examplesIf requireNamespace("flexclust", quietly = TRUE)
#' x <- exc(iris, "Species")
#' iris_km <- cluster(x, algorithm = "KMeans", verbosity = 0L)
#' clustering_metrics(iris_km, x, verbosity = 0L)
clustering_metrics <- function(clust, x, verbosity = 1L) {
  check_is_S7(clust, Clustering)
  x <- as.data.frame(x)
  features <- clust@config@features
  missing_cols <- setdiff(features, names(x))
  if (length(missing_cols) > 0L) {
    rtemis.core::abort(
      "`x` is missing ",
      length(missing_cols),
      " column(s) the clustering was fitted on.\n",
      "Missing: ",
      paste(missing_cols, collapse = ", "),
      ".",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  if (!is.null(features)) {
    x <- x[, features, drop = FALSE]
  }
  if (NROW(x) != length(clust@clusters)) {
    rtemis.core::abort(
      "`x` has ",
      NROW(x),
      " rows, and the clustering was fitted on ",
      length(clust@clusters),
      " cases. Pass the data the clustering was fitted on.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  check_unsupervised_data(x, allow_missing = FALSE, verbosity = verbosity)
  check_clustering_data(clust, x)
  compute_clustering_metrics(clust, data = x)
} # /rtemis::clustering_metrics


# %% check_clustering_data ----
#' Require the data a clustering was fitted on
#'
#' Compares `x` with `clust@data_fingerprint`, recomputed by the reference's
#' own method, as `cluster()` fingerprinted it: the clustered features as a
#' matrix. Pairing fitted assignments with other data would describe neither,
#' so a difference raises an error.
#'
#' @param clust `Clustering` object.
#' @param x data.frame: The clustered features.
#'
#' @return NULL, invisibly. Called for the check.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_clustering_data <- function(clust, x) {
  reference <- clust@data_fingerprint
  # "file" hashes bytes on disk, which an in-memory frame cannot reproduce.
  if (is.null(reference) || reference@method == "file") {
    return(invisible(NULL))
  }
  difference <- fingerprint_diff(
    reference,
    data_fingerprint(
      as.matrix(x),
      method = reference@method,
      algorithm = reference@algorithm
    )
  )
  if (!is.null(difference)) {
    rtemis.core::abort(
      "`x` is not the data this clustering was fitted on: ",
      difference,
      ". The metrics pair each case with its fitted assignment, so they ",
      "require the fitted data.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  invisible(NULL)
} # /rtemis::check_clustering_data
