# 135_ClusteringMetrics.R
# ::rtemis::
# 2026- EDG rtemis.org

# The clustering metric set, declared once. `clustering_metric_columns()` and
# `clustering_status_columns()` are both generated from `CLUSTERING_MEASURES`,
# so a measure cannot exist as a value without a status or the reverse, and the
# two vocabularies cannot drift.
#
# Why a status beside the value rather than `NA` alone: `NA` conflates a measure
# that was computed and came out undefined, one the result cannot support, one
# nobody asked for, and one whose prerequisites failed. A record exists to
# answer "why is there no number here", and `NA` cannot.

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
ClusteringMetrics <- new_class(
  name = "ClusteringMetrics",
  parent = Metrics,
  properties = list(
    metrics = prop_state(prop_table(
      columns = clustering_metric_columns(),
      nullable = TRUE,
      description = "Clustering metrics, one row."
    )),
    status = prop_state(prop_table(
      columns = clustering_status_columns(),
      nullable = TRUE,
      description = "Why each metric holds the value it does, one row."
    ))
  ),
  constructor = function(..., sample = NULL) {
    values <- list(...)
    measures <- names(CLUSTERING_MEASURES)
    supplied <- values[intersect(names(values), measures)]
    # A measure nobody supplied is unsupported by this result, which is the
    # commonest case: entropy on a hard clustering, noise on an algorithm that
    # labels none. The absent value is a *typed* NA, since the table declares
    # each column's type and a bare logical NA is not one of them.
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
      if (is.null(supplied[[nm]])) "unsupported" else "computed"
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
  # The rule the value/status split creates: `computed` promises a number, and
  # every other status promises none. Mirrored into the published schema by
  # `data-raw/schema_registry.R`, since a validator the schema lacks is a rule
  # only R obeys.
  validator = function(self) {
    if (is.null(self@metrics) || is.null(self@status)) {
      return(NULL)
    }
    for (nm in names(CLUSTERING_MEASURES)) {
      computed <- identical(self@status[[nm]], "computed")
      present <- !is.na(self@metrics[[nm]])
      if (computed && !present) {
        return(paste0(
          "@metrics$",
          nm,
          " must hold a value when its status is 'computed'."
        ))
      }
      if (!computed && present) {
        return(paste0(
          "@metrics$",
          nm,
          " must be NA when its status is not 'computed'."
        ))
      }
    }
    NULL
  }
) # /rtemis::ClusteringMetrics


# %% compute_clustering_metrics ----
#' Metrics for a fitted clustering
#'
#' Every measure here is O(nk) over values already in hand, so `cluster()`
#' computes them inline. Anything quadratic -- a silhouette over a full distance
#' matrix -- would belong behind a function the user calls deliberately, not on
#' every run.
#'
#' @param x `Clustering` object.
#'
#' @return `ClusteringMetrics` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compute_clustering_metrics <- function(x) {
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
  do.call(ClusteringMetrics, c(values, list(sample = "Training")))
} # /rtemis::compute_clustering_metrics
