# test_Clustering.R
# ::rtemis::
# 2025- EDG rtemis.org

# Data ----
x <- iris[, -5]

# setup_KMeans ----
test_that("setup_KMeans() succeeds", {
  expect_s7_class(setup_KMeans(), KMeansConfig)
})

# setup_KMeans throws error ----
test_that("setup_KMeans() throws error with bad values or wrong types", {
  expect_error(setup_KMeans(k = -1L))
  expect_error(setup_KMeans(dist = "foo"))
})

# cluster KMeans ----
test_that("cluster_KMeans() succeeds", {
  iris_kmeans <- cluster(
    x,
    algorithm = "kmeans",
    config = setup_KMeans(k = 3L)
  )
  expect_s7_class(iris_kmeans, Clustering)
})

# cluster KMeans with k = 10 ----
test_that("cluster_KMeans() with k = 10 succeeds", {
  skip_if_not_installed("flexclust")
  iris_kmeans10 <- cluster(
    x,
    algorithm = "kmeans",
    config = setup_KMeans(k = 10L)
  )
  expect_s7_class(iris_kmeans10, Clustering)
})

# setup_HardCL ----
test_that("setup_HardCL() succeeds", {
  expect_s7_class(setup_HardCL(), HardCLConfig)
})

# cluster HardCL ----
test_that("cluster_HardCL() succeeds", {
  skip_if_not_installed("flexclust")
  iris_hardcl <- cluster(
    x,
    algorithm = "HardCL",
    config = setup_HardCL(k = 3L)
  )
  expect_s7_class(iris_hardcl, Clustering)
})

# setup_NeuralGas ----
test_that("setup_NeuralGas() succeeds", {
  expect_s7_class(setup_NeuralGas(), NeuralGasConfig)
})

# cluster NeuralGas ----
test_that("cluster_NeuralGas() succeeds", {
  skip_if_not_installed("flexclust")
  iris_neuralgas <- cluster(
    x,
    algorithm = "NeuralGas",
    config = setup_NeuralGas(k = 3L)
  )
  expect_s7_class(iris_neuralgas, Clustering)
})

# setup_CMeans ----
test_that("setup_CMeans() succeeds", {
  expect_s7_class(setup_CMeans(), CMeansConfig)
})

# cluster CMeans ----
test_that("cluster_CMeans() succeeds", {
  skip_if_not_installed("e1071")
  iris_cmeans <- cluster(
    x,
    algorithm = "CMeans",
    config = setup_CMeans(k = 3L)
  )
  expect_s7_class(iris_cmeans, Clustering)
})

# setup_DBSCAN ----
test_that("setup_DBSCAN() succeeds", {
  expect_s7_class(setup_DBSCAN(), DBSCANConfig)
})

# cluster DBSCAN ----
test_that("cluster_DBSCAN() succeeds", {
  skip_if_not_installed("dbscan")
  iris_dbscan <- cluster(
    x,
    algorithm = "DBSCAN",
    config = setup_DBSCAN(eps = 0.5, min_points = 5L)
  )
  expect_s7_class(iris_dbscan, Clustering)
})

# DBSCAN noise is not a cluster ----
test_that("cluster() does not count DBSCAN noise as a cluster", {
  skip_if_not_installed("dbscan")
  # `eps` small enough that most cases are noise (label 0) but some clusters
  # remain: `@k` must count the clusters, not the noise label.
  partial <- cluster(
    x,
    algorithm = "DBSCAN",
    config = setup_DBSCAN(eps = 0.3, min_points = 5L),
    verbosity = 0L
  )
  expect_true(0L %in% partial@clusters)
  expect_identical(partial@k, length(setdiff(unique(partial@clusters), 0L)))
  expect_false(partial@k == length(unique(partial@clusters)))

  # Nothing dense enough to cluster: zero clusters, not one.
  all_noise <- cluster(
    x,
    algorithm = "DBSCAN",
    config = setup_DBSCAN(eps = 0.01, min_points = 50L),
    verbosity = 0L
  )
  expect_identical(unique(all_noise@clusters), 0L)
  expect_identical(all_noise@k, 0L)
})

# setup_HOPACH ----
test_that("setup_HOPACH() succeeds", {
  expect_s7_class(setup_HOPACH(), HOPACHConfig)
})

# setup_HOPACH throws error ----
test_that("setup_HOPACH() throws error with bad values or wrong types", {
  expect_error(setup_HOPACH(dist = "foo"))
  expect_error(setup_HOPACH(max_levels = 0L))
  expect_error(setup_HOPACH(min_improvement = 2))
})

# cluster HOPACH ----
test_that("cluster_HOPACH() succeeds", {
  skip_if_not_installed("hopach")
  iris_hopach <- cluster(
    x,
    algorithm = "HOPACH",
    config = setup_HOPACH(dist = "euclid", max_levels = 3L, max_children = 5L),
    verbosity = 0L
  )
  expect_s7_class(iris_hopach, Clustering)
  # HOPACH discovers k rather than taking one, so `@k` must be what the run
  # found and not a configured bound.
  expect_identical(iris_hopach@k, length(unique(iris_hopach@clusters)))
  # HOPACH's own labels are level-path codes, remapped to 1:k integers.
  expect_type(iris_hopach@clusters, "integer")
  expect_length(iris_hopach@clusters, nrow(x))
  expect_identical(
    sort(unique(iris_hopach@clusters)),
    seq_len(iris_hopach@k)
  )
})

# HOPACH refuses new data ----
test_that("clustpredict_HOPACH() refuses newdata", {
  skip_if_not_installed("hopach")
  iris_hopach <- cluster(
    x,
    algorithm = "HOPACH",
    config = setup_HOPACH(dist = "euclid", max_levels = 3L, max_children = 5L),
    verbosity = 0L
  )
  expect_error(
    clustpredict_HOPACH(iris_hopach@clust, newdata = x),
    class = "rtemis_unsupported_error"
  )
})

# setup_PAM ----
test_that("setup_PAM() succeeds", {
  expect_s7_class(setup_PAM(), PAMConfig)
})

# setup_PAM throws error ----
test_that("setup_PAM() throws error with bad values or wrong types", {
  expect_error(setup_PAM(k = -1L))
  expect_error(setup_PAM(dist = "foo"))
})

# cluster PAM ----
test_that("cluster_PAM() succeeds", {
  skip_if_not_installed("cluster")
  iris_pam <- cluster(
    x,
    algorithm = "PAM",
    config = setup_PAM(k = 3L),
    verbosity = 0L
  )
  expect_s7_class(iris_pam, Clustering)
  # PAM prescribes k, so `@k` is what was configured.
  expect_identical(iris_pam@k, 3L)
  expect_type(iris_pam@clusters, "integer")
  expect_length(iris_pam@clusters, nrow(x))
  expect_identical(sort(unique(iris_pam@clusters)), seq_len(3L))
})

# setup_PAMK ----
test_that("setup_PAMK() succeeds", {
  expect_s7_class(setup_PAMK(), PAMKConfig)
})

# cluster PAMK ----
test_that("cluster_PAMK() succeeds", {
  skip_if_not_installed("fpc")
  iris_pamk <- cluster(
    x,
    algorithm = "PAMK",
    config = setup_PAMK(krange = 2:5),
    verbosity = 0L
  )
  expect_s7_class(iris_pamk, Clustering)
  # PAMK discovers k, so `@k` must be what the search chose and not a bound.
  expect_identical(iris_pamk@k, length(unique(iris_pamk@clusters)))
  expect_true(iris_pamk@k %in% 2:5)
  expect_type(iris_pamk@clusters, "integer")
  expect_length(iris_pamk@clusters, nrow(x))
  expect_identical(
    sort(unique(iris_pamk@clusters)),
    seq_len(iris_pamk@k)
  )
})

# PAMK via CLARA returns a different fit class ----
test_that("cluster_PAMK() accepts the CLARA fit that use_pam = FALSE produces", {
  skip_if_not_installed("fpc")
  iris_pamk <- cluster(
    x,
    algorithm = "PAMK",
    config = setup_PAMK(krange = 2:4, use_pam = FALSE),
    verbosity = 0L
  )
  expect_s7_class(iris_pamk, Clustering)
  expect_s3_class(iris_pamk@clust[["pamobject"]], "clara")
  expect_identical(iris_pamk@k, length(unique(iris_pamk@clusters)))
})

# PAM family refuses new data ----
test_that("clustpredict_PAM() and clustpredict_PAMK() refuse newdata", {
  skip_if_not_installed("fpc")
  iris_pam <- cluster(
    x,
    algorithm = "PAM",
    config = setup_PAM(k = 3L),
    verbosity = 0L
  )
  iris_pamk <- cluster(
    x,
    algorithm = "PAMK",
    config = setup_PAMK(krange = 2:4),
    verbosity = 0L
  )
  expect_error(
    clustpredict_PAM(iris_pam@clust, newdata = x),
    class = "rtemis_unsupported_error"
  )
  expect_error(
    clustpredict_PAMK(iris_pamk@clust, newdata = x),
    class = "rtemis_unsupported_error"
  )
})


# setup_GMM ----
test_that("setup_GMM() succeeds", {
  expect_s7_class(setup_GMM(), GMMConfig)
  expect_null(setup_GMM()[["k"]])
})

# cluster GMM ----
test_that("cluster_GMM() is soft, and k is fixed or selected", {
  skip_if_not_installed("mclust")
  fixed <- cluster(
    x,
    algorithm = "GMM",
    config = setup_GMM(k = 3L),
    verbosity = 0L
  )
  expect_s7_class(fixed, SoftClustering)
  expect_identical(fixed@k, 3L)
  expect_identical(dim(fixed@membership), c(nrow(x), 3L))
  # `k` unset: BIC selects it, and `@k` is the fitted component count.
  selected <- cluster(
    x,
    algorithm = "GMM",
    config = setup_GMM(),
    verbosity = 0L
  )
  expect_identical(selected@k, ncol(selected@membership))
})


# %% The hard/soft variant pair ----

test_that("Clustering is abstract; every result is one of the two variants", {
  expect_error(Clustering())
  km <- cluster(
    x,
    algorithm = "KMeans",
    config = setup_KMeans(k = 3L),
    verbosity = 0L
  )
  expect_s7_class(km, HardClustering)
  expect_s7_class(km, Clustering)
  expect_false(S7_inherits(km, SoftClustering))
})

test_that("CMeans yields a SoftClustering carrying its membership matrix", {
  skip_if_not_installed("e1071")
  cl <- cluster(
    x,
    algorithm = "CMeans",
    config = setup_CMeans(k = 3L),
    verbosity = 0L
  )
  expect_s7_class(cl, SoftClustering)
  m <- cl@membership
  expect_true(is.matrix(m) && is.numeric(m))
  expect_identical(dim(m), c(nrow(x), 3L))
  expect_true(all(abs(rowSums(m) - 1) <= sqrt(.Machine$double.eps)))
  # The hard labels are the argmax of the matrix, which is the correspondence
  # `@membership` column j <-> cluster j rests on.
  expect_identical(
    as.integer(max.col(m, ties.method = "first")),
    as.integer(cl@clusters)
  )
})


# %% SoftClustering invariants ----

test_that("SoftClustering rejects a permuted membership matrix", {
  skip_if_not_installed("e1071")
  cl <- cluster(
    x,
    algorithm = "CMeans",
    config = setup_CMeans(k = 3L),
    verbosity = 0L
  )
  build <- function(m, k = cl@k) {
    SoftClustering(
      algorithm = "CMeans",
      clust = cl@clust,
      k = k,
      clusters = cl@clusters,
      config = cl@config,
      membership = m
    )
  }
  m <- cl@membership
  expect_s7_class(build(m), SoftClustering)
  # A permutation preserves every dimension, so only assignment consistency
  # catches it.
  expect_error(build(m[, c(2L, 1L, 3L)]))
  expect_error(build(m * 2)) # values out of [0, 1]
  expect_error(build(m / 2)) # rows no longer sum to 1
  expect_error(build(m[, 1:2, drop = FALSE])) # ncol != k
  expect_error(build(m[1:10, , drop = FALSE])) # nrow != n cases
  bad_na <- m
  bad_na[1L, 1L] <- NA_real_
  expect_error(build(bad_na))
})


# %% k comes from the fit, not the labels ----

test_that("cluster_k has no label-counting default", {
  # The default aborts rather than guessing: counting labels is correct only
  # where a backend's non-noise labels enumerate its fitted clusters.
  expect_error(
    cluster_k(config = setup_KMeans(k = 3L), clust = list()),
    class = "rtemis_unsupported_error"
  )
})


# %% ClusteringMetrics ----

test_that("cluster() attaches metrics, with soft measures only where supported", {
  hard <- cluster(
    x,
    algorithm = "KMeans",
    config = setup_KMeans(k = 3L),
    verbosity = 0L
  )
  expect_s7_class(hard@metrics, ClusteringMetrics)
  m <- hard@metrics@metrics
  st <- hard@metrics@status
  expect_identical(m[["n_cases"]], nrow(x))
  # Copied from `@k`, never recounted.
  expect_identical(m[["n_clusters"]], hard@k)
  expect_identical(st[["mean_assignment_uncertainty"]], "unsupported")
  expect_true(is.na(m[["mean_assignment_uncertainty"]]))

  skip_if_not_installed("e1071")
  soft <- cluster(
    x,
    algorithm = "CMeans",
    config = setup_CMeans(k = 3L),
    verbosity = 0L
  )
  sm <- soft@metrics@metrics
  expect_identical(soft@metrics@status[["mean_assignment_entropy"]], "computed")
  expect_gte(sm[["mean_assignment_uncertainty"]], 0)
  expect_gte(sm[["mean_assignment_entropy"]], 0)
})

test_that("a status of computed requires a value, and any other forbids one", {
  # The rule the value/status split creates, mirrored into the published schema.
  expect_error(ClusteringMetrics(sample = "Training", n_cases = NA_integer_))
  ok <- ClusteringMetrics(sample = "Training", n_cases = 10L)
  expect_identical(ok@status[["n_cases"]], "computed")
  expect_identical(ok@status[["noise_fraction"]], "unsupported")
  expect_true(is.na(ok@metrics[["noise_fraction"]]))
})

test_that("DBSCAN noise reaches the metrics as a fraction, not a cluster", {
  skip_if_not_installed("dbscan")
  cl <- cluster(
    x,
    algorithm = "DBSCAN",
    config = setup_DBSCAN(eps = 0.3, min_points = 5L),
    verbosity = 0L
  )
  expect_gt(cl@metrics@metrics[["noise_fraction"]], 0)
  expect_identical(cl@metrics@metrics[["n_clusters"]], cl@k)
})


# %% Capability roster ----
# Hand-written, and required to name every registered algorithm exactly. Never
# derived from the registered methods: deriving it would conceal the omission
# the roster exists to catch.
.clust_capabilities <- list(
  KMeans = list(soft = FALSE, prescribes_k = TRUE),
  HardCL = list(soft = FALSE, prescribes_k = TRUE),
  NeuralGas = list(soft = FALSE, prescribes_k = TRUE),
  CMeans = list(soft = TRUE, prescribes_k = TRUE),
  DBSCAN = list(soft = FALSE, prescribes_k = FALSE),
  # The hybrid: `k` is nullable, so unset means BIC selects it and GMM must
  # register `cluster_k()`. `prescribes_k` asks whether the config *always*
  # prescribes one, and GMM does not.
  GMM = list(soft = TRUE, prescribes_k = FALSE),
  HOPACH = list(soft = FALSE, prescribes_k = FALSE),
  PAM = list(soft = FALSE, prescribes_k = TRUE),
  PAMK = list(soft = FALSE, prescribes_k = FALSE)
)

test_that("the capability roster covers every registered algorithm", {
  expect_setequal(names(.clust_capabilities), clust_algorithms[, 1])
  expect_false(anyDuplicated(names(.clust_capabilities)) > 0L)
})

test_that("membership and k methods are registered exactly where expected", {
  # `S7::method()` performs dispatch and therefore inherits, so "a method
  # resolves" is true for every class once a default exists. Specialization is
  # the resolved method differing from the base fallback.
  fallback_membership <- method(cluster_membership, ClusteringConfig)
  fallback_k <- method(cluster_k, ClusteringConfig)
  expect_null(fallback_membership(setup_KMeans(), list()))

  for (nm in names(.clust_capabilities)) {
    cfg <- get_default_clusterparams(nm)
    cls <- S7_class(cfg)
    caps <- .clust_capabilities[[nm]]

    specialized_membership <- !identical(
      method(cluster_membership, cls),
      fallback_membership
    )
    expect_identical(specialized_membership, caps[["soft"]], info = nm)

    # A prescribed-k algorithm never reaches `cluster_k()`, so it must not
    # register one; a discovering algorithm must.
    specialized_k <- !identical(method(cluster_k, cls), fallback_k)
    expect_identical(specialized_k, !caps[["prescribes_k"]], info = nm)
  }
})

test_that("every algorithm produces the variant the roster claims", {
  configs <- list(
    KMeans = setup_KMeans(k = 3L),
    HardCL = setup_HardCL(k = 3L),
    NeuralGas = setup_NeuralGas(k = 3L),
    CMeans = setup_CMeans(k = 3L),
    DBSCAN = setup_DBSCAN(eps = 0.3, min_points = 5L),
    GMM = setup_GMM(k = 3L),
    HOPACH = setup_HOPACH(dist = "euclid", max_levels = 3L, max_children = 5L),
    PAM = setup_PAM(k = 3L),
    PAMK = setup_PAMK(krange = 2:5)
  )
  pkgs <- c(
    KMeans = "flexclust",
    HardCL = "flexclust",
    NeuralGas = "flexclust",
    CMeans = "e1071",
    DBSCAN = "dbscan",
    GMM = "mclust",
    HOPACH = "hopach",
    PAM = "cluster",
    PAMK = "fpc"
  )
  for (nm in names(.clust_capabilities)) {
    skip_if_not_installed(pkgs[[nm]])
    cl <- cluster(x, algorithm = nm, config = configs[[nm]], verbosity = 0L)
    want <- if (.clust_capabilities[[nm]][["soft"]]) {
      SoftClustering
    } else {
      HardClustering
    }
    expect_s7_class(cl, want)
    expect_type(cl@k, "integer")
    expect_length(cl@clusters, nrow(x))
  }
})
