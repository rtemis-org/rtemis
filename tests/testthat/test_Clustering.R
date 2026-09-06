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
