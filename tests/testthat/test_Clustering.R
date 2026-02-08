# test_Clustering.R
# ::rtemis::
# 2025 EDG rtemis.org

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
