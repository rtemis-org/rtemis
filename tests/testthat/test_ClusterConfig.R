# test_ClusterConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% setup_ClusterConfig() ----
test_that("setup_ClusterConfig() succeeds", {
  cc <- setup_ClusterConfig(
    dat_path = "data.csv",
    clustering_config = setup_KMeans(k = 3L),
    outdir = "results/",
    verbosity = 1L
  )
  expect_s7_class(cc, ClusterConfig)
})


# %% cluster ClusterConfig ----
test_that("cluster() works with ClusterConfig", {
  testthat::skip("For local testing only; requires CSV file")
  cc <- setup_ClusterConfig(
    dat_path = "~/Data/iris_numeric.csv",
    clustering_config = setup_KMeans(k = 3L),
    outdir = "cluster_out/",
    verbosity = 1L
  )
  clust <- cluster(cc)
  expect_s7_class(clust, Clustering)
})


# %% write_config.ClusterConfig & read_config ----
test_that("ClusterConfig round-trips through write_config/read_config JSON", {
  x <- setup_ClusterConfig(
    dat_path = "data.csv",
    clustering_config = setup_KMeans(k = 3L),
    outdir = "results/"
  )
  file <- file.path(tempdir(), "rtemis_cluster.json")
  write_config(x, file, overwrite = TRUE)
  expect_true(file.exists(file))
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/cluster/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, ClusterConfig)
  expect_s7_class(xtoo@clustering_config, ClusteringConfig)
  expect_identical(
    xtoo@clustering_config@algorithm,
    x@clustering_config@algorithm
  )
})
