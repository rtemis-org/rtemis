# test-Decomposition.R
# ::rtemis::
# 2025 EDG rtemis.org

library(rtemis)
# Data ----
x <- iris[, -5]

# PCA ----
config <- setup_PCA()
test_that("setup_PCA() succeeds", {
  expect_s7_class(config, PCAConfig)
})
iris_pca <- decomp(x, algorithm = "pca", config = config)
iris_pca
test_that("decomp() PCA succeeds", {
  expect_s7_class(iris_pca, Decomposition)
})

# ICA ----
config <- setup_ICA()
test_that("setup_ICA() succeeds", {
  expect_s7_class(config, ICAConfig)
})
iris_ica <- decomp(x, algorithm = "ica", config = config)
test_that("decomp() ICA succeeds", {
  expect_s7_class(iris_ica, Decomposition)
})

# NMF ----
config <- setup_NMF()
test_that("setup_NMF() succeeds", {
  expect_s7_class(config, NMFConfig)
})
iris_nmf <- decomp(x, algorithm = "nmf", config = config)
test_that("decomp() NMF succeeds", {
  expect_s7_class(iris_nmf, Decomposition)
})

# UMAP ----
config <- setup_UMAP()
test_that("setup_UMAP() succeeds", {
  expect_s7_class(config, UMAPConfig)
})
iris_umap <- decomp(x, algorithm = "umap", config = config)
iris_umap <- decomp(
  x,
  algorithm = "umap",
  config = setup_UMAP(n_neighbors = 20L)
)

# t-SNE ----
config <- setup_tSNE()
test_that("setup_tSNE() succeeds", {
  expect_s7_class(config, tSNEConfig)
})
# Test that t-SNE fails with duplicates
test_that("decomp() t-SNE fails with duplicates", {
  expect_error(decomp(x, algorithm = "tsne", config = config))
})

# Test that t-SNE works after removing duplicates
xp <- preprocess(x, setup_Preprocessor(remove_duplicates = TRUE))
iris_tsne <- decomp(
  xp@preprocessed,
  algorithm = "tsne",
  config = config
)
test_that("decomp() t-SNE succeeds after removing duplicates", {
  expect_s7_class(iris_tsne, Decomposition)
})

# Isomap ----
config <- setup_Isomap()
test_that("setup_Isomap() succeeds", {
  expect_s7_class(config, IsomapConfig)
})
iris_isomap <- decomp(x, algorithm = "isomap", config = config)
test_that("decomp() Isomap succeeds", {
  expect_s7_class(iris_isomap, Decomposition)
})
