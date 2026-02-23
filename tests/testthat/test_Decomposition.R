# test_Decomposition.R
# ::rtemis::
# 2025 EDG rtemis.org

# Data ----
x <- iris[, -5]

# PCA ----
test_that("setup_PCA() succeeds", {
  config <- setup_PCA()
  expect_s7_class(config, PCAConfig)
})

test_that("decomp() PCA succeeds", {
  iris_pca <- decomp(x, algorithm = "pca", config = setup_PCA())
  iris_pca
  expect_s7_class(iris_pca, Decomposition)
})

# ICA ----
test_that("setup_ICA() succeeds", {
  config <- setup_ICA()
  expect_s7_class(config, ICAConfig)
})

test_that("decomp() ICA succeeds", {
  skip_if_not_installed("fastICA")
  iris_ica <- decomp(x, algorithm = "ica", config = setup_ICA())
  expect_s7_class(iris_ica, Decomposition)
})

# NMF ----
test_that("setup_NMF() succeeds", {
  config <- setup_NMF()
  expect_s7_class(config, NMFConfig)
})

test_that("decomp() NMF succeeds", {
  skip_if_not_installed("NMF")
  iris_nmf <- decomp(x, algorithm = "nmf", config = setup_NMF())
  expect_s7_class(iris_nmf, Decomposition)
})

# UMAP ----
test_that("setup_UMAP() succeeds", {
  config <- setup_UMAP()
  expect_s7_class(config, UMAPConfig)
})

test_that("decomp() UMAP succeeds", {
  skip_if_not_installed("uwot")
  iris_umap <- decomp(x, algorithm = "umap", config = setup_UMAP())
  iris_umap <- decomp(
    x,
    algorithm = "umap",
    config = setup_UMAP(n_neighbors = 20L)
  )
  expect_s7_class(iris_umap, Decomposition)
})

# t-SNE ----
test_that("setup_tSNE() succeeds", {
  config <- setup_tSNE()
  expect_s7_class(config, tSNEConfig)
})

# Test that t-SNE fails with duplicates
test_that("decomp() t-SNE fails with duplicates", {
  skip_if_not_installed("Rtsne")
  expect_error(decomp(x, algorithm = "tsne"))
})

# Test that t-SNE works after removing duplicates
test_that("decomp() t-SNE succeeds after removing duplicates", {
  skip_if_not_installed("Rtsne")
  xp <- preprocess(x, setup_Preprocessor(remove_duplicates = TRUE))
  iris_tsne <- decomp(
    xp@preprocessed,
    algorithm = "tsne",
    config = setup_tSNE()
  )
  expect_s7_class(iris_tsne, Decomposition)
})

# Isomap ----
test_that("setup_Isomap() succeeds", {
  config <- setup_Isomap()
  expect_s7_class(config, IsomapConfig)
})

test_that("decomp() Isomap succeeds", {
  skip_if_not_installed("vegan")
  iris_isomap <- decomp(x, algorithm = "isomap", config = setup_Isomap())
  expect_s7_class(iris_isomap, Decomposition)
})
