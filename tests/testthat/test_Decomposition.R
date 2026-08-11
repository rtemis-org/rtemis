# test_Decomposition.R
# ::rtemis::
# 2025- EDG rtemis.org

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

test_that("NMF scores are non-negative and reproduce on their own data", {
  skip_if_not_installed("NMF")
  iris_nmf <- decomp(x, algorithm = "nmf", config = setup_NMF(k = 3L))
  transformed <- iris_nmf@transformed
  expect_true(all(transformed >= 0))
  expect_identical(dim(transformed), c(nrow(x), 3L))
  # Applying a fit to its own training data must return what the fit returned:
  # both go through the same non-negative least squares solve on the basis.
  expect_equal(
    as.matrix(apply_decomp(iris_nmf, x, verbosity = 0L)),
    transformed,
    tolerance = 1e-8
  )
})

test_that("NMF scores reconstruct the data through the basis", {
  skip_if_not_installed("NMF")
  iris_nmf <- decomp(x, algorithm = "nmf", config = setup_NMF(k = 3L))
  basis <- NMF::basis(iris_nmf@decom)
  reconstructed <- iris_nmf@transformed %*% t(basis)
  # The scores solve the least squares problem on this basis, so the
  # reconstruction cannot be worse than the one the raw projection gives.
  relative_error <- function(fitted) {
    norm(as.matrix(x) - fitted, "F") / norm(as.matrix(x), "F")
  }
  expect_lt(relative_error(reconstructed), 0.1)
  expect_lte(
    relative_error(reconstructed),
    relative_error(as.matrix(x) %*% basis %*% t(basis))
  )
})

test_that("setup_NMF(method=) reaches NMF::nmf()", {
  skip_if_not_installed("NMF")
  iris_nmf <- decomp(
    x,
    algorithm = "nmf",
    config = setup_NMF(k = 2L, method = "lee"),
    verbosity = 0L
  )
  expect_identical(NMF::algorithm(iris_nmf@decom), "lee")
})

test_that("NMF rejects more components than the basis can identify", {
  skip_if_not_installed("NMF")
  # A basis with more columns than the data has features cannot have full
  # column rank, so the non-negative coefficients are not identified.
  expect_error(
    decomp(
      x,
      algorithm = "nmf",
      config = setup_NMF(k = ncol(x) + 2L),
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
})

# Reconstruction ----
# `reconstruct_()` must return the data in the units it was handed to
# `decomp()`, whatever centering or scaling the backend applied internally.
# Reconstruction error taken in a backend's internal space would make two
# configurations of the same algorithm incomparable.
relative_error <- function(reconstructed, original) {
  original <- as.matrix(original)
  norm(original - as.matrix(reconstructed), "F") / norm(original, "F")
}

reconstruct_decomp <- function(decom, x) {
  reconstruct_(
    config = decom@config,
    decom = decom@decom,
    transformed = decom@transformed,
    x = x,
    verbosity = 0L
  )
}

test_that("PCA reconstructs in input units for every center/scale setting", {
  for (center in c(TRUE, FALSE)) {
    for (scale in c(TRUE, FALSE)) {
      # A full-rank fit loses nothing, so the reconstruction is the input.
      decom <- decomp(
        x,
        algorithm = "pca",
        config = setup_PCA(k = ncol(x), center = center, scale = scale),
        verbosity = 0L
      )
      expect_equal(
        reconstruct_decomp(decom, x),
        as.matrix(x),
        tolerance = 1e-8,
        ignore_attr = TRUE,
        info = paste("center =", center, "scale =", scale)
      )
    }
  }
})

test_that("PCA reconstruction degrades gracefully below full rank", {
  decom <- decomp(
    x,
    algorithm = "pca",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_lt(relative_error(reconstruct_decomp(decom, x), x), 0.1)
})

test_that("ICA reconstructs in input units for both row_norm settings", {
  skip_if_not_installed("fastICA")
  # `row_norm` subtracts each case's mean across features, so every row of the
  # preprocessed matrix sums to zero and its rank is at most `ncol(x) - 1`.
  # That, not `ncol(x)`, is the full-rank fit under `row_norm`.
  for (row_norm in c(TRUE, FALSE)) {
    k <- if (row_norm) ncol(x) - 1L else ncol(x)
    decom <- decomp(
      x,
      algorithm = "ica",
      config = setup_ICA(k = k, row_norm = row_norm),
      verbosity = 0L
    )
    expect_equal(
      reconstruct_decomp(decom, x),
      as.matrix(x),
      tolerance = 1e-8,
      ignore_attr = TRUE,
      info = paste("row_norm =", row_norm)
    )
  }
})

test_that("ICA reconstruction rejects a mismatched number of cases", {
  skip_if_not_installed("fastICA")
  decom <- decomp(
    x,
    algorithm = "ica",
    config = setup_ICA(k = 2L, row_norm = TRUE),
    verbosity = 0L
  )
  # The per-case statistics `row_norm` divided out belong to the cases the
  # components describe, so silently pairing them with other cases is refused.
  expect_error(
    reconstruct_decomp(decom, x[1:10, ]),
    class = "rtemis_dim_error"
  )
})

test_that("NMF reconstructs through its basis in input units", {
  skip_if_not_installed("NMF")
  decom <- decomp(
    x,
    algorithm = "nmf",
    config = setup_NMF(k = ncol(x)),
    verbosity = 0L
  )
  expect_lt(relative_error(reconstruct_decomp(decom, x), x), 0.05)
})

test_that("reconstruct() round-trips in input units and preserves layout", {
  for (config in list(setup_PCA(k = ncol(x)), setup_ICA(k = ncol(x) - 1L))) {
    decom <- decomp(x, config = config, verbosity = 0L)
    reconstructed <- reconstruct(decom, x, verbosity = 0L)
    expect_s3_class(reconstructed, "data.frame")
    expect_identical(names(reconstructed), names(x))
    # A full-rank fit loses nothing, so the round trip is the identity.
    expect_equal(
      as.matrix(reconstructed),
      as.matrix(x),
      tolerance = 1e-8,
      ignore_attr = TRUE,
      info = decom@algorithm
    )
  }
})


test_that("reconstruct() below full rank approximates rather than reproduces", {
  decom <- decomp(
    x,
    algorithm = "pca",
    config = setup_PCA(k = 1L),
    verbosity = 0L
  )
  reconstructed <- as.matrix(reconstruct(decom, x, verbosity = 0L))
  expect_false(isTRUE(all.equal(reconstructed, as.matrix(x))))
  expect_lt(
    norm(as.matrix(x) - reconstructed, "F") / norm(as.matrix(x), "F"),
    0.2
  )
})


test_that("reconstruct() passes through columns the fit did not decompose", {
  features <- c("Sepal.Length", "Sepal.Width")
  decom <- decomp(
    x,
    algorithm = "pca",
    config = setup_PCA(k = 2L, features = features),
    verbosity = 0L
  )
  reconstructed <- reconstruct(decom, x, verbosity = 0L)
  # Same columns in the same order as the input, not the decomposed ones moved
  # to the end: the result is meant to line up with `x` cell for cell.
  expect_identical(names(reconstructed), names(x))
  untouched <- setdiff(names(x), features)
  expect_identical(reconstructed[, untouched], x[, untouched])
  # Two components on two features is full rank, so those reconstruct exactly.
  expect_equal(
    as.matrix(reconstructed[, features]),
    as.matrix(x[, features]),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})


test_that("reconstruct() refuses algorithms with no inverse", {
  skip_if_not_installed("uwot")
  decom <- decomp(x, algorithm = "umap", verbosity = 0L)
  expect_error(
    reconstruct(decom, x, verbosity = 0L),
    class = "rtemis_unsupported_error"
  )
})


test_that("reconstruct() agrees with the metrics' own reconstruction", {
  # `decomp()` scores using `@transformed` directly while `reconstruct()`
  # re-encodes `x`; if the two maps ever diverged, the reported reconstruction
  # error would not describe what `reconstruct()` returns.
  decom <- decomp(
    x,
    algorithm = "pca",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  reconstructed <- as.matrix(reconstruct(decom, x, verbosity = 0L))
  relative <- norm(as.matrix(x) - reconstructed, "F") / norm(as.matrix(x), "F")
  expect_equal(relative, decom@metrics[["relative_reconstruction_error"]])
})


test_that("apply_decomp() on training data reproduces the fitted components", {
  # Fit and apply must be the same map, or a fit-on-train apply-to-both-splits
  # workflow silently compares two different embeddings.
  for (config in list(setup_PCA(k = 3L), setup_ICA(k = 3L))) {
    decom <- decomp(x, config = config, verbosity = 0L)
    expect_equal(
      as.matrix(apply_decomp(decom, x, verbosity = 0L)),
      as.matrix(decom@transformed),
      tolerance = 1e-8,
      ignore_attr = TRUE,
      info = decom@algorithm
    )
  }
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
  # The backend's own message must survive `do_call()`'s error handling, which
  # is what makes the failure actionable; asserting only that it errors let a
  # broken handler report "cannot coerce type 'closure'" instead.
  expect_error(decomp(x, algorithm = "tsne"), "Remove duplicates")
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


# features selection ----
test_that("decomp() fits on config@features, so apply_decomp() replays it", {
  config <- setup_PCA(k = 2L, features = c("Sepal.Length", "Sepal.Width"))
  fit <- decomp(x, algorithm = "PCA", config = config, verbosity = 0L)
  # Fitted on the two selected columns only: applying to the same data must
  # work, and the undecomposed columns come back alongside the components.
  applied <- apply_decomp(fit, x, verbosity = 0L)
  expect_identical(
    names(applied),
    c("Petal.Length", "Petal.Width", "PC1", "PC2")
  )
  # A fit that used all four columns could not be replayed against two.
  expect_identical(ncol(fit@transformed), 2L)
})

test_that("decomp() validates config@features against the data", {
  expect_error(
    decomp(
      iris,
      algorithm = "PCA",
      config = setup_PCA(k = 2L, features = c("Sepal.Length", "Species")),
      verbosity = 0L
    ),
    "must name numeric training features"
  )
  expect_error(
    decomp(
      x,
      algorithm = "PCA",
      config = setup_PCA(k = 2L, features = c("Sepal.Length", "nope")),
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
})

test_that("an unset features decomposes every column", {
  fit <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_null(fit@config@features)
  expect_identical(names(apply_decomp(fit, x, verbosity = 0L)), c("PC1", "PC2"))
})
