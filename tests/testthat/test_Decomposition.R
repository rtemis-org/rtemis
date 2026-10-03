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
  configs <- list(setup_PCA(k = 3L), setup_ICA(k = 3L))
  if (requireNamespace("vegan", quietly = TRUE)) {
    configs <- c(
      configs,
      list(setup_PCoA(k = 3L), setup_PCoA(k = 2L, dist_method = "bray"))
    )
  }
  for (config in configs) {
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


test_that("UMAP fits on the execution config's threads and applies on the applier's", {
  skip_if_not_installed("uwot")
  # uwot's own default is half the hardware threads, whatever the core limit.
  requested <- integer()
  local_mocked_bindings(
    umap_transform = function(X, model, n_threads, ...) {
      requested <<- c(requested, as.integer(n_threads))
      matrix(0, nrow = NROW(X), ncol = 2L)
    },
    .package = "uwot"
  )
  decom <- decomp(
    x,
    algorithm = "umap",
    execution_config = setup_SerialExecution(n_workers_algorithm = 2L),
    verbosity = 0L
  )
  apply_decomp(
    decom,
    x,
    execution_config = setup_SerialExecution(n_workers_algorithm = 1L),
    verbosity = 0L
  )
  expect_identical(requested, 1L)
  apply_decomp(decom, x, verbosity = 0L)
  expect_identical(requested, c(1L, default_n_workers()))
})

test_that("the default thread resolution is at most two under a CRAN check", {
  # uwot's own default is half the hardware threads, whatever the core limit;
  # every threaded fit and application resolves through this instead.
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = "TRUE")
  expect_lte(algorithm_threads(), 2L)
  expect_lte(algorithm_threads(setup_FutureExecution()), 2L)
  expect_identical(
    algorithm_threads(setup_SerialExecution(n_workers_algorithm = 3L)),
    3L
  )
})

test_that("decomp() prints one resources line, with threads only for a threaded algorithm", {
  resources <- function(algorithm) {
    paste(
      gsub(
        "\\033\\[[0-9;]*m",
        "",
        testthat::capture_messages(decomp(
          x,
          algorithm = algorithm,
          execution_config = setup_SerialExecution(n_workers_algorithm = 2L),
          verbosity = 1L
        ))
      ),
      collapse = ""
    )
  }
  expect_match(
    resources("PCA"),
    "// CPU | serial | 1 worker: algorithm 1 thread",
    fixed = TRUE
  )
  skip_if_not_installed("uwot")
  expect_match(
    resources("UMAP"),
    "// CPU | serial | 1 worker: algorithm 2 threads (as set)",
    fixed = TRUE
  )
})

test_that("decomp() seeds the fit from the execution config and restores the caller's stream", {
  skip_if_not_installed("fastICA")
  fit <- function(seed) {
    decomp(
      x,
      algorithm = "ICA",
      execution_config = setup_SerialExecution(seed = seed),
      verbosity = 0L
    )@transformed
  }
  expect_identical(fit(2026L), fit(2026L))
  expect_false(isTRUE(all.equal(fit(2026L), fit(7L))))
  set.seed(1)
  fit(2026L)
  after_fit <- stats::runif(1L)
  set.seed(1)
  expect_identical(after_fit, stats::runif(1L))
})

test_that("the run's input records the execution config it ran under", {
  decom <- decomp(
    x,
    algorithm = "PCA",
    execution_config = setup_SerialExecution(seed = 11L),
    verbosity = 0L
  )
  expect_s7_class(
    decom@decompose_config@execution_config,
    SerialExecutionConfig
  )
  expect_identical(decom@decompose_config@execution_config@seed, 11L)
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


# PCoA ----
test_that("setup_PCoA() succeeds", {
  config <- setup_PCoA(dist_method = "bray")
  expect_s7_class(config, PCoAConfig)
  expect_identical(config[["dist_method"]], "bray")
})

test_that("decomp() PCoA returns k components, equal to PCA scores under euclidean", {
  skip_if_not_installed("vegan")
  decom <- decomp(x, config = setup_PCoA(k = 3L), verbosity = 0L)
  expect_s7_class(decom, Decomposition)
  expect_identical(dim(decom@transformed), c(nrow(x), 3L))
  expect_identical(colnames(decom@transformed), paste0("PCoA_", 1:3))
  scores <- stats::prcomp(x, center = TRUE, scale. = FALSE)[["x"]][, 1:3]
  expect_equal(
    abs(unname(as.matrix(decom@transformed))),
    abs(unname(scores)),
    tolerance = 1e-8
  )
})

test_that("PCoA applied to new data under euclidean equals the PCA projection", {
  # An independent implementation of the same map: Gower's formula on Euclidean
  # distances is the projection onto the training principal axes.
  skip_if_not_installed("vegan")
  train_rows <- seq(1L, nrow(x), by = 2L)
  decom <- decomp(
    x[train_rows, ],
    config = setup_PCoA(k = 2L),
    verbosity = 0L
  )
  pca <- stats::prcomp(x[train_rows, ], center = TRUE, scale. = FALSE)
  signs <- sign(colSums(
    as.matrix(decom@transformed) * pca[["x"]][, 1:2]
  ))
  expected <- sweep(
    stats::predict(pca, x[-train_rows, ])[, 1:2],
    2L,
    signs,
    FUN = "*"
  )
  expect_equal(
    unname(as.matrix(apply_decomp(decom, x[-train_rows, ], verbosity = 0L))),
    unname(expected),
    tolerance = 1e-8
  )
})

test_that("PCoA applies each case independently of the batch it arrives in", {
  # A fit on 60 cases applies to 150 in three chunks; each case's coordinates
  # must not depend on the chunk or the other cases.
  skip_if_not_installed("vegan")
  decom <- decomp(
    x[1:60, ],
    config = setup_PCoA(k = 2L, dist_method = "bray"),
    verbosity = 0L
  )
  all_cases <- as.matrix(apply_decomp(decom, x, verbosity = 0L))
  some <- c(3L, 77L, 150L)
  expect_equal(
    as.matrix(apply_decomp(decom, x[some, ], verbosity = 0L)),
    all_cases[some, ],
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("PCoA rejects more components than positive eigenvalues", {
  skip_if_not_installed("vegan")
  # Four Euclidean features span four dimensions.
  expect_error(
    decomp(x, config = setup_PCoA(k = 5L), verbosity = 0L),
    "at most 4",
    class = "rtemis_data_error"
  )
  expect_error(
    decomp(x[1:3, ], config = setup_PCoA(k = 3L), verbosity = 0L),
    class = "rtemis_data_error"
  )
})

test_that("non-negative dissimilarities reject negative and all-zero cases", {
  skip_if_not_installed("vegan")
  scaled <- as.data.frame(scale(x))
  expect_error(
    decomp(scaled, config = setup_PCoA(dist_method = "bray"), verbosity = 0L),
    "non-negative",
    class = "rtemis_data_error"
  )
  zero <- rbind(x, setNames(as.data.frame(t(rep(0, 4L))), names(x)))
  expect_error(
    decomp(
      zero,
      config = setup_PCoA(dist_method = "hellinger"),
      verbosity = 0L
    ),
    "rows 151",
    class = "rtemis_data_error"
  )
  decom <- decomp(
    x,
    config = setup_PCoA(dist_method = "hellinger"),
    verbosity = 0L
  )
  expect_error(
    apply_decomp(decom, zero[151L, ], verbosity = 0L),
    class = "rtemis_data_error"
  )
})

test_that("train() learns PCoA as its decomposition step and predict() replays it", {
  skip_if_not_installed("vegan")
  dat <- iris[, c(2L, 3L, 4L, 1L)]
  mod <- train(
    dat,
    decomposition_config = setup_PCoA(k = 2L, dist_method = "manhattan"),
    hyperparameters = setup_GLMNET(alpha = 0, lambda = 0.01),
    verbosity = 0L
  )
  expect_s7_class(mod@decomposition@config, PCoAConfig)
  predicted <- predict(mod, features(dat))
  expect_length(predicted, nrow(dat))
  expect_false(anyNA(predicted))
})


# MDS ----
test_that("setup_MDS() succeeds", {
  config <- setup_MDS(model = "linear", nstart = 3L)
  expect_s7_class(config, MDSConfig)
  expect_identical(config[["nstart"]], 3L)
})

test_that("decomp() MDS returns k uncorrelated components", {
  skip_if_not_installed("vegan")
  decom <- decomp(
    x,
    config = setup_MDS(k = 3L, nstart = 3L),
    verbosity = 0L
  )
  expect_s7_class(decom, Decomposition)
  expect_identical(dim(decom@transformed), c(nrow(x), 3L))
  expect_identical(colnames(decom@transformed), paste0("MDS_", 1:3))
  # The orthogonal trait: monoMDS rotates the configuration to principal axes.
  cp <- crossprod(scale(as.matrix(decom@transformed), scale = FALSE))
  expect_lt(max(abs(cp[upper.tri(cp)])), 1e-8 * max(diag(cp)))
})

test_that("MDS keeps the lowest-stress start, and one start is deterministic", {
  skip_if_not_installed("vegan")
  fit <- function(nstart, seed = NULL) {
    decomp(
      x,
      config = setup_MDS(nstart = nstart),
      execution_config = setup_SerialExecution(seed = seed),
      verbosity = 0L
    )
  }
  one <- fit(1L)
  expect_identical(one@transformed, fit(1L)@transformed)
  # The first of several starts is the single start, so the best is no worse.
  several <- fit(5L, seed = 2026L)
  expect_lte(several@decom[["stress"]], one@decom[["stress"]])
  expect_identical(several@transformed, fit(5L, seed = 2026L)@transformed)
  for (model in c("local", "linear")) {
    expect_s7_class(
      decomp(
        x,
        config = setup_MDS(model = model, nstart = 1L, max_iter = 500L),
        verbosity = 0L
      ),
      Decomposition
    )
  }
})

test_that("MDS warns when the best start reaches max_iter", {
  skip_if_not_installed("vegan")
  expect_message(
    decomp(
      x,
      config = setup_MDS(nstart = 1L, max_iter = 1L),
      verbosity = 0L
    ),
    "max_iter"
  )
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

test_that("an unset features decomposes every numeric column, resolved onto the fit", {
  # `null` means every numeric column, and the fit records which those were, so
  # the replay transforms the same matrix and the run record states what ran.
  fit <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_identical(fit@config@features, names(x))
  expect_identical(names(apply_decomp(fit, x, verbosity = 0L)), c("PC1", "PC2"))

  # A column the backend cannot read is not decomposed, and does not stop the
  # run: `iris`' species column comes back beside the components.
  mixed <- decomp(iris, config = setup_PCA(k = 2L), verbosity = 0L)
  expect_identical(mixed@config@features, names(x))
  expect_identical(
    names(apply_decomp(mixed, iris, verbosity = 0L)),
    c("Species", "PC1", "PC2")
  )
})


# one place for the algorithm ----
test_that("decomp() takes the algorithm from config and refuses a disagreeing label", {
  fit <- decomp(x, config = setup_PCA(k = 2L), verbosity = 0L)
  expect_identical(fit@algorithm, "PCA")
  expect_error(
    decomp(x, algorithm = "ICA", config = setup_PCA(k = 2L), verbosity = 0L),
    "pass one or the other",
    class = "rtemis_value_error"
  )
  fit2 <- decomp(
    x,
    algorithm = "pca",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_identical(fit2@algorithm, "PCA")
})


test_that("the run record states the execution config once, with origins only for flat fields", {
  decom <- decomp(
    x,
    algorithm = "PCA",
    execution_config = setup_SerialExecution(seed = 3L),
    verbosity = 0L
  )
  rec <- record(decom)
  expect_identical(rec[["execution_config"]][["seed"]], 3L)
  expect_identical(rec[["execution_config"]][["origin"]][["seed"]], "user")
  # `execution_config` and `decomposition_config` carry their own origins; the
  # document's covers its flat fields and nothing unnamed.
  expect_setequal(names(rec[["origin"]]), c("dat_path", "outdir", "verbosity"))
  expect_false(anyNA(names(rec[["origin"]])))
})


test_that("NMF with several runs fits, sequentially", {
  skip_if_not_installed("NMF")
  # NMF's default for `nrun > 1` is a parallel setup that requires the package
  # attached; reached through `NMF::` it failed with "none of the packages are
  # loaded".
  decom <- decomp(
    x,
    config = setup_NMF(k = 2L, nrun = 2L),
    execution_config = setup_SerialExecution(seed = 2026L),
    verbosity = 0L
  )
  expect_s7_class(decom, Decomposition)
  expect_identical(nrow(decom@transformed), nrow(x))
})

test_that("setup_tSNE(num_threads =) is deprecated in favor of the execution config", {
  expect_warning(setup_tSNE(num_threads = 2L), class = "deprecatedWarning")
  expect_false("num_threads" %in% names(tSNEConfig@properties))
  expect_no_warning(setup_tSNE())
})
