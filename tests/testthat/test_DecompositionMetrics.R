# test_DecompositionMetrics.R
# ::rtemis::
# 2026- EDG rtemis.org

x <- exc(iris, "Species")

# Derivation ----
test_that("applicable_metrics() is derived from the traits table", {
  matrix_form <- applicable_metrics()
  expect_identical(matrix_form[["name"]], decom_metrics[["name"]])
  for (algorithm in decom_algorithms[["name"]]) {
    traits <- decomposition_traits(algorithm)
    for (i in seq_len(nrow(decom_metrics))) {
      required <- decom_metrics[["requires"]][[i]]
      expect_identical(
        matrix_form[[algorithm]][i],
        all(unlist(traits[required], use.names = FALSE)),
        info = paste(algorithm, decom_metrics[["name"]][i])
      )
    }
  }
})


test_that("applicable_metrics() reflects what each trait buys", {
  # Reconstruction needs an inverse; component metrics need nothing.
  expect_true(all(
    c("explained_variance_ratio", "reconstruction_rmse") %in%
      applicable_metrics("PCA")[["name"]]
  ))
  expect_false(any(
    c("explained_variance_ratio", "reconstruction_rmse") %in%
      applicable_metrics("UMAP")[["name"]]
  ))
  expect_true(
    "max_abs_component_correlation" %in% applicable_metrics("tSNE")[["name"]]
  )
  # UMAP applies to new data but does not invert, so it gets no out-of-sample
  # reconstruction either.
  expect_false(
    "oos_explained_variance_ratio" %in% applicable_metrics("UMAP")[["name"]]
  )
})


test_that("the metrics computed are exactly the metrics derived as applicable", {
  # The guard that keeps the derivation honest: if a metric is computed for an
  # algorithm whose traits do not support it, or omitted for one they do, the
  # traits table has stopped describing the code.
  for (algorithm in c("PCA", "ICA", "NMF", "UMAP", "Isomap")) {
    skip_if_not_installed(decomposition_traits(algorithm)[["package"]])
    decom <- decomp(x, algorithm = algorithm, verbosity = 0L)
    applicable <- applicable_metrics(algorithm)
    expected <- applicable[["name"]][!applicable[["needs_new_data"]]]
    populated <- names(decom@metrics@metrics)[
      !is.na(unlist(decom@metrics@metrics))
    ]
    expect_setequal(populated, expected)
  }
})


# Closed forms ----
test_that("PCA explained_variance_ratio matches prcomp's variance proportion", {
  k <- 3L
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = k, center = TRUE, scale = FALSE),
    verbosity = 0L
  )
  reference <- prcomp(x, center = TRUE, scale. = FALSE)
  expect_equal(
    decom@metrics[["explained_variance_ratio"]],
    sum(reference[["sdev"]][1:k]^2) / sum(reference[["sdev"]]^2)
  )
})


test_that("reconstruction metrics are in input units, not the scaled space", {
  # The trap this whole design exists to avoid. Under `scale = TRUE` the
  # variance proportion `prcomp` reports is computed on standardized columns;
  # the metric is computed on the data as `decomp()` received it, so the two
  # differ and only the metric is comparable with another algorithm.
  k <- 3L
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = k, center = TRUE, scale = TRUE),
    verbosity = 0L
  )
  reference <- prcomp(x, center = TRUE, scale. = TRUE)
  scaled_space <- sum(reference[["sdev"]][1:k]^2) / sum(reference[["sdev"]]^2)
  expect_false(isTRUE(all.equal(
    decom@metrics[["explained_variance_ratio"]],
    scaled_space
  )))
  # Still a sane number: the reconstruction is good, just measured elsewhere.
  expect_gt(decom@metrics[["explained_variance_ratio"]], 0.9)
})


test_that("a full-rank PCA reconstructs perfectly", {
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = ncol(x)),
    verbosity = 0L
  )
  expect_equal(decom@metrics[["explained_variance_ratio"]], 1)
  expect_equal(decom@metrics[["relative_reconstruction_error"]], 0)
  expect_equal(decom@metrics[["reconstruction_rmse"]], 0)
})


test_that("PCA reconstruction error decreases monotonically in k", {
  errors <- vapply(
    seq_len(ncol(x)),
    function(k) {
      decomp(
        x,
        algorithm = "PCA",
        config = setup_PCA(k = k),
        verbosity = 0L
      )@metrics[["relative_reconstruction_error"]]
    },
    numeric(1L)
  )
  expect_true(all(diff(errors) <= 0))
})


test_that("PCA components are uncorrelated and NMF's are not required to be", {
  pca <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 3L),
    verbosity = 0L
  )
  expect_lt(pca@metrics[["max_abs_component_correlation"]], 1e-8)
  expect_gte(pca@metrics[["effective_dimensionality"]], 1)
  expect_lte(pca@metrics[["effective_dimensionality"]], 3)
})


test_that("component metrics are defined for a non-invertible embedding", {
  skip_if_not_installed("uwot")
  decom <- decomp(
    x,
    algorithm = "UMAP",
    config = setup_UMAP(k = 2L),
    verbosity = 0L
  )
  expect_false(is.na(decom@metrics[["max_abs_component_correlation"]]))
  expect_false(is.na(decom@metrics[["effective_dimensionality"]]))
  expect_true(is.na(decom@metrics[["explained_variance_ratio"]]))
})


# Out of sample ----
test_that("decomp_metrics() adds out-of-sample metrics only with new_data", {
  train <- x[1:100, ]
  test <- x[101:150, ]
  decom <- decomp(
    train,
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )

  in_sample <- decomp_metrics(decom, train, verbosity = 0L)
  expect_s7_class(in_sample, DecompositionMetrics)
  expect_true(is.na(in_sample[["oos_explained_variance_ratio"]]))
  expect_true(is.na(in_sample[["reconstruction_gap"]]))
  # Recomputing on the fitted data reproduces what `decomp()` stored.
  expect_equal(
    in_sample[["relative_reconstruction_error"]],
    decom@metrics[["relative_reconstruction_error"]]
  )

  out_of_sample <- decomp_metrics(decom, train, new_data = test, verbosity = 0L)
  expect_false(is.na(out_of_sample[["oos_explained_variance_ratio"]]))
  expect_equal(
    out_of_sample[["reconstruction_gap"]],
    out_of_sample[["oos_relative_reconstruction_error"]] -
      out_of_sample[["relative_reconstruction_error"]]
  )
  # PCA generalizes: it fits a subspace, not the cases.
  expect_lt(abs(out_of_sample[["reconstruction_gap"]]), 0.1)
})


test_that("decomp_metrics() refuses new_data for an algorithm that cannot use it", {
  skip_if_not_installed("Rtsne")
  # tSNE has no out-of-sample extension, so there is nothing to project.
  deduplicated <- preprocess(
    x,
    setup_Preprocessor(remove_duplicates = TRUE),
    verbosity = 0L
  )@preprocessed
  decom <- decomp(deduplicated, algorithm = "tSNE", verbosity = 0L)
  expect_error(
    decomp_metrics(
      decom,
      deduplicated,
      new_data = deduplicated,
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("decomp() stores metrics on its result", {
  decom <- decomp(x, algorithm = "PCA", verbosity = 0L)
  expect_s7_class(decom@metrics, DecompositionMetrics)
  expect_identical(decom@metrics@sample, "Training")
  expect_identical(nrow(decom@metrics@metrics), 1L)
  # Every column is declared whether or not the algorithm supports it, so the
  # table's shape does not depend on the algorithm.
  expect_identical(
    names(decom@metrics@metrics),
    decom_metrics[["name"]]
  )
})
