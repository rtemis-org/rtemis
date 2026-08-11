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


test_that("effective_dimensionality is NA, not an error, for a variance that is not finite", {
  # `sum(variances^2)` is NA when any component's variance is -- a single case,
  # or values large enough that `var()` overflows -- and the undefined case
  # returns NA rather than raising.
  expect_true(is.na(component_scores(
    matrix(c(1, 2), nrow = 1L)
  )[["effective_dimensionality"]]))
  expect_true(is.na(component_scores(
    cbind(c(Inf, Inf, Inf), c(1, 2, 3))
  )[["effective_dimensionality"]]))
  expect_true(is.na(component_scores(
    cbind(c(NA_real_, 1, 2), c(1, 2, 3))
  )[["effective_dimensionality"]]))
})


test_that("effective_dimensionality never lands below its declared minimum of 1", {
  # `sum(v)^2 >= sum(v^2)` for non-negative `v` by Cauchy-Schwarz, and the
  # column is declared `min = 1`, which is a hard validator: a value below it
  # aborts construction rather than being recorded. The bound has to hold in
  # floating point over the whole range of variance vectors a fit can produce.
  set.seed(2026)
  generators <- list(
    single = function() abs(rnorm(1L)) * 10^runif(1L, -300, 300),
    dominant = function() c(10^runif(1L, 0, 300), 10^runif(5L, -320, -1)),
    wide_range = function() 10^runif(sample(2:50, 1L), -300, 300),
    near_equal = function() {
      rep(10^runif(1L, -150, 150), 20L) *
        (1 + rnorm(20L) * 1e-16)
    },
    with_zeros = function() {
      v <- 10^runif(20L, -300, 300)
      v[sample(20L, 10L)] <- 0
      v
    }
  )
  # A ratio that is not finite is the case the metric already returns NA for --
  # both sums overflowing gives Inf/Inf -- and the column's validator drops NA.
  # The bound is a claim about the values that do get recorded.
  observed <- vapply(
    rep(generators, each = 2000L),
    function(generate) {
      v <- generate()
      v <- v[is.finite(v)]
      if (length(v) == 0L) {
        return(NA_real_)
      }
      sum(v)^2 / sum(v^2)
    },
    numeric(1L)
  )
  observed <- observed[is.finite(observed)]
  expect_gt(length(observed), 2000L)
  expect_gte(min(observed), 1)
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


# Feature subsets ----
test_that("metrics read only the features the decomposition was fitted on", {
  # A fit made with `features` spans those columns only, so its reconstruction
  # is narrower than the frame it was fitted from. Passing that whole frame is
  # the documented call, and it has to agree with passing the subset.
  features <- c("Sepal.Length", "Sepal.Width")
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 1L, features = features),
    verbosity = 0L
  )
  full <- decomp_metrics(decom, x, verbosity = 0L)
  subset_only <- decomp_metrics(decom, x[, features], verbosity = 0L)
  expect_equal(
    full[["relative_reconstruction_error"]],
    subset_only[["relative_reconstruction_error"]]
  )
  expect_equal(
    full[["relative_reconstruction_error"]],
    decom@metrics[["relative_reconstruction_error"]]
  )
  # `new_data` carries the extra columns too and is subset the same way.
  oos <- decomp_metrics(decom, x, new_data = x[101:150, ], verbosity = 0L)
  expect_false(is.na(oos[["oos_relative_reconstruction_error"]]))
})


test_that("decomp_metrics() rejects an `x` that is not the fitted data", {
  # The stored components are the fit's own, so they describe the fitted cases
  # and nothing else. A case count that cannot be theirs says so.
  decom <- decomp(
    x[1:100, ],
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_error(
    decomp_metrics(decom, x, verbosity = 0L),
    class = "rtemis_dim_error"
  )
  # And it aborts without first hashing to say the same thing less precisely.
  expect_error(
    expect_no_message(decomp_metrics(decom, x, verbosity = 1L)),
    class = "rtemis_dim_error"
  )
})


test_that("a same-shape, same-names, different-values `x` is caught", {
  # What shape and column names cannot see, and the reason the check is a
  # fingerprint: two halves of one split agree on both.
  first <- x[1:75, ]
  second <- x[76:150, ]
  decom <- decomp(
    first,
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  expect_identical(dim(first), dim(second))
  expect_identical(names(first), names(second))
  expect_message(
    decomp_metrics(decom, second, verbosity = 1L),
    "same shape and column names, different values"
  )
})


# Data identity ----
test_that("decomp() fingerprints the features it decomposed", {
  features <- c("Sepal.Length", "Sepal.Width")
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 1L, features = features),
    verbosity = 0L
  )
  expect_s7_class(decom@data_fingerprint, DataFingerprint)
  # The columns it did not decompose are not data it used.
  expect_identical(decom@data_fingerprint@column_names, features)
  expect_true(same_data(
    decom@data_fingerprint,
    data_fingerprint(as.matrix(as.data.frame(x)[, features, drop = FALSE]))
  ))
})


test_that("the fingerprint survives the container the data arrived in", {
  # An "object" hash is taken over serialized bytes, and a data.frame and the
  # data.table it came from order their attributes differently -- so they hash
  # differently while being `identical()`. Reducing to a matrix drops that.
  from_df <- decomp(as.data.frame(x), algorithm = "PCA", verbosity = 0L)
  from_dt <- decomp(
    data.table::as.data.table(x),
    algorithm = "PCA",
    verbosity = 0L
  )
  expect_true(same_data(from_df@data_fingerprint, from_dt@data_fingerprint))
})


test_that("decomp_metrics() reports an `x` of the right shape and wrong data", {
  decom <- decomp(
    x,
    algorithm = "PCA",
    config = setup_PCA(k = 2L),
    verbosity = 0L
  )
  altered <- x
  altered[1L, 1L] <- altered[1L, 1L] + 1
  # A notice, not a condition to catch: the metrics are still returned, because
  # scoring a fit against an altered copy of its input is a legitimate thing.
  expect_message(
    metrics <- decomp_metrics(decom, altered, verbosity = 1L),
    "same shape and column names, different values"
  )
  expect_s7_class(metrics, DecompositionMetrics)
  # The fitted data itself is silent, and `verbosity = 0L` silences the notice.
  expect_silent(decomp_metrics(decom, x, verbosity = 1L))
  expect_silent(decomp_metrics(decom, altered, verbosity = 0L))
})


test_that("decomp() records the data it used in the run record", {
  decom <- decomp(x, algorithm = "PCA", verbosity = 0L)
  provenance <- record(decom)[["provenance"]]
  expect_false(is.null(provenance[["data_training"]]))
  expect_identical(
    provenance[["data_training"]][["hash"]],
    decom@data_fingerprint@hash
  )
})


test_that("decomp() and cluster() fingerprint one dataset identically", {
  skip_if_not_installed("flexclust")
  # Comparing runs across a batch is what a fingerprint is for, so two families
  # given one dataset must record one hash for it -- including when they were
  # handed different containers.
  decom <- decomp(x, algorithm = "PCA", verbosity = 0L)
  clust <- cluster(
    data.table::as.data.table(x),
    algorithm = "KMeans",
    verbosity = 0L
  )
  expect_true(same_data(decom@data_fingerprint, clust@data_fingerprint))
})


test_that("cluster() records the data it used in the run record", {
  skip_if_not_installed("flexclust")
  clust <- cluster(x, algorithm = "KMeans", verbosity = 0L)
  expect_s7_class(clust@data_fingerprint, DataFingerprint)
  expect_identical(
    record(clust)[["provenance"]][["data_training"]][["hash"]],
    clust@data_fingerprint@hash
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
