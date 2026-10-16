# test_GetMetrics.R
# ::rtemis::
# 2026- EDG rtemis.org

# Data ----
set.seed(2026)
datr <- data.frame(a = rnorm(120), b = rnorm(120))
datr[["y"]] <- datr[["a"]] * 2 + rnorm(120, sd = 0.3)
datr_train <- datr[1:80, ]
datr_validation <- datr[81:100, ]
datr_test <- datr[101:120, ]

datc <- data.frame(a = rnorm(100), b = rnorm(100))
datc[["y"]] <- factor(
  ifelse(datc[["a"]] + rnorm(100) > 0, "pos", "neg"),
  levels = c("pos", "neg")
)
datc_train <- datc[1:70, ]
datc_test <- datc[71:100, ]

mod_r <- train(
  datr_train,
  dat_validation = datr_validation,
  dat_test = datr_test,
  hyperparameters = setup_GLM(),
  verbosity = 0L
)
mod_r_notest <- train(
  datr_train,
  hyperparameters = setup_GLM(),
  verbosity = 0L
)
mod_c <- train(
  datc_train,
  dat_test = datc_test,
  hyperparameters = setup_GLM(),
  verbosity = 0L
)
mod_c_res <- train(
  datc,
  hyperparameters = setup_GLM(),
  outer_resampling_config = setup_KFold(n_resamples = 3L, seed = 2026L),
  execution_config = setup_SerialExecution(),
  verbosity = 0L
)

# get_metrics<Supervised> ----
test_that("get_metrics() returns the stored Metrics object of each sample", {
  expect_identical(get_metrics(mod_r, "training"), mod_r@metrics_training)
  expect_identical(get_metrics(mod_r, "validation"), mod_r@metrics_validation)
  expect_identical(get_metrics(mod_r, "test"), mod_r@metrics_test)
  expect_s7_class(get_metrics(mod_r, "test"), RegressionMetrics)
  expect_identical(
    get_metrics(mod_r, "test")$rsq,
    mod_r@metrics_test@metrics[["rsq"]]
  )
  expect_s7_class(get_metrics(mod_c, "test"), ClassificationMetrics)
})

test_that("get_metrics() requires sample for a Supervised model", {
  expect_error(
    get_metrics(mod_r),
    "`sample` is required.*'training', 'validation', 'test'",
    class = "rtemis_value_error"
  )
})

test_that("get_metrics() rejects an unknown sample", {
  expect_error(
    get_metrics(mod_r, "testing"),
    "must be one of 'training', 'validation', 'test'.*'testing'",
    class = "rtemis_value_error"
  )
  expect_error(get_metrics(mod_r, c("training", "test")), "sample")
  expect_error(get_metrics(mod_r, 1L), "sample")
})

test_that("get_metrics() names the stored samples when one is absent", {
  expect_error(
    get_metrics(mod_r_notest, "test"),
    "stores no test metrics.*`dat_test`.*'training'",
    class = "rtemis_value_error"
  )
  expect_error(
    get_metrics(mod_r_notest, "validation"),
    "stores no validation metrics",
    class = "rtemis_value_error"
  )
})

test_that("get_metrics() rejects calibrated = TRUE on an uncalibrated model", {
  expect_error(
    get_metrics(mod_c, "test", calibrated = TRUE),
    "requires a calibrated classifier",
    class = "rtemis_value_error"
  )
  expect_error(get_metrics(mod_c, "test", calibrated = NA), "calibrated")
  expect_error(get_metrics(mod_c, "test", calibrated = "yes"), "calibrated")
})

# get_metrics<CalibratedClassification> ----
test_that("get_metrics() returns the calibrated metrics of a calibrated classifier", {
  cal <- calibrate(
    mod_c,
    predicted_probabilities = mod_c@predicted_prob_training,
    true_labels = mod_c@y_training,
    verbosity = 0L
  )
  expect_identical(get_metrics(cal, "test"), cal@metrics_test)
  expect_identical(
    get_metrics(cal, "test", calibrated = TRUE),
    cal@metrics_test_calibrated
  )
  expect_identical(
    get_metrics(cal, "training", calibrated = TRUE),
    cal@metrics_training_calibrated
  )
  expect_error(
    get_metrics(cal, "validation", calibrated = TRUE),
    "stores no validation metrics",
    class = "rtemis_value_error"
  )
})

# get_metrics<SupervisedRes> ----
test_that("get_metrics() returns the MetricsRes object of a resampled model", {
  expect_identical(
    get_metrics(mod_c_res, "training"),
    mod_c_res@metrics_training
  )
  expect_identical(get_metrics(mod_c_res, "test"), mod_c_res@metrics_test)
  expect_s7_class(get_metrics(mod_c_res, "test"), ClassificationMetricsRes)
  expect_identical(
    get_metrics(mod_c_res, "test")$mean_metrics,
    mod_c_res@metrics_test@mean_metrics
  )
})

test_that("get_metrics() rejects the validation sample on a resampled model", {
  expect_error(
    get_metrics(mod_c_res, "validation"),
    "must be one of 'training', 'test'",
    class = "rtemis_value_error"
  )
  expect_error(
    get_metrics(mod_c_res),
    "`sample` is required",
    class = "rtemis_value_error"
  )
})

test_that("get_metric() reads one metric through get_metrics()", {
  expect_identical(
    get_metric(mod_r, "test", "rsq"),
    mod_r@metrics_test@metrics[["rsq"]]
  )
  expect_length(get_metric(mod_c_res, "test", "balanced_accuracy"), 3L)
})

# get_metrics<Clustering> ----
test_that("get_metrics() returns the stored metrics of a clustering", {
  clust <- cluster(
    iris[, -5],
    algorithm = "kmeans",
    config = setup_KMeans(k = 3L),
    verbosity = 0L
  )
  expect_identical(get_metrics(clust), clust@metrics)
  expect_identical(get_metrics(clust, "training"), clust@metrics)
  expect_s7_class(get_metrics(clust), ClusteringMetrics)
  expect_error(
    get_metrics(clust, "test"),
    "stores metrics for the data it was fitted on only",
    class = "rtemis_value_error"
  )
  expect_error(
    get_metrics(clust, calibrated = TRUE),
    "calibrated classifiers only",
    class = "rtemis_value_error"
  )
})

# get_metrics<Decomposition> ----
test_that("get_metrics() returns the stored metrics of a decomposition", {
  pca <- decomp(
    iris[, -5],
    algorithm = "pca",
    config = setup_PCA(),
    verbosity = 0L
  )
  expect_identical(get_metrics(pca), pca@metrics)
  expect_identical(get_metrics(pca, "training"), pca@metrics)
  expect_error(
    get_metrics(pca, "test"),
    "decomp_metrics\\(\\)",
    class = "rtemis_value_error"
  )
})

# `$` and `[[` ----
test_that("`$` and `[[` read properties of result objects by name", {
  expect_identical(mod_r$metrics_test, mod_r@metrics_test)
  expect_identical(mod_r[["y_training"]], mod_r@y_training)
  expect_identical(mod_c_res$metrics_test, mod_c_res@metrics_test)
  expect_identical(mod_c_res[["outer_resampler"]], mod_c_res@outer_resampler)
  expect_identical(
    mod_c_res$metrics_test$res_metrics,
    mod_c_res@metrics_test@res_metrics
  )
  pca <- decomp(
    iris[, -5],
    algorithm = "pca",
    config = setup_PCA(),
    verbosity = 0L
  )
  expect_identical(pca[["metrics"]], pca@metrics)
  expect_identical(pca$transformed, pca@transformed)
})

test_that("`$` and `[[` reject a name that is not a property", {
  expect_error(
    mod_r$metric_test,
    "No property named 'metric_test'.*Available properties:.*metrics_test",
    class = "rtemis_value_error"
  )
  expect_error(mod_c_res[["validation"]], class = "rtemis_value_error")
  expect_error(mod_c_res$metrics_test$mean, class = "rtemis_value_error")
  expect_error(mod_r[[1L]], class = "rtemis_value_error")
})

test_that("`.DollarNames` completes property names", {
  expect_true(
    all(
      c("metrics_training", "metrics_test") %in%
        utils::.DollarNames(mod_c_res, "^metrics")
    )
  )
  expect_identical(
    utils::.DollarNames(mod_r, "^metrics_te"),
    "metrics_test"
  )
})
