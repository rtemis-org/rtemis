# test-plot_true_pred.R
# ::rtemis::
# 2026- EDG rtemis.org

# Construct results directly: adapter tests do not need to train a model.
regression_plot_fixture <- function() {
  cls <- utils::getFromNamespace("Regression", "rtemis")
  cls(
    algorithm = "GLM",
    hyperparameters = rtemis::setup_GLM(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = 1:5,
    predicted_training = c(1.2, 1.8, 3.3, 3.8, 5.2),
    y_validation = 2:5,
    predicted_validation = c(2.2, 2.7, 4.3, 4.8),
    y_test = 1:3,
    predicted_test = c(1.4, 2.1, 2.8)
  )
}


regression_res_plot_fixture <- function() {
  mod <- regression_plot_fixture()
  cls <- utils::getFromNamespace("RegressionRes", "rtemis")
  splits <- rtemis::resample(
    data.frame(x = 1:8),
    config = rtemis::setup_KFold(n_resamples = 2L, seed = 1L),
    verbosity = 0L
  )
  cls(
    algorithm = "GLM",
    models = list(Fold1 = mod, Fold2 = mod),
    hyperparameters = rtemis::setup_GLM(),
    tuner_config = NULL,
    outer_resampler = splits,
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = list(1:5, 2:5),
    predicted_training = list(
      c(1.2, 1.8, 3.3, 3.8, 5.2),
      c(2.2, 2.7, 4.3, 4.8)
    ),
    y_test = list(1:3, 2:5),
    predicted_test = list(c(1.4, 2.1, 2.8), c(2.2, 2.7, 4.3, 4.8))
  )
}


test_that("rtemis owns the generic and dispatches on regression results", {
  mod <- regression_plot_fixture()
  expect_identical(plot_true_pred, rtemis::plot_true_pred)
  expect_s3_class(plot_true_pred(mod), "htmlwidget")
  data <- regression_plot_data(mod)
  expect_identical(
    data[["sample"]],
    rep(c("Training", "Validation", "Test"), c(5L, 4L, 3L))
  )
  expect_equal(data[["true"]], c(1:5, 2:5, 1:3))
  w <- plot_true_pred(
    mod,
    what = c("test", "training"),
    labelify = FALSE,
    fit = NULL
  )
  series <- w[["x"]][["option"]][["series"]]
  expect_identical(vapply(series[1:2], `[[`, "", "name"), c("test", "training"))
  expect_equal(lengths(lapply(series[1:2], `[[`, "data")), c(3L, 5L))
})


test_that("all skips unavailable samples while explicit selections are checked", {
  mod <- regression_plot_fixture()
  mod@y_validation <- NULL
  mod@predicted_validation <- NULL
  expect_equal(nrow(regression_plot_data(mod)), 8L)
  expect_error(
    plot_true_pred(mod, what = "validation"),
    "both true and predicted"
  )
  expect_error(plot_true_pred(mod, what = "other"), "sample names")
  expect_error(plot_true_pred(mod, what = c("all", "training")), "sample names")
  expect_error(plot_true_pred(mod, what = c("training", "training")), "unique")
  expect_error(plot_true_pred(mod, labelify = NA))
  mod@predicted_test <- NULL
  expect_error(plot_true_pred(mod), "both true and predicted")
})


test_that("resampled results pool aligned folds without averaging", {
  mod <- regression_res_plot_fixture()
  data <- regression_plot_data(mod)
  expect_identical(data[["sample"]], rep(c("Training", "Test"), c(9L, 7L)))
  expect_equal(data[["true"]], c(1:5, 2:5, 1:3, 2:5))
  expect_equal(
    data[["predicted"]][1:9],
    c(1.2, 1.8, 3.3, 3.8, 5.2, 2.2, 2.7, 4.3, 4.8)
  )
  expect_s3_class(plot_true_pred(mod), "htmlwidget")
  expect_error(plot_true_pred(mod, what = "validation"), "sample names")
  # Matching pooled lengths are insufficient when individual folds differ.
  mod@predicted_training <- list(1:4, 1:5)
  expect_error(regression_plot_data(mod), "same positive length")
})


test_that("paired missing test folds remain aligned", {
  mod <- regression_res_plot_fixture()
  mod@y_test <- list(NULL, 2:5)
  mod@predicted_test <- list(NULL, c(2.2, 2.7, 4.3, 4.8))
  expect_equal(nrow(regression_plot_data(mod, what = "test")), 4L)
  mod@y_test <- list(NULL, NULL)
  mod@predicted_test <- list(NULL, NULL)
  expect_equal(nrow(regression_plot_data(mod)), 9L)
  expect_error(regression_plot_data(mod, what = "test"), "at least one set")
})
