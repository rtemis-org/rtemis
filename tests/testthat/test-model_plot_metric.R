metric_model_fixture <- function(classification = FALSE) {
  if (classification) {
    skip_if_not_installed("rpart")
  }
  rtemis::train(
    if (classification) iris else mtcars[, c("wt", "mpg")],
    hyperparameters = if (classification) {
      rtemis::setup_CART()
    } else {
      rtemis::setup_GLM()
    },
    outer_resampling_config = rtemis::setup_KFold(n_resamples = 3L, seed = 41L),
    execution_config = rtemis::setup_SerialExecution(),
    verbosity = 0L
  )
}

test_that("metric adapters use stored scalar scores and preserve producer fold order", {
  for (classification in c(FALSE, TRUE)) {
    model <- metric_model_fixture(classification)
    old_generic <- utils::getFromNamespace("plot_metric", "rtemis")
    old_method <- S7::method(old_generic, S7::S7_class(model))
    expect_identical(plot_metric, old_generic)
    data <- metric_plot_data(model)
    metric <- if (classification) "balanced_accuracy" else "rsq"
    expect_equal(unique(data[["metric"]]), metric)
    expect_equal(unique(data[["split"]]), c("Training", "Test"))
    expect_equal(data[["fold"]], rep(model@resample_ids, 2))
    expected <- vapply(
      model@metrics_test@res_metrics,
      function(fold) {
        table <- fold@metrics
        if (classification) {
          table <- table[["overall"]]
        }
        table[[metric]]
      },
      numeric(1L)
    )
    expect_equal(tail(data[["value"]], 3), unname(expected))
    expect_s3_class(plot_metric(model), "htmlwidget")
    expect_identical(S7::method(old_generic, S7::S7_class(model)), old_method)
    expect_equal(
      unique(metric_plot_data(model, c("test", "training"))[["split"]]),
      c("Test", "Training")
    )
    expect_error(plot_metric(model, what = c("test", "test")), "unique")
    expect_error(plot_metric(model, what = "validation"), "sample names")
    expect_error(plot_metric(model, metric = "absent"), "stored metric")
  }
})

test_that("missing fold metrics are retained independently of aggregate summaries", {
  model <- metric_model_fixture()
  folds <- model@metrics_test@res_metrics
  # Names are not identities for res_metrics: its producer contract is positional.
  names(folds) <- rev(model@resample_ids)
  table <- folds[[2]]@metrics
  table[["rsq"]] <- NA_real_
  folds[[2]]@metrics <- table
  model@metrics_test@res_metrics <- folds
  data <- metric_plot_data(model, "test")
  expect_identical(data[["fold"]], model@resample_ids)
  expect_true(is.na(data[["value"]][[2]]))
  w <- suppressMessages(plot_metric(model, "test"))[["x"]][["option"]]
  expect_length(w[["series"]][[2]][["data"]], 2)
  expect_null(w[["title"]][["subtext"]])
  model@metrics_test@res_metrics <- folds[1:2]
  expect_error(plot_metric(model), "one metric report")
})
