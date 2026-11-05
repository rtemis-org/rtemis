pr_model_fixture <- function(multiclass = FALSE) {
  cls <- utils::getFromNamespace("Classification", "rtemis")
  y <- if (multiclass) {
    factor(rep(c("A", "B", "C"), 2))
  } else {
    factor(c("no", "yes", "no", "no", "yes", "no"))
  }
  p <- if (multiclass) {
    out <- matrix(.1, length(y), 3, dimnames = list(NULL, levels(y)))
    out[cbind(seq_along(y), as.integer(y))] <- .8
    out
  } else {
    matrix(c(.1, .8, .6, .2, .5, .3), ncol = 1, dimnames = list(NULL, "yes"))
  }
  cls(
    algorithm = "CART",
    hyperparameters = rtemis::setup_CART(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = y,
    predicted_training = y,
    predicted_prob_training = p,
    y_validation = y,
    predicted_validation = y,
    predicted_prob_validation = p,
    y_test = y,
    predicted_test = y,
    predicted_prob_test = p
  )
}

pr_res_model_fixture <- function(multiclass = FALSE) {
  mod <- pr_model_fixture(multiclass)
  cls <- utils::getFromNamespace("ClassificationRes", "rtemis")
  splits <- rtemis::resample(
    data.frame(x = 1:12),
    config = rtemis::setup_KFold(n_resamples = 2L, seed = 1L),
    verbosity = 0L
  )
  cls(
    algorithm = "CART",
    models = list(Fold1 = mod, Fold2 = mod),
    hyperparameters = rtemis::setup_CART(),
    tuner_config = NULL,
    outer_resampler = splits,
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = list(Fold1 = mod@y_training, Fold2 = mod@y_training),
    y_test = list(Fold1 = mod@y_test, Fold2 = mod@y_test),
    predicted_training = list(Fold1 = mod@y_training, Fold2 = mod@y_training),
    predicted_test = list(Fold1 = mod@y_test, Fold2 = mod@y_test),
    predicted_prob_training = list(
      Fold1 = mod@predicted_prob_training,
      Fold2 = mod@predicted_prob_training
    ),
    predicted_prob_test = list(
      Fold1 = mod@predicted_prob_test,
      Fold2 = mod@predicted_prob_test
    )
  )
}

pr_data_of <- function(...) {
  utils::getFromNamespace("pr_plot_data", "rtemis")(...)
}

test_that("plot_pr() draws one stepped curve and baseline per available sample", {
  mod <- pr_model_fixture()
  option <- plot_pr(mod)[["x"]][["option"]]
  expect_identical(
    substr(unlist(option[["legend"]][["data"]]), 1L, 5L),
    c("Train", "Valid", "Test:")
  )
  series <- option[["series"]]
  expect_length(series, 6L)
  expect_identical(
    unique(vapply(series[1:3], `[[`, character(1), "step")),
    "start"
  )
  for (i in 4:6) {
    expect_equal(as.numeric(series[[i]][["data"]][[1L]]), c(0, 1 / 3))
    expect_identical(series[[i]][["name"]], series[[i - 3L]][["name"]])
  }
  expect_identical(
    plot_pr(mod, what = "test")[["x"]][["option"]][["series"]][[1L]][["name"]],
    series[[3L]][["name"]]
  )
  expect_identical(
    plot_pr(mod, legend_position = "top-right")[["x"]][["legendPosition"]],
    "top-right"
  )
})

test_that("plot_pr() validates samples and variants as plot_roc() does", {
  mod <- pr_model_fixture()
  expect_error(plot_pr(mod, what = c("test", "test")), "unique")
  expect_error(plot_pr(mod, what = "bad"), "unique")
  expect_error(plot_pr(mod, variant = "per_resample"), "ordinary")
  mod@predicted_prob_training <- NULL
  expect_error(plot_pr(mod, what = "training"), "no probabilities")
  expect_length(
    plot_pr(mod)[["x"]][["option"]][["legend"]][["data"]],
    2L
  )
  mod@predicted_prob_validation <- NULL
  mod@predicted_prob_test <- NULL
  expect_error(plot_pr(mod), "No predicted probabilities")
})

test_that("records agree with precision-recall computed from the stored predictions", {
  mod <- pr_model_fixture()
  out <- pr_data_of(mod, "test", "aggregate", NULL, FALSE)
  y <- mod@y_test == "yes"
  score <- as.numeric(mod@predicted_prob_test)
  thresholds <- sort(unique(score), decreasing = TRUE)
  precision <- vapply(
    thresholds,
    function(t) sum(y & score >= t) / sum(score >= t),
    numeric(1)
  )
  recall <- vapply(
    thresholds,
    function(t) sum(y & score >= t) / sum(y),
    numeric(1)
  )
  expect_equal(out[["precision"]][-1L], precision)
  expect_equal(out[["recall"]][-1L], recall)
  expect_equal(unique(out[["ap"]]), sum(diff(c(0, recall)) * precision))
  expect_equal(unique(out[["prevalence"]]), mean(y))
  expect_identical(unique(out[["split"]]), "test")
  expect_identical(unique(out[["fold"]]), "aggregate")
})

test_that("the positive class and binary scores keep their identity", {
  mod <- pr_model_fixture()
  positive <- pr_data_of(mod, "test", "aggregate", "no", TRUE)
  expect_identical(unique(positive[["class"]]), "no")
  expect_equal(unique(positive[["prevalence"]]), 2 / 3)
  default <- pr_data_of(mod, "test", "aggregate", NULL, TRUE)
  expect_identical(unique(default[["class"]]), "yes")
  expect_false(isTRUE(all.equal(default[["ap"]][[1L]], positive[["ap"]][[1L]])))
})

test_that("multiclass curves use class color, with one baseline per class and split", {
  mod <- pr_model_fixture(multiclass = TRUE)
  out <- pr_data_of(mod, "test", "aggregate", NULL, TRUE)
  expect_identical(unique(out[["class"]]), c("A", "B", "C"))
  expect_equal(unique(out[["prevalence"]]), 1 / 3)
  expect_equal(unique(out[["ap"]]), 1)
  series <- plot_pr(mod, what = "test")[["x"]][["option"]][["series"]]
  expect_length(series, 6L)
})

test_that("resampled curves pool rows or summarize folds", {
  mod <- pr_res_model_fixture()
  single <- pr_model_fixture()
  pooled <- pr_data_of(mod, "training", "aggregate", NULL, FALSE)
  expect_identical(unique(pooled[["fold"]]), "aggregate")
  # Doubling every case leaves each precision and recall unchanged.
  expect_equal(
    pooled[["precision"]],
    pr_data_of(single, "training", "aggregate", NULL, FALSE)[["precision"]]
  )
  folds <- pr_data_of(mod, "training", "per_resample", NULL, FALSE)
  expect_identical(unique(folds[["fold"]]), c("Fold1", "Fold2"))
  option <- plot_pr(mod, "training", variant = "per_resample")[["x"]][[
    "option"
  ]]
  expect_length(option[["legend"]][["data"]], 1L)
  expect_match(option[["legend"]][["data"]][[1L]], "AP mean")
  # Two fold curves share a name with their one baseline.
  expect_length(option[["series"]], 3L)
  expect_error(plot_pr(mod, what = "validation"), "unique")
})

test_that("a resample without probabilities stays an undefined curve", {
  mod <- pr_res_model_fixture()
  mod@predicted_prob_test[["Fold2"]] <- NULL
  mod@predicted_prob_test <- c(mod@predicted_prob_test, list(Fold2 = NULL))
  folds <- pr_data_of(mod, "test", "per_resample", NULL, FALSE)
  expect_true(all(is.na(folds[folds[["fold"]] == "Fold2", "ap"])))
  expect_false(anyNA(folds[folds[["fold"]] == "Fold1", "ap"]))
  expect_message(
    option <- plot_pr(mod, "test", variant = "per_resample")[["x"]][["option"]],
    "1 undefined curve"
  )
  expect_match(option[["legend"]][["data"]][[1L]], "SD NA")
})

test_that("plot_pr() and plot_roc() select the same samples and folds", {
  for (mod in list(pr_model_fixture(), pr_res_model_fixture())) {
    roc <- utils::getFromNamespace("roc_plot_data", "rtemis")(
      mod,
      "all",
      "aggregate",
      NULL,
      TRUE
    )
    pr <- pr_data_of(mod, "all", "aggregate", NULL, TRUE)
    expect_identical(unique(pr[["split"]]), unique(roc[["split"]]))
    expect_identical(unique(pr[["class"]]), unique(roc[["class"]]))
    expect_identical(unique(pr[["omitted"]]), unique(roc[["omitted"]]))
  }
})

test_that("pr_table() returns the curves plot_pr() draws", {
  mod <- pr_model_fixture()
  curves <- pr_table(mod)
  expect_s3_class(curves, "data.table")
  expect_named(
    curves,
    c("sample", "class", "recall", "precision", "ap", "prevalence", "omitted")
  )
  expect_identical(
    as.character(unique(curves[["sample"]])),
    c("training", "validation", "test")
  )
  test <- pr_table(mod, "test")
  expect_identical(unique(test[["sample"]]), "test")
  drawn <- plot_pr(mod, what = "test")[["x"]][["option"]][["series"]][[1L]]
  expect_equal(
    vapply(drawn[["data"]], `[[`, numeric(1L), 1L),
    test[["recall"]]
  )
  expect_equal(
    vapply(drawn[["data"]], `[[`, numeric(1L), 2L),
    test[["precision"]]
  )
  expect_equal(
    drawn[["data"]][[1L]][[3L]],
    test[["ap"]][[1L]]
  )
  expect_error(
    pr_table(mod, variant = "per_resample"),
    "ordinary"
  )
  expect_error(pr_table(1L), class = "rtemis_type_error")
  expect_error(pr_table(mod, max_points = 1L), class = "rtemis_value_error")
})

test_that("pr_table() labels per-resample curves and pools the aggregate", {
  mod <- pr_res_model_fixture()
  pooled <- pr_table(mod, "training")
  expect_false("resample" %in% names(pooled))
  folds <- pr_table(mod, "training", variant = "per_resample")
  expect_identical(
    names(folds)[1:3],
    c("sample", "resample", "class")
  )
  expect_identical(unique(folds[["resample"]]), c("Fold1", "Fold2"))
})

test_that("max_points keeps curve ends and every curve summary", {
  set.seed(1L)
  y <- factor(sample(c("no", "yes"), 200L, TRUE, c(.8, .2)))
  cls <- utils::getFromNamespace("Classification", "rtemis")
  mod <- cls(
    algorithm = "CART",
    hyperparameters = rtemis::setup_CART(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = y,
    predicted_training = y,
    predicted_prob_training = matrix(
      stats::runif(200L),
      ncol = 1L,
      dimnames = list(NULL, "yes")
    )
  )
  full <- pr_table(mod, "training")
  capped <- pr_table(mod, "training", max_points = 10L)
  expect_lte(nrow(capped), 10L)
  expect_equal(
    capped[["recall"]][c(1L, 10L)],
    full[["recall"]][c(1L, nrow(full))]
  )
  expect_equal(
    capped[["precision"]][c(1L, 10L)],
    full[["precision"]][c(1L, nrow(full))]
  )
  expect_identical(unique(capped[["ap"]]), unique(full[["ap"]]))
  expect_identical(unique(capped[["prevalence"]]), unique(full[["prevalence"]]))
})

test_that("a fitted model draws precision-recall curves", {
  skip_if_not_installed("rpart")
  dat <- iris[iris[["Species"]] != "setosa", ]
  dat[["Species"]] <- droplevels(dat[["Species"]])
  mod <- train(dat, hyperparameters = setup_CART(), verbosity = 0L)
  widget <- plot_pr(mod)
  expect_s3_class(widget, "htmlwidget")
  expect_match(
    widget[["x"]][["option"]][["legend"]][["data"]][[1L]],
    "virginica \\(AP"
  )
  multiclass <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  expect_length(
    plot_pr(multiclass)[["x"]][["option"]][["legend"]][["data"]],
    3L
  )
})
