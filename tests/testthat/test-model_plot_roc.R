roc_model_fixture <- function(multiclass = FALSE) {
  cls <- utils::getFromNamespace("Classification", "rtemis")
  y <- if (multiclass) {
    factor(rep(c("A", "B", "C"), 2))
  } else {
    factor(c("no", "yes", "no", "yes"))
  }
  p <- if (multiclass) {
    out <- matrix(.1, length(y), 3, dimnames = list(NULL, levels(y)))
    out[cbind(seq_along(y), as.integer(y))] <- .8
    out
  } else {
    matrix(c(.1, .8, .5, .5), ncol = 1, dimnames = list(NULL, "yes"))
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

roc_res_model_fixture <- function(multiclass = FALSE) {
  mod <- roc_model_fixture(multiclass)
  cls <- utils::getFromNamespace("ClassificationRes", "rtemis")
  splits <- rtemis::resample(
    data.frame(x = 1:8),
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

test_that("classification ROC dispatch is draw-owned and selects available probability samples", {
  mod <- roc_model_fixture()
  old <- S7::method(rtemis::plot_roc, S7::S7_class(mod))
  d <- roc_plot_data(mod)
  expect_identical(unique(d[["split"]]), c("Training", "Validation", "Test"))
  expect_equal(unique(d[["auc"]]), .875)
  expect_identical(plot_roc(mod)[["x"]], rtemis.draw::draw_roc(d)[["x"]])
  expect_identical(
    plot_roc(mod, legend_position = "top-right")[["x"]][["legendPosition"]],
    "top-right"
  )
  expect_identical(old, S7::method(rtemis::plot_roc, S7::S7_class(mod)))
  expect_identical(
    unique(roc_plot_data(mod, c("test", "training"), labelify = FALSE)[[
      "split"
    ]]),
    c("test", "training")
  )
  expect_error(plot_roc(mod, what = c("test", "test")), "unique")
  expect_error(plot_roc(mod, what = "bad"), "unique")
  expect_error(plot_roc(mod, variant = "per_resample"), "ordinary")
  mod@predicted_prob_training <- NULL
  expect_identical(
    unique(roc_plot_data(mod)[["split"]]),
    c("Validation", "Test")
  )
  expect_error(plot_roc(mod, what = "training"), "no probabilities")
  mod@predicted_prob_validation <- NULL
  mod@predicted_prob_test <- NULL
  expect_error(plot_roc(mod), "No predicted probabilities")
})

test_that("resampled ROC pools matrix rows with class and fold identity intact", {
  mod <- roc_res_model_fixture(TRUE)
  d <- roc_plot_data(mod)
  expect_identical(unique(d[["class"]]), c("A", "B", "C"))
  expect_identical(unique(d[["fold"]]), "aggregate")
  expect_equal(unique(d[["auc"]]), 1)
  expected <- rtemis.draw::roc_vertices(rtemis.draw::roc_probabilities(
    factor(
      rep(as.character(mod@y_training[[1]]), 2),
      levels = c("A", "B", "C")
    ),
    do.call(rbind, mod@predicted_prob_training)
  ))
  expect_equal(d[d[["split"]] == "Training", names(expected)], expected)
  # Independently reorder named fold lists and class columns.
  mod@predicted_prob_training <- rev(lapply(
    mod@predicted_prob_training,
    function(p) p[, c(3, 1, 2)]
  ))
  expect_identical(roc_plot_data(mod), d)
  fold <- roc_plot_data(mod, "training", "per_resample")
  expect_identical(unique(fold[["fold"]]), c("Fold1", "Fold2"))
  expect_length(
    plot_roc(mod, "training", variant = "per_resample")[["x"]][["option"]][[
      "series"
    ]],
    7
  )
  expect_error(plot_roc(mod, what = "validation"), "unique")
  expect_error(
    roc_align_folds(list(a = 1, b = 2), c("Fold1", "Fold2")),
    "match"
  )
  expect_error(roc_align_folds(list(1), c("Fold1", "Fold2")), "one label")
  expect_length(roc_align_folds(NULL, c("Fold1", "Fold2")), 2)
})

test_that("resampled ROC distinguishes pooled AUC from mean fold AUC and unavailable folds", {
  mod <- roc_res_model_fixture()
  y <- factor(c("no", "yes"), levels = c("no", "yes"))
  mod@y_training <- list(Fold1 = y, Fold2 = y)
  mod@predicted_prob_training <- list(
    Fold1 = matrix(c(.1, .2), 2),
    Fold2 = matrix(c(.8, .9), 2)
  )
  pooled <- roc_plot_data(mod, "training")
  expect_equal(unique(pooled[["auc"]]), .75)
  folds <- roc_plot_data(mod, "training", "per_resample")
  expect_equal(unique(folds[["auc"]]), 1)
  mod@predicted_prob_training <- list(
    Fold1 = matrix(c(.1, .2), 2),
    Fold2 = NULL
  )
  d <- roc_plot_data(mod, "training", "per_resample")
  expect_true(all(is.na(d[["auc"]][d[["fold"]] == "Fold2"])))
  expect_equal(unique(d[["omitted"]][d[["fold"]] == "Fold2"]), 2)
  widget <- plot_roc(mod, "training", variant = "per_resample")[["x"]][[
    "option"
  ]]
  expect_match(widget[["legend"]][["data"]][[1]], "SD NA[)]$")
  expect_null(widget[["title"]][["subtext"]])
  pooled <- roc_plot_data(mod, "training")
  expect_equal(unique(pooled[["omitted"]]), 2)
  expect_equal(unique(pooled[["auc"]]), 1)
})

test_that("binary named score columns preserve the positive class across reversed fold levels", {
  mod <- roc_res_model_fixture()
  exact <- matrix(c(.125, .875, .5, .5), ncol = 1, dimnames = list(NULL, "yes"))
  mod@predicted_prob_training <- list(Fold1 = exact, Fold2 = exact)
  original <- roc_plot_data(mod, "training")
  mod@y_training <- list(
    Fold1 = mod@y_training[[1]],
    Fold2 = factor(as.character(mod@y_training[[2]]), levels = c("yes", "no"))
  )
  p <- mod@predicted_prob_training
  p[[2]] <- 1 - p[[2]]
  colnames(p[[2]]) <- "no"
  mod@predicted_prob_training <- p
  expect_identical(roc_plot_data(mod, "training"), original)
})

test_that("unnamed model score columns follow their producer's class order", {
  mod <- roc_res_model_fixture()
  p <- matrix(c(.125, .875, .5, .5), ncol = 1)
  mod@predicted_prob_training <- list(Fold1 = p, Fold2 = 1 - p)
  mod@y_training <- list(
    Fold1 = mod@y_training[[1]],
    Fold2 = factor(as.character(mod@y_training[[2]]), levels = c("yes", "no"))
  )
  expect_equal(unique(roc_plot_data(mod, "training")[["auc"]]), .875)
  ordinary <- roc_model_fixture()
  reversed <- factor(
    as.character(ordinary@y_test),
    levels = c("yes", "no")
  )
  expect_error(
    ordinary@y_test <- reversed,
    "supervised.levels.y_test.y_training",
    fixed = TRUE
  )
  # Legacy objects can carry a test dictionary distinct from the producer's.
  attr(ordinary, "y_test") <- reversed
  attr(ordinary, "predicted_prob_test") <- matrix(
    1 - c(.125, .875, .5, .5),
    ncol = 1
  )
  d <- roc_plot_data(ordinary, "test")
  expect_identical(unique(d[["class"]]), "yes")
  expect_equal(unique(d[["auc"]]), .875)
})

test_that("empirical ROC agrees with the existing rtemis statistical engine", {
  skip_if_not_installed("pROC")
  y <- factor(c("no", "yes", "no", "yes"), levels = c("no", "yes"))
  p <- c(.1, .8, .5, .5)
  old <- rtemis::roc_curve(y, p)
  new <- rtemis.draw::roc_vertices(rtemis.draw::roc_probabilities(y, p))
  expect_equal(new[c("class", "fpr", "tpr", "auc")], old)
})
