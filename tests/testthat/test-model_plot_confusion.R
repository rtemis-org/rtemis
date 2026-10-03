classification_plot_fixture <- function() {
  cls <- utils::getFromNamespace("Classification", "rtemis")
  f <- function(x) factor(x, levels = c("no", "yes"))
  cls(
    algorithm = "CART",
    hyperparameters = rtemis::setup_CART(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = f(c("yes", "yes", "yes", "no", "no")),
    predicted_training = f(c("yes", "no", "yes", "no", "yes")),
    y_validation = f(c("yes", "no", "no")),
    predicted_validation = f(c("yes", "no", "yes")),
    y_test = f(c("yes", "yes", "no", "no")),
    predicted_test = f(c("yes", "yes", "no", "yes"))
  )
}

classification_res_plot_fixture <- function() {
  mod <- classification_plot_fixture()
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
    y_training = list(Fold1 = mod@y_training, Fold2 = mod@y_validation),
    predicted_training = list(
      Fold1 = mod@predicted_training,
      Fold2 = mod@predicted_validation
    ),
    y_test = list(Fold1 = mod@y_test, Fold2 = mod@y_validation),
    predicted_test = list(
      Fold1 = mod@predicted_test,
      Fold2 = mod@predicted_validation
    )
  )
}

test_that("ordinary classification uses stored counts, best sample, and draw-owned dispatch", {
  mod <- classification_plot_fixture()
  legacy <- S7::method(rtemis::plot_true_pred, S7::S7_class(mod))
  data <- classification_plot_data(mod)
  expect_identical(unique(data[["panel"]]), "Test")
  expected <- mod@metrics_test@confusion_long
  expect_identical(data[names(expected)], expected)
  expect_identical(
    rtemis.draw::draw_confusion(mod@metrics_test)[["x"]],
    rtemis.draw::draw_confusion(expected)[["x"]]
  )
  expect_identical(
    plot_true_pred(mod)[["x"]],
    rtemis.draw::draw_confusion(data)[["x"]]
  )
  expect_identical(
    S7::method(rtemis::plot_true_pred, S7::S7_class(mod)),
    legacy
  )
  # The model report's recorded frequencies are authoritative, as in live.
  mod@predicted_test <- rev(mod@predicted_test)
  expect_identical(classification_plot_data(mod), data)
  ordered <- classification_plot_data(
    mod,
    c("test", "training"),
    labelify = FALSE
  )
  expect_identical(unique(ordered[["panel"]]), c("test", "training"))
  expect_identical(
    unique(classification_plot_data(mod, "all")[["panel"]]),
    c("Training", "Validation", "Test")
  )
  expect_error(plot_true_pred(mod, what = c("training", "training")), "unique")
  expect_error(plot_true_pred(mod, what = "unknown"), "Select unique")
  expect_error(
    rtemis.draw::draw_confusion(mod@metrics_test, y = "yes"),
    "Omit `y`"
  )
  mod@metrics_test <- NULL
  mod@y_test <- NULL
  mod@predicted_test <- NULL
  expect_identical(
    unique(classification_plot_data(mod)[["panel"]]),
    "Validation"
  )
  expect_error(classification_plot_data(mod, "test"), "needs stored counts")
})

test_that("resampled classification uses pooled counts and shares class order across panels", {
  mod <- classification_res_plot_fixture()
  data <- classification_plot_data(mod)
  expect_identical(unique(data[["panel"]]), c("Training", "Test"))
  out <- rtemis.draw:::confusion_data(
    rtemis.draw::resolve(
      rtemis.draw::setup_ConfusionConfig(panel = "panel"),
      data
    ),
    data
  )
  expect_equal(out[["panels"]][[1L]][["total"]], 8)
  expect_equal(out[["panels"]][[2L]][["total"]], 7)
  expect_identical(
    rtemis.draw::draw_confusion(mod@metrics_test)[["x"]],
    rtemis.draw::draw_confusion(mod@metrics_test@confusion_long)[["x"]]
  )
  expect_s3_class(plot_true_pred(mod), "htmlwidget")
  expect_length(plot_true_pred(mod)[["x"]][["option"]][["grid"]], 8L)
  expect_error(plot_true_pred(mod, what = "validation"), "Select unique")
})

test_that("ordinary fallback counts missing pairs and rejects mismatched lengths", {
  mod <- classification_plot_fixture()
  mod@metrics_test <- NULL
  mod@predicted_test[[1L]] <- NA
  data <- classification_plot_data(mod)
  p <- rtemis.draw:::confusion_data(
    rtemis.draw::resolve(
      rtemis.draw::setup_ConfusionConfig(panel = "panel"),
      data
    ),
    data
  )[["panels"]][[1L]]
  expect_equal(p[["total"]], 3)
  expect_equal(p[["omitted"]], 1)
  shortened <- mod@predicted_test[-1L]
  expect_error(
    mod@predicted_test <- shortened,
    "supervised.rows.y_test.predicted_test",
    fixed = TRUE
  )
  # Bypass S7 to retain defensive coverage for malformed legacy objects.
  attr(mod, "predicted_test") <- shortened
  expect_error(classification_plot_data(mod), "equally sized")
})

test_that("pooled reports remain authoritative when raw fold payloads change", {
  mod <- classification_res_plot_fixture()
  before <- classification_plot_data(mod)
  mod@predicted_training <- mod@predicted_training[2:1]
  expect_identical(classification_plot_data(mod), before)
})


test_that("the model filename exports every selected sample once", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  mod <- classification_res_plot_fixture()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  expect_s3_class(plot_true_pred(mod, filename = path), "htmlwidget")
  svg <- readLines(path, warn = FALSE)
  expect_true(any(grepl(">Training</text>", svg, fixed = TRUE)))
  expect_true(any(grepl(">Test</text>", svg, fixed = TRUE)))
  expect_equal(sum(grepl('<path .*ecmeta_ssr_type="chart"', svg)), 28L)
})
