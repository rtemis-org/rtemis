present_model_fixture <- function(
  classification = FALSE,
  resamples = NULL,
  shift = 0
) {
  if (classification) {
    skip_if_not_installed("rpart")
  }
  data <- if (classification) {
    iris
  } else {
    transform(mtcars[, c("wt", "mpg")], mpg = mpg + shift)
  }
  rtemis::train(
    data,
    dat_test = if (is.null(resamples)) data else NULL,
    hyperparameters = if (classification) {
      rtemis::setup_CART()
    } else {
      rtemis::setup_GLM()
    },
    outer_resampling_config = if (is.null(resamples)) {
      NULL
    } else {
      rtemis::setup_KFold(n_resamples = resamples, seed = 23L)
    },
    execution_config = rtemis::setup_SerialExecution(),
    verbosity = 0L
  )
}

test_that("single-model presentation preserves descriptions and registered methods", {
  for (classification in c(FALSE, TRUE)) {
    for (resamples in list(NULL, 2L)) {
      m <- present_model_fixture(classification, resamples)
      legacy <- S7::method(rtemis::present, S7::S7_class(m))
      expect_identical(present, rtemis::present)
      expect_s3_class(present(m, verbosity = 0L), "htmlwidget")
      expect_identical(legacy, S7::method(rtemis::present, S7::S7_class(m)))
      expect_output(
        present(m, verbosity = 1L),
        if (classification) "CART" else "GLM"
      )
      if (classification && is.null(resamples)) {
        expect_identical(
          present(m, type = "confusion", verbosity = 0L)[["x"]],
          plot_true_pred(m, what = "all")[["x"]]
        )
        expect_identical(
          present(m, type = "ROC", verbosity = 0L)[["x"]],
          plot_roc(m, what = "all")[["x"]]
        )
        m@y_test <- NULL
        m@predicted_test <- NULL
        m@metrics_test <- NULL
        m@predicted_prob_test <- NULL
        expect_s3_class(
          present(m, type = "confusion", verbosity = 0L),
          "htmlwidget"
        )
        expect_error(present(m, type = "bad", verbosity = 0L))
      }
      expect_error(present(m, verbosity = -1L), "nonnegative")
    }
  }
})

test_that("ordinary comparisons use stored metrics and unique aligned labels", {
  m <- present_model_fixture()
  x <- list(Linear = m, Linear = m)
  d <- comparison_data(x)
  expect_equal(d[["models"]], c("Linear", "Linear_1"))
  expect_equal(d[["splits"]], c("Training", "Test"))
  expect_equal(
    d[["data"]][["value"]],
    rep(
      c(m@metrics_training@metrics[["rsq"]], m@metrics_test@metrics[["rsq"]]),
      2
    )
  )
  w <- present(x, verbosity = 0L)[["x"]][["option"]]
  expect_equal(as.character(w[["xAxis"]][["data"]]), c("Linear", "Linear_1"))
  expect_equal(vapply(w[["series"]], `[[`, "", "name"), c("Training", "Test"))
  expect_equal(
    comparison_data(
      x,
      model_names = c("A", "B"),
      what = c("test", "training")
    )[["splits"]],
    c("Test", "Training")
  )
  m@metrics_test <- NULL
  partial <- present(list(m, x[[2]]), verbosity = 0L)[["x"]][["option"]]
  expect_true(is.na(partial[["series"]][[2]][["data"]][[1]]))
  expect_null(partial[["title"]][["subtext"]])
  expect_equal(comparison_data(list(m, m))[["splits"]], "Training")
  expect_error(
    present(list(m, m), what = "test", verbosity = 0L),
    "absent|unavailable"
  )
  expect_error(comparison_data(list(m, m), metric = "bad"), "absent")
  expect_error(comparison_data(list(m, m), model_names = "A"), "one nonempty")
  expect_error(comparison_data(list(m, m), what = c("test", "test")), "unique")
  expect_error(
    present(list(m, m), ylim = c(1, 0), verbosity = 0L),
    "increasing"
  )
})

test_that("resampled comparisons preserve unequal folds and common panel limits", {
  a <- present_model_fixture(resamples = 2L)
  b <- present_model_fixture(resamples = 3L)
  data <- comparison_data(list(A = a, B = b))
  expect_equal(
    as.integer(table(data[["data"]][["model"]])),
    c(4L, 6L)
  )
  w <- present(list(A = a, B = b), verbosity = 0L)
  expect_s3_class(w, "rtemis-panels")
  panels <- w[["x"]][["panels"]]
  expect_length(panels, 2)
  expect_equal(
    panels[[1]][["option"]][["yAxis"]][c("min", "max")],
    panels[[2]][["option"]][["yAxis"]][c("min", "max")]
  )
  for (panel in panels) {
    expect_equal(panel[["option"]][["xAxis"]][["data"]], c("A", "B"))
    expect_length(panel[["option"]][["series"]][[2]][["data"]], 5)
  }
  exact <- present(list(A = a, B = b), verbosity = 0L, ylim = c(-1, 1))[["x"]][[
    "panels"
  ]]
  expect_equal(exact[[1]][["option"]][["yAxis"]][["min"]], -1)
  expect_equal(exact[[2]][["option"]][["yAxis"]][["max"]], 1)
  expect_s3_class(present(list(a), verbosity = 0L), "rtemis-panels")
  folds <- a@metrics_test@res_metrics
  for (i in seq_along(folds)) {
    values <- folds[[i]]@metrics
    values[["rsq"]] <- NA_real_
    folds[[i]]@metrics <- values
  }
  a@metrics_test@res_metrics <- folds
  partial <- present(list(A = a, B = b), verbosity = 0L)[["x"]][["panels"]][[
    2
  ]][["option"]]
  expect_null(partial[["title"]][["subtext"]])
  expect_equal(partial[["xAxis"]][["data"]], c("A", "B"))
  expect_length(partial[["series"]][[2]][["data"]], 3)
  expect_equal(comparison_data(list(a, a))[["splits"]], "Training")
  expect_error(
    present(list(a, a), what = "test", verbosity = 0L),
    "no available"
  )
})

test_that("classification comparisons select recorded overall balanced accuracy", {
  for (resamples in list(NULL, 2L)) {
    model <- present_model_fixture(TRUE, resamples)
    data <- comparison_data(list(A = model, B = model))
    expect_identical(data[["metric"]], "balanced_accuracy")
    expect_s3_class(
      present(list(A = model, B = model), verbosity = 0L),
      "htmlwidget"
    )
    if (is.null(resamples)) {
      expect_equal(
        data[["data"]][["value"]][[1]],
        model@metrics_training@metrics[["overall"]][["balanced_accuracy"]]
      )
    } else {
      expect_equal(
        data[["data"]][["value"]][[1]],
        model@metrics_training@res_metrics[[1]]@metrics[["overall"]][[
          "balanced_accuracy"
        ]]
      )
    }
  }
})

test_that("model comparison validates task and sampling kinds and preserves fingerprint notice", {
  a <- present_model_fixture()
  b <- present_model_fixture(shift = 1)
  cls <- present_model_fixture(TRUE)
  res <- present_model_fixture(resamples = 2L)
  expect_error(comparison_data(list()), "nonempty")
  expect_error(comparison_data(list(a, 1)), "ordinary")
  expect_error(comparison_data(list(a, res)), "ordinary")
  expect_error(comparison_data(list(a, cls)), "same")
  # The producer's notice is intentionally not an R warning.
  expect_message(
    expect_no_warning(present(list(a, b), verbosity = 0L)),
    "not all trained on the same data"
  )
  expect_output(present(list(a, a), verbosity = 1L), "GLM")
})
