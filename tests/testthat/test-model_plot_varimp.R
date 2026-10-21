# test-plot_varimp.R
# ::rtemis::
# 2026- EDG rtemis.org

varimp_model_fixture <- function(resampled = FALSE, classification = FALSE) {
  cls <- utils::getFromNamespace(
    if (classification) "Classification" else "Regression",
    "rtemis"
  )
  y <- if (classification) factor(c("a", "a", "b", "b")) else 1:4
  mod <- cls(
    algorithm = "GLM",
    hyperparameters = rtemis::setup_GLM(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = c("a", "b"),
    y_training = y,
    predicted_training = y,
    y_test = y,
    predicted_test = y
  )
  mod@varimp <- .varimp_from_table(data.table::data.table(
    variable = c("b", "a"),
    gain = c(2, 8),
    other = c(4, -3)
  ))
  if (!resampled) {
    return(mod)
  }
  res_cls <- utils::getFromNamespace(
    if (classification) "ClassificationRes" else "RegressionRes",
    "rtemis"
  )
  splits <- rtemis::resample(
    data.frame(x = 1:8),
    config = rtemis::setup_KFold(n_resamples = 2L, seed = 1L),
    verbosity = 0L
  )
  res_cls(
    algorithm = "GLM",
    models = list(A = mod, B = mod),
    hyperparameters = rtemis::setup_GLM(),
    tuner_config = NULL,
    outer_resampler = splits,
    execution_config = rtemis::setup_SerialExecution(),
    xnames = c("a", "b"),
    y_training = list(y, y),
    predicted_training = list(y, y),
    y_test = list(y, y),
    predicted_test = list(y, y),
    varimp = list(
      mod@varimp,
      .varimp_from_table(data.table::data.table(
        variable = "a",
        other = 5,
        gain = 4
      ))
    )
  )
}

test_that("draw-owned importance dispatch works for both supervised families", {
  expect_identical(plot_varimp, rtemis::plot_varimp)
  for (classification in c(FALSE, TRUE)) {
    for (resampled in c(FALSE, TRUE)) {
      mod <- varimp_model_fixture(resampled, classification)
      before <- mod@varimp
      records <- varimp_plot_data(mod)
      expect_s3_class(records[["data"]], "data.frame")
      expect_equal(nrow(records[["data"]]), if (resampled) 3 else 2)
      if (resampled) {
        expect_identical(records[["folds"]], c("A", "B"))
        expect_identical(records[["data"]][["fold"]], c("A", "A", "B"))
      } else {
        expect_null(records[["folds"]])
      }
      expect_identical(
        plot_varimp(mod, measure = "other")[["x"]],
        rtemis.draw::draw_varimp(
          records[["data"]],
          measure = "other",
          folds = records[["folds"]],
          title = "GLM variable importance"
        )[["x"]]
      )
      expect_null(plot_varimp(mod, title = NULL)[["x"]][["option"]][["title"]])
      expect_identical(mod@varimp, before)
    }
  }
})

test_that("fold extraction aligns named measures and retains unavailable folds", {
  mod <- varimp_model_fixture(TRUE)
  vi <- .varimp_from_table
  mod@varimp <- list(
    mod@varimp[[1L]],
    vi(data.table::data.table(variable = "a", another = 20))
  )
  data <- varimp_plot_data(mod)[["data"]]
  expect_identical(
    names(data),
    c("variable", "gain", "other", "fold", "another")
  )
  expect_equal(data[["gain"]], c(2, 8, NA))
  expect_equal(data[["another"]], c(NA, NA, 20))
  expect_error(plot_varimp(mod, measure = "unknown"), "must be one of")
  mod@varimp <- list(NULL, mod@varimp[[1L]])
  records <- varimp_plot_data(mod)
  expect_identical(records[["folds"]], c("A", "B"))
  expect_identical(records[["data"]][["fold"]], c("B", "B"))
  option <- plot_varimp(mod)[["x"]][["option"]]
  expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(2, 8))
  expect_identical(
    option[["yAxis"]][["data"]],
    c("b", "a")
  )
})

test_that("an omitted row is an unavailable score for every algorithm", {
  mod <- varimp_model_fixture(TRUE)
  vi <- .varimp_from_table
  for (algorithm in c("CART", "LightGBM", "GLM")) {
    mod@algorithm <- algorithm
    mod@varimp <- list(
      vi(data.table::data.table(variable = c("a", "b"), Gain = c(8, 2))),
      vi(data.table::data.table(variable = "b", Gain = 4))
    )
    option <- plot_varimp(mod)[["x"]][["option"]]
    expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(3, 8))
    expect_identical(option[["yAxis"]][["data"]], c("b", "a"))
    expect_error(plot_varimp(mod, absent = "zero"), "unused argument")
  }
})

test_that("missing importance and inconsistent fold records fail informatively", {
  mod <- varimp_model_fixture()
  mod@varimp <- NULL
  expect_error(
    plot_varimp(mod),
    "No variable importance",
    class = "rtemis_null_input"
  )
  mod <- varimp_model_fixture(TRUE)
  mod@varimp <- list(NULL, NULL)
  expect_error(plot_varimp(mod), "No variable importance")
  mod <- varimp_model_fixture(TRUE)
  mod@varimp <- mod@varimp[1L]
  expect_error(varimp_plot_data(mod), "one importance result per resample")
})

test_that("model distributions preserve producer omissions and legacy dispatch", {
  for (classification in c(FALSE, TRUE)) {
    mod <- varimp_model_fixture(TRUE, classification)
    original <- S7::method(rtemis::plot_varimp, S7::S7_class(mod))
    before <- mod@varimp
    records <- varimp_plot_data(mod)
    expect_identical(
      suppressMessages(plot_varimp(mod, type = "boxplot"))[["x"]],
      suppressMessages(rtemis.draw::draw_varimp(
        records[["data"]],
        type = "boxplot",
        folds = records[["folds"]],
        title = "GLM variable importance"
      ))[["x"]]
    )
    expect_identical(
      original,
      S7::method(rtemis::plot_varimp, S7::S7_class(mod))
    )
    expect_identical(before, mod@varimp)
    # Explicit NA, omitted rows, and entire unavailable folds are unavailable
    # scores. List names do not reorder producer IDs.
    vi <- .varimp_from_table
    mod@algorithm <- "CART"
    mod@varimp <- list(
      B = vi(data.table::data.table(
        variable = c("a", "b"),
        importance = c(8, NA)
      )),
      A = vi(data.table::data.table(variable = "b", importance = 4))
    )
    opt <- suppressMessages(plot_varimp(mod, type = "boxplot"))[["x"]][[
      "option"
    ]]
    expect_length(opt[["series"]][[2]][["data"]], 2)
    expect_setequal(
      vapply(opt[["series"]][[2]][["data"]], function(p) p[["value"]][[3]], ""),
      c("A", "B")
    )
    mod@varimp <- list(NULL, mod@varimp[[2]])
    opt <- suppressMessages(plot_varimp(mod, type = "boxplot"))[["x"]][[
      "option"
    ]]
    expect_length(opt[["series"]][[2]][["data"]], 1)
    expect_equal(opt[["yAxis"]][["data"]], list("b"))
    expect_null(opt[["title"]][["subtext"]])
  }
  expect_error(
    plot_varimp(varimp_model_fixture(), type = "boxplot"),
    "fold.*column"
  )
})


test_that("a SHAP explanation plots its mean absolute contributions", {
  x <- data.frame(age = rnorm(60), bmi = rnorm(60))
  x[["y"]] <- x[["age"]] * 2 + rnorm(60, sd = 0.3)
  mod <- train(x, hyperparameters = setup_GLM(), verbosity = 0L)
  shap <- explain(
    mod,
    features(x)[1:10, ],
    background = features(x),
    verbosity = 0L
  )
  records <- varimp_plot_data(shap)
  expect_null(records[["folds"]])
  expect_identical(records[["data"]][["variable"]], c("age", "bmi"))
  expect_equal(
    records[["data"]][[names(shap@phi)]],
    unname(colMeans(abs(shap@phi[[1L]])))
  )
  option <- plot_varimp(shap)[["x"]][["option"]]
  expect_setequal(option[["yAxis"]][["data"]], c("age", "bmi"))
})


test_that("varimp_ranking() ranks each measure by its direction and the mean", {
  for (classification in c(FALSE, TRUE)) {
    ranking <- varimp_ranking(varimp_model_fixture(TRUE, classification))
    expect_identical(
      names(ranking),
      c("measure", "variable", "fold", "value", "summary", "rank")
    )
    expect_identical(ranking[["measure"]], rep(c("gain", "other"), each = 3))
    expect_identical(ranking[["variable"]], c("a", "a", "b", "b", "a", "a"))
    expect_identical(ranking[["fold"]], c("A", "B", "A", "A", "A", "B"))
    expect_equal(ranking[["value"]], c(8, 4, 2, 4, -3, 5))
    expect_equal(ranking[["summary"]], c(6, 6, 2, 4, 1, 1))
    expect_identical(ranking[["rank"]], c(1L, 1L, 2L, 1L, 2L, 2L))

    single <- varimp_ranking(varimp_model_fixture(FALSE, classification))
    expect_identical(
      names(single),
      c("measure", "variable", "value", "summary", "rank")
    )
    expect_identical(single[["variable"]], c("a", "b", "b", "a"))
    expect_equal(single[["summary"]], single[["value"]])
    expect_identical(single[["rank"]], c(1L, 2L, 1L, 2L))
  }
})

test_that("varimp_ranking() agrees with the order plot_varimp() draws", {
  mod <- varimp_model_fixture(TRUE)
  ranking <- varimp_ranking(mod)
  for (measure in c("gain", "other")) {
    rows <- ranking[ranking[["measure"]] == measure, ]
    option <- plot_varimp(mod, measure = measure)[["x"]][["option"]]
    expect_identical(
      rev(unlist(option[["yAxis"]][["data"]])),
      unique(rows[["variable"]])
    )
  }
})

test_that("varimp_ranking() orders by smaller values and omits unavailable scores", {
  mod <- varimp_model_fixture(TRUE)
  ns <- asNamespace("rtemis")
  pvalue <- function(variable, value) {
    ns[["VariableImportance"]](
      measures = list(
        p = ns[["importance_measure"]](
          variable,
          value,
          kind = "model_selection",
          direction = "smaller",
          description = "Test measure."
        )
      )
    )
  }
  mod@varimp <- list(
    pvalue(c("a", "b"), c(0.5, 0.01)),
    pvalue(c("a", "b"), c(NA, 0.03))
  )
  ranking <- varimp_ranking(mod)
  expect_identical(ranking[["variable"]], c("b", "b", "a"))
  expect_equal(ranking[["summary"]], c(0.02, 0.02, 0.5))
  expect_identical(ranking[["rank"]], c(1L, 1L, 2L))
  mod@varimp <- NULL
  expect_error(varimp_ranking(mod), "No variable importance")
})
