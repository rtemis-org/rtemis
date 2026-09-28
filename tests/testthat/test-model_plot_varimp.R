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
  vi <- utils::getFromNamespace("VariableImportance", "rtemis")
  mod@varimp <- vi(data.table::data.table(
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
      vi(data.table::data.table(variable = "a", other = 5, gain = 4))
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
  vi <- utils::getFromNamespace("VariableImportance", "rtemis")
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
  option <- plot_varimp(mod, absent = "zero")[["x"]][["option"]]
  expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(2, 8))
  expect_identical(
    option[["yAxis"]][["data"]],
    c("b", "a")
  )
})

test_that("only verified sparse producer measures imply structural zeros", {
  mod <- varimp_model_fixture(TRUE)
  vi <- utils::getFromNamespace("VariableImportance", "rtemis")
  for (case in list(
    c("CART", "importance"),
    c("LightGBM", "Gain"),
    c("LightGBM", "Cover"),
    c("LightGBM", "Frequency")
  )) {
    mod@algorithm <- case[[1L]]
    first <- data.table::data.table(variable = c("a", "b"), score = c(8, 2))
    second <- data.table::data.table(variable = "b", score = 4)
    data.table::setnames(first, "score", case[[2L]])
    data.table::setnames(second, "score", case[[2L]])
    mod@varimp <- list(vi(first), vi(second))
    option <- plot_varimp(mod)[["x"]][["option"]]
    expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(3, 4))
    expect_identical(option[["yAxis"]][["data"]], c("b", "a"))
    option <- plot_varimp(mod, absent = "missing")[["x"]][["option"]]
    expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(3, 8))
  }
  mod@algorithm <- "Unknown"
  option <- plot_varimp(mod)[["x"]][["option"]]
  expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(3, 8))
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
    # Known sparse measures add only the absent row, preserving explicit NA
    # and entire unavailable folds. List names do not reorder producer IDs.
    vi <- utils::getFromNamespace("VariableImportance", "rtemis")
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
    expect_length(opt[["series"]][[2]][["data"]], 3)
    expect_equal(
      opt[["series"]][[2]][["data"]][[3]][["value"]][2:3],
      list(0, "B")
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
