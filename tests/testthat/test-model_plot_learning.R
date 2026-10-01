# test-plot_learning.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("learning methods dispatch on fitted regression and classification objects", {
  expect_identical(plot_learning, rtemis::plot_learning)
  for (outcome in list(
    c(1, 1, 2, 2, 5, 5, 8, 8),
    factor(rep(c("a", "b"), each = 4))
  )) {
    model <- rtemis::train(
      data.frame(x = 1:8, y = outcome),
      hyperparameters = rtemis::setup_LINAD(max_leaves = 3L),
      verbosity = 0L
    )
    curve <- rtemis::get_learning_curve(model)
    expect_s3_class(curve, "data.frame")
    drawing <- plot_learning(model)
    expect_s3_class(drawing, "htmlwidget")
    expect_identical(
      drawing[["x"]],
      rtemis.draw::draw_learning_curve(curve, title = "LINAD learning curve")[[
        "x"
      ]]
    )
    expect_identical(
      drawing[["x"]][["option"]][["xAxis"]][["name"]],
      "Leaves"
    )
    custom <- plot_learning(model, title = "Custom", xlab = "Size", width = 700)
    expect_identical(custom[["x"]][["option"]][["title"]][["text"]], "Custom")
    expect_identical(custom[["x"]][["option"]][["xAxis"]][["name"]], "Size")
    expect_equal(custom[["width"]], 700)
    expect_null(plot_learning(model, title = NULL)[["x"]][["option"]][[
      "title"
    ]])
  }
})

test_that("the model method aggregates the forest extractor's per-tree curves", {
  # Exercise the actual forest extractor with small fitted trees; verify its
  # aggregation independently using the long table it returns.
  model <- rtemis::train(
    data.frame(x = 1:30, y = rep(c(1, 4, 2), each = 10)),
    hyperparameters = rtemis::setup_LINADForest(n_trees = 2L, max_leaves = 3L),
    execution_config = rtemis::setup_SerialExecution(seed = 17L),
    verbosity = 0L
  )
  curve <- rtemis::get_learning_curve(model)
  expect_true("tree" %in% names(curve))
  drawing <- plot_learning(model)
  option <- drawing[["x"]][["option"]]
  training <- option[["series"]][[1L]][["data"]]
  expected <- vapply(
    sort(unique(curve[["iteration"]])),
    function(step) {
      mean(curve[["loss_training"]][curve[["iteration"]] == step], na.rm = TRUE)
    },
    numeric(1L)
  )
  expect_equal(vapply(training, `[[`, 0, 2L), expected)
  expect_identical(option[["title"]][["text"]], "LINADForest learning curve")
})

test_that("models without recorded curves fail with a corrective message", {
  cls <- utils::getFromNamespace("Regression", "rtemis")
  model <- cls(
    algorithm = "GLM",
    hyperparameters = rtemis::setup_GLM(),
    execution_config = rtemis::setup_SerialExecution(),
    xnames = "x",
    y_training = 1:3,
    predicted_training = 1:3
  )
  expect_error(
    plot_learning(model),
    "GLM records no learning curve",
    class = "rtemis_unsupported_error"
  )
})
