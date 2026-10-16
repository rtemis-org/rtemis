# test-model_plot_colors.R
# ::rtemis::
# 2026- EDG rtemis.org

# Expected hexes are literal: training teal, validation purple, test magenta;
# confusion correct blue, incorrect orange.
teal <- "#6CA3A0"
purple <- "#7364F2"
magenta <- "#BE2E5F"

series_colors <- function(widget, field) {
  series <- widget[["x"]][["option"]][["series"]]
  named <- Filter(
    function(s) !is.null(s[["name"]]) && !is.null(field(s)),
    series
  )
  stats::setNames(
    vapply(named, field, ""),
    vapply(named, `[[`, "", "name")
  )
}

color_fixture <- function(classification = FALSE, resamples = NULL) {
  data <- if (classification) {
    d <- iris[51:150, ]
    d[["Species"]] <- droplevels(d[["Species"]])
    d
  } else {
    mtcars[, c("wt", "mpg")]
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
      rtemis::setup_KFold(n_resamples = resamples, seed = 7L)
    },
    execution_config = rtemis::setup_SerialExecution(),
    verbosity = 0L
  )
}

test_that("split_colors() maps each split to one fixed color, case-insensitively", {
  expect_identical(
    split_colors(),
    c(training = teal, validation = purple, test = magenta)
  )
  expect_identical(
    split_colors(c("Test", "training")),
    c(Test = magenta, training = teal)
  )
  expect_error(split_colors("Fold1"), class = "rtemis_value_error")
})

test_that("regression predictions color each split by name", {
  m <- color_fixture()
  point_color <- function(s) {
    if (identical(s[["type"]], "scatter")) s[["itemStyle"]][["color"]]
  }
  colors <- series_colors(graphics::plot(m), point_color)
  expect_identical(unname(colors), c(teal, magenta))
  expect_true(all(startsWith(names(colors), c("Training", "Test"))))
  test_only <- series_colors(plot_true_pred(m, what = "test"), point_color)
  expect_identical(unname(test_only), magenta)
  custom <- series_colors(
    graphics::plot(m, palette = c("#000000", "#111111")),
    point_color
  )
  expect_identical(unname(custom), c("#000000", "#111111"))
})

test_that("resampled metric boxes color each split by name", {
  m <- color_fixture(resamples = 2L)
  boxes <- graphics::plot(m)[["x"]][["option"]][["series"]][[1L]][["data"]]
  expect_identical(
    vapply(boxes, function(b) b[["itemStyle"]][["borderColor"]], ""),
    c(teal, magenta)
  )
  expect_identical(vapply(boxes, `[[`, "", "name"), c("Training", "Test"))
  # A single box carries its color on the series.
  test_only <- plot_metric(m, what = "test")[["x"]][["option"]][["series"]][[
    1L
  ]]
  expect_identical(test_only[["itemStyle"]][["borderColor"]], magenta)
})

test_that("binary ROC curves color each split; multiclass curves color classes", {
  line_color <- function(s) s[["lineStyle"]][["color"]]
  binary <- color_fixture(TRUE)
  colors <- series_colors(plot_roc(binary), line_color)
  expect_identical(unname(colors), c(teal, magenta))
  expect_true(all(startsWith(names(colors), c("Training", "Test"))))
  expect_identical(
    unname(series_colors(plot_roc(binary, what = "test"), line_color)),
    magenta
  )
  multiclass <- rtemis::train(
    iris,
    dat_test = iris,
    hyperparameters = rtemis::setup_CART(),
    execution_config = rtemis::setup_SerialExecution(),
    verbosity = 0L
  )
  curves <- Filter(
    function(s) !is.null(s[["name"]]),
    plot_roc(multiclass)[["x"]][["option"]][["series"]]
  )
  expect_length(curves, 6L)
  colors <- unlist(lapply(curves, line_color))
  expect_false(any(colors %in% c(teal, purple, magenta)))
})

test_that("confusion matrices shade correct cells blue and incorrect cells orange", {
  m <- color_fixture(TRUE)
  ranges <- unlist(lapply(
    graphics::plot(m)[["x"]][["option"]][["visualMap"]],
    function(v) v[["inRange"]][["color"]][[2L]]
  ))
  expect_true(all(c("#2D83D3", "#C55E28") %in% ranges))
  expect_false(any(c(teal, purple, magenta, "#0F6A66") %in% ranges))
})

test_that("ordinary model comparison bars color each split by name", {
  m <- color_fixture()
  series <- present(list(A = m, B = m), verbosity = 0L)[["x"]][["option"]][[
    "series"
  ]]
  expect_identical(vapply(series, `[[`, "", "name"), c("Training", "Test"))
  expect_identical(vapply(series, `[[`, "", "color"), c(teal, magenta))
})
