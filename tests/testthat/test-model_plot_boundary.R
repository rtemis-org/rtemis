# test-model_plot_boundary.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("stored metric validation preserves scalar missing values", {
  expect_identical(plot_metric_value(1L), 1)
  expect_identical(plot_metric_value(NA), NA_real_)
  for (bad in list(NULL, numeric(), 1:2, "1", Inf, 1i, matrix(1))) {
    expect_error(plot_metric_value(bad), class = "rtemis_input_error")
  }
})

test_that("the exported MassGLM method has a standard deprecation path", {
  model <- MassGLM(
    summary = data.table::data.table(
      Variable = "Outcome",
      Coefficient_age = 1,
      p_value_age = .01
    ),
    ynames = "Outcome",
    xnames = "age",
    coefnames = "age",
    family = "gaussian"
  )
  condition <- NULL
  widget <- withCallingHandlers(
    plot_manhattan.MassGLM(model),
    deprecatedWarning = function(w) {
      condition <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(condition, "deprecatedWarning")
  expect_identical(condition[["new"]], "plot_manhattan")
  expect_identical(widget[["x"]], plot_manhattan(model)[["x"]])
  expect_identical(plot.MassGLM(model)[["x"]], graphics::plot(model)[["x"]])
})

test_that("Plotly data drawing APIs remain available", {
  skip_if_not_installed("plotly")
  expect_s3_class(draw_scatter(1:3, 1:3), "plotly")
  expect_s3_class(rtemis.draw::draw_scatter(1:3, 1:3), "htmlwidget")
})
