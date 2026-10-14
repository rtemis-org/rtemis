# test_MassGLM.R
# ::rtemis::
# 2025- EDG rtemis.org

# library(rtemis)
# library(data.table)
# library(testthat)
set.seed(2022)
n <- 40L
y <- data.table(rnormmat(500, n))
x <- data.table(
  x1 = y[[3]] - y[[5]] + y[[14]] + rnorm(500),
  x2 = y[[21]] + rnorm(500)
)

# massGLM ----
massmod <- massGLM(x, y)
test_that("massGLM creates MassGLM object", {
  expect_s7_class(massmod, MassGLM)
})

# plot.MassGLM ----
test_that("plot.MassGLM creates an rtemis.draw widget", {
  plt <- plot(massmod)
  expect_s3_class(plt, c("rtemis-draw", "htmlwidget"), exact = TRUE)
})

# plot_manhattan.MassGLM ----
test_that("plot_manhattan() on a MassGLM creates an rtemis.draw widget", {
  plt <- plot_manhattan(massmod)
  expect_s3_class(plt, c("rtemis-draw", "htmlwidget"), exact = TRUE)
})


# Sign colors ----
test_that("every plot of a coefficient's sign agrees on what a sign looks like", {
  # The volcano and Manhattan views color the same MassGLM coefficient by sign,
  # and the LINAD tables reuse the pair, so a reader moving between them reads
  # one palette. Colors are read back out of the drawn widgets, so inverting
  # any one of them fails.
  signs <- toupper(rtemis:::sign_colors())
  series_colors <- function(widget) {
    series <- widget[["x"]][["option"]][["series"]]
    series <- Filter(function(s) !is.null(s[["name"]]), series)
    colors <- vapply(series, function(s) s[["itemStyle"]][["color"]], "")
    stats::setNames(toupper(colors), vapply(series, `[[`, "", "name"))
  }
  for (widget in list(
    plot(massmod, coefname = "x1"),
    plot_manhattan(massmod, coefname = "x1")
  )) {
    colors <- series_colors(widget)
    expect_identical(
      colors[["Significant negative"]],
      signs[["negative"]]
    )
    expect_identical(
      colors[["Significant positive"]],
      signs[["positive"]]
    )
  }
})
