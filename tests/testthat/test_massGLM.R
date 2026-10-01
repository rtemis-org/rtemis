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
  # and the plotly volcano and LINAD tables reuse the pair, so a reader moving
  # between them must not have to relearn the palette. They once used the same
  # two colors with opposite meanings. Colors are read back out of the drawn
  # widgets, so re-inverting any one of them fails.
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

  coefs <- massmod@summary[["Coefficient_x1"]]
  pvals <- massmod@summary[["p_value_x1"]]
  volcano <- draw_volcano(coefs, pvals, verbosity = 0L)
  traces <- Filter(
    Negate(is.null),
    lapply(volcano[["x"]][["attrs"]], function(a) a[["marker"]][["color"]])
  )
  # Groups go in as Low, NS, High, so the first and last traces are the
  # negative and positive ends.
  to_hex <- function(x) {
    channels <- as.integer(strsplit(
      gsub("^rgba\\(|,[^,]*\\)$", "", x),
      ","
    )[[1L]])
    toupper(grDevices::rgb(
      channels[[1L]],
      channels[[2L]],
      channels[[3L]],
      maxColorValue = 255
    ))
  }
  expect_identical(to_hex(traces[[1L]]), signs[["negative"]])
  expect_identical(to_hex(traces[[length(traces)]]), signs[["positive"]])
})
