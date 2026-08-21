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
test_that("plot.MassGLM creates plotly object", {
  plt <- plot(massmod)
  expect_s3_class(plt, "plotly")
})

# plot_manhattan.MassGLM ----
test_that("plot_manhattan.MassGLM creates plotly object", {
  plt <- plot_manhattan(massmod)
  expect_s3_class(plt, "plotly")
})


# Sign colors ----
test_that("the volcano and Manhattan plots agree on what a sign looks like", {
  # Both plots color the same MassGLM object by the sign of the same
  # coefficient, so a reader moving between them must not have to relearn the
  # palette. They once used the same two colors with opposite meanings.
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
  expect_identical(
    to_hex(traces[[1L]]),
    toupper(unname(rtemis:::SIGN_COLORS[["negative"]]))
  )
  expect_identical(
    to_hex(traces[[length(traces)]]),
    toupper(unname(rtemis:::SIGN_COLORS[["positive"]]))
  )

  # And the Manhattan plot pairs them the same way round.
  defaults <- formals(rtemis:::plot_manhattan.MassGLM)
  expect_identical(
    eval(defaults[["col_pos"]]),
    rtemis:::SIGN_COLORS[["positive"]]
  )
  expect_identical(
    eval(defaults[["col_neg"]]),
    rtemis:::SIGN_COLORS[["negative"]]
  )
})
