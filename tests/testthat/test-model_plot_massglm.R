massglm_fixture <- function() {
  cls <- utils::getFromNamespace("MassGLM", "rtemis")
  cls(
    summary = data.table::data.table(
      Variable = c("C", "A", "B"),
      Coefficient_age = c(3, -2, .1),
      p_value_age = c(.002, .001, .6),
      Coefficient_sex = c(-1, 2, 0),
      p_value_sex = c(.03, .02, .5)
    ),
    ynames = c("A", "B", "C"),
    xnames = c("age", "sex"),
    coefnames = c("age", "sex"),
    family = "gaussian"
  )
}


test_that("MassGLM extraction aligns summary identities and selects coefficients", {
  model <- massglm_fixture()
  before <- data.table::copy(model@summary)
  out <- massglm_plot_data(model)
  expect_identical(out[["coefname"]], "age")
  expect_identical(out[["data"]][["label"]], c("A", "B", "C"))
  expect_equal(out[["data"]][["estimate"]], c(-2, .1, 3))
  expect_equal(out[["data"]][["p_value"]], c(.001, .6, .002))
  expect_equal(
    massglm_plot_data(model, "sex")[["data"]][["estimate"]],
    c(2, 0, -1)
  )
  expect_identical(model@summary, before)
  for (coefname in list("bad", c("age", "sex"), NA_character_, 1)) {
    expect_error(
      massglm_plot_data(model, coefname),
      "Select one",
      class = "rtemis_input_error"
    )
  }
  model@coefnames <- character()
  expect_error(
    massglm_plot_data(model),
    "no coefficients",
    class = "rtemis_input_error"
  )
})


test_that("MassGLM extraction rejects ambiguous or inconsistent outcome identity", {
  for (kind in c("duplicate", "missing", "extra", "column", "outcomes")) {
    model <- massglm_fixture()
    table <- data.table::copy(model@summary)
    if (kind == "duplicate") {
      table[["Variable"]] <- c("A", "A", "B")
    }
    if (kind == "missing") {
      table[["Variable"]][[1L]] <- NA_character_
    }
    if (kind == "extra") {
      table[["Variable"]][[1L]] <- "unknown"
    }
    if (kind == "column") {
      table[["Coefficient_age"]] <- NULL
    }
    if (kind == "outcomes") {
      model@ynames <- c("A", "A", "C")
    }
    model@summary <- table
    expect_error(massglm_plot_data(model), class = "rtemis_input_error")
  }
})


test_that("plot uses the shared graphics generic and named view stays rtemis-owned", {
  model <- massglm_fixture()
  expect_identical(plot_manhattan, rtemis::plot_manhattan)
  expect_identical(names(formals(plot_manhattan)), c("x", "coefname", "..."))
  expect_identical(
    utils::getS3method("plot", "rtemis::MassGLM"),
    draw_massglm_volcano
  )
  expect_identical(
    graphics::plot(model)[["x"]],
    rtemis.draw::draw_volcano(
      c(-2, .1, 3),
      c(.001, .6, .002),
      c("A", "B", "C"),
      title = "MassGLM: age"
    )[["x"]]
  )
  expect_identical(
    plot_manhattan(model, "sex")[["x"]],
    rtemis.draw::draw_manhattan(
      c(2, 0, -1),
      c(.02, .5, .03),
      c("A", "B", "C"),
      title = "MassGLM: sex"
    )[["x"]]
  )
  expect_null(graphics::plot(model, title = NULL)[["x"]][["option"]][["title"]])
  # S7 installs an S3 bridge for the namespaced S7 class. The old bare-class
  # S3 registration and rtemis's named plotting generic remain unchanged.
  legacy <- utils::getS3method("plot", "MassGLM", optional = TRUE)
  if (!is.null(legacy)) {
    expect_identical(environment(legacy), asNamespace("rtemis"))
  }
  expect_identical(
    environment(S7::method(
      rtemis::plot_manhattan,
      utils::getFromNamespace("MassGLM", "rtemis")
    )),
    asNamespace("rtemis")
  )
})


test_that("both MassGLM methods support the static filename route", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  model <- massglm_fixture()
  for (method in list(graphics::plot, plot_manhattan)) {
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path), add = TRUE)
    expect_s3_class(method(model, filename = path), "htmlwidget")
    svg <- readLines(path, warn = FALSE)
    expect_true(any(grepl("MassGLM: age", svg, fixed = TRUE)))
    expect_equal(
      sum(grepl(
        '<path .*fill="#[[:xdigit:]]{6}".*ecmeta_ssr_type="chart"',
        svg
      )),
      3L
    )
  }
})
