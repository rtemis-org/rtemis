# test_fit_predict.R
# ::rtemis::
# 2026- EDG rtemis.org

fit_predict_x <- data.frame(x = seq(-2, 2, length.out = 60))
fit_predict_y <- fit_predict_x[["x"]]^2 +
  stats::rnorm(60, sd = .1) * seq_len(60) / 60
fit_predict_new <- data.frame(x = c(-1, 0, 1))

test_that("fit_predict() matches stats::glm predictions and standard errors", {
  fit <- fit_predict(
    fit_predict_x,
    fit_predict_y,
    fit_predict_new,
    algorithm = "glm",
    se = TRUE
  )
  reference <- stats::predict(
    stats::glm(y ~ x, data = data.frame(fit_predict_x, y = fit_predict_y)),
    newdata = fit_predict_new,
    se.fit = TRUE
  )
  expect_identical(fit[["algorithm"]], "GLM")
  expect_equal(fit[["fitted"]], unname(reference[["fit"]]))
  expect_equal(fit[["se"]], unname(reference[["se.fit"]]))
  expect_true(is.numeric(fit[["rsq"]]) && length(fit[["rsq"]]) == 1L)
})

test_that("fit_predict() fits any learner by name, without standard errors", {
  fit <- fit_predict(
    fit_predict_x,
    fit_predict_y,
    fit_predict_new,
    algorithm = "cart",
    se = TRUE
  )
  expect_identical(fit[["algorithm"]], "CART")
  expect_length(fit[["fitted"]], 3L)
  expect_null(fit[["se"]])
  expect_gt(fit[["rsq"]], .5)
})

test_that("fit_predict() fits two features and passes params to setup", {
  x <- data.frame(a = rep(1:6, 6), b = rep(1:6, each = 6))
  fit <- fit_predict(
    x,
    x[["a"]] * x[["b"]],
    data.frame(a = c(1, 6), b = c(1, 6)),
    algorithm = "CART",
    params = list(maxdepth = 1)
  )
  expect_length(unique(fit[["fitted"]]), 2L)
  expect_null(fit[["se"]])
})

test_that("fit_params_hyperparameters() calls the setup function", {
  hp <- fit_params_hyperparameters("linad", list(max_leaves = 4))
  expect_identical(hp, setup_LINAD(max_leaves = 4))
  expect_identical(fit_params_hyperparameters("GLM", NULL), setup_GLM())
})

test_that("fit_predict() rejects malformed inputs and unknown algorithms", {
  expect_error(
    fit_predict(fit_predict_x[["x"]], fit_predict_y, fit_predict_new, "GLM"),
    class = "rtemis_type_error"
  )
  expect_error(
    fit_predict(fit_predict_x, fit_predict_y[-1], fit_predict_new, "GLM"),
    class = "rtemis_type_error"
  )
  expect_error(
    fit_predict(fit_predict_x, fit_predict_y, data.frame(z = 1), "GLM"),
    class = "rtemis_value_error"
  )
  expect_error(
    fit_predict(fit_predict_x, fit_predict_y, fit_predict_new, "nonesuch"),
    class = "rtemis_input_error"
  )
  expect_error(
    fit_predict(
      fit_predict_x,
      fit_predict_y,
      fit_predict_new,
      "LINAD",
      params = list(max_leafs = 4)
    ),
    "does not take `max_leafs`",
    class = "rtemis_value_error"
  )
  expect_error(
    fit_predict(
      fit_predict_x,
      fit_predict_y,
      fit_predict_new,
      "LINAD",
      params = list(4)
    ),
    class = "rtemis_type_error"
  )
  # Values are validated by the setup function itself.
  expect_error(
    fit_predict(
      fit_predict_x,
      fit_predict_y,
      fit_predict_new,
      "LINAD",
      params = list(max_leaves = -1)
    )
  )
})
