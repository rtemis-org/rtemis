# test_LightRF.R
# ::rtemis::
# 2026- EDG rtemis.org

# LightGBM's random-forest mode scores a tree that cannot split as 0 instead of
# the initial score; `score_unsplit_lightrf_trees()` rescores such trees. These
# tests pin the upstream behavior (so a LightGBM fix shows up here and the
# workaround can go) and prove the rescoring is exactly the intended change.

testthat::skip_if_not_installed("lightgbm")

# %% helpers ----
.rf_params <- function(objective, ...) {
  c(
    list(
      objective = objective,
      boosting = "rf",
      bagging_fraction = 0.623,
      bagging_freq = 1L,
      feature_fraction = 0.33,
      min_data_in_leaf = 20L,
      num_leaves = 4096L,
      num_threads = 1L,
      verbose = -1L
    ),
    list(...)
  )
}

.rf_fit <- function(x, y, params, weight = NULL, nrounds = 100L) {
  data <- lightgbm::lgb.Dataset(x, label = y, weight = weight)
  model <- lightgbm::lgb.train(
    params = params,
    data = data,
    nrounds = nrounds,
    verbose = -1L
  )
  list(model = model, data = data)
}

.n_single <- function(model) {
  txt <- model[["save_model_to_string"]]()
  n_leaves <- as.integer(regmatches(
    txt,
    gregexpr("(?<=\\nnum_leaves=)[0-9]+", txt, perl = TRUE)
  )[[1L]])
  c(single = sum(n_leaves == 1L), total = length(n_leaves))
}

set.seed(2026)
.N <- 400L
.X <- matrix(rnorm(.N * 5L), .N, 5L)
.Y <- 100 + .X[, 1L] + rnorm(.N)


# %% Upstream behavior ----
test_that("LightGBM's rf mode scores a tree that cannot split as 0", {
  # If this fails, LightGBM scores such trees correctly and the rescoring in
  # train_LightRF.R can be removed.
  fit <- .rf_fit(.X[1:22, ], .Y[1:22], .rf_params("regression"))
  expect_identical(.n_single(fit[["model"]])[["single"]], 100L)
  expect_lt(abs(mean(predict(fit[["model"]], .X[1:22, ]))), 1)
})


# %% Rescoring ----
test_that("rescoring adds exactly the single-leaf trees' share of the training mean", {
  n <- 64L
  params <- .rf_params("regression")
  fit <- .rf_fit(.X[1:n, ], .Y[1:n], params)
  counts <- .n_single(fit[["model"]])
  # A partial case: some trees split, some do not.
  expect_gt(counts[["single"]], 0L)
  expect_lt(counts[["single"]], counts[["total"]])
  before <- predict(fit[["model"]], .X[1:n, ])
  patched <- score_unsplit_lightrf_trees(
    fit[["model"]],
    fit[["data"]],
    params,
    verbosity = 0L
  )
  after <- predict(patched, .X[1:n, ])
  # The forest averages its trees, so each rescored tree adds mean(y) / T.
  expected <- before + counts[["single"]] / counts[["total"]] * mean(.Y[1:n])
  expect_equal(after, expected, tolerance = 1e-6)
  expect_identical(.n_single(patched), counts)
})

test_that("with weights, the added score is the weighted training mean", {
  n <- 22L
  w <- seq(0.5, 2, length.out = n)
  params <- .rf_params("regression")
  fit <- .rf_fit(.X[1:n, ], .Y[1:n], params, weight = w)
  counts <- .n_single(fit[["model"]])
  expect_identical(counts[["single"]], counts[["total"]])
  before <- predict(fit[["model"]], .X[1:n, ])
  patched <- score_unsplit_lightrf_trees(
    fit[["model"]],
    fit[["data"]],
    params,
    verbosity = 0L
  )
  expect_equal(
    predict(patched, .X[1:n, ]),
    before + stats::weighted.mean(.Y[1:n], w),
    tolerance = 1e-6
  )
  # Every tree is a stump, so the forest predicts close to the outcome's center.
  expect_lt(abs(mean(predict(patched, .X[1:n, ])) - mean(.Y[1:n])), 0.5)
})

test_that("binary rescoring adds the log-odds of the base rate", {
  # Probabilities, not `type = "raw"`: LightGBM averages an rf model's trees
  # when it predicts probabilities but sums them for a raw score.
  n <- 50L
  yb <- as.integer(.X[1:n, 1L] + rnorm(n) > 0.8)
  params <- .rf_params("binary")
  fit <- .rf_fit(.X[1:n, ], yb, params)
  counts <- .n_single(fit[["model"]])
  expect_identical(counts[["single"]], counts[["total"]])
  before <- predict(fit[["model"]], .X[1:n, ])
  patched <- score_unsplit_lightrf_trees(
    fit[["model"]],
    fit[["data"]],
    params,
    verbosity = 0L
  )
  expect_equal(
    stats::qlogis(predict(patched, .X[1:n, ])),
    stats::qlogis(before) + stats::qlogis(mean(yb)),
    tolerance = 1e-6
  )
})

test_that("multiclass rescoring adds each class its own log prior", {
  n <- 45L
  yk <- rep(0:2, times = c(10L, 15L, 20L))
  params <- .rf_params("multiclass", num_class = 3L)
  fit <- .rf_fit(.X[1:n, ], yk, params, nrounds = 20L)
  counts <- .n_single(fit[["model"]])
  expect_identical(counts[["single"]], counts[["total"]])
  before <- matrix(predict(fit[["model"]], .X[1:n, ]), nrow = n)
  patched <- score_unsplit_lightrf_trees(
    fit[["model"]],
    fit[["data"]],
    params,
    verbosity = 0L
  )
  after <- matrix(predict(patched, .X[1:n, ]), nrow = n)
  # Softmax probabilities identify the scores up to a constant per case, so
  # compare log-probability shifts centered across classes.
  center <- function(m) m - rowMeans(m)
  shift <- center(log(after)) - center(log(before))
  prior <- log(c(10, 15, 20) / n)
  expect_equal(
    shift,
    matrix(prior - mean(prior), nrow = n, ncol = 3L, byrow = TRUE),
    tolerance = 1e-6
  )
})

test_that("a forest whose trees all split is returned unchanged", {
  params <- .rf_params("regression")
  fit <- .rf_fit(.X, .Y, params)
  expect_identical(.n_single(fit[["model"]])[["single"]], 0L)
  expect_identical(
    score_unsplit_lightrf_trees(
      fit[["model"]],
      fit[["data"]],
      params,
      verbosity = 0L
    ),
    fit[["model"]]
  )
})


# %% Through train() ----
test_that("LightRF on a small sample predicts around the outcome, not around 0", {
  mod <- train(
    mtcars[1:22, ],
    hyperparameters = setup_LightRF(),
    verbosity = 0L
  )
  outcome_mean <- mean(mtcars[["carb"]][1:22])
  # Every tree is a stump predicting its bagged sample's mean.
  expect_lt(abs(mean(mod@predicted_training) - outcome_mean), 0.1)
  expect_message(
    train(mtcars[1:22, ], hyperparameters = setup_LightRF(), verbosity = 1L),
    "set min_data_in_leaf to at most 6"
  )
  # Rescored trees survive prediction on new data, and a saved model.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(mod, path)
  expect_equal(
    predict(readRDS(path), mtcars[23:32, -11L], verbosity = 0L),
    predict(mod, mtcars[23:32, -11L], verbosity = 0L)
  )
  expect_lt(
    abs(mean(predict(mod, mtcars[23:32, -11L], verbosity = 0L)) - outcome_mean),
    0.1
  )
})
