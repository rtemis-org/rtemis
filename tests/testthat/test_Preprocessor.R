# test_Preprocessor.R
# ::rtemis::
# 2025- EDG rtemis.org

# library(testthat)

# PreprocessorConfig ----
prp <- setup_Preprocessor()
prp
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(setup_Preprocessor(), PreprocessorConfig)
})

prp <- setup_Preprocessor(
  remove_constants = TRUE,
  remove_duplicates = TRUE
)
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(prp, PreprocessorConfig)
})

testthat::test_that("integer-typed args accept friendly numeric input", {
  # Every integer-typed property is cleaned, so users need not write `L`.
  # `exclude` is vector-valued, and `c(1, 2)` is a double vector in R.
  expect_identical(setup_Preprocessor(exclude = c(1, 2))@exclude, c(1L, 2L))
  expect_identical(setup_Preprocessor(exclude = 3)@exclude, 3L)
  expect_identical(setup_Preprocessor(numeric_cut_n = 4)@numeric_cut_n, 4L)
  expect_null(setup_Preprocessor()@exclude)
  # Non-integral input is still an error, not a silent truncation.
  expect_error(setup_Preprocessor(exclude = 1.5), class = "rtemis_type_error")
})

# Preprocessor: preprocess(PreprocessorConfig) ----
res <- resample(iris, setup_Resampler(seed = 2025))
iris_train <- iris[res$Fold_1, ]
iris_test <- iris[-res$Fold_1, ]
iris_Pre <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE)
)
test_that("preprocess(x, PreprocessorConfig) succeeds", {
  expect_s7_class(iris_Pre, Preprocessor)
})
iris_Pre
iris_Pre@preprocessed
iris_Pre@values

iris_test_pre <- apply_preprocessor(iris_Pre, iris_test)
test_that("apply_preprocessor(Preprocessor, new_data) returns preprocessed data", {
  expect_s3_class(iris_test_pre, "data.frame")
})

iris_Pre_too <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE),
  dat_test = iris_test
)
test_that("preprocess(x, PreprocessorConfig) succeeds", {
  expect_s7_class(iris_Pre_too, Preprocessor)
})

test_that("preprocess(x, PreprocessorConfig) and apply_preprocessor() give same test set", {
  expect_equal(iris_Pre_too@preprocessed$test, iris_test_pre)
})

# Preprocessor: preprocess(x, ...) with setup_Preprocessor arguments ----
iris_Pre_direct <- preprocess(
  iris_train,
  remove_duplicates = TRUE,
  scale = TRUE,
  center = TRUE
)
test_that("preprocess(x, ...) with direct arguments matches PreprocessorConfig call", {
  expect_s7_class(iris_Pre_direct, Preprocessor)
  expect_equal(iris_Pre_direct@preprocessed, iris_Pre@preprocessed)
})

test_that("preprocess(x) with no preprocessing parameters errors", {
  expect_error(preprocess(iris_train), class = "rtemis_input_error")
})

test_that("preprocess(x, config, ...) with extra setup arguments errors", {
  expect_error(
    preprocess(iris_train, setup_Preprocessor(scale = TRUE), center = TRUE)
  )
})

# impute meanMode ----
x <- iris
# Continuous
x[10:15, 1] <- NA
# Categorical
x[20:25, 5] <- NA
xp <- preprocess(
  x,
  setup_Preprocessor(impute = TRUE, impute_type = "meanMode")
)[["preprocessed"]]

test_that("impute meanMode works", {
  expect_false(anyNA(xp))
})

# Test one_hot ----
n <- 10
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.frame(x, g, y)
datr_onehot <- preprocess(
  datr,
  setup_Preprocessor(one_hot = TRUE)
)[["preprocessed"]]
test_that("one_hot.data.frame works", {
  expect_s3_class(datr_onehot, "data.frame")
})


# %% Preprocessing inside train() ----

test_that("train() preprocesses features, never the outcome", {
  x <- rnormmat(120L, 3L, seed = 3L)
  datr <- data.frame(x, y = 10 * x[, 1L] + 50 + rnorm(120L))
  plain <- train(datr, hyperparameters = setup_GLM(), verbosity = 0L)
  scaled <- train(
    datr,
    preprocessor_config = setup_Preprocessor(scale = TRUE, center = TRUE),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  # Scaling the outcome would silently report error metrics in scaled units --
  # and R-squared, being scale-invariant, would look identical either way.
  expect_identical(scaled@y_training, plain@y_training)
  expect_equal(
    scaled@metrics_training[["rmse"]],
    plain@metrics_training[["rmse"]],
    tolerance = 1e-8
  )
  # Predictions therefore stay in the outcome's units.
  expect_equal(
    mean(predict(scaled, features(datr))),
    mean(datr[["y"]]),
    tolerance = 1
  )
})


test_that("train() rejects a preprocessor that removes cases", {
  datr <- data.frame(a = rnorm(40L), b = rnorm(40L), y = rnorm(40L))
  for (op in PREPROCESSOR_CASE_OPS) {
    config <- do.call(
      setup_Preprocessor,
      stats::setNames(
        list(if (op == "remove_cases_thres") 0.5 else TRUE),
        op
      )
    )
    expect_error(
      train(
        x = datr,
        preprocessor_config = config,
        hyperparameters = setup_GLM(),
        verbosity = 0L
      ),
      "cannot replay",
      info = op
    )
  }
})


test_that("a fitted preprocessor returns one prediction per row", {
  # A case-removing step could not be replayed on new data: asked for n rows,
  # `predict()` must return n predictions.
  datr <- data.frame(a = rnorm(60L), b = rnorm(60L))
  datr[["y"]] <- datr[["a"]] + rnorm(60L)
  mod <- train(
    datr,
    preprocessor_config = setup_Preprocessor(scale = TRUE, center = TRUE),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  newdata <- features(datr)
  expect_length(predict(mod, newdata), nrow(newdata))
})
