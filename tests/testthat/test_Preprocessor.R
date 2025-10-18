# test-Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

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

iris_test_Pre <- preprocess(iris_test, iris_Pre)
test_that("preprocess(x, Preprocessor) succeeds", {
  expect_s7_class(iris_test_Pre, Preprocessor)
})

iris_Pre_too <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE),
  dat_test = iris_test
)
test_that("preprocess(x, PreprocessorConfig) succeeds", {
  expect_s7_class(iris_Pre_too, Preprocessor)
})

test_that("preprocess(x, PreprocessorConfig) and preprocess(x, Preprocessor) give same test set", {
  expect_equal(iris_Pre_too@preprocessed$test, iris_test_Pre@preprocessed)
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
