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

# The factors deliberately do not lead the frame: a level map subscripted by a
# full-frame column index is correct only when they do.
oh_train <- data.frame(
  a = rnorm(6L),
  g = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c")),
  h = factor(c("x", "y", "x", "y", "x", "y"))
)

test_that("one_hot encodes training data as it always has", {
  pre <- preprocess(
    oh_train,
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  )
  out <- preprocessed(pre)
  expect_identical(names(out), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out[["g_a"]], c(1, 0, 0, 1, 0, 0))
  expect_identical(out[["g_c"]], c(0, 0, 1, 0, 0, 1))
  expect_identical(out[["h_y"]], c(0, 1, 0, 1, 0, 1))

  # The levels are published, one entry per encoded feature.
  expect_identical(
    pre@values[["one_hot_levels"]],
    list(g = c("a", "b", "c"), h = c("x", "y"))
  )
})


test_that("one_hot indexes its level map by feature name", {
  x <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "b", "a"), levels = c("a", "b")),
    h = factor(c("x", "y", "x"), levels = c("x", "y", "z"))
  )
  out <- one_hot(
    x,
    factor_levels = list(g = c("a", "b"), h = c("x", "y", "z")),
    verbosity = 0L
  )
  expect_identical(names(out), c("a", "g_a", "g_b", "h_x", "h_y", "h_z"))
  expect_identical(out[["g_b"]], c(0, 1, 0))

  # A map carrying a key with no matching column is tolerated: `train()`
  # learns one on data that includes the outcome and applies it to features.
  out_extra <- one_hot(
    x,
    factor_levels = list(
      g = c("a", "b"),
      h = c("x", "y", "z"),
      y = c("neg", "pos")
    ),
    verbosity = 0L
  )
  expect_identical(out_extra, out)
})


test_that("apply_preprocessor encodes new data as the training data was encoded", {
  pre <- preprocess(
    oh_train,
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  )

  # New data missing level "a" and holding the rest in a different order.
  # Read off its own levels, "c" would land in the first column here and the
  # third in training, and the frame would be one column narrower.
  reordered <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("c", "b")),
    h = factor(c("y", "x", "y"))
  )
  out <- apply_preprocessor(pre, reordered, verbosity = 0L)
  expect_identical(names(out), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out[["g_a"]], c(0, 0, 0))
  expect_identical(out[["g_c"]], c(1, 0, 1))
  expect_identical(out[["h_y"]], c(1, 0, 1))

  # A level unseen in training has no column to take, so the row is all-zero.
  # `NA` is encoded the same way; the preprocessor has dedicated steps for
  # missingness.
  unseen <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "d", NA), levels = c("a", "d")),
    h = factor(c("x", "x", "x"))
  )
  out_unseen <- apply_preprocessor(pre, unseen, verbosity = 0L)
  expect_identical(names(out_unseen), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out_unseen[["g_a"]], c(1, 0, 0))
  expect_identical(out_unseen[["g_b"]], c(0, 0, 0))
  expect_identical(out_unseen[["g_c"]], c(0, 0, 0))
  expect_identical(out_unseen[["h_y"]], c(0, 0, 0))
})


# Test factor2integer ----
f2i_train <- data.frame(
  a = rnorm(6L),
  g = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c")),
  h = factor(c("x", "y", "x", "y", "x", "y"))
)

test_that("factor2integer codes training data as it always has", {
  # Capturing the levels must not change what `preprocess()` returns: every
  # algorithm that converts factors reads this output.
  out <- preprocessed(preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  ))
  # Codes are integer on both `startat0` branches: a category code indexes an
  # embedding table or a LightGBM category, and a double cannot.
  expect_identical(out[["g"]], as.integer(f2i_train[["g"]]) - 1L)
  expect_identical(out[["h"]], as.integer(f2i_train[["h"]]) - 1L)
  expect_identical(out[["a"]], f2i_train[["a"]])

  out_1based <- preprocessed(preprocess(
    f2i_train,
    setup_Preprocessor(
      factor2integer = TRUE,
      factor2integer_startat0 = FALSE
    ),
    verbosity = 0L
  ))
  expect_identical(out_1based[["g"]], as.integer(f2i_train[["g"]]))
  expect_identical(out_1based[["h"]], as.integer(f2i_train[["h"]]))

  # The levels are now published, one entry per converted feature.
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  )
  expect_identical(
    pre@values[["factor2integer_levels"]],
    list(g = c("a", "b", "c"), h = c("x", "y"))
  )
})


test_that("apply_preprocessor codes new data as the training data was coded", {
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  )

  # New data missing level "a" and holding the rest in a different order.
  # Read off its own levels, "c" would code as 0 here and 2 in training.
  reordered <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("c", "b")),
    h = factor(c("y", "x", "y"))
  )
  out <- apply_preprocessor(pre, reordered, verbosity = 0L)
  expect_identical(out[["g"]], c(2L, 1L, 2L))
  expect_identical(out[["h"]], c(1L, 0L, 1L))

  # A level unseen in training takes the reserved index above the known ones,
  # so an embedding sized at length(levels) + 1 can index it. NA stays NA.
  unseen <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "d", NA), levels = c("a", "d")),
    h = factor(c("x", "x", "x"))
  )
  out_unseen <- apply_preprocessor(pre, unseen, verbosity = 0L)
  expect_identical(out_unseen[["g"]], c(0L, 3L, NA))
})


test_that("scale and center leave factor2integer codes alone", {
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE, scale = TRUE, center = TRUE),
    verbosity = 0L
  )
  out <- preprocessed(pre)

  # Standardizing a category code yields a fraction of an index, so the coded
  # columns are not numeric features as far as scale/center is concerned.
  expect_identical(out[["g"]], as.integer(f2i_train[["g"]]) - 1L)
  expect_identical(out[["h"]], as.integer(f2i_train[["h"]]) - 1L)
  expect_equal(mean(out[["a"]]), 0, tolerance = 1e-12)
  expect_equal(sd(out[["a"]]), 1, tolerance = 1e-12)

  # The learned values cover exactly the features that were scaled. `a` is the
  # only one, which is also the case that would drop a data.frame to a vector
  # were the name check written `names(x[, numeric_index])`.
  expect_identical(names(pre@values[["scale_centers"]]), "a")
  expect_identical(names(pre@values[["scale_coefficients"]]), "a")

  # Replaying the fit must agree with it on which columns are numeric.
  newdata <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("a", "b", "c")),
    h = factor(c("y", "x", "y"), levels = c("x", "y"))
  )
  applied <- apply_preprocessor(pre, newdata, verbosity = 0L)
  expect_identical(applied[["g"]], c(2L, 1L, 2L))
  expect_identical(
    applied[["a"]],
    (newdata[["a"]] - pre@values[["scale_centers"]][["a"]]) /
      pre@values[["scale_coefficients"]][["a"]]
  )
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
