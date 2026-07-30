# test_Metrics.R
# ::rtemis::
# 2025- EDG rtemis.org

# Regression Data ----
set.seed(2025)
true <- rnorm(500)
predicted <- true + rnorm(500) / 2
predicted2 <- true + rnorm(500) / 2

# RegressionMetrics ----
reg_metrics <- regression_metrics(true, predicted, sample = "Training")
reg_metrics
test_that("regression_metrics() succeeds", {
  expect_s7_class(regression_metrics(true, predicted), RegressionMetrics)
})
reg_metrics2 <- regression_metrics(true, predicted2, sample = "Test")

# Classification Data ----
true_labels <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
predicted_labels <- factor(c("a", "b", "a", "b", "b", "a", "b", "b", "b", "a"))
predicted_prob <- c(0.3, 0.6, 0.45, 0.75, 0.57, 0.3, 0.8, 0.63, 0.62, 0.39)
predicted_prob2 <- c(0.2, 0.52, 0.28, 0.85, 0.64, 0.45, 0.9, 0.78, 0.78, 0.47)

# ClassificationMetrics ----
class_metrics1 <- classification_metrics(
  true_labels,
  predicted_labels,
  predicted_prob,
  sample = "Training"
)
class_metrics2 <- classification_metrics(
  true_labels,
  predicted_labels,
  predicted_prob2,
  sample = "Test"
)

test_that("classification_metrics() succeeds", {
  expect_s7_class(class_metrics1, ClassificationMetrics)
  expect_s7_class(class_metrics2, ClassificationMetrics)
})

# Test that class_metrics2 has higher AUC and lower Brier score than class_metrics1
test_that("classification_metrics() returns correct metrics", {
  expect_true(
    class_metrics2@metrics[["overall"]][["auc"]] >
      class_metrics1@metrics[["overall"]][["auc"]]
  )
  expect_true(
    class_metrics2@metrics[["overall"]][["brier_score"]] <
      class_metrics1@metrics[["overall"]][["brier_score"]]
  )
})

# auc() backend fallback ----
# Regression test: an optional AUC backend that cannot be loaded (e.g. lightAUC
# after an upstream RcppParallel ABI bump) must not abort; auc() falls back to
# ROCR, and returns NaN only when no backend is available.
true_int <- 2L - as.integer(true_labels)

test_that("auc() falls back to ROCR when lightAUC cannot be loaded", {
  skip_if_not_installed("ROCR")
  expected <- auc(true_int, predicted_prob, method = "ROCR")
  local_mocked_bindings(
    auc_backend_available = function(pkg) pkg != "lightAUC"
  )
  expect_no_error(
    fallback <- auc(true_int, predicted_prob, method = "lightAUC")
  )
  expect_equal(fallback, expected)
})

test_that("auc() returns NaN (no abort) when no backend is available", {
  local_mocked_bindings(
    auc_backend_available = function(pkg) FALSE
  )
  expect_no_error(
    result <- auc(true_int, predicted_prob, method = "lightAUC")
  )
  expect_identical(result, NaN)
})

# RegressionMetricsRes ----
res_metrics <- list(mod1 = reg_metrics, mod2 = reg_metrics2)
rmcv <- RegressionMetricsRes(
  sample = "Test",
  res_metrics = res_metrics
)
rmcv
test_that("RegressionMetricsRes() succeeds", {
  expect_s7_class(rmcv, RegressionMetricsRes)
})

# ClassificationMetricsRes ----
res_metrics <- list(mod1 = class_metrics1, mod2 = class_metrics2)
cmcv <- ClassificationMetricsRes(
  sample = "Test",
  confusion_matrix = table(true_labels, predicted_labels),
  res_metrics = res_metrics
)
cmcv

test_that("ClassificationMetricsRes() succeeds", {
  expect_s7_class(cmcv, ClassificationMetricsRes)
})


# %% Typed metrics ----

test_that("per-class metrics name their level in a column, not a row name", {
  # Row names have no row-oriented JSON form: carried as row names, the class
  # labels would be dropped on serialization, leaving unlabelled metrics.
  m <- classification_metrics(iris$Species, iris$Species, sample = "Training")
  expect_identical(m[["class"]][["level"]], levels(iris$Species))
  expect_identical(rownames(m[["class"]]), as.character(seq_len(3L)))
})


test_that("positive_class is NULL, not NA, when there is no positive class", {
  # NULL is the only "unset" value; a bare NA would also make the field logical
  # for multiclass and character for binary.
  multi <- classification_metrics(iris$Species, iris$Species)
  expect_null(multi[["positive_class"]])
  # Declared nullable and optional, so present-and-null is how it is spelled;
  # `list(x = NULL)` keeps the element rather than dropping it.
  expect_true("positive_class" %in% names(multi@metrics))
  two <- factor(c("a", "b", "a", "b"))
  binary <- classification_metrics(two, two)
  expect_type(binary[["positive_class"]], "character")
})


test_that("overall carries only the columns its task defines", {
  # Every column is declared; which are present says what was computed.
  multi <- classification_metrics(iris$Species, iris$Species)
  expect_identical(
    names(multi[["overall"]]),
    c("balanced_accuracy", "f1", "accuracy")
  )
  two <- factor(c("a", "b", "a", "b"))
  binary <- classification_metrics(two, two, predicted_prob = c(.9, .1, .8, .2))
  expect_true(all(
    c("sensitivity", "specificity", "ppv", "npv", "auc", "brier_score") %in%
      names(binary[["overall"]])
  ))
})


test_that("the typed metrics reject a malformed table", {
  m <- classification_metrics(iris$Species, iris$Species)
  bad <- m@metrics
  bad[["overall"]][["accuracy"]] <- 1.5
  expect_error(set_props(m, metrics = bad), "must be <= 1")
  bad <- m@metrics
  bad[["overall"]][["oops"]] <- 1
  expect_error(set_props(m, metrics = bad), "undeclared column")
  bad <- m@metrics
  bad[["overall"]][["accuracy"]] <- NULL
  expect_error(set_props(m, metrics = bad), "missing required column")
  r <- regression_metrics(c(1, 2, 3), c(1.1, 2.1, 2.9))
  bad <- r@metrics
  bad[["mae"]] <- -1
  expect_error(set_props(r, metrics = bad), "must be >= 0")
})


test_that("confusion_long is the stored, row-oriented confusion matrix", {
  m <- classification_metrics(iris$Species, iris$Species)
  long <- m[["confusion_long"]]
  expect_identical(names(long), c("reference", "predicted", "n"))
  expect_identical(nrow(long), 9L)
  expect_type(long[["reference"]], "character")
  expect_type(long[["n"]], "integer")
  expect_identical(sum(long[["n"]]), 150L)
  # The long form is what the class stores and publishes: a `table`'s column
  # names are the outcome levels, so no schema can declare them.
  expect_identical(
    prop_role(ClassificationMetrics@properties$confusion_long),
    "state"
  )
  expect_identical(
    sum(long[["n"]][long[["reference"]] == long[["predicted"]]]),
    sum(diag(m@confusion_matrix))
  )
})


test_that("confusion_matrix is derived from the long form, labels and all", {
  true <- iris$Species
  set.seed(2026L)
  predicted <- factor(sample(levels(true), 150L, replace = TRUE))
  m <- classification_metrics(true, predicted)
  expected <- table(true, predicted)
  names(dimnames(expected)) <- c("Reference", "Predicted")
  # Level order, counts and both dimension names survive the round trip, so the
  # wide view is indistinguishable from the table that was handed in.
  expect_identical(m@confusion_matrix, expected)
  expect_identical(
    prop_role(ClassificationMetrics@properties$confusion_matrix),
    "computed"
  )
})


test_that("a metrics object serializes without the wide confusion matrix", {
  m <- classification_metrics(iris$Species, iris$Species, sample = "Training")
  j <- to_json(m)
  # A `table` has no `asJSON` method; the long form is what travels, and it is
  # the declared property rather than a conversion each consumer repeats.
  expect_false("confusion_matrix" %in% names(j))
  expect_identical(j[["confusion_long"]], m@confusion_long)
  expect_true(jsonlite::validate(
    jsonlite::toJSON(j, auto_unbox = TRUE, na = "null", null = "null")
  ))
})


test_that("accessors reach properties as well as metrics members", {
  # Reaching only into `@metrics` left the confusion matrix -- a property in its
  # own right -- unreachable by the accessor that reaches everything else.
  m <- classification_metrics(iris$Species, iris$Species, sample = "Training")
  expect_identical(m$confusion_matrix, m@confusion_matrix)
  expect_identical(m[["confusion_matrix"]], m@confusion_matrix)
  expect_identical(m$sample, "Training")
  expect_identical(m$overall, m@metrics[["overall"]])
  expect_true(all(
    c("confusion_matrix", "overall", "class") %in% metric_names(m)
  ))
})
