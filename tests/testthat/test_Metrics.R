# test_Metrics.R
# ::rtemis::
# 2025 EDG rtemis.org

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
    class_metrics2@metrics[["Overall"]][["AUC"]] >
      class_metrics1@metrics[["Overall"]][["AUC"]]
  )
  expect_true(
    class_metrics2@metrics[["Overall"]][["Brier_Score"]] <
      class_metrics1@metrics[["Overall"]][["Brier_Score"]]
  )
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
  res_metrics = res_metrics
)
cmcv

test_that("ClassificationMetricsRes() succeeds", {
  expect_s7_class(cmcv, ClassificationMetricsRes)
})
