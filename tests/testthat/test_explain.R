# test-explain.R
# ::rtemis::
# 2025 EDG rtemis.org

# Test individual-level explanations

# library(data.table)
# library(rtemis)
# library(testthat)

# Data ----
## Regression Data ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.table(x, g, y)
names(datr) <- c(paste0("Feature_", 1:6), "Outcome")
resr <- resample(datr)
datr_train <- datr[resr$Fold_1, ]
datr_test <- datr[-resr$Fold_1, ]

## Classification Data ----
### binary ----
datc2 <- iris[51:150, ]
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Fold_1, ]
datc2_test <- datc2[-resc2$Fold_1, ]


## GLMET Regression ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(lambda = 0.01)
)

ex_r_glmnet <- explain(
  model = mod_r_glmnet,
  x = features(datr_test[1, ]),
  dat_training = features(datr_train)
)

test_that("explain() on GLMNET Regression succeeds", {
  expect_s3_class(ex_r_glmnet, "shapr")
})


## GLMNET Classification ----
mod_c_glmnet <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(lambda = 0.01)
)

ex_c_glmnet <- explain(
  model = mod_c_glmnet,
  x = features(datc2_test[1, ]),
  dat_training = features(datc2_train)
)

test_that("explain() on GLMNET Classification succeeds", {
  expect_s3_class(ex_c_glmnet, "shapr")
})

## LightRF Regression ----
mod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "LightRF",
  hyperparameters = setup_LightRF(nrounds = 10L)
)

ex_r_lightrf <- explain(
  model = mod_r_lightrf,
  x = features(datr_test[1, ])
)
test_that("explain() on LightRF Regression succeeds", {
  expect_true(is.list(ex_r_lightrf))
})
