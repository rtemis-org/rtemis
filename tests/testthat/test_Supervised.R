# test_Supervised.R
# ::rtemis::
# EDG rtemis.org

# Key
# {Algorithm}[method]<Class> Further conditions

# Setup ----
# library(rtemis)
# library(testthat)
library(data.table)

# Data ----
## Regression Data ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.table(x, g, y)
resr <- resample(datr)
datr_train <- datr[resr$Fold_1, ]
datr_test <- datr[-resr$Fold_1, ]

## Classification Data ----
### Binary ----
datc2 <- data.frame(
  gn = factor(sample(c("alpha", "beta", "gamma"), 100, replace = TRUE)),
  iris[51:150, ]
)
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Fold_1, ]
datc2_test <- datc2[-resc2$Fold_1, ]

### 3-class ----
datc3 <- iris
resc3 <- resample(datc3)
datc3_train <- datc3[resc3$Fold_1, ]
datc3_test <- datc3[-resc3$Fold_1, ]

### Synthetic binary data where positive class is 10% of the data ----
# set.seed(2025)
# n <- 500
# datc2 <- data.frame(
#   x1 = rnorm(n),
#   x2 = rnorm(n),
#   x3 = rnorm(n),
#   g = factor(sample(c("A", "B"), n, replace = TRUE, prob = c(.1, .9)))
# )
# Binary outcome dependent on x2 and g, with levels "neg" and "pos", where "pos" is 10% of the data
# datc2$y <- factor(ifelse(datc2$x2 > 0 & datc2$g == "A", "pos", "neg"))
# resc2 <- resample(datc2)
# datc2_train <- datc2[resc2$Fold_1, ]
# datc2_test <- datc2[-resc2$Fold_1, ]

# Utils ----
test_that("class_imbalance() works", {
  expect_type(class_imbalance(outcome(datc2)), "double")
})

# --- GLM ------------------------------------------------------------------------------------------
## {GLM}[train]<Regression> ----
mod_r_glm <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glm"
)
test_that("train() GLM Regression succeeds", {
  expect_s7_class(mod_r_glm, Regression)
})
test_that("train() GLM standard errors are available", {
  expect_type(mod_r_glm@se_training, "double")
  expect_type(mod_r_glm@se_test, "double")
})

## {GLM}[train]<Regression> Throw error with missing data ----
datr_train_na <- datr_train
datr_train_na[10:2, 1] <- NA
test_that("train() GLM Regression with missing data throws error", {
  expect_error(
    train(x = datr_train_na, dat_test = datr_test, algorithm = "glm")
  )
})

## {GLM}[predict]<Regression> ----
predicted <- predict(mod_r_glm, features(datr_test))
test_that("predict() GLM Regression succeeds", {
  expect_identical(mod_r_glm@predicted_test, predicted)
  expect_null(dim(predicted))
})

## {GLM}[train]<RegressionRes> ----
resmod_r_glm <- train(
  x = datr,
  algorithm = "glm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res GLM Regression succeeds", {
  expect_s7_class(resmod_r_glm, RegressionRes)
})

## {GLM}[train]<Classification> ----
mod_c_glm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "glm"
)
test_that("train() GLM Classification succeeds", {
  expect_s7_class(mod_c_glm, Classification)
})

## {GLM}[train]<Classification> IFW ----
mod_c_glm_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "glm",
  hyperparameters = setup_GLM(ifw = TRUE)
)
test_that("train() GLM Classification with IFW succeeds", {
  expect_s7_class(mod_c_glm_ifw, Classification)
})

## {GLM}[train]<ClassificationRes> ----
resmod_c_glm <- train(
  x = datc2,
  algorithm = "glm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() GLM ClassificationRes succeeds", {
  expect_s7_class(resmod_c_glm, ClassificationRes)
})

# --- GLMNET ---------------------------------------------------------------------------------------

## {GLMNET}[train]<Regression> ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(lambda = 0.01)
)
test_that("train() GLMNET Regression with fixed lambda succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## {GLMNET}[predict]<Regression> ----
predicted <- predict(mod_r_glmnet, features(datr_test))
test_that("predict() GLMNET Regression succeeds", {
  expect_identical(mod_r_glmnet@predicted_test, predicted)
  expect_null(dim(predicted))
})

## {GLMNET}[train]<Regression> auto-lambda grid search using future ----
test_that(
  paste(
    "train > tune_GridSearch resets future plan after execution",
    "train() GLMNET Regression with auto-lambda grid search using future succeeds"
  ),
  {
    # for local testing only, can't assume multisession or multicore are available
    skip_if_not_installed("futurize")
    # Simulate user has set plan to multisession with 2 workers
    # with(future::plan("multisession", workers = 2L), local = TRUE)
    # Simulate user has set plan to sequential
    with(future::plan("sequential"), local = TRUE)
    # Run train with multicore and 4 workers
    modt_r_glmnet <- train(
      x = datr_train,
      dat_test = datr_test,
      algorithm = "glmnet",
      hyperparameters = setup_GLMNET(alpha = 1),
      parallel_type = "future",
      n_workers = 2L, # Limit to 2 workers for CRAN
      future_plan = "mirai_multisession", # which gets converted to "future.mirai::mirai_multisession"
      verbosity = 2L
    )
    # Check that model trained correctly
    expect_s7_class(modt_r_glmnet, Regression)
    # Check that future plan has been reset to "multisession" with 2 workers
    # expect_equal(rtemis:::identify_plan(), "multisession")
    # Check that future plan has been reset to "sequential"
    expect_equal(rtemis:::identify_plan(), "sequential")
    expect_equal(future::nbrOfWorkers(), 1L)
  }
)


## {GLMNET}[train]<Regression> /\Error sequential with >1 worker ----
test_that("sequential with >1 worker throws error", {
  skip_if_not_installed("futurize")
  expect_error(
    modt_r_glmnet <- train(
      x = datr_train,
      dat_test = datr_test,
      algorithm = "glmnet",
      hyperparameters = setup_GLMNET(alpha = 1),
      parallel_type = "future",
      future_plan = "sequential",
      n_workers = 2L
    )
  )
})

## {GLMNET}[train]<Regression> auto-lambda grid search using mirai ----
test_that("train() GLMNET Regression with auto-lambda grid search using mirai succeeds", {
  skip_if_not_installed("mirai")
  modt_r_glmnet <- train(
    x = datr_train,
    dat_test = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(alpha = 1),
    parallel_type = "mirai",
    n_workers = 2L
  )
  expect_s7_class(modt_r_glmnet, Regression)
})

## {GLMNET}[train]<Regression> auto-lambda + alpha grid search ----
test_that("train() GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  modt_r_glmnet <- train(
    x = datr_train,
    dat_test = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(alpha = c(0, 1)),
    parallel_type = "none"
  )
  expect_s7_class(modt_r_glmnet, Regression)
})

## {GLMNET}[train]<RegressionRes> auto-lambda + alpha grid search ----
test_that("train() Res-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  resmodt_r_glmnet <- train(
    x = datr_train,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(alpha = c(0.5, 1)),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    parallel_type = "none"
  )
  expect_s7_class(resmodt_r_glmnet, RegressionRes)
})

## {GLMNET}[train]<Classification> ----
modt_c_glmnet <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLMNET(alpha = 1, lambda = 0.01)
)
test_that("train() GLMNET Classification succeeds", {
  expect_s7_class(modt_c_glmnet, Classification)
})

## {GLMNET}[train]<Classification> Multiclass ----
modt_c3_glmnet <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_GLMNET(alpha = 1),
  parallel_type = "none"
)
test_that("train() GLMNET Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_glmnet, Classification)
})

# --- GAM ------------------------------------------------------------------------------------------
## {GAM}[train]<Regression> spline & parametric ----
hyperparameters <- setup_GAM()
hyperparameters
mod_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "gam"
)
test_that("train() GAM Regression with spline + parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> spline only ----
mod_r_gam <- train(
  x = datr_train[, -6],
  dat_test = datr_test[, -6],
  algorithm = "gam"
)
test_that("train() GAM Regression with only spline terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> parametric only ----
mod_r_gam <- train(
  x = datr_train[, 6:7],
  dat_test = datr_test[, 6:7],
  algorithm = "gam"
)
test_that("train() GAM Regression with only parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> grid search ----
modt_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "gam",
  hyperparameters = setup_GAM(k = c(3, 5, 7)),
  parallel_type = "none"
)
test_that("train() GAM Regression with grid_search() succeeds", {
  expect_s7_class(modt_r_gam, Regression)
})

## {GAM}[predict]<Regression> ----
predicted <- predict(modt_r_gam, datr_test)
test_that("predict() GAM Regression succeeds", {
  expect_identical(modt_r_gam@predicted_test, predicted)
})

## {GAM}[train]<RegressionRes> ----
resmod_r_gam <- train(
  x = datr,
  algorithm = "gam",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)

## {GAM}[train]<Classification> ----
mod_c_gam <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "gam"
)
test_that("train() GAM Classification succeeds", {
  expect_s7_class(mod_c_gam, Classification)
})

## {GAM}[train]<Classification> IFW ----
mod_c_gam_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "gam",
  hyperparameters = setup_GAM(ifw = TRUE)
)
test_that("train() GAM Classification with IFW succeeds", {
  expect_s7_class(mod_c_gam_ifw, Classification)
})

# --- LinearSVM ------------------------------------------------------------------------------------
## {LinearSVM}[train]<Regression> ----
mod_r_svml <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_LinearSVM()
)
test_that("train() LinearSVM Regression succeeds", {
  expect_s7_class(mod_r_svml, Regression)
})

## {LinearSVM}[train]<Regression> Tuned ----
modt_r_svml <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_LinearSVM(cost = c(1, 10)),
  parallel_type = "none"
)
test_that("train() LinearSVM Regression with tuning succeeds", {
  expect_s7_class(modt_r_svml, Regression)
})

## {LinearSVM}[train]<RegressionRes> ----
resmod_r_svml <- train(
  x = datr,
  algorithm = "linearsvm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res LinearSVM Regression succeeds", {
  expect_s7_class(resmod_r_svml, RegressionRes)
})

## {LinearSVM}[train]<Classification> ----
mod_c_linearsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "linearsvm"
)
test_that("train() LinearSVM Classification succeeds", {
  expect_s7_class(mod_c_linearsvm, Classification)
})

## {LinearSVM}[train]<Classification> Multiclass ----
mod_c3_linearsvm <- train(
  x = datc3_train,
  dat_test = datc3_test,
  algorithm = "linearsvm"
)
test_that("train() LinearSVM Multiclass Classification succeeds", {
  expect_s7_class(mod_c3_linearsvm, Classification)
})

## {LinearSVM}[train]<ClassificationRes> ----
resmod_c_linearsvm <- train(
  x = datc2,
  algorithm = "linearsvm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res LinearSVM Classification succeeds", {
  expect_s7_class(resmod_c_linearsvm, ClassificationRes)
})

# --- RadialSVM ------------------------------------------------------------------------------------
## {RadialSVM}[train]<Regression> ----
mod_r_svmr <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_RadialSVM()
)
test_that("train() RadialSVM Regression succeeds", {
  expect_s7_class(mod_r_svmr, Regression)
})

## {RadialSVM}[train]<Regression> Tuned ----
modt_r_svmr <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_RadialSVM(cost = c(1, 10, 100))
)
test_that("train() RadialSVM Regression with tuning succeeds", {
  expect_s7_class(modt_r_svmr, Regression)
})

## {RadialSVM}[train]<RegressionRes> ----
resmod_r_svmr <- train(
  x = datr,
  algorithm = "radialsvm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res RadialSVM Regression succeeds", {
  expect_s7_class(resmod_r_svmr, RegressionRes)
})

## {RadialSVM}[train]<RegressionRes> Tuned ----
resmodt_r_svmr <- train(
  x = datr,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res RadialSVM Regression with tuning succeeds", {
  expect_s7_class(resmodt_r_svmr, RegressionRes)
})

## {RadialSVM}[train]<Classification> ----
mod_c_radialsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "radialsvm"
)
test_that("train() RadialSVM Classification succeeds", {
  expect_s7_class(mod_c_radialsvm, Classification)
})

## {RadialSVM}[train]<Classification> Tuned ----
modt_c_radialsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  parallel_type = "none"
)
test_that("train() RadialSVM Classification with tuning succeeds", {
  expect_s7_class(modt_c_radialsvm, Classification)
})

## {RadialSVM}[train]<ClassificationRes> ----
resmod_c_radialsvm <- train(
  x = datc2,
  algorithm = "radialsvm",
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res RadialSVM Classification succeeds", {
  expect_s7_class(resmod_c_radialsvm, ClassificationRes)
})

## {RadialSVM}[train]<ClassificationRes> Tuned ----
resmodt_c_radialsvm <- train(
  x = datc2,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res RadialSVM Classification with tuning succeeds", {
  expect_s7_class(resmodt_c_radialsvm, ClassificationRes)
})

## {RadialSVM}[train]<Classification> Multiclass ----
modt_c3_radialsvm <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_RadialSVM()
)
test_that("train() RadialSVM Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_radialsvm, Classification)
})

# --- CART -----------------------------------------------------------------------------------------
## {CART}[train]<Regression> ----
mod_r_cart <- train(
  datr_train,
  dat_test = datr_test,
  algorithm = "cart"
)
test_that("train() Regression succeeds", {
  expect_s7_class(mod_r_cart, Regression)
})

## {CART}[train]<Regression> Grid search ----
## {CART} Check tuned == 0----
hyperparameters <- setup_CART(
  maxdepth = c(1, 2, 10),
  minbucket = c(1L, 4L)
)
test_that("tuned field is set correctly", {
  expect_identical(hyperparameters@tuned, 0L)
})

modt_r_cart <- train(
  datr_train,
  dat_test = datr_test,
  hyperparameters = setup_CART(maxdepth = 2:3),
  parallel_type = "none"
)
test_that("train() Regression with grid_search() succeeds", {
  expect_s7_class(modt_r_cart, Regression)
})

## {CART} Check tuned == 1----
test_that("tuned is set correctly", {
  expect_identical(modt_r_cart@hyperparameters@tuned, 1L)
})

## {CART}[train]<RegressionRes> ----
resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(),
  outer_resampling_config = setup_Resampler(3L),
  parallel_type = "none"
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmod_r_cart, RegressionRes)
})

## {CART}[train]<RegressionRes> Tuned ----
resmodt_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(maxdepth = 1:2, prune_cp = c(.001, .01)),
  outer_resampling_config = setup_Resampler(3),
  parallel_type = "none"
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmodt_r_cart, RegressionRes)
})

## {CART}[train]<RegressionRes> prune_cp ----
resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(prune_cp = c(.001, .01)),
  outer_resampling_config = setup_Resampler(3L),
  parallel_type = "none"
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmod_r_cart, RegressionRes)
})

## {CART}[train]<Classification> ----
# model <- train_CART(dat_training = datc2_train, dat_test = datc2_test)
# model$method #"class"
modt_c_cart <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "cart",
  hyperparameters = setup_CART(maxdepth = 1:2)
)
test_that("train() CART Classification succeeds", {
  expect_s7_class(modt_c_cart, Classification)
})

## {CART}[train]<Classification> IFW ----
mod_c_cart_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "cart",
  hyperparameters = setup_CART(
    ifw = TRUE
  )
)
test_that("train() CART Classification with IFW succeeds", {
  expect_s7_class(mod_c_cart_ifw, Classification)
})

## {CART}[train]<Classification> Grid search ----
modt_c_cart_tuned <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_CART(
    maxdepth = c(1L, 2L)
  ),
  parallel_type = "none"
)
test_that("train() Classification with grid_search() succeeds", {
  expect_s7_class(modt_c_cart_tuned, Classification)
})

## {CART}[train]<ClassificationRes> ----
# Can be used to test different parallelization methods during tuning
resmodt_c_cart <- train(
  x = datc2,
  algorithm = "cart",
  hyperparameters = setup_CART(
    maxdepth = c(1L, 2L)
  ),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() CART ClassificationRes succeeds", {
  expect_s7_class(resmodt_c_cart, ClassificationRes)
})

## {CART}[train]<Classification> Multiclass ----
modt_c3_cart <- train(
  x = datc3_train,
  dat_test = datc3_test,
  algorithm = "cart"
)
test_that("train() CART Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_cart, Classification)
})

# --- LightCART ------------------------------------------------------------------------------------
## {LightCART}[train]<Regression> ----
mod_r_lightcart <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightcart"
)
test_that("train() LightCART Regression succeeds", {
  expect_s7_class(mod_r_lightcart, Regression)
})

mod_r_lightcartlin <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightcart",
  hyperparameters = setup_LightCART(
    linear_tree = TRUE
  )
)
test_that("train() LightCART Regression with linear_tree succeeds", {
  expect_s7_class(mod_r_lightcartlin, Regression)
  expect_identical(
    mod_r_lightcartlin@hyperparameters$linear_tree,
    mod_r_lightcartlin@model$params$linear_tree
  )
})

## {LightCART}[train]<Classification> ----
mod_c_lightcart <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightcart"
)
test_that("train() LightCART Classification succeeds", {
  expect_s7_class(mod_c_lightcart, Classification)
})

## {LightCART}[train]<Classification> Multiclass ----
modt_c3_lightcart <- train(
  x = datc3_train,
  dat_test = datc3_test,
  algorithm = "lightcart"
)
test_that("train() LightCART Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_lightcart, Classification)
})

# --- LightRF --------------------------------------------------------------------------------------
## {LightRF}[train]<Regression> ----
mod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    nrounds = 20L,
    lambda_l1 = .1,
    lambda_l2 = .1
  )
)
test_that("train() LightRF Regression with l1, l2 succeeds", {
  expect_s7_class(mod_r_lightrf, Regression)
})

## {LightRF}[predict]<Regression> ----
predicted <- predict(mod_r_lightrf, features(datr_test))
test_that("predict() LightRF Regression succeeds", {
  expect_identical(mod_r_lightrf@predicted_test, predicted)
  expect_null(dim(predicted))
})

## {LightRF}[train]<Regression> Grid search ----
modt_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    nrounds = 20L,
    lambda_l1 = c(0, .1)
  ),
  parallel_type = "none"
)
test_that("train() LightRF Regression with l1 tuning succeeds", {
  expect_s7_class(modt_r_lightrf, Regression)
})

## {LightRF}[train]<RegressionRes> ----
resmodt_r_lightrf <- train(
  x = datr,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    nrounds = 20L,
    lambda_l1 = c(0, 10)
  ),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res LightRF Regression with l1 tuning succeeds", {
  expect_s7_class(resmodt_r_lightrf, RegressionRes)
})

## {LightRF}[train]<Classification> ----
mod_c_lightrf <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_LightRF(nrounds = 20L)
)
test_that("train() LightRF Binary Classification succeeds", {
  expect_s7_class(mod_c_lightrf, Classification)
})

## {LightRF}[predict]<Classification> ----
predicted_prob_test <- predict(mod_c_lightrf, features(datc2_test))
test_that("predict() LightRF Classification succeeds", {
  expect_identical(mod_c_lightrf@predicted_prob_test, predicted_prob_test)
})

## {LightRF}[train]<Classification> Tuned ----
modt_c_lightrf <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_LightRF(nrounds = 20L, max_depth = c(-1, 5))
)
test_that("train() LightRF Binary Classification with tuning succeeds", {
  expect_s7_class(modt_c_lightrf, Classification)
})

## {LightRF}[train]<ClassificationRes> ----
resmod_c_lightrf <- train(
  x = datc2,
  hyperparameters = setup_LightRF(nrounds = 20L),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() LightRF ClassificationRes succeeds", {
  expect_s7_class(resmod_c_lightrf, ClassificationRes)
})

## {LightRF}[train]<Classification> Multiclass ----
modt_c3_lightrf <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_LightRF(nrounds = 20L)
)
test_that("train() LightRF Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_lightrf, Classification)
})

## {LightGBM}[train]<Regression> ----
mod_r_lightgbm <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightgbm",
  hyperparameters = setup_LightGBM(
    force_nrounds = 50
  )
)
test_that("train() LightGBM Regression succeeds", {
  expect_s7_class(mod_r_lightgbm, Regression)
})

## {LightGBM}[train]<Regression> Autotune nrounds ----
modt_r_lightgbm <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_LightGBM()
)
test_that("train() LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(modt_r_lightgbm, Regression)
})

## {LightGBM}[train]<RegressionRes> Autotune nrounds ----
resmodt_r_lightgbm <- train(
  x = datr_train,
  hyperparameters = setup_LightGBM(max_nrounds = 50L),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(resmodt_r_lightgbm, RegressionRes)
})

## {LightGBM}[train]<Classification> ----
mod_c_lightgbm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightgbm",
  # hyperparameters = setup_LightGBM(
  #   force_nrounds = 100L
  # ),
  tuner_config = setup_GridSearch(
    resampler_config = setup_Resampler(
      n_resamples = 3L,
      type = "KFold"
    )
  ),
  parallel_type = "none"
)
test_that("train() LightGBM Classification succeeds", {
  expect_s7_class(mod_c_lightgbm, Classification)
})

## {LightGBM}[train]<Classification> Multiclass ----
modt_c3_lightgbm <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_LightGBM(
    force_nrounds = 20L
  )
)
test_that("train() LightGBM Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_lightgbm, Classification)
})

## {LightRuleFit}[train]<Regression> ----
mod_r_lightrlft_l1l2 <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_LightRuleFit(
    nrounds = 50L,
    lambda_l1 = 10,
    lambda_l2 = 10
  )
)

test_that("train() LightRuleFit Regression with l1, l2 params passed", {
  expect_s7_class(mod_r_lightrlft_l1l2, Regression)
  expect_identical(
    mod_r_lightrlft_l1l2@model@model_lightgbm@model$params$lambda_l1,
    10
  )
  expect_identical(
    mod_r_lightrlft_l1l2@model@model_lightgbm@model$params$lambda_l2,
    10
  )
})

## {LightRuleFit}[train]<Classification> ----
mod_c_lightrlft <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_LightRuleFit(nrounds = 50L)
)
test_that("train() LightRuleFit Binary Classification succeeds", {
  expect_s7_class(mod_c_lightrlft, Classification)
})

## {TabNet}[train]<Regression> ----
# Test if lantern is installed
if (torch::torch_is_installed()) {
  mod_r_tabnet <- train(
    x = datr_train,
    dat_test = datr_test,
    algorithm = "tabnet",
    hyperparameters = setup_TabNet(epochs = 3L, learn_rate = .01)
  )
  test_that("train() TabNet Regression succeeds", {
    expect_s7_class(mod_r_tabnet, Regression)
  })
}

## {TabNet}[train]<Classification> ----
if (torch::torch_is_installed()) {
  mod_c_tabnet <- train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_TabNet(epochs = 3L, learn_rate = .01)
  )
  test_that("train() TabNet Classification succeeds", {
    expect_s7_class(mod_c_tabnet, Classification)
  })
}

## {TabNet}[train]<Classification> Multiclass ----
if (torch::torch_is_installed()) {
  modt_c3_tabnet <- train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_TabNet(epochs = 3L, learn_rate = .01)
  )
  test_that("train() TabNet Multiclass Classification succeeds", {
    expect_s7_class(modt_c3_tabnet, Classification)
  })
}

## {Isotonic}[train]<Regression> ----
x <- rnorm(50)
y <- x^5 + rnorm(50)
dat <- data.table(x, y)
mod_iso <- train(dat, algorithm = "Isotonic")
test_that("train() Isotonic Regression succeeds", {
  expect_s7_class(mod_iso, Regression)
})

## {Isotonic}[train]<Classification> ----
set.seed(2025)
x <- rnorm(200)
y <- factor(ifelse(x > mean(x), "b", "a"))
x <- x + rnorm(200) / 3
dat <- data.frame(x, y)
cmod_iso <- train(dat, algorithm = "Isotonic")
test_that("train() Isotonic Classification succeeds", {
  expect_s7_class(cmod_iso, Classification)
})

# --- Ranger ---------------------------------------------------------------------------------------
## {Ranger}[train]<Regression> ----
mod_r_ranger <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_Ranger(num_trees = 50L)
)
test_that("train() Ranger Regression succeeds", {
  expect_s7_class(mod_r_ranger, Regression)
})

## {Ranger}[train]<Regression> Grid search ----
modt_r_ranger <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_Ranger(num_trees = 50L, mtry = c(3, 6)),
  parallel_type = "none"
)
test_that("train() Ranger Regression with grid search succeeds", {
  expect_s7_class(modt_r_ranger, Regression)
})

## {Ranger}[train]<RegressionRes> ----
resmod_r_ranger <- train(
  x = datr,
  hyperparameters = setup_Ranger(num_trees = 5000L),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res Ranger Regression succeeds", {
  expect_s7_class(resmod_r_ranger, RegressionRes)
})

## {Ranger}[train]<Classification> ----
mod_c_ranger <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_Ranger(num_trees = 10L)
)
test_that("train() Ranger Classification succeeds", {
  expect_s7_class(mod_c_ranger, Classification)
})

## {Ranger}[train]<Classification> Grid search ----
modt_c_ranger <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_Ranger(num_trees = 10L, mtry = c(2, 4)),
  parallel_type = "none"
)
test_that("train() Ranger Classification with grid search succeeds", {
  expect_s7_class(modt_c_ranger, Classification)
})

## {Ranger}[train]<ClassificationRes> ----
resmod_c_ranger <- train(
  x = datc2,
  hyperparameters = setup_Ranger(num_trees = 10L),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  parallel_type = "none"
)
test_that("train() Res Ranger Classification succeeds", {
  expect_s7_class(resmod_c_ranger, ClassificationRes)
})

## {Ranger}[train]<Classification> Multiclass ----
modt_c3_ranger <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_Ranger(num_trees = 10L)
)
test_that("train() Ranger Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_ranger, Classification)
})

# --- Predict SupervisedRes ------------------------------------------------------------------------

## {CART}[predict]<RegressionRes> ----
predicted_mean <- predict(resmod_r_cart, newdata = features(datr_test))
test_that("predict() SupervisedRes succeeds", {
  expect_true(length(predicted_mean) == nrow(datr_test))
})


# --- Calibration ----------------------------------------------------------------------------------
## {LightRF}[calibrate]<Classification> ----
# Calibrate mod_c_lightrf trained above
model <- mod_c_lightrf
predicted_probabilities <- model$predicted_prob_training
true_labels <- model$y_training
mod_c_lightrf_cal <- calibrate(
  mod_c_lightrf,
  predicted_probabilities = mod_c_lightrf$predicted_prob_training,
  true_labels = mod_c_lightrf$y_training
)
test_that("calibrate() succeeds on Classification", {
  expect_s7_class(mod_c_lightrf_cal, CalibratedClassification)
})

## {LightRF}[predict]<CalibratedClassification> ----
newdata <- features(datc2_test)
predicted_prob_test_cal <- predict(mod_c_lightrf_cal, newdata = newdata)
test_that("predict() CalibratedClassification succeeds", {
  expect_identical(
    mod_c_lightrf_cal@predicted_prob_test_calibrated,
    predicted_prob_test_cal
  )
})

# --- CalibratedClassificationRes ------------------------------------------------------------------
## {LightRF}[calibrate]<ClassificationRes>
resmod_c_lightrf_cal <- calibrate(resmod_c_lightrf)
test_that("calibrate() succeeds on ClassificationRes", {
  expect_s7_class(resmod_c_lightrf_cal, CalibratedClassificationRes)
})

## {GLM}[describe]<Regression> ----
test_that("describe.Regression returns character", {
  desc <- describe(mod_r_glm)
  expect_type(desc, "character")
})

## {GLM}[plot_true_pred]<Supervised> ----
test_that("plot_true_pred.Supervised creates a plotly object", {
  p <- plot_true_pred(mod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_true_pred]<Regression> ----
test_that("plot_true_pred creates a plotly object", {
  p <- plot_true_pred(mod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[present]<Supervised> ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[describe]<Classification> ----
test_that("describe.Classification returns character", {
  desc <- describe(mod_c_glm)
  expect_type(desc, "character")
})

## {GLM}[plot_true_pred]<Classification> ----
test_that("plot_true_pred.Classification creates a plotly object", {
  p <- plot_true_pred(mod_c_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_true_pred]<Classification> ----
test_that("plot_true_pred creates a plotly object", {
  p <- plot_true_pred(mod_c_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[draw_roc]<Classification> ----
test_that("draw_roc creates a plotly object", {
  p <- draw_roc(
    true_labels = list(
      Training = mod_c_glm@y_training,
      Test = mod_c_glm@y_test
    ),
    predicted_prob = list(
      Training = mod_c_glm@predicted_prob_training,
      Test = mod_c_glm@predicted_prob_test
    )
  )
  expect_s3_class(p, "plotly")
})
test_that("plot_roc.Classification creates a plotly object", {
  p <- plot_roc(mod_c_glm)
  expect_s3_class(p, "plotly")
})

## {CART}[plot_roc]<ClassificationRes> Tuned ----
test_that("plot_roc.ClassificationRes creates a plotly object", {
  p <- plot_roc(resmodt_c_cart)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_metric]<SupervisedRes> ----
test_that("plot_metric.SupervisedRes creates a plotly object", {
  p <- plot_metric(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_metric]<SupervisedRes> ----
test_that("plot_metric.SupervisedRes creates a plotly object", {
  p <- plot_metric(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_true_pred]<RegressionRes> ----
test_that("plot_true_pred RegressionRes creates a plotly object", {
  p <- plot_true_pred(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[plot_true_pred]<ClassificationRes> ----
test_that("plot_true_pred ClassificationRes creates a plotly object", {
  p <- plot_true_pred(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[present]<Supervised> ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[present]<Supervised> ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_c_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[present]<RegressionRes> ----
test_that("present() RegressionRes object creates a plotly object", {
  p <- present(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

## {GLM}[present]<ClassificationRes> ----
test_that("present() ClassificationRes object creates a plotly object", {
  p <- present(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

## {Multi}[present]<RegressionRes> ----
test_that("present() multiple RegressionRes objects creates a plotly object", {
  p <- present(list(resmod_r_glm, resmod_r_cart))
  expect_s3_class(p, "plotly")
})

## {Multi}[present]<ClassificationRes> ----
test_that("present() multiple ClassificationRes objects creates a plotly object", {
  p <- present(list(resmod_c_glm, resmodt_c_cart))
  expect_s3_class(p, "plotly")
})

## {Multi}[present]<Regression> ----
test_that("present() multiple Regression objects creates a plotly object", {
  p <- present(list(mod_r_glm, mod_r_cart))
  expect_s3_class(p, "plotly")
})

## {CART}[plot_varimp]<RegressionRes> ----
test_that("plot_varimp RegressionRes creates a plotly object", {
  p <- plot_varimp(resmod_r_cart)
  expect_s3_class(p, "plotly")
})

## {GLM}[train]<Supervised> Outdir ----
test_that("train saves model to rds successfully", {
  temp_dir <- withr::local_tempdir()
  outdir <- file.path(temp_dir, "mod_r_glm")

  mod_r_glm <- train(
    x = datr_train,
    dat_test = datr_test,
    algorithm = "glm",
    outdir = outdir
  )
  expect_true(file.exists(file.path(outdir, "train_GLM.rds")))
})

## {GLM}[train]<SupervisedRes> Outdir ----
test_that("train saves SupervisedRes model to rds successfully", {
  temp_dir <- withr::local_tempdir()
  outdir <- file.path(temp_dir, "resmod_r_glm")
  resmod_r_glm <- train(
    x = datr,
    algorithm = "glm",
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    outdir = outdir
  )
  expect_true(file.exists(file.path(outdir, "train_GLM.rds")))
})

## {CART}[repr]<Classification> Tuned ----
modt_c_cart_repr <- repr(modt_c_cart, output_type = "ansi")
test_that("repr() Tuned Classification succeeds", {
  expect_type(modt_c_cart_repr, "character")
})

## {CART}[repr]<ClassificationRes> Tuned ----
resmodt_c_cart_repr <- repr(resmodt_c_cart, output_type = "ansi")
test_that("repr() Tuned ClassificationRes succeeds", {
  expect_type(resmodt_c_cart_repr, "character")
})

## {CART}[repr]<Regression> Tuned ----
modt_r_cart_repr <- repr(modt_r_cart, output_type = "ansi")
test_that("repr() Tuned Regression succeeds", {
  expect_type(modt_r_cart_repr, "character")
})

## {CART}[repr]<RegressionRes> Tuned ----
resmodt_r_cart_repr <- repr(resmodt_r_cart, output_type = "ansi")
test_that("repr() Tuned RegressionRes succeeds", {
  expect_type(resmodt_r_cart_repr, "character")
})

# --- Describe & present list of Supervised --------------------------------------------------------
## {Multi}[describe]<Classification> List ----
x <- list(
  modt_c_cart,
  mod_c_lightrf,
  mod_c_lightgbm
)
out <- describe(x)
test_that("describe() list of Classification objects returns character", {
  expect_type(out, "character")
})

## {Multi}[present]<Classification> List ----
plt <- present(x)
test_that("present() list of Classification objects returns plotly object", {
  expect_s3_class(plt, "plotly")
})

## {Multi}[describe]<Regression> List ----
x <- list(
  mod_r_glmnet,
  mod_r_svmr,
  mod_r_lightrf
)
out <- describe(x)
test_that("describe() list of Regression objects returns character", {
  expect_type(out, "character")
})

## {Multi}[present]<Regression> List ----
plt <- present(x)
test_that("present() list of Regression objects returns plotly object", {
  expect_s3_class(plt, "plotly")
})

# Describe & present list of SupervisedRes----

## {Multi}[describe]<ClassificationRes> List ----
x <- list(
  resmod_c_glm,
  resmod_c_linearsvm,
  resmod_c_lightrf
)
out <- describe(x)
test_that("describe() list of ClassificationRes objects returns character", {
  expect_type(out, "character")
})

## {Multi}[present]<ClassificationRes> List ----
plt <- present(x)
test_that("present() list of ClassificationRes objects returns plotly object", {
  expect_s3_class(plt, "plotly")
})

## {Multi}[describe]<RegressionRes> List ----
x <- list(
  resmod_r_glm,
  resmod_r_svml,
  resmodt_r_lightrf
)
out <- describe(x)
test_that("describe() list of RegressionRes objects returns character", {
  expect_type(out, "character")
})

## {Multi}[present]<RegressionRes> List ----
plt <- present(x)
test_that("present() list of RegressionRes objects returns plotly object", {
  expect_s3_class(plt, "plotly")
})

# --- CalibratedClassificationRes ------------------------------------------------------------------
## {GLM}[calibrate]<ClassificationRes> ----
# Using resmod_c_glm from above
resmod_c_glm_cal <- calibrate(resmod_c_glm)
test_that("calibrate() ClassificationRes succeeds", {
  expect_s7_class(resmod_c_glm_cal, CalibratedClassificationRes)
})

## {GLM}[predict]<CalibratedClassificationRes> ----
predicted_cal <- predict(resmod_c_glm_cal, features(datc2_test))
test_that("predict() CalibratedClassificationRes succeeds", {
  expect_type(predicted_cal, "double")
  expect_length(predicted_cal, nrow(datc2_test))
})
