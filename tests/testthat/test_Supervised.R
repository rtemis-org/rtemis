# test_Supervised.R
# ::rtemis::
# EDG rtemis.org

# Key
# {Algorithm}[method]<Class> Further conditions

# Note
# We are using very small and simple datasets to reduce runtime.
# GLM models are expected to give warnings, including:
#   - "glm.fit: fitted probabilities numerically 0 or 1 occurred"
#   - "glm.fit: algorithm did not converge"
# SPLS with classifier = "logistic" fits a glm on the latent components, so it
# gives the same warnings on the near-separable binary iris fixture.

# %% Packages ----
library(data.table)

# %% Suggests-gated fits ----
# Several backends are in Suggests, so a developer without them installed must
# still be able to run this file. Their fits happen at the top level, before
# any `test_that()` runs, so a missing package would fail the whole file rather
# than skip a section. Gating the fit leaves NULL behind, and every test in
# that section calls `skip_if_not_installed()` and so skips before touching it.
fit_if_installed <- function(pkg, expr) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(NULL)
  }
  expr
}

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
  hyperparameters = setup_GLM()
)
test_that("train() GLM Regression succeeds", {
  expect_s7_class(mod_r_glm, Regression)
})
test_that("se() computes standard errors on demand, for the models that have them", {
  # Not stored: only a few algorithms produce them, so storing per-case
  # vectors on every regression result would serve almost none of them.
  se_training <- se(mod_r_glm, features(datr_train))
  expect_type(se_training, "double")
  expect_length(se_training, nrow(datr_train))
  expect_type(se(mod_r_glm, features(datr_test)), "double")
  # An algorithm with no `se_super()` method has no standard error, which is an
  # answer rather than a failure.
  mod_r_cart <- train(
    x = datr_train,
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  expect_null(se(mod_r_cart, features(datr_test)))
})

## {GLM}[train]<Regression> Throw error with missing data ----
datr_train_na <- datr_train
datr_train_na[10:2, 1] <- NA
test_that("train() GLM Regression with missing data throws error", {
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_GLM()
    )
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
  hyperparameters = setup_GLM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res GLM Regression succeeds", {
  expect_s7_class(resmod_r_glm, RegressionRes)
})

## {GLM}[train]<Classification> ----
mod_c_glm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLM()
)
test_that("train() GLM Classification succeeds", {
  expect_s7_class(mod_c_glm, Classification)
})

## {GLM}[train]<Classification> IFW ----
mod_c_glm_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLM(ifw = TRUE)
)
test_that("train() GLM Classification with IFW succeeds", {
  expect_s7_class(mod_c_glm_ifw, Classification)
})

## {GLM}[train]<ClassificationRes> ----
resmod_c_glm <- train(
  x = datc2,
  hyperparameters = setup_GLM(),
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
      hyperparameters = setup_GLMNET(alpha = 1),
      execution_config = setup_ExecutionConfig(
        backend = "future",
        n_workers = 2L, # Limit to 2 workers for CRAN
        future_plan = "mirai_multisession" # which gets converted to "future.mirai::mirai_multisession"
      ),
      verbosity = 1L
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
      hyperparameters = setup_GLMNET(alpha = 1),
      execution_config = setup_ExecutionConfig(
        backend = "future",
        future_plan = "sequential",
        n_workers = 2L
      )
    )
  )
})

## {GLMNET}[train]<Regression> auto-lambda grid search using mirai ----
test_that("train() GLMNET Regression with auto-lambda grid search using mirai succeeds", {
  skip_if_not_installed("mirai")
  modt_r_glmnet <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_GLMNET(alpha = 1),
    execution_config = setup_ExecutionConfig(backend = "mirai", n_workers = 2L)
  )
  expect_s7_class(modt_r_glmnet, Regression)
})

## {GLMNET}[train]<Regression> auto-lambda + alpha grid search ----
test_that("train() GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  modt_r_glmnet <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_GLMNET(alpha = c(0, 1)),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  expect_s7_class(modt_r_glmnet, Regression)
})

## {GLMNET}[train]<RegressionRes> auto-lambda + alpha grid search ----
test_that("train() Res-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  resmodt_r_glmnet <- train(
    x = datr_train,
    hyperparameters = setup_GLMNET(alpha = c(0.5, 1)),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
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
test_that("train() GLMNET Multiclass Classification succeeds", {
  modt_c3_glmnet <- train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_GLMNET(alpha = 1),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  expect_s7_class(modt_c3_glmnet, Classification)
})

# --- GAM ------------------------------------------------------------------------------------------
## {GAM}[train]<Regression> spline & parametric ----
mod_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_GAM()
)
test_that("train() GAM Regression with spline + parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> spline only ----
mod_r_gam <- train(
  x = datr_train[, -6],
  dat_test = datr_test[, -6],
  hyperparameters = setup_GAM()
)
test_that("train() GAM Regression with only spline terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> parametric only ----
mod_r_gam <- train(
  x = datr_train[, 6:7],
  dat_test = datr_test[, 6:7],
  hyperparameters = setup_GAM()
)
test_that("train() GAM Regression with only parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## {GAM}[train]<Regression> grid search ----
modt_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_GAM(k = c(3, 5, 7))
)
test_that("train() GAM Regression with grid_search() succeeds", {
  expect_s7_class(modt_r_gam, Regression)
})

## {GAM}[predict]<Regression> ----
test_that("predict() GAM Regression works", {
  expect_error(predicted <- predict(modt_r_gam, datr_test))
  predicted <- predict(modt_r_gam, features(datr_test))
  expect_identical(modt_r_gam@predicted_test, predicted)
})

## {GAM}[train]<RegressionRes> ----
resmod_r_gam <- train(
  x = datr,
  hyperparameters = setup_GAM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)

## {GAM}[train]<Classification> ----
mod_c_gam <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GAM()
)
test_that("train() GAM Classification succeeds", {
  expect_s7_class(mod_c_gam, Classification)
})

## {GAM}[train]<Classification> IFW ----
mod_c_gam_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
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
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() LinearSVM Regression with tuning succeeds", {
  expect_s7_class(modt_r_svml, Regression)
})

## {LinearSVM}[train]<RegressionRes> ----
resmod_r_svml <- train(
  x = datr,
  hyperparameters = setup_LinearSVM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res LinearSVM Regression succeeds", {
  expect_s7_class(resmod_r_svml, RegressionRes)
})

## {LinearSVM}[train]<Classification> ----
mod_c_linearsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_LinearSVM()
)
test_that("train() LinearSVM Classification succeeds", {
  expect_s7_class(mod_c_linearsvm, Classification)
})

## {LinearSVM}[train]<Classification> Multiclass ----
mod_c3_linearsvm <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_LinearSVM()
)
test_that("train() LinearSVM Multiclass Classification succeeds", {
  expect_s7_class(mod_c3_linearsvm, Classification)
})

## {LinearSVM}[train]<ClassificationRes> ----
resmod_c_linearsvm <- train(
  x = datc2,
  hyperparameters = setup_LinearSVM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
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
  hyperparameters = setup_RadialSVM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Res RadialSVM Regression succeeds", {
  expect_s7_class(resmod_r_svmr, RegressionRes)
})

## {RadialSVM}[train]<RegressionRes> Tuned ----
resmodt_r_svmr <- train(
  x = datr,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Res RadialSVM Regression with tuning succeeds", {
  expect_s7_class(resmodt_r_svmr, RegressionRes)
})

## {RadialSVM}[train]<Classification> ----
mod_c_radialsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_RadialSVM()
)
test_that("train() RadialSVM Classification succeeds", {
  expect_s7_class(mod_c_radialsvm, Classification)
})

## {RadialSVM}[train]<Classification> Tuned ----
modt_c_radialsvm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() RadialSVM Classification with tuning succeeds", {
  expect_s7_class(modt_c_radialsvm, Classification)
})

## {RadialSVM}[train]<ClassificationRes> ----
resmod_c_radialsvm <- train(
  x = datc2,
  hyperparameters = setup_RadialSVM(),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Res RadialSVM Classification succeeds", {
  expect_s7_class(resmod_c_radialsvm, ClassificationRes)
})

## {RadialSVM}[train]<ClassificationRes> Tuned ----
resmodt_c_radialsvm <- train(
  x = datc2,
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
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
  hyperparameters = setup_CART()
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
  execution_config = setup_ExecutionConfig(backend = "none")
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
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmod_r_cart, RegressionRes)
})

## {CART}[train]<RegressionRes> Tuned ----
resmodt_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(maxdepth = 1:2, prune_cp = c(.001, .01)),
  outer_resampling_config = setup_Resampler(3),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmodt_r_cart, RegressionRes)
})

## {CART}[train]<RegressionRes> prune_cp ----
resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(prune_cp = c(.001, .01)),
  outer_resampling_config = setup_Resampler(3L),
  execution_config = setup_ExecutionConfig(backend = "none")
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
  hyperparameters = setup_CART(maxdepth = 1:2)
)
test_that("train() CART Classification succeeds", {
  expect_s7_class(modt_c_cart, Classification)
})

## {CART}[train]<Classification> IFW ----
mod_c_cart_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
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
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Classification with grid_search() succeeds", {
  expect_s7_class(modt_c_cart_tuned, Classification)
})

## {CART}[train]<ClassificationRes> ----
# Can be used to test different parallelization methods during tuning
resmodt_c_cart <- train(
  x = datc2,
  hyperparameters = setup_CART(
    maxdepth = c(1L, 2L)
  ),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() CART ClassificationRes succeeds", {
  expect_s7_class(resmodt_c_cart, ClassificationRes)
})

## {CART}[train]<Classification> Multiclass ----
modt_c3_cart <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_CART()
)
test_that("train() CART Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_cart, Classification)
})

# --- LightCART ------------------------------------------------------------------------------------
## {LightCART}[train]<Regression> ----
mod_r_lightcart <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_LightCART()
)
test_that("train() LightCART Regression succeeds", {
  expect_s7_class(mod_r_lightcart, Regression)
})

mod_r_lightcartlin <- train(
  x = datr_train,
  dat_test = datr_test,
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
  hyperparameters = setup_LightCART()
)
test_that("train() LightCART Classification succeeds", {
  expect_s7_class(mod_c_lightcart, Classification)
})

## {LightCART}[train]<Classification> Multiclass ----
modt_c3_lightcart <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_LightCART()
)
test_that("train() LightCART Multiclass Classification succeeds", {
  expect_s7_class(modt_c3_lightcart, Classification)
})

# --- LightRF --------------------------------------------------------------------------------------
## {LightRF}[train]<Regression> ----
mod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
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
  hyperparameters = setup_LightRF(
    nrounds = 20L,
    lambda_l1 = c(0, .1)
  ),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() LightRF Regression with l1 tuning succeeds", {
  expect_s7_class(modt_r_lightrf, Regression)
})

## {LightRF}[train]<RegressionRes> ----
resmodt_r_lightrf <- train(
  x = datr,
  hyperparameters = setup_LightRF(
    nrounds = 20L,
    lambda_l1 = c(0, 10)
  ),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
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


# %% Predicted probabilities have one shape ----

test_that("predicted probabilities are a matrix whatever the class count", {
  # Binary is one column -- the positive class's probability, which is all any
  # backend produces -- and multiclass is one column per class. A property that
  # was a vector for one task and a matrix for the other could not be declared,
  # and made every consumer branch on the class count to read it.
  expect_true(is.matrix(mod_c_lightrf@predicted_prob_training))
  expect_identical(ncol(mod_c_lightrf@predicted_prob_training), 1L)
  expect_identical(
    colnames(mod_c_lightrf@predicted_prob_training),
    levels(mod_c_lightrf@y_training)[mod_c_lightrf@binclasspos]
  )
  multi <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  expect_true(is.matrix(multi@predicted_prob_training))
  expect_identical(
    colnames(multi@predicted_prob_training),
    levels(iris$Species)
  )
})


test_that("the probability-based metrics survive the binary matrix", {
  # `positive_prob()` is what feeds AUC and the Brier score; a one-column
  # matrix must reach them as the score vector they take.
  overall <- mod_c_lightrf@metrics_training[["overall"]]
  expect_false(is.null(overall[["auc"]]))
  expect_false(is.null(overall[["brier_score"]]))
  # Multiclass has no single score per case, so those columns are absent
  # rather than wrong.
  multi <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
  expect_false("auc" %in% names(multi@metrics_training[["overall"]]))
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
  execution_config = setup_ExecutionConfig(backend = "none")
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
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Res LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(resmodt_r_lightgbm, RegressionRes)
})

## {LightGBM}[train]<Classification> ----
mod_c_lightgbm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_LightGBM(),
  # hyperparameters = setup_LightGBM(
  #   force_nrounds = 100L
  # ),
  tuner_config = setup_GridSearch(
    resampler_config = setup_Resampler(
      n_resamples = 3L,
      type = "KFold"
    )
  ),
  execution_config = setup_ExecutionConfig(backend = "none")
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
    lambda_l2 = 10,
    lambda = 0.01
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
  hyperparameters = setup_LightRuleFit(nrounds = 50L),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() LightRuleFit Binary Classification succeeds", {
  expect_s7_class(mod_c_lightrlft, Classification)
})

## {LightRuleFit}[train]<Classification> multiclass----
mod_c_lightrlft <- train(
  x = datc3_train,
  dat_test = datc3_test,
  hyperparameters = setup_LightRuleFit(nrounds = 50L),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() LightRuleFit Multiclass Classification succeeds", {
  expect_s7_class(mod_c_lightrlft, Classification)
  # Multiclass glmnet coefficients are a list (one per class); rule
  # importance is the total absolute influence and the signed per-class
  # coefficients are preserved as extra columns.
  vi <- get_varimp(mod_c_lightrlft)@data
  cls <- levels(datc3_train[[ncol(datc3_train)]])
  expect_true(all(cls %in% names(vi)))
  expect_identical(names(vi)[[2L]], "Coefficient")
  expect_true(all(vi[["Coefficient"]] >= 0))
  expect_equal(
    vi[["Coefficient"]],
    unname(rowSums(abs(as.matrix(vi[, cls, with = FALSE]))))
  )
  # Per-class importance is individually plottable.
  expect_no_error(plot_varimp(mod_c_lightrlft, measure = cls[[1L]]))
  # The formatted rule listing breaks out the signed per-class coefficients,
  # with `Coefficient` the aggregate importance used to rank rows.
  tab <- mod_c_lightrlft@model@rules_selected_formatted_coefs
  expect_true(all(cls %in% names(tab)))
  expect_equal(
    tab[["Coefficient"]],
    unname(rowSums(abs(as.matrix(tab[, cls, with = FALSE]))))
  )
  expect_false(is.unsorted(rev(tab[["Coefficient"]])))
})

## {TabNet}[train]<Regression> ----
# Test if lantern is installed
if (torch::torch_is_installed()) {
  mod_r_tabnet <- train(
    x = datr_train,
    dat_test = datr_test,
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
mod_iso <- train(dat, hyperparameters = setup_Isotonic())
test_that("train() Isotonic Regression succeeds", {
  expect_s7_class(mod_iso, Regression)
})

## {Isotonic}[train]<Classification> ----
set.seed(2025)
x <- rnorm(200)
y <- factor(ifelse(x > mean(x), "b", "a"))
x <- x + rnorm(200) / 3
dat <- data.frame(x, y)
cmod_iso <- train(dat, hyperparameters = setup_Isotonic())
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
  execution_config = setup_ExecutionConfig(backend = "none")
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
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() Ranger Classification with grid search succeeds", {
  expect_s7_class(modt_c_ranger, Classification)
})

## {Ranger}[train]<ClassificationRes> ----
resmod_c_ranger <- train(
  x = datc2,
  hyperparameters = setup_Ranger(num_trees = 10L),
  outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
  execution_config = setup_ExecutionConfig(backend = "none")
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

## {Ranger}[train]<Regression> /\Error mtry > n features ----
# validate_hyperparameters() runs before tuning, so an out-of-range value
# anywhere in the search space aborts up front. Without it, the abort happens
# inside each grid cell, where the default on_error = "continue" catches it and
# the bad cells are silently dropped instead of reported.
test_that("train() Ranger aborts when mtry exceeds n features", {
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_Ranger(num_trees = 10L, mtry = 100L)
    ),
    class = "rtemis_range_error"
  )
})

test_that("train() Ranger aborts when any search value of mtry is out of range", {
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_Ranger(num_trees = 10L, mtry = c(3L, 100L))
    ),
    class = "rtemis_range_error"
  )
})

# --- SPLS -----------------------------------------------------------------------------------------
## {SPLS}[train]<Regression> ----
mod_r_spls <- fit_if_installed(
  "spls",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3)
  )
)
test_that("train() SPLS Regression succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(mod_r_spls, Regression)
})

## {SPLS}[train]<Regression> Grid search ----
modt_r_spls <- fit_if_installed(
  "spls",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_SPLS(k = c(1L, 2L), eta = c(0.3, 0.6)),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() SPLS Regression with grid search succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(modt_r_spls, Regression)
})

## {SPLS}[train]<RegressionRes> ----
resmod_r_spls <- fit_if_installed(
  "spls",
  train(
    x = datr,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res SPLS Regression succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(resmod_r_spls, RegressionRes)
})

## {SPLS}[train]<Classification> ----
mod_c_spls <- fit_if_installed(
  "spls",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3)
  )
)
test_that("train() SPLS Classification succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(mod_c_spls, Classification)
})

## {SPLS}[train]<Classification> logistic classifier ----
mod_c_spls_logistic <- fit_if_installed(
  "spls",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3, classifier = "logistic")
  )
)
test_that("train() SPLS Classification with logistic classifier succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(mod_c_spls_logistic, Classification)
})

## {SPLS}[train]<Classification> Grid search ----
modt_c_spls <- fit_if_installed(
  "spls",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_SPLS(k = c(1L, 2L), eta = 0.3),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() SPLS Classification with grid search succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(modt_c_spls, Classification)
})

## {SPLS}[train]<ClassificationRes> ----
resmod_c_spls <- fit_if_installed(
  "spls",
  train(
    x = datc2,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res SPLS Classification succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(resmod_c_spls, ClassificationRes)
})

## {SPLS}[train]<Classification> Multiclass ----
modt_c3_spls <- fit_if_installed(
  "spls",
  train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_SPLS(k = 2L, eta = 0.3)
  )
)
test_that("train() SPLS Multiclass Classification succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(modt_c3_spls, Classification)
})

## {SPLS}[predict]<Regression> ----
predicted_spls <- fit_if_installed(
  "spls",
  predict(mod_r_spls, features(datr_test))
)
test_that("predict() SPLS Regression succeeds", {
  skip_if_not_installed("spls")
  expect_identical(mod_r_spls@predicted_test, predicted_spls)
  expect_null(dim(predicted_spls))
})

## {SPLS}[predict]<Classification> ----
# splsda only returns probabilities for the binary logistic case, so
# predict_super() projects onto the latent components itself and asks the inner
# classifier. Binary must come back as the second level's probability;
# multiclass as one column per class.
test_that("predict() SPLS Classification returns second-level probabilities", {
  skip_if_not_installed("spls")
  predicted_prob <- predict(mod_c_spls, features(datc2_test))
  expect_identical(NCOL(predicted_prob), 1L)
  expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
  expect_identical(mod_c_spls@predicted_prob_test, predicted_prob)
  # A flipped column would still be a valid probability, so check it tracks the
  # outcome rather than its complement.
  expect_gt(
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[2L]]),
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
  )
})

test_that("predict() SPLS Multiclass returns one column per class", {
  skip_if_not_installed("spls")
  predicted_prob <- predict(modt_c3_spls, features(datc3_test))
  expect_identical(NCOL(predicted_prob), nlevels(datc3_test$Species))
  expect_equal(unname(rowSums(predicted_prob)), rep(1, nrow(datc3_test)))
})

## {SPLS}[varimp]<Regression> ----
test_that("get_varimp() SPLS Regression succeeds", {
  skip_if_not_installed("spls")
  vi <- get_varimp(mod_r_spls)
  expect_s7_class(vi, VariableImportance)
  # One coefficient per design-matrix column: the factor `g` is one-hot encoded
  # before spls sees it.
  expect_gte(nrow(vi@data), length(mod_r_spls@xnames))
})

## {SPLS}[train]<Regression> Algorithm name dispatch ----
# The algorithmDB row is what makes the name resolvable; without it this
# aborts with "Incorrect algorithm specified".
mod_r_spls_byname <- fit_if_installed(
  "spls",
  train(
    x = datr_train,
    hyperparameters = get_default_hyperparameters("spls")
  )
)
test_that("train() SPLS from its algorithm name succeeds", {
  skip_if_not_installed("spls")
  expect_s7_class(mod_r_spls_byname, Regression)
  expect_identical(get_alg_name("spls"), "SPLS")
})

## {SPLS}[train]<Regression> /\Error k > n features ----
test_that("train() SPLS aborts when k exceeds n features", {
  skip_if_not_installed("spls")
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_SPLS(k = 100L)
    ),
    class = "rtemis_range_error"
  )
})

test_that("train() SPLS aborts when any search value of k is out of range", {
  skip_if_not_installed("spls")
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_SPLS(k = c(2L, 100L))
    ),
    class = "rtemis_range_error"
  )
})

## {SPLS}[train]<Classification> /\Error ifw unsupported ----
# spls takes no case weights, so IFW cannot be honored and must fail loudly
# rather than fit an unweighted model.
test_that("train() SPLS aborts when ifw is enabled", {
  skip_if_not_installed("spls")
  expect_error(
    train(
      x = datc2_train,
      hyperparameters = setup_SPLS(k = 2L, eta = 0.3, ifw = TRUE)
    ),
    class = "rtemis_unsupported_error"
  )
})

## {SPLS}[train]<Regression> Throw error with missing data ----
test_that("train() SPLS Regression with missing data throws error", {
  skip_if_not_installed("spls")
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_SPLS(k = 2L, eta = 0.3)
    )
  )
})

# --- KNN ------------------------------------------------------------------------------------------
## {KNN}[train]<Regression> ----
mod_r_knn <- fit_if_installed(
  "kknn",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_KNN(k = 5L)
  )
)
test_that("train() KNN Regression succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(mod_r_knn, Regression)
})

## {KNN}[train]<Regression> Grid search ----
modt_r_knn <- fit_if_installed(
  "kknn",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_KNN(
      k = c(3L, 9L),
      kernel = c("rectangular", "optimal")
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() KNN Regression with grid search succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(modt_r_knn, Regression)
})

## {KNN}[train]<RegressionRes> ----
resmod_r_knn <- fit_if_installed(
  "kknn",
  train(
    x = datr,
    hyperparameters = setup_KNN(k = 5L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res KNN Regression succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(resmod_r_knn, RegressionRes)
})

## {KNN}[train]<Regression> Unscaled ----
# `train.kknn` does not record `scale` and `predict.train.kknn` would re-fit
# with its own default of TRUE, so predict must go through `kknn::kknn()` with
# the stashed value: an unscaled fit has to differ from the scaled one.
mod_r_knn_unscaled <- fit_if_installed(
  "kknn",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_KNN(k = 5L, scale = FALSE)
  )
)
test_that("train() KNN Regression honors scale = FALSE at predict time", {
  skip_if_not_installed("kknn")
  expect_s7_class(mod_r_knn_unscaled, Regression)
  expect_false(identical(
    mod_r_knn_unscaled@predicted_test,
    mod_r_knn@predicted_test
  ))
})

## {KNN}[train]<Classification> ----
mod_c_knn <- fit_if_installed(
  "kknn",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_KNN(k = 5L)
  )
)
test_that("train() KNN Classification succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(mod_c_knn, Classification)
})

## {KNN}[train]<Classification> Grid search ----
modt_c_knn <- fit_if_installed(
  "kknn",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_KNN(k = c(3L, 9L)),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() KNN Classification with grid search succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(modt_c_knn, Classification)
})

## {KNN}[train]<ClassificationRes> ----
resmod_c_knn <- fit_if_installed(
  "kknn",
  train(
    x = datc2,
    hyperparameters = setup_KNN(k = 5L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res KNN Classification succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(resmod_c_knn, ClassificationRes)
})

## {KNN}[train]<Classification> Multiclass ----
modt_c3_knn <- fit_if_installed(
  "kknn",
  train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_KNN(k = 5L)
  )
)
test_that("train() KNN Multiclass Classification succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(modt_c3_knn, Classification)
})

## {KNN}[predict]<Regression> ----
predicted_knn <- fit_if_installed(
  "kknn",
  predict(mod_r_knn, features(datr_test))
)
test_that("predict() KNN Regression succeeds", {
  skip_if_not_installed("kknn")
  expect_identical(mod_r_knn@predicted_test, predicted_knn)
  expect_null(dim(predicted_knn))
})

## {KNN}[predict]<Classification> ----
test_that("predict() KNN Classification returns second-level probabilities", {
  skip_if_not_installed("kknn")
  predicted_prob <- predict(mod_c_knn, features(datc2_test))
  expect_identical(NCOL(predicted_prob), 1L)
  expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
  expect_identical(mod_c_knn@predicted_prob_test, predicted_prob)
  # A flipped column would still be a valid probability, so check it tracks the
  # outcome rather than its complement.
  expect_gt(
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[2L]]),
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
  )
})

test_that("predict() KNN Multiclass returns one column per class", {
  skip_if_not_installed("kknn")
  predicted_prob <- predict(modt_c3_knn, features(datc3_test))
  expect_identical(NCOL(predicted_prob), nlevels(datc3_test$Species))
  expect_equal(unname(rowSums(predicted_prob)), rep(1, nrow(datc3_test)))
})

## {KNN}[varimp]<Regression> ----
# kknn provides no measure of variable importance.
test_that("get_varimp() KNN Regression returns NULL", {
  skip_if_not_installed("kknn")
  expect_null(get_varimp(mod_r_knn))
})

## {KNN}[train]<Regression> Algorithm name dispatch ----
# The algorithmDB row is what makes the name resolvable; without it this
# aborts with "Incorrect algorithm specified".
mod_r_knn_byname <- fit_if_installed(
  "kknn",
  train(
    x = datr_train,
    hyperparameters = get_default_hyperparameters("knn")
  )
)
test_that("train() KNN from its algorithm name succeeds", {
  skip_if_not_installed("kknn")
  expect_s7_class(mod_r_knn_byname, Regression)
  expect_identical(get_alg_name("knn"), "KNN")
})

## {KNN}[train]<Regression> /\Error k >= n cases ----
# `train.kknn` picks `k` by leave-one-out CV, so `k` must be strictly less than
# the number of training cases.
test_that("train() KNN aborts when k is not less than n cases", {
  skip_if_not_installed("kknn")
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_KNN(k = nrow(datr_train))
    ),
    class = "rtemis_range_error"
  )
})

test_that("train() KNN aborts when any search value of k is out of range", {
  skip_if_not_installed("kknn")
  expect_error(
    train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_KNN(k = c(5L, nrow(datr_train)))
    ),
    class = "rtemis_range_error"
  )
})

## {KNN}[train]<Classification> /\Error ifw unsupported ----
# kknn takes no case weights, so IFW cannot be honored and must fail loudly
# rather than fit an unweighted model.
test_that("train() KNN aborts when ifw is enabled", {
  skip_if_not_installed("kknn")
  expect_error(
    train(
      x = datc2_train,
      hyperparameters = setup_KNN(k = 5L, ifw = TRUE)
    ),
    class = "rtemis_unsupported_error"
  )
})

## {KNN}[train]<Regression> Throw error with missing data ----
test_that("train() KNN Regression with missing data throws error", {
  skip_if_not_installed("kknn")
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_KNN(k = 5L)
    )
  )
})


# --- BART -----------------------------------------------------------------------------------------
# The sampler is stochastic, so every fit below fixes `seed`.
## {BART}[train]<Regression> ----
mod_r_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    )
  )
)
test_that("train() BART Regression succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_r_bart, Regression)
})

## {BART}[train]<Regression> Grid search ----
modt_r_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_BART(
      num_trees = c(5L, 10L),
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() BART Regression with grid search succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(modt_r_bart, Regression)
})

## {BART}[train]<RegressionRes> ----
resmod_r_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datr,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    ),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res BART Regression succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(resmod_r_bart, RegressionRes)
})

## {BART}[train]<Regression> Heteroskedastic ----
# A variance forest changes the fit and adds a second forest to the samples.
mod_r_bart_het <- fit_if_installed(
  "stochtree",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      variance_forest_num_trees = 5L,
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    )
  )
)
test_that("train() BART Regression with a variance forest succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_r_bart_het, Regression)
  expect_true(mod_r_bart_het@model[["model_params"]][[
    "include_variance_forest"
  ]])
})

## {BART}[train]<Regression> link is classification-only ----
# `link` names the binary outcome model's link, so a continuous outcome must
# be sampled identically whatever it is set to.
test_that("train() BART Regression ignores link", {
  skip_if_not_installed("stochtree")
  mod_r_bart_cloglog <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      link = "cloglog",
      seed = 2026L
    )
  )
  expect_identical(
    mod_r_bart_cloglog@predicted_test,
    mod_r_bart@predicted_test
  )
})

## {BART}[train]<Classification> ----
mod_c_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    )
  )
)
test_that("train() BART Classification succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_c_bart, Classification)
})

## {BART}[train]<Classification> Grid search ----
modt_c_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_BART(
      num_trees = c(5L, 10L),
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() BART Classification with grid search succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(modt_c_bart, Classification)
})

## {BART}[train]<ClassificationRes> ----
resmod_c_bart <- fit_if_installed(
  "stochtree",
  train(
    x = datc2,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      seed = 2026L
    ),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res BART Classification succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(resmod_c_bart, ClassificationRes)
})

## {BART}[train]<Classification> IFW ----
# stochtree applies case weights to the residual variance under the default
# probit link, so IFW is honored rather than rejected.
mod_c_bart_ifw <- fit_if_installed(
  "stochtree",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      ifw = TRUE,
      seed = 2026L
    )
  )
)
test_that("train() BART Classification with IFW succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_c_bart_ifw, Classification)
})

## {BART}[train]<Classification> cloglog link ----
mod_c_bart_cloglog <- fit_if_installed(
  "stochtree",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 10L,
      link = "cloglog",
      seed = 2026L
    )
  )
)
test_that("train() BART Classification with the cloglog link succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_c_bart_cloglog, Classification)
})

## {BART}[predict]<Regression> ----
predicted_bart <- fit_if_installed(
  "stochtree",
  predict(mod_r_bart, features(datr_test))
)
test_that("predict() BART Regression succeeds", {
  skip_if_not_installed("stochtree")
  expect_identical(mod_r_bart@predicted_test, predicted_bart)
  expect_null(dim(predicted_bart))
})

## {BART}[predict]<Classification> ----
test_that("predict() BART Classification returns second-level probabilities", {
  skip_if_not_installed("stochtree")
  predicted_prob <- predict(mod_c_bart, features(datc2_test))
  expect_identical(NCOL(predicted_prob), 1L)
  expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
  expect_identical(mod_c_bart@predicted_prob_test, predicted_prob)
  # A flipped column would still be a valid probability, so check it tracks the
  # outcome rather than its complement.
  expect_gt(
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[2L]]),
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
  )
})

## {BART}[se]<Regression> ----
# The posterior spread of the retained MCMC draws.
test_that("se() BART Regression returns one standard error per case", {
  skip_if_not_installed("stochtree")
  se_bart <- se(mod_r_bart, features(datr_test))
  expect_type(se_bart, "double")
  expect_length(se_bart, nrow(datr_test))
  expect_true(all(se_bart > 0))
})

## {BART}[varimp]<Regression> ----
# Split counts are summed back onto the feature each design column came from,
# so a factor expanded across several design columns still contributes exactly
# one row, and the inclusion proportions sum to 1.
test_that("get_varimp() BART Regression returns inclusion proportions", {
  skip_if_not_installed("stochtree")
  varimp_bart <- get_varimp(mod_r_bart)
  expect_s7_class(varimp_bart, VariableImportance)
  expect_identical(varimp_bart@data[["variable"]], mod_r_bart@xnames)
  expect_equal(sum(varimp_bart@data[["importance"]]), 1)
})

test_that("get_varimp() BART reports inclusion spread beside the mean", {
  skip_if_not_installed("stochtree")
  varimp_bart <- get_varimp(mod_r_bart)
  # Two measures, so `plot_varimp(measure = )` has something to select.
  expect_identical(
    names(varimp_bart@data),
    c("variable", "importance", "inclusion_sd")
  )
  expect_true(all(varimp_bart@data[["inclusion_sd"]] >= 0))
  # The spread is across draws, so it must vanish when there is only one.
  varimp_one_draw <- get_varimp(train(
    x = datr_train,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 1L,
      seed = 2026L
    )
  ))
  expect_true(all(varimp_one_draw@data[["inclusion_sd"]] == 0))
})

test_that("get_varimp() BART is invariant to sampler budget", {
  skip_if_not_installed("stochtree")
  # A raw split count scales with the number of retained samples; a proportion
  # does not, which is the whole reason for normalizing.
  varimp_long <- get_varimp(train(
    x = datr_train,
    hyperparameters = setup_BART(
      num_trees = 10L,
      num_gfr = 2L,
      num_mcmc = 40L,
      seed = 2026L
    )
  ))
  expect_equal(sum(varimp_long@data[["importance"]]), 1)
  # Same data and prior, four times the draws: the ranking must not move.
  expect_identical(
    order(varimp_long@data[["importance"]]),
    order(get_varimp(mod_r_bart)@data[["importance"]])
  )
})

## {BART}[train]<Regression> Algorithm name dispatch ----
# The algorithmDB row is what makes the name resolvable; without it this
# aborts with "Incorrect algorithm specified".
mod_r_bart_byname <- fit_if_installed(
  "stochtree",
  train(
    x = datr_train,
    hyperparameters = get_default_hyperparameters("bart")
  )
)
test_that("train() BART from its algorithm name succeeds", {
  skip_if_not_installed("stochtree")
  expect_s7_class(mod_r_bart_byname, Regression)
  expect_identical(get_alg_name("bart"), "BART")
})

## {BART}[train]<Classification> /\Error multiclass unsupported ----
# stochtree models a discrete outcome as binary or ordinal; rtemis outcomes are
# unordered, so more than two classes has no mapping.
test_that("train() BART aborts on multiclass classification", {
  skip_if_not_installed("stochtree")
  expect_error(
    train(
      x = datc3_train,
      hyperparameters = setup_BART(num_trees = 10L, num_gfr = 2L, num_mcmc = 5L)
    ),
    class = "rtemis_unsupported_error"
  )
})

## {BART}[train]<Classification> /\Error weights under cloglog ----
# stochtree rejects observation weights under a cloglog link, so IFW cannot be
# honored there and must fail loudly rather than fit an unweighted model.
test_that("train() BART aborts when ifw is combined with the cloglog link", {
  skip_if_not_installed("stochtree")
  expect_error(
    train(
      x = datc2_train,
      hyperparameters = setup_BART(
        num_trees = 10L,
        num_gfr = 2L,
        num_mcmc = 5L,
        link = "cloglog",
        ifw = TRUE
      )
    ),
    class = "rtemis_unsupported_error"
  )
})

## {BART}[train]<Regression> /\Error num_features_subsample > n features ----
test_that("train() BART aborts when num_features_subsample exceeds n features", {
  skip_if_not_installed("stochtree")
  expect_error(
    train(
      x = datr_train,
      hyperparameters = setup_BART(
        num_trees = 10L,
        num_gfr = 2L,
        num_mcmc = 5L,
        num_features_subsample = 100L
      )
    ),
    class = "rtemis_range_error"
  )
})

test_that("train() BART aborts when any search value of num_features_subsample is out of range", {
  skip_if_not_installed("stochtree")
  expect_error(
    train(
      x = datr_train,
      hyperparameters = setup_BART(
        num_trees = 10L,
        num_gfr = 2L,
        num_mcmc = 5L,
        num_features_subsample = c(3L, 100L)
      )
    ),
    class = "rtemis_range_error"
  )
})

## {BART}[train]<Regression> Throw error with missing data ----
test_that("train() BART Regression with missing data throws error", {
  skip_if_not_installed("stochtree")
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_BART(num_trees = 10L, num_gfr = 2L, num_mcmc = 5L)
    )
  )
})


# --- HAL ------------------------------------------------------------------------------------------
# The basis grows as C(n_features, max_degree), so every fit below stays at
# max_degree = 1 to keep runtime down. The internal cross-validation that
# selects lambda draws folds at random, so every fit fixes `seed`.
## {HAL}[train]<Regression> ----
mod_r_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L)
  )
)
test_that("train() HAL Regression succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_r_hal, Regression)
})

## {HAL}[train]<Regression> Grid search ----
modt_r_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_HAL(
      max_degree = 1L,
      smoothness_orders = c(0L, 1L),
      seed = 2026L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() HAL Regression with grid search succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(modt_r_hal, Regression)
})

## {HAL}[train]<RegressionRes> ----
resmod_r_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datr,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res HAL Regression succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(resmod_r_hal, RegressionRes)
})

## {HAL}[train]<Classification> ----
mod_c_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L)
  )
)
test_that("train() HAL Classification succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_c_hal, Classification)
})

## {HAL}[train]<Classification> Grid search ----
modt_c_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_HAL(
      max_degree = 1L,
      smoothness_orders = c(0L, 1L),
      seed = 2026L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() HAL Classification with grid search succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(modt_c_hal, Classification)
})

## {HAL}[train]<ClassificationRes> ----
resmod_c_hal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res HAL Classification succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(resmod_c_hal, ClassificationRes)
})

## {HAL}[train]<Classification> IFW ----
# The backend forwards case weights to the lasso, so IFW is honored.
mod_c_hal_ifw <- fit_if_installed(
  "hal9001",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_HAL(max_degree = 1L, ifw = TRUE, seed = 2026L)
  )
)
test_that("train() HAL Classification with IFW succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_c_hal_ifw, Classification)
})

## {HAL}[train]<Regression> Reproducible fold assignment ----
# `seed` reaches the internal cross-validation as a fold vector, so the lambda
# it selects must not depend on the ambient RNG.
test_that("train() HAL seeds the internal cross-validation reproducibly", {
  skip_if_not_installed("hal9001")
  set.seed(1L)
  first <- train(
    x = datr_train,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L)
  )
  set.seed(99L)
  second <- train(
    x = datr_train,
    hyperparameters = setup_HAL(max_degree = 1L, seed = 2026L)
  )
  expect_identical(
    first@model[["lambda_star"]],
    second@model[["lambda_star"]]
  )
})

## {HAL}[predict]<Regression> ----
predicted_hal <- fit_if_installed(
  "hal9001",
  predict(mod_r_hal, features(datr_test))
)
test_that("predict() HAL Regression succeeds", {
  skip_if_not_installed("hal9001")
  expect_identical(mod_r_hal@predicted_test, predicted_hal)
  expect_null(dim(predicted_hal))
})

## {HAL}[predict]<Classification> ----
test_that("predict() HAL Classification returns second-level probabilities", {
  skip_if_not_installed("hal9001")
  predicted_prob <- predict(mod_c_hal, features(datc2_test))
  expect_identical(NCOL(predicted_prob), 1L)
  expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
  expect_identical(mod_c_hal@predicted_prob_test, predicted_prob)
  # A flipped column would still be a valid probability, so check it tracks the
  # outcome rather than its complement.
  expect_gt(
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[2L]]),
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
  )
})

## {HAL}[varimp]<Regression> ----
# Coefficients are aggregated over the basis functions each feature appears in,
# and the basis is built over the one-hot encoded design matrix, so a factor
# contributes one row per level rather than one row per feature.
test_that("get_varimp() HAL Regression aggregates coefficients per feature", {
  skip_if_not_installed("hal9001")
  varimp_hal <- get_varimp(mod_r_hal)
  expect_s7_class(varimp_hal, VariableImportance)
  expect_gt(nrow(varimp_hal@data), length(mod_r_hal@xnames))
  expect_true(all(varimp_hal@data[["importance"]] >= 0))
})

test_that("get_varimp() HAL reports the peak coefficient beside the sum", {
  skip_if_not_installed("hal9001")
  varimp_hal <- get_varimp(mod_r_hal)
  # Two measures, so `plot_varimp(measure = )` has something to select.
  expect_identical(
    names(varimp_hal@data),
    c("variable", "importance", "max_coefficient")
  )
  # A sum over the same terms the maximum is taken over cannot be smaller.
  expect_true(all(
    varimp_hal@data[["importance"]] >= varimp_hal@data[["max_coefficient"]]
  ))
})

test_that("get_varimp() HAL recovers the features the outcome was built from", {
  skip_if_not_installed("hal9001")
  # datr's y is x[, 3] + x[, 5] + a factor effect. At smoothness_orders = 0 the
  # basis is made of indicators, so the coefficients are unit-free and the two
  # signal columns must outrank the three noise ones.
  varimp_hal <- get_varimp(train(
    x = datr_train,
    hyperparameters = setup_HAL(
      max_degree = 1L,
      smoothness_orders = 0L,
      seed = 2026L
    )
  ))
  importance <- setNames(
    varimp_hal@data[["importance"]],
    varimp_hal@data[["variable"]]
  )
  expect_gt(
    min(importance[c("V3", "V5")]),
    max(importance[c("V1", "V2", "V4")])
  )
})

## {HAL}[train]<Regression> Algorithm name dispatch ----
# The algorithmDB row is what makes the name resolvable; without it this
# aborts with "Incorrect algorithm specified".
mod_r_hal_byname <- fit_if_installed(
  "hal9001",
  train(
    x = datr_train,
    hyperparameters = get_default_hyperparameters("hal")
  )
)
test_that("train() HAL from its algorithm name succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_r_hal_byname, Regression)
  expect_identical(get_alg_name("hal"), "HAL")
})

## {HAL}[train]<Classification> /\Error multiclass unsupported ----
# hal9001 has no multinomial family.
test_that("train() HAL aborts on multiclass classification", {
  skip_if_not_installed("hal9001")
  expect_error(
    train(x = datc3_train, hyperparameters = setup_HAL(max_degree = 1L)),
    class = "rtemis_unsupported_error"
  )
})

## {HAL}[train]<Regression> /\Error max_degree > n features ----
test_that("train() HAL aborts when max_degree exceeds n features", {
  skip_if_not_installed("hal9001")
  expect_error(
    train(x = datr_train, hyperparameters = setup_HAL(max_degree = 100L)),
    class = "rtemis_range_error"
  )
})

test_that("train() HAL aborts when any search value of max_degree is out of range", {
  skip_if_not_installed("hal9001")
  expect_error(
    train(
      x = datr_train,
      hyperparameters = setup_HAL(max_degree = c(1L, 100L))
    ),
    class = "rtemis_range_error"
  )
})

## {HAL}[train]<Regression> /\Error projected basis over max_basis ----
# The basis is enumerated before anything is fit, so an over-large one has to
# be caught by projection rather than by failing.
test_that("train() HAL aborts when the projected basis exceeds max_basis", {
  skip_if_not_installed("hal9001")
  expect_error(
    train(
      x = datr_train,
      hyperparameters = setup_HAL(max_degree = 2L, max_basis = 100L)
    ),
    class = "rtemis_range_error"
  )
  # The same fit goes ahead once the ceiling is raised past the projection.
  expect_s7_class(
    train(
      x = datr_train,
      hyperparameters = setup_HAL(
        max_degree = 2L,
        num_knots = c(4L, 2L),
        seed = 2026L
      )
    ),
    Regression
  )
})

## {HAL}[train]<Regression> Throw error with missing data ----
test_that("train() HAL Regression with missing data throws error", {
  skip_if_not_installed("hal9001")
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_HAL(max_degree = 1L)
    )
  )
})


# --- MonotonicHAL --------------------------------------------------------------------------------
# Interaction degree is fixed at 1 by the algorithm, so the basis stays small.
# The internal cross-validation that selects lambda draws folds at random, so
# every fit fixes `seed`.
## {MonotonicHAL}[train]<Regression> ----
mod_r_monotonichal <- fit_if_installed(
  "hal9001",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MonotonicHAL(seed = 2026L)
  )
)
test_that("train() MonotonicHAL Regression succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_r_monotonichal, Regression)
})

## {MonotonicHAL}[train]<RegressionRes> ----
resmod_r_monotonichal <- fit_if_installed(
  "hal9001",
  train(
    x = datr,
    hyperparameters = setup_MonotonicHAL(seed = 2026L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res MonotonicHAL Regression succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(resmod_r_monotonichal, RegressionRes)
})

## {MonotonicHAL}[train]<Classification> ----
mod_c_monotonichal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MonotonicHAL(seed = 2026L)
  )
)
test_that("train() MonotonicHAL Classification succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(mod_c_monotonichal, Classification)
})

## {MonotonicHAL}[train]<Classification> Grid search ----
modt_c_monotonichal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MonotonicHAL(
      smoothness_orders = c(0L, 1L),
      seed = 2026L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() MonotonicHAL Classification with grid search succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(modt_c_monotonichal, Classification)
})

## {MonotonicHAL}[train]<ClassificationRes> ----
resmod_c_monotonichal <- fit_if_installed(
  "hal9001",
  train(
    x = datc2,
    hyperparameters = setup_MonotonicHAL(seed = 2026L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res MonotonicHAL Classification succeeds", {
  skip_if_not_installed("hal9001")
  expect_s7_class(resmod_c_monotonichal, ClassificationRes)
})

## {MonotonicHAL}[train]<Classification> Multiclass ----
test_that("train() MonotonicHAL rejects multiclass classification", {
  skip_if_not_installed("hal9001")
  # hal9001 has no multinomial family.
  expect_error(
    train(
      x = datc3_train,
      hyperparameters = setup_MonotonicHAL(seed = 2026L),
      verbosity = 0L
    ),
    "multiclass"
  )
})

## {MonotonicHAL}[predict]<Classification> Monotonicity ----
test_that("MonotonicHAL predictions are non-decreasing in the feature", {
  skip_if_not_installed("hal9001")
  # The constraint is the reason the algorithm exists; assert it directly on a
  # single-feature fit, where "non-decreasing in the feature" is unambiguous.
  set.seed(2026)
  n <- 300L
  score <- runif(n)
  labels <- factor(
    ifelse(rbinom(n, 1L, plogis(4 * (score - 0.5))) == 1L, "pos", "neg"),
    levels = c("neg", "pos")
  )
  mod <- train(
    data.table(score = score, labels = labels),
    hyperparameters = setup_MonotonicHAL(seed = 2026L),
    verbosity = 0L
  )
  predicted <- predict(mod, data.frame(score = seq(0, 1, length.out = 200L)))
  expect_true(all(diff(as.numeric(predicted)) >= -1e-10))
})

## {MonotonicHAL}[varimp]<Classification> ----
test_that("get_varimp() MonotonicHAL returns a VariableImportance", {
  skip_if_not_installed("hal9001")
  # Dispatch is shared with HAL: both fit the `hal9001` class.
  expect_s7_class(get_varimp(mod_c_monotonichal), VariableImportance)
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
    hyperparameters = setup_GLM(),
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
    hyperparameters = setup_GLM(),
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
test_that("calibrate() GLM ClassificationRes succeeds", {
  expect_s7_class(resmod_c_glm_cal, CalibratedClassificationRes)
})

## {GLM}[predict]<CalibratedClassificationRes> ----
test_that("predict() GLM CalibratedClassificationRes succeeds", {
  predicted_cal <- predict(resmod_c_glm_cal, features(datc2_test))
  expect_type(predicted_cal, "double")
  expect_length(predicted_cal, nrow(datc2_test))
})

## {CART}[calibrate]<ClassificationRes> ----
# Using resmodt_c_cart from above
resmodt_c_cart_cal <- calibrate(resmodt_c_cart)
test_that("calibrate() CART ClassificationRes succeeds", {
  expect_s7_class(resmodt_c_cart_cal, CalibratedClassificationRes)
})

## {CART}[predict]<CalibratedClassificationRes> ----
test_that("predict() CART CalibratedClassificationRes succeeds", {
  predicted_cal <- predict(resmodt_c_cart_cal, features(datc2_test))
  expect_type(predicted_cal, "double")
  expect_length(predicted_cal, nrow(datc2_test))
})


# %% Test preprocessing in train() is applied to test data in predict() ----
## {GLM}[train]<Classification> Preprocessing ----
mod_c_glm_pp <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLM(),
  preprocessor = setup_Preprocessor(
    scale = TRUE,
    center = TRUE
  )
)
test_that("train() with preprocessor creates a model with the preprocessor", {
  expect_s7_class(mod_c_glm_pp, Classification)
  expect_true(!is.null(mod_c_glm_pp@preprocessor))
})


# %% Resolved hyperparameters reach the fitted model ----
# "All runs are observable": a model that trained with an `objective` it chose
# from the outcome type must say so. R copies `hyperparameters` into `train_()`,
# so a method that resolves a value has to return it or the caller keeps the
# unresolved object.
test_that("a fitted model reports the values its algorithm resolved", {
  mod <- train(iris, hyperparameters = setup_LightRF(), verbosity = 0L)
  # NULL means "decide from the outcome type" in the config; on a fitted model
  # it would mean the record cannot say what was run.
  expect_identical(mod@hyperparameters[["objective"]], "multiclass")
  expect_false(is.null(mod@hyperparameters[["feature_fraction"]]))
  expect_true(mod@hyperparameters[["feature_fraction"]] > 0)
})

test_that("resolved values survive for every algorithm that resolves one", {
  mod <- train(iris, hyperparameters = setup_LightGBM(), verbosity = 0L)
  expect_identical(mod@hyperparameters[["objective"]], "multiclass")
  # `nrounds` is resolved from `max_nrounds` / early stopping, so a fitted
  # model must report the count it actually ran.
  expect_false(is.null(mod@hyperparameters[["nrounds"]]))

  dat <- data.frame(a = rnorm(40L), b = rnorm(40L), y = rnorm(40L))
  glmnet_mod <- train(dat, hyperparameters = setup_GLMNET(), verbosity = 0L)
  # `lambda` is chosen by cv.glmnet during the fit.
  expect_false(is.null(glmnet_mod@hyperparameters[["lambda"]]))
})


# %% The run's input is stored beside what ran ----
# A record must say where each value came from, and that needs both: the
# resolved hyperparameters say what ran, the stored config says what was asked
# for. Neither is derivable from the other.
test_that("train() stores the input config alongside the resolved one", {
  mod <- train(iris, hyperparameters = setup_LightRF(), verbosity = 0L)
  expect_s7_class(mod@config, SuperConfig)
  # The distinction that makes `origin` computable: NULL in, resolved out.
  expect_null(mod@config@hyperparameters[["objective"]])
  expect_identical(mod@hyperparameters[["objective"]], "multiclass")
  # A value the user did supply reads back unchanged on both sides.
  expect_identical(mod@config@hyperparameters[["nrounds"]], 500L)
})

test_that("a tuning search space survives on the input config", {
  # After tuning, `@hyperparameters` holds the single chosen value; only the
  # input still shows it was searched, which is what marks the origin `tuned`.
  mod <- train(
    iris,
    hyperparameters = setup_CART(maxdepth = c(2L, 4L)),
    tuner_config = setup_GridSearch(
      resampler_config = setup_Resampler(n_resamples = 2L, type = "KFold")
    ),
    verbosity = 0L
  )
  expect_length(mod@config@hyperparameters[["maxdepth"]], 2L)
  expect_length(mod@hyperparameters[["maxdepth"]], 1L)
})

test_that("only the top-level call carries the input", {
  # One record per `train()` call, not one per outer resample: a sub-model
  # shares its parent's input.
  mod <- train(
    iris,
    hyperparameters = setup_CART(),
    outer_resampling_config = setup_Resampler(n_resamples = 2L, type = "KFold"),
    verbosity = 0L
  )
  expect_s7_class(mod@config, SuperConfig)
  expect_null(mod@models[[1L]]@config)
})


# %% Records are written and are not configs ----
test_that("train() writes a record beside the model when outdir is set", {
  outdir <- file.path(
    tempdir(),
    paste0("rec_", as.integer(runif(1L, 1e6, 9e6)))
  )
  dir.create(outdir, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  train(iris, hyperparameters = setup_CART(), outdir = outdir, verbosity = 0L)
  # Paired with the model by name, so a directory of runs reads by inspection.
  expect_true(file.exists(file.path(outdir, "train_CART.rds")))
  expect_true(file.exists(file.path(outdir, "train_CART.record.json")))
})

test_that("a record is rejected where a config is expected", {
  outdir <- file.path(
    tempdir(),
    paste0("rec_", as.integer(runif(1L, 1e6, 9e6)))
  )
  dir.create(outdir, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  train(iris, hyperparameters = setup_CART(), outdir = outdir, verbosity = 0L)
  # A record reads *as* a config -- same field names, every value resolved --
  # so silent acceptance would pin settings this call should decide, including
  # ones derived from data it never saw.
  err <- tryCatch(
    read_config(file.path(outdir, "train_CART.record.json")),
    error = function(e) e
  )
  expect_s3_class(err, "rtemis_value_error")
  expect_match(conditionMessage(err), "record", fixed = TRUE)
})

test_that("a record reports what ran, not what was asked for", {
  mod <- train(iris, hyperparameters = setup_LightRF(), verbosity = 0L)
  rec <- record(mod)
  # Top level is what was *asked for*: NULL, meaning "decide from the outcome".
  expect_null(rec[["hyperparameters"]][["hyperparameters"]][["objective"]])
  # `folds` is what *ran*. A single fit is one fold, not a second shape.
  expect_length(rec[["folds"]], 1L)
  hp <- rec[["folds"]][[1L]][["hyperparameters"]][["hyperparameters"]]
  expect_identical(hp[["objective"]], "multiclass")
  expect_identical(hp[["origin"]][["objective"]], "default")
  expect_identical(rec[["provenance"]][["outcome"]], "completed")
  expect_true(nzchar(rec[["provenance"]][["rtemis_version"]]))
})

test_that("record() refuses a model with no stored input", {
  # A per-fold sub-model shares its parent's input, so it cannot say what was
  # asked for and must not guess.
  mod <- train(
    iris,
    hyperparameters = setup_CART(),
    outer_resampling_config = setup_Resampler(n_resamples = 2L, type = "KFold"),
    verbosity = 0L
  )
  expect_error(record(mod@models[[1L]]), class = "rtemis_null_input")
})


# %% A resampled record loses nothing ----
test_that("each fold's resolved values are recorded separately", {
  # Early stopping settles on a different `nrounds` per fold, so collapsing
  # them to one value would state something no fold did.
  mod <- train(
    iris,
    hyperparameters = setup_LightGBM(),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    verbosity = 0L
  )
  rec <- record(mod)
  expect_length(rec[["folds"]], 3L)
  # Asked for: nothing -- "determine by early stopping".
  expect_null(rec[["hyperparameters"]][["hyperparameters"]][["nrounds"]])
  ran <- vapply(
    rec[["folds"]],
    function(f) f[["hyperparameters"]][["hyperparameters"]][["nrounds"]],
    numeric(1L)
  )
  expect_length(ran, 3L)
  expect_true(all(ran > 0))
  # Each fold says how its value came about.
  expect_identical(
    rec[["folds"]][[1L]][["hyperparameters"]][["hyperparameters"]][["origin"]][[
      "nrounds"
    ]],
    "tuned"
  )
})

test_that("a tuned run records the grid and the winner per fold", {
  mod <- train(
    iris,
    hyperparameters = setup_CART(maxdepth = c(2L, 4L)),
    tuner_config = setup_GridSearch(
      # Seeded: which depth wins follows from the fold split, so an unseeded
      # resampler makes this assertion depend on the global RNG position and
      # therefore on every test that ran before it.
      resampler_config = setup_Resampler(
        n_resamples = 2L,
        type = "KFold",
        seed = 2026L
      )
    ),
    verbosity = 0L
  )
  tuning <- record(mod)[["folds"]][[1L]][["tuning"]]
  # The search must be re-examinable from the record alone, not just its result.
  expect_true(all(
    c("param_grid", "training", "validation", "best") %in% names(tuning)
  ))
  # `best` reaches the record from the tuner's results and `@hyperparameters`
  # from what `train()` adopted, so agreeing proves the record names the value
  # the kept model was actually trained with.
  expect_identical(
    tuning[["best"]][["maxdepth"]],
    mod@hyperparameters[["maxdepth"]]
  )
})
