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
# MARS classification fits a binomial glm on the MARS basis, one per response
# column, so it gives the same warnings on both iris fixtures.

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
    hyperparameters = setup_GLMNET(alpha = tune_over(0, 1)),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  expect_s7_class(modt_r_glmnet, Regression)
})

## {GLMNET}[train]<RegressionRes> auto-lambda + alpha grid search ----
test_that("train() Res-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  resmodt_r_glmnet <- train(
    x = datr_train,
    hyperparameters = setup_GLMNET(alpha = tune_over(0.5, 1)),
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
  hyperparameters = setup_GAM(k = tune_over(3, 5, 7))
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
  hyperparameters = setup_LinearSVM(cost = tune_over(1, 10)),
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
  hyperparameters = setup_RadialSVM(cost = tune_over(1, 10, 100))
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
  hyperparameters = setup_RadialSVM(cost = tune_over(1, 10)),
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
  hyperparameters = setup_RadialSVM(cost = tune_over(1, 10)),
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
  hyperparameters = setup_RadialSVM(cost = tune_over(1, 10)),
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
  maxdepth = tune_over(1, 2, 10),
  minbucket = tune_over(1L, 4L)
)
test_that("tuned field is set correctly", {
  expect_identical(hyperparameters@tuned, 0L)
})

modt_r_cart <- train(
  datr_train,
  dat_test = datr_test,
  hyperparameters = setup_CART(maxdepth = tune_over(2:3)),
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
  hyperparameters = setup_CART(
    maxdepth = tune_over(1:2),
    prune_cp = tune_over(.001, .01)
  ),
  outer_resampling_config = setup_Resampler(3),
  execution_config = setup_ExecutionConfig(backend = "none")
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmodt_r_cart, RegressionRes)
})

## {CART}[train]<RegressionRes> prune_cp ----
resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(prune_cp = tune_over(.001, .01)),
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
  hyperparameters = setup_CART(maxdepth = tune_over(1:2))
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
    maxdepth = tune_over(1L, 2L)
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
    maxdepth = tune_over(1L, 2L)
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
    lambda_l1 = tune_over(0, .1)
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
    lambda_l1 = tune_over(0, 10)
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
  hyperparameters = setup_LightRF(nrounds = 20L, max_depth = tune_over(-1, 5))
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
  hyperparameters = setup_Ranger(num_trees = 50L, mtry = tune_over(3, 6)),
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
  hyperparameters = setup_Ranger(num_trees = 10L, mtry = tune_over(2, 4)),
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
      hyperparameters = setup_Ranger(
        num_trees = 10L,
        mtry = tune_over(3L, 100L)
      )
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
    hyperparameters = setup_SPLS(
      k = tune_over(1L, 2L),
      eta = tune_over(0.3, 0.6)
    ),
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
    hyperparameters = setup_SPLS(k = tune_over(1L, 2L), eta = 0.3),
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
      hyperparameters = setup_SPLS(k = tune_over(2L, 100L))
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

# --- MARS -----------------------------------------------------------------------------------------
## {MARS}[train]<Regression> ----
mod_r_mars <- fit_if_installed(
  "earth",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MARS(degree = 1L, nprune = 8L)
  )
)
test_that("train() MARS Regression succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(mod_r_mars, Regression)
})

## {MARS}[train]<Regression> Grid search ----
modt_r_mars <- fit_if_installed(
  "earth",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MARS(degree = tune_over(1L, 2L), nprune = 8L),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() MARS Regression with grid search succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(modt_r_mars, Regression)
})

## {MARS}[train]<Regression> Internal cross-validated pruning ----
# `pmethod = "cv"` picks the number of terms from out-of-fold error inside
# earth, which is the one path that reads `nfold`, `ncross` and `stratify`.
mod_r_mars_cv <- fit_if_installed(
  "earth",
  train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MARS(degree = 1L, pmethod = "cv", nfold = 3L)
  )
)
test_that("train() MARS Regression with cross-validated pruning succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(mod_r_mars_cv, Regression)
})

## {MARS}[train]<RegressionRes> ----
resmod_r_mars <- fit_if_installed(
  "earth",
  train(
    x = datr,
    hyperparameters = setup_MARS(degree = 1L, nprune = 8L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
)
test_that("train() Res MARS Regression succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(resmod_r_mars, RegressionRes)
})

## {MARS}[train]<Classification> ----
mod_c_mars <- fit_if_installed(
  "earth",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MARS(degree = 1L, nprune = 6L)
  )
)
test_that("train() MARS Classification succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(mod_c_mars, Classification)
})

## {MARS}[train]<Classification> IFW ----
mod_c_mars_ifw <- fit_if_installed(
  "earth",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MARS(degree = 1L, nprune = 6L, ifw = TRUE)
  )
)
test_that("train() MARS Classification with IFW succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(mod_c_mars_ifw, Classification)
})

## {MARS}[train]<Classification> Grid search ----
modt_c_mars <- fit_if_installed(
  "earth",
  train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MARS(degree = 1L, nprune = tune_over(4L, 6L)),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() MARS Classification with grid search succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(modt_c_mars, Classification)
})

## {MARS}[train]<ClassificationRes> ----
resmod_c_mars <- fit_if_installed(
  "earth",
  train(
    x = datc2,
    hyperparameters = setup_MARS(degree = 1L, nprune = 6L),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
)
test_that("train() Res MARS Classification succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(resmod_c_mars, ClassificationRes)
})

## {MARS}[train]<Classification> Multiclass ----
modt_c3_mars <- fit_if_installed(
  "earth",
  train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_MARS(degree = 1L, nprune = 6L)
  )
)
test_that("train() MARS Multiclass Classification succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(modt_c3_mars, Classification)
})

## {MARS}[predict]<Regression> ----
predicted_mars <- fit_if_installed(
  "earth",
  predict(mod_r_mars, features(datr_test))
)
test_that("predict() MARS Regression succeeds", {
  skip_if_not_installed("earth")
  expect_identical(mod_r_mars@predicted_test, predicted_mars)
  expect_null(dim(predicted_mars))
})

## {MARS}[predict]<Classification> ----
# Classification fits a binomial GLM per response column. Binary codes the
# second level as 1, so its single column is already that level's probability.
test_that("predict() MARS Classification returns second-level probabilities", {
  skip_if_not_installed("earth")
  predicted_prob <- predict(mod_c_mars, features(datc2_test))
  expect_identical(NCOL(predicted_prob), 1L)
  expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
  expect_identical(mod_c_mars@predicted_prob_test, predicted_prob)
  # A flipped column would still be a valid probability, so check it tracks the
  # outcome rather than its complement.
  expect_gt(
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[2L]]),
    mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
  )
})

test_that("predict() MARS Multiclass returns one column per class", {
  skip_if_not_installed("earth")
  predicted_prob <- predict(modt_c3_mars, features(datc3_test))
  expect_identical(NCOL(predicted_prob), nlevels(datc3_test$Species))
  # The per-class GLMs are independent, so predict_super() normalizes them.
  expect_equal(unname(rowSums(predicted_prob)), rep(1, nrow(datc3_test)))
})

## {MARS}[varimp]<Regression> ----
test_that("get_varimp() MARS Regression reports earth's three criteria", {
  skip_if_not_installed("earth")
  vi <- get_varimp(mod_r_mars)
  expect_s7_class(vi, VariableImportance)
  # Column 2 is what plot_varimp() shows by default, so the order is contract.
  expect_identical(
    names(vi@data),
    c("variable", "importance", "rss", "subset_proportion")
  )
  # One row per design-matrix column: the factor `g` is one-hot encoded before
  # earth sees it, so this exceeds the feature count.
  expect_gte(nrow(vi@data), length(mod_r_mars@xnames))
  expect_true(all(vi@data[["subset_proportion"]] >= 0))
  expect_true(all(vi@data[["subset_proportion"]] <= 1))
})

test_that("get_varimp() MARS recovers the features that drive the outcome", {
  skip_if_not_installed("earth")
  # datr is y = V3 + V5 + a `g` effect, so those must outrank the pure noise.
  vi <- get_varimp(mod_r_mars)@data
  top <- vi[["variable"]][order(vi[["importance"]], decreasing = TRUE)][1:3]
  expect_true(all(c("V3", "V5") %in% top))
})

test_that("get_varimp() MARS subset_proportion is comparable across model sizes", {
  skip_if_not_installed("earth")
  # earth's own count scales with the number of terms, so a grid search over
  # `nprune` would report two incomparable scales. The proportion does not.
  small <- get_varimp(
    train(x = datr_train, hyperparameters = setup_MARS(nprune = 4L))
  )@data
  large <- get_varimp(
    train(x = datr_train, hyperparameters = setup_MARS(nprune = 12L))
  )@data
  expect_true(all(small[["subset_proportion"]] <= 1))
  expect_true(all(large[["subset_proportion"]] <= 1))
  expect_identical(
    small[["variable"]][which.max(small[["subset_proportion"]])],
    large[["variable"]][which.max(large[["subset_proportion"]])]
  )
})

## {MARS}[train]<Regression> Algorithm name dispatch ----
# The algorithmDB row is what makes the name resolvable; without it this
# aborts with "Incorrect algorithm specified".
mod_r_mars_byname <- fit_if_installed(
  "earth",
  train(
    x = datr_train,
    hyperparameters = get_default_hyperparameters("mars")
  )
)
test_that("train() MARS from its algorithm name succeeds", {
  skip_if_not_installed("earth")
  expect_s7_class(mod_r_mars_byname, Regression)
  expect_identical(get_alg_name("mars"), "MARS")
})

## {MARS}[train]<Classification> /\Error multiclass pmethod ----
# earth prunes a multi-column response with "backward" or "none" only. The
# check belongs to validate_hyperparameters(), so it must fire before tuning
# rather than being swallowed per grid cell.
test_that("train() MARS aborts on multiclass with an unsupported pmethod", {
  skip_if_not_installed("earth")
  expect_error(
    train(
      x = datc3_train,
      hyperparameters = setup_MARS(nprune = 6L, pmethod = "forward")
    ),
    class = "rtemis_value_error"
  )
})

test_that("train() MARS allows an unsupported pmethod on binary outcomes", {
  skip_if_not_installed("earth")
  # A binary outcome is a single response column, so the restriction does not
  # apply and the same setting must go through.
  expect_s7_class(
    train(
      x = datc2_train,
      hyperparameters = setup_MARS(nprune = 6L, pmethod = "forward")
    ),
    Classification
  )
})

## {MARS}[train]<Regression> /\Error nfold > n cases ----
test_that("train() MARS aborts when nfold exceeds n cases", {
  skip_if_not_installed("earth")
  expect_error(
    train(
      x = datr_train,
      hyperparameters = setup_MARS(pmethod = "cv", nfold = 10000L)
    ),
    class = "rtemis_range_error"
  )
})

## {MARS}[train]<Regression> Throw error with missing data ----
test_that("train() MARS Regression with missing data throws error", {
  skip_if_not_installed("earth")
  expect_error(
    train(
      x = datr_train_na,
      dat_test = datr_test,
      hyperparameters = setup_MARS(degree = 1L, nprune = 8L)
    )
  )
})

# --- MLP ------------------------------------------------------------------------------------------
# Every fit is gated on libtorch being present, which `requireNamespace()`
# cannot tell: the R package installs without the runtime it downloads on first
# use. Kept tiny -- a handful of epochs on a narrow network -- and pinned to the
# cpu device, since mps does not honor a seed.
mlp_installed <- torch::torch_is_installed()

## {MLP}[train]<Regression> ----
if (mlp_installed) {
  mod_r_mlp <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 5L,
      batch_size = 64L,
      device = "cpu",
      seed = 2025L
    )
  )
  test_that("train() MLP Regression succeeds", {
    expect_s7_class(mod_r_mlp, Regression)
  })

  ## {MLP}[train]<Regression> Generated architecture ----
  # The path a user who types nothing takes: the widths come from the shape and
  # the encoded input width, and the fit reports what it used.
  mod_r_mlp_shape <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MLP(
      shape = "funnel",
      shape_layers = 2L,
      shape_max_units = 16L,
      max_epochs = 5L,
      batch_size = 64L,
      device = "cpu",
      seed = 2025L
    )
  )
  test_that("train() MLP records the architecture it generated", {
    expect_s7_class(mod_r_mlp_shape, Regression)
    # The widths land in `hidden_units` itself, beside the shape settings that
    # produced them, so the record reads them as derived rather than needing a
    # second property.
    expect_identical(
      mod_r_mlp_shape@hyperparameters[["hidden_units"]],
      c(16L, 8L)
    )
    expect_identical(mod_r_mlp_shape@hyperparameters[["shape"]], "funnel")
    expect_identical(mod_r_mlp_shape@model@hidden_units, c(16L, 8L))
  })

  ## {MLP}[train]<Regression> Grid search ----
  # One grid cell is one whole architecture: this is the first shipped
  # hyperparameter that is both vector-valued and tunable.
  modt_r_mlp <- train(
    x = datr_train,
    dat_test = datr_test,
    hyperparameters = setup_MLP(
      hidden_units = tune_over(c(8L), c(16L, 8L)),
      max_epochs = 5L,
      batch_size = 64L,
      device = "cpu",
      seed = 2025L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  test_that("train() MLP Regression with grid search succeeds", {
    expect_s7_class(modt_r_mlp, Regression)
    # The winner is whichever architecture scored best, so pin that one of the
    # two was chosen and that it came back as a value rather than the search.
    chosen <- modt_r_mlp@hyperparameters[["hidden_units"]]
    expect_type(chosen, "integer")
    expect_true(identical(chosen, 8L) || identical(chosen, c(16L, 8L)))
    expect_identical(modt_r_mlp@model@hidden_units, chosen)
  })

  ## {MLP}[train]<RegressionRes> ----
  resmod_r_mlp <- train(
    x = datr,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 5L,
      batch_size = 64L,
      device = "cpu",
      seed = 2025L
    ),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold")
  )
  test_that("train() Res MLP Regression succeeds", {
    expect_s7_class(resmod_r_mlp, RegressionRes)
  })

  ## {MLP}[train]<Classification> ----
  mod_c_mlp <- train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 30L,
      batch_size = 32L,
      device = "cpu",
      seed = 2025L
    )
  )
  test_that("train() MLP Classification succeeds", {
    expect_s7_class(mod_c_mlp, Classification)
  })

  ## {MLP}[train]<Classification> IFW ----
  mod_c_mlp_ifw <- train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 5L,
      batch_size = 32L,
      device = "cpu",
      seed = 2025L,
      ifw = TRUE
    )
  )
  test_that("train() MLP Classification with IFW succeeds", {
    expect_s7_class(mod_c_mlp_ifw, Classification)
  })

  ## {MLP}[train]<Classification> Grid search ----
  modt_c_mlp <- train(
    x = datc2_train,
    dat_test = datc2_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      lr = tune_over(1e-3, 1e-2),
      max_epochs = 5L,
      batch_size = 32L,
      device = "cpu",
      seed = 2025L
    ),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  test_that("train() MLP Classification with grid search succeeds", {
    expect_s7_class(modt_c_mlp, Classification)
  })

  ## {MLP}[train]<ClassificationRes> ----
  resmod_c_mlp <- train(
    x = datc2,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 5L,
      batch_size = 32L,
      device = "cpu",
      seed = 2025L
    ),
    outer_resampling_config = setup_Resampler(n_resamples = 3L, type = "KFold"),
    execution_config = setup_ExecutionConfig(backend = "none")
  )
  test_that("train() Res MLP Classification succeeds", {
    expect_s7_class(resmod_c_mlp, ClassificationRes)
  })

  ## {MLP}[train]<Classification> Multiclass ----
  modt_c3_mlp <- train(
    x = datc3_train,
    dat_test = datc3_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 30L,
      batch_size = 32L,
      device = "cpu",
      seed = 2025L
    )
  )
  test_that("train() MLP Multiclass Classification succeeds", {
    expect_s7_class(modt_c3_mlp, Classification)
  })

  ## {MLP}[train]<Regression> Early stopping ----
  # `patience` only bites against a validation set, and the weights kept are
  # the best epoch's rather than the last.
  mod_r_mlp_es <- train(
    x = datr_train,
    dat_validation = datr_test,
    dat_test = datr_test,
    hyperparameters = setup_MLP(
      hidden_units = c(16L, 8L),
      max_epochs = 100L,
      patience = 2L,
      batch_size = 64L,
      device = "cpu",
      seed = 2025L
    )
  )
  test_that("train() MLP stops early against a validation set", {
    expect_s7_class(mod_r_mlp_es, Regression)
    expect_lt(mod_r_mlp_es@model@epochs_trained, 100L)
    expect_lte(mod_r_mlp_es@model@best_epoch, mod_r_mlp_es@model@epochs_trained)
  })

  ## {MLP}[train]<Regression> Reproducibility ----
  test_that("train() MLP is reproducible under a fixed seed", {
    refit <- train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_MLP(
        hidden_units = c(16L, 8L),
        max_epochs = 5L,
        batch_size = 64L,
        device = "cpu",
        seed = 2025L
      ),
      verbosity = 0L
    )
    expect_equal(refit@predicted_test, mod_r_mlp@predicted_test)
  })

  ## {MLP}[predict]<Regression> ----
  predicted_mlp <- predict(mod_r_mlp, features(datr_test))
  test_that("predict() MLP Regression succeeds", {
    expect_identical(mod_r_mlp@predicted_test, predicted_mlp)
    expect_null(dim(predicted_mlp))
  })

  ## {MLP}[predict]<Regression> Survives a save/load round trip ----
  test_that("predict() MLP works on a model written to disk and read back", {
    # A torch module holds external pointers and does not survive saveRDS, so
    # the model carries its parameters serialized and rebuilds the module.
    path <- tempfile(fileext = ".rds")
    on.exit(unlink(path), add = TRUE)
    saveRDS(mod_r_mlp, path)
    expect_identical(
      predict(readRDS(path), features(datr_test)),
      predicted_mlp
    )
  })

  ## {MLP}[predict]<Classification> ----
  test_that("predict() MLP Classification returns second-level probabilities", {
    predicted_prob <- predict(mod_c_mlp, features(datc2_test))
    expect_identical(NCOL(predicted_prob), 1L)
    expect_true(all(predicted_prob >= 0 & predicted_prob <= 1))
    expect_identical(mod_c_mlp@predicted_prob_test, predicted_prob)
    # A flipped column would still be a valid probability, so check it tracks
    # the outcome rather than its complement.
    expect_gt(
      mean(predicted_prob[
        datc2_test$Species == levels(datc2_test$Species)[2L]
      ]),
      mean(predicted_prob[datc2_test$Species == levels(datc2_test$Species)[1L]])
    )
  })

  test_that("predict() MLP Multiclass returns one column per class", {
    predicted_prob <- predict(modt_c3_mlp, features(datc3_test))
    expect_identical(NCOL(predicted_prob), nlevels(datc3_test$Species))
    expect_equal(unname(rowSums(predicted_prob)), rep(1, nrow(datc3_test)))
  })

  ## {MLP}[varimp]<Regression> ----
  test_that("get_varimp() MLP returns NULL", {
    # A torch network has no native importance measure; the method exists so
    # that train() gets NULL rather than a dispatch error.
    expect_null(get_varimp(mod_r_mlp))
  })

  ## {MLP}[train]<Regression> One-hot encoding instead of embeddings ----
  test_that("train() MLP one-hot encodes when embeddings are off", {
    mod <- train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_MLP(
        hidden_units = c(8L),
        embeddings = FALSE,
        max_epochs = 5L,
        batch_size = 64L,
        device = "cpu",
        seed = 2025L
      )
    )
    expect_s7_class(mod, Regression)
    expect_length(mod@model@categorical_features, 0L)
    # `g` becomes one indicator column per level, so the design is wider than
    # the feature count.
    expect_gt(length(mod@model@numeric_features), length(mod@xnames))
  })

  ## {MLP}[train]<Regression> Residual blocks over a tapering shape ----
  test_that("train() MLP fits residual blocks across changing widths", {
    # A shortcut needs matching widths, so a taper needs a projection; without
    # one this is a run-time shape error rather than a slower fit.
    mod <- train(
      x = datr_train,
      dat_test = datr_test,
      hyperparameters = setup_MLP(
        hidden_units = c(16L, 8L),
        residual = TRUE,
        norm = "layer_norm",
        max_epochs = 5L,
        batch_size = 64L,
        device = "cpu",
        seed = 2025L
      )
    )
    expect_s7_class(mod, Regression)
  })

  ## {MLP} Device reporting ----
  test_that("training_device() names the device only for a torch algorithm", {
    # train() prints this in the line it already emits, so it has to be exact
    # and side-effect-free: the algorithm resolves the device again for real.
    expect_identical(training_device(setup_MLP(device = "cpu")), "cpu")
    expect_identical(mod_r_mlp@model@device, "cpu")
    expect_null(training_device(setup_CART()))
  })

  ## {MLP}[predict]<Regression> /\\Error missing values ----
  test_that("predict() MLP names the features holding missing values", {
    # Both kinds, and the categorical one is why this is tested: the
    # preprocessor codes NA as NA by design, so an unchecked NA index reaches
    # nn_embedding as an out-of-range lookup and libtorch reports it with a
    # C++ trace naming no column.
    newdata_numeric <- features(datr_test)
    newdata_numeric[["V1"]][1L] <- NA
    expect_error(
      predict(mod_r_mlp, newdata_numeric),
      "'V1'",
      class = "rtemis_value_error"
    )
    newdata_factor <- features(datr_test)
    newdata_factor[["g"]][1L] <- NA
    expect_error(
      predict(mod_r_mlp, newdata_factor),
      "'g'",
      class = "rtemis_value_error"
    )
  })

  ## {MLP}[train]<Regression> All features categorical ----
  test_that("train() MLP fits when every feature is categorical", {
    # The numeric side of the design is then a zero-column tensor, which is the
    # one shape that silently becomes 0 x 0 if it is built by subsetting.
    datr_cat <- data.frame(
      g = datr_train[["g"]],
      h = factor(rep_len(c("p", "q", "r"), nrow(datr_train))),
      y = datr_train[["y"]]
    )
    mod <- train(
      x = datr_cat,
      hyperparameters = setup_MLP(
        hidden_units = c(8L),
        max_epochs = 5L,
        batch_size = 64L,
        device = "cpu",
        seed = 2025L
      ),
      verbosity = 0L
    )
    expect_s7_class(mod, Regression)
    expect_length(mod@model@numeric_features, 0L)
    expect_length(mod@model@categorical_features, 2L)
  })

  ## {MLP}[train]<Regression> /\\Error loss does not fit the outcome ----
  test_that("train() MLP aborts on a loss that does not fit the outcome", {
    expect_error(
      train(
        x = datr_train,
        hyperparameters = setup_MLP(
          hidden_units = c(8L),
          loss = "cross_entropy",
          max_epochs = 2L,
          device = "cpu"
        )
      ),
      class = "rtemis_value_error"
    )
  })

  ## {MLP}[train]<Regression> Throw error with missing data ----
  test_that("train() MLP Regression with missing data throws error", {
    expect_error(
      train(
        x = datr_train_na,
        dat_test = datr_test,
        hyperparameters = setup_MLP(hidden_units = c(8L), max_epochs = 2L)
      )
    )
  })

  ## {MLP}[train]<Regression> From its algorithm name ----
  mod_r_mlp_byname <- train(
    x = datr_train,
    hyperparameters = update(
      get_default_hyperparameters("mlp"),
      list(
        hidden_units = c(8L),
        max_epochs = 3L,
        batch_size = 64L,
        device = "cpu"
      )
    )
  )
  test_that("train() MLP from its algorithm name succeeds", {
    expect_s7_class(mod_r_mlp_byname, Regression)
    expect_identical(get_alg_name("mlp"), "MLP")
  })
}

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
      k = tune_over(3L, 9L),
      kernel = tune_over("rectangular", "optimal")
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
    hyperparameters = setup_KNN(k = tune_over(3L, 9L)),
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
      hyperparameters = setup_KNN(k = tune_over(5L, nrow(datr_train)))
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
      num_trees = tune_over(5L, 10L),
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
      num_trees = tune_over(5L, 10L),
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
        num_features_subsample = tune_over(3L, 100L)
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
      smoothness_orders = tune_over(0L, 1L),
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
      smoothness_orders = tune_over(0L, 1L),
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
      hyperparameters = setup_HAL(max_degree = tune_over(1L, 100L))
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
      smoothness_orders = tune_over(0L, 1L),
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
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 4L)),
    tuner_config = setup_GridSearch(
      resampler_config = setup_Resampler(n_resamples = 2L, type = "KFold")
    ),
    verbosity = 0L
  )
  # The config keeps the domain it was given; the fitted model keeps the one
  # value the tuner picked.
  expect_length(mod@config@hyperparameters[["maxdepth"]]@candidates, 2L)
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
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 4L)),
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

test_that("a record publishes a search space as the bare tag it reads back", {
  # A `HyperparameterCandidates` is an S7 object, and the record writer builds
  # a nested record out of a config-valued property. It is not one: it is the
  # *value* the property holds, and it travels as the tag `wire_value()`
  # writes. Publishing it as a nested record put its R-side `from_vector` and
  # an `origin` of its own on the wire -- neither admitted by the schema's
  # `{"candidates": [...]}` branch -- dropped the property from the block's own
  # `origin`, the one place that reports the run searched it, and defeated
  # `is_wire_candidates()`, so nothing could read the value back.
  mod <- train(
    iris,
    hyperparameters = setup_CART(maxdepth = tune_over(2L, 4L)),
    verbosity = 0L
  )
  hp <- record(mod)[["hyperparameters"]][["hyperparameters"]]
  expect_true(is_wire_candidates(hp[["maxdepth"]]))
  expect_identical(hp[["maxdepth"]][["candidates"]], c(2L, 4L))
  # The user supplied the search, so that is what `origin` reports here; the
  # fold that settled on one value reports `tuned`.
  expect_identical(hp[["origin"]][["maxdepth"]], "user")
})


# %% Meta learners ---------------------------------------------------------------------------------
# The library is deliberately GLM + CART: both are always available, and on the
# fixtures below they are good at visibly different things, which is what makes
# the weights and the regions interpretable rather than arbitrary.
meta_res <- setup_Resampler(n_resamples = 3L, type = "KFold", seed = 2026L)


## {NNLS}[train]<Regression> ----
# NNLS exists as the stacking meta learner, so it is exercised on the shape it
# will see there: non-negative predictors that already span the outcome.
nnls_dat <- data.frame(
  p1 = seq(0, 1, length.out = 100L),
  p2 = rev(seq(0, 1, length.out = 100L))
)
nnls_dat[["y"]] <- 0.75 * nnls_dat[["p1"]] + 0.25 * nnls_dat[["p2"]]
mod_r_nnls <- train(nnls_dat, hyperparameters = setup_NNLS(), verbosity = 0L)

test_that("train() NNLS recovers a non-negative convex combination", {
  expect_s7_class(mod_r_nnls, Regression)
  coefficients <- mod_r_nnls@model@coefficients
  expect_true(all(coefficients >= 0))
  expect_equal(sum(coefficients), 1, tolerance = 1e-8)
  expect_equal(unname(coefficients), c(0.75, 0.25), tolerance = 1e-6)
  # The coefficients are the model, so they are what varimp reports.
  expect_identical(get_varimp(mod_r_nnls)@data[["variable"]], c("p1", "p2"))
})


test_that("NNLS without normalize leaves the coefficients unscaled", {
  unnormalized <- train(
    nnls_dat,
    hyperparameters = setup_NNLS(normalize = FALSE),
    verbosity = 0L
  )
  expect_false(unnormalized@model@normalize)
  expect_true(all(unnormalized@model@coefficients >= 0))
})


test_that("NNLS applies case weights as sqrt(w) on both sides", {
  # Weighted least squares by scaling the system, which is what the
  # SuperLearner literature's method.NNLS does. Checked against `nnls` called
  # directly on the scaled system.
  skip_if_not_installed("nnls")
  set.seed(2026)
  weights <- runif(NROW(nnls_dat), 0.5, 2)
  weighted <- train(
    nnls_dat,
    hyperparameters = setup_NNLS(normalize = FALSE),
    weights = weights,
    verbosity = 0L
  )
  root_w <- sqrt(weights)
  expected <- nnls::nnls(
    as.matrix(nnls_dat[, c("p1", "p2")]) * root_w,
    nnls_dat[["y"]] * root_w
  )
  expect_equal(
    unname(weighted@model@coefficients),
    unname(stats::coef(expected)),
    tolerance = 1e-8
  )
})


test_that("train() NNLS aborts on multiclass and on non-numeric predictors", {
  expect_error(
    train(x = datc3_train, hyperparameters = setup_NNLS(), verbosity = 0L),
    class = "rtemis_unsupported_error"
  )
  # `g` is a factor; NNLS builds a design matrix and has nowhere to put it.
  expect_error(
    train(x = datr_train, hyperparameters = setup_NNLS(), verbosity = 0L),
    class = "rtemis_type_error"
  )
})


## {SuperLearner}[train]<Regression> ----
mod_r_sl <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_SuperLearner(
    base_learners = list(setup_GLM(), setup_CART()),
    inner_resampling_config = meta_res
  ),
  verbosity = 0L
)

test_that("train() SuperLearner Regression succeeds", {
  expect_s7_class(mod_r_sl, Regression)
  expect_s7_class(mod_r_sl@model, StackedLearner)
  expect_identical(names(mod_r_sl@model@base_models), c("GLM", "CART"))
  expect_identical(mod_r_sl@model@meta_model@algorithm, "NNLS")
})


test_that("the level-one matrix is one cross-validated column per entry", {
  level_one <- mod_r_sl@model@level_one_training
  expect_identical(dim(level_one), c(NROW(datr_train), 2L))
  expect_identical(colnames(level_one), c("GLM", "CART"))
  # Every case is predicted by a fit that did not see it, so none is left unset.
  expect_false(anyNA(level_one))
})


test_that("SuperLearner weights sum to 1 and favor the better learner", {
  cv_risk <- mod_r_sl@model@cv_risk
  expect_equal(sum(cv_risk[["weight"]]), 1, tolerance = 1e-8)
  expect_true(all(cv_risk[["weight"]] >= 0))
  # `datr`'s outcome is linear in its features, so GLM should carry the ensemble.
  best <- cv_risk[["learner"]][which.min(cv_risk[["cv_risk"]])]
  expect_identical(best, "GLM")
  expect_gt(
    cv_risk[["weight"]][cv_risk[["learner"]] == "GLM"],
    cv_risk[["weight"]][cv_risk[["learner"]] == "CART"]
  )
})


test_that("SuperLearner matches or beats its worst library entry", {
  # The point of the ensemble: the weights cannot do worse than putting
  # everything on the entry that happens to be worst.
  worst <- max(mod_r_sl@model@cv_risk[["cv_risk"]])
  ensemble <- mean((mod_r_sl@predicted_test - mod_r_sl@y_test)^2)
  expect_lt(ensemble, worst)
})


test_that("SuperLearner varimp names library entries, not features", {
  vi <- get_varimp(mod_r_sl)@data
  expect_identical(vi[["variable"]], c("GLM", "CART"))
  # Column 2 is what `plot_varimp()` shows by default.
  expect_identical(names(vi)[[2L]], "weight")
})


## {SuperLearner}[train]<Classification> Binary ----
mod_c_sl <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_SuperLearner(
    base_learners = list(setup_GLM(), setup_CART()),
    inner_resampling_config = meta_res
  ),
  verbosity = 0L
)

test_that("train() SuperLearner Classification succeeds", {
  expect_s7_class(mod_c_sl, Classification)
  expect_identical(mod_c_sl@model@y_levels, levels(datc2_train[["Species"]]))
})


test_that("SuperLearner predicts the probability of the second level", {
  # A flipped column is still a valid probability, so nothing but this catches
  # it -- the metrics would simply be bad.
  predicted <- predict(mod_c_sl, datc2_test[, -NCOL(datc2_test)])
  outcome <- datc2_test[["Species"]]
  positive <- levels(outcome)[[2L]]
  expect_gt(
    mean(predicted[outcome == positive]),
    mean(predicted[outcome != positive])
  )
  expect_true(all(predicted >= 0 & predicted <= 1))
})


## {SuperLearner}[train]<Regression> Discrete ----
test_that("a discrete SuperLearner keeps one entry and weights it 1", {
  mod <- train(
    x = datr_train,
    hyperparameters = setup_SuperLearner(
      base_learners = list(setup_GLM(), setup_CART()),
      inner_resampling_config = meta_res,
      discrete = TRUE
    ),
    verbosity = 0L
  )
  expect_identical(mod@model@discrete_winner, "GLM")
  # No combination to fit, so no meta model.
  expect_null(mod@model@meta_model)
  expect_identical(sort(mod@model@cv_risk[["weight"]]), c(0, 1))
})


## {SuperLearner}[train]<Regression> Search space expansion ----
test_that("a base learner's search space becomes library entries, untuned", {
  mod <- train(
    x = datr_train,
    hyperparameters = setup_SuperLearner(
      base_learners = list(
        setup_GLM(),
        setup_CART(maxdepth = tune_over(2L, 20L))
      ),
      inner_resampling_config = meta_res
    ),
    verbosity = 0L
  )
  expect_identical(
    names(mod@model@base_models),
    c("GLM", "CART_1", "CART_2")
  )
  # Each entry is trained at one setting, not tuned: the ensemble's own
  # cross-validation is what chooses between them.
  expect_identical(
    mod@model@base_models[["CART_1"]]@hyperparameters$maxdepth,
    2L
  )
  expect_identical(
    mod@model@base_models[["CART_2"]]@hyperparameters$maxdepth,
    20L
  )
  expect_null(mod@model@base_models[["CART_1"]]@tuner)
})


## {SuperLearner}[train]<Regression> Algorithm name dispatch ----
test_that("train() SuperLearner from its algorithm name succeeds", {
  expect_identical(get_alg_name("superlearner"), "SuperLearner")
  expect_s7_class(
    get_default_hyperparameters("superlearner"),
    SuperLearnerHyperparameters
  )
})


## {SuperLearner}[train]<Classification> /\Error multiclass unsupported ----
test_that("train() SuperLearner aborts on multiclass classification", {
  expect_error(
    train(
      x = datc3_train,
      hyperparameters = setup_SuperLearner(
        base_learners = list(setup_GLM(), setup_CART()),
        inner_resampling_config = meta_res
      ),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


## {ModalityStacking}[train]<Regression> ----
mod_r_ms <- train(
  x = datr_train,
  dat_test = datr_test,
  hyperparameters = setup_ModalityStacking(
    feature_groups = list(m1 = c("V1", "V2", "V3"), m2 = c("V4", "V5", "g")),
    base_learners = list(m1 = setup_GLM(), m2 = setup_CART()),
    inner_resampling_config = meta_res
  ),
  verbosity = 0L
)

test_that("train() ModalityStacking gives each entry only its own features", {
  expect_s7_class(mod_r_ms, Regression)
  expect_identical(
    mod_r_ms@model@base_models[["m1"]]@xnames,
    c("V1", "V2", "V3")
  )
  expect_identical(
    mod_r_ms@model@base_models[["m2"]]@xnames,
    c("V4", "V5", "g")
  )
  # The whole feature set still reaches the stacked model, via the two groups.
  expect_identical(mod_r_ms@xnames, names(datr_train)[-NCOL(datr_train)])
})


test_that("ModalityStacking predicts on new data", {
  predicted <- predict(mod_r_ms, datr_test[, -NCOL(datr_test), with = FALSE])
  expect_length(predicted, NROW(datr_test))
  expect_false(anyNA(predicted))
})


## {ModalityStacking}[train]<Regression> Single-feature group ----
test_that("ModalityStacking accepts a group holding a single feature", {
  mod <- train(
    x = datr_train,
    hyperparameters = setup_ModalityStacking(
      feature_groups = list(m1 = "V1", m2 = c("V4", "V5")),
      base_learners = list(m1 = setup_GLM(), m2 = setup_CART()),
      inner_resampling_config = meta_res
    ),
    verbosity = 0L
  )
  expect_s7_class(mod, Regression)
  expect_identical(mod@model@base_models[["m1"]]@xnames, "V1")
  predicted <- predict(mod, datr_test[, -NCOL(datr_test), with = FALSE])
  expect_length(predicted, NROW(datr_test))
  expect_false(anyNA(predicted))
})


## {MetaLearners}[train]<Regression> Tabular class independence ----
# The meta learners index their training data by row and column throughout, and
# each tabular class reads `[` differently. The rest of this file trains them on
# a data.table, so this pins the other two classes to the same predictions.
test_that("meta learners give identical predictions for every tabular class", {
  skip_if_not_installed("tibble")
  newdata <- datr_test[, -NCOL(datr_test), with = FALSE]
  as_class <- list(
    data.table = function(d) data.table::as.data.table(d),
    data.frame = as.data.frame,
    tibble = function(d) tibble::as_tibble(d)
  )
  specs <- list(
    SuperLearner = function() {
      setup_SuperLearner(
        base_learners = list(setup_GLM(), setup_CART()),
        inner_resampling_config = meta_res
      )
    },
    ModalityStacking = function() {
      setup_ModalityStacking(
        feature_groups = list(
          m1 = c("V1", "V2", "V3"),
          m2 = c("V4", "V5", "g")
        ),
        base_learners = list(m1 = setup_GLM(), m2 = setup_CART()),
        inner_resampling_config = meta_res
      )
    },
    ConditionalSuperLearner = function() {
      setup_ConditionalSuperLearner(
        base_learners = list(setup_GLM(), setup_CART(maxdepth = 1L)),
        inner_resampling_config = meta_res,
        n_iterations = 2L
      )
    }
  )
  for (spec_name in names(specs)) {
    predicted <- lapply(as_class, function(convert) {
      mod <- train(
        x = convert(datr_train),
        hyperparameters = specs[[spec_name]](),
        verbosity = 0L
      )
      as.numeric(predict(mod, convert(newdata)))
    })
    expect_equal(
      predicted[["data.frame"]],
      predicted[["data.table"]],
      info = spec_name
    )
    expect_equal(
      predicted[["tibble"]],
      predicted[["data.table"]],
      info = spec_name
    )
  }
})


## {ConditionalSuperLearner}[train]<Regression> ----
# Two regions, and an expert that can serve one and not the other: a line in V1
# where V5 is negative, a clean step in V2 where it is not. The library is a GLM
# (a line, so hopeless on the step) and a depth-1 tree (one split, so hopeless on
# the line). Each is structurally the best available model in exactly one region,
# which is what makes recovering the partition a real test.
#
# A *default* CART would fit both regions on its own, leaving no region structure
# in the per-case losses for the oracle to find; the stump is what avoids that.
set.seed(2026)
n_csl <- 300L
datcsl <- data.frame(
  V1 = rnorm(n_csl),
  V2 = rnorm(n_csl),
  V5 = rnorm(n_csl)
)
csl_region <- ifelse(datcsl[["V5"]] < 0, "linear", "step")
datcsl[["y"]] <- ifelse(
  csl_region == "linear",
  4 * datcsl[["V1"]],
  6 * (datcsl[["V2"]] > 0)
) +
  rnorm(n_csl, sd = 0.3)
csl_experts <- list(GLM = setup_GLM(), Stump = setup_CART(maxdepth = 1L))

mod_r_csl <- train(
  x = datcsl,
  hyperparameters = setup_ConditionalSuperLearner(
    base_learners = csl_experts,
    meta_learner = setup_CART(),
    n_iterations = 5L,
    inner_resampling_config = meta_res
  ),
  verbosity = 0L
)

test_that("train() ConditionalSuperLearner Regression succeeds", {
  expect_s7_class(mod_r_csl, Regression)
  expect_s7_class(mod_r_csl@model, ConditionalSuperLearner)
  expect_identical(names(mod_r_csl@model@experts), c("GLM", "Stump"))
  expect_identical(mod_r_csl@model@oracle@algorithm, "CART")
})


test_that("the Conditional SuperLearner recovers the planted regions", {
  # The oracle's job is to find where each expert wins; here that is exactly the
  # sign of V5.
  agreement <- mean(
    (mod_r_csl@model@assignments == "GLM") == (csl_region == "linear")
  )
  expect_gt(agreement, 0.9)
})


test_that("the Conditional SuperLearner beats every single expert", {
  # If routing bought nothing, the best single expert would match it.
  conditional <- get_metric(mod_r_csl, "training", "rsq")
  for (hyperparameters in csl_experts) {
    alone <- train(datcsl, hyperparameters = hyperparameters, verbosity = 0L)
    expect_gt(conditional, get_metric(alone, "training", "rsq"))
  }
})


test_that("the Conditional SuperLearner records its iterations", {
  expect_length(mod_r_csl@model@iteration_loss, 5L)
  # Not asserted to be monotone: the paper's decrease guarantee is for training
  # losses, and these are cross-validated (section 2.4).
  expect_lt(
    utils::tail(mod_r_csl@model@iteration_loss, 1L),
    mod_r_csl@model@iteration_loss[[1L]]
  )
  expect_identical(dim(mod_r_csl@model@region_sizes), c(5L, 2L))
  # Every case is routed somewhere at every iteration.
  expect_equal(
    rowSums(mod_r_csl@model@region_sizes),
    rep(as.numeric(n_csl), 5L)
  )
  # Cases x experts, from fits that did not see them.
  expect_identical(dim(mod_r_csl@model@cv_loss), c(n_csl, 2L))
})


test_that("the Conditional SuperLearner reports the oracle's varimp", {
  # Which covariates decide *which model applies*: V5, by construction.
  vi <- get_varimp(mod_r_csl)@data
  expect_identical(vi[["variable"]][[which.max(vi[[2L]])]], "V5")
})


test_that("the extended weight transform is [ONE_K - DIAG_K]^-1, clamped", {
  set.seed(2026)
  for (n_experts in 2:4) {
    ones_minus_diag <- matrix(1, n_experts, n_experts)
    diag(ones_minus_diag) <- 0
    case_loss <- matrix(runif(5L * n_experts), 5L, n_experts)
    reference <- t(solve(ones_minus_diag) %*% t(case_loss))
    weights <- csl_extended_weights(case_loss)
    expect_equal(
      unname(weights),
      unname(pmax(reference, 0)),
      tolerance = 1e-12
    )
    # The clamp cannot silence the expert that matters: the best one for a case
    # always keeps a positive weight.
    best <- max.col(-case_loss)
    expect_true(all(weights[cbind(seq_len(5L), best)] > 0))
  }
  # K = 2 is a swap and never goes negative.
  expect_identical(
    unname(csl_extended_weights(matrix(c(1, 2, 3, 4), 2, 2))),
    matrix(c(3, 4, 1, 2), 2, 2)
  )
})


test_that("the extended dataset stacks each case once per expert", {
  feat <- data.frame(a = 1:3, b = 4:6)
  extended <- csl_extended_data(feat, c("one", "two"), "expert")
  expect_identical(NROW(extended), 6L)
  expect_identical(names(extended), c("a", "b", "expert"))
  # Blocked by expert, matching `as.vector()` of a cases x experts weight matrix.
  expect_identical(
    as.character(extended[["expert"]]),
    rep(c("one", "two"), each = 3L)
  )
  expect_identical(extended[["a"]], rep(1:3, times = 2L))
})


## {ConditionalSuperLearner}[train]<Classification> /\Error multiclass ----
test_that("train() ConditionalSuperLearner aborts on multiclass", {
  expect_error(
    train(
      x = datc3_train,
      hyperparameters = setup_ConditionalSuperLearner(
        base_learners = list(setup_GLM(), setup_CART()),
        inner_resampling_config = meta_res
      ),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


## Meta learner records ----
test_that("a meta learner's record carries one block per library entry", {
  # `base_learners` is published as an array of `$ref`s, so each element is a
  # record in its own right -- with its own `origin`, which the generated
  # `record.json` requires of every referenced block.
  #
  # `record()` rather than `outdir`: writing a record validates it against the
  # *published* schemas, which will not know these algorithms until they are
  # published (plan/superlearner.md, step 9).
  payload <- record(mod_r_sl)[["hyperparameters"]][["hyperparameters"]]
  # Every property the leaf schema declares, inherited ones included: these come
  # from three different levels of the class hierarchy.
  expect_true(all(
    c(
      "base_learners",
      "meta_learner",
      "inner_resampling_config",
      "expand_search_spaces",
      "ifw",
      "discrete",
      "origin"
    ) %in%
      names(payload)
  ))
  entries <- payload[["base_learners"]]
  expect_identical(names(entries), c("GLM", "CART"))
  for (entry in entries) {
    expect_identical(names(entry), c("algorithm", "hyperparameters"))
    expect_true("origin" %in% names(entry[["hyperparameters"]]))
  }
  expect_identical(payload[["meta_learner"]][["algorithm"]], "NNLS")
})
