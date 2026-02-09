# test_SupervisedConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% SuperConfig ----
test_that("SuperConfig() succeeds", {
  sc <- SuperConfig(
    dat_training = "train.csv",
    dat_validation = "validation.csv",
    dat_test = "test.csv",
    algorithm = "GLMNET",
    preprocessor_config = setup_Preprocessor(),
    hyperparameters = setup_GLMNET(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    weights = NULL,
    question = "What is the best model?",
    outdir = "results/",
    verbosity = 1L
  )
  expect_s7_class(sc, SuperConfig)
})
