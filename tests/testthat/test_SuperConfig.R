# test_SupervisedConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% SuperConfig ----
test_that("SuperConfig() succeeds", {
  sc <- SuperConfig(
    dat_training_path = "train.csv",
    dat_validation_path = "validation.csv",
    dat_test_path = "test.csv",
    weights = NULL,
    algorithm = "GLMNET",
    preprocessor_config = setup_Preprocessor(),
    hyperparameters = setup_GLMNET(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we predict the future from the past?",
    outdir = "results/",
    verbosity = 1L
  )
  expect_s7_class(sc, SuperConfig)
})

# %% setup_SuperConfig() ----
test_that("setup_SuperConfig() succeeds", {
  sc <- setup_SuperConfig(
    dat_training_path = "train.csv",
    dat_validation_path = "validation.csv",
    dat_test_path = "test.csv",
    weights = NULL,
    preprocessor_config = setup_Preprocessor(),
    algorithm = "LightGBM",
    hyperparameters = setup_LightGBM(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we predict the future from the past?",
    outdir = "models/",
    verbosity = 1L
  )
  expect_s7_class(sc, SuperConfig)
})


# %% train SuperConfig ----
test_that("train() works with SuperConfig", {
  testthat::skip("For local testing only; requires CSV file")
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    dat_validation_path = NULL,
    dat_test_path = NULL,
    weights = NULL,
    preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
    algorithm = "LightRF",
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we tell iris species apart given their measurements?",
    outdir = "models/",
    verbosity = 1L
  )
  mod <- train(x)
  expect_s7_class(mod, SupervisedRes)
})


# %% Test converting SuperConfig to TOML ----
test_that("SuperConfig can be written to TOML", {
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    dat_validation_path = NULL,
    dat_test_path = NULL,
    weights = NULL,
    preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
    algorithm = "LightRF",
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we tell iris species apart given their measurements?",
    outdir = "models/",
    verbosity = 1L
  )
  toml_str <- to_toml(x)
  expect_type(toml_str, "character")
})


# %% write SuperConfig to TOML file ----
test_that("SuperConfig can be written to TOML", {
  skip("For local testing only; requires writing to file")
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    dat_validation_path = NULL,
    dat_test_path = NULL,
    weights = NULL,
    preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
    algorithm = "LightRF",
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we tell iris species apart given their measurements?",
    outdir = "models/",
    verbosity = 1L
  )
  write_toml(x, "~/dev/rtemis.toml")
  testthat::expect_true(file.exists("~/dev/rtemis.toml"))
})

x <- read_config("~/dev/rtemis.toml")
