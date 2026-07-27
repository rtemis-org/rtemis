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


# %% write_config.SuperConfig & read_config ----
test_that("SuperConfig round-trips through write_config/read_config JSON", {
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    dat_validation_path = NULL,
    dat_test_path = NULL,
    weights = NULL,
    preprocessor_config = setup_Preprocessor(remove_duplicates = TRUE),
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_Resampler(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we tell iris species apart given their measurements?",
    outdir = "models/",
    verbosity = 1L
  )
  file <- file.path(tempdir(), "rtemis_super.json")
  write_config(x, file, overwrite = TRUE)
  expect_true(file.exists(file))
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/supervised/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, SuperConfig)
  expect_identical(
    xtoo@hyperparameters@algorithm,
    x@hyperparameters@algorithm
  )
})


# %% read_config ignores nested $schema ----
test_that("read_config ignores `$schema` on nested configs", {
  # Every family schema permits a `$schema`, so a nested config lifted verbatim
  # out of its own standalone config file carries one. It is document metadata,
  # not a `setup_*` argument, and must never reach the setup functions.
  file <- file.path(tempdir(), "rtemis_nested_schema.json")
  jsonlite::write_json(
    list(
      `$schema` = "https://schema.rtemis.org/supervised/v1/schema.json",
      dat_training_path = "~/Data/iris.csv",
      preprocessor_config = list(
        `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
        remove_duplicates = TRUE
      ),
      decomposition_config = list(
        `$schema` = "https://schema.rtemis.org/decomposition/v1/schema.json",
        algorithm = "PCA",
        config = list(k = 2L)
      ),
      execution_config = list(
        `$schema` = "https://schema.rtemis.org/execution/v1/schema.json",
        n_workers = 1L
      )
    ),
    file,
    auto_unbox = TRUE
  )
  x <- read_config(file)
  expect_s7_class(x, SuperConfig)
  expect_true(x@preprocessor_config@remove_duplicates)
  expect_s7_class(x@decomposition_config, DecompositionConfig)
  expect_identical(x@execution_config@n_workers, 1L)
})


# %% write_config.DecompositionConfig & read_config ----
test_that("DecompositionConfig round-trips through write_config/read_config", {
  x <- setup_PCA(k = 3L)
  file <- file.path(tempdir(), "rtemis_decom.json")
  write_config(x, file, overwrite = TRUE)
  expect_true(file.exists(file))
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/decomposition/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, DecompositionConfig)
  expect_identical(xtoo@algorithm, x@algorithm)
})


# %% write_config.ClusteringConfig & read_config ----
test_that("ClusteringConfig round-trips through write_config/read_config", {
  x <- setup_DBSCAN(eps = 0.5, min_points = 5L)
  file <- file.path(tempdir(), "rtemis_clust.json")
  write_config(x, file, overwrite = TRUE)
  expect_true(file.exists(file))
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/clustering/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, ClusteringConfig)
  expect_identical(xtoo@algorithm, x@algorithm)
})


# %% read_config rejects missing $schema ----
test_that("read_config errors when $schema is missing", {
  file <- file.path(tempdir(), "rtemis_noschema.json")
  jsonlite::write_json(
    list(algorithm = "LightRF"),
    file,
    auto_unbox = TRUE
  )
  expect_error(read_config(file), class = "rtemis_value_error")
})


# %% read_config rejects unsupported $schema ----
test_that("read_config errors on an unrecognized $schema", {
  file <- file.path(tempdir(), "rtemis_badschema.json")
  jsonlite::write_json(
    list(`$schema` = "https://schema.rtemis.org/bogus/v1/schema.json"),
    file,
    auto_unbox = TRUE
  )
  expect_error(read_config(file), class = "rtemis_value_error")
})
