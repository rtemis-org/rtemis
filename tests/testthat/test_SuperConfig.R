# test_SupervisedConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% SuperConfigPaths ----
test_that("SuperConfigPaths() succeeds", {
  sc <- SuperConfigPaths(
    dat_training_path = "train.csv",
    dat_validation_path = "validation.csv",
    dat_test_path = "test.csv",
    weights = NULL,
    preprocessor_config = setup_SupervisedPreprocessor(),
    hyperparameters = setup_GLMNET(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_KFold(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we predict the future from the past?",
    outdir = "results/",
    verbosity = 1L
  )
  expect_s7_class(sc, SuperConfigPaths)
})

# %% setup_SuperConfig() ----
test_that("setup_SuperConfig() succeeds", {
  sc <- setup_SuperConfig(
    dat_training_path = "train.csv",
    dat_validation_path = "validation.csv",
    dat_test_path = "test.csv",
    weights = NULL,
    preprocessor_config = setup_SupervisedPreprocessor(),
    hyperparameters = setup_LightGBM(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_KFold(),
    execution_config = setup_ExecutionConfig(),
    question = "Can we predict the future from the past?",
    outdir = "models/",
    verbosity = 1L
  )
  expect_s7_class(sc, SuperConfigPaths)
})

test_that("setup_SuperConfig() takes a NULL outdir", {
  # "Write nothing to disk" is a state a config must be able to express -- it is
  # what every live run does -- and the property is nullable accordingly. The
  # function default stays "results/", so a recipe that omits the field still
  # writes where it always did.
  expect_null(setup_SuperConfig(outdir = NULL)@outdir)
  expect_identical(setup_SuperConfig()@outdir, "results/")
})


test_that("a config stores the paths it was given, unresolved", {
  # A config is a portable recipe: the path it carries must read the same on the
  # machine that authored it and the one that runs it. `normalizePath()` resolves
  # a relative path against the working directory -- on Windows whether or not it
  # exists, on POSIX only when it does -- and expands `~`, so storing its result
  # would make the document depend on where R was started and on which platform.
  withr::local_dir(withr::local_tempdir())
  dir.create("results")
  expect_identical(setup_SuperConfig(outdir = "results/")@outdir, "results/")
  expect_identical(
    setup_SuperConfig(dat_training_path = "~/Data/iris.csv")@dat_training_path,
    "~/Data/iris.csv"
  )
  expect_identical(
    setup_SuperConfigLive(dat_training = iris, outdir = "results/")@outdir,
    "results/"
  )
  expect_identical(
    setup_ClusterConfig(dat_path = "data.csv")@dat_path,
    "data.csv"
  )
  expect_identical(
    setup_DecomposeConfig(outdir = "results/")@outdir,
    "results/"
  )
  # What the function returns changed; what it refuses did not.
  expect_error(
    setup_SuperConfig(outdir = "| rm -rf ."),
    class = "rtemis_value_error"
  )
  expect_error(
    setup_SuperConfig(dat_training_path = "http://example.com/iris.csv"),
    class = "rtemis_value_error"
  )
})


test_that("a NULL outdir round-trips through JSON as an explicit null", {
  # `outdir` is the one property that is nullable *and* carries a non-NULL
  # `setup_SuperConfig` default, so omitting it and writing `null` must stay
  # distinguishable on the wire: omitted keeps "results/", `null` means write
  # nothing. The schema declares it `["string", "null"]` and not required.
  x <- setup_SuperConfig(hyperparameters = setup_LightRF(), outdir = NULL)
  file <- file.path(tempdir(), "rtemis_super_null_outdir.json")
  write_config(x, file, overwrite = TRUE)
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_true("outdir" %in% names(xl))
  expect_null(xl[["outdir"]])
  expect_null(read_config(file)@outdir)
})


test_that("an omitted outdir reads back as the setup_SuperConfig default", {
  x <- setup_SuperConfig(hyperparameters = setup_LightRF(), outdir = "models/")
  file <- file.path(tempdir(), "rtemis_super_no_outdir.json")
  write_config(x, file, overwrite = TRUE)
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  xl[["outdir"]] <- NULL
  writeLines(
    as.character(jsonlite::toJSON(xl, auto_unbox = TRUE, pretty = TRUE)),
    file
  )
  expect_identical(read_config(file)@outdir, "results/")
})


# %% train SuperConfig ----
test_that("train() works with SuperConfig", {
  testthat::skip("For local testing only; requires CSV file")
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    dat_validation_path = NULL,
    dat_test_path = NULL,
    weights = NULL,
    preprocessor_config = setup_SupervisedPreprocessor(scale = TRUE),
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_KFold(),
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
    preprocessor_config = setup_SupervisedPreprocessor(scale = TRUE),
    hyperparameters = setup_LightRF(),
    tuner_config = setup_GridSearch(),
    outer_resampling_config = setup_KFold(),
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
  expect_s7_class(xtoo, SuperConfigPaths)
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
        `$schema` = "https://schema.rtemis.org/supervisedpreprocessor/v1/schema.json",
        scale = TRUE
      ),
      decomposition_config = list(
        `$schema` = "https://schema.rtemis.org/decomposition/v1/schema.json",
        algorithm = "PCA",
        k = 2L
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
  expect_s7_class(x, SuperConfigPaths)
  expect_true(x@preprocessor_config@scale)
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
    list(dat_training_path = "~/Data/iris.csv"),
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


# %% Tuned hyperparameters round-trip ----
test_that("a search space survives write_config/read_config", {
  x <- setup_SuperConfig(
    dat_training_path = "~/Data/iris.csv",
    hyperparameters = setup_LightRF(
      max_depth = tune_over(3L, 4L, 5L),
      num_leaves = 32L
    ),
    tuner_config = setup_GridSearch()
  )
  file <- file.path(tempdir(), "rtemis_super_tuned.json")
  write_config(x, file, overwrite = TRUE)

  # The wire form is tagged, so a reader tells a search space from a value
  # without consulting the schema.
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  hp <- xl[["hyperparameters"]]
  expect_identical(names(hp[["max_depth"]]), "candidates")
  expect_identical(unlist(hp[["max_depth"]][["candidates"]]), c(3L, 4L, 5L))
  # A value is written plainly, with no tag to strip.
  expect_identical(hp[["num_leaves"]], 32L)

  # And it comes back as the domain it went out as.
  xtoo <- read_config(file)
  restored <- xtoo@hyperparameters@max_depth
  expect_s7_class(restored, HyperparameterCandidates)
  expect_identical(restored@candidates, list(3L, 4L, 5L))
  expect_identical(xtoo@hyperparameters@num_leaves, 32L)
  # The reconstructed config still knows it needs tuning.
  expect_true(needs_tuning(xtoo@hyperparameters))
})


# %% wire round-trip ----

test_that("every SuperConfig property survives the wire converter", {
  # `.list_to_SuperConfig()` maps wire keys to `setup_SuperConfig()` arguments
  # by hand, one line per property. A property added to the class and not to
  # that list is dropped silently: the document carries it, the reconstructed
  # config does not, and every check downstream reasons about a config the user
  # did not write.
  #
  # `outcome` and `features` were added and not mapped, so a plan naming its
  # predictors was validated against every column in the table -- reported as a
  # finding about a column the plan had excluded. `check_wire_keys()` did not
  # catch it: it rejects keys the class does not have, and this is the mirror
  # case, a property the converter does not read.
  #
  # Derived from the class rather than listed here, so the next property is
  # covered without this test being edited.
  wire_read <- names(formals(rtemis:::.list_to_SuperConfig))
  properties <- names(SuperConfigPaths@properties)
  body_text <- paste(
    deparse(body(rtemis:::.list_to_SuperConfig)),
    collapse = "\n"
  )
  unmapped <- Filter(
    function(prop) !grepl(paste0('x\\[\\["', prop, '"\\]\\]'), body_text),
    properties
  )
  expect_identical(
    unmapped,
    character(0),
    info = paste(
      "properties the wire converter never reads:",
      paste(unmapped, collapse = ", ")
    )
  )
  expect_length(wire_read, 1L)
})


test_that("a config's outcome and features round-trip through JSON", {
  config <- list(
    `$schema` = "https://schema.rtemis.org/supervised/v1/schema.json",
    hyperparameters = list(algorithm = "GLM"),
    outcome = "y",
    features = c("a", "b")
  )
  restored <- rtemis:::.config_from_list(config)
  expect_identical(restored@outcome, "y")
  expect_identical(restored@features, c("a", "b"))
})
