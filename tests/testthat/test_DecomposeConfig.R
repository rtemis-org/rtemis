# test_DecomposeConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% setup_DecomposeConfig() ----
test_that("setup_DecomposeConfig() succeeds", {
  dc <- setup_DecomposeConfig(
    dat_path = "data.csv",
    decomposition_config = setup_PCA(k = 3L),
    outdir = "results/",
    verbosity = 1L
  )
  expect_s7_class(dc, DecomposeConfig)
})


# %% decomp DecomposeConfig ----
test_that("decomp() works with DecomposeConfig", {
  testthat::skip("For local testing only; requires CSV file")
  dc <- setup_DecomposeConfig(
    dat_path = "~/Data/iris_numeric.csv",
    decomposition_config = setup_PCA(k = 3L),
    outdir = "decomp_out/",
    verbosity = 1L
  )
  decom <- decomp(dc)
  expect_s7_class(decom, Decomposition)
})


# %% write_config.DecomposeConfig & read_config ----
test_that("DecomposeConfig round-trips through write_config/read_config JSON", {
  x <- setup_DecomposeConfig(
    dat_path = "data.csv",
    decomposition_config = setup_PCA(k = 3L),
    outdir = "results/"
  )
  file <- file.path(tempdir(), "rtemis_decompose.json")
  write_config(x, file, overwrite = TRUE)
  expect_true(file.exists(file))
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/decompose/r/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, DecomposeConfig)
  expect_s7_class(xtoo@decomposition_config, DecompositionConfig)
  expect_identical(
    xtoo@decomposition_config@algorithm,
    x@decomposition_config@algorithm
  )
})


# %% one place for the algorithm ----
test_that("a decompose document names its algorithm only inside decomposition_config", {
  expect_false("algorithm" %in% names(DecomposeConfig@properties))
  expect_error(
    .list_to_DecomposeConfig(list(
      algorithm = "PCA",
      decomposition_config = list(algorithm = "PCA", k = 2L)
    )),
    class = "rtemis_input_error"
  )
  x <- .list_to_DecomposeConfig(list(
    decomposition_config = list(algorithm = "PCA", k = 2L)
  ))
  expect_identical(x@decomposition_config@algorithm, "PCA")
})


# %% algorithms without an out-of-sample map ----
test_that("a decompose document round-trips for an algorithm that cannot be applied to new data", {
  # Standalone decomposition fits and embeds in one call, so `can_apply` does
  # not restrict it; only the supervised pipeline needs to apply a fit.
  for (config in list(setup_tSNE(k = 2L), setup_Isomap(k = 2L))) {
    x <- setup_DecomposeConfig(decomposition_config = config)
    file <- withr::local_tempfile(fileext = ".json")
    write_config(x, file, verbosity = 0L)
    xtoo <- read_config(file)
    expect_s7_class(xtoo, DecomposeConfig)
    expect_identical(
      xtoo@decomposition_config@algorithm,
      config@algorithm,
      info = config@algorithm
    )
  }
})


# %% execution config ----
test_that("a decompose document carries its execution config through write and read", {
  x <- setup_DecomposeConfig(
    decomposition_config = setup_PCA(k = 2L),
    execution_config = setup_SerialExecution(
      n_workers_algorithm = 2L,
      seed = 5L
    )
  )
  file <- withr::local_tempfile(fileext = ".json")
  write_config(x, file, verbosity = 0L)
  xtoo <- read_config(file)
  expect_s7_class(xtoo@execution_config, SerialExecutionConfig)
  expect_identical(xtoo@execution_config@n_workers_algorithm, 2L)
  expect_identical(xtoo@execution_config@seed, 5L)
})

test_that("a decompose document without an execution config reads with the default", {
  x <- .list_to_DecomposeConfig(list(
    decomposition_config = list(algorithm = "PCA", k = 2L)
  ))
  expect_s7_class(x@execution_config, ExecutionConfig)
  expect_identical(x@execution_config@backend, "future")
})
