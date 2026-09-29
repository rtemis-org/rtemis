# test_Autoencoder.R
# ::rtemis::
# 2026- EDG rtemis.org

# Data ----
x <- iris[, -5]
# Few epochs: these tests check the pipeline's contracts, not the fit's quality.
quick <- function(...) {
  setup_Autoencoder(max_epochs = 5L, ...)
}
seeded <- function(seed = 2026L) {
  setup_SerialExecution(seed = seed)
}


# Setup ----
test_that("setup_Autoencoder() builds a config with the shared torch settings", {
  config <- setup_Autoencoder()
  expect_s7_class(config, AutoencoderConfig)
  expect_s7_class(config, AutoencoderBaseConfig)
  expect_identical(config@algorithm, "Autoencoder")
  expect_identical(config[["k"]], 2L)
  expect_null(config[["hidden_units"]])
  expect_null(config[["batch_size"]])
  expect_identical(config[["validation_fraction"]], 0.1)
  # The factory declarations MLP also splices.
  for (nm in c("activation", "norm", "dropout", "optimizer", "max_grad_norm")) {
    expect_identical(
      get_spec_fields(AutoencoderConfig@properties[[nm]])[["description"]],
      get_spec_fields(MLPHyperparameters@properties[[nm]])[["description"]],
      info = nm
    )
  }
  expect_error(AutoencoderBaseConfig())
})


# Fit ----
test_that("decomp() fits an autoencoder and records what it resolved", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_s7_class(decom, Decomposition)
  expect_s7_class(decom@decom, AutoencoderFit)
  expect_identical(dim(decom@transformed), c(nrow(x), 2L))
  expect_identical(
    colnames(decom@transformed),
    c("Autoencoder_1", "Autoencoder_2")
  )
  # Derived from the data and written back, so the record states them.
  expect_identical(decom@config[["hidden_units"]], 32L)
  expect_identical(decom@config[["batch_size"]], 16L)
  origin <- record(decom)[["decomposition_config"]][["origin"]]
  expect_identical(origin[["hidden_units"]], "derived")
  expect_identical(origin[["batch_size"]], "derived")
  expect_identical(origin[["features"]], "derived")
  expect_identical(origin[["max_epochs"]], "user")
  # The input half of the run keeps what was asked for.
  expect_null(decom@decompose_config@decomposition_config[["hidden_units"]])
})


test_that("the traits give the autoencoder the reconstruction metrics", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  for (metric in c(
    "explained_variance_ratio",
    "relative_reconstruction_error",
    "reconstruction_rmse",
    "max_abs_component_correlation",
    "effective_dimensionality"
  )) {
    expect_false(is.na(decom@metrics[[metric]]), info = metric)
  }
})


test_that("given hidden widths and batch size are used as given", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(hidden_units = c(8L, 4L), batch_size = 50L),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_identical(decom@decom@hidden_units, c(8L, 4L))
  expect_identical(decom@config[["batch_size"]], 50L)
  expect_identical(
    record(decom)[["decomposition_config"]][["origin"]][["hidden_units"]],
    "user"
  )
})


test_that("the derived architecture follows its documented bounds", {
  expect_identical(autoencoder_hidden_units(NULL, 4L, 2L, verbosity = 0L), 32L)
  expect_identical(
    autoencoder_hidden_units(NULL, 10000L, 10L, verbosity = 0L),
    316L
  )
  expect_identical(
    autoencoder_hidden_units(NULL, 100000L, 10L, verbosity = 0L),
    512L
  )
  expect_identical(autoencoder_hidden_units(NULL, 5L, 40L, verbosity = 0L), 40L)
  expect_identical(autoencoder_batch_size(NULL, 135L), 16L)
  expect_identical(autoencoder_batch_size(NULL, 1000L), 100L)
  expect_identical(autoencoder_batch_size(NULL, 100000L), 256L)
})


# Fit and apply are one map ----
test_that("apply_decomp() on training data reproduces the fitted components", {
  skip_if_no_torch()
  # With denoising on, too: the corruption is training-only.
  for (config in list(
    quick(),
    quick(input_noise = 0.5, input_dropout = 0.2, dropout = 0.1),
    quick(norm = "batch_norm")
  )) {
    decom <- decomp(
      x,
      config = config,
      execution_config = seeded(),
      verbosity = 0L
    )
    expect_identical(
      as.matrix(apply_decomp(decom, x, verbosity = 0L)),
      decom@transformed,
      ignore_attr = TRUE
    )
  }
})


test_that("apply_decomp() refuses missing features and missing values by name", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  holey <- x
  holey[3L, "Petal.Width"] <- NA
  expect_error(
    apply_decomp(decom, holey, verbosity = 0L),
    "Petal.Width",
    class = "rtemis_value_error"
  )
  expect_error(
    apply_decomp_(
      decom@config,
      decom@decom,
      x[, 1:3],
      verbosity = 0L
    ),
    "Petal.Width",
    class = "rtemis_value_error"
  )
})


# Reconstruction ----
test_that("reconstruction is in input units", {
  skip_if_no_torch()
  # Standardization makes the fit invariant to a positive affine map of each
  # column, so a fit on rescaled data must reconstruct the rescaled
  # reconstruction. A reconstruction left in standardized units fails this by
  # orders of magnitude.
  a <- c(1000, 0.01, 5, 1)
  b <- c(-50, 3, 0, 1e4)
  rescaled <- as.data.frame(sweep(sweep(as.matrix(x), 2L, a, "*"), 2L, b, "+"))
  fit <- function(data) {
    decom <- decomp(
      data,
      config = quick(validation_fraction = 0),
      execution_config = seeded(),
      verbosity = 0L
    )
    as.matrix(reconstruct(decom, data, verbosity = 0L))
  }
  expected <- sweep(sweep(fit(x), 2L, a, "*"), 2L, b, "+")
  expect_equal(fit(rescaled), expected, tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("reconstruct() agrees with the metrics' own reconstruction", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  reconstructed <- as.matrix(reconstruct(decom, x, verbosity = 0L))
  relative <- norm(as.matrix(x) - reconstructed, "F") / norm(as.matrix(x), "F")
  expect_equal(relative, decom@metrics[["relative_reconstruction_error"]])
})


test_that("a trained autoencoder reconstructs iris about as well as PCA", {
  skip_if_no_torch()
  skip_on_cran()
  decom <- decomp(
    x,
    config = setup_Autoencoder(),
    execution_config = seeded(),
    verbosity = 0L
  )
  pca <- decomp(x, config = setup_PCA(k = 2L), verbosity = 0L)
  expect_gt(
    decom@metrics[["explained_variance_ratio"]],
    pca@metrics[["explained_variance_ratio"]] - 0.05
  )
})


test_that("a constant feature is centered, not scaled", {
  skip_if_no_torch()
  flat <- cbind(x, constant = 7)
  decom <- decomp(
    flat,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_identical(decom@decom@scale[[5L]], 1)
  expect_false(anyNA(decom@transformed))
})


# Reproducibility ----
test_that("the execution config's seed reproduces the fit, noise and holdout included", {
  skip_if_no_torch()
  fit <- function(seed) {
    decomp(
      x,
      config = quick(input_noise = 0.3, input_dropout = 0.1),
      execution_config = seeded(seed),
      verbosity = 0L
    )@transformed
  }
  expect_identical(fit(1L), fit(1L))
  expect_false(isTRUE(all.equal(fit(1L), fit(2L))))
  set.seed(9)
  fit(1L)
  after_fit <- stats::runif(1L)
  set.seed(9)
  expect_identical(after_fit, stats::runif(1L))
})


test_that("denoising corrupts training: the same seed fits differently with noise", {
  skip_if_no_torch()
  clean <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  noisy <- decomp(
    x,
    config = quick(input_noise = 0.5),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_false(isTRUE(all.equal(clean@transformed, noisy@transformed)))
})


test_that("a fitted autoencoder survives saveRDS() and applies after readRDS()", {
  skip_if_no_torch()
  decom <- decomp(
    x,
    config = quick(),
    execution_config = seeded(),
    verbosity = 0L
  )
  file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(decom, file)
  reloaded <- readRDS(file)
  expect_type(reloaded@decom@state, "raw")
  expect_identical(
    as.matrix(apply_decomp(reloaded, x, verbosity = 0L)),
    decom@transformed,
    ignore_attr = TRUE
  )
})


# Early stopping ----
test_that("validation_fraction holds out cases for early stopping", {
  skip_if_no_torch()
  held <- decomp(
    x,
    config = quick(validation_fraction = 0.2),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_identical(held@decom@n_validation, 30L)
  expect_false(anyNA(held@decom@history[["loss_validation"]]))
  none <- decomp(
    x,
    config = quick(validation_fraction = 0),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_identical(none@decom@n_validation, 0L)
  expect_identical(none@decom@epochs_trained, 5L)
  expect_true(all(is.na(none@decom@history[["loss_validation"]])))
})


test_that("too few cases to hold out trains without early stopping, and says so", {
  expect_identical(autoencoder_holdout(3L, 0.1, verbosity = 0L), integer())
  expect_message(
    autoencoder_holdout(3L, 0.1, verbosity = 1L),
    "without early stopping"
  )
  expect_identical(autoencoder_holdout(3L, 0, verbosity = 1L), integer())
  expect_length(autoencoder_holdout(100L, 0.1, verbosity = 0L), 10L)
})


# Resources ----
test_that("the fit uses the execution config's threads and apply the applier's", {
  skip_if_no_torch()
  requested <- integer()
  local_mocked_bindings(
    set_torch_threads = function(n_threads, verbosity = 1L) {
      requested <<- c(requested, as.integer(n_threads))
      as.integer(n_threads)
    }
  )
  decom <- decomp(
    x,
    config = quick(),
    execution_config = setup_SerialExecution(
      n_workers_algorithm = 2L,
      seed = 1L
    ),
    verbosity = 0L
  )
  # The fit and the metrics' reconstruction, both on the run's threads.
  expect_identical(requested, c(2L, 2L))
  apply_decomp(
    decom,
    x,
    execution_config = setup_SerialExecution(n_workers_algorithm = 1L),
    verbosity = 0L
  )
  reconstruct(decom, x, verbosity = 0L)
  expect_identical(
    requested,
    c(2L, 2L, 1L, default_n_workers(), default_n_workers())
  )
})


test_that("decomp() names the autoencoder's device and threads in its resources line", {
  skip_if_no_torch()
  line <- paste(
    gsub(
      "\\033\\[[0-9;]*m",
      "",
      testthat::capture_messages(decomp(
        x,
        config = quick(),
        execution_config = setup_SerialExecution(
          n_workers_algorithm = 2L,
          device = "cpu"
        ),
        verbosity = 1L
      ))
    ),
    collapse = ""
  )
  expect_match(
    line,
    "// CPU | serial | 1 worker: algorithm 2 threads (as set)",
    fixed = TRUE
  )
  expect_identical(
    training_device(setup_Autoencoder(), NULL),
    torch_training_device(NULL)
  )
  expect_null(training_device(setup_PCA(), NULL))
})


# Pipelines ----
test_that("an autoencoder config round-trips through a DecomposeConfig document", {
  config <- setup_DecomposeConfig(
    dat_path = "data.csv",
    decomposition_config = setup_Autoencoder(
      k = 3L,
      hidden_units = c(16L, 8L),
      input_noise = 0.1,
      optimizer = "sgd",
      momentum = 0.9
    ),
    outdir = "results/"
  )
  file <- withr::local_tempfile(fileext = ".json")
  write_config(config, file, overwrite = TRUE, verbosity = 0L)
  read_back <- read_config(file)
  expect_identical(
    read_back@decomposition_config@config,
    config@decomposition_config@config
  )
})


test_that("train() learns an autoencoder as its decomposition step and predict() replays it", {
  skip_if_no_torch()
  # Regression on Sepal.Length from the other three measurements.
  dat <- iris[, c(2L, 3L, 4L, 1L)]
  mod <- train(
    dat,
    decomposition_config = quick(k = 2L),
    hyperparameters = setup_GLMNET(alpha = 0, lambda = 0.01),
    execution_config = seeded(),
    verbosity = 0L
  )
  expect_s7_class(mod@decomposition, Decomposition)
  expect_s7_class(mod@decomposition@config, AutoencoderConfig)
  expect_identical(ncol(mod@decomposition@transformed), 2L)
  predicted <- predict(mod, features(dat))
  expect_length(predicted, nrow(dat))
  expect_false(anyNA(predicted))
})
