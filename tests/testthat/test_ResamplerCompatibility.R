# test_ResamplerCompatibility.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Compatibility dispatch ----
test_that("deprecated resampler dispatch matches each dedicated setup", {
  cases <- list(
    KFold = list(
      n_resamples = 3L,
      stratify_var = "Species",
      strat_n_bins = 3L,
      id_strat = "id",
      seed = 42L
    ),
    StratSub = list(
      n_resamples = 4L,
      train_p = .6,
      stratify_var = "Species",
      strat_n_bins = 3L,
      id_strat = "id",
      seed = 42L
    ),
    StratBoot = list(
      n_resamples = 4L,
      train_p = .6,
      target_length = 20L,
      stratify_var = "Species",
      strat_n_bins = 3L,
      id_strat = "id",
      seed = 42L
    ),
    Bootstrap = list(n_resamples = 4L, id_strat = "id", seed = 42L),
    LOOCV = list(),
    Custom = list(resamples = list(1:3, 2:4))
  )
  for (type in names(cases)) {
    args <- cases[[type]]
    expect_warning(
      actual <- do.call(setup_Resampler, c(list(type = type), args)),
      paste0("setup_", type),
      class = "deprecatedWarning"
    )
    expect_equal(actual, do.call(get(paste0("setup_", type)), args))
    deprecation <- tryCatch(
      do.call(setup_Resampler, c(list(type = type), args)),
      deprecatedWarning = identity
    )
    expect_identical(deprecation[["old"]], "setup_Resampler")
    expect_identical(deprecation[["new"]], paste0("setup_", type))
    expect_identical(deprecation[["package"]], "rtemis")
  }
})


# %% Defaults and argument matching ----
test_that("compatibility preserves defaults, positional arguments, and matching", {
  expect_warning(actual <- setup_Resampler(), "setup_KFold")
  expect_equal(actual, setup_KFold())
  for (type in c("KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV")) {
    expect_equal(
      suppressWarnings(setup_Resampler(type = type)),
      do.call(get(paste0("setup_", type)), list())
    )
  }
  expect_equal(
    suppressWarnings(setup_Resampler(
      3L,
      "kfold",
      NULL,
      .75,
      4L,
      NULL,
      NULL,
      42L,
      0L
    )),
    setup_KFold(3L, seed = 42L)
  )
  expect_equal(
    suppressWarnings(setup_Resampler(type = "strats")),
    setup_StratSub()
  )
  expect_equal(
    suppressWarnings(setup_Resampler(seed = NULL)),
    setup_KFold(seed = NULL)
  )
  expect_warning(
    setup_Resampler(verbosity = 0L),
    "deprecated",
    class = "deprecatedWarning"
  )
})


# %% Validation and irrelevant arguments ----
test_that("compatibility delegates relevant validation and ignores other settings", {
  expect_equal(
    suppressWarnings(setup_Resampler(
      type = "LOOCV",
      n_resamples = 0L,
      train_p = -1
    )),
    setup_LOOCV()
  )
  expect_equal(
    suppressWarnings(setup_Resampler(
      type = "Bootstrap",
      train_p = -1,
      stratify_var = "unused"
    )),
    setup_Bootstrap()
  )
  expect_error(suppressWarnings(setup_Resampler(n_resamples = 0L)))
  expect_error(suppressWarnings(setup_Resampler(
    type = "StratSub",
    train_p = 2
  )))
  expect_error(
    suppressWarnings(setup_Resampler(type = "Custom")),
    "supply `resamples`"
  )
  expect_error(setup_Resampler(type = "unknown"))
  expect_error(suppressWarnings(setup_Resampler(id_strat = c("a", "b"))))
})
