# test_PartitionConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("setup_RandomPartition() succeeds with defaults", {
  cfg <- setup_RandomPartition()
  expect_s7_class(cfg, RandomPartitionConfig)
  expect_equal(cfg@train_p, 0.75)
})


test_that("setup_TimePartition() requires a column", {
  expect_error(setup_TimePartition(), class = "rtemis_value_error")
  cfg <- setup_TimePartition(column = "visit_date")
  expect_s7_class(cfg, TimePartitionConfig)
  expect_equal(cfg@column, "visit_date")
})


test_that("setup_GroupPartition() requires a column", {
  expect_error(setup_GroupPartition(), class = "rtemis_value_error")
  cfg <- setup_GroupPartition(column = "subject_id", seed = 1L)
  expect_s7_class(cfg, GroupPartitionConfig)
})


test_that("setup_PredefinedPartition() requires a column", {
  expect_error(setup_PredefinedPartition(), class = "rtemis_value_error")
  cfg <- setup_PredefinedPartition(column = "split")
  expect_s7_class(cfg, PredefinedPartitionConfig)
  expect_equal(cfg@training_value, "train")
  expect_equal(cfg@test_value, "test")
})


test_that("partition() splits randomly and every case lands on one side", {
  set.seed(2026L)
  dt <- data.frame(a = 1:100, y = rnorm(100))
  man <- partition(dt, setup_RandomPartition(train_p = 0.8, seed = 2026L))
  expect_equal(man[["outputs"]][["training"]][["n_rows"]], 80L)
  expect_equal(man[["outputs"]][["test"]][["n_rows"]], 20L)
  expect_equal(
    man[["outputs"]][["training"]][["n_rows"]] +
      man[["outputs"]][["test"]][["n_rows"]],
    100L
  )
})


test_that("partition() is reproducible with the same seed", {
  dt <- data.frame(a = 1:100, y = rnorm(100))
  m1 <- partition(dt, setup_RandomPartition(train_p = 0.8, seed = 42L))
  m2 <- partition(dt, setup_RandomPartition(train_p = 0.8, seed = 42L))
  expect_identical(
    m1[["outputs"]][["training"]][["fingerprint"]][["hash"]],
    m2[["outputs"]][["training"]][["fingerprint"]][["hash"]]
  )
})


test_that("partition() orders a time split correctly", {
  dt <- data.frame(t = 10:1, y = 1:10)
  man <- partition(dt, setup_TimePartition(column = "t", train_p = 0.7))
  expect_equal(man[["outputs"]][["training"]][["n_rows"]], 7L)
  expect_equal(man[["outputs"]][["test"]][["n_rows"]], 3L)
})


test_that("partition() keeps a group on one side with a group split", {
  dt <- data.frame(
    subject = rep(1:10, each = 5),
    y = rnorm(50)
  )
  man <- partition(
    dt,
    setup_GroupPartition(column = "subject", train_p = 0.8, seed = 1L)
  )
  # Every subject appears exactly 5 times, so a correct group split has row
  # counts that are multiples of 5 on both sides.
  expect_equal(man[["outputs"]][["training"]][["n_rows"]] %% 5L, 0L)
  expect_equal(man[["outputs"]][["test"]][["n_rows"]] %% 5L, 0L)
})


test_that("partition() applies a predefined split", {
  dt <- data.frame(
    split = c(rep("train", 7), rep("test", 3)),
    y = 1:10
  )
  man <- partition(dt, setup_PredefinedPartition(column = "split"))
  expect_equal(man[["outputs"]][["training"]][["n_rows"]], 7L)
  expect_equal(man[["outputs"]][["test"]][["n_rows"]], 3L)
})


test_that("partition() rejects an unexpected value in a predefined split column", {
  dt <- data.frame(split = c("train", "test", "validation"), y = 1:3)
  expect_error(
    partition(dt, setup_PredefinedPartition(column = "split")),
    class = "rtemis_value_error"
  )
})


test_that("partition() rejects a column not present in the data", {
  dt <- data.frame(a = 1:10)
  expect_error(
    partition(dt, setup_TimePartition(column = "no_such_column")),
    class = "rtemis_value_error"
  )
})


test_that("partition() writes Parquet files when outdir is given", {
  skip_if_not_installed("arrow")
  dt <- data.frame(a = 1:20, y = rnorm(20))
  dir <- withr::local_tempdir()
  man <- partition(
    dt,
    setup_RandomPartition(train_p = 0.8, seed = 1L),
    outdir = dir
  )
  expect_true(file.exists(man[["outputs"]][["training"]][["path"]]))
  expect_true(file.exists(man[["outputs"]][["test"]][["path"]]))
})


test_that("a PartitionConfig round-trips through write_config/read_config", {
  x <- setup_TimePartition(column = "visit_date", train_p = 0.8)
  file <- file.path(tempdir(), "rtemis_partition.json")
  write_config(x, file, overwrite = TRUE)
  xl <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(
    xl[["$schema"]],
    "https://schema.rtemis.org/partition/v1/schema.json"
  )
  xtoo <- read_config(file)
  expect_s7_class(xtoo, TimePartitionConfig)
  expect_identical(xtoo@column, "visit_date")
})


test_that(".list_to_PartitionConfig rejects a key belonging to a different method", {
  # A key belonging to a different method reaches that method's constructor
  # and is refused as an unused argument -- a base R error naming the key,
  # not a classed rtemis one, the same as `.list_to_IngestConfig()`.
  expect_error(
    rtemis:::.list_to_PartitionConfig(list(
      method = "random",
      column = "not_a_random_arg"
    )),
    "column"
  )
})
