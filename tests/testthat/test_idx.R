# test_idx.R
# ::rtemis::
# 2025 EDG rtemis.org

# Packages ----
library(data.table)

# Data ----
xdf <- iris
xdt <- as.data.table(iris)
idx <- c("Sepal.Length", "Species")
idi <- c(1L, 5L)

# Test inc(data.frame) ----
xdf[, idx, drop = FALSE]
xdf[, idi, drop = FALSE]
inc(xdf, idx)
inc(xdf, idi)
test_that("inc(data.frame) works", {
  expect_equal(xdf[, idx, drop = FALSE], inc(xdf, idx))
  expect_equal(xdf[, idi, drop = FALSE], inc(xdf, idi))
  expect_equal(inc(xdf, idx), inc(xdf, idi))
})

# Test inc(data.table) ----
xdt[, ..idx]
xdt[, idx, with = FALSE]
xdt[, ..idi]
xdt[, idi, with = FALSE]
inc(xdt, idx)
inc(xdt, idi)
test_that("inc(data.table) works", {
  expect_equal(xdt[, ..idx], inc(xdt, idx))
  expect_equal(xdt[, ..idi], inc(xdt, idi))
  expect_equal(inc(xdt, idx), inc(xdt, idi))
})

# Test exc(data.frame) ----
xdf[, -which(names(xdf) %in% idx)]
xdf[, -idi]
exc(xdf, idx)
exc(xdf, idi)
test_that("exc(data.frame) works", {
  expect_equal(xdf[, -which(names(xdf) %in% idx)], exc(xdf, idx))
  expect_equal(xdf[, -idi], exc(xdf, idi))
})

# Test exc(data.table) ----
xdt[, !..idx]
xdt[, !idx, with = FALSE]
xdt[, !..idi]
xdt[, !idi, with = FALSE]
exc(xdt, idx)
test_that("exc(data.table) works", {
  expect_equal(xdt[, !..idx, with = FALSE], exc(xdt, idx))
})
