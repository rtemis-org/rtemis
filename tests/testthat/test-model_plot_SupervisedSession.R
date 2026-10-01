# test-plot_SupervisedSession.R
# Tests for the plot method on rtemis SupervisedSession. Skipped when rtemis is
# not installed (the class lives in rtemis and is referenced via getFromNamespace).

# Build a small SupervisedSession with a known tree and mixed statuses.
make_session <- function() {
  SS <- utils::getFromNamespace("SupervisedSession", "rtemis")
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  ev <- function(node_id, parent_id, kind, label, status, off_start, off_end) {
    list(
      node_id = node_id,
      parent_id = parent_id,
      kind = kind,
      label = label,
      status = status,
      t_start = t0 + off_start,
      t_end = t0 + off_end,
      meta = list()
    )
  }
  events <- list(
    ev("n1", NULL, "train", "CART Classification", "ok", 0, 2),
    ev("n2", "n1", "tune", NULL, "ok", 0, 1),
    ev("n3", "n2", "grid_cell", "#1", "ok", 0, 0.4),
    ev("n4", "n2", "grid_cell", "#2", "error", 0.4, 0.5),
    ev("n5", "n1", "train_alg", "CART", "ok", 1, 1.8),
    ev("n6", "n1", "predict", NULL, "ok", 1.8, 2)
  )
  SS(id = "test", events = events, started = t0, finished = t0 + 2)
}


test_that("plot.SupervisedSession returns an htmlwidget", {
  expect_s3_class(plot(make_session()), "htmlwidget")
})


test_that("plot.SupervisedSession makes one category row per node in DFS order", {
  w <- plot(make_session())
  rows <- unlist(w$x$option$yAxis$data)
  expect_length(rows, 6L)
  # Depth-first: train, tune, its two grid cells, then train_algo, predict.
  expect_match(rows[[1]], "train CART Classification")
  expect_match(rows[[2]], "^\\s+tune$")
  expect_match(rows[[3]], "grid_cell #1")
  expect_match(rows[[4]], "grid_cell #2")
  expect_match(rows[[5]], "train_alg CART")
  expect_match(rows[[6]], "predict")
})


test_that("plot.SupervisedSession groups series by event kind", {
  w <- plot(make_session())
  series_names <- vapply(
    w$x$option$series,
    function(s) s$name %||% "",
    character(1L)
  )
  # One series per kind present (not by status).
  expect_setequal(
    series_names,
    c("train", "tune", "grid_cell", "train_alg", "predict")
  )
})


test_that("plot.SupervisedSession outlines failed bars via the border flag", {
  w <- plot(make_session())
  gc <- Filter(function(s) identical(s$name, "grid_cell"), w$x$option$series)[[
    1
  ]]
  # The error grid_cell (#2) carries a truthy 4th value; the ok one does not.
  flags <- vapply(
    gc$data,
    function(d) as.numeric(d$value[[4]] %||% 0),
    numeric(1L)
  )
  expect_true(any(flags == 1)) # the errored cell
  expect_true(any(flags == 0)) # the ok cell
})


test_that("plot.SupervisedSession positions bars by elapsed milliseconds", {
  w <- plot(make_session())
  # Collect [row, start, end] (first 3 dims; failed bars carry a 4th flag).
  vals <- do.call(
    rbind,
    lapply(w$x$option$series, function(s) {
      do.call(rbind, lapply(s$data, function(d) unlist(d$value)[1:3]))
    })
  )
  # Root "train" spans 0 -> 2000 ms.
  root <- vals[vals[, 1] == 0, , drop = FALSE]
  expect_equal(root[1, 2], 0)
  expect_equal(root[1, 3], 2000)
})


test_that("plot.SupervisedSession errors on an empty session", {
  SS <- utils::getFromNamespace("SupervisedSession", "rtemis")
  empty <- SS(id = "empty", events = list(), started = Sys.time())
  expect_error(plot(empty), "no recorded events")
})
