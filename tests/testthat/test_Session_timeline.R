# test_Session_timeline.R
# Tests for session_timeline() and session_kind_colors().

# Fixed origin so elapsed-ms expectations are exact.
t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

# Minimal event record, mirroring node_enter()/node_exit() output.
# `end_s = NA` leaves the node unclosed (t_end = NA), as for running/aborted.
make_event <- function(
  id,
  parent,
  kind,
  label = NULL,
  status = "ok",
  start_s = 0,
  end_s = NA
) {
  list(
    node_id = id,
    parent_id = parent,
    kind = kind,
    label = label,
    status = status,
    t_start = t0 + start_s,
    t_end = if (is.na(end_s)) as.POSIXct(NA) else t0 + end_s,
    meta = list()
  )
}

make_session <- function(events, finished = t0 + 10) {
  rtemis:::SupervisedSession(
    id = "test-session",
    events = events,
    started = t0,
    finished = finished
  )
}

test_that("session_timeline returns one row per node in depth-first order", {
  # Insertion order: n1, n2, n4, n3 -- but n3 is a child of n2, so the DFS
  # walk must place it before n4 (insertion order != DFS order).
  events <- list(
    make_event("n1", NA_character_, "train", start_s = 0, end_s = 10),
    make_event("n2", "n1", "tune", start_s = 0, end_s = 5),
    make_event("n4", "n1", "train_alg", label = "GLM", start_s = 5, end_s = 9),
    make_event("n3", "n2", "grid_cell", label = "#1", start_s = 1, end_s = 4)
  )
  tl <- session_timeline(make_session(events))
  expect_s3_class(tl, "data.table")
  expect_equal(nrow(tl), 4L)
  expect_named(
    tl,
    c("label", "start", "end", "kind", "status", "failed", "tip")
  )
  expect_equal(tl[["kind"]], c("train", "tune", "grid_cell", "train_alg"))
  # Labels are indented two spaces per depth and include the detail label.
  expect_equal(
    tl[["label"]],
    c("train", "  tune", "    grid_cell #1", "  train_alg GLM")
  )
})

test_that("session_timeline computes elapsed milliseconds from session start", {
  events <- list(
    make_event("n1", NA_character_, "train", start_s = 0, end_s = 10),
    make_event("n2", "n1", "tune", start_s = 1, end_s = 4.5)
  )
  tl <- session_timeline(make_session(events))
  expect_equal(tl[["start"]], c(0, 1000))
  expect_equal(tl[["end"]], c(10000, 4500))
})

test_that("session_timeline closes unfinished nodes at session finish", {
  events <- list(
    make_event("n1", NA_character_, "train", start_s = 0, end_s = NA),
    make_event(
      "n2",
      "n1",
      "train_alg",
      status = "running",
      start_s = 2,
      end_s = NA
    )
  )
  tl <- session_timeline(make_session(events, finished = t0 + 8))
  # Both unclosed nodes extend to the session finish time (8 s = 8000 ms).
  expect_equal(tl[["end"]], c(8000, 8000))
  expect_match(tl[["tip"]][[2L]], "running")
})

test_that("session_timeline falls back to latest start when finish is missing", {
  events <- list(
    make_event("n1", NA_character_, "train", start_s = 0, end_s = NA),
    make_event("n2", "n1", "tune", status = "aborted", start_s = 3, end_s = NA)
  )
  tl <- session_timeline(make_session(events, finished = NULL))
  # No finish time: unclosed nodes extend to the latest recorded start (3 s).
  expect_equal(tl[["end"]], c(3000, 3000))
})

test_that("session_timeline flags failed and aborted nodes", {
  events <- list(
    make_event("n1", NA_character_, "train", start_s = 0, end_s = 10),
    make_event(
      "n2",
      "n1",
      "train_alg",
      status = "error",
      start_s = 0,
      end_s = 1
    ),
    make_event("n3", "n1", "tune", status = "aborted", start_s = 1, end_s = 2),
    make_event("n4", "n1", "predict", status = "ok", start_s = 2, end_s = 3)
  )
  tl <- session_timeline(make_session(events))
  expect_equal(tl[["failed"]], c(FALSE, TRUE, TRUE, FALSE))
  expect_match(tl[["tip"]][[2L]], "\\[error\\]")
})

test_that("session_timeline disambiguates duplicate labels", {
  # Two grid cells with identical kind + label at the same depth would
  # otherwise collapse onto one category row.
  events <- list(
    make_event("n1", NA_character_, "tune", start_s = 0, end_s = 5),
    make_event("n2", "n1", "grid_cell", label = "#1", start_s = 0, end_s = 2),
    make_event("n3", "n1", "grid_cell", label = "#1", start_s = 0, end_s = 3)
  )
  tl <- session_timeline(make_session(events))
  expect_equal(anyDuplicated(tl[["label"]]), 0L)
})

test_that("session_timeline rejects invalid input", {
  expect_error(
    session_timeline(list()),
    class = "rtemis_type_error"
  )
  expect_error(
    session_timeline(make_session(list())),
    class = "rtemis_value_error"
  )
})

test_that("session_kind_colors maps known kinds and recycles for unknown", {
  kinds <- c("train", "tune", "grid_cell", "train_alg")
  cols <- session_kind_colors(kinds)
  expect_named(cols, kinds)
  expect_false(anyNA(cols))
  expect_equal(cols[["train"]], "#808080")
  # All values are valid colors (col2rgb errors on invalid specs).
  expect_no_error(grDevices::col2rgb(cols))
  # Unknown kinds fall back to the rtemis palette rather than NA.
  extra <- session_kind_colors(c("train", "custom_step"))
  expect_false(anyNA(extra))
  expect_equal(extra[["custom_step"]], rtemis_colors[[1L]])
})
