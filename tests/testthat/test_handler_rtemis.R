# test_handler_rtemis.R

test_that("handler_rtemis() bridges progressr to rtemis progress envelopes", {
  skip_if_not_installed("progressr")
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  rtemis.core::with_msg_sink(
    function(m) {
      if (identical(m[["level"]], "progress")) {
        captured[[length(captured) + 1L]] <<- m
      }
    },
    progressr::with_progress(
      {
        p <- progressr::progressor(steps = 3L)
        for (i in 1:3) {
          p()
        }
      },
      handlers = handler_rtemis(label = "Bridged", kind = "tune"),
      enable = TRUE
    )
  )
  statuses <- vapply(captured, function(m) m[["status"]], character(1L))
  expect_equal(statuses[[1L]], "start")
  expect_equal(statuses[[length(statuses)]], "done")
  expect_true("update" %in% statuses)
  expect_equal(captured[[1L]][["kind"]], "tune")
  expect_equal(captured[[1L]][["total"]], 3L)
  # Final update reports the last completed step.
  updates <- Filter(function(m) m[["status"]] == "update", captured)
  expect_equal(updates[[length(updates)]][["current"]], 3L)
})

test_that("handler_rtemis() renders begin/completion lines on the console", {
  skip_if_not_installed("progressr")
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  msgs <- capture_messages(
    progressr::with_progress(
      {
        p <- progressr::progressor(steps = 2L)
        p()
        p()
      },
      handlers = handler_rtemis(label = "Bridged", output_type = "plain"),
      enable = TRUE
    )
  )
  stripped <- rtemis.core::strip_ansi(paste(msgs, collapse = ""))
  expect_match(stripped, "Bridged", fixed = TRUE)
  expect_match(stripped, "done in", fixed = TRUE)
})
