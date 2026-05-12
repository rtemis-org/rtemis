# test_msg_sink.R
# ::rtemis::
# 2026- EDG rtemis.org

# set_msg_sink ----
test_that("set_msg_sink() accepts a function", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(function(m) NULL)
  expect_true(is.function(get_msg_sink()))
})

test_that("set_msg_sink() accepts NULL", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(function(m) NULL)
  set_msg_sink(NULL)
  expect_null(get_msg_sink())
})

test_that("set_msg_sink() rejects non-function, non-NULL inputs", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  expect_error(set_msg_sink("a string"))
  expect_error(set_msg_sink(123))
  expect_error(set_msg_sink(list()))
})

test_that("set_msg_sink() returns previous sink invisibly", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  fn1 <- function(m) NULL
  fn2 <- function(m) NULL
  set_msg_sink(NULL)
  prev1 <- set_msg_sink(fn1)
  expect_null(prev1)
  prev2 <- set_msg_sink(fn2)
  expect_identical(prev2, fn1)
})


# get_msg_sink ----
test_that("get_msg_sink() returns NULL when no sink is set", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(NULL)
  expect_null(get_msg_sink())
})

test_that("get_msg_sink() returns the current sink function", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  fn <- function(m) NULL
  set_msg_sink(fn)
  expect_identical(get_msg_sink(), fn)
})


# with_msg_sink ----
test_that("with_msg_sink() sets and restores", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(NULL)
  with_msg_sink(function(m) NULL, {
    expect_true(is.function(get_msg_sink()))
  })
  expect_null(get_msg_sink())
})

test_that("with_msg_sink() restores even on error", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(NULL)
  expect_error(
    with_msg_sink(function(m) NULL, stop("boom")),
    "boom"
  )
  expect_null(get_msg_sink())
})

test_that("with_msg_sink() preserves a previously set outer sink", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  outer <- function(m) NULL
  inner <- function(m) NULL
  set_msg_sink(outer)
  with_msg_sink(inner, {
    expect_identical(get_msg_sink(), inner)
  })
  expect_identical(get_msg_sink(), outer)
})


# msg() / msg0() / msgstart() / msgdone() routing ----
test_that("msg() emits a console message when no sink is set", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(NULL)
  expect_message(msg("hello"))
})

test_that("msg() routes to the sink and suppresses console output", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      expect_silent(msg("hello world"))
    }
  )
  expect_length(captured, 1L)
  expect_equal(captured[[1L]][["text"]], "hello world")
  expect_equal(captured[[1L]][["level"]], "info")
  expect_true(is.character(captured[[1L]][["ts"]]))
})

test_that("msg0() routes to the sink with sep = ''", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      expect_silent(msg0("hello", "world"))
    }
  )
  expect_length(captured, 1L)
  expect_equal(captured[[1L]][["text"]], "helloworld")
  expect_equal(captured[[1L]][["level"]], "info")
})

test_that("msgstart() routes to the sink with level = 'start'", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      expect_silent(msgstart("Starting..."))
    }
  )
  expect_length(captured, 1L)
  expect_equal(captured[[1L]][["text"]], "Starting...")
  expect_equal(captured[[1L]][["level"]], "start")
})

test_that("msgdone() routes to the sink with level = 'done' and a caller", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      expect_silent(msgdone())
    }
  )
  expect_length(captured, 1L)
  expect_equal(captured[[1L]][["level"]], "done")
  # caller may be NA depending on test harness call stack — assert type only
  expect_true(
    is.character(captured[[1L]][["caller"]]) ||
      is.na(captured[[1L]][["caller"]])
  )
})

test_that("multiple msg variants accumulate as separate sink events in order", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      msg("one")
      msg0("two")
      msgstart("three")
      msgdone()
    }
  )
  expect_length(captured, 4L)
  expect_equal(
    vapply(captured, `[[`, character(1L), "level"),
    c("info", "info", "start", "done")
  )
  expect_equal(captured[[1L]][["text"]], "one")
  expect_equal(captured[[2L]][["text"]], "two")
  expect_equal(captured[[3L]][["text"]], "three")
})

test_that("verbosity = 0 short-circuits before reaching the sink", {
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      msg("ignored", verbosity = 0L)
      msg0("ignored", verbosity = 0L)
    }
  )
  expect_length(captured, 0L)
})

test_that("clearing the sink restores console output", {
  on.exit(set_msg_sink(NULL), add = TRUE)
  set_msg_sink(function(m) NULL)
  expect_silent(msg("under sink"))
  set_msg_sink(NULL)
  expect_message(msg("after clear"))
})
