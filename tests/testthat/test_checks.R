# test_checks.R
# ::rtemis::
# 2025- EDG rtemis.org

# Test do_call ----
test_that("do_call() succeeds", {
  expect_equal(do_call(sum, list(1, 2, 3)), 6)
})


# do_call passes warnings through ----
test_that("do_call() leaves a backend warning as a condition", {
  # A warning belongs to the caller, so `do_call()` must not muffle it into a
  # print: muffling made `suppressWarnings()`, `tryCatch()` and
  # `options(warn = 2)` all inert around every rtemis backend call, with no
  # visible symptom.
  warner <- function() {
    warning("NAs introduced by coercion")
    42
  }
  expect_warning(do_call(warner, list()), "NAs introduced by coercion")
  expect_equal(
    suppressWarnings(do_call(warner, list(), verbosity = 0L)),
    42
  )
  expect_equal(
    tryCatch(
      do_call(warner, list(), verbosity = 0L),
      warning = function(w) conditionMessage(w)
    ),
    "NAs introduced by coercion"
  )
  # Every occurrence reaches the caller, not just the first.
  repeat_warner <- function() {
    for (i in seq_len(5L)) {
      warning("NAs introduced by coercion")
    }
    42
  }
  seen <- 0L
  withCallingHandlers(
    do_call(repeat_warner, list(), verbosity = 0L),
    warning = function(w) {
      seen <<- seen + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(seen, 5L)
})


# do_call advice obeys verbosity ----
test_that("do_call() prints advice only when matched, and only when verbose", {
  warner <- function() {
    warning("NAs introduced by coercion")
    42
  }
  unmatched <- function() {
    warning("a warning rtemis has no advice for")
    42
  }
  # The advice is an rtemis message, so `verbosity` and `suppressMessages()`
  # both govern it; the warning it advises on is a separate condition governed
  # by `suppressWarnings()`.
  expect_message(
    suppressWarnings(do_call(warner, list(), verbosity = 1L)),
    "Check that the input is of the correct type"
  )
  expect_silent(suppressWarnings(do_call(warner, list(), verbosity = 0L)))
  expect_no_message(suppressWarnings(do_call(warner, list(), verbosity = 0L)))
  # An unrecognized warning is left to speak for itself rather than echoed.
  expect_silent(suppressWarnings(do_call(unmatched, list(), verbosity = 1L)))
  # Advice keyed to one warning is given once, however often it repeats.
  repeat_warner <- function() {
    for (i in seq_len(5L)) {
      warning("NAs introduced by coercion")
    }
    42
  }
  advice <- capture.output(
    type = "message",
    invisible(suppressWarnings(do_call(repeat_warner, list(), verbosity = 1L)))
  )
  expect_length(
    grep("Check that the input is of the correct type", advice),
    1L
  )
})
