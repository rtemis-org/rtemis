# handler_rtemis.R
# ::rtemis::
# 2026- EDG rtemis.org

#' progressr handler rendering through the rtemis progress system
#'
#' A [progressr::make_progression_handler()] backend that maps `progression`
#' conditions onto [rtemis.core::progress_begin()] /
#' [rtemis.core::progress_update()] / [rtemis.core::progress_end()]. This is
#' how parallel loops report progress in rtemis: workers signal progressions
#' (e.g. via `progressr::progressor()`), the future framework relays them to
#' the main session, and this handler renders them - so parallel tuning gets
#' the same breadcrumb status line, nests correctly under any active
#' sequential progress (e.g. outer resamples), and emits structured
#' `level = "progress"` envelopes when a message sink is set (see
#' [rtemis.core::set_msg_sink()]).
#'
#' @details
#' Used internally by `tune_GridSearch()` around its future backend, wrapped
#' as `progressr::with_progress(..., handlers = handler_rtemis(...))`. It can
#' also be registered globally for arbitrary progressr-instrumented code:
#' `progressr::handlers(handler_rtemis())`.
#'
#' `enable` defaults to `TRUE` (unlike most progressr handlers, which default
#' to interactive sessions only) because the rtemis progress system handles
#' non-interactive output itself and must emit sink envelopes regardless of
#' the display; console rendering is gated by `verbosity` downstream. Note
#' that [progressr::with_progress()] additionally gates delivery on the
#' global `progressr.enable` option (FALSE in non-interactive sessions), so
#' non-interactive callers should also pass `enable = TRUE` to
#' `with_progress()` itself, as `tune_GridSearch()` does.
#'
#' Update frequency is bounded by when the parallel framework relays
#' conditions (typically as chunks resolve) and by the rtemis.core throttle
#' (`options(rtemis.progress_throttle = )`).
#'
#' @param label Character: Display label for the progress node.
#' @param kind Character: Node kind forwarded in the sink envelope.
#' @param verbosity Integer or NULL: Overrides `rtemis.core::get_verbosity()`
#'   when supplied. Gates only console rendering, never sink events.
#' @param output_type Character or NULL: `"ansi"`, `"html"`, or `"plain"`;
#'   resolved via [rtemis.core::get_output_type()] when NULL.
#' @param enable Logical: If TRUE, handler is active (see Details).
#' @param target Character: progressr handler target.
#' @param ... Additional arguments passed to
#'   [progressr::make_progression_handler()].
#'
#' @return A `progression_handler` function, for use with
#'   [progressr::with_progress()] or [progressr::handlers()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' progressr::with_progress(
#'   {
#'     p <- progressr::progressor(steps = 5L)
#'     for (i in 1:5) {
#'       Sys.sleep(0.1)
#'       p()
#'     }
#'   },
#'   handlers = handler_rtemis(label = "Working"),
#'   enable = TRUE
#' )
#' }
handler_rtemis <- function(
  label = "Progress",
  kind = "progress",
  verbosity = NULL,
  output_type = NULL,
  enable = TRUE,
  target = "terminal",
  ...
) {
  check_dependencies("progressr")
  reporter <- local({
    handle <- NULL
    list(
      reset = function(...) {
        handle <<- NULL
      },
      initiate = function(config, state, progression, ...) {
        if (!state[["enabled"]]) {
          return()
        }
        if (is.null(handle)) {
          handle <<- rtemis.core::progress_begin(
            total = config[["max_steps"]],
            label = label,
            kind = kind,
            verbosity = verbosity,
            output_type = output_type
          )
        }
      },
      update = function(config, state, progression, ...) {
        if (!state[["enabled"]] || is.null(handle)) {
          return()
        }
        if (progression[["amount"]] == 0) {
          return()
        }
        rtemis.core::progress_update(handle, current = state[["step"]])
      },
      finish = function(config, state, progression, ...) {
        if (is.null(handle)) {
          return()
        }
        rtemis.core::progress_end(handle, status = "done")
        handle <<- NULL
      }
    )
  })
  progressr::make_progression_handler(
    "rtemis",
    reporter,
    enable = enable,
    target = target,
    ...
  )
}
