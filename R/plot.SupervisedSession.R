# plot.SupervisedSession.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% draw_supervised_session ----
#' Plot a SupervisedSession Execution Timeline
#'
#' Render the execution graph captured in a `SupervisedSession` (from rtemis
#' `train()`) as a timeline / Gantt chart: one bar per recorded step, ordered as
#' a depth-first walk of the execution tree, positioned by elapsed time and
#' colored by node kind (failed/aborted steps are outlined in red).
#'
#' The timeline table and the kind-color map come from
#' `session_timeline()` and `session_kind_colors()`, the shared
#' helpers also used by rtemis.server for the rtemislive web UI, so both
#' renderers stay in sync.
#'
#' Registered as the `plot()` method for `SupervisedSession`.
#'
#' @param x `rtemis::SupervisedSession`: Session object, e.g. `model@session`.
#' @param title Optional Character: Chart title.
#' @param theme Optional [rtemis.draw::Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file
#'   via [rtemis.draw::save_drawing()].
#' @param ... Not used.
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
draw_supervised_session <- function(
  x,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL,
  ...
) {
  # One row per node, DFS order, ms offsets, unique indented labels, tooltip
  # text, and a `failed` flag -- all computed by the shared rtemis helper.
  tasks <- session_timeline(x)

  # Color by event KIND so the legend filters by type and the fill is identical
  # for like events, making same-color overlap read as a parallel process.
  # rtemis.draw::draw_gantt() zips groups in first-seen (DFS) order, so index the map by
  # that order.
  cols <- session_kind_colors(unique(tasks[["kind"]]))

  rtemis.draw::draw_gantt(
    tasks,
    group = "kind",
    axis_type = "value",
    tooltip = "tip",
    # Outline failed/aborted nodes (fill still encodes the kind, so a parallel
    # failed cell keeps its siblings' color -> the same-palette = parallel
    # reading holds, while failures still pop via the red border).
    border = "failed",
    xlab = "Elapsed (ms)",
    title = title,
    palette = unname(cols),
    theme = theme,
    width = width,
    height = height,
    filename = filename
  )
} # /rtemis::draw_supervised_session


# %% plot_session ----
#' Plot a Supervised object's session timeline
#'
#' Plots the session timeline of a `Supervised` or `SupervisedRes` object using
#' [rtemis.draw::draw_gantt()].
#'
#' Dispatches on ordinary and resampled supervised results.
#'
#' @param x `rtemis::Supervised` or `rtemis::SupervisedRes` object.
#' @param ... Additional arguments passed to [rtemis.draw::draw_gantt()].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' # GLM keeps the example dependency-free: rtemis fits it with stats::glm().
#'   mod <- train(
#'     mtcars[, c("wt", "hp", "mpg")],
#'     hyperparameters = setup_GLM(),
#'     verbosity = 0L
#'   )
#'   plot_session(mod)
plot_session <- new_generic("plot_session", "x")


# %% draw_object_session ----
#' Plot the session timeline held on a fitted rtemis object
#'
#' Shared implementation for the `plot_session()` methods on
#' `rtemis::Supervised` and `rtemis::SupervisedRes`: both hold their run's
#' session on `@session`.
#'
#' @param x `rtemis::Supervised` or `rtemis::SupervisedRes` object.
#' @param ... Additional arguments passed to [rtemis.draw::draw_gantt()].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
draw_object_session <- function(x, ...) {
  plot(x@session, ...)
} # /rtemis::draw_object_session
