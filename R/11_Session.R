# 11_Session.R
# ::rtemis::
# 2026- EDG rtemis.org

# Observability session: captures the execution graph of a train() run as a flat,
# append-only list of node records. See specs/observability.md for the full design.

# %% SupervisedSession ----
#' SupervisedSession Class
#'
#' @description
#' Execution observability session for a `train()` run. Holds the run's unique id and
#' the flat, append-only event log from which the execution graph (tree/DAG) is
#' reconstructed via each record's `parent_id`.
#'
#' @details
#' Each element of `@events` is a list with fields: `node_id`, `parent_id`, `kind`,
#' `label`, `status` (one of `"running"`, `"ok"`, `"error"`, `"aborted"`), `t_start`,
#' `t_end`, and `meta` (kind-specific payload).
#'
#' @author EDG
#' @noRd
SupervisedSession <- new_class(
  name = "SupervisedSession",
  package = "rtemis",
  properties = list(
    id = class_character,
    events = class_list,
    started = class_any,
    finished = class_any
  ),
  constructor = function(id, events = list(), started = NULL, finished = NULL) {
    new_object(
      S7::S7_object(),
      id = id,
      events = events,
      started = started,
      finished = finished
    )
  }
) # /rtemis::SupervisedSession


# %% repr.SupervisedSession ----
method(repr, SupervisedSession) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  out <- repr_S7name("SupervisedSession", pad = pad, output_type = output_type)
  events <- x@events
  if (length(events) == 0L) {
    return(paste0(
      out,
      strrep(" ", pad + 2L),
      gray("(no events recorded)", output_type = output_type),
      "\n"
    ))
  }
  # Index records by node_id and collect children per parent.
  ids <- vapply(events, \(e) e[["node_id"]], character(1L))
  by_id <- stats::setNames(events, ids)
  parents <- vapply(
    events,
    function(e) {
      p <- e[["parent_id"]]
      if (is.null(p)) NA_character_ else p
    },
    character(1L)
  )
  children <- split(ids, factor(parents, levels = unique(parents)))
  roots <- ids[is.na(parents)]
  # Recursively render the tree.
  render_node <- function(node_id, depth) {
    rec <- by_id[[node_id]]
    indent <- strrep("  ", depth + 1L)
    glyph <- session_status_glyph(rec[["status"]], output_type = output_type)
    dur <- session_format_duration(rec[["t_start"]], rec[["t_end"]])
    line <- paste0(
      indent,
      glyph,
      " ",
      fmt(rec[["kind"]], col = col_info, output_type = output_type),
      session_label_suffix(rec),
      if (!is.null(dur)) {
        gray(paste0(" (", dur, ")"), output_type = output_type)
      },
      "\n"
    )
    kids <- children[[node_id]]
    if (!is.null(kids)) {
      for (k in kids) {
        line <- paste0(line, render_node(k, depth + 1L))
      }
    }
    line
  }
  for (r in roots) {
    out <- paste0(out, render_node(r, 0L))
  }
  out
} # /rtemis::repr.SupervisedSession


# %% print.SupervisedSession ----
method(print, SupervisedSession) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.SupervisedSession


# %% session_status_glyph ----
#' Status glyph for a session node
#'
#' @param status Character: Node status.
#' @param output_type Character: Output type.
#'
#' @return Character: Styled glyph.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_status_glyph <- function(status, output_type = NULL) {
  switch(
    status,
    ok = fmt("\u2714", col = col_success, output_type = output_type),
    error = fmt("\u2718", col = col_error, output_type = output_type),
    aborted = fmt("\u26A0", col = col_warn, output_type = output_type),
    running = fmt("\u2026", col = col_info, output_type = output_type),
    fmt("\u2022", output_type = output_type)
  )
} # /rtemis::session_status_glyph


# %% session_format_duration ----
#' Format a node duration
#'
#' @param t_start POSIXct: Start time.
#' @param t_end POSIXct or NA: End time.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_format_duration <- function(t_start, t_end) {
  if (is.null(t_end) || length(t_end) == 0L || is.na(t_end)) {
    return(NULL)
  }
  secs <- as.numeric(difftime(t_end, t_start, units = "secs"))
  if (secs < 1) {
    ms <- secs * 1000
    # A value that rounds to "0 ms" under %.0f was actually below 0.5 ms; say so.
    if (ms < 1) "<0.5 ms" else sprintf("%.0f ms", ms)
  } else if (secs < 60) {
    sprintf("%.1f s", secs)
  } else {
    sprintf("%.1f min", secs / 60)
  }
} # /rtemis::session_format_duration


# %% session_label_suffix ----
#' Non-redundant label suffix for a node
#'
#' Returns `" <label>"` when the record carries a distinguishing detail, or `""` when it
#' has none or merely repeats `kind`. The `kind` token already names the node, so labels
#' must add only what `kind` does not say.
#'
#' @param rec List: Node record.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_label_suffix <- function(rec) {
  lbl <- rec[["label"]]
  if (!is.null(lbl) && nzchar(lbl) && !identical(lbl, rec[["kind"]])) {
    paste0(" ", lbl)
  } else {
    ""
  }
} # /rtemis::session_label_suffix


# %% --- Ambient session lifecycle ----
# The active session lives at a single fixed key `live[["session"]]` (a mutable
# environment), so node_enter()/node_exit() and nested train() calls find "the current
# session" with no arguments. See specs/observability.md section 3.

# %% session_start ----
#' Start an observability session
#'
#' Creates the ambient session in `live[["session"]]`. A nested `train()` call detects
#' an active session and does **not** create a new one, so its nodes nest under the
#' current stack.
#'
#' @param verbosity Integer: Top-level verbosity; gates the console renderer only.
#'
#' @return Logical: `TRUE` if a session was created (top-level call), `FALSE` if one was
#' already active (nested call).
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_start <- function(verbosity = 1L) {
  if (!is.null(live[["session"]])) {
    return(FALSE)
  }
  s <- new.env(parent = emptyenv())
  s[["id"]] <- session_new_id()
  s[["nodes"]] <- new.env(parent = emptyenv())
  s[["order"]] <- character()
  s[["stack"]] <- character()
  s[["counter"]] <- 0L
  s[["started"]] <- Sys.time()
  s[["verbosity"]] <- verbosity
  live[["session"]] <- s
  TRUE
} # /rtemis::session_start


# %% session_active ----
#' Is an observability session active?
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_active <- function() {
  !is.null(live[["session"]])
} # /rtemis::session_active


# %% session_new_id ----
#' Generate a unique session id
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_new_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    paste0(sample(c(0:9, letters[1:6]), 6L, replace = TRUE), collapse = "")
  )
} # /rtemis::session_new_id


# %% node_enter ----
#' Enter an execution graph node
#'
#' Records a new `"running"` node under the current stack top, pushes it onto the stack,
#' and fires the enriched message sink (live stream). No-op when no session is active
#' (safe in functions callable standalone, and on daemons whose `live` env has no
#' session).
#'
#' @param kind Character: Node kind (e.g. `"tune"`, `"preprocess"`, `"train_alg"`).
#' @param label Optional Character: Human-readable label.
#' @param meta List: Kind-specific payload.
#' @param total Optional Integer: Expected number of child steps (for progress).
#'
#' @return Character `node_id`, or `NULL` if no session is active.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_enter <- function(kind, label = NULL, meta = list(), total = NULL) {
  s <- live[["session"]]
  if (is.null(s)) {
    return(NULL)
  }
  s[["counter"]] <- s[["counter"]] + 1L
  node_id <- paste0("n", s[["counter"]])
  parent_id <- if (length(s[["stack"]])) {
    s[["stack"]][length(s[["stack"]])]
  } else {
    NA_character_
  }
  rec <- list(
    node_id = node_id,
    parent_id = parent_id,
    kind = kind,
    # `label` carries only the distinguishing detail (e.g. "10/10", "LightRF"); the `kind`
    # already names the node, so labels must not repeat it. NULL when there is no detail.
    label = label,
    status = "running",
    t_start = Sys.time(),
    t_end = as.POSIXct(NA),
    meta = meta
  )
  s[["nodes"]][[node_id]] <- rec
  s[["order"]] <- c(s[["order"]], node_id)
  s[["stack"]] <- c(s[["stack"]], node_id)
  session_render(rec, phase = "start", verbosity = s[["verbosity"]])
  session_emit_sink(rec, phase = "start", total = total)
  node_id
} # /rtemis::node_enter


# %% node_exit ----
#' Exit an execution graph node
#'
#' Finalizes a node's status, end time, and merged meta, pops the stack, and fires the
#' enriched sink. No-op when `node_id` is `NULL` or no session is active.
#'
#' @param node_id Character or NULL: The id returned by `node_enter()`.
#' @param status Character: One of `"ok"`, `"error"`, `"aborted"`.
#' @param meta List: Additional payload to merge (e.g. metric).
#' @param error Optional condition: Captured error; its message/class are stored on meta.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_exit <- function(node_id, status = "ok", meta = list(), error = NULL) {
  if (is.null(node_id)) {
    return(invisible(NULL))
  }
  s <- live[["session"]]
  if (is.null(s)) {
    return(invisible(NULL))
  }
  rec <- s[["nodes"]][[node_id]]
  if (is.null(rec)) {
    return(invisible(NULL))
  }
  rec[["t_end"]] <- Sys.time()
  rec[["status"]] <- status
  if (!is.null(error)) {
    meta[["error_message"]] <- conditionMessage(error)
    meta[["error_class"]] <- class(error)
  }
  if (length(meta)) {
    rec[["meta"]] <- utils::modifyList(rec[["meta"]], meta)
  }
  s[["nodes"]][[node_id]] <- rec
  # Render before popping: session_render derives indentation from the stack length, so
  # the "done" line must see the same stack depth as the matching "start" line did.
  session_render(rec, phase = "done", verbosity = s[["verbosity"]])
  # Pop the stack down to (and excluding) this node; defensive against skipped exits.
  pos <- match(node_id, s[["stack"]])
  if (!is.na(pos)) {
    s[["stack"]] <- s[["stack"]][seq_len(pos - 1L)]
  }
  session_emit_sink(rec, phase = if (status == "ok") "done" else status)
  invisible(NULL)
} # /rtemis::node_exit


# %% session_finalize ----
#' Finalize the active session into a `SupervisedSession` object
#'
#' Closes any still-open nodes as `"aborted"`, assembles the ordered event list, and
#' returns a `SupervisedSession`. Does not clear the ambient slot; see `session_clear()`.
#'
#' @return `SupervisedSession` object, or `NULL` if no session is active.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_finalize <- function() {
  s <- live[["session"]]
  if (is.null(s)) {
    return(NULL)
  }
  # Close any nodes still open (e.g. on an aborted run) as "aborted".
  for (nid in rev(s[["stack"]])) {
    rec <- s[["nodes"]][[nid]]
    if (!is.null(rec) && identical(rec[["status"]], "running")) {
      rec[["status"]] <- "aborted"
      rec[["t_end"]] <- Sys.time()
      s[["nodes"]][[nid]] <- rec
    }
  }
  s[["stack"]] <- character()
  events <- lapply(s[["order"]], function(nid) s[["nodes"]][[nid]])
  SupervisedSession(
    id = s[["id"]],
    events = events,
    started = s[["started"]],
    finished = Sys.time()
  )
} # /rtemis::session_finalize


# %% session_clear ----
#' Clear the ambient session slot
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_clear <- function() {
  live[["session"]] <- NULL
  invisible(NULL)
} # /rtemis::session_clear


# %% session_emit_sink ----
#' Forward a node event to the message sink (live stream)
#'
#' Fires the rtemis.core message sink (if installed) with the enriched envelope so
#' rtemis.server can emit nested `job.progress` events. No-op when no sink is installed
#' (e.g. plain interactive use). The recorder and this sink fire regardless of verbosity;
#' only the console renderer is verbosity-gated.
#'
#' @param rec List: Node record.
#' @param phase Character: `"start"`, `"done"`, `"error"`, or `"aborted"`.
#' @param total Optional Integer: Expected child count.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_emit_sink <- function(rec, phase, total = NULL) {
  sink <- rtemis.core::get_msg_sink()
  if (is.null(sink)) {
    return(invisible(NULL))
  }
  sink(list(
    text = paste0(rec[["kind"]], session_label_suffix(rec)),
    caller = "train",
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level = if (phase == "error") "error" else "info",
    node_id = rec[["node_id"]],
    parent_id = rec[["parent_id"]],
    kind = rec[["kind"]],
    status = phase,
    total = total
  ))
  invisible(NULL)
} # /rtemis::session_emit_sink


# %% session_render ----
#' Render a node event to the console (nested progress)
#'
#' Verbosity-gated console rendering of the unified event stream: indented lines that
#' reflect node depth. Replaces the previous `cli_progress_along`/`progressr` UIs for the
#' instrumented steps.
#'
#' @param rec List: Node record.
#' @param phase Character: `"start"` or `"done"`.
#' @param verbosity Integer: Top-level verbosity.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_render <- function(rec, phase, verbosity = 1L) {
  # Console rendering of the execution graph is diagnostic detail, off by default. The
  # recorder (graph) and the sink (wire) fire regardless; only this console path is gated,
  # at verbosity >= 2. See specs/observability.md section 6.
  if (is.null(verbosity) || verbosity < 2L) {
    return(invisible(NULL))
  }
  s <- live[["session"]]
  depth <- if (!is.null(s)) max(length(s[["stack"]]) - 1L, 0L) else 0L
  indent <- strrep("  ", depth)
  if (phase == "start") {
    msg0(
      indent,
      fmt("\u25b6 ", col = col_info),
      fmt(rec[["kind"]], col = col_info),
      session_label_suffix(rec),
      verbosity = verbosity
    )
  } else {
    dur <- session_format_duration(rec[["t_start"]], rec[["t_end"]])
    msg0(
      indent,
      session_status_glyph(rec[["status"]]),
      " ",
      fmt(rec[["kind"]], col = col_info),
      session_label_suffix(rec),
      if (!is.null(dur)) gray(paste0(" (", dur, ")")),
      verbosity = verbosity
    )
  }
  invisible(NULL)
} # /rtemis::session_render


# %% node_meta ----
#' Merge metadata onto a session node
#'
#' Updates the meta of the current stack-top node (or a given `node_id`). No-op when no
#' session is active.
#'
#' @param meta List: Metadata to merge.
#' @param node_id Optional Character: Target node; defaults to the current stack top.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_meta <- function(meta, node_id = NULL) {
  s <- live[["session"]]
  if (is.null(s)) {
    return(invisible(NULL))
  }
  if (is.null(node_id)) {
    if (!length(s[["stack"]])) {
      return(invisible(NULL))
    }
    node_id <- s[["stack"]][length(s[["stack"]])]
  }
  rec <- s[["nodes"]][[node_id]]
  if (is.null(rec)) {
    return(invisible(NULL))
  }
  rec[["meta"]] <- utils::modifyList(rec[["meta"]], meta)
  s[["nodes"]][[node_id]] <- rec
  invisible(NULL)
} # /rtemis::node_meta


# %% session_add_node ----
#' Add a completed node to the session (host-synthesize)
#'
#' Records an already-finished node under the current stack top without pushing the stack.
#' Used to reconstruct nodes for work that ran in daemons (e.g. grid cells), where the
#' host knows the dispatched structure and fills status/meta from returned results. See
#' specs/observability.md section 4. No-op when no session is active.
#'
#' @param kind Character: Node kind.
#' @param label Optional Character: Label.
#' @param status Character: Final status.
#' @param meta List: Payload.
#' @param t_start,t_end Optional POSIXct: Timestamps.
#'
#' @return Character `node_id`, or `NULL` if no session is active.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_add_node <- function(
  kind,
  label = NULL,
  status = "ok",
  meta = list(),
  t_start = NULL,
  t_end = NULL
) {
  s <- live[["session"]]
  if (is.null(s)) {
    return(NULL)
  }
  s[["counter"]] <- s[["counter"]] + 1L
  node_id <- paste0("n", s[["counter"]])
  parent_id <- if (length(s[["stack"]])) {
    s[["stack"]][length(s[["stack"]])]
  } else {
    NA_character_
  }
  now <- Sys.time()
  rec <- list(
    node_id = node_id,
    parent_id = parent_id,
    kind = kind,
    label = label,
    status = status,
    t_start = if (is.null(t_start)) now else t_start,
    t_end = if (is.null(t_end)) now else t_end,
    meta = meta
  )
  s[["nodes"]][[node_id]] <- rec
  s[["order"]] <- c(s[["order"]], node_id)
  session_emit_sink(rec, phase = if (status == "ok") "done" else status)
  node_id
} # /rtemis::session_add_node


# %% session_report ----
#' End-of-run model-count and failure report
#'
#' Derives all counts from the session graph (so they cannot drift from reality) and
#' prints a structured summary: how many models were trained, broken out, with
#' succeeded/failed and elapsed time. See specs/observability.md section 8.
#'
#' @param session `SupervisedSession` object or NULL.
#' @param verbosity Integer: Verbosity.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
session_report <- function(session, verbosity = 1L) {
  # Diagnostic detail, off by default; shown only at verbosity >= 2.
  if (is.null(session) || verbosity < 2L) {
    return(invisible(NULL))
  }
  events <- session@events
  if (!length(events)) {
    return(invisible(NULL))
  }
  kinds <- vapply(events, function(e) e[["kind"]], character(1L))
  statuses <- vapply(events, function(e) e[["status"]], character(1L))
  count_all <- function(k) sum(kinds == k)
  count_fail <- function(k) {
    sum(kinds == k & statuses %in% c("error", "aborted"))
  }
  n_grid <- count_all("grid_cell")
  n_grid_fail <- count_fail("grid_cell")
  n_final <- count_all("train_alg")
  n_final_fail <- count_fail("train_alg")
  n_outer <- count_all("outer_fold")
  total <- n_grid + n_final
  total_fail <- n_grid_fail + n_final_fail
  dur <- session_format_duration(session@started, session@finished)
  # Recover combos x inner from a tune node, if present.
  tune_idx <- which(kinds == "tune")
  n_combos <- n_inner <- NULL
  if (length(tune_idx)) {
    tmeta <- events[[tune_idx[1L]]][["meta"]]
    n_combos <- tmeta[["n_combos"]]
    n_inner <- tmeta[["n_inner"]]
  }
  msg0("Models trained:", verbosity = verbosity)
  if (!is.null(n_combos) && !is.null(n_inner)) {
    per_fold <- n_combos * n_inner + 1L
    msg0(
      "  tuning:  ",
      n_combos,
      " combos \u2a09 ",
      n_inner,
      " inner resamples = ",
      n_combos * n_inner,
      verbosity = verbosity
    )
    msg0("  + final: 1 per fit = 1", verbosity = verbosity)
    if (n_outer > 1L) {
      msg0(
        "  \u00D7 ",
        n_outer,
        " outer folds = ",
        total,
        " models",
        verbosity = verbosity
      )
    } else {
      msg0("  total = ", per_fold, " models", verbosity = verbosity)
    }
  } else {
    msg0(
      "  ",
      total,
      " models",
      if (n_grid > 0L) paste0(" (", n_grid, " tuning, ", n_final, " final)"),
      verbosity = verbosity
    )
  }
  msg0(
    "  ",
    total - total_fail,
    " succeeded",
    if (total_fail > 0L) paste0(", ", total_fail, " failed"),
    if (!is.null(dur)) paste0(" in ", dur),
    verbosity = verbosity
  )
  invisible(NULL)
} # /rtemis::session_report
