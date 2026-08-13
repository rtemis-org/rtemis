# utils_async.R
# ::rtemis::
# 2026- EDG rtemis.org

# Define allowed future plans
ALLOWED_PLANS <- c(
  "sequential",
  "multicore",
  "multisession",
  "cluster",
  "remote",
  "transparent",
  "future.mirai::mirai_multisession", # what user sets
  "mirai_multisession" # what future::plan() returns
)


#' Check if system is Windows
#'
#' @return Logical: TRUE if Windows, FALSE otherwise
#' @noRd
is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
} # /is_windows


#' Identify future plan
#'
#' @return Character: Name of current plan
#'
#' @noRd
identify_plan <- function(x = NULL) {
  if (is.null(x)) {
    x <- future::plan()
  }
  for (p in ALLOWED_PLANS) {
    if (inherits(x, p)) {
      return(p)
    }
  }
  rtemis.core::abort(
    "Detected future plan not in allowed plans (",
    paste(ALLOWED_PLANS, collapse = ", "),
    "). Detected plan class: ",
    paste(class(x), collapse = "/"),
    ".",
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /rtemis::identify_plan


#' Set preferred plan
#'
#' Sets the future plan according to system and user preference:
#' - Check whether a plan has been set by the user
#' - Check whether there is an option set for future plan
#' - Check available cores
#' - Check if Windows
#'
#' @param requested_plan Optional character: Requested plan, one of "multicore", "multisession", "sequential".
#' @param n_workers Optional integer: Number of workers to use.
#'
#' @return Character: Name of plan set
#'
#' @author EDG
#' @keywords internal
#' @noRd
set_preferred_plan <- function(
  requested_plan = NULL,
  n_workers = NULL,
  envir = parent.frame(),
  verbosity = 1L
) {
  # If user has requested a specific plan, try to set it
  if (!is.null(requested_plan)) {
    # Security check
    if (!requested_plan %in% ALLOWED_PLANS) {
      rtemis.core::abort(
        "Requested plan '",
        requested_plan,
        "' is not one of allowed plans: ",
        paste(ALLOWED_PLANS, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    # future::plan will determine workers if NULL & will set to sequential if only 1 core available
    # therefore plan set by following call is not always the requested one and needs to be
    # determined.

    if (requested_plan == "sequential") {
      with(
        future::plan(strategy = requested_plan),
        local = TRUE,
        envir = envir
      )
    } else {
      with(
        future::plan(strategy = requested_plan, workers = n_workers),
        local = TRUE,
        envir = envir
      )
    }

    return(identify_plan())
  }

  # If user has not requested a specific plan, check if they have set one
  current_plan <- future::plan()

  # If the plan is not sequential, we must assume user set it and respect it (though it might
  # have been set by a different package)
  if (!inherits(current_plan, "sequential")) {
    return(identify_plan(current_plan))
  }
  # If the plan is sequential, we can't currently tell if it was set by the user or is default
  # -> Ideally, we would know this. <-
  # We therefore proceed to set our preferred plan based on OS, n available cores, and requested
  # n workers.
  # If n_workers was set to 1 and no requested_plan was defined, use sequential
  if (!is.null(n_workers) && n_workers == 1L) {
    with(
      future::plan(strategy = "sequential"),
      local = TRUE,
      envir = envir
    )
    return("sequential")
  }

  if (is_windows()) {
    # On Windows, multicore is not available
    preferred_plan <- "multisession"
  } else {
    preferred_plan <- "multicore"
  }
  with(
    future::plan(strategy = preferred_plan, workers = n_workers),
    local = TRUE,
    envir = envir
  )
  # This will still be sequential and not "preferred_plan" if n_workers = 1
  identify_plan()
} # /set_preferred_plan


# Payload below which sharing is not worth its fixed cost. The saving is this size times
# the task count, so the break-even is well under a megabyte; the threshold is set here
# rather than lower to keep small runs entirely on the ordinary path.
SHARE_MIN_BYTES <- 1e6


# Future plans that are known to place workers on this machine. `cluster` is absent
# deliberately: it accepts remote hostnames and its name alone does not say which, so it
# cannot be treated as local without inspecting the plan's workers.
LOCAL_PLANS <- c(
  "sequential",
  "multicore",
  "multisession",
  "transparent",
  "future.mirai::mirai_multisession",
  "mirai_multisession"
)


# %% workers_are_local ----
#' Are this run's workers on this machine?
#'
#' Shared memory is local RAM, so it only applies when every worker can map the same
#' physical pages.
#'
#' @details
#' The mirai backend is always local here: `progress_plapply()` starts its own daemons
#' with `mirai::daemons(n_workers)` and never connects to remote ones. For the future
#' backend the answer comes from the plan name; anything not in `LOCAL_PLANS` -- `remote`,
#' and `cluster`, which may or may not be -- counts as not local.
#'
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#' @param future_plan Optional Character: Future plan, when `backend` is `"future"`.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
workers_are_local <- function(backend, future_plan) {
  if (backend != "future") {
    return(TRUE)
  }
  !is.null(future_plan) && future_plan %in% LOCAL_PLANS
} # /rtemis::workers_are_local


# %% share_payload ----
#' Place a worker payload in shared memory
#'
#' Every parallel task receives the same training data and slices its own rows from it,
#' so without sharing the data is serialized once per task -- 9 GB for a 96 x 5 grid over
#' a 19 MB training set. `mori::share()` replaces those copies with one shared-memory
#' region that every worker maps, transmitting a name of about thirty bytes in place of
#' the contents.
#'
#' @details
#' The returned wrapper reads directly from the shared region and behaves like the
#' original object, so callers index it exactly as before; a worker slicing rows
#' materializes only its own subset. Regions are mapped read-only, so a worker cannot
#' corrupt what the others are reading.
#'
#' `"auto"` is best-effort and returns the object untouched whenever sharing would not
#' help or cannot work. `"always"` shares regardless of size -- including under
#' `backend = "none"`, which is what allows a run to be compared against its own shared
#' counterpart -- and raises when the request cannot be honored at all.
#'
#' **The caller must keep the returned value alive until the workers have mapped it.**
#' Both call sites do so by capturing it in the task-runner factory's frame, which lives
#' until `progress_plapply()` has collected every task.
#'
#' @param obj Object to share. `NULL` passes through untouched.
#' @param mode Character \{"none", "auto", "always"\}: Sharing policy.
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#' @param future_plan Optional Character: Future plan, when `backend` is `"future"`.
#' @param n_workers Integer [1, Inf): Number of workers.
#' @param label Character: What this payload is, for the skip message.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `obj`, or a \pkg{mori} shared wrapper around it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
share_payload <- function(
  obj,
  mode = "none",
  backend = "none",
  future_plan = NULL,
  n_workers = 1L,
  label = "payload",
  verbosity = 1L
) {
  if (identical(mode, "none") || is.null(obj)) {
    return(obj)
  }
  local_workers <- workers_are_local(backend, future_plan)
  if (identical(mode, "always")) {
    if (!local_workers) {
      rtemis.core::abort(
        "shared_memory = \"always\" but this run's workers are not known to be on ",
        "this machine (future plan: ",
        future_plan %||% "unset",
        "). Shared memory is local RAM. Use shared_memory = \"auto\" to share ",
        "wherever it is possible, or \"none\" to disable it.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    check_dependencies("mori")
    return(mori::share(obj))
  }
  # "auto" from here: every remaining condition is a reason not to share, never an error.
  if (!local_workers) {
    msg0(
      "Not sharing ",
      label,
      ": workers are not on this machine.",
      verbosity = verbosity
    )
    return(obj)
  }
  if (backend == "none" || n_workers == 1L) {
    # Sequential execution transfers nothing, so there is nothing to save.
    return(obj)
  }
  if (identical(future_plan, "multicore")) {
    # Forked workers already share these pages copy-on-write.
    msg0(
      "Not sharing ",
      label,
      ": forked workers already share memory.",
      verbosity = verbosity
    )
    return(obj)
  }
  if (!requireNamespace("mori", quietly = TRUE)) {
    msg0(
      "Not sharing ",
      label,
      ": the mori package is not installed.",
      verbosity = verbosity
    )
    return(obj)
  }
  # Measured before sharing: `object.size()` does not understand ALTREP and reports an
  # already-shared object at its full nominal size.
  if (as.numeric(utils::object.size(obj)) < SHARE_MIN_BYTES) {
    # Gated a level above the other skip reasons: a small payload is the ordinary case, and
    # a caller shares several per run, so at verbosity 1 this would be the noisiest line of
    # a run that did nothing unusual.
    if (verbosity > 1L) {
      msg0(
        "Not sharing ",
        label,
        ": below the size threshold.",
        verbosity = verbosity
      )
    }
    return(obj)
  }
  mori::share(obj)
} # /rtemis::share_payload


# %% with_preserved_rng ----
#' Evaluate an expression without disturbing the caller's RNG
#'
#' Restores both the generator kind and `.Random.seed` after `expr`, so machinery that
#' draws random numbers -- or switches generators to do so -- cannot shift the stream the
#' actual computation runs on.
#'
#' @details
#' The kind is restored explicitly rather than left to `.Random.seed[1L]`, which encodes
#' it. Two reasons: with no prior `.Random.seed` there is no vector to encode anything, so
#' a generator switch inside `expr` would persist; and `RNGkind()` reports a cached value
#' that is only reconciled with `.Random.seed` on the next draw, so a caller inspecting it
#' in between would be told the wrong generator is active.
#'
#' Restoring the kind reinitializes `.Random.seed`, so the seed is restored after it.
#' When the caller had no `.Random.seed` at all, the one `expr` forced into existence is
#' removed again, leaving the next RNG use to initialize from the clock as it would have.
#'
#' @param expr Expression: Evaluated for its value.
#'
#' @return The value of `expr`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
with_preserved_rng <- function(expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  old_kind <- RNGkind()
  on.exit(
    {
      # `sample.kind = "Rounding"` is deprecated and warns when set; a caller already on
      # it should not be scolded for our round-trip.
      suppressWarnings(RNGkind(
        kind = old_kind[1L],
        normal.kind = old_kind[2L],
        sample.kind = old_kind[3L]
      ))
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )
  expr
} # /rtemis::with_preserved_rng


# %% rng_substreams ----
#' Derive independent RNG substreams
#'
#' Derives `n` L'Ecuyer-CMRG substreams from a single master seed. Substreams are
#' provably non-overlapping, unlike the `seed + i` construction, whose nearby seeds can
#' produce correlated streams.
#'
#' Assigning one substream per task **by task index** is what makes a parallel run
#' reproducible and identical to the sequential one: the stream a task gets depends on
#' its position, never on which worker picked it up or in what order it finished.
#'
#' The caller's `.Random.seed` is restored on exit, so deriving streams does not consume
#' the calling session's RNG.
#'
#' @param seed Optional Integer: Master seed. `NULL` returns `NULL` (no seeding).
#' @param n Integer [1, Inf): Number of substreams.
#'
#' @return List of `n` `.Random.seed` vectors, or `NULL` if `seed` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rng_substreams <- function(seed, n) {
  if (is.null(seed)) {
    return(NULL)
  }
  check_pos_integer_scalar(n)
  with_preserved_rng({
    set.seed(clean_int(seed), kind = "L'Ecuyer-CMRG")
    out <- vector("list", n)
    stream <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    for (i in seq_len(n)) {
      out[[i]] <- stream
      stream <- parallel::nextRNGStream(stream)
    }
    out
  })
} # /rtemis::rng_substreams


# %% rng_set_substream ----
#' Activate an RNG substream
#'
#' Installs a substream from `rng_substreams()` as the session's RNG state. Assigning
#' `.Random.seed` directly is the mechanism `parallel::clusterSetRNGStream()` uses;
#' calling `RNGkind()` first would reset the seed instead of adopting it.
#'
#' @param stream Optional Integer vector: A `.Random.seed` vector. `NULL` is a no-op.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rng_set_substream <- function(stream) {
  if (!is.null(stream)) {
    assign(".Random.seed", stream, envir = globalenv())
  }
  invisible(NULL)
} # /rtemis::rng_set_substream


# %% make_task_runner ----
#' Build the per-task body dispatched to workers
#'
#' Returns a closure that runs element `.index` of `X`, first installing that index's RNG
#' substream when one was supplied.
#'
#' Serializing a closure walks its enclosing environments, so the body is built here, in
#' a factory whose frame holds only `X`, `FUN`, and `seeds` and whose parent is the
#' \pkg{rtemis} namespace. Defining it inside `progress_plapply()` would ship that whole
#' frame to every worker -- including the `progress` callback, which may hold an open
#' socket and need not be serializable at all.
#'
#' Errors are captured and returned rather than raised, so every backend reports task
#' failure the same way.
#'
#' @param X Vector or list: Elements to iterate over.
#' @param FUN Function: Applied to one element of `X`.
#' @param seeds Optional List: One RNG substream per element of `X`.
#'
#' @return Function of `(.index, ...)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_task_runner <- function(X, FUN, seeds) {
  force(X)
  force(FUN)
  force(seeds)
  function(.index, ...) {
    if (!is.null(seeds)) {
      rng_set_substream(seeds[[.index]])
    }
    tryCatch(FUN(X[[.index]], ...), error = function(e) e)
  }
} # /rtemis::make_task_runner


# %% progress_plapply ----
#' Parallel lapply with rtemis progress reporting
#'
#' A parallel-capable counterpart to [rtemis.core::progress_lapply()]: applies `FUN`
#' over `X` sequentially or across `future`/`mirai` workers, rendering the same rtemis
#' progress line and emitting the same sink envelopes in every case.
#'
#' It lives in \pkg{rtemis} rather than \pkg{rtemis.core} because the backends pull in
#' `future`/`mirai`, which \pkg{rtemis.core} must not depend on.
#'
#' @details
#' **`FUN` must be self-contained.** Everything it needs arrives through `...` or its own
#' enclosing environment, both of which are serialized to workers. Nothing is captured
#' from the caller's frame.
#'
#' **Errors never propagate out of a task.** A condition raised by `FUN` is returned as
#' that element's value, uniformly across backends -- `future::value()` would otherwise
#' re-raise while `mirai` returns an error object. The caller owns the failure policy and
#' inspects results with `inherits(res, "condition")`. Callers whose policy is fatal set
#' `stop_on_error`, which re-raises the first failure instead of finishing work whose
#' results are about to be discarded.
#'
#' **Results are always in the order of `X`**, whatever order tasks completed in.
#'
#' Both parallel backends drain through one loop: block on task `j` in index order, then
#' recount resolved tasks, so the counter stays accurate when tasks finish out of order.
#' Waiting is event-driven -- no polling interval to tune.
#'
#' @param X Vector or list: Elements to iterate over, as in [lapply()].
#' @param FUN Function: Applied to each element of `X`.
#' @param ... Additional arguments passed to `FUN`.
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend. `"none"`
#' delegates to [rtemis.core::progress_lapply()].
#' @param n_workers Integer [1, Inf): Number of parallel workers. A value of 1 runs
#' sequentially whatever `backend` says.
#' @param future_plan Optional Character: Future plan, used when `backend` is `"future"`.
#' @param seeds Optional List: One RNG substream per element of `X`, from
#' `rng_substreams()`. Applied by index, so results do not depend on backend,
#' `n_workers`, or completion order.
#' @param label Character: Display label for the progress node.
#' @param kind Character: Node kind forwarded in the sink envelope.
#' @param progress Optional Function: Callback invoked as
#' `progress(stage, current, total, message)`. Sequential runs fire it before each
#' element; parallel runs fire it on each completion, with `current` counting completed
#' tasks and `message` naming the tasks still in flight.
#' @param stage Character: `stage` value passed to `progress`.
#' @param stop_on_error Logical: If TRUE, re-raise the first task failure instead of
#' returning it, abandoning any tasks still in flight.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List of `length(X)` results, in the order of `X`, names preserved.
#'
#' @author EDG
#' @keywords internal
#' @noRd
progress_plapply <- function(
  X,
  FUN,
  ...,
  backend = "none",
  n_workers = 1L,
  future_plan = NULL,
  seeds = NULL,
  label = "Processing",
  kind = "progress",
  progress = NULL,
  stage = NULL,
  stop_on_error = FALSE,
  verbosity = 1L
) {
  # Input validation ----
  FUN <- match.fun(FUN)
  backend <- match.arg(backend, c("none", "future", "mirai"))
  check_pos_integer_scalar(n_workers)
  check_character(future_plan, allow_null = TRUE)
  if (!is.null(seeds) && length(seeds) != length(X)) {
    rtemis.core::abort(
      "`seeds` must have one substream per element of `X`: got ",
      length(seeds),
      " for ",
      length(X),
      " elements.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (!is.null(progress) && !is.function(progress)) {
    rtemis.core::abort(
      "`progress` must be a function or NULL.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(stage)) {
    stage <- kind
  }
  n <- length(X)
  if (n == 0L) {
    return(list())
  }

  # A single worker has nothing to gain from a backend and everything to lose in
  # serialization, so it takes the sequential path regardless of what was requested.
  if (n_workers == 1L) {
    backend <- "none"
  }
  if (backend == "future") {
    check_dependencies("future")
    if (!is.null(future_plan) && startsWith(future_plan, "future.mirai::")) {
      check_dependencies("future.mirai")
    }
    if (identical(future_plan, "sequential")) {
      rtemis.core::abort(
        "Requested the 'sequential' future plan, which runs 1 worker, but ",
        n_workers,
        " workers were requested. Use backend = \"none\" for sequential execution.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  } else if (backend == "mirai") {
    check_dependencies("mirai")
  }

  # Task body ----
  run_one <- make_task_runner(X, FUN, seeds)

  # Progress callback ----
  # Errors raised by the callback are swallowed: a broken sink must not interrupt the
  # work it is only reporting on.
  notify <- function(current, message) {
    if (is.null(progress)) {
      return(invisible(NULL))
    }
    tryCatch(
      progress(
        stage = stage,
        current = current,
        total = n,
        message = message
      ),
      error = function(e) NULL
    )
    invisible(NULL)
  }

  # Execution ----
  # Wrapped so a seeded run leaves the caller's RNG exactly as it found it: the run's
  # randomness comes entirely from its own substreams, which means calling the same
  # seeded run twice gives the same answer without re-seeding in between. Unseeded runs
  # are left alone, keeping the ordinary R behavior of advancing the caller's stream.
  execute <- function() {
    # Sequential ----
    if (backend == "none") {
      out <- progress_lapply(
        seq_len(n),
        function(.index, ...) {
          notify(.index, paste0(label, " ", .index, "/", n))
          res <- run_one(.index, ...)
          if (stop_on_error && inherits(res, "condition")) {
            stop(res)
          }
          res
        },
        ...,
        label = label,
        kind = kind,
        verbosity = verbosity,
        package = "rtemis"
      )
      names(out) <- names(X)
      return(out)
    }

    # Parallel ----
    if (verbosity > 0L) {
      msg0(
        "Dispatching ",
        highlight(n),
        ngettext(n, " task to ", " tasks to "),
        highlight(n_workers),
        ngettext(n_workers, " worker", " workers"),
        " (",
        bold(backend),
        ")."
      )
    }
    if (backend == "future") {
      future_plan <- set_preferred_plan(
        requested_plan = future_plan,
        n_workers = n_workers,
        envir = environment(),
        verbosity = verbosity
      )
      dots <- list(...)
      tasks <- vector("list", n)
      submitted <- 0L
      # `future()` blocks when every worker is busy, so tasks are submitted in a window
      # of `n_workers` rather than all at once: submitting all n up front would block
      # inside dispatch and freeze the progress line until the last was accepted.
      submit_upto <- function(k) {
        while (submitted < k) {
          submitted <<- submitted + 1L
          index <- submitted
          tasks[[index]] <<- future::future(
            do.call(run_one, c(list(index), dots)),
            # `seed = TRUE` hands future its own L'Ecuyer stream, which silences its RNG
            # check; `run_one` then installs ours over it when `seeds` is supplied.
            seed = TRUE,
            globals = list(run_one = run_one, index = index, dots = dots)
          )
        }
        invisible(NULL)
      }
      submit_upto(min(n_workers, n))
      # Only tasks submitted but not yet collected can change state: below `from` the drain
      # loop already holds the value, above `submitted` nothing has been dispatched. That
      # window is `n_workers` wide, so drain accounting stays linear in the task count.
      resolved_tasks <- function(from) {
        done <- logical(n)
        done[seq_len(from)] <- TRUE
        for (k in seq_len(submitted - from)) {
          done[from + k] <- future::resolved(tasks[[from + k]])
        }
        done
      }
      await <- function(j) {
        submit_upto(j)
        value <- tryCatch(future::value(tasks[[j]]), error = function(e) e)
        # Task j is done, so a worker is free: refill the window without blocking.
        submit_upto(min(n, j + n_workers))
        value
      }
      # `set_preferred_plan()` scopes the plan to this frame, so leaving early tears it
      # down. Tearing it down with futures still running interrupts them, and the damage
      # lands on whatever runs next: a later, unrelated call gets back a
      # `FutureInterruptError` in place of its value. Outstanding futures are therefore
      # canceled explicitly before unwinding, rather than abandoned.
      # Canceling is only half of it: an interrupted forked worker stays in
      # `parallel:::children()` until someone takes its value, and future's core accounting
      # counts that orphan as a process it cannot attribute to any future -- it warns, and
      # undercounts the cores left, until a later multicore run has a task handed back as
      # interrupted. Cancel every task first so they wind down concurrently, then collect,
      # with `signal = FALSE` so the interrupts are not re-raised here on top of the
      # caller's own error.
      reap <- function() {
        for (k in seq_len(submitted)) {
          tryCatch(future::cancel(tasks[[k]]), error = function(e) NULL)
        }
        for (k in seq_len(submitted)) {
          tryCatch(
            future::value(tasks[[k]], signal = FALSE),
            error = function(e) NULL
          )
        }
        invisible(NULL)
      }
    } else {
      mirai::daemons(n_workers, dispatcher = TRUE)
      on.exit(mirai::daemons(0L), add = TRUE)
      tasks <- mirai::mirai_map(
        .x = seq_len(n),
        .f = run_one,
        .args = list(...)
      )
      # `mirai_map()` submits every task up front, so unlike the future backend there is no
      # submission window to bound the scan; skipping the collected prefix is what keeps it
      # from re-reading values the drain loop already holds.
      resolved_tasks <- function(from) {
        done <- logical(n)
        done[seq_len(from)] <- TRUE
        for (k in seq_len(n - from)) {
          done[from + k] <- !mirai::unresolved(tasks[[from + k]])
        }
        done
      }
      await <- function(j) {
        mirai::call_mirai(tasks[[j]])
        value <- tasks[[j]][["data"]]
        # mirai reports a worker-side failure as a `miraiError`; normalize it to a plain
        # condition so callers can test every backend's failures the same way.
        if (inherits(value, "miraiError")) {
          simpleError(paste(as.character(value), collapse = "\n"))
        } else {
          value
        }
      }
      # `mirai::daemons(0L)` on the way out is an orderly shutdown of daemons this call
      # owns, so nothing outstanding needs collecting first.
      reap <- function() invisible(NULL)
    }

    # Drain ----
    handle <- progress_begin(
      n,
      label = label,
      kind = kind,
      verbosity = verbosity,
      package = "rtemis"
    )
    drained <- FALSE
    on.exit(
      if (!drained) {
        progress_end(handle, status = "error")
      },
      add = TRUE
    )
    out <- vector("list", n)
    for (j in seq_len(n)) {
      out[[j]] <- await(j)
      if (stop_on_error && inherits(out[[j]], "condition")) {
        reap()
        stop(out[[j]])
      }
      done <- resolved_tasks(j)
      n_done <- sum(done)
      running <- which(!done)
      progress_update(
        handle,
        current = n_done,
        label = running_label(label, running)
      )
      notify(
        n_done,
        paste0(
          label,
          " ",
          n_done,
          "/",
          n,
          " complete",
          if (length(running)) {
            paste0("; running ", format_task_ids(running))
          } else {
            ""
          }
        )
      )
    }
    drained <- TRUE
    progress_end(handle, status = "done")
    names(out) <- names(X)
    out
  }

  if (is.null(seeds)) {
    execute()
  } else {
    with_preserved_rng(execute())
  }
} # /rtemis::progress_plapply


# %% format_task_ids ----
#' Format a set of in-flight task indices
#'
#' @param ids Integer vector: Task indices.
#' @param max_shown Integer: Ids listed before truncating.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_task_ids <- function(ids, max_shown = 6L) {
  if (length(ids) > max_shown) {
    paste0(paste(ids[seq_len(max_shown)], collapse = ", "), ", ...")
  } else {
    paste(ids, collapse = ", ")
  }
} # /rtemis::format_task_ids


# %% running_label ----
#' Progress label naming the in-flight tasks
#'
#' @param label Character: Base label.
#' @param running Integer vector: Indices of tasks still in flight.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
running_label <- function(label, running) {
  if (length(running) == 0L) {
    label
  } else {
    paste0(label, " [running ", format_task_ids(running), "]")
  }
} # /rtemis::running_label
