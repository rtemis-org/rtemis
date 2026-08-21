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


# %% resolve_future_plan ----
#' Qualify a future plan name with the package that provides it
#'
#' `ExecutionConfig` names plans as a user would (`"mirai_multisession"`), while
#' `future::plan()` resolves a strategy by name off the search path and cannot
#' see one in a package that is loaded but not attached. Qualifying it here is
#' what makes the mirai plan reachable from a Suggests-gated namespace, and
#' doing it in one place is what keeps every dispatch site reaching the same
#' plan.
#'
#' @param backend Character: Dispatch backend.
#' @param future_plan Optional Character: Plan name from an `ExecutionConfig`.
#'
#' @return Character or NULL: The plan name to hand `progress_plapply()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_future_plan <- function(backend, future_plan) {
  if (
    identical(backend, "future") &&
      identical(future_plan, "mirai_multisession")
  ) {
    return("future.mirai::mirai_multisession")
  }
  future_plan
} # /rtemis::resolve_future_plan


#' Set preferred plan
#'
#' Sets the future plan, in order of precedence:
#' - A plan named by the caller is set as asked.
#' - A non-sequential plan already in place is assumed to be the user's and is respected.
#' - Otherwise the plan is `sequential` for one worker and `multisession` for more.
#'
#' The plan is scoped to `envir` and unwinds with it.
#'
#' @param requested_plan Optional character: Requested plan, one of "multicore", "multisession", "sequential".
#' @param n_workers Optional integer: Number of workers to use.
#' @param envir Environment: Frame the plan is scoped to.
#' @param verbosity Integer: Verbosity level.
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
  # We therefore proceed to set our preferred plan based on the requested n workers.
  # If n_workers was set to 1 and no requested_plan was defined, use sequential
  if (!is.null(n_workers) && n_workers == 1L) {
    with(
      future::plan(strategy = "sequential"),
      local = TRUE,
      envir = envir
    )
    return("sequential")
  }

  # `multisession` on every platform, rather than forking where forking is available.
  # Forking is only safe in a process with one thread and nothing open across the boundary,
  # which a loaded R session -- threaded BLAS, graphics devices, event loops, connections --
  # frequently is not; R's own documentation restricts `mcparallel()` on those grounds. The
  # failure is not graceful: a child can die at the fork, which surfaces as a task that
  # resolves instantly and comes back a `FutureInterruptError`, and it is load-dependent
  # enough to look intermittent. This branch is reached precisely because no plan was
  # requested, so it picks the one that works everywhere; `setup_ExecutionConfig()` defaults
  # the same way, and `"multicore"` stays available to anyone who asks for it by name.
  with(
    future::plan(strategy = "multisession", workers = n_workers),
    local = TRUE,
    envir = envir
  )
  # `future` resolves to a sequential plan when this leaves it a single worker, so report
  # the plan that was actually set rather than the one asked for.
  identify_plan()
} # /set_preferred_plan


# %% warm_workers ----
#' Load \pkg{rtemis} in every worker
#'
#' A worker loads the package when it deserializes its first task, so left alone the cost
#' falls on whichever dispatch happens to be first. Under outer resampling that is the
#' first fold's tuning, where it reads as that fold being slower than the nine after it
#' rather than as setup. Paying it here moves it inside the `worker_pool` node, where it
#' is labeled and measured.
#'
#' @details
#' One task per worker, each holding its worker for a moment, so the scheduler spreads
#' them rather than reusing the one that finished first. The tasks are self-contained --
#' `loadNamespace()` is base -- so nothing is captured from this frame.
#'
#' Failures are swallowed. A pool that cannot be warmed is still a usable pool: the load
#' simply happens on the first real task, exactly as it did before.
#'
#' @param backend Character \{"future", "mirai"\}: Execution backend.
#' @param n_workers Integer [1, Inf): Number of workers to warm.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
warm_workers <- function(backend, n_workers) {
  if (backend == "future") {
    tasks <- lapply(seq_len(n_workers), function(i) {
      future::future(
        {
          loadNamespace("rtemis")
          Sys.sleep(0.05)
          TRUE
        },
        seed = TRUE,
        globals = FALSE
      )
    })
    for (task in tasks) {
      tryCatch(future::value(task), error = function(e) NULL)
    }
  } else {
    tryCatch(
      {
        tasks <- mirai::mirai_map(
          seq_len(n_workers),
          function(i) {
            loadNamespace("rtemis")
            Sys.sleep(0.05)
            TRUE
          }
        )
        mirai::call_mirai(tasks)
      },
      error = function(e) NULL
    )
  }
  invisible(NULL)
} # /rtemis::warm_workers


# %% worker_pool_start ----
#' Start a worker pool for the duration of a run
#'
#' Establishes the run's workers once, so every `progress_plapply()` beneath this frame
#' dispatches onto the same pool instead of building and tearing down its own.
#'
#' @details
#' Standing a pool up costs about a second -- processes spawned, \pkg{rtemis} loaded in
#' each. A dispatcher that pays that per call pays it once per outer fold, because tuning
#' dispatches inside the fold loop, and on a short grid the setup outweighs what the
#' parallelism saves.
#'
#' The whole of that cost, spawning and loading both, is recorded as one `worker_pool`
#' node so it appears in the execution graph as setup rather than inflating the first
#' thing that dispatches.
#'
#' The pool is recorded in `live[["worker_pool"]]`, which is what
#' `worker_pool_available()` reads and what makes a second call here a no-op: an outer
#' fold recurses into `train()`, which reaches this function again with the same config.
#'
#' Teardown differs by backend and is why `worker_pool_stop()` is not symmetric with this
#' function. A future plan is scoped to `envir` and unwinds with that frame on its own; a
#' mirai pool is global state that has to be released explicitly.
#'
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#' @param n_workers Integer [1, Inf): Pool size. A value of 1 starts no pool.
#' @param future_plan Optional Character: Future plan, when `backend` is `"future"`.
#' @param warm Logical: If TRUE, load \pkg{rtemis} in every worker before returning, so
#' the whole setup cost falls inside this node. FALSE leaves each worker to load on its
#' first task, which is cheaper overall by about one dispatch and charges the difference
#' to whatever dispatches first. Exposed so the two can be measured against each other.
#' @param envir Environment: Frame the future plan is scoped to.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Logical: TRUE if this call started the pool, and must therefore stop it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
worker_pool_start <- function(
  backend,
  n_workers,
  future_plan = NULL,
  warm = TRUE,
  envir = parent.frame(),
  verbosity = 1L
) {
  check_pos_integer_scalar(n_workers)
  if (backend == "none" || n_workers == 1L) {
    return(FALSE)
  }
  if (!is.null(live[["worker_pool"]])) {
    return(FALSE)
  }
  t_start <- Sys.time()
  if (backend == "future") {
    # Left to `progress_plapply()`, which raises on it: a plan that runs one worker is not
    # a pool, and reporting it as one here would hide the error behind a quieter symptom.
    if (identical(future_plan, "sequential")) {
      return(FALSE)
    }
    check_dependencies("future")
    if (!is.null(future_plan) && startsWith(future_plan, "future.mirai::")) {
      check_dependencies("future.mirai")
    }
    plan_set <- set_preferred_plan(
      requested_plan = future_plan,
      n_workers = n_workers,
      envir = envir,
      verbosity = verbosity
    )
    # `future` resolves a single-worker request to a sequential plan, so the plan that was
    # set is not always the one asked for.
    if (identical(plan_set, "sequential")) {
      return(FALSE)
    }
  } else {
    check_dependencies("mirai")
    mirai::daemons(n_workers, dispatcher = TRUE)
  }
  # Spawning the processes is only half the cost; the other half is each one loading
  # rtemis, which it does on its first task. Both belong to setup, so both are paid and
  # timed here rather than one of them landing in the first fold that tunes.
  if (warm) {
    warm_workers(backend, n_workers)
  }
  live[["worker_pool"]] <- list(backend = backend, n_workers = n_workers)
  session_add_node(
    "worker_pool",
    label = paste0(n_workers, " ", backend),
    meta = list(
      backend = backend,
      n_workers = n_workers,
      future_plan = future_plan
    ),
    t_start = t_start,
    t_end = Sys.time()
  )
  if (verbosity > 0L) {
    msg0(
      "Started ",
      highlight(n_workers),
      ngettext(n_workers, " worker", " workers"),
      " (",
      bold(backend),
      ") for this run."
    )
  }
  TRUE
} # /rtemis::worker_pool_start


# %% worker_pool_stop ----
#' Release the run's worker pool
#'
#' Called only by the frame whose `worker_pool_start()` returned TRUE. A future plan
#' unwinds with the frame it was scoped to, so only the marker is cleared for that
#' backend; mirai daemons are global and are shut down here.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
worker_pool_stop <- function() {
  pool <- live[["worker_pool"]]
  if (is.null(pool)) {
    return(invisible(NULL))
  }
  live[["worker_pool"]] <- NULL
  if (identical(pool[["backend"]], "mirai")) {
    mirai::daemons(0L)
  }
  invisible(NULL)
} # /rtemis::worker_pool_stop


# %% worker_pool_available ----
#' Is there a run-level pool this dispatch can use?
#'
#' The backend must match, because borrowing is the decision to skip this dispatch's own
#' setup: a future dispatch onto mirai daemons would find no plan set, and a mirai
#' dispatch under a future plan would start daemons that then shut down the plan's own.
#'
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
worker_pool_available <- function(backend) {
  pool <- live[["worker_pool"]]
  !is.null(pool) && identical(pool[["backend"]], backend)
} # /rtemis::worker_pool_available


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
#' `"auto"` is best-effort: it shares wherever it can and returns the object untouched
#' wherever it cannot, including when `mori::share()` itself fails. `"always"` shares
#' even under `backend = "none"`, which is what allows a run to be compared against its
#' own shared counterpart, and raises when the request cannot be honored at all.
#'
#' Silent by design: the policy is stated once per run by `report_shared_memory()`, not
#' once per payload per dispatch. `share_decision()` holds the policy both consult.
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
  n_workers = 1L
) {
  if (identical(mode, "none") || is.null(obj)) {
    return(obj)
  }
  if (identical(mode, "always")) {
    if (!workers_are_local(backend, future_plan)) {
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
  if (!share_decision(obj, mode, backend, future_plan, n_workers)[["share"]]) {
    return(obj)
  }
  # "auto" is best-effort, and as the default policy it is on the path of every parallel
  # run, so a failure here degrades to the ordinary transport rather than ending the fit.
  # Warned once per run, because a failure is a defect rather than a policy decision and
  # a run that quietly stopped sharing is a run whose memory ceiling moved: the caller
  # shares three payloads per dispatch and tuning dispatches once per outer fold, so
  # warning at each would bury the first one.
  tryCatch(mori::share(obj), error = function(e) {
    if (is.null(live[["share_warned"]])) {
      live[["share_warned"]] <- TRUE
      rtemis.core::warn(
        "Could not place a worker payload in shared memory: ",
        conditionMessage(e),
        "\nContinuing without it; each task receives its own copy."
      )
    }
    obj
  })
} # /rtemis::share_payload


# %% format_bytes ----
#' Human-readable byte count
#'
#' SI units, so a megabyte reads back as "1 MB" rather than as the 976.6 Kb the same
#' number of bytes is in binary units.
#'
#' @param bytes Numeric: Size in bytes.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_bytes <- function(bytes) {
  format(
    structure(bytes, class = "object_size"),
    units = "auto",
    standard = "SI"
  )
} # /rtemis::format_bytes


# %% share_decision ----
#' Will this payload be shared, and if not why not
#'
#' The single statement of the sharing policy. `share_payload()` acts on it and
#' `report_shared_memory()` describes it, so what a run reports cannot drift from what
#' it does.
#'
#' @details
#' Conditions are ordered so the reason given is the first one that applies, which is
#' also the most specific: "not asked to" before "cannot". There is no "not worth it":
#' the transport is cheaper at every size measured, so anything that can be shared is.
#'
#' `"always"` reports as sharing. Its one impossible case -- workers that are not on this
#' machine -- is an error rather than a decision, and is raised by `share_payload()`.
#'
#' @param obj Object to share, or NULL.
#' @param mode Character \{"none", "auto", "always"\}: Sharing policy.
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#' @param future_plan Optional Character: Future plan, when `backend` is `"future"`.
#' @param n_workers Integer [1, Inf): Number of workers.
#'
#' @return List with `share` (logical), `reason` (character, NULL when sharing) and
#' `bytes` (numeric).
#'
#' @author EDG
#' @keywords internal
#' @noRd
share_decision <- function(obj, mode, backend, future_plan, n_workers) {
  # Measured before sharing: `object.size()` does not understand ALTREP and reports an
  # already-shared object at its full nominal size.
  bytes <- if (is.null(obj)) 0 else as.numeric(utils::object.size(obj))
  no <- function(reason) list(share = FALSE, reason = reason, bytes = bytes)
  if (identical(mode, "none")) {
    return(no("shared_memory is \"none\""))
  }
  if (is.null(obj)) {
    return(no("there is nothing to share"))
  }
  if (!workers_are_local(backend, future_plan)) {
    return(no("the workers are not on this machine"))
  }
  if (identical(mode, "always")) {
    return(list(share = TRUE, reason = NULL, bytes = bytes))
  }
  if (backend == "none" || n_workers == 1L) {
    return(no("this run transfers nothing to workers"))
  }
  if (identical(future_plan, "multicore")) {
    return(no("forked workers already share memory"))
  }
  if (!requireNamespace("mori", quietly = TRUE)) {
    return(no("the mori package is not installed"))
  }
  # No size test. Measured on a 190 x 117 frame (~190 kB), sharing costs 0.06 ms once and
  # 0.17 ms per task in slower slicing, against 0.30 ms per task saved on serializing and
  # unserializing it -- a net gain before the transport those bytes would also have to
  # cross. The fixed cost a threshold would be avoiding is not there to avoid.
  list(share = TRUE, reason = NULL, bytes = bytes)
} # /rtemis::share_decision


# %% report_shared_memory ----
#' Report what a run will do about shared memory
#'
#' Reported once per run, from `train()`, and only when sharing was asked for: a run left
#' on the default policy has nothing to say about a feature it is not using.
#'
#' @details
#' The individual `share_payload()` calls are silent. There are three payloads per
#' dispatch site and tuning dispatches once per outer fold, so a message at each would be
#' thirty lines on a ten-fold run, all of them saying the same thing.
#'
#' @param obj Object the report is about: the training data.
#' @param mode Character \{"none", "auto", "always"\}: Sharing policy.
#' @param backend Character \{"none", "future", "mirai"\}: Execution backend.
#' @param future_plan Optional Character: Future plan, when `backend` is `"future"`.
#' @param n_workers Integer [1, Inf): Number of workers.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
report_shared_memory <- function(
  obj,
  mode,
  backend,
  future_plan,
  n_workers,
  verbosity = 1L
) {
  if (identical(mode, "none") || verbosity < 1L) {
    return(invisible(NULL))
  }
  decision <- share_decision(obj, mode, backend, future_plan, n_workers)
  size <- format_bytes(decision[["bytes"]])
  if (decision[["share"]]) {
    msg0(
      "Shared memory (",
      bold(mode),
      "): training data (",
      highlight(size),
      ") shared across ",
      highlight(n_workers),
      ngettext(n_workers, " worker", " workers"),
      "."
    )
  } else {
    msg0(
      "Shared memory (",
      bold(mode),
      "): not sharing training data (",
      highlight(size),
      ") -- ",
      decision[["reason"]],
      "."
    )
  }
  invisible(NULL)
} # /rtemis::report_shared_memory


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
#' frame to every worker -- the dispatcher's own bookkeeping, task handles and progress
#' handle included, none of which a task needs and some of which cannot be serialized.
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
#' **Workers are built here only if nobody built them first.** With a run-level pool
#' standing (`worker_pool_start()`), this dispatches onto it and neither starts nor stops
#' anything; without one it stands up its own workers for the call and releases them on
#' the way out. Either way `n_workers` governs the submission window.
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
  # The execution-graph node this loop runs inside, so its progress events graft onto
  # the graph rather than reporting as a second root. Read once, here: the stack top
  # moves as the loop body enters nodes of its own.
  parent_node <- session_current_node()
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
  # A run-level pool, if one is standing, is dispatched onto rather than replaced. Read
  # before the task body so both backend branches and the teardown agree on one answer.
  borrowed <- worker_pool_available(backend)

  # Task body ----
  run_one <- make_task_runner(X, FUN, seeds)

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
          res <- run_one(.index, ...)
          if (stop_on_error && inherits(res, "condition")) {
            stop(res)
          }
          res
        },
        ...,
        label = label,
        kind = kind,
        parent_id = parent_node,
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
      if (!borrowed) {
        future_plan <- set_preferred_plan(
          requested_plan = future_plan,
          n_workers = n_workers,
          envir = environment(),
          verbosity = verbosity
        )
      }
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
      # Leaving early must not abandon work in flight, whoever owns the workers. A plan
      # this call set is scoped to this frame and is torn down on the way out, which
      # interrupts anything still running; a borrowed pool instead outlives the call, and
      # a task still occupying a worker delays -- or returns its value to -- the next
      # dispatch onto that same pool. Either way the damage lands on whatever runs next,
      # as a `FutureInterruptError` in place of its value. Outstanding futures are
      # therefore canceled explicitly before unwinding, rather than abandoned.
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
      if (!borrowed) {
        mirai::daemons(n_workers, dispatcher = TRUE)
        on.exit(mirai::daemons(0L), add = TRUE)
      }
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
      # owns, so nothing outstanding needs collecting first. A borrowed pool is not shut
      # down at all, and mirai discards the results of a map whose handle goes out of
      # scope, so there is likewise nothing to collect.
      reap <- function() invisible(NULL)
    }

    # Drain ----
    handle <- progress_begin(
      n,
      label = label,
      kind = kind,
      parent_id = parent_node,
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
