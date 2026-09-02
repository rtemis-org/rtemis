# test_parallel_outer.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# Parallel execution of the outer resampling loop: the dispatcher (`progress_plapply`),
# the RNG substreams that make a parallel run reproduce its sequential counterpart, and
# the session grafting that keeps the execution graph the same shape either way.

progress_plapply <- getFromNamespace("progress_plapply", "rtemis")
rng_substreams <- getFromNamespace("rng_substreams", "rtemis")
with_preserved_rng <- getFromNamespace("with_preserved_rng", "rtemis")
set_preferred_plan <- getFromNamespace("set_preferred_plan", "rtemis")
worker_pool_start <- getFromNamespace("worker_pool_start", "rtemis")
worker_pool_stop <- getFromNamespace("worker_pool_stop", "rtemis")
worker_pool_available <- getFromNamespace("worker_pool_available", "rtemis")
live <- getFromNamespace("live", "rtemis")

# %% rng_substreams ----
testthat::test_that("rng_substreams() derives distinct, deterministic streams", {
  streams <- rng_substreams(2026L, 4L)
  expect_length(streams, 4L)
  expect_identical(streams, rng_substreams(2026L, 4L))
  expect_false(identical(streams, rng_substreams(99L, 4L)))
  # Distinct substreams, not the same stream four times.
  expect_length(unique(lapply(streams, \(s) s[2L])), 4L)
})

testthat::test_that("rng_substreams() returns NULL for a NULL seed", {
  expect_null(rng_substreams(NULL, 4L))
})

testthat::test_that("rng_substreams() leaves the caller's RNG untouched", {
  # Deriving streams switches the generator to L'Ecuyer-CMRG; leaking that would change
  # what every later `set.seed()` in the session produces.
  set.seed(1L)
  before_kind <- RNGkind()
  before_seed <- get(".Random.seed", envir = globalenv())
  invisible(rng_substreams(2026L, 4L))
  expect_identical(RNGkind(), before_kind)
  expect_identical(get(".Random.seed", envir = globalenv()), before_seed)
})


# %% with_preserved_rng ----
testthat::test_that("with_preserved_rng() restores kind and seed", {
  set.seed(1L)
  before_kind <- RNGkind()
  before_seed <- get(".Random.seed", envir = globalenv())
  invisible(with_preserved_rng({
    set.seed(7L, kind = "L'Ecuyer-CMRG")
    runif(10L)
  }))
  expect_identical(RNGkind(), before_kind)
  expect_identical(get(".Random.seed", envir = globalenv()), before_seed)
})


# %% set_preferred_plan ----
testthat::test_that("an unrequested plan is never a forking one", {
  testthat::skip_if_not_installed("future")
  # Forking is only safe in a process that has stayed single-threaded, which a loaded R
  # session need not be, so it is something a caller opts into by name rather than
  # something picked for them.
  # An already-set plan is respected rather than replaced, so reaching the fallback at all
  # needs the ambient plan to be sequential.
  old_plan <- future::plan("sequential")
  on.exit(future::plan(old_plan), add = TRUE)
  # One frame per call: the plan is scoped to the frame it is handed and unwinds with it,
  # so calling twice in this frame would have the second call read the first one's plan
  # back as a choice the user had made and respect it.
  plan_for <- function(n) {
    set_preferred_plan(n_workers = n, envir = environment(), verbosity = 0L)
  }
  expect_identical(plan_for(2L), "multisession")
  expect_identical(plan_for(1L), "sequential")
})


# %% progress_plapply: sequential ----
testthat::test_that("progress_plapply() matches lapply() sequentially", {
  expect_identical(
    progress_plapply(1:4, function(i) i^2, verbosity = 0L),
    lapply(1:4, function(i) i^2)
  )
})

testthat::test_that("progress_plapply() returns an empty list for empty input", {
  expect_identical(progress_plapply(list(), identity, verbosity = 0L), list())
})

testthat::test_that("progress_plapply() forwards ... to FUN", {
  expect_identical(
    progress_plapply(1:3, function(i, k) i * k, k = 10L, verbosity = 0L),
    list(10L, 20L, 30L)
  )
})

testthat::test_that("progress_plapply() rejects a seeds/X length mismatch", {
  expect_error(
    progress_plapply(
      1:4,
      identity,
      seeds = rng_substreams(1L, 3L),
      verbosity = 0L
    ),
    class = "rtemis_length_error"
  )
})


# %% progress_plapply: failure semantics ----
testthat::test_that("progress_plapply() returns task failures as conditions", {
  out <- progress_plapply(
    1:4,
    function(i) if (i == 3L) stop("boom") else i,
    verbosity = 0L
  )
  expect_s3_class(out[[3L]], "condition")
  expect_identical(out[-3L], list(1L, 2L, 4L))
})

testthat::test_that("progress_plapply() re-raises under stop_on_error", {
  expect_error(
    progress_plapply(
      1:4,
      function(i) if (i == 3L) stop("boom") else i,
      stop_on_error = TRUE,
      verbosity = 0L
    ),
    "boom"
  )
})


# %% progress_plapply: parallel backends ----
# One task per element, so `n_workers` never has to divide the task count.
backends <- c(mirai = "mirai", future = "future")

for (backend_name in names(backends)) {
  backend <- backends[[backend_name]]

  testthat::test_that(
    paste0("progress_plapply() matches the sequential result (", backend, ")"),
    {
      testthat::skip_on_cran()
      testthat::skip_if_not_installed(backend)
      seeds <- rng_substreams(2026L, 4L)
      sequential <- progress_plapply(
        1:4,
        function(i) stats::runif(3L),
        seeds = seeds,
        verbosity = 0L
      )
      parallel_out <- progress_plapply(
        1:4,
        function(i) stats::runif(3L),
        seeds = seeds,
        backend = backend,
        n_workers = 2L,
        verbosity = 0L
      )
      expect_identical(sequential, parallel_out)
      # Streams are assigned by task index, so the worker count cannot change the answer.
      expect_identical(
        sequential,
        progress_plapply(
          1:4,
          function(i) stats::runif(3L),
          seeds = seeds,
          backend = backend,
          n_workers = 3L,
          verbosity = 0L
        )
      )
    }
  )

  testthat::test_that(
    paste0("progress_plapply() preserves input order (", backend, ")"),
    {
      testthat::skip_on_cran()
      testthat::skip_if_not_installed(backend)
      # Reversed durations, so completion order is the opposite of input order.
      out <- progress_plapply(
        1:4,
        function(i) {
          Sys.sleep((5L - i) * 0.1)
          i
        },
        backend = backend,
        n_workers = 4L,
        verbosity = 0L
      )
      expect_identical(out, list(1L, 2L, 3L, 4L))
    }
  )

  testthat::test_that(
    paste0("progress_plapply() reports failures uniformly (", backend, ")"),
    {
      testthat::skip_on_cran()
      testthat::skip_if_not_installed(backend)
      failing <- function(i) if (i == 3L) stop("boom") else i
      out <- progress_plapply(
        1:4,
        failing,
        backend = backend,
        n_workers = 2L,
        verbosity = 0L
      )
      expect_s3_class(out[[3L]], "condition")
      expect_identical(out[-3L], list(1L, 2L, 4L))
      expect_error(
        progress_plapply(
          1:4,
          failing,
          backend = backend,
          n_workers = 2L,
          stop_on_error = TRUE,
          verbosity = 0L
        ),
        "boom"
      )
    }
  )
}


# %% train() with parallel outer resampling ----
# CART is not a parallelized learner and needs no tuning, so the worker ladder assigns
# every worker to the outer folds -- the branch this file exercises. `xval` makes rpart's
# pruning cross-validation draw from the RNG, so the run is genuinely seed-dependent.
parallel_dat <- local({
  set.seed(2026L)
  n <- 120L
  data.frame(
    a = stats::rnorm(n),
    b = stats::rnorm(n),
    y = factor(sample(c("no", "yes"), n, replace = TRUE))
  )
})
parallel_resampler <- setup_KFold(
  n_resamples = 4L,
  seed = 2026L
)

fit_folds <- function(backend, n_workers, seed = 2026L, ...) {
  train(
    parallel_dat,
    hyperparameters = setup_CART(xval = 10L, prune_cp = 0.05),
    outer_resampling_config = parallel_resampler,
    execution_config = setup_ExecutionConfig(
      backend = backend,
      n_workers = n_workers,
      seed = seed,
      ...
    ),
    verbosity = 0L
  )
}

# Trees rather than predictions: the pruning path is where rpart consumes the RNG, so it
# is what a seeding regression would show up in first.
fold_trees <- function(mod) {
  lapply(mod@models, function(m) as.numeric(m@model[["cptable"]]))
}

node_kinds <- function(mod) {
  table(vapply(mod@session@events, function(e) e[["kind"]], character(1L)))
}

# The modeling nodes alone. `worker_pool` records the workers being built, so it exists in
# a parallel run and cannot exist in a sequential one -- the one node whose presence is
# *supposed* to differ, and therefore the one that must be set aside when asserting that
# nothing else does.
model_node_kinds <- function(mod) {
  kinds <- node_kinds(mod)
  kinds[names(kinds) != "worker_pool"]
}

testthat::test_that("parallel outer folds reproduce the sequential run", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  sequential <- fit_folds("none", 1L)
  expect_identical(
    fold_trees(sequential),
    fold_trees(fit_folds("mirai", 2L))
  )
  # Not merely stable: different seeds must give different answers, or the comparison
  # above would pass on a run that ignored the seed entirely.
  expect_false(identical(
    fold_trees(sequential),
    fold_trees(fit_folds("mirai", 2L, seed = 99L))
  ))
})

testthat::test_that("the result does not depend on the worker count", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  expect_identical(
    fold_trees(fit_folds("mirai", 2L)),
    fold_trees(fit_folds("mirai", 3L))
  )
})

testthat::test_that("the future backend agrees with the others", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("future")
  expect_identical(
    fold_trees(fit_folds("none", 1L)),
    fold_trees(fit_folds("future", 2L, future_plan = "multisession"))
  )
})

testthat::test_that("a parallel run yields the same execution graph", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  expect_identical(
    model_node_kinds(fit_folds("none", 1L)),
    model_node_kinds(fit_folds("mirai", 2L))
  )
})

# "Dispatched in parallel" does not imply "no session in this process": a forked worker
# inherits the host's `live` env, ambient session included, and a `multicore` plan that
# cannot fork runs the body on the host outright. A fold body that relies on that
# implication nests into the reachable session, brings home no sub-log to graft, and leaves
# a fold node the host then duplicates with its own.
#
# Disabling forking is what makes that condition reachable on demand: `future` runs the
# plan in process, where the ambient session is unambiguously the host's. Actually forking
# reaches the same fold-body branch, but only as far as the test process happens to be
# fork-safe, which is not something a test can assert -- see `set_preferred_plan()`.
testthat::test_that("a parallel fold never shares the host's session", {
  testthat::skip_on_cran()
  testthat::skip_on_os("windows") # multicore is unavailable there
  testthat::skip_if_not_installed("future")
  withr::local_options(future.fork.enable = FALSE)
  expect_identical(
    model_node_kinds(fit_folds("none", 1L)),
    model_node_kinds(fit_folds("future", 2L, future_plan = "multicore"))
  )
})

testthat::test_that("the grafted graph is well formed", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  mod <- fit_folds("mirai", 2L)
  events <- mod@session@events
  ids <- vapply(events, function(e) e[["node_id"]], character(1L))
  parents <- vapply(events, function(e) e[["parent_id"]], character(1L))
  kinds <- vapply(events, function(e) e[["kind"]], character(1L))
  # Worker node ids collide by construction, so this is the check that namespacing works.
  expect_false(any(duplicated(ids)))
  expect_true(all(parents[!is.na(parents)] %in% ids))
  expect_identical(sum(is.na(parents)), 1L)
  # Every fold's sub-log arrived, rather than a bare fold node with nothing under it.
  fold_ids <- ids[kinds == "outer_fold"]
  expect_length(fold_ids, 4L)
  expect_true(all(vapply(
    fold_ids,
    function(f) sum(parents %in% f) > 0L,
    logical(1L)
  )))
})

testthat::test_that("fold sub-models carry no run-level state", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  mod <- fit_folds("mirai", 2L)
  # The graph now lives on the root session; a copy on each sub-model would store it
  # twice. The input config and fingerprint belong to the run, not to a fold.
  expect_true(all(vapply(
    mod@models,
    function(m) is.null(m@session),
    logical(1L)
  )))
  expect_true(all(vapply(
    mod@models,
    function(m) is.null(m@config),
    logical(1L)
  )))
  expect_false(is.null(mod@data_fingerprint))
})

testthat::test_that("the progress sink reports monotonic fold completions", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  seen <- list()
  rtemis.core::with_msg_sink(
    function(m) {
      if (identical(m[["kind"]], "outer_resampling")) {
        seen[[length(seen) + 1L]] <<- m
      }
    },
    invisible(train(
      parallel_dat,
      hyperparameters = setup_CART(),
      outer_resampling_config = parallel_resampler,
      execution_config = setup_ExecutionConfig(
        backend = "mirai",
        n_workers = 2L,
        seed = 2026L
      ),
      # Sink events fire regardless of verbosity - it gates the console
      # renderer only, so a silent run still streams to rtemis.server.
      verbosity = 0L
    ))
  )
  expect_gt(length(seen), 0L)
  expect_true(all(vapply(
    seen,
    function(m) identical(m[["level"]], "progress"),
    logical(1L)
  )))
  expect_true(all(vapply(seen, function(m) m[["total"]] == 4L, logical(1L))))
  expect_true(all(vapply(
    seen,
    function(m) {
      identical(m[["label"]], "Outer resamples") ||
        startsWith(m[["label"]], "Outer resamples [")
    },
    logical(1L)
  )))
  # Folds finish out of order, so the count must never go backwards.
  expect_false(is.unsorted(vapply(
    seen,
    function(m) as.numeric(m[["current"]]),
    numeric(1L)
  )))
  statuses <- vapply(seen, function(m) m[["status"]], character(1L))
  expect_identical(statuses[[1L]], "start")
  expect_identical(statuses[[length(statuses)]], "done")
  expect_identical(as.numeric(seen[[length(seen)]][["current"]]), 4)
})


# %% Failure policy across the parallel boundary ----
# An outcome class holding a single case: the fold that trains without it sees one class
# only, and fails. Exactly one of the four folds does, which is what makes this usable as
# a "some folds failed" fixture rather than an "everything failed" one.
failing_dat <- local({
  set.seed(2026L)
  n <- 200L
  data.frame(
    a = stats::rnorm(n),
    b = stats::rnorm(n),
    y = factor(c(rep("common", n - 1L), "rare"))
  )
})

fit_failing <- function(on_error) {
  train(
    failing_dat,
    hyperparameters = setup_CART(),
    outer_resampling_config = parallel_resampler,
    execution_config = setup_ExecutionConfig(
      backend = "mirai",
      n_workers = 2L,
      seed = 2026L,
      on_error = on_error
    ),
    verbosity = 0L
  )
}

testthat::test_that("a failed parallel fold is tolerated under 'continue'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  mod <- suppressWarnings(fit_failing("continue"))
  expect_lt(length(mod@models), 4L)
  expect_gt(length(mod@models), 0L)
  # The failure is on the graph, not only in a warning that scrolled past.
  expect_gt(
    sum(vapply(
      mod@session@events,
      function(e) identical(e[["status"]], "error"),
      logical(1L)
    )),
    0L
  )
})

testthat::test_that("a failed parallel fold aborts under 'stop_outer'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  expect_error(fit_failing("stop_outer"), "Outer fold")
})


# %% workers reach inside a sequential fold ----
# The ladder gives the run's workers to exactly one level. When that level is inside a
# fold -- a parallelized algorithm, or tuning -- the folds run one at a time and each must
# still receive the workers. A fold config hard-coded to 1 worker serializes both, which
# is invisible in the results and only shows up as a run that takes k times as long.

testthat::test_that("a self-parallelizing algorithm keeps its workers inside a fold", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("ranger")
  mod <- train(
    parallel_dat,
    hyperparameters = setup_Ranger(num_trees = 50L),
    outer_resampling_config = parallel_resampler,
    execution_config = setup_ExecutionConfig(
      backend = "mirai",
      n_workers = 2L,
      seed = 2026L
    ),
    verbosity = 0L
  )
  # Ranger takes the workers as threads, so the folds themselves stay sequential and
  # nothing is dispatched: the only evidence is the worker count each sub-model ran with.
  expect_identical(
    unname(vapply(
      mod@models,
      function(m) m@hyperparameters@n_workers,
      integer(1L)
    )),
    rep(2L, length(mod@models))
  )
})


testthat::test_that("tuning inside a sequential fold dispatches in parallel", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  # Tuning outranks outer resampling in the ladder, so the folds run one at a time and
  # every fold's grid must go to the workers. `progress_plapply()` announces each dispatch,
  # which is the only externally visible difference between a parallel grid and a serial one.
  # Verbosity 2 because a fold runs one level below its caller, and the announcement is
  # made at 1; the handler muffles what it captures, so the run stays quiet.
  dispatches <- character()
  pool_starts <- character()
  withCallingHandlers(
    train(
      parallel_dat,
      hyperparameters = setup_CART(maxdepth = tune_over(2L, 4L)),
      outer_resampling_config = setup_KFold(
        n_resamples = 2L,
        seed = 2026L
      ),
      tuner_config = setup_GridSearch(
        resampler_config = setup_KFold(
          n_resamples = 2L,
          seed = 2026L
        )
      ),
      execution_config = setup_ExecutionConfig(
        backend = "mirai",
        n_workers = 2L,
        seed = 2026L
      ),
      verbosity = 2L
    ),
    message = function(m) {
      # Messages carry ANSI styling, which sits between the digits and the word.
      txt <- gsub("\033\\[[0-9;]*m", "", conditionMessage(m))
      if (grepl("Dispatching", txt, fixed = TRUE)) {
        dispatches <<- c(dispatches, txt)
      }
      if (grepl("for this run", txt, fixed = TRUE)) {
        pool_starts <<- c(pool_starts, txt)
      }
      invokeRestart("muffleMessage")
    }
  )
  # One per fold: each of the two folds tunes, and each tuning run dispatches.
  expect_length(dispatches, 2L)
  expect_true(all(grepl("to 2 workers", dispatches, fixed = TRUE)))
  # Both dispatches land on one pool. Each fold recurses into train() and reaches
  # `worker_pool_start()` again, so a pool per fold is what this rules out.
  expect_length(pool_starts, 1L)
  # And the run leaves nothing standing behind it.
  expect_null(live[["worker_pool"]])
})


# %% worker pool ----
# Workers are stood up once per run and dispatched onto repeatedly. Owned by the dispatch
# instead, they are rebuilt once per outer fold, which costs more than the parallelism
# saves on a short grid.

testthat::test_that("a pool is started once and released", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  expect_false(worker_pool_available("mirai"))
  started <- worker_pool_start("mirai", 2L, verbosity = 0L)
  on.exit(worker_pool_stop(), add = TRUE)
  expect_true(started)
  expect_true(worker_pool_available("mirai"))
  # A nested caller -- an outer fold recursing into train() -- finds it standing, so it
  # neither builds a second pool nor takes on the duty of stopping this one.
  expect_false(worker_pool_start("mirai", 2L, verbosity = 0L))
  # Borrowing is per backend: a future dispatch onto mirai daemons would find no plan.
  expect_false(worker_pool_available("future"))
  worker_pool_stop()
  expect_false(worker_pool_available("mirai"))
})


testthat::test_that("no pool is started when nothing would dispatch", {
  expect_false(worker_pool_start("none", 4L, verbosity = 0L))
  expect_false(worker_pool_start("mirai", 1L, verbosity = 0L))
  expect_false(worker_pool_available("mirai"))
  expect_null(live[["worker_pool"]])
})


testthat::test_that("the worker pool is a node in the execution graph", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  # CART needs no tuning, so the ladder gives the workers to the folds and a pool is built.
  mod <- fit_folds("mirai", 2L)
  pool <- Filter(
    function(e) identical(e[["kind"]], "worker_pool"),
    mod@session@events
  )
  # Once per run, whatever dispatches beneath it.
  expect_length(pool, 1L)
  expect_identical(pool[[1L]][["meta"]][["n_workers"]], 2L)
  expect_identical(pool[[1L]][["meta"]][["backend"]], "mirai")
  # Nested under the run, not reported as a second root.
  expect_false(is.na(pool[[1L]][["parent_id"]]))
  # Setup is work that took time, so the bar has width to read.
  expect_gt(
    as.numeric(difftime(
      pool[[1L]][["t_end"]],
      pool[[1L]][["t_start"]],
      units = "secs"
    )),
    0
  )
})


testthat::test_that("a sequential run reports no worker pool", {
  testthat::skip_on_cran()
  mod <- fit_folds("none", 1L)
  expect_false("worker_pool" %in% names(node_kinds(mod)))
})


testthat::test_that("the worker_pool kind has a fixed color", {
  # Kinds outside the fixed map fall back to a recycled palette, so the bar would change
  # color between runs depending on which other kinds the run reported.
  expect_true("worker_pool" %in% names(session_kind_colors()))
  expect_identical(
    session_kind_colors("worker_pool"),
    c(worker_pool = unname(session_kind_colors()[["worker_pool"]]))
  )
})


# %% shared memory reporting ----
share_decision <- getFromNamespace("share_decision", "rtemis")
report_shared_memory <- getFromNamespace("report_shared_memory", "rtemis")

testthat::test_that("share_decision() gives the first applicable reason", {
  small <- rep(1L, 10L)
  big <- rep(1L, 1e6L)
  # Not asked to, before anything else.
  expect_false(share_decision(big, "none", "mirai", NULL, 4L)[["share"]])
  # Asked to, but nothing to gain: one worker transfers nothing.
  d <- share_decision(big, "auto", "mirai", NULL, 1L)
  expect_false(d[["share"]])
  expect_match(d[["reason"]], "transfers nothing")
  # "always" is decided before 'mori' is looked for, so it shares here whether
  # or not the package is installed -- locality still binds.
  expect_true(share_decision(small, "always", "mirai", NULL, 4L)[["share"]])
  expect_false(share_decision(big, "auto", "future", "remote", 4L)[["share"]])
})


testthat::test_that("share_decision() shares under \"auto\" when it can", {
  # The only outcomes that need 'mori' itself, which is a Suggests: without it
  # installed, "not installed" is the correct answer rather than a failure.
  testthat::skip_if_not_installed("mori")
  small <- rep(1L, 10L)
  big <- rep(1L, 1e6L)
  # Asked to, and worth it.
  expect_true(share_decision(big, "auto", "mirai", NULL, 4L)[["share"]])
  # Size is not a reason to decline: a small payload on parallel local workers shares.
  expect_true(share_decision(small, "auto", "mirai", NULL, 4L)[["share"]])
})


testthat::test_that("the shared-memory report is silent unless sharing was asked for", {
  expect_silent(report_shared_memory(
    rep(1L, 10L),
    mode = "none",
    backend = "mirai",
    future_plan = NULL,
    n_workers = 4L,
    verbosity = 1L
  ))
})


testthat::test_that("the shared-memory report names the size and the reason", {
  msgs <- testthat::capture_messages(
    report_shared_memory(
      rep(1L, 10L),
      mode = "auto",
      backend = "none",
      future_plan = NULL,
      n_workers = 1L,
      verbosity = 1L
    )
  )
  txt <- gsub("\033\\[[0-9;]*m", "", paste(msgs, collapse = ""))
  # Both the size and the reason, so a reader who expected sharing learns why it did not
  # happen rather than only that it did not.
  expect_match(txt, "not sharing training data")
  expect_match(txt, "transfers nothing")
})
