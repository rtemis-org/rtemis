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
parallel_resampler <- setup_Resampler(
  n_resamples = 4L,
  type = "KFold",
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
    node_kinds(fit_folds("none", 1L)),
    node_kinds(fit_folds("mirai", 2L))
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
    node_kinds(fit_folds("none", 1L)),
    node_kinds(fit_folds("future", 2L, future_plan = "multicore"))
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

testthat::test_that("the progress callback reports monotonic completions", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  seen <- list()
  callback <- function(stage, current, total, message) {
    seen[[length(seen) + 1L]] <<- list(
      stage = stage,
      current = current,
      total = total
    )
  }
  invisible(train(
    parallel_dat,
    hyperparameters = setup_CART(),
    outer_resampling_config = parallel_resampler,
    execution_config = setup_ExecutionConfig(
      backend = "mirai",
      n_workers = 2L,
      seed = 2026L
    ),
    progress = callback,
    verbosity = 0L
  ))
  expect_gt(length(seen), 0L)
  expect_true(all(vapply(
    seen,
    function(s) identical(s[["stage"]], "outer_fold"),
    logical(1L)
  )))
  expect_true(all(vapply(seen, function(s) s[["total"]] == 4L, logical(1L))))
  # Folds finish out of order, so the count must never go backwards.
  expect_false(is.unsorted(vapply(
    seen,
    function(s) as.numeric(s[["current"]]),
    numeric(1L)
  )))
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
