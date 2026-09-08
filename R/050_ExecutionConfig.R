# 050_ExecutionConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% ExecutionConfig ----
#' ExecutionConfig Class
#'
#' @description
#' Abstract base for the execution family: how a run dispatches work. `backend`
#' is the discriminator and the variant holds what that backend actually needs.
#' Concrete variants are `SerialExecutionConfig`, `FutureExecutionConfig` and
#' `MiraiExecutionConfig`; build one with [setup_FutureExecution].
#'
#' @author EDG
#' @noRd
ExecutionConfig <- new_class(
  name = "ExecutionConfig",
  package = "rtemis",
  abstract = TRUE,
  # Only what every variant shares *unchanged*. A property a variant needs to
  # constrain is declared on the variants instead, never here and overridden:
  # `own_prop_names()` subtracts by name, so an override is not a leaf's own
  # property and would vanish from the published leaf schema, taking its
  # constraint with it.
  properties = list(
    backend = class_character,
    # Threads inside a worker rather than worker processes, which is why this composes
    # with either dispatch level and is declared on the base: it is meaningful under
    # serial execution too.
    n_workers_algorithm = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Threads for a self-parallelizing algorithm. NULL = assigned by the worker ladder."
    ),
    warm_workers = prop_boolean(
      TRUE,
      description = "Load rtemis in every worker when the pool is built, rather than on each worker's first task."
    ),
    on_error = prop_string(
      "continue",
      enum = c("continue", "stop", "stop_outer"),
      description = "Failure policy."
    ),
    # Master seed for the run's *computation* RNG, distinct from a ResamplerConfig's
    # seed, which governs how the data is split. Independent substreams are derived
    # from it and assigned by task index, so a run's results do not depend on backend,
    # worker count, or the order tasks happened to finish in.
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Master seed for the run's computation RNG."
    ),
    # Every parallel task receives the same training data and slices its own rows from
    # it, so the data is serialized once per task. Shared memory replaces those copies
    # with one region every worker maps. Default "auto" because the transport is cheaper
    # at every measured size and identical in result; it is skipped, silently, wherever
    # it cannot apply.
    shared_memory = prop_string(
      "auto",
      enum = c("none", "auto", "always"),
      description = "Share worker data through OS shared memory."
    )
  )
) # /rtemis::ExecutionConfig


# %% SerialExecutionConfig ----
#' SerialExecutionConfig Class
#'
#' @description
#' Execution in the calling process: nothing is dispatched to a worker. The
#' backend value is `"none"` because `backend` names the backend and there is
#' no third backend, only the absence of one; the class is named for what it
#' is.
#'
#' `n_workers` and the two dispatch levels are constrained rather than removed.
#' A variant that dropped them would invalidate every stored serial document
#' under `additionalProperties: false`, so the rules that governed them become
#' type facts instead: the pool is fixed at one worker and neither dispatch
#' level can exceed one.
#'
#' @author EDG
#' @noRd
SerialExecutionConfig <- new_class(
  name = "SerialExecutionConfig",
  parent = ExecutionConfig,
  package = "rtemis",
  properties = list(
    backend = prop_algorithm("none")
  )
) # /rtemis::SerialExecutionConfig


# %% ParallelExecutionConfig ----
#' ParallelExecutionConfig Class
#'
#' @description
#' Abstract base for the backends that dispatch to worker processes. Named for
#' parallelism rather than asynchrony: what varies across these variants is how
#' many workers run at once, not whether the caller blocks -- `train()` blocks
#' under every backend.
#'
#' Exists so the one surviving cross-field rule is declared once instead of on
#' each concrete backend.
#'
#' @author EDG
#' @noRd
ParallelExecutionConfig <- new_class(
  name = "ParallelExecutionConfig",
  parent = ExecutionConfig,
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    n_workers = prop_integer(
      1L,
      min = 1L,
      description = "Number of parallel workers."
    ),
    # Per-level overrides. NULL means "let the ladder decide"; setting any one of them
    # turns the ladder off entirely and the unset levels take 1, so a config never mixes
    # a hand-picked level with an inferred one.
    n_workers_outer = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Workers for outer resampling. NULL = assigned by the worker ladder."
    ),
    n_workers_tuning = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Workers for tuning. NULL = assigned by the worker ladder."
    )
  ),
  validator = function(self) {
    # `NULL > 1L` is `logical(0)`, so `isTRUE()` reads an unset level as "not parallel"
    # without a separate is.null() guard at each use.
    outer_parallel <- isTRUE(self@n_workers_outer > 1L)
    tuning_parallel <- isTRUE(self@n_workers_tuning > 1L)
    if (outer_parallel && tuning_parallel) {
      paste0(
        "Only one dispatch level can run in parallel, but n_workers_outer is ",
        self@n_workers_outer,
        " and n_workers_tuning is ",
        self@n_workers_tuning,
        ". An outer fold runs in a worker process and cannot dispatch again from ",
        "inside one. Set one of them to 1. n_workers_algorithm is threads within a ",
        "worker and combines with either."
      )
    }
  }
) # /rtemis::ParallelExecutionConfig


# %% FutureExecutionConfig ----
#' FutureExecutionConfig Class
#'
#' @description
#' Dispatch through \pkg{future}. `future_plan` is non-nullable here, which is
#' what the old "must be set when backend is 'future'" rule stated at runtime.
#'
#' @author EDG
#' @noRd
FutureExecutionConfig <- new_class(
  name = "FutureExecutionConfig",
  parent = ParallelExecutionConfig,
  package = "rtemis",
  properties = list(
    backend = prop_algorithm("future"),
    future_plan = prop_string(
      "mirai_multisession",
      description = "Future plan to use."
    )
  )
) # /rtemis::FutureExecutionConfig


# %% MiraiExecutionConfig ----
#' MiraiExecutionConfig Class
#'
#' @description
#' Dispatch through \pkg{mirai}.
#'
#' @author EDG
#' @noRd
MiraiExecutionConfig <- new_class(
  name = "MiraiExecutionConfig",
  parent = ParallelExecutionConfig,
  package = "rtemis",
  properties = list(
    backend = prop_algorithm("mirai")
  )
) # /rtemis::MiraiExecutionConfig


# %% execution_n_workers ----
#' The worker pool an execution config builds
#'
#' Declared on `ParallelExecutionConfig` because only a parallel backend has a
#' pool. Serial execution runs in the calling process, which is one worker, and
#' says so here rather than by carrying a property fixed at 1 that a form would
#' then offer.
#'
#' @param x `ExecutionConfig` object.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
execution_n_workers <- function(x) {
  if (S7_inherits(x, ParallelExecutionConfig)) x@n_workers else 1L
} # /rtemis::execution_n_workers


# %% execution_dispatch_level ----
#' A named dispatch level, or NULL where the backend has none
#'
#' `n_workers_outer` and `n_workers_tuning` are declared on
#' `ParallelExecutionConfig`: they name how many worker *processes* a level
#' claims, and serial execution dispatches to none. Reading them through here
#' lets the worker ladder ask every config the same question.
#'
#' @param x `ExecutionConfig` object.
#' @param level Character \{"n_workers_outer", "n_workers_tuning"\}.
#'
#' @return Integer, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
execution_dispatch_level <- function(x, level) {
  if (S7_inherits(x, ParallelExecutionConfig)) prop(x, level) else NULL
} # /rtemis::execution_dispatch_level


# %% execution_future_plan ----
#' The future plan an execution config runs under, or NULL
#'
#' Only `FutureExecutionConfig` has one; the other backends do not consult a
#' plan, so they do not declare the property.
#'
#' @param x `ExecutionConfig` object.
#'
#' @return Character, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
execution_future_plan <- function(x) {
  if (S7_inherits(x, FutureExecutionConfig)) x@future_plan else NULL
} # /rtemis::execution_future_plan


# %% repr.ExecutionConfig ----
method(repr, ExecutionConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name(S7_class(x)@name, pad = pad, output_type = output_type)
  .props <- props(x)
  # An unset level is the ordinary case and says nothing the reader does not already
  # know from `n_workers`; a set one is the whole point and stays.
  for (level in c(
    "n_workers_outer",
    "n_workers_tuning",
    "n_workers_algorithm"
  )) {
    if (is.null(.props[[level]])) {
      .props[[level]] <- NULL
    }
  }
  out <- paste0(
    out,
    repr_ls(.props, pad = pad, output_type = output_type)
  )
} # /rtemis::repr.ExecutionConfig


# %% print.ExecutionConfig ----
method(print, ExecutionConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
} # /rtemis::print.ExecutionConfig


# %% default_n_workers ----
#' Default number of workers
#'
#' Determine the default number of parallel workers, guarding against errors in
#' environments where `parallelly::availableCores()` is unavailable (e.g. wasm/webR).
#'
#' @param omit Integer: Number of cores to omit from the count.
#'
#' @return Integer: Number of workers.
#'
#' @author EDG
#' @keywords internal
#' @noRd
default_n_workers <- function(omit = 3L) {
  # `availableCores()` names its result after the mechanism it consulted ("system",
  # "cgroups", ...), and that name rides along into every message that prints the worker
  # count -- "Max workers: c(system = 7)".
  # The fallback covers that probe failing in an environment it cannot read, not the
  # package being absent: `future` is an Import and hard-imports `parallelly`, so it is
  # always installed. `availableCores()` floors at 1 and does not error on `omit`
  # exceeding the core count, so this is reached only by an unreadable environment.
  unname(tryCatch(
    parallelly::availableCores(omit = omit),
    error = function(e) 1L
  ))
} # /rtemis::default_n_workers


# %% --- User API ----

# %% EXECUTION_CLASSES ----
# The execution family, keyed by the `backend` value each variant declares.
EXECUTION_CLASSES <- list(
  none = SerialExecutionConfig,
  future = FutureExecutionConfig,
  mirai = MiraiExecutionConfig
)

EXECUTION_SETUP <- c(
  none = "setup_SerialExecution",
  future = "setup_FutureExecution",
  mirai = "setup_MiraiExecution"
)


# %% .execution_common ----
#' Validate and resolve the settings every execution variant shares
#'
#' @param n_workers_outer,n_workers_tuning,n_workers_algorithm Optional Integer.
#' @param on_error,shared_memory Character: Already matched by the caller.
#' @param seed Optional Integer.
#' @param warm_workers Logical.
#'
#' @return Named list of resolved shared settings.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.execution_common <- function(
  n_workers_algorithm,
  on_error,
  seed,
  shared_memory,
  warm_workers
) {
  check_logical_scalar(warm_workers)
  if (!is.null(n_workers_algorithm)) {
    n_workers_algorithm <- clean_int(n_workers_algorithm)
    check_pos_integer_scalar(n_workers_algorithm)
  }
  # "always" is a demand, so an unusable request is an error here rather than a surprise
  # at dispatch. "auto" is best-effort: a missing mori is one more reason it cannot
  # share, not a mistake to correct.
  if (shared_memory == "always") {
    check_dependencies("mori")
  }
  # Resolved here rather than at run time so it is recorded on the config, and
  # therefore in the run record: an unseeded run would otherwise be unreproducible, and
  # "all runs are auditable & reproducible" has to hold for the default path too.
  # Drawing from the current stream keeps `set.seed(1); train(...)` deterministic.
  seed <- if (is.null(seed)) {
    sample.int(.Machine[["integer.max"]], 1L)
  } else {
    clean_int(seed)
  }
  list(
    n_workers_algorithm = n_workers_algorithm,
    on_error = on_error,
    seed = seed,
    shared_memory = shared_memory,
    warm_workers = warm_workers
  )
} # /rtemis::.execution_common


# %% .execution_dispatch ----
#' Validate and resolve the settings only a parallel backend has
#'
#' @param n_workers,n_workers_outer,n_workers_tuning Optional Integer.
#'
#' @return Named list of resolved dispatch settings.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.execution_dispatch <- function(n_workers, n_workers_outer, n_workers_tuning) {
  for (nm in c("n_workers_outer", "n_workers_tuning")) {
    v <- get(nm)
    if (!is.null(v)) {
      v <- clean_int(v)
      check_pos_integer_scalar(v)
      assign(nm, v)
    }
  }
  list(
    n_workers = .resolve_n_workers(
      n_workers,
      n_workers_outer,
      n_workers_tuning
    ),
    n_workers_outer = n_workers_outer,
    n_workers_tuning = n_workers_tuning
  )
} # /rtemis::.execution_dispatch


# %% .resolve_n_workers ----
#' Size the worker pool for a parallel backend
#'
#' With the dispatch levels named explicitly the pool follows from them rather
#' than from `default_n_workers()`. Only the dispatch levels count: algorithm
#' workers are threads inside a worker, not workers of their own.
#'
#' @param n_workers Optional Integer: What the caller supplied.
#' @param n_workers_outer,n_workers_tuning Optional Integer.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.resolve_n_workers <- function(n_workers, n_workers_outer, n_workers_tuning) {
  if (is.null(n_workers)) {
    n_workers <- if (is.null(n_workers_outer) && is.null(n_workers_tuning)) {
      default_n_workers()
    } else {
      max(1L, n_workers_outer %||% 1L, n_workers_tuning %||% 1L)
    }
  }
  n_workers <- clean_int(n_workers)
  if (n_workers < 1L) {
    rtemis.core::abort(
      "n_workers must be at least 1.",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  n_workers
} # /rtemis::.resolve_n_workers


# %% --- User API ----

# %% setup_FutureExecution ----
#' Set up a `FutureExecutionConfig`
#'
#' @description
#' Parallel execution through \pkg{future}. A plan is required, and is filled in
#' from `getOption("future.plan", "mirai_multisession")` when not given.
#'
#' @param n_workers Integer [1, Inf): Number of parallel workers. Left `NULL`,
#' it follows the named dispatch levels, or `default_n_workers()` if none is named.
#' @param n_workers_outer Optional Integer [1, Inf): Workers for outer resampling,
#' overriding the automatic assignment.
#' @param n_workers_tuning Optional Integer [1, Inf): Workers for tuning, overriding the
#' automatic assignment.
#' @param n_workers_algorithm Optional Integer [1, Inf): Threads for a self-parallelizing
#' algorithm, overriding the automatic assignment.
#' @param future_plan Character: Future plan to use. Defaults to
#' `getOption("future.plan", "mirai_multisession")`.
#' @param on_error Character \{"continue", "stop", "stop_outer"\}: Failure policy.
#' `"continue"` makes grid cells and unscorable hyperparameter combinations
#' non-fatal (recorded, warned, and excluded), failing only when nothing is scorable or
#' the final model fails; `"stop"` aborts on any error; `"stop_outer"` tolerates grid-cell
#' failures but aborts on an outer-fold failure.
#' @param seed Optional Integer [0, Inf): Master seed for the run's computation RNG,
#' from which one independent substream per parallel task is derived. Left `NULL`, a
#' seed is drawn from the current RNG stream and recorded on the returned object, so
#' every run is reproducible and the seed it used is auditable. Distinct from a
#' `ResamplerConfig` seed, which governs how the data is split.
#' @param shared_memory Character \{"none", "auto", "always"\}: Whether to hand workers
#' the training data through OS shared memory instead of serializing a copy to each.
#' `"auto"` is the default: it shares whenever it can -- workers parallel and on this
#' machine, \pkg{mori} installed -- and quietly does not when it cannot, falling back to
#' the ordinary transport. `"none"` disables it. `"always"` is a demand rather than a
#' preference: it shares even when the run is sequential, which is what allows a run to
#' be compared against its own shared counterpart, and raises rather than degrades when
#' the request cannot be honored -- which is what a caller relying on sharing to stay
#' inside a memory budget needs.
#' @param warm_workers Logical: Load \pkg{rtemis} in every worker as the pool is built,
#' rather than leaving each worker to load it on its first task.
#'
#' @details
#' **Worker levels**
#'
#' There are three levels that can absorb workers: a self-parallelizing algorithm,
#' tuning, and outer resampling. Left alone, `n_workers` is assigned to exactly one of
#' them, in that order of priority, so no two levels compete for the same cores.
#'
#' Naming any of `n_workers_outer`, `n_workers_tuning` or `n_workers_algorithm` takes
#' over that assignment: the automatic one is switched off entirely and any level not
#' named gets 1, so a run never mixes a hand-picked level with an inferred one.
#'
#' Outer resampling and tuning dispatch to worker *processes*, and an outer fold runs
#' inside one of those processes, so only one of the two can be parallel -- setting both
#' above 1 is an error. `n_workers_algorithm` is threads within a worker, so it combines
#' with either.
#'
#' **Reproducibility**
#'
#' Substreams are assigned by task index, so a run gives the same answer under every
#' backend and worker count, and the parallel result matches the sequential one exactly.
#'
#' Shared memory needs \pkg{mori}, whose minimum R version (4.3) is above
#' \pkg{rtemis}'s own (4.1), so `"auto"` is the portable setting for a script that may
#' run anywhere. It is local RAM: workers on another machine cannot map it.
#'
#' @return `FutureExecutionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_FutureExecution(n_workers = 4L, future_plan = "multisession")
setup_FutureExecution <- function(
  n_workers = NULL,
  n_workers_outer = NULL,
  n_workers_tuning = NULL,
  n_workers_algorithm = NULL,
  future_plan = getOption("future.plan", "mirai_multisession"),
  on_error = c("continue", "stop", "stop_outer"),
  seed = NULL,
  shared_memory = c("auto", "none", "always"),
  warm_workers = TRUE
) {
  # Captured before anything is filled in: this function's defaults are not the
  # class's, so a record comparing the two would report the pool it sized and
  # the seed it drew as the caller's choices.
  origins <- supplied_origins()
  on_error <- match.arg(on_error)
  shared_memory <- match.arg(shared_memory)
  check_dependencies("future")
  check_character(future_plan)
  if (!future_plan %in% ALLOWED_PLANS) {
    rtemis.core::abort(
      "'",
      future_plan,
      "' is not an allowed future plan. Allowed plans: ",
      paste(ALLOWED_PLANS, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  out <- do.call(
    FutureExecutionConfig,
    c(
      .execution_common(
        n_workers_algorithm,
        on_error,
        seed,
        shared_memory,
        warm_workers
      ),
      .execution_dispatch(n_workers, n_workers_outer, n_workers_tuning),
      list(future_plan = future_plan)
    )
  )
  config_origins(out) <- origins
  out
} # /rtemis::setup_FutureExecution


# %% setup_MiraiExecution ----
#' Set up a `MiraiExecutionConfig`
#'
#' @description
#' Parallel execution through \pkg{mirai}. Takes no plan: \pkg{mirai} has no
#' equivalent of a `future_plan`, which is why it is not an argument here.
#'
#' @inheritParams setup_FutureExecution
#'
#' @details
#' See [setup_FutureExecution] for how the three worker levels are assigned and
#' how the run's seed is resolved; both are identical under either backend.
#'
#' @return `MiraiExecutionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_MiraiExecution(n_workers = 2L)
setup_MiraiExecution <- function(
  n_workers = NULL,
  n_workers_outer = NULL,
  n_workers_tuning = NULL,
  n_workers_algorithm = NULL,
  on_error = c("continue", "stop", "stop_outer"),
  seed = NULL,
  shared_memory = c("auto", "none", "always"),
  warm_workers = TRUE
) {
  origins <- supplied_origins()
  on_error <- match.arg(on_error)
  shared_memory <- match.arg(shared_memory)
  check_dependencies("mirai")
  out <- do.call(
    MiraiExecutionConfig,
    c(
      .execution_common(
        n_workers_algorithm,
        on_error,
        seed,
        shared_memory,
        warm_workers
      ),
      .execution_dispatch(n_workers, n_workers_outer, n_workers_tuning)
    )
  )
  config_origins(out) <- origins
  out
} # /rtemis::setup_MiraiExecution


# %% setup_SerialExecution ----
#' Set up a `SerialExecutionConfig`
#'
#' @description
#' Execution in the calling process: nothing is dispatched to a worker. Takes
#' no `n_workers` and no `future_plan`, neither of which a serial run can act
#' on.
#'
#' @inheritParams setup_FutureExecution
#'
#' @details
#' `n_workers_algorithm` still applies: it is threads *within the calling
#' process*, not worker processes, so a self-parallelizing algorithm runs
#' multi-threaded under a serial config.
#'
#' See [setup_FutureExecution] for how the run's seed is resolved.
#'
#' @return `SerialExecutionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_SerialExecution()
setup_SerialExecution <- function(
  n_workers_algorithm = NULL,
  on_error = c("continue", "stop", "stop_outer"),
  seed = NULL,
  shared_memory = c("auto", "none", "always"),
  warm_workers = TRUE
) {
  origins <- supplied_origins()
  on_error <- match.arg(on_error)
  shared_memory <- match.arg(shared_memory)
  out <- do.call(
    SerialExecutionConfig,
    .execution_common(
      n_workers_algorithm,
      on_error,
      seed,
      shared_memory,
      warm_workers
    )
  )
  config_origins(out) <- origins
  out
} # /rtemis::setup_SerialExecution


# %% .list_to_ExecutionConfig ----
#' Rebuild an execution config from a wire list
#'
#' @param x Named list: The `execution_config` block of a config document.
#'
#' @return `ExecutionConfig` subclass object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_ExecutionConfig <- function(x) {
  keys <- unique(unlist(c(
    list(names(ExecutionConfig@properties)),
    lapply(EXECUTION_CLASSES, function(cls) names(cls@properties))
  )))
  check_wire_keys(x, keys, "execution config")
  args <- .drop_meta_keys(x)
  backend <- args[["backend"]]
  if (is.null(backend) || !backend %in% names(EXECUTION_SETUP)) {
    rtemis.core::abort(
      "An execution config needs a `backend`, one of: ",
      paste0("'", names(EXECUTION_SETUP), "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  args[["backend"]] <- NULL
  # A variant declares only what its backend acts on, so a key it has no
  # formal for is a document claiming something that backend cannot do -- a
  # `n_workers` on a serial config, a `future_plan` on a mirai one. The
  # published schema rejects it too: the dispatcher closes the composed
  # document with `unevaluatedProperties`, and the leaf declares no such
  # property to evaluate it.
  fn <- get(EXECUTION_SETUP[[backend]], envir = asNamespace("rtemis"))
  carried <- setdiff(names(args), names(formals(fn)))
  if (length(carried) > 0L) {
    rtemis.core::abort(
      "An execution config with backend '",
      backend,
      "' cannot carry: ",
      paste(carried, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  do.call(fn, args)
} # /rtemis::.list_to_ExecutionConfig
