# 050_ExecutionConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% ExecutionConfig ----
#' ExecutionConfig Class
#'
#' @description
#' Execution Configuration Class, defining sequential/parallel/distributed execution settings.
#'
#' @author EDG
#' @noRd
ExecutionConfig <- new_class(
  name = "ExecutionConfig",
  package = "rtemis",
  properties = list(
    # "none" (sequential) is the conservative default: valid with no
    # `future_plan` and `n_workers = 1`. setup_ExecutionConfig defaults to
    # "future" for interactive convenience.
    backend = prop_string(
      "none",
      enum = c("future", "mirai", "none"),
      description = "Execution backend."
    ),
    n_workers = prop_integer(
      1L,
      min = 1L,
      description = "Number of parallel workers (used when backend is 'future' or 'mirai')."
    ),
    future_plan = prop_string(
      NULL,
      nullable = TRUE,
      description = "Future plan to use when backend is 'future'."
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
    # with one region every worker maps.
    shared_memory = prop_string(
      "none",
      enum = c("none", "auto", "always"),
      description = "Share worker data through OS shared memory."
    )
  ),
  # Cross-field constraints (the per-field type/enum/bounds are enforced by
  # the prop_* validators).
  validator = function(self) {
    if (self@backend == "future" && is.null(self@future_plan)) {
      "@future_plan must be set when backend is 'future'."
    } else if (self@backend == "none" && self@n_workers != 1L) {
      "n_workers must be 1 when backend is 'none'."
    }
  }
) # /rtemis::ExecutionConfig


# %% repr.ExecutionConfig ----
method(repr, ExecutionConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("ExecutionConfig", pad = pad, output_type = output_type)
  .props <- props(x)
  if (.props[["backend"]] != "future") {
    .props[["future_plan"]] <- NULL
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
  unname(tryCatch(
    parallelly::availableCores(omit = omit),
    error = function(e) 1L
  ))
} # /rtemis::default_n_workers


# %% --- User API ----

# %% setup_ExecutionConfig ----
#' Setup Execution Configuration
#'
#' @param backend Character \{"future", "mirai", "none"\}: Execution backend.
#' @param n_workers Integer [1, Inf): Number of workers for parallel execution. Only used if `backend is
#'  "future"` or "mirai". Set this to an appropriate number depending
#' on your system.
#' @param future_plan Optional Character: Future plan to use if `backend` is "future".
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
#' `"none"` is the default. `"auto"` shares when it is possible and worthwhile -- workers
#' local, payload above a size threshold, \pkg{mori} installed -- and quietly does not
#' when it is not. `"always"` shares regardless of size and treats an impossible request
#' as an error.
#'
#' @details
#' Substreams are assigned by task index, so a run gives the same answer under every
#' backend and worker count, and the parallel result matches the sequential one exactly.
#'
#' Shared memory needs \pkg{mori}, whose minimum R version (4.3) is above
#' \pkg{rtemis}'s own (4.1), so `"auto"` is the portable setting for a script that may
#' run anywhere. It is local RAM: workers on another machine cannot map it.
#'
#' @return `ExecutionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_ExecutionConfig(backend = "future", n_workers = 4L, future_plan = "multisession")
setup_ExecutionConfig <- function(
  backend = c("future", "mirai", "none"),
  n_workers = NULL,
  future_plan = NULL,
  on_error = c("continue", "stop", "stop_outer"),
  seed = NULL,
  shared_memory = c("none", "auto", "always")
) {
  backend <- match.arg(backend)
  on_error <- match.arg(on_error)
  shared_memory <- match.arg(shared_memory)
  # "always" is a demand, so an unusable request is an error here rather than a surprise
  # at dispatch. "auto" is best-effort: a missing mori is one more reason it cannot
  # share, not a mistake to correct.
  if (shared_memory == "always") {
    check_dependencies("mori")
  }
  if (backend == "future") {
    check_dependencies("future")
    check_character(future_plan, allow_null = TRUE)
    if (is.null(future_plan)) {
      future_plan <- getOption("future.plan", "mirai_multisession")
    }
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
    if (is.null(n_workers)) {
      n_workers <- default_n_workers()
    }
  } else if (backend == "mirai") {
    check_dependencies("mirai")
    if (is.null(n_workers)) {
      n_workers <- default_n_workers()
    }
  } else if (backend == "none") {
    if (is.null(n_workers)) {
      n_workers <- 1L
    } else if (n_workers != 1L) {
      rtemis.core::abort(
        "n_workers must be 1 when backend is 'none'.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  n_workers <- clean_int(n_workers)
  if (n_workers < 1L) {
    rtemis.core::abort(
      "n_workers must be at least 1.",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  # Resolve the seed here rather than at run time so it is recorded on the config, and
  # therefore in the run record: an unseeded run would otherwise be unreproducible, and
  # "all runs are auditable & reproducible" has to hold for the default path too.
  # Drawing from the current stream keeps `set.seed(1); train(...)` deterministic.
  seed <- if (is.null(seed)) {
    sample.int(.Machine[["integer.max"]], 1L)
  } else {
    clean_int(seed)
  }
  ExecutionConfig(
    backend = backend,
    n_workers = n_workers,
    future_plan = if (backend == "future") future_plan else NULL,
    on_error = on_error,
    seed = seed,
    shared_memory = shared_memory
  )
} # /rtemis::setup_ExecutionConfig
