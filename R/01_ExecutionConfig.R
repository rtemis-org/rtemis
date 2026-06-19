# ExecutionConfig.R
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
    backend = class_character,
    n_workers = class_integer,
    future_plan = class_character | NULL,
    on_error = class_character
  ),
  constructor = function(
    backend,
    n_workers,
    future_plan,
    on_error = "continue"
  ) {
    n_workers <- clean_int(n_workers)
    check_character(backend, allow_null = FALSE)
    check_character(future_plan, allow_null = TRUE)
    check_character(on_error, allow_null = FALSE)
    new_object(
      S7::S7_object(),
      backend = backend,
      n_workers = n_workers,
      future_plan = future_plan,
      on_error = on_error
    )
  },
  validator = function(self) {
    if (!self@on_error %in% c("continue", "stop", "stop_outer")) {
      "@on_error must be one of 'continue', 'stop', 'stop_outer'."
    } else if (self@backend == "future" && is.null(self@future_plan)) {
      "@future_plan must be set when backend is 'future'."
    } else if (self@backend == "none" && self@n_workers != 1L) {
      "n_workers must be 1 when backend is 'none'."
    } else if (self@backend == "mirai" && self@n_workers < 1L) {
      "n_workers must be at least 1 when backend is 'mirai'."
    } else if (self@backend == "future" && self@n_workers < 1L) {
      "n_workers must be at least 1 when backend is 'future'."
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
  tryCatch(
    parallelly::availableCores(omit = omit),
    error = function(e) 1L
  )
} # /rtemis::default_n_workers


# %% --- User API ----

# %% setup_ExecutionConfig ----
#' Setup Execution Configuration
#'
#' @param backend Character: Execution backend: "future", "mirai", or "none".
#' @param n_workers Integer: Number of workers for parallel execution. Only used if `backend is
#'  "future"` or "mirai". Do not rely on the default value, set to an appropriate number depending
#' on your system.
#' @param future_plan Character: Future plan to use if `backend` is "future".
#' @param on_error Character \{"continue", "stop", "stop_outer"\}: Failure policy.
#' `"continue"` (default) makes grid cells and unscorable hyperparameter combinations
#' non-fatal (recorded, warned, and excluded), failing only when nothing is scorable or
#' the final model fails; `"stop"` aborts on any error; `"stop_outer"` tolerates grid-cell
#' failures but aborts on an outer-fold failure.
#'
#' @return `ExecutionConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_ExecutionConfig(backend = "future", n_workers = 4L, future_plan = "multisession")
setup_ExecutionConfig <- function(
  backend = c("future", "mirai", "none"),
  n_workers = NULL,
  future_plan = NULL,
  on_error = c("continue", "stop", "stop_outer")
) {
  backend <- match.arg(backend)
  on_error <- match.arg(on_error)
  if (backend == "future") {
    check_dependencies("futurize")
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
  ExecutionConfig(
    backend = backend,
    n_workers = n_workers,
    future_plan = if (backend == "future") future_plan else NULL,
    on_error = on_error
  )
} # /rtemis::setup_ExecutionConfig
