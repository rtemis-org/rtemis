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
  properties = list(
    backend = class_character,
    n_workers = class_integer,
    future_plan = class_character | NULL
  ),
  constructor = function(backend, n_workers, future_plan) {
    n_workers <- clean_int(n_workers)
    check_character(backend, allow_null = FALSE)
    check_character(future_plan, allow_null = TRUE)
    new_object(
      S7::S7_object(),
      backend = backend,
      n_workers = n_workers,
      future_plan = future_plan
    )
  },
  validator = function(self) {
    if (self@backend == "future" && is.null(self@future_plan)) {
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


# %% repr ExecutionConfig ----
method(repr, ExecutionConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("ExecutionConfig", pad = pad, output_type = output_type)
  .props <- props(x)
  if (.props[["backend"]] != "future") {
    .props[["future_plan"]] <- NULL
  }
  out <- paste0(
    out,
    repr_ls(.props, pad = pad + 2L, output_type = output_type)
  )
} # /rtemis::repr.ExecutionConfig

# %%print ExecutionConfig ----
method(print, ExecutionConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
} # /rtemis::print.ExecutionConfig


# %% User API ----
#' Setup Execution Configuration
#'
#' @param backend Character: Execution backend: "future", "mirai", or "none".
#' @param n_workers Integer: Number of workers for parallel execution. Only used if `backend is
#'  "future"` or "mirai". Do not rely on the default value, set to an appropriate number depending
#' on your system.
#' @param future_plan Character: Future plan to use if `backend` is "future".
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
  future_plan = NULL
) {
  backend <- match.arg(backend)
  if (backend == "future") {
    check_dependencies("futurize")
    check_character(future_plan, allow_null = TRUE)
    if (is.null(future_plan)) {
      future_plan <- getOption("future.plan", "mirai_multisession")
    }
    if (!future_plan %in% ALLOWED_PLANS) {
      cli::cli_abort(
        "{.val {future_plan}} is not an allowed future plan. Allowed plans: {.val {ALLOWED_PLANS}}."
      )
    }
    if (is.null(n_workers)) {
      n_workers <- parallelly::availableCores(omit = 3L)
    }
  } else if (backend == "mirai") {
    check_dependencies("mirai")
    if (is.null(n_workers)) {
      n_workers <- parallelly::availableCores(omit = 3L)
    }
  } else if (backend == "none") {
    if (is.null(n_workers)) {
      n_workers <- 1L
    } else if (n_workers != 1L) {
      cli::cli_abort("n_workers must be 1 when backend is 'none'.")
    }
  }
  n_workers <- clean_int(n_workers)
  if (n_workers < 1L) {
    cli::cli_abort("n_workers must be at least 1.")
  }
  ExecutionConfig(
    backend = backend,
    n_workers = n_workers,
    future_plan = if (backend == "future") future_plan else NULL
  )
} # /rtemis::setup_ExecutionConfig
