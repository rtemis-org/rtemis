# utils_async.R
# ::rtemis::
# 2026 EDG rtemis.org

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
  cli::cli_abort(
    "Detected future plan not in allowed plans ({.val {ALLOWED_PLANS}}). Detected plan class: {.val {class(x)}}"
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
      cli::cli_abort(
        "Requested plan {.val {requested_plan}} is not one of allowed plans: {.val {ALLOWED_PLANS}}"
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
