# helper-execution.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% skip_ci_parallel_integration ----
# Real worker integration requires opt-in in CI and automated agent sessions.
# Sequential execution and worker-policy assertions do not use this gate.
skip_ci_parallel_integration <- function() {
  requested <- tolower(trimws(Sys.getenv("RTEMIS_RUN_PARALLEL_TESTS")))
  if (!requested %in% c("", "true", "false", "1", "0")) {
    stop("RTEMIS_RUN_PARALLEL_TESTS must be true, false, 1, or 0.")
  }
  automated <- any(tolower(Sys.getenv(c("CI", "CODEX_CI"))) %in% c("true", "1"))
  enabled <- requested %in% c("true", "1") || (requested == "" && !automated)
  testthat::skip_if(
    !enabled,
    "Parallel worker integration requires RTEMIS_RUN_PARALLEL_TESTS=true in automated runs; false disables it everywhere"
  )
}


# Build an execution config for a backend named at run time.
#
# The `setup_*` functions are per variant, so that no function accepts an
# argument its backend cannot act on -- which means a test parameterized over
# backends has to choose the constructor rather than pass a flag. `n_workers`
# is a parallel-only argument and is dropped for the serial variant, whose pool
# is fixed at one worker by its class.
exec_config <- function(backend, n_workers = 1L, ...) {
  if (backend == "none") {
    return(setup_SerialExecution(...))
  }
  setup <- switch(
    backend,
    future = setup_FutureExecution,
    mirai = setup_MiraiExecution
  )
  do.call(setup, c(list(n_workers = n_workers), list(...)))
}
