# helper-execution.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% skip_ci_parallel_integration ----
# Real worker integration runs locally and requires an explicit CI opt-in.
# Sequential execution and worker-policy assertions do not use this gate.
skip_ci_parallel_integration <- function() {
  testthat::skip_if(
    identical(tolower(Sys.getenv("CI")), "true") &&
      !identical(tolower(Sys.getenv("RTEMIS_RUN_PARALLEL_TESTS")), "true"),
    "Parallel worker integration requires RTEMIS_RUN_PARALLEL_TESTS=true in CI"
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
