# test-parallel.R
# ::rtemis::
# 2026- EDG rtemis.org

# A process deadline can stop a blocked native wait; an R time limit cannot.
args <- commandArgs(trailingOnly = TRUE)
child <- length(args) > 0L && identical(args[[1L]], "--child")
if (!child) {
  if (length(args) != 3L) {
    stop(
      "Usage: test-parallel.R <output-directory> <timeout-seconds> <test-filter>"
    )
  }
  out <- args[[1L]]
  timeout <- suppressWarnings(as.numeric(args[[2L]]))
  if (length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
    stop("Timeout must be a positive finite number.")
  }
  if (
    dir.exists(out) && length(list.files(out, all.files = TRUE, no.. = TRUE))
  ) {
    stop(
      "Output directory must be empty; previous evidence is never overwritten."
    )
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out, mustWork = TRUE)
  script <- sub(
    "^--file=",
    "",
    commandArgs()[startsWith(commandArgs(), "--file=")]
  )
  stopifnot(length(script) == 1L)
  Sys.setenv(
    RTEMIS_RUN_PARALLEL_TESTS = "true",
    RTEMIS_PARALLEL_DIAGNOSTIC_OUT = out
  )
  start <- as.numeric(Sys.time())
  deadline <- start + timeout
  process <- processx::process[["new"]](
    file.path(R.home("bin"), "Rscript"),
    c(normalizePath(script), "--child", args[[3L]]),
    stdout = file.path(out, "process.log"),
    stderr = "2>&1",
    cleanup_tree = TRUE
  )
  # Short waits recheck wall time after host suspension or a delayed poll.
  repeat {
    remaining <- deadline - as.numeric(Sys.time())
    if (remaining <= 0 || !process[["is_alive"]]()) {
      break
    }
    process[["wait"]](min(remaining, 1) * 1000)
  }
  timed_out <- as.numeric(Sys.time()) >= deadline
  if (timed_out && process[["is_alive"]]()) {
    process[["kill_tree"]]()
    process[["wait"]](5000)
    if (process[["is_alive"]]()) process[["kill"]]()
  }
  status <- process[["get_exit_status"]]()
  verdict <- list(
    exit_code = status,
    timed_out = timed_out,
    elapsed_seconds = as.numeric(Sys.time()) - start,
    timeout_seconds = timeout,
    filter = args[[3L]]
  )
  jsonlite::write_json(
    verdict,
    file.path(out, "process.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  cat(
    "Parallel diagnostic:",
    if (timed_out) "timed out" else paste("exit", status),
    "-",
    out,
    "\n"
  )
  quit(
    status = if (timed_out) {
      124L
    } else if (identical(status, 0L)) {
      0L
    } else {
      1L
    }
  )
}
stopifnot(length(args) == 2L)
# Diagnostic logs intentionally record only an allowlisted environment snapshot.
out <- Sys.getenv("RTEMIS_PARALLEL_DIAGNOSTIC_OUT")
stopifnot(nzchar(out), dir.exists(out))
events <- file.path(out, "events.jsonl")
emit <- function(event, ...) {
  entry <- c(
    list(
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      pid = Sys.getpid(),
      event = event
    ),
    list(...)
  )
  cat(
    jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null"),
    "\n",
    file = events,
    append = TRUE
  )
}
variables <- c(
  "CI",
  "CODEX_CI",
  "NOT_CRAN",
  "R_LIBS",
  "R_LIBS_USER",
  "R_PROFILE",
  "R_PROFILE_USER",
  "R_ENVIRON",
  "R_ENVIRON_USER",
  "TERM",
  "RTEMIS_RUN_PARALLEL_TESTS",
  "OMP_NUM_THREADS",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS"
)
packages <- c(
  "rtemis",
  "rtemis.core",
  "testthat",
  "future",
  "future.mirai",
  "parallelly",
  "mirai",
  "nanonext"
)
jsonlite::write_json(
  list(
    command = commandArgs(),
    interactive = interactive(),
    stdin_tty = isatty(stdin()),
    stdout_tty = isatty(stdout()),
    environment = as.list(Sys.getenv(variables)),
    libraries = .libPaths(),
    versions = stats::setNames(
      lapply(packages, function(p) as.character(utils::packageVersion(p))),
      packages
    ),
    installed_rtemis = find.package("rtemis")
  ),
  file.path(out, "context.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
suppressMessages(devtools::load_all(quiet = TRUE))
for (package in c("rtemis", "mirai")) {
  functions <- if (package == "rtemis") {
    c(
      "worker_pool_start",
      "warm_workers",
      "worker_pool_stop",
      "progress_plapply"
    )
  } else {
    c("daemons", "mirai_map", "call_mirai", "stop_mirai")
  }
  for (name in functions) {
    label <- paste(package, name, sep = "::")
    trace(
      name,
      where = asNamespace(package),
      print = FALSE,
      tracer = substitute(
        get("emit", globalenv())("enter", operation = LABEL),
        list(LABEL = label)
      ),
      exit = substitute(
        get("emit", globalenv())("exit", operation = LABEL),
        list(LABEL = label)
      )
    )
  }
}
Reporter <- R6::R6Class(
  "ParallelDiagnosticReporter",
  inherit = testthat::ListReporter,
  public = list(
    start_test = function(context, test) {
      emit("test_start", test = test, not_cran = Sys.getenv("NOT_CRAN"))
      super[["start_test"]](context, test)
    },
    end_test = function(context, test) {
      super[["end_test"]](context, test)
      emit("test_end", test = test)
    }
  )
)
result <- testthat::test_local(
  filter = args[[2L]],
  reporter = Reporter[["new"]](),
  stop_on_failure = FALSE
)
saveRDS(result, file.path(out, "results.rds"))
df <- as.data.frame(result)
summary <- list(
  passed = sum(df[["passed"]]),
  failed = sum(df[["failed"]]),
  errors = sum(df[["error"]]),
  warnings = sum(df[["warning"]]),
  skipped = sum(df[["skipped"]])
)
jsonlite::write_json(
  summary,
  file.path(out, "verdict.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
emit("finished", summary = summary)
stopifnot(summary[["failed"]] == 0L, summary[["errors"]] == 0L)
