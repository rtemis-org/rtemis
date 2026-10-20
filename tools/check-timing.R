# tools/check-timing.R
# ::rtemis::
# 2026- EDG rtemis.org

# Summarizes the timings of an `R CMD check` directory as Markdown on standard
# output: the elapsed time of the whole check, the timed stages of
# `00check.log`, the slowest examples from `<pkg>-Ex.timings`, and the tests
# that `tests/ci/test-groups.R` reports as slow in each `tests/*.Rout` file.
# Arguments: the check directory (`<pkg>.Rcheck`) and, optionally, the elapsed
# seconds of the check.
#
# The cran-incoming workflow writes this summary to each job's step summary.

args <- commandArgs(TRUE)
check_dir <- args[[1L]]
elapsed <- if (length(args) > 1L) {
  suppressWarnings(as.numeric(args[[2L]]))
} else {
  NA
}
n_examples <- 20L
n_tests <- 30L

# %% md_table ----
md_table <- function(x) {
  cells <- vapply(x, as.character, character(nrow(x)))
  if (is.null(dim(cells))) {
    cells <- matrix(cells, nrow = 1L)
  }
  rows <- apply(cells, 1L, paste, collapse = " | ")
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("|", strrep("---|", ncol(x))),
    paste0("| ", rows, " |")
  )
}

# %% read_ex_timings ----
# Each data row of `<pkg>-Ex.timings` ends in a tab, so the rows carry one more
# field than the header; fields are split by hand.
read_ex_timings <- function(path) {
  lines <- readLines(path, warn = FALSE)[-1L]
  fields <- strsplit(lines, "\t", fixed = TRUE)
  fields <- fields[lengths(fields) >= 4L]
  num <- function(i) as.numeric(vapply(fields, `[[`, "", i))
  data.frame(
    name = vapply(fields, `[[`, "", 1L),
    user = num(2L),
    system = num(3L),
    elapsed = num(4L)
  )
}

out <- c("## R CMD check timings", "")
if (!dir.exists(check_dir)) {
  cat(c(out, paste0("No check directory `", check_dir, "`."), ""), sep = "\n")
  quit(status = 0L)
}

log_path <- file.path(check_dir, "00check.log")
log <- if (file.exists(log_path)) {
  readLines(log_path, warn = FALSE)
} else {
  character()
}
status <- grep("^Status:", log, value = TRUE)
out <- c(
  out,
  paste0(
    "- Check elapsed: ",
    if (is.na(elapsed)) {
      "unknown"
    } else {
      sprintf("%.0f s (%.1f min)", elapsed, elapsed / 60)
    }
  ),
  paste0(
    "- ",
    if (length(status)) status[[length(status)]] else "Status: not reported"
  ),
  ""
)

# R CMD check reports `[elapsed]` on Windows and `[cpu/elapsed]` elsewhere.
stages <- grep("\\[[0-9.]+m?s(/[0-9.]+m?s)?\\]", log, value = TRUE)
if (length(stages)) {
  out <- c(out, "### Timed check stages", "", "```", stages, "```", "")
}

ex_path <- list.files(check_dir, pattern = "-Ex[.]timings$", full.names = TRUE)
if (length(ex_path)) {
  ex <- read_ex_timings(ex_path[[1L]])
  ex <- ex[order(-ex[["elapsed"]]), ]
  # CPU time above elapsed time means more than one core was busy.
  ratio <- (ex[["user"]] + ex[["system"]]) / ex[["elapsed"]]
  top <- utils::head(
    data.frame(
      example = ex[["name"]],
      elapsed = sprintf("%.2f", ex[["elapsed"]]),
      cpu = sprintf("%.2f", ex[["user"]] + ex[["system"]]),
      `cpu/elapsed` = ifelse(ex[["elapsed"]] > 0, sprintf("%.2f", ratio), ""),
      check.names = FALSE
    ),
    n_examples
  )
  out <- c(
    out,
    sprintf(
      "### Slowest examples (%d of %d; %.1f s in example bodies)",
      nrow(top),
      nrow(ex),
      sum(ex[["elapsed"]])
    ),
    "",
    md_table(top),
    ""
  )
}

# `run_test_group()` lists each test that takes at least 2 s as
# `[<seconds>s] <file>: <test>`; testthat's summary repeats the slowest ten.
rout <- list.files(
  file.path(check_dir, "tests"),
  pattern = "[.]Rout(\\.fail)?$",
  full.names = TRUE
)
slow_pattern <- "^\\[([0-9.]+)s\\] (.*)$"
slow <- unique(unlist(lapply(rout, function(path) {
  grep(slow_pattern, readLines(path, warn = FALSE), value = TRUE)
})))
if (length(slow)) {
  seconds <- as.numeric(sub(slow_pattern, "\\1", slow))
  tests <- data.frame(
    seconds = sprintf("%.2f", seconds),
    test = sub(slow_pattern, "\\2", slow)
  )[order(-seconds), ]
  top <- utils::head(tests, n_tests)
  out <- c(
    out,
    sprintf(
      "### Slowest tests (%d of %d at or above 2 s; %.1f s in total)",
      nrow(top),
      nrow(tests),
      sum(seconds)
    ),
    "",
    md_table(top),
    ""
  )
}

if (length(log)) {
  out <- c(
    out,
    "<details><summary>00check.log</summary>",
    "",
    "```",
    log,
    "```",
    "",
    "</details>",
    ""
  )
}

cat(out, sep = "\n")
