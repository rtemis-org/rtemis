# audit_props.R
# ::rtemis::
# 2026- EDG rtemis.org

# Runner for the property documentation audit (see R/utils_audit_props.R).
# Compares each prop_*-declared property's roxygen `@param` type declaration
# against its enforced `PropertySpec` and prints a ranked report.
#
# Run with: Rscript data-raw/audit_props.R [R_DIR]

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
r_dir <- if (length(args) >= 1L) args[[1L]] else "R"

findings <- audit_prop_docs(r_dir)

if (nrow(findings) == 0L) {
  cat("No findings: every documented contract matches its PropertySpec.\n")
} else {
  cat(
    "Property documentation audit:",
    nrow(findings),
    "finding(s) across",
    length(unique(findings[["class"]])),
    "class(es).\n\n"
  )
  cat("By check (most serious first):\n")
  counts <- table(factor(findings[["check"]], levels = AUDIT_SEVERITY))
  for (nm in names(counts)) {
    if (counts[[nm]] > 0L) {
      cat(sprintf("  %-14s %d\n", nm, counts[[nm]]))
    }
  }
  cat("\n")
  for (check in AUDIT_SEVERITY) {
    rows <- findings[findings[["check"]] == check, , drop = FALSE]
    if (nrow(rows) == 0L) {
      next
    }
    cat("## ", check, " (", nrow(rows), ")\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      cat(sprintf(
        "  %s@%s\n      documented: %s\n      declared:   %s\n",
        rows[["class"]][[i]],
        rows[["property"]][[i]],
        rows[["documented"]][[i]],
        rows[["declared"]][[i]]
      ))
    }
    cat("\n")
  }
}

invisible(findings)
