# document.R
# ::rtemis::
# 2026- EDG rtemis.org

# Run roxygen2 and exit with an error if it reported any problem.
#
# roxygen2 reports unresolved links, mismatched @param tags and similar defects
# as messages, so roxygenize() succeeds regardless and a problem scrolls past.
# This counts every message roxygen2 marks as an error or warning, and every R
# warning, while printing all output unchanged. It then counts every export
# without an Rd alias, which R CMD check reports as "Undocumented code objects".

problems <- 0L
withCallingHandlers(
  roxygen2::roxygenize(),
  message = function(m) {
    text <- conditionMessage(m)
    if (startsWith(text, cli::symbol[["cross"]]) || startsWith(text, "!")) {
      problems <<- problems + 1L
    }
  },
  warning = function(w) {
    problems <<- problems + 1L
  }
)
pkg_dir <- normalizePath(".")
exports <- parseNamespaceFile(basename(pkg_dir), dirname(pkg_dir))[["exports"]]
rd_lines <- unlist(lapply(
  list.files("man", pattern = "[.]Rd$", full.names = TRUE),
  readLines
))
aliases <- sub(
  "^\\\\alias\\{(.*)\\}$",
  "\\1",
  grep("^\\\\alias\\{", rd_lines, value = TRUE)
)
aliases <- gsub("\\\\(.)", "\\1", aliases)
undocumented <- setdiff(exports, aliases)
if (length(undocumented) > 0L) {
  message(
    "! Exported without an Rd file (remove @noRd): ",
    paste(undocumented, collapse = ", ")
  )
  problems <- problems + length(undocumented)
}
if (problems > 0L) {
  message("roxygen2 reported ", problems, " problem(s) above. Fix them.")
  quit(status = 1L)
}
