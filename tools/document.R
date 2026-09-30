# document.R
# ::rtemis::
# 2026- EDG rtemis.org

# Run roxygen2 and exit with an error if it reported any problem.
#
# roxygen2 reports unresolved links, mismatched @param tags and similar defects
# as messages, so roxygenize() succeeds regardless and a problem scrolls past.
# This counts every message roxygen2 marks as an error or warning, and every R
# warning, while printing all output unchanged.

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
if (problems > 0L) {
  message("roxygen2 reported ", problems, " problem(s) above. Fix them.")
  quit(status = 1L)
}
