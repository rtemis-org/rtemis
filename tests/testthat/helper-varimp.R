# helper-varimp.R
# ::rtemis::
# 2026- EDG rtemis.org

# A `VariableImportance` from a wide table: one measure per column other than
# `variable`, each described as a test measure.
.varimp_from_table <- function(table) {
  ns <- asNamespace("rtemis")
  measures <- lapply(
    stats::setNames(nm = setdiff(names(table), "variable")),
    function(nm) {
      ns[["importance_measure"]](
        table[["variable"]],
        table[[nm]],
        kind = "split_gain",
        signed = TRUE,
        direction = "absolute",
        description = "Test measure."
      )
    }
  )
  ns[["VariableImportance"]](measures = measures)
}

# The categories and values of a bar drawing in drawn order. rtemis.draw's
# widget builds the bar option from the build request: one bar per row of the
# bound columns, in row order (`order = "input"`), listed top to bottom when
# horizontal.
drawn_bars <- function(widget) {
  build <- widget[["x"]][["build"]]
  data <- jsonlite::fromJSON(build[["data"]])[["columns"]]
  list(
    categories = data[[build[["config"]][["x"]]]],
    values = data[[build[["config"]][["y"]]]]
  )
}

# The ECharts option a drawing renders, built in Node.js by rtemis.draw when
# @rtemis/draw builds it, with every JSON array a list as an option built in R
# holds it.
drawn_option <- function(widget) {
  testthat::skip_if(!nzchar(Sys.which("node")), "Node.js not found")
  rtemis.draw::echarts_option(widget, simplify = FALSE)
}
