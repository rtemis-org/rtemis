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
