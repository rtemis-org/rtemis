# result-rules.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
oracle <- new.env(parent = asNamespace("rtemis"))
sys.source("tests/testthat/fixtures/result-rule-cases.R", oracle)
urls <- schema_reference_urls(schema_catalog(), "https://schema.rtemis.org")
cases <- lapply(oracle[["result_rule_cases"]](), function(case) {
  cls <- S7_class(case[["base"]])
  object <- oracle[["result_rule_probe"]](case)
  fields <- Filter(prop_published, cls@properties)
  document <- lapply(names(fields), function(nm) {
    wire_value(prop(object, nm), fields[[nm]])
  })
  names(document) <- names(fields)
  errors <- validate_class_rules(object, schema_rules(cls))
  actual <- sort(sub("^\\[([^]]+)\\].*$", "\\1", errors))
  stopifnot(identical(actual, case[["ids"]]))
  list(
    id = case[["id"]],
    schema = unname(urls[[paste0(cls@package, "::", cls@name)]]),
    document = S7_to_list(document),
    semantic_ids = I(actual)
  )
})
jsonlite::write_json(
  list(cases = cases),
  args[[1L]],
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null",
  digits = NA
)
cat(length(cases), "supervised result cases exported\n")
