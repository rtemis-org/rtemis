# schema-rules.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
oracle <- new.env(parent = asNamespace("rtemis"))
sys.source("tests/testthat/fixtures/schema-native-validators.R", oracle)
sys.source("tests/testthat/fixtures/schema-rule-cases.R", oracle)
cases <- oracle$schema_rule_cases()
schemas <- list()
validators <- list()
surrogates <- list()
for (nm in unique(vapply(cases, `[[`, character(1L), "class"))) {
  cls <- get(nm, asNamespace("rtemis"))
  schemas[[nm]] <- S7_to_JSONSchema(
    cls,
    base = family_base(cls),
    id = paste0("https://example.org/", nm, "/schema.json")
  )
  # GridSearch's nullable resampler reference is never populated by this corpus.
  # The real referenced graph is checked separately by schema-graph.R.
  schema <- schemas[[nm]]
  if (nm == "GridSearchConfig") {
    schema$properties$resampler_config <- list(type = "null")
  }
  validators[[nm]] <- jsonvalidate::json_validator(
    jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
    engine = "ajv"
  )
  surrogates[[nm]] <- S7::new_class(
    paste0("RuleProbe", nm),
    properties = cls@properties
  )
}
rows <- lapply(seq_along(cases), function(i) {
  case <- cases[[i]]
  object <- do.call(surrogates[[case$class]], case$arguments)
  native <- tryCatch(
    oracle$.legacy_validators[[case$class]](object),
    error = identity
  )
  expected <- case$expected %||% !length(native)
  fields <- intersect(
    names(surrogates[[case$class]]@properties),
    names(schemas[[case$class]]$properties)
  )
  document <- stats::setNames(
    lapply(fields, function(nm) {
      wire_value(prop(object, nm), surrogates[[case$class]]@properties[[nm]])
    }),
    fields
  )
  wire <- jsonlite::toJSON(
    document,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  list(
    id = i,
    class = case$class,
    document = document,
    expected = expected,
    schema_valid = isTRUE(validators[[case$class]](wire)),
    oracle_error = if (inherits(native, "error")) conditionMessage(native),
    oracle = if (is.null(case$expected)) {
      "native-validator"
    } else {
      "ifw-truth-table"
    }
  )
})
document <- list(
  schemas = lapply(schemas, function(s) s[["x-rtemis"]][["validation"]]),
  cases = rows
)
jsonlite::write_json(
  document,
  args[[1L]],
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  digits = NA,
  pretty = TRUE
)
cat(length(rows), "native-oracle cases exported for the foreign runtime\n")
