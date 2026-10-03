# test_ResultRules.R
# ::rtemis::
# 2026- EDG rtemis.org

source(test_path("fixtures", "result-rule-cases.R"), local = TRUE)

test_that("result relations enforce independent native boundary cases", {
  covered <- character()
  cases <- result_rule_cases()
  for (case in cases) {
    cls <- S7_class(case[["base"]])
    probe <- result_rule_probe(case)
    rules <- schema_rules(cls)
    errors <- validate_class_rules(probe, rules)
    ids <- sort(sub("^\\[([^]]+)\\].*$", "\\1", errors))
    expect_equal(ids, case[["ids"]], info = case[["id"]])
    value <- case[["base"]]
    if (length(case[["ids"]])) {
      expect_error(
        props(value) <- case[["changes"]],
        case[["ids"]][[1L]],
        fixed = TRUE
      )
    } else {
      expect_no_error(props(value) <- case[["changes"]])
    }
    covered <- union(covered, ids)
  }
  required <- Filter(
    function(rule) {
      rule[["kind"]] %in%
        c("RowCountMatches", "FactorLevelsMatch", "ProbabilityColumnsMatch")
    },
    schema_rules(Classification)
  )
  expect_setequal(covered, vapply(required, `[[`, character(1L), "id"))
})


test_that("result rules reject incompatible declarations and retain published metadata", {
  for (rule in list(
    RowCountMatches(
      id = "test.rows",
      left = "x",
      right = "y",
      message = "Match rows."
    ),
    FactorLevelsMatch(
      id = "test.levels",
      left = "x",
      right = "y",
      message = "Match levels."
    ),
    ProbabilityColumnsMatch(
      id = "test.columns",
      probabilities = "x",
      outcome = "y",
      message = "Match columns."
    )
  )) {
    expect_error(
      schema_class(
        "InvalidResultRelation",
        properties = list(x = prop_float(1), y = prop_float(2)),
        rules = list(rule)
      ),
      "unsupported type"
    )
    fields <- schema_rule_fields(rule)
    expect_identical(
      schema_rule_fields(schema_rule_from_fields(fields)),
      fields
    )
  }
})


test_that("artifact-reconstructed result relations enforce the same boundaries", {
  Original <- schema_class(
    "ResultRelationRoundtrip",
    properties = list(
      observed = prop_factor(),
      predicted = prop_factor(),
      probability = prop_matrix(items = prop_float(NULL, nullable = TRUE))
    ),
    rules = list(
      RowCountMatches(
        id = "result.rows",
        left = "observed",
        right = "predicted",
        message = "Match rows."
      ),
      FactorLevelsMatch(
        id = "result.levels",
        left = "observed",
        right = "predicted",
        message = "Match levels."
      ),
      ProbabilityColumnsMatch(
        id = "result.columns",
        probabilities = "probability",
        outcome = "observed",
        message = "Match columns."
      )
    )
  )
  artifact <- jsonlite::fromJSON(
    jsonlite::toJSON(
      S7_to_JSONSchema(Original, id = "https://example.test/result-relations"),
      auto_unbox = TRUE,
      null = "null"
    ),
    simplifyVector = FALSE
  )
  Restored <- JSONSchema_to_S7(artifact, name = "RestoredResultRelations")
  y <- factor(c("a", "b"), levels = c("a", "b"))
  for (cls in list(Original, Restored)) {
    expect_no_error(cls(
      observed = y,
      predicted = y,
      probability = matrix(0.5, 2, 1)
    ))
    expect_error(
      cls(observed = y, predicted = y[1], probability = matrix(0.5, 2, 1)),
      "result.rows",
      fixed = TRUE
    )
    expect_error(
      cls(
        observed = y,
        predicted = factor(y, levels = c("b", "a")),
        probability = matrix(0.5, 2, 1)
      ),
      "result.levels",
      fixed = TRUE
    )
    expect_error(
      cls(observed = y, predicted = y, probability = matrix(0.5, 2, 2)),
      "result.columns",
      fixed = TRUE
    )
  }
  expect_identical(schema_rules(Restored), schema_rules(Original))
})
