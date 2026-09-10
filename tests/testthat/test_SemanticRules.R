# test_SemanticRules.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("typed rules preserve native validation across boundary and candidate cases", {
  oracle <- new.env(parent = asNamespace("rtemis"))
  sys.source(test_path("fixtures", "schema-native-validators.R"), oracle)
  sys.source(test_path("fixtures", "schema-rule-cases.R"), oracle)
  classes <- unique(vapply(
    oracle$schema_rule_cases(),
    `[[`,
    character(1L),
    "class"
  ))
  originals <- lapply(classes, function(nm) get(nm, asNamespace("rtemis")))
  names(originals) <- classes
  legacy <- lapply(classes, function(nm) {
    S7::new_class(
      paste0("Oracle", nm),
      properties = originals[[nm]]@properties,
      validator = oracle$.legacy_validators[[nm]]
    )
  })
  names(legacy) <- classes
  rebuilt <- lapply(originals, function(cls) {
    schema <- S7_to_JSONSchema(
      cls,
      id = "https://example.org/contract/schema.json",
      base = family_base(cls)
    )
    defaults <- lapply(Filter(prop_published, cls@properties), function(p) {
      get_spec_fields(p)[["default"]]
    })
    # These classes contain primitive declarations and one nullable reference.
    defaults <- defaults[names(schema$properties)]
    defaults <- defaults[!vapply(defaults, is.language, logical(1L))]
    JSONSchema_to_S7(
      jsonlite::fromJSON(
        jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
        simplifyVector = FALSE
      ),
      defaults = defaults
    )
  })
  for (case in oracle$schema_rule_cases()) {
    call <- function(cls) {
      tryCatch(
        {
          do.call(cls, case$arguments)
          TRUE
        },
        error = function(e) FALSE
      )
    }
    expected <- case$expected %||% call(legacy[[case$class]])
    label <- paste(case$class, paste(names(case$arguments), collapse = ","))
    expect_identical(call(originals[[case$class]]), expected, info = label)
    expect_identical(call(rebuilt[[case$class]]), expected, info = label)
  }
})


test_that("rule declarations reject incompatible shapes before construction", {
  rule <- CompareFields(
    id = "test.comparison",
    left = "x",
    right = "y",
    message = "Choose valid counts."
  )
  expect_error(
    schema_class(
      "BadComparison",
      properties = list(
        x = prop_integer(1L, tunable = TRUE),
        y = prop_integer(2L)
      ),
      rules = list(rule)
    ),
    "unsupported type"
  )
  rule <- LengthMatches(
    id = "test.length",
    values = "x",
    count = "missing",
    message = "Match the length."
  )
  expect_error(
    schema_class(
      "BadLength",
      properties = list(x = prop_integer(1L, vector = TRUE)),
      rules = list(rule)
    ),
    "unsupported type"
  )
})


test_that("relational declarations reject ambiguous names and nested value shapes", {
  compare <- CompareFields(
    id = "test.comparison",
    left = "a.b",
    right = "z",
    message = "Reduce a.b."
  )
  expect_error(
    schema_class(
      "DottedRule",
      properties = list(a.b = prop_float(1), z = prop_float(2)),
      rules = list(compare)
    ),
    "contain no periods"
  )
  expect_error(
    schema_class(
      "NullableStringRule",
      properties = list(x = prop_string(NULL, nullable = TRUE)),
      rules = list(NonEmptyStrings(
        id = "test.empty",
        properties = "x",
        message = "Set x."
      ))
    ),
    "nonnullable"
  )
  expect_error(
    schema_class(
      "NestedArrayRule",
      properties = list(
        x = prop_array(prop_float(1, vector = TRUE), nullable = TRUE)
      ),
      rules = list(NonIncreasing(
        id = "test.order",
        property = "x",
        message = "Order x."
      ))
    ),
    "unsupported type"
  )
})


test_that("serialized rule discriminators and literals have unambiguous JSON types", {
  expect_error(
    schema_rule_from_fields(list(kind = 1L)),
    "kind must be one string"
  )
  for (value in list(I(TRUE), factor("yes"), c(named = 1))) {
    expect_error(
      SchemaPredicate(property = "x", equals = value),
      "without attributes"
    )
  }
})


test_that("inherited rules validate every concrete property override", {
  Parent <- schema_class(
    "ComparisonParent",
    properties = list(x = prop_float(1), y = prop_float(2)),
    rules = list(CompareFields(
      id = "test.parent",
      left = "x",
      right = "y",
      message = "Reduce x."
    ))
  )
  for (replacement in list(
    prop_float(1, vector = TRUE),
    prop_float(1, tunable = TRUE)
  )) {
    expect_error(
      schema_class(
        "ComparisonChild",
        parent = Parent,
        properties = list(x = replacement)
      ),
      "unsupported type"
    )
  }
})


test_that("inherited property contracts match retained S7 validation", {
  Parent <- schema_class(
    "ScalarConflictParent",
    properties = list(x = prop_integer(1L, max = 4L), y = prop_integer(1L)),
    rules = list(ForbidTogether(
      id = "test.inherited-conflict",
      message = "Reduce one value.",
      conditions = list(
        SchemaPredicate(property = "x", minimum = 2),
        SchemaPredicate(property = "y", minimum = 2)
      )
    ))
  )
  for (replacement in list(
    prop_integer(1L, max = 4L, tunable = TRUE),
    prop_integer(1L, max = 5L),
    S7::new_property(S7::class_integer)
  )) {
    expect_error(
      schema_class(
        "InvalidConflictChild",
        parent = Parent,
        properties = list(x = replacement)
      ),
      "validation contract unchanged"
    )
  }
  Child <- schema_class(
    "ScalarConflictChild",
    parent = Parent,
    properties = list(
      x = prop_integer(2L, max = 4L, description = "An amount.")
    )
  )
  expect_identical(get_spec_fields(Child@properties$x)[["default"]], 2L)
  expect_identical(Child(x = 2L)@x, 2L)
  expect_error(Child(x = 2L, y = 2L), "test.inherited-conflict")
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(
      S7_to_JSONSchema(Child, id = "https://example.org/child/schema.json"),
      auto_unbox = TRUE,
      null = "null"
    ),
    engine = "ajv"
  )
  expect_true(validate('{"x":2,"y":1}'))
  expect_false(validate('{"x":2,"y":2}'))
  expect_false(validate('{"x":{"candidates":[1,3]},"y":1}'))
})
