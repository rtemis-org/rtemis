# test_ResultValues.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("typed unions preserve alternatives and declaration defaults", {
  property <- prop_union(list(
    prop_array(prop_float(NULL, nullable = TRUE)),
    prop_factor(allow_missing = TRUE)
  ))
  spec <- get_spec(property)
  schema <- prop_to_schema(property)
  defaults <- default_declarations(spec, schema, "/properties/value")
  restored <- schema_to_spec(
    schema,
    declarations = defaults,
    path = "/properties/value"
  )
  expect_identical(spec_fields(restored), spec_fields(spec))
  expect_identical(
    names(defaults),
    c(
      "/properties/value",
      "/properties/value/anyOf/0",
      "/properties/value/anyOf/0/items",
      "/properties/value/anyOf/1"
    )
  )
  expect_error(prop_union(list(prop_float(0))), "at least two")
  expect_error(
    prop_union(list(prop_float(0), prop_string(NULL, nullable = TRUE))),
    "nonnullable"
  )
  expect_error(prop_union(list(prop_float(0), class_factor)), "alternatives")
  Demo <- new_class("UnionValues", properties = list(value = property))
  expect_error(Demo(), "explicit value")
  expect_error(Demo(value = TRUE))
  expect_error(Demo(value = "yes"))
  values <- list(
    c(1, NA_real_, 3),
    factor(c("yes", NA, "no"), levels = c("yes", "no", "unused")),
    factor("only"),
    c(NA_real_)
  )
  for (value in values) {
    object <- Demo(value = value)
    document <- jsonlite::fromJSON(
      jsonlite::toJSON(
        S7_to_list(object),
        auto_unbox = TRUE,
        na = "null",
        null = "null"
      ),
      simplifyVector = FALSE
    )
    expect_identical(do.call(Demo, from_wire(document, Demo))@value, value)
  }
  Ambiguous <- new_class(
    "OverlappingUnion",
    properties = list(value = prop_union(list(prop_float(0), prop_integer(0L))))
  )
  expect_no_error(Ambiguous(value = 1L))
  expect_no_error(Ambiguous(value = 1.0))
  expect_no_error(Ambiguous(value = 1.5))
})


test_that("categorical values preserve missing positions and reject invalid codes", {
  Demo <- new_class(
    "CategoricalValues",
    properties = list(value = prop_factor(allow_missing = TRUE))
  )
  factor_value <- factor(c("b", NA, "a"), levels = c("b", "a", "unused"))
  expect_identical(Demo(value = factor_value)@value, factor_value)
  valid <- list(levels = list("b", "a", "unused"), codes = list(1L, NULL, 2L))
  expect_identical(from_wire_factor(valid), factor_value)
  for (codes in list(
    list(0L),
    list(-1L),
    list(4L),
    list(1.5),
    list(TRUE),
    list("1"),
    list(),
    list(a = 1L)
  )) {
    expect_error(from_wire_factor(list(
      levels = valid[["levels"]],
      codes = codes
    )))
  }
  for (levels in list(
    list("a", "a"),
    list(NULL),
    list(TRUE),
    list(),
    list(a = "x")
  )) {
    expect_error(from_wire_factor(list(levels = levels, codes = list(1L))))
  }
  expect_error(from_wire_factor(list(levels = "a", codes = 1L)), "arrays")
  schema <- prop_to_schema(Demo@properties[["value"]])
  expect_true(schema[["x-rtemis"]][["allow_missing"]])
  expect_setequal(
    as.character(schema[["properties"]][["codes"]][["items"]][["type"]]),
    c("integer", "null")
  )
})


test_that("union selection and required conditions agree with JSON Schema", {
  Choice <- schema_class(
    "SelectedOutcome",
    package = "rtemis",
    properties = list(
      type = prop_string(
        NULL,
        nullable = TRUE,
        enum = c("numeric", "categorical")
      ),
      value = prop_union(
        list(
          prop_array(prop_float(NULL, nullable = TRUE)),
          prop_factor(allow_missing = TRUE)
        ),
        nullable = TRUE
      )
    ),
    rules = list(
      UnionSelectionRule(
        id = "choice.numeric",
        message = "Use numeric outcomes.",
        property = "value",
        alternative = 1L,
        when = SchemaPredicate(property = "type", equals = "numeric")
      ),
      UnionSelectionRule(
        id = "choice.categorical",
        message = "Use categorical outcomes.",
        property = "value",
        alternative = 2L,
        when = SchemaPredicate(property = "type", equals = "categorical")
      )
    )
  )
  schema <- S7_to_JSONSchema(
    Choice,
    id = "https://example.test/choice",
    asserted = TRUE
  )
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
    engine = "ajv"
  )
  values <- list(
    numeric = c(1, 2),
    categorical = factor(c("b", "a"), levels = c("b", "a"))
  )
  for (type in names(values)) {
    expect_no_error(Choice(type = type, value = values[[type]]))
    other <- setdiff(names(values), type)
    expect_error(
      Choice(type = type, value = values[[other]]),
      paste0("choice.", type),
      fixed = TRUE
    )
    for (value in list(NULL, values[[type]], values[[other]])) {
      document <- list(
        type = type,
        value = wire_value(value, Choice@properties[["value"]])
      )
      expected <- is.null(value) || identical(value, values[[type]])
      expect_identical(
        validate(jsonlite::toJSON(
          document,
          auto_unbox = TRUE,
          null = "null",
          na = "null"
        )),
        expected
      )
    }
  }
  Required <- schema_class(
    "RequiredType",
    package = "rtemis",
    parent = Choice,
    rules = list(RequireConditions(
      id = "required.type",
      message = "Select numeric type.",
      conditions = list(SchemaPredicate(property = "type", equals = "numeric"))
    ))
  )
  expect_no_error(Required(type = "numeric", value = c(1, 2)))
  expect_error(
    Required(type = "categorical", value = values[["categorical"]]),
    "required.type",
    fixed = TRUE
  )
  expect_error(
    Required(type = NULL, value = NULL),
    "required.type",
    fixed = TRUE
  )
  for (rule in schema_rules(Required)) {
    expect_identical(schema_rule_fields(schema_rule_from_fields(rule)), rule)
  }
  expect_error(
    schema_class(
      "BadSelection",
      properties = Choice@properties,
      rules = list(UnionSelectionRule(
        id = "bad.alternative",
        message = "Select an existing alternative.",
        property = "value",
        alternative = 3L,
        when = SchemaPredicate(property = "type", equals = "numeric")
      ))
    ),
    "existing alternative"
  )
})


test_that("probability columns have a canonical categorical interpretation", {
  probabilities <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.6, 0.4),
    nrow = 2L,
    dimnames = list(NULL, c("c", "a", "b"))
  )
  normalized <- prob_matrix(probabilities, c("a", "b", "c"))
  expect_identical(colnames(normalized), c("a", "b", "c"))
  expect_identical(normalized, probabilities[, c("a", "b", "c"), drop = FALSE])
  binary <- matrix(
    c(0.2, 0.3, 0.8, 0.7),
    nrow = 2L,
    dimnames = list(NULL, c("yes", "no"))
  )
  expect_identical(
    prob_matrix(binary, c("no", "yes")),
    binary[, "yes", drop = FALSE]
  )
  expect_identical(
    prob_matrix(binary, c("no", "yes"), binclasspos = 1L),
    binary[, "no", drop = FALSE]
  )
  expect_error(prob_matrix(matrix(0.5, ncol = 1L), c("a", "b", "c")), "columns")
  colnames(probabilities) <- c("a", "a", "b")
  expect_error(prob_matrix(probabilities, c("a", "b", "c")), "once")
})


test_that("inherited class rules reconstruct once and reject inconsistent children", {
  Parent <- schema_class(
    "RuleParent",
    properties = list(kind = prop_string("a", enum = c("a", "b"))),
    rules = list(RequireConditions(
      id = "parent.kind",
      message = "Use kind a.",
      conditions = list(SchemaPredicate(property = "kind", equals = "a"))
    ))
  )
  Child <- schema_class("RuleChild", parent = Parent)
  schema <- S7_to_JSONSchema(Child, id = "https://example.test/child")
  Restored <- JSONSchema_to_S7(
    schema,
    defaults = list(kind = "a"),
    parent = Parent,
    name = "RestoredChild"
  )
  expect_identical(schema_rules(Restored), schema_rules(Parent))
  expect_no_error(Restored())
  expect_error(Restored(kind = "b"), "parent.kind", fixed = TRUE)
  changed <- schema
  changed[["x-rtemis"]][["rules"]][[1L]][["message"]] <- "Changed rule."
  expect_error(
    JSONSchema_to_S7(changed, defaults = list(kind = "a"), parent = Parent),
    "changes an inherited rule"
  )
  changed[["x-rtemis"]][["rules"]] <- list()
  expect_error(
    JSONSchema_to_S7(changed, defaults = list(kind = "a"), parent = Parent),
    "missing inherited rules"
  )
  condition <- SchemaPredicate(property = "kind", equals = "a")
  expect_error(
    RequireConditions(
      id = "duplicate.kind",
      message = "Use one condition.",
      conditions = list(condition, condition)
    ),
    "distinct"
  )
})


test_that("union readers retain already decoded native values", {
  Demo <- new_class(
    "NativeUnion",
    properties = list(
      value = prop_union(list(
        prop_array(prop_float(NULL, nullable = TRUE)),
        prop_factor(allow_missing = TRUE)
      ))
    )
  )
  for (value in list(
    c(1, NA_real_),
    factor(c("b", NA), levels = c("b", "a"))
  )) {
    expect_identical(from_wire(list(value = value), Demo)[["value"]], value)
  }
})


test_that("nested union references use the supplied publication graph", {
  Target <- schema_class(
    "NestedTarget",
    package = "test",
    properties = list(value = prop_integer(1L))
  )
  union <- prop_union(list(prop_object(Target), prop_string("x")))
  property <- prop_struct(list(value = union), nullable = TRUE)
  references <- c("test::NestedTarget" = "https://example.test/target")
  schema <- spec_to_schema(get_spec(property), reference_urls = references)
  expect_identical(
    schema[["properties"]][["value"]][["anyOf"]][[1L]][["$ref"]],
    references[[1L]]
  )
})
