# 024_SemanticRules.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% CompareFields ----
#' Require one scalar number not to exceed another
#' @field left,right Character: Numeric property names.
#' @field conditions List of `SchemaPredicate` objects: Conditions enabling the rule.
#' @keywords internal
#' @noRd
CompareFields <- new_class(
  "CompareFields",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    left = prop_string(
      "",
      description = "Numeric property bounded above by the other property."
    ),
    right = prop_string(
      "",
      description = "Numeric property supplying the upper bound."
    ),
    conditions = prop_collection(
      SchemaPredicate,
      description = "Conditions enabling the comparison."
    )
  )
)


# %% SumBound ----
#' Bound the smallest sum available from numeric candidate domains
#' @field properties Character vector: Numeric property names.
#' @field maximum Numeric: Inclusive upper bound for the sum.
#' @field conditions List of `SchemaPredicate` objects: Conditions enabling the rule.
#' @keywords internal
#' @noRd
SumBound <- new_class(
  "SumBound",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    properties = prop_string(
      c("left", "right"),
      vector = TRUE,
      min_items = 2L,
      unique_items = TRUE,
      description = "Numeric properties whose minimum candidate values are summed."
    ),
    maximum = prop_float(
      1,
      description = "Inclusive upper bound for the smallest reachable sum."
    ),
    conditions = prop_collection(
      SchemaPredicate,
      description = "Conditions enabling the sum bound."
    )
  ),
  validator = function(self) {
    if (!is.finite(self@maximum)) "@maximum must be finite."
  }
)


# %% LengthMatches ----
#' Match an array length to a fixed integer property
#' @field values,count Character: Array and integer property names.
#' @keywords internal
#' @noRd
LengthMatches <- new_class(
  "LengthMatches",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    values = prop_string(
      "",
      description = "Array whose length is constrained."
    ),
    count = prop_string(
      "",
      description = "Fixed integer giving the required length."
    )
  )
)


# %% SubsetOf ----
#' Require one string array to be a subset of another when both are set
#' @field subset,superset Character: String array property names.
#' @keywords internal
#' @noRd
SubsetOf <- new_class(
  "SubsetOf",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    subset = prop_string(
      "",
      description = "Array whose entries must occur in the other array."
    ),
    superset = prop_string(
      "",
      description = "Array of permitted entries; unset imposes no restriction."
    )
  )
)


# %% NonIncreasing ----
#' Require an array to be non-increasing
#' @field property Character: Numeric array property name.
#' @keywords internal
#' @noRd
NonIncreasing <- new_class(
  "NonIncreasing",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    property = prop_string(
      "",
      description = "Numeric array whose successive entries may not increase."
    )
  )
)


# %% PresenceRule ----
#' Require a value under a condition and optionally forbid it otherwise
#' @field property Character: Property whose presence is constrained.
#' @field when `SchemaPredicate`: Condition requiring a non-null value.
#' @field forbid_otherwise Logical: Whether the value must be unset outside the condition.
#' @keywords internal
#' @noRd
PresenceRule <- new_class(
  "PresenceRule",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    property = prop_string(
      "",
      description = "Property required to have a non-null value when the condition holds."
    ),
    when = prop_object(
      SchemaPredicate,
      description = "Condition requiring the property to be set."
    ),
    forbid_otherwise = prop_boolean(
      FALSE,
      description = "Whether the property must be unset outside the condition."
    )
  )
)


# %% NonEmptyStrings ----
#' Require observed string values to contain characters
#' @field properties Character vector: String property names.
#' @keywords internal
#' @noRd
NonEmptyStrings <- new_class(
  "NonEmptyStrings",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    properties = prop_string(
      "value",
      vector = TRUE,
      unique_items = TRUE,
      description = "Observed string properties that must contain characters."
    )
  )
)


# %% validate_relation_declaration ----
#' Validate the property references and value shapes of a relational rule
#' @param rule Named list: Portable rule fields.
#' @param cls S7 class: Declaring class.
#' @param fail Function: Raise a declaration error.
#' @return NULL.
#' @keywords internal
#' @noRd
validate_relation_declaration <- function(rule, cls, fail) {
  require_spec <- function(
    nm,
    types = PROP_TYPES,
    container = "none",
    tunable = FALSE,
    flat = TRUE
  ) {
    if (!nzchar(nm) || grepl(".", nm, fixed = TRUE)) {
      fail(
        "relational property names must be non-empty and contain no periods."
      )
    }
    spec <- get_spec(cls@properties[[nm]])
    if (
      is.null(spec) ||
        !spec@type %in% types ||
        spec@container != container ||
        (!tunable && spec@tunable) ||
        spec@broadcast ||
        (flat && !is.null(spec@items)) ||
        !is.null(spec@target_class)
    ) {
      fail(paste0(
        "@",
        nm,
        " has an unsupported type or value shape for ",
        rule[["kind"]],
        "."
      ))
    }
    spec
  }
  switch(
    rule[["kind"]],
    CompareFields = {
      require_spec(rule[["left"]], c("integer", "number"))
      require_spec(rule[["right"]], c("integer", "number"))
      if (rule[["left"]] == rule[["right"]]) {
        fail("comparison must name distinct properties.")
      }
    },
    SumBound = for (nm in rule[["properties"]]) {
      require_spec(nm, c("integer", "number"), tunable = TRUE)
    },
    LengthMatches = {
      require_spec(rule[["values"]], container = "array", flat = FALSE)
      require_spec(rule[["count"]], "integer", tunable = TRUE)
    },
    SubsetOf = {
      require_spec(rule[["subset"]], "string", "array")
      require_spec(rule[["superset"]], "string", "array")
      if (rule[["subset"]] == rule[["superset"]]) {
        fail("subset must name distinct properties.")
      }
    },
    NonIncreasing = require_spec(
      rule[["property"]],
      c("integer", "number"),
      "array"
    ),
    PresenceRule = {
      spec <- require_spec(rule[["property"]])
      if (!spec@nullable) {
        fail("a presence constraint requires a nullable property.")
      }
      if (rule[["property"]] == rule[["when"]][["property"]]) {
        fail("a presence condition must name another property.")
      }
    },
    NonEmptyStrings = for (nm in rule[["properties"]]) {
      if (require_spec(nm, "string")@nullable) {
        fail("non-empty string constraints require nonnullable properties.")
      }
    },
    fail("unsupported rule kind.")
  )
  NULL
}


# %% relation_rule_fails ----
#' Evaluate a typed relation on validated R property values
#' @param self S7 object: Object under validation.
#' @param rule Named list: Portable rule fields.
#' @return Logical scalar indicating a violation.
#' @keywords internal
#' @noRd
relation_rule_fails <- function(self, rule) {
  value <- function(nm) prop(self, nm)
  if (
    length(rule[["conditions"]]) &&
      !all(vapply(
        rule[["conditions"]],
        function(p) matches_rule_predicate(value(p[["property"]]), p),
        logical(1L)
      ))
  ) {
    return(FALSE)
  }
  switch(
    rule[["kind"]],
    CompareFields = !is.null(value(rule[["left"]])) &&
      !is.null(value(rule[["right"]])) &&
      value(rule[["left"]]) > value(rule[["right"]]),
    SumBound = {
      values <- lapply(rule[["properties"]], function(nm) {
        candidate_values(value(nm))
      })
      if (any(vapply(values, is.null, logical(1L)))) {
        FALSE
      } else {
        sum(vapply(values, min, numeric(1L))) > rule[["maximum"]]
      }
    },
    LengthMatches = !is.null(value(rule[["values"]])) &&
      !is.null(value(rule[["count"]])) &&
      (is_candidates(value(rule[["count"]])) ||
        length(value(rule[["values"]])) != value(rule[["count"]])),
    SubsetOf = !is.null(value(rule[["subset"]])) &&
      !is.null(value(rule[["superset"]])) &&
      !all(value(rule[["subset"]]) %in% value(rule[["superset"]])),
    NonIncreasing = !is.null(value(rule[["property"]])) &&
      is.unsorted(rev(value(rule[["property"]]))),
    PresenceRule = {
      active <- matches_rule_predicate(
        value(rule[["when"]][["property"]]),
        rule[["when"]]
      )
      if (active) {
        is.null(value(rule[["property"]]))
      } else {
        rule[["forbid_otherwise"]] && !is.null(value(rule[["property"]]))
      }
    },
    NonEmptyStrings = any(vapply(
      rule[["properties"]],
      function(nm) !nzchar(value(nm)),
      logical(1L)
    )),
    rtemis.core::abort(
      "Unsupported relation kind.",
      class = "rtemis_schema_error"
    )
  )
}


# %% predicate_schema ----
#' Emit a predicate for the scalar and candidate representations a property accepts
#' @param predicate Named list: Predicate fields.
#' @param spec `PropertySpec`: Declared property shape.
#' @return JSON Schema fragment.
#' @keywords internal
#' @noRd
predicate_schema <- function(predicate, spec) {
  scalar <- if (!is.null(predicate[["minimum"]])) {
    list(type = "number", minimum = predicate[["minimum"]])
  } else if (!is.null(predicate[["maximum"]])) {
    list(type = "number", maximum = predicate[["maximum"]])
  } else if (!is.null(predicate[["exclusive_maximum"]])) {
    list(type = "number", exclusiveMaximum = predicate[["exclusive_maximum"]])
  } else {
    list(const = predicate[["equals"]])
  }
  if (!spec@tunable) {
    return(scalar)
  }
  candidates <- list(type = "array", minItems = 1L)
  candidates[[
    if (predicate[["quantifier"]] == "all") "items" else "contains"
  ]] <- scalar
  list(
    anyOf = list(
      scalar,
      list(
        type = "object",
        required = I("candidates"),
        properties = list(candidates = candidates)
      )
    )
  )
}


# %% relation_rule_clauses ----
#' Emit the standard JSON Schema portion of a relational rule
#' @param rule Named list: Portable rule fields.
#' @param cls S7 class: Declaring class.
#' @return List of JSON Schema clauses.
#' @keywords internal
#' @noRd
relation_rule_clauses <- function(rule, cls) {
  if (rule[["kind"]] == "NonEmptyStrings") {
    return(list(list(
      `$comment` = rule[["id"]],
      properties = stats::setNames(
        rep(list(list(minLength = 1L)), length(rule[["properties"]])),
        rule[["properties"]]
      )
    )))
  }
  if (rule[["kind"]] == "PresenceRule") {
    antecedent <- list(
      required = I(rule[["when"]][["property"]]),
      properties = stats::setNames(
        list(predicate_schema(
          rule[["when"]],
          get_spec(cls@properties[[rule[["when"]][["property"]]]])
        )),
        rule[["when"]][["property"]]
      )
    )
    out <- list(
      `$comment` = rule[["id"]],
      `if` = antecedent,
      then = list(
        properties = stats::setNames(
          list(list(not = list(type = "null"))),
          rule[["property"]]
        )
      )
    )
    if (rule[["forbid_otherwise"]]) {
      out[["else"]] <- list(
        `if` = list(required = I(rule[["when"]][["property"]])),
        then = list(
          properties = stats::setNames(
            list(list(type = "null")),
            rule[["property"]]
          )
        )
      )
    }
    return(list(out))
  }
  list()
}


# %% rule_logic_node ----
#' Build one JSONLogic operation
#' @param op Character: Standard JSONLogic operator.
#' @param ... Expressions or literal arguments.
#' @return Named list containing the operation.
#' @keywords internal
#' @noRd
rule_logic_node <- function(op, ...) stats::setNames(list(list(...)), op)


# %% rule_logic_var ----
#' Read one JSONLogic path with explicit null for a missing value
#' @param path Character: Dotted property path.
#' @return JSONLogic expression.
#' @keywords internal
#' @noRd
rule_logic_var <- function(path) rule_logic_node("var", path, NULL)


# %% predicate_logic ----
#' Compile a predicate to standard JSONLogic with explicit candidate quantification
#' @param predicate Named list: Predicate fields.
#' @return Boolean JSONLogic expression.
#' @keywords internal
#' @noRd
predicate_logic <- function(predicate) {
  op <- rule_logic_node
  v <- rule_logic_var
  scalar <- function(value) {
    if (!is.null(predicate[["minimum"]])) {
      op(">=", value, predicate[["minimum"]])
    } else if (!is.null(predicate[["maximum"]])) {
      op("<=", value, predicate[["maximum"]])
    } else if (!is.null(predicate[["exclusive_maximum"]])) {
      op("<", value, predicate[["exclusive_maximum"]])
    } else {
      op("===", value, predicate[["equals"]])
    }
  }
  path <- predicate[["property"]]
  domain <- v(paste0(path, ".candidates"))
  reduced <- op(
    "reduce",
    domain,
    op(
      if (predicate[["quantifier"]] == "all") "and" else "or",
      v("accumulator"),
      scalar(v("current"))
    ),
    predicate[["quantifier"]] == "all"
  )
  op(
    "and",
    op("!==", v(path), NULL),
    op("if", op("!==", domain, NULL), reduced, scalar(v(path)))
  )
}


# %% relation_rule_logic ----
#' Compile a document-internal relation to a Boolean JSONLogic violation
#'
#' Expressions read the document directly, use strict equality, and guard null
#' before arithmetic or iteration. Reductions retain outer operands in their
#' initial accumulator, so nested scopes never depend on a port-specific lookup.
#'
#' @param rule Named list: Portable rule fields.
#' @return Boolean JSONLogic expression, or NULL for rules wholly expressed by schema.
#' @keywords internal
#' @noRd
relation_rule_logic <- function(rule) {
  op <- rule_logic_node
  v <- rule_logic_var
  present <- function(nm) op("!==", v(nm), NULL)
  count <- function(x) op("reduce", x, op("+", v("accumulator"), 1L), 0L)
  last <- function(x) op("reduce", x, v("current"), NULL)
  conditions <- lapply(rule[["conditions"]], predicate_logic)
  expr <- switch(
    rule[["kind"]],
    CompareFields = op(
      "and",
      present(rule[["left"]]),
      present(rule[["right"]]),
      op(">", v(rule[["left"]]), v(rule[["right"]]))
    ),
    SumBound = {
      smallest <- lapply(rule[["properties"]], function(nm) {
        domain <- v(paste0(nm, ".candidates"))
        op(
          "if",
          op("!==", domain, NULL),
          op(
            "reduce",
            domain,
            op(
              "if",
              op("===", v("accumulator"), NULL),
              v("current"),
              op("min", v("accumulator"), v("current"))
            ),
            NULL
          ),
          v(nm)
        )
      })
      do.call(
        op,
        c(
          list("and"),
          lapply(rule[["properties"]], present),
          list(op(">", do.call(op, c(list("+"), smallest)), rule[["maximum"]]))
        )
      )
    },
    LengthMatches = op(
      "and",
      present(rule[["values"]]),
      present(rule[["count"]]),
      op(
        "or",
        present(paste0(rule[["count"]], ".candidates")),
        op("!==", count(v(rule[["values"]])), v(rule[["count"]]))
      )
    ),
    SubsetOf = {
      reduced <- op(
        "reduce",
        v(rule[["subset"]]),
        list(
          v("accumulator.0"),
          op(
            "and",
            v("accumulator.1"),
            op("in", v("current"), v("accumulator.0"))
          )
        ),
        list(v(rule[["superset"]]), TRUE)
      )
      op(
        "and",
        present(rule[["subset"]]),
        present(rule[["superset"]]),
        op("!", last(reduced))
      )
    },
    NonIncreasing = {
      reduced <- op(
        "reduce",
        v(rule[["property"]]),
        list(
          v("current"),
          op(
            "and",
            v("accumulator.1"),
            op(
              "or",
              op("===", v("accumulator.0"), NULL),
              op("<=", v("current"), v("accumulator.0"))
            )
          )
        ),
        list(NULL, TRUE)
      )
      op("and", present(rule[["property"]]), op("!", last(reduced)))
    },
    PresenceRule = op(
      "if",
      predicate_logic(rule[["when"]]),
      op("===", v(rule[["property"]]), NULL),
      if (rule[["forbid_otherwise"]]) present(rule[["property"]]) else FALSE
    ),
    NULL
  )
  if (is.null(expr)) {
    return(NULL)
  }
  if (length(conditions)) {
    do.call(op, c(list("and"), conditions, list(expr)))
  } else {
    expr
  }
}


# %% class_document_rules ----
#' Publish the semantic phase of a class's declarative contract
#' @param cls S7 class: Class whose contract is emitted.
#' @return NULL or a JSONLogic evaluation contract and named rules.
#' @keywords internal
#' @noRd
class_document_rules <- function(cls) {
  rules <- Filter(
    Negate(is.null),
    lapply(schema_rules(cls), function(rule) {
      logic <- relation_rule_logic(rule)
      if (is.null(logic)) {
        return(NULL)
      }
      list(id = rule[["id"]], message = rule[["message"]], condition = logic)
    })
  )
  if (!length(rules)) {
    return(NULL)
  }
  list(
    expression_language = "jsonlogic",
    phase = "resolved",
    evaluation = "Evaluate each condition against the schema-valid document after resolving the separately published defaults. A true condition is a violation. Missing paths yield null. Equality is strict and conditional operands are Boolean.",
    rules = rules
  )
}
