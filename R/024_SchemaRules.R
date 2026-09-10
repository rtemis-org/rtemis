# 024_SchemaRules.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% SchemaPredicate ----
#' A scalar condition in a class rule
#' @field property Character: Property tested by the condition.
#' @field equals Optional scalar: Exact Boolean, numeric, or character value.
#' @field minimum Optional Numeric: Inclusive lower bound.
#' @field maximum Optional Numeric: Inclusive upper bound.
#' @field exclusive_maximum Optional Numeric: Exclusive upper bound.
#' @field quantifier Character \{"any", "all"\}: Candidate values that must satisfy the condition.
#' @keywords internal
#' @noRd
SchemaPredicate <- new_class(
  "SchemaPredicate",
  package = "rtemis",
  properties = list(
    property = prop_string(
      "",
      description = "Property tested by this condition."
    ),
    equals = class_any,
    minimum = prop_float(
      NULL,
      nullable = TRUE,
      description = "Inclusive lower bound for the condition."
    ),
    maximum = prop_float(
      NULL,
      nullable = TRUE,
      description = "Inclusive upper bound for the condition."
    ),
    exclusive_maximum = prop_float(
      NULL,
      nullable = TRUE,
      description = "Exclusive upper bound for the condition."
    ),
    quantifier = prop_string(
      "any",
      enum = c("any", "all"),
      description = "Candidate values that must satisfy the condition."
    )
  ),
  validator = function(self) {
    if (!nzchar(self@property)) {
      return("@property must be non-empty.")
    }
    if (
      sum(
        !vapply(
          list(self@equals, self@minimum, self@maximum, self@exclusive_maximum),
          is.null,
          logical(1L)
        )
      ) !=
        1L
    ) {
      return("declare exactly one equality or numeric bound.")
    }
    bounds <- c(self@minimum, self@maximum, self@exclusive_maximum)
    if (length(bounds) && any(!is.finite(bounds))) {
      return("numeric predicate bounds must be finite.")
    }
    value <- self@equals
    if (
      !is.null(value) &&
        (length(value) != 1L ||
          !is.null(attributes(value)) ||
          anyNA(value) ||
          !(is.logical(value) || is.numeric(value) || is.character(value)) ||
          (is.numeric(value) && !is.finite(value)))
    ) {
      return("@equals must be a finite, non-missing scalar without attributes.")
    }
    NULL
  }
)


# %% repr.SchemaPredicate ----
#' @keywords internal
#' @noRd
method(repr, SchemaPredicate) <- function(x, output_type = NULL, ...) {
  fmt(paste0("SchemaPredicate: ", x@property), output_type = output_type)
}


# %% SchemaRule ----
#' A named invariant declared by a class
#' @field id Character: Stable rule identity.
#' @field message Character: Corrective explanation of the invariant.
#' @keywords internal
#' @noRd
SchemaRule <- new_class(
  "SchemaRule",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    id = prop_string("", description = "Stable identity of the class rule."),
    message = prop_string(
      "",
      description = "Corrective explanation when this rule fails."
    )
  ),
  validator = function(self) {
    if (!grepl("^[a-z][a-z0-9_.-]*$", self@id)) {
      return("@id must be a lowercase rule identifier.")
    }
    if (!nzchar(self@message)) {
      return("@message must be non-empty.")
    }
    NULL
  }
)


# %% repr.SchemaRule ----
#' @keywords internal
#' @noRd
method(repr, SchemaRule) <- function(x, output_type = NULL, ...) {
  fmt(paste0("SchemaRule: ", x@id), output_type = output_type)
}


# %% ForbidTogether ----
#' Forbid the simultaneous satisfaction of scalar conditions
#' @field conditions List of `SchemaPredicate` objects: Conditions that must not all hold.
#' @keywords internal
#' @noRd
ForbidTogether <- new_class(
  "ForbidTogether",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    conditions = prop_collection(
      SchemaPredicate,
      min_items = 2L,
      description = "Conditions that must not all hold at once."
    )
  ),
  validator = function(self) {
    fields <- vapply(self@conditions, function(p) p@property, character(1L))
    if (anyDuplicated(fields)) {
      return("@conditions must name distinct properties.")
    }
    NULL
  }
)


# %% StatusValueRule ----
#' Pair metric availability with a row of statuses
#' @field values,statuses Character: Names of the value and status table properties.
#' @field computed_status Character: Status requiring a non-null value.
#' @keywords internal
#' @noRd
StatusValueRule <- new_class(
  "StatusValueRule",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    values = prop_string(
      "",
      description = "Property holding the row of metric values."
    ),
    statuses = prop_string(
      "",
      description = "Property holding the row of metric statuses."
    ),
    computed_status = prop_string(
      "computed",
      description = "Status that requires a non-null metric value."
    )
  )
)


# %% schema_rule_fields ----
#' Store a rule as portable fields without embedding S7 class definitions
#' @param rule `SchemaRule`: Validated rule declaration.
#' @return Named list of rule fields.
#' @keywords internal
#' @noRd
schema_rule_fields <- function(rule) {
  check_is_S7(rule, SchemaRule)
  out <- props(rule)
  rtemis.core::assert_description_language(
    list(description = rule@message),
    rule@id
  )
  out[["kind"]] <- S7_class(rule)@name
  if ("conditions" %in% names(out)) {
    out$conditions <- lapply(rule@conditions, props)
  }
  if ("when" %in% names(out)) {
    out$when <- props(rule@when)
  }
  plain <- function(value) {
    if (is.list(value)) {
      return(all(vapply(value, plain, logical(1L))))
    }
    is.null(attributes(value))
  }
  if (!plain(out)) {
    rtemis.core::abort(
      "Rule metadata must use plain values without serialization attributes.",
      class = "rtemis_schema_error"
    )
  }
  out
}


# %% schema_rule_from_fields ----
#' Rebuild a typed rule from its published annotation
#' @param fields Named list: Rule fields.
#' @return `SchemaRule` object.
#' @keywords internal
#' @noRd
schema_rule_from_fields <- function(fields) {
  if (
    !is.list(fields) ||
      !is.character(fields$kind) ||
      length(fields$kind) != 1L ||
      is.na(fields$kind)
  ) {
    rtemis.core::abort(
      "Schema rule kind must be one string.",
      class = "rtemis_schema_error"
    )
  }
  cls <- switch(
    fields$kind,
    ForbidTogether = ForbidTogether,
    StatusValueRule = StatusValueRule,
    CompareFields = CompareFields,
    SumBound = SumBound,
    LengthMatches = LengthMatches,
    SubsetOf = SubsetOf,
    NonIncreasing = NonIncreasing,
    PresenceRule = PresenceRule,
    NonEmptyStrings = NonEmptyStrings,
    rtemis.core::abort(
      "Unsupported schema rule kind.",
      class = "rtemis_schema_error"
    )
  )
  fields$kind <- NULL
  if ("conditions" %in% names(fields)) {
    fields$conditions <- lapply(fields$conditions, function(p) {
      do.call(SchemaPredicate, p)
    })
  }
  if ("when" %in% names(fields)) {
    fields$when <- do.call(SchemaPredicate, fields$when)
  }
  for (nm in intersect(c("properties"), names(fields))) {
    fields[[nm]] <- unlist(fields[[nm]], use.names = FALSE)
  }
  do.call(cls, fields)
}


# %% schema_rules ----
#' Read a class's own and inherited rule declarations
#' @param cls S7 class: Class to inspect.
#' @return List of portable rule field lists.
#' @keywords internal
#' @noRd
schema_rules <- function(cls) {
  ancestors <- c(rev(schema_class_ancestors(cls)), list(cls))
  unlist(
    lapply(ancestors, function(x) attr(x, "rtemis_rules", exact = TRUE)),
    recursive = FALSE
  )
}


# %% validate_rule_declaration ----
#' Check that a rule applies to the declared property shapes
#' @param rule Named list: Portable rule fields.
#' @param cls S7 class: Declaring class.
#' @return NULL; invalid references or unsupported shapes abort.
#' @keywords internal
#' @noRd
validate_rule_declaration <- function(rule, cls) {
  fail <- function(message) {
    rtemis.core::abort(
      cls@name,
      " rule ",
      rule$id,
      ": ",
      message,
      class = "rtemis_schema_error"
    )
  }
  predicates <- c(rule$conditions, if (!is.null(rule$when)) list(rule$when))
  for (predicate in predicates) {
    if (grepl(".", predicate$property, fixed = TRUE)) {
      fail("condition property names must contain no periods.")
    }
    spec <- get_spec_fields(cls@properties[[predicate$property]])
    if (
      is.null(spec) || spec$container != "none" || !is.null(spec$target_class)
    ) {
      fail("conditions require declared scalar properties, optionally tunable.")
    }
    if (
      any(
        !vapply(
          predicate[c("minimum", "maximum", "exclusive_maximum")],
          is.null,
          logical(1L)
        )
      ) &&
        !spec$type %in% c("integer", "number")
    ) {
      fail("a bound requires a numeric property.")
    }
    value <- predicate$equals
    if (!is.null(value)) {
      type <- if (is.logical(value)) {
        "boolean"
      } else if (is.numeric(value)) {
        "number"
      } else {
        "string"
      }
      if (
        !identical(type, if (spec$type == "integer") "number" else spec$type)
      ) {
        fail("equality value has the wrong property type.")
      }
    }
  }
  if (rule$kind == "ForbidTogether") {
    return(NULL)
  } else if (rule$kind == "StatusValueRule") {
    specs <- lapply(c(rule$values, rule$statuses), function(nm) {
      get_spec(cls@properties[[nm]])
    })
    if (
      identical(rule$values, rule$statuses) ||
        any(vapply(specs, is.null, logical(1L)))
    ) {
      fail("name two distinct declared table properties.")
    }
    if (
      !all(vapply(
        specs,
        function(s) {
          s@container == "table" &&
            s@min_items == 1L &&
            identical(s@max_items, 1L)
        },
        logical(1L)
      ))
    ) {
      fail("status/value pairing requires tables with exactly one row.")
    }
    if (!identical(names(specs[[1L]]@members), names(specs[[2L]]@members))) {
      fail("value and status columns must match.")
    }
    for (column in specs[[2L]]@members) {
      if (column@type != "string" || !rule$computed_status %in% column@enum) {
        fail("every status column must enumerate the computed status.")
      }
    }
  } else {
    validate_relation_declaration(rule, cls, fail)
  }
  NULL
}


# %% matches_rule_predicate ----
#' Evaluate a scalar condition without treating null as a value
#' @param value Property value.
#' @param predicate Named list: Portable predicate fields.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
matches_rule_predicate <- function(value, predicate) {
  value <- candidate_values(value)
  if (is.null(value) || !length(value) || !is.atomic(value) || anyNA(value)) {
    return(FALSE)
  }
  matched <- if (!is.null(predicate$minimum)) {
    value >= predicate$minimum
  } else if (!is.null(predicate$maximum)) {
    value <= predicate$maximum
  } else if (!is.null(predicate$exclusive_maximum)) {
    value < predicate$exclusive_maximum
  } else {
    value == predicate$equals
  }
  if (identical(predicate$quantifier, "all")) all(matched) else any(matched)
}


# %% validate_class_rules ----
#' Enforce rules declared by one class
#' @param self S7 object: Object under validation.
#' @param rules List: Portable rule fields from the declaring class.
#' @return Character validation messages or NULL.
#' @keywords internal
#' @noRd
validate_class_rules <- function(self, rules) {
  failures <- lapply(rules, function(rule) {
    if (rule$kind == "ForbidTogether") {
      if (
        all(vapply(
          rule$conditions,
          function(p) matches_rule_predicate(prop(self, p$property), p),
          logical(1L)
        ))
      ) {
        return(paste0("[", rule$id, "] ", rule$message))
      }
    } else if (rule$kind == "StatusValueRule") {
      values <- prop(self, rule$values)
      statuses <- prop(self, rule$statuses)
      if (
        is.null(values) ||
          is.null(statuses) ||
          !is.data.frame(values) ||
          !is.data.frame(statuses) ||
          nrow(values) != 1L ||
          nrow(statuses) != 1L
      ) {
        return(NULL)
      }
      for (nm in intersect(names(values), names(statuses))) {
        computed <- identical(statuses[[nm]], rule$computed_status)
        present <- !is.na(values[[nm]])
        if (length(present) == 1L && computed != isTRUE(present)) {
          return(paste0(
            "[",
            rule$id,
            "] @",
            rule$values,
            "$",
            nm,
            " ",
            rule$message
          ))
        }
      }
    } else if (relation_rule_fails(self, rule)) {
      return(paste0("[", rule$id, "] ", rule$message))
    }
    NULL
  })
  unlist(failures, use.names = FALSE)
}


# %% class_rule_clauses ----
#' Generate enforceable clauses from a class's rule declarations
#' @param cls S7 class: Class whose contract is emitted.
#' @return List of JSON Schema clauses.
#' @keywords internal
#' @noRd
class_rule_clauses <- function(cls) {
  unlist(
    lapply(schema_rules(cls), function(rule) {
      if (rule$kind == "ForbidTogether") {
        predicates <- lapply(rule$conditions, function(p) {
          predicate_schema(p, get_spec(cls@properties[[p$property]]))
        })
        names(predicates) <- vapply(
          rule$conditions,
          `[[`,
          character(1L),
          "property"
        )
        return(list(list(
          `$comment` = rule$id,
          not = list(required = I(names(predicates)), properties = predicates)
        )))
      }
      if (rule$kind != "StatusValueRule") {
        return(relation_rule_clauses(rule, cls))
      }
      columns <- names(get_spec(cls@properties[[rule$values]])@members)
      unlist(
        lapply(columns, function(nm) {
          lapply(c(TRUE, FALSE), function(computed) {
            status <- list(const = rule$computed_status)
            if (!computed) {
              status <- list(not = status)
            }
            antecedent <- stats::setNames(
              list(
                list(type = "array"),
                list(
                  type = "array",
                  items = list(
                    properties = stats::setNames(list(status), nm),
                    required = I(nm)
                  )
                )
              ),
              c(rule$values, rule$statuses)
            )
            value <- if (computed) {
              list(not = list(type = "null"))
            } else {
              list(type = "null")
            }
            list(
              `$comment` = rule$id,
              `if` = list(
                required = I(c(rule$values, rule$statuses)),
                properties = antecedent
              ),
              then = list(
                properties = stats::setNames(
                  list(list(
                    items = list(properties = stats::setNames(list(value), nm))
                  )),
                  rule$values
                )
              )
            )
          })
        }),
        recursive = FALSE
      )
    }),
    recursive = FALSE
  )
}
