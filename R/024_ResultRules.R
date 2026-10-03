# 024_ResultRules.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% RowCountMatches ----
#' Match the observation counts of two result values when both are available
#' @field left,right Character: Array, factor, or matrix property names.
#' @keywords internal
#' @noRd
#' @include 024_SchemaRules.R
RowCountMatches <- new_class(
  "RowCountMatches",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    left = prop_string("", description = "First observation property."),
    right = prop_string("", description = "Second observation property.")
  )
)


# %% FactorLevelsMatch ----
#' Match ordered dictionaries when both values are categorical
#' @field left,right Character: Factor or primitive-array/factor union properties.
#' @keywords internal
#' @noRd
FactorLevelsMatch <- new_class(
  "FactorLevelsMatch",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    left = prop_string("", description = "First categorical property."),
    right = prop_string("", description = "Reference categorical property.")
  )
)


# %% ProbabilityColumnsMatch ----
#' Match probability columns to a categorical dictionary
#' @field probabilities Character: Matrix property name.
#' @field outcome Character: Property supplying the ordered class dictionary.
#' @keywords internal
#' @noRd
ProbabilityColumnsMatch <- new_class(
  "ProbabilityColumnsMatch",
  package = "rtemis",
  parent = SchemaRule,
  properties = list(
    probabilities = prop_string(
      "",
      description = "Probability matrix property."
    ),
    outcome = prop_string("", description = "Property supplying class levels.")
  )
)


# %% validate_result_relation ----
#' Check result relation operands against their complete property declarations
#' @param rule Named list: Portable rule fields.
#' @param cls S7 class: Declaring class.
#' @param fail Function: Raise a declaration error.
#' @return NULL.
#' @keywords internal
#' @noRd
validate_result_relation <- function(rule, cls, fail) {
  require_shape <- function(nm, containers) {
    if (!nzchar(nm) || grepl(".", nm, fixed = TRUE)) {
      fail(
        "relational property names must be non-empty and contain no periods."
      )
    }
    valid <- function(spec) {
      if (is.null(spec) || spec@tunable || spec@broadcast) {
        return(FALSE)
      }
      if (spec@type == "union") {
        return(all(vapply(spec@alternatives, valid, logical(1L))))
      }
      if (!spec@container %in% containers || !is.null(spec@target_class)) {
        return(FALSE)
      }
      if (spec@container %in% c("array", "matrix") && !is.null(spec@items)) {
        return(
          spec@items@container == "none" &&
            spec@items@type %in% c("number", "integer", "string", "boolean") &&
            is.null(spec@items@target_class)
        )
      }
      TRUE
    }
    spec <- get_spec(cls@properties[[nm]])
    if (!valid(spec)) {
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
  require_factor <- function(nm) {
    spec <- require_shape(nm, c("array", "factor"))
    alternatives <- if (spec@type == "union") spec@alternatives else list(spec)
    if (
      !any(vapply(
        alternatives,
        function(x) x@container == "factor",
        logical(1L)
      ))
    ) {
      fail("a dictionary relation requires a categorical alternative.")
    }
  }
  if (rule[["kind"]] == "ProbabilityColumnsMatch") {
    require_shape(rule[["probabilities"]], "matrix")
    require_factor(rule[["outcome"]])
    operands <- c(rule[["probabilities"]], rule[["outcome"]])
  } else {
    containers <- if (rule[["kind"]] == "RowCountMatches") {
      c("array", "factor", "matrix")
    } else {
      c("array", "factor")
    }
    operands <- c(rule[["left"]], rule[["right"]])
    for (nm in operands) {
      require_shape(nm, containers)
      if (rule[["kind"]] == "FactorLevelsMatch") require_factor(nm)
    }
  }
  if (anyDuplicated(operands)) {
    fail("comparison must name distinct properties.")
  }
  NULL
}


# %% result_relation_fails ----
#' Check result relations without materializing external payloads
#' @param self S7 object: Object under validation.
#' @param rule Named list: Portable rule fields.
#' @return Logical scalar indicating a violation.
#' @keywords internal
#' @noRd
result_relation_fails <- function(self, rule) {
  value <- function(nm) prop(self, nm)
  external <- function(x) S7_inherits(x, DataRef)
  rows <- function(x) if (external(x)) x@n_rows else NROW(x)
  dictionary <- function(x) if (external(x)) x@levels else levels(x)
  if (rule[["kind"]] == "ProbabilityColumnsMatch") {
    x <- value(rule[["probabilities"]])
    lev <- dictionary(value(rule[["outcome"]]))
    if (is.null(x) || is.null(lev)) {
      return(FALSE)
    }
    width <- if (length(lev) == 2L) 1L else length(lev)
    # An empty wire matrix carries no column dimension.
    if (!external(x) && NROW(x) == 0L) {
      return(FALSE)
    }
    return((if (external(x)) length(x@columns) else NCOL(x)) != width)
  }
  left <- value(rule[["left"]])
  right <- value(rule[["right"]])
  if (is.null(left) || is.null(right)) {
    return(FALSE)
  }
  if (rule[["kind"]] == "RowCountMatches") {
    return(rows(left) != rows(right))
  }
  a <- dictionary(left)
  b <- dictionary(right)
  !is.null(a) && !is.null(b) && !identical(a, b)
}


# %% result_relation_logic ----
#' Compile result dimensions and dictionaries into portable JSONLogic
#' @param rule Named list: Portable rule fields.
#' @return Boolean JSONLogic expression indicating a violation.
#' @keywords internal
#' @noRd
result_relation_logic <- function(rule) {
  op <- rule_logic_node
  v <- rule_logic_var
  present <- function(path) op("!==", v(path), NULL)
  count <- function(x) op("reduce", x, op("+", v("accumulator"), 1L), 0L)
  rows <- function(nm) {
    op(
      "if",
      present(paste0(nm, ".n_rows")),
      v(paste0(nm, ".n_rows")),
      present(paste0(nm, ".codes")),
      count(v(paste0(nm, ".codes"))),
      count(v(nm))
    )
  }
  if (rule[["kind"]] == "RowCountMatches") {
    return(op(
      "and",
      present(rule[["left"]]),
      present(rule[["right"]]),
      op("!==", rows(rule[["left"]]), rows(rule[["right"]]))
    ))
  }
  if (rule[["kind"]] == "FactorLevelsMatch") {
    a <- paste0(rule[["left"]], ".levels")
    b <- paste0(rule[["right"]], ".levels")
    return(op("and", present(a), present(b), op("!==", v(a), v(b))))
  }
  nm <- rule[["probabilities"]]
  lev <- paste0(rule[["outcome"]], ".levels")
  width <- op("if", op("===", count(v(lev)), 2L), 1L, count(v(lev)))
  reduced <- op(
    "reduce",
    v(nm),
    list(
      v("accumulator.0"),
      op(
        "or",
        v("accumulator.1"),
        op("!==", count(v("current")), v("accumulator.0"))
      )
    ),
    list(width, FALSE)
  )
  # Keep the expected width in the accumulator across row-local scopes.
  failed <- op("reduce", reduced, v("current"), FALSE)
  op(
    "and",
    present(nm),
    present(lev),
    op(
      "if",
      present(paste0(nm, ".columns")),
      op("!==", count(v(paste0(nm, ".columns"))), width),
      failed
    )
  )
}
