# 009_DefaultPolicy.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% DefaultPolicy ----
#' Typed input-default policy
#'
#' @field kind Character: One of declaration, none, literal, expression, runtime.
#' @field value Optional ANY: Literal default value.
#' @field expression Optional List: Bounded JSONLogic expression.
#' @field requires Optional Character vector: Runtime context keys.
#' @field reason Optional Character: Explanation of deferred resolution.
#' @field on_null Logical: Whether explicit NULL also triggers resolution.
#' @keywords internal
#' @noRd
DefaultPolicy <- new_class(
  "DefaultPolicy",
  package = "rtemis",
  properties = list(
    kind = class_character,
    value = class_any,
    expression = NULL | class_list,
    requires = NULL | class_character,
    reason = NULL | class_character,
    on_null = new_property(class_logical, default = FALSE)
  ),
  validator = function(self) {
    if (length(self@kind) != 1L || is.na(self@kind) ||
        !self@kind %in% c("declaration", "none", "literal", "expression", "runtime")) {
      return("@kind must identify a supported default policy.")
    }
    if (length(self@on_null) != 1L || is.na(self@on_null)) {
      return("@on_null must be one non-missing logical value.")
    }
    if (self@kind != "literal" && !is.null(self@value)) {
      return("Only literal policies accept @value.")
    }
    if (self@kind == "expression") {
      if (is.null(self@expression)) return("Expression policies require @expression.")
      problem <- tryCatch({ default_expression_dependencies(self@expression); NULL },
        error = function(e) conditionMessage(e))
      if (!is.null(problem)) return(problem)
    } else if (!is.null(self@expression)) {
      return("Only expression policies accept @expression.")
    }
    if (self@kind == "runtime") {
      if (is.null(self@requires) || length(self@requires) == 0L ||
          anyNA(self@requires) || any(!nzchar(self@requires)) || anyDuplicated(self@requires)) {
        return("Runtime policies require unique, non-empty context keys.")
      }
      if (length(self@reason) != 1L || is.na(self@reason) || !nzchar(self@reason)) {
        return("Runtime policies require one non-empty @reason.")
      }
    } else if (!is.null(self@requires) || !is.null(self@reason)) {
      return("Only runtime policies accept @requires and @reason.")
    }
    NULL
  }
)


# %% repr.DefaultPolicy ----
method(repr, DefaultPolicy) <- function(x, pad = 0L, output_type = NULL) {
  paste0(repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(props(x), pad = pad, output_type = output_type))
}


# %% default_expression_dependencies ----
#' Validate the bounded default-expression grammar and collect inputs
#' @param expression List or atomic value: JSONLogic node.
#' @return Character vector of document property dependencies.
#' @keywords internal
#' @noRd
default_expression_dependencies <- function(expression) {
  if (!is.list(expression)) {
    if (is.null(expression) || (is.atomic(expression) && length(expression) == 1L &&
        !anyNA(expression) && is.null(attributes(expression)) &&
        (!is.numeric(expression) || is.finite(expression)))) return(character())
    rtemis.core::abort("Default expressions require finite JSON scalar literals.", class = "rtemis_schema_error")
  }
  if (length(expression) != 1L || is.null(names(expression))) {
    rtemis.core::abort("A default expression must have exactly one operation.", class = "rtemis_schema_error")
  }
  op <- names(expression)[[1L]]
  args <- expression[[1L]]
  if (op == "var") {
    if (!is.character(args) || length(args) != 1L || is.na(args) ||
        !grepl("^[A-Za-z_][A-Za-z0-9_]*$", args)) {
      rtemis.core::abort("Default inputs must name one declared sibling property.", class = "rtemis_schema_error")
    }
    return(args)
  }
  arity <- c("if" = 3L, "===" = 2L, "!==" = 2L, ">" = 2L, "<" = 2L,
    ">=" = 2L, "<=" = 2L, "+" = 2L, "-" = 2L, "*" = 2L, "/" = 2L)
  if (!op %in% names(arity) || !is.list(args) || !is.null(names(args)) || length(args) != arity[[op]]) {
    rtemis.core::abort("Unsupported default operation or arity: ", op, ".", class = "rtemis_schema_error")
  }
  unique(unlist(lapply(args, default_expression_dependencies), use.names = FALSE))
}
