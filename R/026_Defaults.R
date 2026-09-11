# 026_Defaults.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% prop_default ----
#' Attach an input policy to a factory property
#' @param property S7 property: Factory-built property.
#' @param policy DefaultPolicy: Input resolution policy.
#' @return S7 property retaining its declaration default and validation.
#' @keywords internal
#' @noRd
prop_default <- function(property, policy) {
  check_is_S7(policy, DefaultPolicy)
  fields <- get_spec_fields(property)
  if (is.null(fields)) {
    rtemis.core::abort("Default policies require a factory property.", class = "rtemis_schema_error")
  }
  fields[["default_policy"]] <- props(policy)
  spec_object(fields)
  property[["spec"]] <- fields
  property
}


# %% class_default_policies ----
#' Read inherited input policies without constructing an instance
#' @param cls S7 class: Class to inspect.
#' @return Named list of DefaultPolicy objects.
#' @keywords internal
#' @noRd
class_default_policies <- function(cls) {
  out <- list()
  for (nm in names(cls@properties)) {
    property <- cls@properties[[nm]]
    spec <- get_spec(property)
    if (is.null(spec) || !prop_role(property) %in% "config") next
    policy <- spec@default_policy
    if (is.null(policy)) {
      policy <- if (spec@default_on_null || spec@tune_on_null) {
        DefaultPolicy(kind = "runtime", on_null = TRUE,
          requires = if (spec@tune_on_null) "tuning" else "task",
          reason = if (spec@tune_on_null) "Resolved by tuning." else "Resolved from the task.")
      } else if (spec@default_present) {
        DefaultPolicy(kind = "declaration")
      } else {
        DefaultPolicy(kind = "none")
      }
    }
    out[[nm]] <- policy
  }
  for (ancestor in c(rev(schema_class_ancestors(cls)), list(cls))) {
    overrides <- attr(ancestor, "rtemis_defaults", exact = TRUE)
    for (nm in names(overrides)) out[[nm]] <- do.call(DefaultPolicy, overrides[[nm]])
  }
  for (nm in names(out)) {
    policy <- out[[nm]]
    if (policy@kind == "expression") {
      deps <- default_expression_dependencies(policy@expression)
      if (any(!deps %in% names(cls@properties))) {
        rtemis.core::abort(cls@name, "@", nm, " default references an unknown property.", class = "rtemis_schema_error")
      }
    }
  }
  out
}


# %% default_wire_value ----
#' Serialize a declared value with its exact property shape
#' @param value ANY: Declared or resolved value.
#' @param fields Optional List: Property specification fields.
#' @return JSON-ready value.
#' @keywords internal
#' @noRd
default_wire_value <- function(value, fields = NULL) {
  if (is.null(value)) return(NULL)
  if (is.language(value) || is.function(value) || is.environment(value)) {
    rtemis.core::abort("Defaults must be declared values or typed policies, not executable R expressions.", class = "rtemis_schema_error")
  }
  if (S7_inherits(value)) {
    cls <- S7_class(value)
    base <- family_base(cls)
    nms <- if (is.null(base)) {
      names(Filter(prop_serialized, cls@properties))
    } else {
      unique(c(schema_publication(base)@discriminator, family_shared_names(base),
        names(Filter(prop_serialized, cls@properties[own_prop_names(cls, base)]))))
    }
    out <- lapply(nms, function(nm) default_wire_value(prop(value, nm), get_spec_fields(cls@properties[[nm]])))
    names(out) <- nms
    return(out)
  }
  if (is.atomic(value) && anyNA(value)) {
    rtemis.core::abort("Default literals cannot contain missing values.", class = "rtemis_schema_error")
  }
  if (is.numeric(value) && any(!is.finite(value))) {
    rtemis.core::abort("Default literals must contain finite numbers.", class = "rtemis_schema_error")
  }
  if (!is.null(fields)) value <- wire_value(value, list(spec = fields))
  if (is.list(value)) value <- lapply(value, default_wire_value)
  if (is.list(value) && length(value) == 0L &&
      (!is.null(fields) && (fields[["container"]] %in% c("map", "struct") ||
        (fields[["container"]] == "none" && fields[["type"]] == "object")))) {
    names(value) <- character()
  }
  value
}


# %% default_pointer ----
#' Escape a JSON Pointer segment
#' @param name Character: Segment.
#' @return Character.
#' @keywords internal
#' @noRd
default_pointer <- function(name) gsub("/", "~1", gsub("~", "~0", name, fixed = TRUE), fixed = TRUE)


# %% schema_child_pointer ----
#' Find a schema node's path within its parent
#' @param schema,child List: Parent and child schema nodes.
#' @param path Character: Current JSON Pointer.
#' @return Character path or NULL.
#' @keywords internal
#' @noRd
schema_child_pointer <- function(schema, child, path = "") {
  if (identical(schema, child)) return(path)
  if (!is.list(schema)) return(NULL)
  for (i in seq_along(schema)) {
    key <- if (is.null(names(schema))) as.character(i - 1L) else default_pointer(names(schema)[[i]])
    found <- schema_child_pointer(schema[[i]], child, paste0(path, "/", key))
    if (!is.null(found)) return(found)
  }
  NULL
}


# %% default_declarations ----
#' Export every default in a property specification tree
#' @param spec PropertySpec: Source declaration.
#' @param schema List: Generated property schema.
#' @param path Character: Schema JSON Pointer.
#' @return Named list of default descriptors keyed by schema pointer.
#' @keywords internal
#' @noRd
default_declarations <- function(spec, schema, path) {
  node <- if (spec@default_present) {
    list(kind = "literal", value = default_wire_value(spec@default, spec_fields(spec)))
  } else list(kind = "none")
  node[["policy"]] <- if (!is.null(spec@default_policy)) props(spec@default_policy) else NULL
  out <- stats::setNames(list(node), path)
  if (!is.null(spec@items)) {
    child <- schema_element(schema, spec@container, spec@tunable, spec@broadcast)
    child_path <- schema_child_pointer(schema, child)
    out <- c(out, default_declarations(spec@items, child, paste0(path, child_path)))
  }
  if (!is.null(spec@members)) {
    object <- if (spec@container == "table") schema[["items"]] else schema
    prefix <- if (spec@container == "table") "/items" else ""
    for (nm in names(spec@members)) {
      out <- c(out, default_declarations(spec@members[[nm]], object[["properties"]][[nm]],
        paste0(path, prefix, "/properties/", default_pointer(nm))))
    }
  }
  out
}


# %% default_expression_value ----
#' Evaluate the bounded default-expression grammar
#' @param expression List or atomic value: Validated expression.
#' @param values Named list: Resolved sibling values.
#' @return Scalar result. Missing dependencies and candidate domains raise errors.
#' @keywords internal
#' @noRd
default_expression_value <- function(expression, values) {
  if (!is.list(expression)) return(expression)
  op <- names(expression)[[1L]]
  args <- expression[[1L]]
  if (op == "var") {
    if (!args %in% names(values)) rtemis.core::abort("Unresolved default input: ", args, ".", class = "rtemis_defaults_pending")
    value <- values[[args]]
    if (is_candidates(value) || is_wire_candidates(value)) {
      rtemis.core::abort("Default input requires a selected candidate: ", args, ".", class = "rtemis_defaults_pending")
    }
    return(value)
  }
  if (op == "if") {
    condition <- default_expression_value(args[[1L]], values)
    if (!is.logical(condition) || length(condition) != 1L || is.na(condition)) {
      rtemis.core::abort("Default conditions must produce one Boolean.", class = "rtemis_schema_error")
    }
    return(default_expression_value(args[[if (condition) 2L else 3L]], values))
  }
  a <- default_expression_value(args[[1L]], values)
  b <- default_expression_value(args[[2L]], values)
  if (op %in% c("===", "!==")) {
    same <- if (is.numeric(a) && is.numeric(b)) identical(as.numeric(a), as.numeric(b)) else identical(a, b)
    return(if (op == "===") same else !same)
  }
  if (!is.numeric(a) || length(a) != 1L || !is.numeric(b) || length(b) != 1L) {
    rtemis.core::abort("Default arithmetic requires numeric scalars.", class = "rtemis_schema_error")
  }
  switch(op, ">" = a > b, "<" = a < b, ">=" = a >= b, "<=" = a <= b,
    "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b)
}


# %% resolve_class_defaults ----
#' Resolve class-owned input policies
#' @param cls S7 class: Configuration class.
#' @param values Named list: Authored values, preserving explicit NULL keys.
#' @param context Optional List: Explicit runtime values keyed by requirement.
#' @return List with values, origins, and pending requirements.
#' @keywords internal
#' @noRd
resolve_class_defaults <- function(cls, values, context = NULL) {
  policies <- class_default_policies(cls)
  origins <- stats::setNames(as.list(rep("user", length(values))), names(values))
  pending <- list()
  active <- character()
  done <- character()
  resolve <- function(nm) {
    if (nm %in% done) return(invisible(NULL))
    if (nm %in% active) rtemis.core::abort("Cyclic default dependency at ", cls@name, "@", nm, ".", class = "rtemis_schema_error")
    policy <- policies[[nm]]
    if (is.null(policy)) return(invisible(NULL))
    if (nm %in% names(values) && !(policy@on_null && is.null(values[[nm]]))) return(invisible(NULL))
    active <<- c(active, nm)
    on.exit({ active <<- setdiff(active, nm); done <<- c(done, nm) })
    fields <- get_spec_fields(cls@properties[[nm]])
    if (policy@kind == "none") return(invisible(NULL))
    value <- switch(policy@kind,
      declaration = fields[["default"]],
      literal = policy@value,
      expression = {
        deps <- default_expression_dependencies(policy@expression)
        for (dep in deps) resolve(dep)
        if (any(deps %in% names(pending))) {
          pending[[nm]] <<- list(requires = deps, reason = "Default inputs are unresolved.")
          return(invisible(NULL))
        }
        tryCatch(default_expression_value(policy@expression, values),
          rtemis_defaults_pending = function(e) {
            pending[[nm]] <<- list(requires = deps, reason = conditionMessage(e))
            NULL
          })
      },
      runtime = {
        if (length(policy@requires) != 1L || !policy@requires %in% names(context)) {
          pending[[nm]] <<- list(requires = policy@requires, reason = policy@reason)
          return(invisible(NULL))
        }
        context[[policy@requires]]
      }
    )
    if (nm %in% names(pending)) return(invisible(NULL))
    if (!is.null(fields) && is.null(fields[["target_class"]])) {
      if (fields[["type"]] == "integer" && is.numeric(value) && !is_candidates(value)) value <- clean_int(value)
      problem <- validate_with_spec(value, fields)
      if (!is.null(problem)) rtemis.core::abort(cls@name, "@", nm, " default ", problem, class = "rtemis_schema_error")
    }
    values[nm] <<- list(value)
    origins[[nm]] <<- if (policy@kind %in% c("expression", "runtime")) "derived" else "default"
    invisible(NULL)
  }
  for (nm in names(policies)) resolve(nm)
  list(values = values, origins = origins, pending = pending)
}


# %% default_from_wire ----
#' Restore a default using its declared wire shape
#' @param value ANY: JSON-decoded value.
#' @param schema List: Property schema.
#' @return Typed R value.
#' @keywords internal
#' @noRd
default_from_wire <- function(value, schema) {
  if (is.null(value)) return(NULL)
  ann <- schema[["x-rtemis"]]
  container <- ann[["container"]] %||% "none"
  target <- ann[["target_class"]]
  if (!is.null(target)) {
    restore <- function(value) normalize_default_object(from_wire_object(value, target))
    if (container == "none") return(restore(value))
    return(lapply(value, restore))
  }
  if (is_wire_candidates(value)) {
    return(HyperparameterCandidates(candidates = lapply(value[["candidates"]], function(v) {
      default_from_wire(v, modifyList(schema, list(`x-rtemis` = modifyList(ann, list(tunable = FALSE)))))
    })))
  }
  if (container == "factor") return(from_wire_factor(value))
  if (container == "table") {
    columns <- schema[["items"]][["properties"]]
    out <- lapply(names(columns), function(nm) {
      cells <- lapply(value, `[[`, nm)
      coerce_to_type(unlist(cells, use.names = FALSE), columns[[nm]][["x-rtemis"]][["type"]])
    })
    names(out) <- names(columns)
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }
  if (container == "struct") {
    return(stats::setNames(lapply(names(value), function(nm) {
      default_from_wire(value[[nm]], schema[["properties"]][[nm]])
    }), names(value)))
  }
  child <- schema_element(schema, container, isTRUE(ann[["tunable"]]), isTRUE(ann[["broadcast"]]))
  if (container %in% c("map", "array") && !is.null(child[["x-rtemis"]])) {
    return(lapply(value, default_from_wire, schema = child))
  }
  if (ann[["type"]] == "object") return(if (length(value) == 0L) list() else value)
  if (container != "none" && is.list(value)) value <- unlist(value, use.names = container == "map")
  valid_type <- switch(ann[["type"]], boolean = is.logical(value),
    integer = is.numeric(value), number = is.numeric(value), string = is.character(value))
  if (length(value) && !isTRUE(valid_type)) rtemis.core::abort("Default literal has the wrong JSON type.", class = "rtemis_schema_error")
  if (ann[["type"]] == "integer" && any(!is.finite(value) | value != trunc(value))) {
    rtemis.core::abort("Integer defaults must be finite whole numbers.", class = "rtemis_schema_error")
  }
  nms <- names(value)
  value <- if (length(value) == 0L) {
    switch(ann[["type"]], boolean = logical(), integer = integer(), number = numeric(), string = character())
  } else coerce_to_type(value, ann[["type"]])
  if (!is.null(nms)) names(value) <- nms
  value
}


# %% normalize_default_object ----
#' Restore numeric storage types in a decoded object default
#' @param value S7 object: Decoded default.
#' @return S7 object with its declared numeric property types.
#' @keywords internal
#' @noRd
normalize_default_object <- function(value) {
  cls <- S7_class(value)
  for (nm in names(cls@properties)) {
    fields <- get_spec_fields(cls@properties[[nm]])
    if (is.null(fields) || !prop_serialized(cls@properties[[nm]]) || fields[["constant"]]) next
    item <- prop(value, nm)
    if (S7_inherits(item)) {
      item <- normalize_default_object(item)
    } else if (!is.null(fields[["target_class"]]) && is.list(item)) {
      item <- lapply(item, normalize_default_object)
    } else if (identical(fields[["type"]], "number") && is.integer(item)) {
      storage.mode(item) <- "double"
    }
    if (!identical(item, prop(value, nm))) prop(value, nm) <- item
  }
  value
}


# %% default_catalog_entries ----
#' Derive defaults publication locations from the class catalog
#' @param catalog List: Derived schema catalog.
#' @param base_url Character: Publication base URL.
#' @return Named list keyed by schema ID, with class and relative file path.
#' @keywords internal
#' @noRd
default_catalog_entries <- function(catalog, base_url = "https://schema.rtemis.org") {
  out <- list()
  add <- function(cls, path) {
    out[[paste0(base_url, "/", path)]] <<- list(cls = cls, path = path)
  }
  for (nm in names(catalog[["families"]])) {
    family <- catalog[["families"]][[nm]]
    add(family[["base_class"]], paste0(nm, "/v1/schema.json"))
    for (leaf in family[["algorithms"]]) {
      cls <- leaf[["cls"]]
      slug <- tolower(discriminator_value(cls, family[["discriminator"]]))
      add(cls, paste0(nm, "/", slug, "/v1/schema.json"))
    }
  }
  for (nm in names(catalog[["flat_configs"]])) {
    add(catalog[["flat_configs"]][[nm]][["cls"]], paste0(nm, "/v1/schema.json"))
  }
  out[sort(names(out))]
}


# %% apply_setup_defaults ----
#' Apply declared defaults to omitted setup arguments
#' @param cls S7 class: Class returned by the setup function.
#' @param envir Environment: Setup call frame, before argument normalization.
#' @return NULL, invisibly. Resolved arguments are assigned in the setup frame.
#' @keywords internal
#' @noRd
apply_setup_defaults <- function(cls, envir = parent.frame()) {
  fn <- sys.function(sys.parent())
  nms <- intersect(names(formals(fn)), names(cls@properties))
  supplied <- vapply(nms, function(nm) !eval(call("missing", as.name(nm)), envir), logical(1L))
  values <- lapply(nms[supplied], get, envir = envir, inherits = FALSE)
  names(values) <- nms[supplied]
  resolved <- resolve_class_defaults(cls, values)
  policies <- class_default_policies(cls)
  for (nm in intersect(names(resolved[["pending"]]), nms)) {
    if (policies[[nm]]@kind == "expression") {
      rtemis.core::abort(cls@name, "@", nm, ": ", resolved[["pending"]][[nm]][["reason"]],
        class = "rtemis_defaults_pending")
    }
  }
  for (nm in intersect(names(resolved[["values"]]), nms)) {
    if (identical(resolved[["origins"]][[nm]], "user")) next
    value <- resolved[["values"]][[nm]]
    fields <- get_spec_fields(cls@properties[[nm]])
    if (!is.null(fields[["target_class"]]) && !is.null(value) && !S7_inherits(value) &&
        fields[["container"]] == "none") {
      value <- from_wire_object(value, fields[["target_class"]])
    }
    assign(nm, value, envir = envir)
  }
  invisible(NULL)
}
