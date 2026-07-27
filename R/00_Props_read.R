# 00_Props_read.R
# ::rtemis::
# 2026- EDG rtemis.org

# The read direction: JSON Schema -> PropertySpec -> S7 class, inverting
# `spec_to_schema()` / `S7_to_JSONSchema()` in `00_Props.R`.
#
# What makes this possible is `x-rtemis`, which carries the axes standard JSON
# Schema cannot express. Two of them are load-bearing here:
#
#   - `tunable` and `broadcast` emit *identical* `oneOf` shapes. Nothing in the
#     standard keywords distinguishes "a search space" from "a value that may be
#     broadcast", so a reader without the annotation must guess.
#   - `type` names the leaf type. JSON has one number type, so an integer
#     property and a float property are otherwise indistinguishable once a
#     default has been through a JSON encoder.
#
# A schema alone is not sufficient to rebuild a class: `default` is deliberately
# not a schema keyword (see `plan/config-artifacts.md`), so defaults arrive
# separately, from the artifact `data-raw/generate_defaults.R` publishes. That
# split is the contract a port has to honour too, which is why `defaults` is an
# argument here rather than something inferred.

# %% schema_is_nullable ----
#' Does a property schema admit null?
#'
#' Null appears two ways: as a member of a `type` array (the plain and container
#' cases) or as a `{"type": "null"}` branch of a `oneOf` (the tunable and
#' broadcast cases).
#'
#' @param x Named list: A JSON Schema property.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_is_nullable <- function(x) {
  branches <- x[["oneOf"]]
  if (!is.null(branches)) {
    return(any(vapply(
      branches,
      function(b) identical(unname(b[["type"]]), "null"),
      logical(1L)
    )))
  }
  "null" %in% x[["type"]]
} # /rtemis::schema_is_nullable


# %% schema_element ----
#' The sub-schema `spec_to_schema()` emitted as a container's element
#'
#' Mirrors that function's branches. For `container = "none"` the element is the
#' scalar leaf itself. A matrix's rows are arrays, so its element is one step
#' further in.
#'
#' @param x Named list: A JSON Schema property.
#' @param container Character \{"none", "array", "map", "matrix"\}: How values
#' are wrapped.
#' @param tunable Logical: Whether the property is tunable.
#' @param broadcast Logical: Whether a bare scalar stands in for the container.
#'
#' @return Named list: The element sub-schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_element <- function(x, container, tunable, broadcast) {
  # `spec_to_schema()` builds its `oneOf` in a fixed order -- an optional null
  # first, then the bare/scalar form, then the container form -- so branches are
  # identified by position. Matching on `type` instead would be ambiguous
  # whenever the element is itself an array (a list of per-tree vectors emits
  # two "array" branches).
  branches <- function() {
    Filter(
      function(b) !identical(unname(b[["type"]]), "null"),
      x[["oneOf"]]
    )
  }
  if (container == "array") {
    if (!broadcast) {
      return(x[["items"]])
    }
    b <- branches()
    return(b[[length(b)]][["items"]])
  }
  if (container == "matrix") {
    return(x[["items"]][["items"]])
  }
  if (container == "map") {
    return(x[["additionalProperties"]])
  }
  if (tunable) {
    # The trailing branch is the search-values array; the leading one is the
    # scalar leaf.
    return(branches()[[1L]])
  }
  x
} # /rtemis::schema_element


# %% coerce_to_type ----
#' Coerce a JSON-decoded value to the R type a spec declares
#'
#' JSON has a single number type, so an integer default survives a round trip
#' through JSON as a double. `x-rtemis.type` is what restores the distinction.
#'
#' @param value Value as decoded from JSON, or NULL.
#' @param type Character \{"boolean", "integer", "number", "string",
#' "object"\}: Declared leaf type.
#'
#' @return `value` coerced to `type`, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
coerce_to_type <- function(value, type) {
  if (is.null(value)) {
    return(NULL)
  }
  switch(
    type,
    boolean = as.logical(value),
    integer = as.integer(value),
    number = as.numeric(value),
    string = as.character(value),
    object = as.list(value)
  )
} # /rtemis::coerce_to_type


# %% element_placeholder ----
#' A conforming stand-in for an element descriptor's unused default
#'
#' `PropertySpec` requires `@default` to conform to the spec, but the `@items`
#' spec of a container describes an *element type*, and nothing reads its
#' default: `prop_array()` and `prop_map()` both document it as unused, and
#' `validate_with_spec()` never consults it. So the emitter does not publish it
#' and there is nothing to restore. This fills the slot with the smallest value
#' the declared constraints admit, which keeps the reconstructed spec valid
#' without implying the original held that value.
#'
#' @param type Character \{"boolean", "integer", "number", "string",
#' "object"\}: Declared leaf type.
#' @param minimum Optional Numeric: Lower bound, if declared.
#' @param enum Optional Character: Permitted values, if declared.
#'
#' @return A scalar of the declared type.
#'
#' @author EDG
#' @keywords internal
#' @noRd
element_placeholder <- function(type, minimum = NULL, enum = NULL) {
  if (!is.null(enum)) {
    return(enum[[1L]])
  }
  switch(
    type,
    boolean = FALSE,
    string = "",
    integer = as.integer(minimum %||% 0L),
    number = as.numeric(minimum %||% 0),
    object = list()
  )
} # /rtemis::element_placeholder


# %% strip_suffix ----
#' Remove a trailing sentence from a description
#'
#' @param x Character: The description.
#' @param suffix Character: The sentence to remove.
#'
#' @return Character: `x` without `suffix`, right-trimmed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
strip_suffix <- function(x, suffix) {
  if (!nzchar(suffix) || !endsWith(x, suffix)) {
    return(x)
  }
  trimws(substr(x, 1L, nchar(x) - nchar(suffix)), which = "right")
} # /rtemis::strip_suffix


# %% schema_to_spec ----
#' Convert a JSON Schema property to a PropertySpec
#'
#' The inverse of `spec_to_schema()`. Bounds and enum are read from the leaf,
#' nullability from the type union or `oneOf`, and everything standard JSON
#' Schema cannot express from `x-rtemis`.
#'
#' @param x Named list: A JSON Schema property, as generated by
#' `spec_to_schema()`.
#' @param default Optional: The property's default, from the defaults artifact.
#' A `const` property takes its value from the schema and ignores this.
#' @param element Logical: If TRUE, `x` describes one element of a container
#' rather than a settable property, so its default is a placeholder (see
#' `element_placeholder()`).
#'
#' @return `PropertySpec` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_to_spec <- function(x, default = NULL, element = FALSE) {
  ann <- x[["x-rtemis"]]
  if (is.null(ann) || is.null(ann[["type"]])) {
    rtemis.core::abort(
      "Property schema carries no `x-rtemis.type`, so it was not generated from a PropertySpec and cannot be read back.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  type <- ann[["type"]]
  container <- ann[["container"]] %||% "none"
  broadcast <- isTRUE(ann[["broadcast"]])
  tunable <- isTRUE(ann[["tunable"]])
  constant <- identical(ann[["role"]], "constant")
  data_bound <- ann[["data_bound"]]

  description <- x[["description"]] %||% ""
  if (!is.null(data_bound)) {
    description <- strip_suffix(
      description,
      data_bound_note(data_bound, container, broadcast)
    )
  }

  if (constant) {
    # `const` is the whole schema for a constant: bounds and enum were never
    # emitted because `prop_const()` never sets them.
    return(PropertySpec(
      type = type,
      default = coerce_to_type(x[["const"]], type),
      nullable = FALSE,
      tunable = FALSE,
      container = "none",
      broadcast = FALSE,
      constant = TRUE,
      description = description
    ))
  }

  child <- schema_element(x, container, tunable, broadcast)
  # `@items` is only meaningful for "array" and "map" (a matrix's element type
  # is fixed, and a scalar has no element). Within those, a nested element
  # carries its own annotations while a scalar leaf does not, which is exactly
  # the condition under which `spec_to_schema()` recursed.
  items <- if (
    container %in% c("array", "map") && !is.null(child[["x-rtemis"]])
  ) {
    schema_to_spec(child, element = TRUE)
  } else {
    NULL
  }
  # When `@items` is set the outer spec's own bounds are unused (`prop_array()`
  # and `prop_map()` leave them NULL), so the leaf keywords belong to the item.
  leaf <- if (is.null(items)) child else list()
  as_bound <- function(v) {
    if (is.null(v)) {
      NULL
    } else if (type == "integer") {
      as.integer(v)
    } else {
      as.numeric(v)
    }
  }

  leaf_enum <- if (is.null(leaf[["enum"]])) {
    NULL
  } else {
    as.character(leaf[["enum"]])
  }
  PropertySpec(
    type = type,
    default = if (element) {
      element_placeholder(type, as_bound(leaf[["minimum"]]), leaf_enum)
    } else {
      coerce_to_type(default, type)
    },
    minimum = as_bound(leaf[["minimum"]]),
    maximum = as_bound(leaf[["maximum"]]),
    exclusive_minimum = as_bound(leaf[["exclusiveMinimum"]]),
    exclusive_maximum = as_bound(leaf[["exclusiveMaximum"]]),
    enum = leaf_enum,
    nullable = schema_is_nullable(x),
    tunable = tunable,
    container = container,
    items = items,
    broadcast = broadcast,
    tune_on_null = isTRUE(ann[["tune_on_null"]]),
    data_bound = data_bound,
    data_dependent = isTRUE(ann[["data_dependent"]]),
    description = description
  )
} # /rtemis::schema_to_spec


# %% JSONSchema_to_S7 ----
#' Build an S7 class from a JSON Schema
#'
#' The read direction of [S7_to_JSONSchema]: reads each property's
#' `PropertySpec` back out of the schema and assembles a live S7 class whose
#' properties carry the same types, bounds, enums, containers, and validators as
#' the class the schema was generated from.
#'
#' Defaults are supplied separately because they are not a schema keyword: the
#' published tree carries no `default`, and defaults live in their own versioned
#' artifact keyed by schema `$id`. A property that is neither nullable nor
#' `const` therefore needs an entry in `defaults`, and the error names any that
#' are missing rather than inventing a value.
#'
#' Properties holding a nested config are published as a `$ref` to that config's
#' own schema, which this function does not fetch; supply the corresponding S7
#' classes via `refs`.
#'
#' @param schema Named list: A JSON Schema, as produced by [S7_to_JSONSchema]
#' or parsed from the published tree with `jsonlite::fromJSON(simplifyVector =
#' FALSE)`.
#' @param defaults Optional named list: Property name to default value, as
#' published in the defaults artifact under this schema's `$id`.
#' @param refs Optional named list: Property name to S7 class, for properties
#' published as a `$ref` to another schema.
#' @param name Optional Character: Class name. Defaults to the schema `title`.
#' @param package Optional Character: Package name recorded on the class.
#'
#' @return S7 class.
#'
#' @author EDG
#' @export
#' @examples
#' # A schema as `S7_to_JSONSchema()` emits it: standard keywords carry the
#' # bounds, `x-rtemis` carries the leaf type.
#' schema <- list(
#'   title = "Demo",
#'   properties = list(
#'     k = list(
#'       type = "integer",
#'       minimum = 1L,
#'       maximum = 10L,
#'       description = "Number of components.",
#'       `x-rtemis` = list(type = "integer")
#'     )
#'   )
#' )
#' Demo <- JSONSchema_to_S7(schema, defaults = list(k = 2L))
#' Demo()@k
#' Demo(k = 5L)@k
#' # The reconstructed class enforces the schema's bounds.
#' tryCatch(Demo(k = 99L), error = conditionMessage)
JSONSchema_to_S7 <- function(
  schema,
  defaults = NULL,
  refs = NULL,
  name = NULL,
  package = NULL
) {
  if (!is.list(schema) || is.null(schema[["properties"]])) {
    rtemis.core::abort(
      "`schema` must be a JSON Schema with a `properties` object.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  props <- schema[["properties"]]
  # `$schema` identifies the document, not a field of the class.
  props[["$schema"]] <- NULL

  is_ref <- vapply(
    props,
    function(p) {
      !is.null(p[["$ref"]]) ||
        any(vapply(
          p[["oneOf"]] %||% list(),
          function(b) !is.null(b[["$ref"]]),
          logical(1L)
        ))
    },
    logical(1L)
  )
  unresolved <- setdiff(names(props)[is_ref], names(refs))
  if (length(unresolved) > 0L) {
    rtemis.core::abort(
      "No class supplied for `$ref` propert",
      if (length(unresolved) == 1L) "y: " else "ies: ",
      paste(unresolved, collapse = ", "),
      ". Pass them in `refs`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  spec_names <- names(props)[!is_ref]
  # Checked from the schema rather than from the built specs: `PropertySpec`
  # rejects a NULL default on a non-nullable property, so it would abort with
  # its own message before this one could name every offender.
  needs_default <- vapply(
    spec_names,
    function(nm) {
      p <- props[[nm]]
      constant <- identical(p[["x-rtemis"]][["role"]], "constant")
      !constant && !schema_is_nullable(p) && is.null(defaults[[nm]])
    },
    logical(1L)
  )
  if (any(needs_default)) {
    missing_default <- spec_names[needs_default]
    rtemis.core::abort(
      "No default supplied for non-nullable propert",
      if (length(missing_default) == 1L) "y: " else "ies: ",
      paste(missing_default, collapse = ", "),
      ". Pass them in `defaults`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  specs <- lapply(spec_names, function(nm) {
    schema_to_spec(props[[nm]], default = defaults[[nm]])
  })
  names(specs) <- spec_names

  properties <- lapply(names(props), function(nm) {
    if (is_ref[[nm]]) {
      cls <- refs[[nm]]
      return(if (schema_is_nullable(props[[nm]])) NULL | cls else cls)
    }
    prop <- make_prop(specs[[nm]])
    # `readOnly` is how run state appears in the published contract.
    if (isTRUE(props[[nm]][["readOnly"]])) prop_state(prop) else prop
  })
  names(properties) <- names(props)

  new_class(
    name = name %||% schema[["title"]] %||% "JSONSchemaClass",
    package = package,
    properties = properties
  )
} # /rtemis::JSONSchema_to_S7
