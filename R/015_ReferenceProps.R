# 015_ReferenceProps.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% validate_reference_value ----
#' Validate an object or collection against its declared element class
#' @param value Property value.
#' @param fields Named list: PropertySpec fields.
#' @return Character validation message or NULL.
#' @keywords internal
#' @noRd
validate_reference_value <- function(value, fields) {
  if (is.null(value)) {
    return(if (fields[["nullable"]]) NULL else "must not be NULL.")
  }
  container <- fields[["container"]]
  if (container == "none") {
    values <- list(value)
  } else {
    if (!is.list(value) || is.data.frame(value)) {
      return("must be a list of S7 objects.")
    }
    if (length(value) < fields[["min_items"]]) {
      return(paste0("must hold at least ", fields[["min_items"]], " objects."))
    }
    maximum <- fields[["max_items"]]
    if (!is.null(maximum) && length(value) > maximum) {
      return(paste0("must hold at most ", maximum, " objects."))
    }
    if (container == "map" && length(value) > 0L) {
      nms <- names(value)
      if (
        is.null(nms) ||
          anyNA(nms) ||
          any(!nzchar(nms)) ||
          anyDuplicated(nms) > 0L
      ) {
        return("must have unique, non-empty names.")
      }
      invalid <- rep(FALSE, length(nms))
      if (!is.null(fields[["key_pattern"]])) {
        invalid <- invalid | !grepl(fields[["key_pattern"]], nms, perl = TRUE)
      }
      if (!is.null(fields[["key_not_pattern"]])) {
        invalid <- invalid |
          grepl(fields[["key_not_pattern"]], nms, perl = TRUE)
      }
      if (any(invalid)) {
        return(paste0(
          "names violate their declared key grammar: ",
          paste(nms[invalid], collapse = ", "),
          "."
        ))
      }
    }
    values <- value
  }
  valid <- vapply(
    values,
    function(x) {
      S7_inherits(x) &&
        inherits(x, c(fields[["target_class"]], fields[["alternate_class"]]))
    },
    logical(1L)
  )
  if (!all(valid)) {
    return(paste0(
      "must hold ",
      fields[["target_class"]],
      " objects; invalid element ",
      which(!valid)[[1L]],
      "."
    ))
  }
  if (isTRUE(fields[["same_variant"]]) && length(values) > 1L) {
    family <- reference_family(fields[["target_class"]])
    variants <- vapply(
      values,
      function(x) prop(x, family$discriminator),
      character(1L)
    )
    if (length(unique(variants)) > 1L) {
      return("every member must be for the same algorithm variant.")
    }
  }
  NULL
}


# %% make_reference_prop ----
#' Build an S7 property from a class-reference declaration
#' @param spec `PropertySpec`: Reference declaration.
#' @return S7 property.
#' @keywords internal
#' @noRd
make_reference_prop <- function(spec) {
  fields <- spec_fields(spec)
  # Target identity is validated without loading or constructing that class.
  # This allows the same declaration to represent forward and recursive refs.
  cls <- if (spec@container == "none") S7_object else class_list
  p <- new_property(
    class = if (spec@nullable) NULL | cls else cls,
    default = spec@default,
    validator = spec_validator(fields)
  )
  p[["spec"]] <- fields
  p
}


# %% prop_object ----
#' Declare a property holding an S7 object
#' @param cls S7 class: Required object type, including its subclasses.
#' @param default Optional: Literal or constructor expression evaluated per instance.
#' @param nullable Logical: Whether NULL is accepted.
#' @param description Character: Description for schema consumers.
#' @return S7 property carrying a PropertySpec.
#' @keywords internal
#' @noRd
prop_object <- function(
  cls,
  default = NULL,
  nullable = FALSE,
  description = ""
) {
  if (
    !inherits(cls, "S7_class") || is.null(cls@package) || !nzchar(cls@package)
  ) {
    rtemis.core::abort(
      "`cls` must be an S7 class with a package identity.",
      class = "rtemis_type_error"
    )
  }
  make_prop(PropertySpec(
    type = "object",
    target_class = paste0(cls@package, "::", cls@name),
    default = default,
    nullable = nullable,
    tunable = FALSE,
    container = "none",
    broadcast = FALSE,
    description = description
  ))
}


# %% prop_collection ----
#' Declare a homogeneous collection of S7 objects
#' @param cls S7 class: Required type of every element.
#' @param default Optional: Literal or constructor expression evaluated per instance.
#' @param container Character \{"array", "map"\}: Positional array or named map.
#' @param nullable Logical: Whether NULL is accepted.
#' @param min_items Integer [0, Inf): Minimum collection size.
#' @param max_items Optional Integer [0, Inf): Maximum collection size.
#' @param same_variant Logical: Whether every member must share its family discriminator.
#' @param key_pattern,key_not_pattern Optional Character: Required and forbidden patterns constraining map keys.
#' @param description Character: Description for schema consumers.
#' @return S7 property carrying a PropertySpec.
#' @keywords internal
#' @noRd
prop_collection <- function(
  cls,
  default = NULL,
  container = "array",
  nullable = FALSE,
  min_items = 0L,
  max_items = NULL,
  same_variant = FALSE,
  key_pattern = NULL,
  key_not_pattern = NULL,
  description = ""
) {
  if (
    !inherits(cls, "S7_class") || is.null(cls@package) || !nzchar(cls@package)
  ) {
    rtemis.core::abort(
      "`cls` must be an S7 class with a package identity.",
      class = "rtemis_type_error"
    )
  }
  if (
    !is.character(container) ||
      length(container) != 1L ||
      is.na(container) ||
      !container %in% c("array", "map")
  ) {
    rtemis.core::abort(
      "`container` must be 'array' or 'map'.",
      class = "rtemis_value_error"
    )
  }
  make_prop(PropertySpec(
    type = "object",
    target_class = paste0(cls@package, "::", cls@name),
    default = default,
    nullable = nullable,
    tunable = FALSE,
    container = container,
    broadcast = FALSE,
    min_items = min_items,
    max_items = max_items,
    same_variant = same_variant,
    key_pattern = key_pattern,
    key_not_pattern = key_not_pattern,
    description = description
  ))
}


# %% schema_reference_urls ----
#' Resolve reference identities against an already discovered publication graph
#' @param catalog Named list: Derived schema catalog.
#' @param base_url Character: Publication base URL.
#' @param record Logical: Whether referenced configs use their record form.
#' @return Named character vector mapping qualified class identities to URLs.
#' @keywords internal
#' @noRd
schema_reference_urls <- function(catalog, base_url, record = FALSE) {
  out <- character()
  for (slug in names(catalog$families)) {
    family <- catalog$families[[slug]]
    base <- family$base_class
    file <- if (record) "record.json" else "schema.json"
    out[[paste0(base@package, "::", base@name)]] <- paste0(
      base_url,
      "/",
      slug,
      "/v1/",
      file
    )
    for (leaf in family$algorithms) {
      cls <- leaf$cls
      variant <- tolower(discriminator_value(cls, family$discriminator))
      out[[paste0(cls@package, "::", cls@name)]] <- paste0(
        base_url,
        "/",
        slug,
        "/",
        variant,
        "/v1/",
        file
      )
    }
  }
  for (slug in names(catalog$flat_configs)) {
    entry <- catalog$flat_configs[[slug]]
    cls <- entry$cls
    file <- if (record && entry$kind %in% c("config", "pipeline")) {
      "record.json"
    } else {
      "schema.json"
    }
    out[[paste0(cls@package, "::", cls@name)]] <- paste0(
      base_url,
      "/",
      slug,
      "/v1/",
      file
    )
  }
  out
}


# %% reference_schema ----
#' Emit the standard JSON Schema shape of a typed class reference
#' @param spec `PropertySpec`: Object or collection declaration.
#' @param target Character: Resolved target schema URL.
#' @return Named list containing standard JSON Schema keywords.
#' @keywords internal
#' @noRd
reference_schema <- function(spec, target) {
  if (is.null(target)) {
    rtemis.core::abort(
      "No published schema for reference target ",
      spec@target_class,
      ".",
      class = "rtemis_schema_error"
    )
  }
  ref <- list(`$ref` = target)
  if (!is.null(spec@alternate_class)) {
    return(reference_choice_schema(spec, target))
  }
  if (spec@container == "none") {
    return(
      if (spec@nullable) list(oneOf = list(list(type = "null"), ref)) else ref
    )
  }
  out <- if (spec@container == "array") {
    Filter(
      Negate(is.null),
      list(
        type = if (spec@nullable) I(c("array", "null")) else "array",
        items = ref,
        minItems = spec@min_items,
        maxItems = spec@max_items
      )
    )
  } else {
    Filter(
      Negate(is.null),
      list(
        type = if (spec@nullable) I(c("object", "null")) else "object",
        additionalProperties = ref,
        propertyNames = list(minLength = 1L),
        minProperties = spec@min_items,
        maxProperties = spec@max_items
      )
    )
  }
  if (!is.null(spec@key_pattern)) {
    out[["propertyNames"]][["pattern"]] <- spec@key_pattern
  }
  if (!is.null(spec@key_not_pattern)) {
    out[["propertyNames"]][["not"]] <- list(pattern = spec@key_not_pattern)
  }
  if (spec@same_variant) {
    family <- reference_family(spec@target_class)
    variants <- vapply(
      family$algorithms,
      function(a) discriminator_value(a$cls, family$discriminator),
      character(1L)
    )
    branches <- lapply(variants, function(value) {
      item <- list(
        properties = stats::setNames(
          list(list(const = value)),
          family$discriminator
        )
      )
      stats::setNames(
        list(item),
        if (spec@container == "map") "additionalProperties" else "items"
      )
    })
    out[["allOf"]] <- list(list(anyOf = branches))
  }
  out
}


# %% reference_family ----
#' Resolve a qualified identity to a declared family
#' @param target Character: Qualified family class identity.
#' @return Named list: Family catalog entry.
#' @keywords internal
#' @noRd
reference_family <- function(target) {
  families <- schema_catalog()$families
  matches <- Filter(
    function(f) {
      identical(paste0(f$base_class@package, "::", f$base_class@name), target)
    },
    families
  )
  if (length(matches) != 1L) {
    rtemis.core::abort(
      "Reference ",
      target,
      " must identify a published family.",
      class = "rtemis_schema_error"
    )
  }
  matches[[1L]]
}


# %% prop_object_choice ----
#' Declare two object forms selected by the presence of a structural key
#' @param primary,alternate S7 class: Ordinary reference and inline alternative.
#' @param presence_key Character: Key selecting the alternate form.
#' @param nullable Logical: Whether NULL is accepted.
#' @param description Character: Guidance for selecting a form.
#' @return S7 property carrying both class identities and the selection rule.
#' @keywords internal
#' @noRd
prop_object_choice <- function(
  primary,
  alternate,
  presence_key,
  nullable = FALSE,
  description = ""
) {
  primary_spec <- get_spec(prop_object(
    primary,
    nullable = nullable,
    description = description
  ))
  alternate_spec <- get_spec(prop_object(alternate))
  if (
    !is.character(presence_key) ||
      length(presence_key) != 1L ||
      is.na(presence_key) ||
      !presence_key %in% published_prop_names(alternate) ||
      presence_key %in% published_prop_names(primary)
  ) {
    rtemis.core::abort(
      "`presence_key` must belong to the alternate class alone.",
      class = "rtemis_schema_error"
    )
  }
  fields <- spec_fields(primary_spec)
  fields[["alternate_class"]] <- alternate_spec@target_class
  fields[["presence_key"]] <- presence_key
  make_prop(do.call(PropertySpec, fields))
}


# %% reference_choice_schema ----
#' Generate the presence-selected form from its inline class declaration
#' @param spec `PropertySpec`: Object choice declaration.
#' @param target Character: Resolved primary reference URL.
#' @return Named list: Property schema.
#' @keywords internal
#' @noRd
reference_choice_schema <- function(spec, target) {
  alternate <- schema_catalog()$inline[[spec@alternate_class]]
  if (is.null(alternate)) {
    rtemis.core::abort(
      "Alternate class ",
      spec@alternate_class,
      " must declare an inline schema.",
      class = "rtemis_schema_error"
    )
  }
  record <- endsWith(target, "/record.json")
  schema <- S7_to_JSONSchema(
    alternate$cls,
    id = paste0("urn:rtemis:inline:", alternate$cls@name),
    title = alternate$title,
    description = alternate$description,
    record = record,
    asserted = !record
  )
  schema[c("$id", "$schema", "required")] <- NULL
  list(
    type = if (spec@nullable) I(c("object", "null")) else "object",
    allOf = list(
      list(
        `if` = list(type = "object", required = I(spec@presence_key)),
        then = schema
      ),
      list(
        `if` = list(
          type = "object",
          not = list(required = I(spec@presence_key))
        ),
        then = list(`$ref` = target)
      )
    )
  )
}


# %% from_wire_object ----
#' Reconstruct a referenced object with its class's reader
#' @param value Named list or S7 object: Referenced value.
#' @param target Character: Qualified class identity.
#' @return S7 object of the declared type.
#' @keywords internal
#' @noRd
from_wire_object <- function(value, target) {
  if (S7_inherits(value)) {
    if (!inherits(value, target)) {
      rtemis.core::abort("Expected ", target, ".", class = "rtemis_type_error")
    }
    return(value)
  }
  identity <- strsplit(target, "::", fixed = TRUE)[[1L]]
  ns <- asNamespace(identity[[1L]])
  reader_name <- paste0(".list_to_", identity[[2L]])
  if (!exists(reader_name, envir = ns, inherits = FALSE)) {
    rtemis.core::abort(
      "No wire reader declared for ",
      target,
      ".",
      class = "rtemis_schema_error"
    )
  }
  result <- get(reader_name, envir = ns, inherits = FALSE)(value)
  if (!S7_inherits(result) || !inherits(result, target)) {
    rtemis.core::abort(
      "Wire reader for ",
      target,
      " returned the wrong class.",
      class = "rtemis_schema_error"
    )
  }
  result
}
