# 00_Props.R
# ::rtemis::
# 2026- EDG rtemis.org

# S7 property factories: each factory returns an S7 property whose type,
# default, and validation logic are generated from a `PropertySpec` — a nested
# S7 object that rides along on the property. Because the constraints are data
# (not closures), `S7_to_JSONSchema()` can convert any class built from these
# factories to a JSON Schema mechanically: one declaration produces the R
# validator, the default, the schema, and (downstream) the TUI form.
#
# JSON Schema mapping (see spec_to_schema):
# - type/bounds/enum      -> "type", "minimum"/"maximum"/"exclusiveMinimum"/
#                            "exclusiveMaximum", "enum"
# - nullable = TRUE       -> "null" added to "type"
# - tunable = TRUE        -> "oneOf": [scalar, array-of-scalar (search values)]
# - vector = TRUE         -> "type": "array" (a genuinely vector-valued field,
#                            e.g. per-feature weights; NOT search values —
#                            mutually exclusive with tunable)
# - default, description  -> "default", "description" (annotations)

# %% PropertySpec ----
#' PropertySpec
#'
#' @description
#' Machine-readable specification of an S7 property: its JSON type, bounds,
#' enum, default, and flags. Created by the `prop_*` factories; consumed by
#' the generated validators and by [S7_to_JSONSchema]. The validator also
#' checks that the default itself conforms to the spec, so a bad declaration
#' (e.g. a default outside its own bounds, or a double default for an integer
#' property) fails at factory time — i.e. at package load — rather than at
#' first instantiation.
#'
#' @field type Character: JSON Schema base type
#'   \{"boolean", "integer", "number", "string"\}.
#' @field default Default value (scalar of `type`, or NULL).
#' @field minimum,maximum Numeric or NULL: Inclusive bounds.
#' @field exclusive_minimum,exclusive_maximum Numeric or NULL: Exclusive bounds.
#' @field enum Character or NULL: Allowed values (string type only).
#' @field nullable Logical: If TRUE, NULL is a valid value.
#' @field tunable Logical: If TRUE, a vector of search values (length >= 1) is
#'   accepted; if FALSE, only a scalar.
#' @field vector Logical: If TRUE, the value is genuinely vector-valued
#'   (length >= 1; e.g. per-feature weights) and maps to a JSON array.
#'   Mutually exclusive with `tunable` (a vector value is not a set of
#'   search values).
#' @field description Character: Human-readable description (schema
#'   "description", TUI help text).
#'
#' @author EDG
#' @noRd
PropertySpec <- new_class(
  name = "PropertySpec",
  package = "rtemis",
  properties = list(
    type = class_character,
    default = class_any,
    minimum = class_numeric | NULL,
    maximum = class_numeric | NULL,
    exclusive_minimum = class_numeric | NULL,
    exclusive_maximum = class_numeric | NULL,
    enum = class_character | NULL,
    nullable = class_logical,
    tunable = class_logical,
    vector = class_logical,
    description = class_character
  ),
  validator = function(self) {
    if (!self@type %in% c("boolean", "integer", "number", "string")) {
      return("@type must be one of 'boolean', 'integer', 'number', 'string'.")
    }
    if (!is.null(self@enum) && self@type != "string") {
      return("@enum is only supported for type 'string'.")
    }
    if (self@vector && self@tunable) {
      return(
        "@vector and @tunable are mutually exclusive (a vector value is not a set of search values)."
      )
    }
    if (
      !is.null(self@minimum) &&
        !is.null(self@maximum) &&
        self@minimum > self@maximum
    ) {
      return("@minimum must not exceed @maximum.")
    }
    # The default must itself conform to the spec.
    # An invalid declaration fails on package load, not at first instantiation.
    if (!is.null(self@default)) {
      type_ok <- switch(
        self@type,
        boolean = is.logical(self@default),
        integer = is.integer(self@default),
        number = is.numeric(self@default),
        string = is.character(self@default)
      )
      if (!type_ok) {
        return(paste0("@default must be of type '", self@type, "'."))
      }
    }
    default_msg <- validate_with_spec(self@default, self)
    if (!is.null(default_msg)) {
      return(paste0("@default ", default_msg))
    }
    NULL
  }
) # /rtemis::PropertySpec


# %% validate_with_spec ----
#' Validate a property value against its PropertySpec
#'
#' Shared validator body for all factory-generated properties. Returns NULL if
#' valid, otherwise a character message (the S7 validator contract). The
#' property's S7 class (set by the factory) already enforces the base type;
#' this checks arity, missingness, bounds, and enum membership.
#'
#' @param value Property value being set.
#' @param spec `PropertySpec` object.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_with_spec <- function(value, spec) {
  if (is.null(value) || length(value) == 0L) {
    # S7 initializes a `class | NULL` union property to the empty vector of
    # its first member, so zero-length is the package-wide "unset" (compare
    # `.compact_config`, which drops zero-length values on write).
    return(if (spec@nullable) NULL else "must not be NULL or empty.")
  }
  if (length(value) > 1L && !spec@tunable && !spec@vector) {
    return("must be a single value (not tunable, no search values allowed).")
  }
  if (anyNA(value)) {
    return("must not contain missing values.")
  }
  if (!is.null(spec@minimum) && any(value < spec@minimum)) {
    return(paste0("must be >= ", spec@minimum, "."))
  }
  if (!is.null(spec@maximum) && any(value > spec@maximum)) {
    return(paste0("must be <= ", spec@maximum, "."))
  }
  if (
    !is.null(spec@exclusive_minimum) && any(value <= spec@exclusive_minimum)
  ) {
    return(paste0("must be > ", spec@exclusive_minimum, "."))
  }
  if (
    !is.null(spec@exclusive_maximum) && any(value >= spec@exclusive_maximum)
  ) {
    return(paste0("must be < ", spec@exclusive_maximum, "."))
  }
  if (!is.null(spec@enum) && !all(value %in% spec@enum)) {
    return(paste0(
      "must be one of ",
      paste0("'", spec@enum, "'", collapse = ", "),
      "."
    ))
  }
  NULL
} # /rtemis::validate_with_spec


# %% make_prop ----
#' Build an S7 property from a PropertySpec
#'
#' Internal engine behind the `prop_*` factories: derives the property's S7
#' class from the spec's type (union with NULL when nullable), installs a
#' spec-driven validator, and stores the spec on the property (element
#' `"spec"`; S7 properties are named lists, and S7 accesses its own fields by
#' name, so the extra element is inert to S7 itself).
#'
#' @param spec `PropertySpec` object.
#'
#' @return S7 property (with the spec attached as `$spec`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_prop <- function(spec) {
  base_class <- switch(
    spec@type,
    boolean = class_logical,
    integer = class_integer,
    number = class_numeric,
    string = class_character
  )
  p <- new_property(
    class = if (spec@nullable) base_class | NULL else base_class,
    default = spec@default,
    validator = function(value) validate_with_spec(value, spec)
  )
  p[["spec"]] <- spec
  p
} # /rtemis::make_prop


# %% prop_boolean ----
#' Logical (boolean) S7 property with attached PropertySpec
#'
#' @param default Logical: Default value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_boolean <- function(default = FALSE, tunable = FALSE, description = "") {
  make_prop(PropertySpec(
    type = "boolean",
    default = default,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = FALSE,
    tunable = tunable,
    vector = FALSE,
    description = description
  ))
} # /rtemis::prop_boolean


# %% prop_integer ----
#' Integer S7 property with attached PropertySpec
#'
#' @param default Integer: Default value (NULL only if `nullable`).
#' @param min,max Integer or NULL: Inclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_integer <- function(
  default,
  min = NULL,
  max = NULL,
  nullable = FALSE,
  tunable = FALSE,
  vector = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "integer",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = tunable,
    vector = vector,
    description = description
  ))
} # /rtemis::prop_integer


# %% prop_float ----
#' Numeric (floating-point) S7 property with attached PropertySpec
#'
#' The only factory whose name differs from its JSON Schema type: it emits
#' type "number" (which in JSON Schema includes integers), but is named
#' `prop_float` because declarers think in the integer/float pairing —
#' "number" next to `prop_integer` invites the same ambiguity as R's
#' "numeric". Accepts R integer values too (`class_numeric`): JSON numbers
#' parse to double anyway, and floats are a superset of integers.
#'
#' @param default Numeric: Default value (NULL only if `nullable`).
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_float <- function(
  default,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  tunable = FALSE,
  vector = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "number",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = exclusive_min,
    exclusive_maximum = exclusive_max,
    enum = NULL,
    nullable = nullable,
    tunable = tunable,
    vector = vector,
    description = description
  ))
} # /rtemis::prop_float


# %% prop_string ----
#' Character (string) S7 property with attached PropertySpec
#'
#' @param default Character: Default value (NULL only if `nullable`).
#' @param enum Character or NULL: Allowed values.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_string <- function(
  default,
  enum = NULL,
  nullable = FALSE,
  tunable = FALSE,
  vector = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "string",
    default = default,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = enum,
    nullable = nullable,
    tunable = tunable,
    vector = vector,
    description = description
  ))
} # /rtemis::prop_string


# %% get_spec ----
#' Get the PropertySpec of an S7 property, or NULL
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return `PropertySpec` object or NULL if the property was not built by a
#'   `prop_*` factory.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_spec <- function(prop) {
  prop[["spec"]]
} # /rtemis::get_spec


# %% spec_prop_names ----
#' Names of factory-declared properties of an S7 class
#'
#' @param x S7 class.
#'
#' @return Character vector: Names of properties carrying a `PropertySpec`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_prop_names <- function(x) {
  names(Filter(function(p) !is.null(get_spec(p)), x@properties))
} # /rtemis::spec_prop_names


# %% tunable_spec_names ----
#' Names of tunable factory-declared properties of an S7 class
#'
#' @param x S7 class.
#'
#' @return Character vector: Names of properties whose `PropertySpec` has
#'   `tunable = TRUE`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tunable_spec_names <- function(x) {
  names(Filter(
    function(p) {
      s <- get_spec(p)
      !is.null(s) && s@tunable
    },
    x@properties
  ))
} # /rtemis::tunable_spec_names


# %% fixed_spec_names ----
#' Names of fixed (non-tunable) factory-declared properties of an S7 class
#'
#' @param x S7 class.
#'
#' @return Character vector: Names of properties whose `PropertySpec` has
#'   `tunable = FALSE`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fixed_spec_names <- function(x) {
  names(Filter(
    function(p) {
      s <- get_spec(p)
      !is.null(s) && !s@tunable
    },
    x@properties
  ))
} # /rtemis::fixed_spec_names


# %% spec_prop_values ----
#' Collect factory-declared property values from an S7 instance
#'
#' Returns the current values of all spec-carrying properties as a named
#' list, mapping zero-length values to NULL (the package-wide "unset"; see
#' `validate_with_spec`).
#'
#' @param self S7 object whose class declares `prop_*` properties.
#'
#' @return Named list of property values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_prop_values <- function(self) {
  nms <- spec_prop_names(S7_class(self))
  out <- lapply(nms, function(nm) {
    v <- prop(self, nm)
    if (length(v) == 0L) NULL else v
  })
  names(out) <- nms
  out
} # /rtemis::spec_prop_values


# %% spec_to_schema ----
#' Convert a PropertySpec to a JSON Schema property (as a list)
#'
#' Bounds and enum map to their JSON Schema keywords; `nullable` adds "null"
#' to the type; `tunable` wraps the scalar schema in
#' `oneOf: [scalar, array-of-scalar]`, the array being tuning search values.
#' Values that must serialize as JSON arrays even at length 1 (enum, type
#' unions) are wrapped in `I()` for `jsonlite::toJSON(auto_unbox = TRUE)`.
#'
#' @param spec `PropertySpec` object.
#'
#' @return Named list (JSON Schema property).
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_to_schema <- function(spec) {
  scalar <- Filter(
    Negate(is.null),
    list(
      type = spec@type,
      minimum = spec@minimum,
      maximum = spec@maximum,
      exclusiveMinimum = spec@exclusive_minimum,
      exclusiveMaximum = spec@exclusive_maximum,
      enum = if (!is.null(spec@enum)) I(spec@enum) else NULL
    )
  )
  out <- if (spec@vector) {
    # A genuinely vector-valued field (e.g. per-feature weights).
    arr <- list(
      type = if (spec@nullable) I(c("array", "null")) else "array",
      items = scalar,
      minItems = 1L
    )
    arr
  } else if (spec@tunable) {
    # Scalar, or an array of search values for the Tuner.
    array_schema <- list(
      type = "array",
      items = scalar,
      minItems = 1L,
      description = "Tuning search values."
    )
    branches <- list(scalar, array_schema)
    if (spec@nullable) {
      branches <- c(list(list(type = "null")), branches)
    }
    list(oneOf = branches)
  } else if (spec@nullable) {
    scalar[["type"]] <- I(c(spec@type, "null"))
    scalar
  } else {
    scalar
  }
  # Annotations last, at the top level of the property schema.
  if (!is.null(spec@default)) {
    out[["default"]] <- spec@default
  } else if (spec@nullable) {
    out[["default"]] <- NA # -> null (jsonlite na = "null")
  }
  if (nzchar(spec@description)) {
    out[["description"]] <- spec@description
  }
  out
} # /rtemis::spec_to_schema


# %% S7_to_JSONSchema ----
#' Convert an S7 class built with `prop_*` factories to a JSON Schema
#'
#' Walks the class's properties, reads each attached `PropertySpec`, and
#' assembles a draft 2020-12 JSON Schema. Properties without a spec must be
#' explicitly listed in `exclude` (e.g. runtime state like `tuned`,
#' `resampled`, `n_workers`) — an unexpected spec-less property is an error,
#' so a class that drifts from the factory vocabulary fails loudly instead of
#' emitting a wrong schema.
#'
#' @param x S7 class (e.g. `LightRFHyperparameters`).
#' @param id Character: Schema `$id` URL
#'   (e.g. "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json").
#' @param title Character: Schema title. Defaults to the class name.
#' @param description Character: Schema description. If empty, the
#'   "description" keyword is omitted from the schema.
#' @param exclude Character: Names of properties to omit (runtime state,
#'   data-dependent values).
#' @param required Character: Names of required properties. Default NULL: all
#'   optional, so omitted fields fall back to their `setup_*` defaults on read
#'   (matching [write_config]'s compaction).
#' @param extra Named list merged into the schema after generation, for
#'   cross-field constraints that are not per-property (e.g. an `allOf` of
#'   if/then clauses for kernel-specific SVM hyperparameters).
#' @param instance_schema_url Character or NULL: If set, adds a `$schema`
#'   const property (instances self-identify, as in the config families).
#'
#' @return Named list: the JSON Schema. Serialize with [write_JSONSchema].
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' schema <- S7_to_JSONSchema(
#'   LightRFHyperparameters,
#'   id = "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json",
#'   exclude = c("algorithm", "tuned", "resampled", "n_workers")
#' )
#' }
S7_to_JSONSchema <- function(
  x,
  id,
  title = NULL,
  description = "",
  exclude = character(),
  required = NULL,
  extra = NULL,
  instance_schema_url = NULL
) {
  check_character(id, allow_null = FALSE)
  if (!inherits(x, "S7_class")) {
    rtemis.core::abort(
      "`x` must be an S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  props <- x@properties
  props <- props[!names(props) %in% exclude]
  specless <- names(props)[vapply(
    props,
    function(p) is.null(get_spec(p)),
    logical(1L)
  )]
  if (length(specless) > 0L) {
    rtemis.core::abort(
      "Properties without a PropertySpec (build them with the prop_* factories, or `exclude` them): ",
      paste(specless, collapse = ", "),
      ".",
      class = "rtemis_input_error"
    )
  }
  properties <- lapply(props, function(p) spec_to_schema(get_spec(p)))
  if (!is.null(instance_schema_url)) {
    properties <- c(
      list(
        `$schema` = list(
          type = "string",
          const = instance_schema_url,
          description = "JSON Schema URI for this config instance."
        )
      ),
      properties
    )
  }
  schema <- list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    `$id` = id,
    title = if (is.null(title)) x@name else title,
    description = if (nzchar(description)) description else NULL,
    type = "object",
    additionalProperties = FALSE,
    properties = properties
  )
  schema <- Filter(Negate(is.null), schema)
  if (!is.null(required)) {
    schema[["required"]] <- I(required)
  }
  if (!is.null(extra)) {
    schema <- utils::modifyList(schema, extra)
  }
  schema
} # /rtemis::S7_to_JSONSchema


# %% write_JSONSchema ----
#' Write a schema list produced by [S7_to_JSONSchema] to a JSON file
#'
#' @param schema Named list: Schema produced by [S7_to_JSONSchema].
#' @param file Character: Path to output JSON file.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `schema`, invisibly.
#'
#' @author EDG
#' @export
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' schema <- list(
#'   `$schema` = "https://json-schema.org/draft/2020-12/schema",
#'   `$id` = "https://example.org/demo/v1/schema.json",
#'   title = "Demo",
#'   type = "object",
#'   properties = list(n = list(type = "integer", minimum = 1L))
#' )
#' tmpfile <- file.path(tempdir(), "demo.schema.json")
#' write_JSONSchema(schema, tmpfile, overwrite = TRUE, verbosity = 0L)
write_JSONSchema <- function(schema, file, overwrite = FALSE, verbosity = 1L) {
  check_dependencies("jsonlite")
  json_str <- as.character(jsonlite::toJSON(
    schema,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null",
    digits = NA
  ))
  write_lines(
    json_str,
    file = file,
    overwrite = overwrite,
    verbosity = verbosity
  )
  invisible(schema)
} # /rtemis::write_JSONSchema
