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
#' @param default Logical: Default value (NULL only if `nullable`).
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_boolean <- function(
  default = FALSE,
  nullable = FALSE,
  tunable = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "boolean",
    default = default,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
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


# %% prop_accepts_null ----
#' Does an S7 property accept NULL?
#'
#' TRUE when the property's class is a union with NULL as a member (e.g.
#' `NULL | PreprocessorConfig`), i.e. the field is optional. Used to decide
#' whether a nested-config `$ref` should also admit `null`.
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_accepts_null <- function(prop) {
  cls <- prop[["class"]]
  inherits(cls, "S7_union") &&
    any(vapply(cls[["classes"]], is.null, logical(1L)))
} # /rtemis::prop_accepts_null


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


# --- Config-family machinery -----------------------------------------------

# %% prop_algorithm ----
#' Computed constant `algorithm` property
#'
#' Each config subclass overrides the inherited `algorithm` property with a
#' computed constant, so the value is always correct and never stored.
#'
#' @param algorithm Character: Algorithm name.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_algorithm <- function(algorithm) {
  force(algorithm)
  new_property(class_character, getter = function(self) algorithm)
} # /rtemis::prop_algorithm


# Shared by the config families whose public shape is an `algorithm` + a named
# parameter list (`hyperparameters`, decomposition/clustering `config`): an
# abstract base holding run/meta state, and per-algorithm subclasses that
# declare each parameter as its own (factory or plain) property. The base's
# list property is computed from the subclass's own properties (getter) and
# assignments route back to them (setter). "Own" = declared by the subclass,
# i.e. not inherited from the base.

# %% own_prop_names ----
#' Names of a subclass's own properties (excluding the base's)
#'
#' @param x S7 class (a subclass of `base`).
#' @param base S7 class: the family base class.
#'
#' @return Character vector of property names declared by `x` itself.
#'
#' @author EDG
#' @keywords internal
#' @noRd
own_prop_names <- function(x, base) {
  setdiff(names(x@properties), names(base@properties))
} # /rtemis::own_prop_names


# %% own_prop_values ----
#' Collect a subclass's own property values as a named list
#'
#' Zero-length values (S7's "unset" for `class | NULL` unions) map to NULL;
#' function-valued properties (e.g. a torch optimizer) pass through.
#'
#' @param self S7 object.
#' @param base S7 class: the family base class.
#'
#' @return Named list of the subclass's own property values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
own_prop_values <- function(self, base) {
  nms <- own_prop_names(S7_class(self), base)
  out <- lapply(nms, function(nm) {
    v <- prop(self, nm)
    # Zero-length is the "unset union" convention -> NULL (this also
    # normalizes an empty list, so `class_list | NULL` props read as NULL
    # whether S7 initialized them to `list()` or `NULL`). Functions pass
    # through (e.g. a torch optimizer).
    if (length(v) == 0L && !is.function(v)) NULL else v
  })
  names(out) <- nms
  out
} # /rtemis::own_prop_values


# %% config_prop_values ----
#' A subclass's own property values that belong in a serialized config
#'
#' The runtime parameter list (`@hyperparameters` / `@config`) intentionally
#' carries everything a training backend needs, including algorithm constants
#' and run state. A *serialized config* is narrower: only the declared,
#' user-settable parameters — i.e. properties built by a `prop_*` factory —
#' plus nested config objects (which serialize as their own schema). Dropped
#' here, and correspondingly absent from the generated schemas:
#' unsettable constants (`hp_constants()`, which are not properties at all),
#' run state written during training (e.g. GLMNET `lambda.min`, LightGBM
#' `best_iter`), and values with no JSON form (e.g. tSNE `Y_init`, TabNet
#' `optimizer`). All are reconstructed or re-derived on read.
#'
#' @param self S7 object.
#' @param base S7 class: the family base class.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_prop_values <- function(self, base) {
  values <- own_prop_values(self, base)
  declared <- spec_prop_names(S7_class(self))
  keep <- vapply(
    names(values),
    function(nm) nm %in% declared || S7_inherits(values[[nm]]),
    logical(1L)
  )
  values[keep]
} # /rtemis::config_prop_values


# %% route_config_assignment ----
#' Route a named-list assignment to a config object's properties
#'
#' Shared setter body for the computed `hyperparameters` / `config` property.
#' Each named element is assigned to the matching own property (where it is
#' validated). Names in `constants` are unsettable: assigning an identical
#' value is a no-op, a different value errors. Unknown names error. This
#' makes `x@config[["p"]] <- v` (which desugars to a whole-list round-trip)
#' both validate `v` and reject stray keys.
#'
#' @param self S7 object being modified.
#' @param base S7 class: the family base class.
#' @param value Named list of values to assign.
#' @param constants Named list of unsettable constants (default none).
#' @param label Character: object label for error messages (e.g. the
#'   algorithm name).
#' @param noun Character: what the named elements are, for error messages
#'   (e.g. "hyperparameter", "parameter").
#'
#' @return `self`, modified.
#'
#' @author EDG
#' @keywords internal
#' @noRd
route_config_assignment <- function(
  self,
  base,
  value,
  constants = list(),
  label = "config",
  noun = "parameter"
) {
  settable <- own_prop_names(S7_class(self), base)
  for (nm in names(value)) {
    if (nm %in% settable) {
      prop(self, nm) <- value[[nm]]
    } else if (nm %in% names(constants)) {
      if (!identical(value[[nm]], constants[[nm]])) {
        rtemis.core::abort(
          label,
          " ",
          noun,
          " '",
          nm,
          "' is a constant and cannot be changed.",
          class = "rtemis_input_error"
        )
      }
    } else {
      rtemis.core::abort(
        "Unknown ",
        label,
        " ",
        noun,
        " '",
        nm,
        "'.",
        class = "rtemis_input_error"
      )
    }
  }
  self
} # /rtemis::route_config_assignment


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
#' @param refs Named character: Properties holding a nested config object,
#'   mapped to the `$id` of the schema for that config. Each emits a `$ref` (or
#'   `oneOf: [null, $ref]` when the property accepts NULL, detected from its
#'   S7 union), instead of requiring a `PropertySpec`. Names must match
#'   existing properties.
#' @param closed Logical: If TRUE (default) the schema sets
#'   `additionalProperties: false`. Pass FALSE for leaves composed into a
#'   top-level-mode dispatcher, which enforces strictness with
#'   `unevaluatedProperties` instead (see [S7_dispatcher_JSONSchema]).
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
  refs = NULL,
  closed = TRUE,
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
  if (!is.null(refs)) {
    unknown <- setdiff(names(refs), names(props))
    if (length(unknown) > 0L) {
      rtemis.core::abort(
        "`refs` names no such (or excluded) propert",
        if (length(unknown) == 1L) "y: " else "ies: ",
        paste(unknown, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  ref_props <- props[names(props) %in% names(refs)]
  props <- props[!names(props) %in% names(refs)]
  specless <- names(props)[vapply(
    props,
    function(p) is.null(get_spec(p)),
    logical(1L)
  )]
  if (length(specless) > 0L) {
    rtemis.core::abort(
      "Properties without a PropertySpec (build them with the prop_* factories, `refs` them, or `exclude` them): ",
      paste(specless, collapse = ", "),
      ".",
      class = "rtemis_input_error"
    )
  }
  properties <- lapply(props, function(p) spec_to_schema(get_spec(p)))
  # Nested config properties reference their own schema. A property whose S7
  # class is a union containing NULL is optional, so it also admits null.
  for (nm in names(ref_props)) {
    ref <- list(`$ref` = unname(refs[[nm]]))
    properties[[nm]] <- if (prop_accepts_null(ref_props[[nm]])) {
      list(oneOf = list(list(type = "null"), ref))
    } else {
      ref
    }
  }
  # Preserve declaration order.
  properties <- properties[intersect(
    names(x@properties),
    names(properties)
  )]
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
    additionalProperties = if (closed) FALSE else NULL,
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


# %% S7_dispatcher_JSONSchema ----
#' Generate a per-algorithm dispatcher JSON Schema
#'
#' Assembles the `<family>/v1` schema for a config family: an object with an
#' `algorithm` discriminator and an algorithm-specific payload
#' (`config` / `hyperparameters`), plus an `allOf` of `if/then` clauses that
#' validate the payload against the per-algorithm leaf schema
#' (`<family>/<algorithm>/v1`) selected by `algorithm`. The algorithm enum,
#' the leaf `$ref` URLs, and the `allOf` table are all derived from the
#' classes, so the dispatcher cannot drift from the leaves it dispatches to.
#'
#' The leaf URLs are derived from `id`: for a dispatcher
#' `.../<family>/v1/schema.json`, algorithm `A` maps to
#' `.../<family>/<tolower(A)>/v1/schema.json` — matching [S7_to_JSONSchema]'s
#' `id` convention for the leaves.
#'
#' @param classes List of S7 classes: the family's per-variant subclasses
#'   (each carries a computed constant discriminator property).
#' @param id Character: Dispatcher `$id` URL
#'   (e.g. "https://schema.rtemis.org/decomposition/v1/schema.json").
#' @param discriminator Character: Name of the property that selects the
#'   variant (e.g. "algorithm", "type").
#' @param payload Character or NULL: Name of the variant-specific field
#'   (e.g. "config", "hyperparameters"). `NULL` selects top-level mode, where
#'   the variant's fields are siblings of the discriminator (see Details).
#' @param title Optional Character: Schema title.
#' @param description Character: Schema description. If empty, omitted.
#' @param discriminator_description Character: Description of the
#'   discriminator property.
#' @param extra_properties Named list: Additional top-level properties merged
#'   after the discriminator and the payload (e.g. decomposition's
#'   `features`, or the shared base fields in top-level mode).
#' @param instance_schema_url Character or NULL: If set, adds a `$schema`
#'   const property so instances can self-identify.
#'
#' @details
#' Two shapes, matching how the R classes serialize:
#'
#' * **Nested payload** (`payload` set): the variant's parameters live in one
#'   object (`config` / `hyperparameters`), so each `then` narrows that
#'   property to the leaf `$ref`. Leaves are closed
#'   (`additionalProperties: false`) and independently valid.
#' * **Top-level mode** (`payload = NULL`): the variant's fields are siblings
#'   of the discriminator (as in `ResamplerConfig`), so each `then` applies
#'   the leaf `$ref` to the whole object. `additionalProperties` is evaluated
#'   per-schema and would not see the leaf's properties, so strictness comes
#'   from draft 2020-12's `unevaluatedProperties: false`, which does account
#'   for properties evaluated by the applied `$ref`. Leaves for this mode
#'   must be generated open (`closed = FALSE` in [S7_to_JSONSchema]) so they
#'   compose.
#'
#' @return Named list: the dispatcher JSON Schema. Serialize with
#'   [write_JSONSchema].
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' schema <- S7_dispatcher_JSONSchema(
#'   classes = list(PCAConfig, ICAConfig),
#'   id = "https://schema.rtemis.org/decomposition/v1/schema.json",
#'   payload = "config"
#' )
#' }
S7_dispatcher_JSONSchema <- function(
  classes,
  id,
  discriminator = "algorithm",
  payload = "config",
  title = NULL,
  description = "",
  discriminator_description = "Algorithm name.",
  extra_properties = list(),
  instance_schema_url = NULL
) {
  check_character(id, allow_null = FALSE)
  check_character(discriminator, allow_null = FALSE)
  variants <- vapply(
    classes,
    function(cls) {
      if (!inherits(cls, "S7_class")) {
        rtemis.core::abort(
          "`classes` must be a list of S7 classes.",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      value <- prop(cls(), discriminator)
      if (!is.character(value) || length(value) != 1L) {
        rtemis.core::abort(
          "Discriminator `",
          discriminator,
          "` must be a constant string on each class, but ",
          cls@name,
          " gave a value of length ",
          length(value),
          ".",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      value
    },
    character(1L)
  )
  if (anyDuplicated(variants) > 0L) {
    rtemis.core::abort(
      "Duplicate `",
      discriminator,
      "` values across classes: ",
      paste(unique(variants[duplicated(variants)]), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Leaf URLs share the dispatcher's family base; variant -> lowercase slug.
  family_base <- sub("/v1/schema\\.json$", "", id)
  leaf_id <- function(variant) {
    paste0(family_base, "/", tolower(variant), "/v1/schema.json")
  }
  top_level <- is.null(payload)
  properties <- list()
  if (!is.null(instance_schema_url)) {
    properties[["$schema"]] <- list(
      type = "string",
      const = instance_schema_url,
      description = "JSON Schema URI for this config instance."
    )
  }
  properties[[discriminator]] <- list(
    type = "string",
    enum = I(variants),
    description = discriminator_description
  )
  if (!top_level) {
    properties[[payload]] <- list(
      type = "object",
      description = paste0(
        "Variant-specific parameters. Validated per `",
        discriminator,
        "` below."
      )
    )
  }
  properties <- c(properties, extra_properties)
  all_of <- lapply(variants, function(variant) {
    # `required` on the discriminator: a `properties`-only `if` is vacuously
    # true when the property is absent, which would apply every branch at once.
    condition <- list(
      properties = stats::setNames(
        list(list(const = variant)),
        discriminator
      ),
      required = I(discriminator)
    )
    consequence <- if (top_level) {
      list(`$ref` = leaf_id(variant))
    } else {
      list(
        properties = stats::setNames(
          list(list(`$ref` = leaf_id(variant))),
          payload
        )
      )
    }
    list(`if` = condition, then = consequence)
  })
  schema <- list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    `$id` = id,
    title = title,
    description = if (nzchar(description)) description else NULL,
    type = "object",
    required = I(c(discriminator, payload)),
    # Top-level mode composes the leaf into this object, so strictness must
    # account for properties evaluated by the applied `$ref`.
    additionalProperties = if (top_level) NULL else FALSE,
    unevaluatedProperties = if (top_level) FALSE else NULL,
    properties = properties,
    allOf = all_of
  )
  Filter(Negate(is.null), schema)
} # /rtemis::S7_dispatcher_JSONSchema


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
