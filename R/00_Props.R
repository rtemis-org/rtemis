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
# - container = "array"   -> "type": "array" (a genuinely vector-valued field,
#                            e.g. per-feature weights; NOT search values —
#                            mutually exclusive with tunable)
# - container = "map"     -> "type": "object" + "additionalProperties"
# - items                 -> the element schema, for nested shapes (a matrix is
#                            an array whose items are an array)
# - broadcast = TRUE      -> "oneOf": [element, array] (a scalar stands in for
#                            the whole container)
# - description           -> "description" (annotation). `default` is
#                            deliberately NOT emitted; see spec_to_schema.

# %% DATA_BOUNDS ----
# Training-data dimensions a hyperparameter's valid values can be tied to.
# A property declares one via `data_bound =`; `check_data_bounds()` resolves it
# against the training data and checks every declared property in one pass, so
# an out-of-range value is reported before any training work begins.
#
# The check that applies is determined by the property's `container`:
# - "none"   value must be <= the dimension (an upper bound, e.g. mtry)
# - anything else  length(value) must equal the dimension (e.g. per-feature
#                  costs, per-case offsets)
# "feature_names" is the exception: values must be a subset of the feature
# names, regardless of arity.
DATA_BOUNDS <- c("n_features", "n_cases", "n_classes", "feature_names")

# %% PROP_CONTAINERS ----
# How a property's values are wrapped. See `PropertySpec@container`.
# - "none"   a single value
# - "array"  a JSON array (per-feature weights, an initial embedding matrix
#            when combined with a nested `items`)
# - "map"    a string-keyed object (per-feature scaling centres, one-hot levels)
PROP_CONTAINERS <- c("none", "array", "map")

# %% PROP_TYPES ----
# JSON Schema base types a property's leaf value may take. "object" is an
# opaque pass-through: a named list handed to a foreign backend, with no
# per-key contract (see `prop_bag()`).
PROP_TYPES <- c("boolean", "integer", "number", "string", "object")

# Nouns used to build error messages from a bound name.
DATA_BOUND_NOUN <- c(
  n_features = "feature",
  n_cases = "case",
  n_classes = "class"
)
DATA_BOUND_NOUN_PLURAL <- c(
  n_features = "features",
  n_cases = "cases",
  n_classes = "classes"
)


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
#' @field container Character \{"none", "array", "map"\}: How values are
#'   wrapped. Anything but "none" is mutually exclusive with `tunable` (a
#'   container holds values; a tunable array holds search values).
#' @field items `PropertySpec` or NULL: Element spec, for nested shapes such as
#'   a matrix (array of arrays) or a map of arrays. NULL means the element is
#'   this spec's own leaf type and constraints.
#' @field broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   container, meaning "this value for every element".
#' @field data_bound Character or NULL: Name of the training-data dimension
#'   this value is constrained by \{"n_features", "n_cases", "n_classes",
#'   "feature_names"\}. Checked against the data by
#'   `validate_hyperparameters()`, not at construction time. See
#'   `check_data_bounds()`.
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
    minimum = NULL | class_numeric,
    maximum = NULL | class_numeric,
    exclusive_minimum = NULL | class_numeric,
    exclusive_maximum = NULL | class_numeric,
    enum = NULL | class_character,
    nullable = class_logical,
    tunable = class_logical,
    # Arity, as three orthogonal axes rather than one boolean:
    # - `container` says how values are wrapped: a scalar, a JSON array, or a
    #   string-keyed map.
    # - `items` describes the *element* when it is not simply this spec's own
    #   type and constraints, which is what makes nested shapes (a matrix = an
    #   array of arrays, a map of arrays) expressible. NULL means "the element
    #   is this spec's leaf type", the common case.
    # - `broadcast` allows a bare scalar in place of the container, for fields
    #   that mean "this value for every case/tree".
    # `tunable` is deliberately NOT part of this: its array is a *search space*,
    # not a value, which is why the two are mutually exclusive below and why a
    # reader must never infer one from the other's shape.
    container = class_character,
    # `class_any`, not `NULL | PropertySpec`: the class cannot reference itself
    # while its own `properties` list is being evaluated. The validator below
    # enforces the type instead, and runs once the class exists.
    items = class_any,
    broadcast = class_logical,
    data_bound = NULL | class_character,
    description = class_character
  ),
  validator = function(self) {
    if (!self@type %in% PROP_TYPES) {
      return(paste0(
        "@type must be one of ",
        paste0("'", PROP_TYPES, "'", collapse = ", "),
        "."
      ))
    }
    if (!is.null(self@enum) && self@type != "string") {
      return("@enum is only supported for type 'string'.")
    }
    if (!self@container %in% PROP_CONTAINERS) {
      return(paste0(
        "@container must be one of ",
        paste0("'", PROP_CONTAINERS, "'", collapse = ", "),
        "."
      ))
    }
    if (self@container != "none" && self@tunable) {
      return(
        "@container and @tunable are mutually exclusive (a container holds values, a tunable array holds search values)."
      )
    }
    if (!is.null(self@items) && !S7_inherits(self@items, PropertySpec)) {
      return("@items must be a PropertySpec or NULL.")
    }
    if (self@container == "none" && !is.null(self@items)) {
      return("@items is only meaningful when @container is not 'none'.")
    }
    if (self@container == "map" && is.null(self@items)) {
      return("@items must describe the value type when @container is 'map'.")
    }
    if (self@broadcast && self@container == "none") {
      return("@broadcast requires a @container to broadcast into.")
    }
    if (!is.null(self@data_bound)) {
      if (length(self@data_bound) != 1L) {
        return("@data_bound must be a single value.")
      }
      if (!self@data_bound %in% DATA_BOUNDS) {
        return(paste0(
          "@data_bound must be one of ",
          paste0("'", DATA_BOUNDS, "'", collapse = ", "),
          "."
        ))
      }
      if (self@data_bound == "feature_names" && self@type != "string") {
        return(
          "@data_bound 'feature_names' is only supported for type 'string'."
        )
      }
      if (self@data_bound != "feature_names" && self@type == "string") {
        return(
          "@data_bound on a string property must be 'feature_names'."
        )
      }
    }
    if (
      !is.null(self@minimum) &&
        !is.null(self@maximum) &&
        self@minimum > self@maximum
    ) {
      return("@minimum must not exceed @maximum.")
    }
    if (
      !is.null(self@exclusive_minimum) &&
        !is.null(self@exclusive_maximum) &&
        self@exclusive_minimum > self@exclusive_maximum
    ) {
      return("@exclusive_minimum must not exceed @exclusive_maximum.")
    }
    # The default must itself conform to the spec.
    # An invalid declaration fails on package load, not at first instantiation.
    if (!is.null(self@default)) {
      type_ok <- switch(
        self@type,
        boolean = is.logical(self@default),
        integer = is.integer(self@default),
        number = is.numeric(self@default),
        string = is.character(self@default),
        object = is.list(self@default)
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
  if (is.null(value)) {
    return(if (spec@nullable) NULL else "must not be NULL.")
  }
  if (spec@type == "object" && spec@container == "none") {
    # One named list is a single value however many keys it holds, and its
    # contents are the backend's contract, not ours.
    return(if (is.list(value)) NULL else "must be a list.")
  }
  if (length(value) == 0L) {
    # NULL is the only "unset" value: nullable properties declare their class
    # as `NULL | <base>` so that S7 prototypes them to NULL rather than to the
    # base class's empty vector. An empty vector reaching here is a real value
    # and is rejected, so that `!is.null()` guards downstream stay meaningful.
    # Only point at NULL when NULL is actually accepted.
    return(
      if (spec@nullable) {
        "must not be empty (use NULL to leave it unset)."
      } else {
        "must not be empty."
      }
    )
  }
  if (length(value) > 1L && !spec@tunable && spec@container == "none") {
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
    string = class_character,
    object = class_list
  )
  p <- new_property(
    class = if (spec@nullable) NULL | base_class else base_class,
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
    container = "none",
    broadcast = FALSE,
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
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
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
  data_bound = NULL,
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
    container = if (vector) "array" else "none",
    broadcast = FALSE,
    data_bound = data_bound,
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
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
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
  data_bound = NULL,
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
    container = if (vector) "array" else "none",
    broadcast = FALSE,
    data_bound = data_bound,
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
#' @param data_bound Character or NULL: Only "feature_names" is meaningful for
#'   a string property: values must be a subset of the training features.
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
  data_bound = NULL,
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
    container = if (vector) "array" else "none",
    broadcast = FALSE,
    data_bound = data_bound,
    description = description
  ))
} # /rtemis::prop_string


# %% prop_bag ----
#' Open-object S7 property with attached PropertySpec
#'
#' A named list handed straight to a foreign backend (`missRanger`'s
#' parameters, a clustering backend's control list). The keys are that
#' backend's contract, so the schema models it as an object with no per-key
#' constraint and validation stops at "is a list".
#'
#' @param default List: Default value (NULL only if `nullable`).
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_bag <- function(
  default = list(),
  nullable = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "object",
    default = default,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "none",
    items = NULL,
    broadcast = FALSE,
    description = description
  ))
} # /rtemis::prop_bag


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


# %% Property roles ----
# Every property of a config class plays one of three roles, and the role
# decides both what the generated JSON Schema says and what `write_config()`
# emits. Declaring it on the property keeps that decision in one place, next to
# the value it describes:
#
# - "config"   Built by a `prop_*` factory, so it carries a `PropertySpec`:
#              schema generated from the spec, serialized. The common case;
#              inferred, never declared.
# - "external" A config input whose R type the factories cannot express
#              (a matrix, a function-or-string, a list of per-tree vectors).
#              Declared with `prop_external()`. Present in the schema, but its
#              fragment is hand-written and merged in via `S7_to_JSONSchema`'s
#              `extra`, which asserts one is supplied.
# - "state"    Run state written during training or tuning (GLMNET's
#              `lambda.min`, LightGBM's `best_iter`). Declared with
#              `prop_state()`. Never in a schema, never serialized, re-derived
#              on read.
#
# A spec-less property with no role is drift: it is neither a declared input
# nor declared state, and schema generation aborts rather than quietly emitting
# an incomplete contract.
#
# `data_dependent` is a second, orthogonal axis, meaningful on "external": the
# value's shape is tied to a particular dataset (per-case IDs, learned scaling
# centers), so it has no portable form. Every `config` property is
# JSON-expressible and portable by construction.

# %% prop_state ----
#' S7 property holding run state rather than configuration
#'
#' Written during training / tuning, so it is excluded from generated schemas
#' and from serialized configs, and re-derived on read. Contrast
#' `prop_external()`, which marks a genuine config input the `prop_*` factories
#' cannot type.
#'
#' @param class S7 class or union: Property class.
#' @param default Default value.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_state <- function(class, default = NULL) {
  p <- new_property(class, default = default)
  p[["role"]] <- "state"
  p
} # /rtemis::prop_state


# %% prop_external ----
#' S7 property whose JSON Schema is supplied by hand
#'
#' A config input whose R type the `prop_*` factories cannot express. It is part
#' of the contract — `S7_to_JSONSchema()` requires its schema fragment to arrive
#' via `extra` — but is generated from that fragment rather than from a
#' `PropertySpec`.
#'
#' @param class S7 class or union: Property class.
#' @param default Default value.
#' @param data_dependent Logical: If TRUE, the value's shape is tied to a
#'   specific dataset (per-case IDs, learned scaling centers), so it has no
#'   portable form and is never serialized. Distinct from `data_bound`, which
#'   is about *validation* against the training data rather than portability.
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value; see `DATA_BOUNDS`. Only meaningful for atomic values -- a
#'   `prop_external()` holding a list needs its own
#'   `validate_hyperparameters()` method.
#' @param validator Function or NULL: Property validator, as for
#'   `S7::new_property()`.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_external <- function(
  class,
  default = NULL,
  data_dependent = FALSE,
  data_bound = NULL,
  validator = NULL
) {
  if (!is.null(data_bound) && !data_bound %in% DATA_BOUNDS) {
    rtemis.core::abort(
      "`data_bound` must be one of ",
      paste0("'", DATA_BOUNDS, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  p <- new_property(class, default = default, validator = validator)
  p[["role"]] <- "external"
  p[["data_dependent"]] <- data_dependent
  p[["data_bound"]] <- data_bound
  p
} # /rtemis::prop_external


# %% prop_role ----
#' Role of an S7 property
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Character: "config", "external", "state", or `NA_character_` for a
#'   spec-less property with no declared role (i.e. drift).
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_role <- function(prop) {
  role <- prop[["role"]]
  if (!is.null(role)) {
    return(role)
  }
  if (is.null(get_spec(prop))) NA_character_ else "config"
} # /rtemis::prop_role


# %% role_prop_names ----
#' Names of an S7 class's properties with a given role
#'
#' @param x S7 class.
#' @param role Character: "config", "external", or "state".
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
role_prop_names <- function(x, role) {
  names(Filter(function(p) identical(prop_role(p), role), x@properties))
} # /rtemis::role_prop_names


# %% data_dependent_prop_names ----
#' Names of an S7 class's properties with no portable form
#'
#' @param x S7 class.
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
data_dependent_prop_names <- function(x) {
  names(Filter(function(p) isTRUE(p[["data_dependent"]]), x@properties))
} # /rtemis::data_dependent_prop_names


# %% prop_data_bound ----
#' Training-data dimension an S7 property is constrained by, or NULL
#'
#' Reads `data_bound` from the property's `PropertySpec` (factory-built
#' properties) or from the property itself (`prop_external()`).
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_data_bound <- function(prop) {
  spec <- get_spec(prop)
  if (!is.null(spec)) {
    return(spec@data_bound)
  }
  prop[["data_bound"]]
} # /rtemis::prop_data_bound


# %% data_bound_props ----
#' Properties of an S7 class that declare a `data_bound`, as a named character
#'
#' @param x S7 class.
#'
#' @return Named character vector: names are property names, values the bound.
#'
#' @author EDG
#' @keywords internal
#' @noRd
data_bound_props <- function(x) {
  bounds <- lapply(x@properties, prop_data_bound)
  bounds <- Filter(Negate(is.null), bounds)
  if (length(bounds) == 0L) {
    return(character())
  }
  vapply(bounds, identity, character(1L))
} # /rtemis::data_bound_props


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
#' list. Unset properties read as NULL (see `validate_with_spec`).
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
  out <- lapply(nms, function(nm) prop(self, nm))
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
#' Unset properties read as NULL: nullable properties declare their class as
#' `NULL | <base>`, so S7 prototypes them to NULL rather than to the base
#' class's empty value.
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
  out <- lapply(nms, function(nm) prop(self, nm))
  names(out) <- nms
  out
} # /rtemis::own_prop_values


# %% config_prop_values ----
#' A subclass's own property values that belong in a serialized config
#'
#' The runtime parameter list (`@hyperparameters` / `@config`) intentionally
#' carries everything a training backend needs, including algorithm constants
#' and run state. A *serialized config* is narrower: only `"config"`-role
#' properties (see `prop_role()`) plus nested config objects, which serialize as
#' their own schema. Dropped here: unsettable constants (`hp_constants()`, which
#' are not properties at all), `"state"` properties written during training
#' (GLMNET `lambda.min`, LightGBM `best_iter`), and `"external"` properties,
#' whose R types have no JSON form the factories can emit (tSNE `Y_init`, TabNet
#' `optimizer`). All are reconstructed or re-derived on read.
#'
#' Note the asymmetry on `"external"`: those properties *are* in the generated
#' schema and `read_config()` accepts them, but nothing writes them. Closing
#' that gap means also keeping the ones that are not `data_dependent` — a policy
#' change deliberately left for its own discussion.
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
  props <- S7_class(self)@properties
  keep <- vapply(
    names(values),
    function(nm) {
      identical(prop_role(props[[nm]]), "config") || S7_inherits(values[[nm]])
    },
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
  # The element schema: a nested `items` spec when the shape is nested (a
  # matrix, a list of per-tree vectors), otherwise this spec's own leaf.
  element <- if (is.null(spec@items)) scalar else spec_to_schema(spec@items)
  out <- if (spec@container == "array") {
    # A genuinely vector-valued field (e.g. per-feature weights).
    arr <- list(
      type = if (spec@nullable) I(c("array", "null")) else "array",
      items = element,
      minItems = 1L
    )
    if (spec@broadcast) {
      # A bare scalar stands in for the whole container ("this value for every
      # case"). Distinct from `tunable`'s identically-shaped oneOf, which is a
      # search space -- see the note on PropertySpec@container.
      arr[["type"]] <- "array"
      branches <- list(element, arr)
      if (spec@nullable) {
        branches <- c(list(list(type = "null")), branches)
      }
      list(oneOf = branches)
    } else {
      arr
    }
  } else if (spec@container == "map") {
    # A string-keyed object of homogeneous values (per-feature centres).
    list(
      type = if (spec@nullable) I(c("object", "null")) else "object",
      additionalProperties = element
    )
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
  # No `default` keyword is emitted; defaults are published separately by
  # `data-raw/generate_defaults.R`, keyed by schema `$id`.
  # A `data_bound` cannot be expressed structurally - JSON Schema has no view of
  # the training data - so record it in the description, where a consumer
  # building a form or a config can still surface the constraint.
  description <- spec@description
  if (!is.null(spec@data_bound)) {
    note <- if (spec@data_bound == "feature_names") {
      "Values must name training features."
    } else if (spec@container != "none") {
      paste0(
        "Must have one value per ",
        DATA_BOUND_NOUN[[spec@data_bound]],
        "."
      )
    } else {
      paste0(
        "Cannot exceed the number of ",
        DATA_BOUND_NOUN_PLURAL[[spec@data_bound]],
        " in the training data."
      )
    }
    description <- if (nzchar(description)) {
      paste(description, note)
    } else {
      note
    }
  }
  if (nzchar(description)) {
    out[["description"]] <- description
  }
  out
} # /rtemis::spec_to_schema


# %% S7_to_JSONSchema ----
#' Convert an S7 class built with `prop_*` factories to a JSON Schema
#'
#' Walks the class's properties, reads each attached `PropertySpec`, and
#' assembles a draft 2020-12 JSON Schema. Which properties take part is decided
#' by their declared role (see `prop_role()`), not by a list kept here:
#' `"config"` properties are generated from their spec, `"state"` properties are
#' dropped, and `"external"` properties must be supplied by `extra` — asserted
#' after the merge, so a forgotten fragment cannot silently drop a key from the
#' published contract. A spec-less property with no role is an error, so a class
#' that drifts from the factory vocabulary fails loudly instead of emitting a
#' wrong schema.
#'
#' @param x S7 class (e.g. `LightRFHyperparameters`).
#' @param id Character: Schema `$id` URL
#'   (e.g. "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json").
#' @param title Character: Schema title. Defaults to the class name.
#' @param description Character: Schema description. If empty, the
#'   "description" keyword is omitted from the schema.
#' @param base S7 class or NULL: The family base class, whose inherited
#'   properties are machinery (`tuned`, `resampled`, the computed payload list)
#'   rather than config, and are omitted. NULL for a flat config that has no
#'   family base.
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
#'   base = Hyperparameters
#' )
#' }
S7_to_JSONSchema <- function(
  x,
  id,
  title = NULL,
  description = "",
  base = NULL,
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
  if (!is.null(base)) {
    props <- props[own_prop_names(x, base)]
  }
  # Run state never reaches a schema; `external` properties are described by
  # `extra` instead of by a spec, and their arrival is checked below.
  roles <- vapply(props, prop_role, character(1L))
  external <- names(props)[!is.na(roles) & roles == "external"]
  props <- props[is.na(roles) | !roles %in% c("state", "external")]
  if (!is.null(refs)) {
    unknown <- setdiff(names(refs), names(props))
    if (length(unknown) > 0L) {
      rtemis.core::abort(
        "`refs` names no such (or omitted) propert",
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
      "Properties with no declared role (build them with the prop_* factories, `refs` them, or declare them with prop_external() / prop_state()): ",
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
  # An `external` property is part of the contract; only its JSON fragment is
  # hand-written. Losing one is silent otherwise — the schema still validates,
  # it just stops admitting a key the class accepts.
  unsupplied <- setdiff(external, names(schema[["properties"]]))
  if (length(unsupplied) > 0L) {
    rtemis.core::abort(
      "Propert",
      if (length(unsupplied) == 1L) "y " else "ies ",
      "declared with prop_external() but not supplied by `extra`: ",
      paste(unsupplied, collapse = ", "),
      ".",
      class = "rtemis_input_error"
    )
  }
  schema
} # /rtemis::S7_to_JSONSchema


# %% discriminator_value ----
#' Read a class's constant discriminator value without constructing it
#'
#' Each dispatched subclass overrides the discriminator (`type` / `algorithm`)
#' with a computed constant property (`prop_algorithm`), whose getter ignores
#' `self`. Reading it via the getter avoids default-constructing the class,
#' which may deliberately be invalid with defaults (e.g. `ResamplerConfig`
#' requires `n` for every type except LOOCV). Falls back to instantiation for a
#' plain (non-computed) discriminator property.
#'
#' @param cls S7 class.
#' @param discriminator Character: Name of the discriminator property.
#'
#' @return Character scalar: the discriminator value.
#'
#' @author EDG
#' @keywords internal
#' @noRd
discriminator_value <- function(cls, discriminator) {
  getter <- cls@properties[[discriminator]][["getter"]]
  if (is.null(getter)) prop(cls(), discriminator) else getter(NULL)
} # /rtemis::discriminator_value


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
#' @param variant_required Named list keyed by discriminator value: for each
#'   variant, a character vector of top-level property names to mark
#'   `required` in that variant's `if/then` branch. Used to mirror
#'   type-dependent R validators, e.g. `ResamplerConfig` requires `n` for every
#'   type except LOOCV. Variants absent from the list add no extra requirement.
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
  variant_required = list(),
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
      value <- discriminator_value(cls, discriminator)
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
    # Type-dependent required properties (e.g. `n` for every resampler type
    # except LOOCV), mirroring the R class validator.
    req <- variant_required[[variant]]
    if (!is.null(req)) {
      consequence[["required"]] <- I(req)
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
