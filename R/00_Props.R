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
# names, regardless of arity, and applies only to string properties. Other
# bounds are about *length*, so they apply to any type -- a character vector of
# per-case IDs is bound by "n_cases".
DATA_BOUNDS <- c("n_features", "n_cases", "n_classes", "feature_names")

# %% PROP_CONTAINERS ----
# How a property's values are wrapped. See `PropertySpec@container`.
# - "none"   a single value
# - "array"  a JSON array (per-feature weights, an initial embedding matrix
#            when combined with a nested `items`)
# - "map"    a string-keyed object (per-feature scaling centres, one-hot levels)
# - "matrix" a 2-D numeric matrix. Same JSON shape as an array whose `items`
#            are an array, but a distinct R class, so the two cannot share a
#            container value: `matrix` is an R `matrix`, a nested `array` is a
#            list of vectors (per-tree weights, per-tree in-bag counts).
PROP_CONTAINERS <- c("none", "array", "map", "matrix")

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
#' @field constant Logical: If TRUE, the value is determined by the class and
#'   cannot be set; `@default` holds it. Distinct from a *fixed* property,
#'   which is settable but not tunable.
#' @field data_dependent Logical: If TRUE, the value is tied to one dataset and
#'   is not written to a portable config.
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
    # A constant is determined by the class, not chosen by the user: it is not
    # settable, and `@default` holds the single permitted value. Distinct from
    # a *fixed* property, which the user does set but cannot tune.
    constant = new_property(class_logical, default = FALSE),
    data_bound = NULL | class_character,
    # Portability, orthogonal to `data_bound`: the value's shape is tied to one
    # particular dataset (per-case IDs, learned scaling centres, an initial
    # embedding), so it has no meaning in a portable config and is not
    # serialized. `data_bound` is about *validating* against the training data.
    data_dependent = new_property(class_logical, default = FALSE),
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
    if (self@constant) {
      if (is.null(self@default)) {
        return("@default must hold the value when @constant is TRUE.")
      }
      if (self@tunable) {
        return("@constant and @tunable are mutually exclusive.")
      }
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
        spec_r_kind(self),
        matrix = is.matrix(self@default),
        list = is.list(self@default),
        switch(
          self@type,
          boolean = is.logical(self@default),
          integer = is.integer(self@default),
          number = is.numeric(self@default),
          string = is.character(self@default),
          object = is.list(self@default)
        )
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


# %% spec_r_kind ----
#' The kind of R value a spec describes: "matrix", "list", or "atomic"
#'
#' A container of scalars is a plain (possibly named) R vector; only a container
#' whose elements are themselves containers needs a list. A matrix is its own
#' kind: the same JSON shape as a nested array, a different R class.
#'
#' @param spec `PropertySpec` object.
#'
#' @return Character: "matrix", "list", or "atomic".
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_r_kind <- function(spec) {
  if (spec@container == "matrix") {
    return("matrix")
  }
  nested <- spec@container != "none" &&
    !is.null(spec@items) &&
    spec@items@container != "none"
  if (nested) "list" else "atomic"
} # /rtemis::spec_r_kind


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
  if (spec@container == "matrix") {
    if (!is.matrix(value)) {
      return("must be a matrix.")
    }
    if (anyNA(value)) {
      return("must not contain missing values.")
    }
    return(NULL)
  }
  if (spec@container == "map" && is.null(names(value))) {
    return("must be named.")
  }
  if (!is.null(spec@items) && spec@items@container != "none") {
    # Elements are themselves containers, so each is validated against `items`.
    # A container of *scalars* is a plain (possibly named) R vector and falls
    # through to the generic checks below.
    if (spec@broadcast && !is.list(value)) {
      # A bare element stands in for the whole container.
      return(validate_with_spec(value, spec@items))
    }
    if (!is.list(value)) {
      return("must be a list.")
    }
    for (i in seq_along(value)) {
      msg <- validate_with_spec(value[[i]], spec@items)
      if (!is.null(msg)) {
        return(paste0("element ", i, " ", msg))
      }
    }
    return(NULL)
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
  atomic_class <- switch(
    spec@type,
    boolean = class_logical,
    integer = class_integer,
    number = class_numeric,
    string = class_character,
    object = class_list
  )
  # An R vector already holds "many of a scalar", so `array` and `map` over a
  # scalar leaf keep the atomic class (a map is simply a *named* vector). Only
  # a container whose elements are themselves containers needs a list.
  base_class <- switch(
    spec_r_kind(spec),
    matrix = S7::new_S3_class("matrix"),
    list = class_list,
    atomic_class
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
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
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
  broadcast = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
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
    broadcast = broadcast,
    data_bound = data_bound,
    data_dependent = data_dependent,
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
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
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
  broadcast = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
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
    broadcast = broadcast,
    data_bound = data_bound,
    data_dependent = data_dependent,
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
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param data_bound Character or NULL: Only "feature_names" is meaningful for
#'   a string property: values must be a subset of the training features.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
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
  broadcast = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
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
    broadcast = broadcast,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_string


# %% prop_map ----
#' String-keyed map S7 property with attached PropertySpec
#'
#' A named R vector (or named list, when `values` is itself a container) whose
#' keys are data-dependent — per-feature scaling values, per-feature one-hot
#' levels. Maps to a JSON object with `additionalProperties` describing the
#' value.
#'
#' @param values S7 property built by a `prop_*` factory: Describes one value of
#'   the map. Its own `default` is unused.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param data_bound Character or NULL: Training-data dimension the number of
#'   entries is tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_map <- function(
  values,
  nullable = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  description = ""
) {
  value_spec <- get_spec(values)
  if (is.null(value_spec)) {
    rtemis.core::abort(
      "`values` must be a property built by a prop_* factory.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  make_prop(PropertySpec(
    type = value_spec@type,
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "map",
    items = value_spec,
    broadcast = FALSE,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_map


# %% prop_array ----
#' Array-of-containers S7 property with attached PropertySpec
#'
#' An R list whose elements are themselves containers — one weight vector per
#' tree, one in-bag count vector per tree. For a flat vector of scalars use the
#' `vector = TRUE` argument of the scalar factories instead.
#'
#' @param items S7 property built by a `prop_*` factory: Describes one element.
#'   Its own `default` is unused.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param broadcast Logical: If TRUE, a bare element is accepted in place of the
#'   list, meaning "this element for every position".
#' @param data_bound Character or NULL: Training-data dimension the number of
#'   elements is tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_array <- function(
  items,
  nullable = FALSE,
  broadcast = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  description = ""
) {
  item_spec <- get_spec(items)
  if (is.null(item_spec)) {
    rtemis.core::abort(
      "`items` must be a property built by a prop_* factory.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  make_prop(PropertySpec(
    type = item_spec@type,
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "array",
    items = item_spec,
    broadcast = broadcast,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_array


# %% prop_matrix ----
#' Numeric matrix S7 property with attached PropertySpec
#'
#' A 2-D numeric matrix. Serializes as an array of arrays (rows), which is the
#' same JSON shape as a nested `array` container but a distinct R class, so the
#' two are separate containers.
#'
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param data_bound Character or NULL: Training-data dimension the row count is
#'   tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value is tied to one dataset
#'   and is not written to a portable config.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_matrix <- function(
  nullable = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "number",
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "matrix",
    items = NULL,
    broadcast = FALSE,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_matrix


# %% prop_const ----
#' Constant S7 property with attached PropertySpec
#'
#' A value determined by the class rather than chosen by the user — LightRF's
#' `boosting_type = "rf"`, LinearSVM's `kernel = "linear"`. It is what makes
#' the class that class, so it is declared the same way as the constant
#' `algorithm` discriminator: a computed property with no setter, hence
#' immutable by construction.
#'
#' Distinct from a **fixed** property, which the user *does* set but cannot
#' tune. Fixed is about tunability; constant is about settability.
#'
#' Emits `{"const": value}`, which is an assertion — a config supplying any
#' other value fails validation.
#'
#' @param value Scalar: The value. Its type sets the property's type.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_const <- function(value, description = "") {
  type <- if (is.logical(value)) {
    "boolean"
  } else if (is.integer(value)) {
    "integer"
  } else if (is.numeric(value)) {
    "number"
  } else if (is.character(value)) {
    "string"
  } else {
    rtemis.core::abort(
      "`value` must be a logical, integer, numeric, or character scalar.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  spec <- PropertySpec(
    type = type,
    default = value,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = FALSE,
    tunable = FALSE,
    container = "none",
    items = NULL,
    broadcast = FALSE,
    constant = TRUE,
    data_dependent = FALSE,
    description = description
  )
  force(value)
  p <- new_property(
    class = switch(
      type,
      boolean = class_logical,
      integer = class_integer,
      number = class_numeric,
      string = class_character
    ),
    getter = function(self) value
  )
  p[["spec"]] <- spec
  p
} # /rtemis::prop_const


# %% constant_spec_names ----
#' Names of an S7 class's constant properties
#'
#' @param x S7 class.
#'
#' @return Character vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
constant_spec_names <- function(x) {
  names(Filter(
    function(p) {
      s <- get_spec(p)
      !is.null(s) && s@constant
    },
    x@properties
  ))
} # /rtemis::constant_spec_names


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
# Every property of a config class plays one of two roles, and the role decides
# both what the generated JSON Schema says and what `write_config()` emits:
#
# - "config"   Built by a `prop_*` factory, so it carries a `PropertySpec`:
#              schema generated from the spec, serialized. The common case;
#              inferred, never declared.
# - "state"    Run state written during training or tuning (GLMNET's
#              `lambda.min`, LightGBM's `best_iter`). Declared with
#              `prop_state()`. Never in a schema, never serialized, re-derived
#              on read.
#
# A spec-less property with no role is drift: it is neither a declared input
# nor declared state, and schema generation aborts rather than quietly emitting
# an incomplete contract.
#
# `data_dependent` is a second, orthogonal axis on a "config" property: the
# value's shape is tied to a particular dataset (per-case IDs, learned scaling
# centres), so it appears in the schema but is not written to a portable
# config.

# %% prop_state ----
#' S7 property holding run state rather than configuration
#'
#' Written during training / tuning. Appears in the generated schema marked
#' `readOnly` — a reader needs the field to reconstruct the class, and a run
#' record carries it — but is never written to a portable config, where it
#' would be re-derived anyway.
#'
#' Wraps a property built by a `prop_*` factory, so run state is declared with
#' the same type, bounds and description as configuration.
#'
#' @param property S7 property built by a `prop_*` factory.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_state <- function(property) {
  if (is.null(get_spec(property))) {
    rtemis.core::abort(
      "`property` must be built by a prop_* factory.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  property[["role"]] <- "state"
  property
} # /rtemis::prop_state


# %% prop_role ----
#' Role of an S7 property
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Character: "config", "state", or `NA_character_` for a spec-less
#'   property with no declared role (i.e. drift).
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
#' @param role Character: "config" or "state".
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
  names(Filter(
    function(p) {
      spec <- get_spec(p)
      !is.null(spec) && spec@data_dependent
    },
    x@properties
  ))
} # /rtemis::data_dependent_prop_names


# %% prop_data_bound ----
#' Training-data dimension an S7 property is constrained by, or NULL
#'
#' Reads `data_bound` from the property's `PropertySpec`.
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
#' and run state. A *serialized config* is narrower: only what a user actually
#' chose, plus nested config objects, which serialize as their own schema.
#' Dropped here: **constants**, which the algorithm already implies;
#' **data-dependent** values, which mean nothing outside the dataset they were
#' measured from; and **state** written during training (GLMNET `lambda.min`,
#' LightGBM `best_iter`). All are reconstructed or re-derived on read.
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
      if (S7_inherits(values[[nm]])) {
        return(TRUE)
      }
      if (!identical(prop_role(props[[nm]]), "config")) {
        return(FALSE)
      }
      spec <- get_spec(props[[nm]])
      if (is.null(spec)) {
        return(TRUE)
      }
      # Not written to a portable config: a data-dependent value has no meaning
      # outside the dataset it was measured from, and a constant is already
      # implied by the algorithm.
      !spec@data_dependent && !spec@constant
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
  label = "config",
  noun = "parameter"
) {
  cls <- S7_class(self)
  # `x@config[["p"]] <- v` desugars to a whole-list round-trip, so constants
  # come back through here even when untouched: an identical value is a no-op,
  # a changed one is an error.
  constants <- constant_spec_names(cls)
  settable <- setdiff(own_prop_names(cls, base), constants)
  for (nm in names(value)) {
    if (nm %in% settable) {
      prop(self, nm) <- value[[nm]]
    } else if (nm %in% constants) {
      if (!identical(value[[nm]], prop(self, nm))) {
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
#' @param read_only Logical: If TRUE, the property is run state — marked
#'   `readOnly` and annotated `role: "state"`.
#'
#' @return Named list (JSON Schema property).
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_to_schema <- function(spec, read_only = FALSE) {
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
  } else if (spec@container == "matrix") {
    row <- list(type = "array", items = scalar, minItems = 1L)
    list(
      type = if (spec@nullable) I(c("array", "null")) else "array",
      items = row,
      minItems = 1L
    )
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
    # "feature_names" is a membership rule, not a length rule, so it has no
    # noun in the table.
    noun <- if (spec@data_bound == "feature_names") {
      NA_character_
    } else {
      DATA_BOUND_NOUN[[spec@data_bound]]
    }
    note <- if (spec@data_bound == "feature_names") {
      "Values must name training features."
    } else if (spec@container == "matrix") {
      paste0("Must have one row per ", noun, ".")
    } else if (spec@container == "map") {
      paste0("Must have one entry per ", noun, ".")
    } else if (spec@container != "none" && spec@broadcast) {
      # A scalar is explicitly allowed, so the length rule binds only the
      # vector form.
      paste0("A vector must have one value per ", noun, ".")
    } else if (spec@container != "none") {
      paste0("Must have one value per ", noun, ".")
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
  # The axes standard JSON Schema cannot express. Deliberately NOT the whole
  # spec: bounds, enum, and nullability are already recoverable from the
  # standard keywords, and duplicating them would create a second
  # representation that can disagree with the first. What is here is what a
  # reader cannot derive -- most importantly `tunable` vs `broadcast`, which
  # emit identical `oneOf` shapes. Absent keys take their default (FALSE, or
  # "none" for `container`).
  annotations <- Filter(
    Negate(is.null),
    list(
      type = spec@type,
      role = if (read_only) {
        "state"
      } else if (spec@constant) {
        "constant"
      } else {
        NULL
      },
      container = if (spec@container != "none") spec@container else NULL,
      tunable = if (spec@tunable) TRUE else NULL,
      broadcast = if (spec@broadcast) TRUE else NULL,
      data_bound = spec@data_bound,
      data_dependent = if (spec@data_dependent) TRUE else NULL
    )
  )
  if (spec@constant) {
    # An assertion, not an annotation: a config supplying any other value is
    # rejected by validation.
    out <- list(const = spec@default)
    if (nzchar(description)) {
      out[["description"]] <- description
    }
  }
  if (read_only) {
    out[["readOnly"]] <- TRUE
  }
  out[["x-rtemis"]] <- annotations
  if (spec@data_dependent) {
    # Machine-visible in the published contract: a consumer building a form
    # skips these rather than asking a user for a value only the data can give.
    out[["$comment"]] <- paste(
      "Data-dependent: measured from one dataset, so it has no portable value."
    )
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
  # Run state is part of the class, so it is part of the schema — marked
  # `readOnly`, since a user never supplies it. `config_prop_values()` is what
  # keeps it out of a portable config.
  roles <- vapply(props, prop_role, character(1L))
  state_names <- names(props)[!is.na(roles) & roles == "state"]
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
      "Properties with no declared role (build them with the prop_* factories, `refs` them, or declare them with prop_state()): ",
      paste(specless, collapse = ", "),
      ".",
      class = "rtemis_input_error"
    )
  }
  properties <- lapply(
    names(props),
    function(nm) spec_to_schema(get_spec(props[[nm]]), nm %in% state_names)
  )
  names(properties) <- names(props)
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
