# 010_Props.R
# ::rtemis::
# 2026- EDG rtemis.org

# S7 property factories: each factory returns an S7 property whose type,
# default, and validation logic are generated from a `PropertySpec` -- a nested
# S7 object that rides along on the property. Because the constraints are data
# (not closures), `S7_to_JSONSchema()` can convert any class built from these
# factories to a JSON Schema mechanically: one declaration produces the R
# validator, the default, the schema, and (downstream) the TUI form.
#
# JSON Schema mapping (see spec_to_schema):
# - type/bounds/enum      -> "type", "minimum"/"maximum"/"exclusiveMinimum"/
#                            "exclusiveMaximum", "enum"
# - nullable = TRUE       -> "null" added to "type"
# - tunable = TRUE        -> "oneOf": [value, array-of-value (search values)].
#                            A search space sits one level above the property's
#                            own type, so a scalar's is an array and an
#                            "array" container's is an array of arrays.
# - container = "array"   -> "type": "array" (a genuinely vector-valued field,
#                            e.g. per-feature weights). With tunable it takes
#                            the oneOf form above; mutually exclusive with
#                            broadcast, which would collide at the array level.
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
# The name bounds are the exception: values must be a subset of the named
# columns, regardless of arity, and they apply only to string properties. Other
# bounds are about *length*, so they apply to any type -- a character vector of
# per-case IDs is bound by "n_cases".
# - "feature_names"          any feature column
# - "numeric_feature_names"  the numeric feature columns only, for a selection
#                            that is then handed to a numeric-only backend
#                            (decomposition)
DATA_BOUNDS <- c(
  "n_features",
  "n_cases",
  "n_classes",
  "feature_names",
  "numeric_feature_names"
)

# The subset of `DATA_BOUNDS` checked by membership rather than by length.
NAME_BOUNDS <- c("feature_names", "numeric_feature_names")

# %% PROP_CONTAINERS ----
# How a property's values are wrapped. See `PropertySpec@container`.
# - "none"   a single value
# - "array"  a JSON array (per-feature weights, an initial embedding matrix
#            when combined with a nested `items`)
# - "map"    a string-keyed object (per-feature scaling centers, one-hot levels)
# - "matrix" a 2-D numeric matrix. Same JSON shape as an array whose `items`
#            are an array, but a distinct R class, so the two cannot share a
#            container value: `matrix` is an R `matrix`, a nested `array` is a
#            list of vectors (per-tree weights, per-tree in-bag counts).
# - "table"  a data.frame: heterogeneous named columns, each with its own type
#            and bounds. Emitted row-oriented (an array of objects), the
#            encoding pandas, polars and DataFrames.jl all read natively.
#            `matrix` is the homogeneous, unlabeled counterpart.
# - "struct" a named list with *declared*, heterogeneous members, each with its
#            own type -- a JSON object with `properties`. The counterpart of
#            `map`, which is an object with `additionalProperties`: a map's keys
#            are data (one per feature) and its values homogeneous, a struct's
#            keys are part of the contract. A struct member may itself be a
#            container; a table's may not, since a cell is a scalar.
# - "factor" an R factor: a classification outcome or prediction. Emitted as
#            `{levels, codes}` -- the levels in order, and a 1-based index into
#            them per case -- which is what every categorical type stores (an R
#            factor, an Arrow dictionary, a pandas Categorical). An array of
#            labels would lose both the order, which decides which class is
#            positive, and any level with no cases.
PROP_CONTAINERS <- c(
  "none",
  "array",
  "map",
  "matrix",
  "table",
  "struct",
  "factor"
)

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
#' property) fails at factory time -- i.e. at package load -- rather than at
#' first instantiation.
#'
#' @field type Character: JSON Schema base type
#'   \{"boolean", "integer", "number", "string"\}.
#' @field default Default value (scalar of `type`, or NULL).
#' @field minimum,maximum Numeric or NULL: Inclusive bounds.
#' @field exclusive_minimum,exclusive_maximum Numeric or NULL: Exclusive bounds.
#' @field enum Character or NULL: Allowed values (string type only).
#' @field nullable Logical: If TRUE, NULL is a valid value.
#' @field tunable Logical: If TRUE, a search space is accepted alongside a
#'   value. A search space sits one level of nesting above the property's own
#'   type: a vector of search values for a scalar, a list of vectors for an
#'   `array` container. Only "none" and "array" containers may be tunable, and
#'   an `array` one must have scalar elements.
#' @field container Character \{"none", "array", "map", "matrix", "table"\}: How
#'   values are wrapped.
#' @field items `PropertySpec` or NULL: Element spec, for nested shapes such as
#'   a matrix (array of arrays) or a map of arrays. NULL means the element is
#'   this spec's own leaf type and constraints.
#' @field members Named list of `PropertySpec` or NULL: The declared members of
#'   an object shape -- one spec per *column* for a `table` (each describing a
#'   cell, the column being a vector of them), one per *field* for a `struct`.
#' @field required_members Character or NULL: Names of the members that are
#'   always present. Any other declared member is optional, so its absence
#'   means "not computed for this task" rather than "invalid". NULL means all
#'   of them are required.
#' @field broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   container, meaning "this value for every element". Mutually exclusive with
#'   `tunable`: a broadcast element and a one-element search space are the same
#'   shape.
#' @field min_items Integer [1, Inf): Fewest elements an `array` container may
#'   hold.
#' @field unique_items Logical: If TRUE, an `array` container's elements must
#'   be distinct.
#' @field tune_on_null Logical: If TRUE, a NULL value means "determine by
#'   tuning" rather than "unset". Requires `nullable`.
#' @field default_on_null Logical: If TRUE, a NULL value means "apply the
#'   default for this task type" (LightGBM's `objective`). Requires `nullable`;
#'   mutually exclusive with `tune_on_null`.
#' @field constant Logical: If TRUE, the value is determined by the class and
#'   cannot be set; `@default` holds it. Distinct from a *fixed* property,
#'   which is settable but not tunable.
#' @field data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset, so it cannot be supplied without the data in hand. Which
#'   dimension it follows is `data_bound` where one is declared, and the
#'   container's own where not; `data_dependent_comment()` states it. An
#'   annotation only: such a value is still a settable input and is written to
#'   a config.
#' @field data_bound Character or NULL: Name of the training-data dimension
#'   this value is constrained by \{"n_features", "n_cases", "n_classes",
#'   "feature_names"\}. Checked against the data by
#'   `validate_hyperparameters()`, not at construction time. See
#'   `check_data_bounds()`.
#' @field applies_when Named list or NULL: Sibling properties this one is only
#'   in effect for, mapped to the values that put it in effect. Entries are
#'   conjunctive: every named sibling must hold one of its listed values.
#'   Requires `nullable`, since NULL is how "does not apply" is expressed. See
#'   `check_applies_when()`.
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
    # The members of a declared object shape, which one recursive `items` spec
    # cannot describe because they are heterogeneous: each carries its own type
    # and bounds. Shared by the two containers built on that shape -- a `table`
    # is an array of it, a `struct` is one of it -- so the emitted row object
    # and the emitted struct object come from the same declaration.
    # `class_any` for the same reason as `items` -- the class cannot reference
    # itself while its own `properties` list is being evaluated.
    members = class_any,
    # Which members are always present. Declaring every possible one and
    # requiring only these is what lets a conditional column (an AUC that
    # exists only for binary classification) be absent without being invalid,
    # without needing class-level if/then vocabulary.
    required_members = NULL | class_character,
    broadcast = class_logical,
    # How many elements an array container holds, and whether they repeat.
    # Only "array" carries these: a matrix's rows and a map's keys have no
    # equivalent JSON Schema keyword pair, so widening them would publish a
    # constraint nothing enforces.
    min_items = new_property(class_integer, default = 1L),
    unique_items = new_property(class_logical, default = FALSE),
    # A constant is determined by the class, not chosen by the user: it is not
    # settable, and `@default` holds the single permitted value. Distinct from
    # a *fixed* property, which the user does set but cannot tune.
    constant = new_property(class_logical, default = FALSE),
    # NULL means "determine this by tuning" rather than "leave unset".
    # `nullable + tunable` does not imply it: GLMNET's `lambda` is found by
    # cv.glmnet and LightGBM's `nrounds` by early stopping, but a nullable
    # tunable like `mtry` simply falls back to the backend default.
    tune_on_null = new_property(class_logical, default = FALSE),
    # NULL means "apply the default for this task type" -- LightGBM's
    # `objective` is "multiclass" or "regression" depending on the outcome.
    # The sibling of `tune_on_null`: both say what NULL *means*, and the three
    # possibilities are "leave unset", "determine by tuning", and this.
    #
    # The line against `derived`: a task-type default restates what was asked
    # for, while a derived value is measured from the dataset -- its dimensions
    # or its values alike. `feature_fraction`'s sqrt(n_features) is derived; it
    # depends on the data, not on the question.
    default_on_null = new_property(class_logical, default = FALSE),
    data_bound = NULL | class_character,
    # Shape, orthogonal to `data_bound`: the value has one entry per case
    # (`id_strat`, an initial embedding) or per feature (scaling centers,
    # one-hot levels), so it cannot be filled in before the data is seen. Purely
    # an annotation for form builders -- every such property is a settable input
    # and is serialized. `data_bound` is about *validating* against the data.
    data_dependent = new_property(class_logical, default = FALSE),
    # Applicability, orthogonal to validity: the value is well-formed but inert
    # unless a sibling holds one of the listed values. The shape is "sibling is
    # one of a set", conjoined across entries; three consumers evaluate it --
    # the class validator, the tuning grid, and the form builders vendored into
    # the CLI and live.
    applies_when = NULL | class_list,
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
    if (self@tunable && !self@container %in% c("none", "array")) {
      return(paste0(
        "@tunable is only meaningful for a scalar or an 'array' container, not '",
        self@container,
        "'."
      ))
    }
    # A search space sits one level above the property's own type, so a
    # container's is an array of arrays. `broadcast` breaks that: it makes a
    # bare element a value too, so the array level means both "one explicit
    # value" and "a search over broadcast elements" with nothing to separate
    # them. The two markers are therefore exclusive, not merely awkward.
    if (self@broadcast && self@tunable) {
      return(
        "@broadcast and @tunable are mutually exclusive: a broadcast element and a one-element search space are the same shape."
      )
    }
    if (
      self@tunable &&
        self@container == "array" &&
        !is.null(self@items) &&
        self@items@container != "none"
    ) {
      return(
        "@tunable on an 'array' container requires scalar elements: a search space over a container of containers would need a third level of nesting."
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
    if (self@container %in% c("table", "struct")) {
      if (!is.list(self@members) || length(self@members) == 0L) {
        return(paste0(
          "@members must be a non-empty named list when @container is '",
          self@container,
          "'."
        ))
      }
      if (is.null(names(self@members)) || any(!nzchar(names(self@members)))) {
        return("@members must be fully named.")
      }
      if (anyDuplicated(names(self@members)) > 0L) {
        return("@members must have unique names.")
      }
      for (nm in names(self@members)) {
        member <- self@members[[nm]]
        if (!S7_inherits(member, PropertySpec)) {
          return(paste0("@members[['", nm, "']] must be a PropertySpec."))
        }
        if (self@container == "table" && member@container != "none") {
          # A column is a vector of cells; a cell that is itself a container
          # has no row-oriented encoding a data frame reader would recognize.
          # A struct's members carry no such restriction.
          return(paste0(
            "@members[['",
            nm,
            "']] must describe a scalar cell (@container 'none')."
          ))
        }
      }
      unknown <- setdiff(self@required_members, names(self@members))
      if (length(unknown) > 0L) {
        return(paste0(
          "@required_members names undeclared members: ",
          paste0("'", unknown, "'", collapse = ", "),
          "."
        ))
      }
      if (!is.null(self@items)) {
        return("@items and @members are mutually exclusive.")
      }
    } else {
      if (!is.null(self@members)) {
        return(
          "@members is only meaningful when @container is 'table' or 'struct'."
        )
      }
      if (!is.null(self@required_members)) {
        return(paste0(
          "@required_members is only meaningful when @container is 'table' ",
          "or 'struct'."
        ))
      }
    }
    if (self@broadcast && self@container == "none") {
      return("@broadcast requires a @container to broadcast into.")
    }
    if (length(self@min_items) != 1L || self@min_items < 1L) {
      return("@min_items must be a single value >= 1.")
    }
    if (self@container != "array") {
      if (self@min_items != 1L) {
        return("@min_items is only meaningful when @container is 'array'.")
      }
      if (self@unique_items) {
        return("@unique_items is only meaningful when @container is 'array'.")
      }
    }
    if (self@broadcast && self@min_items > 1L) {
      return(
        "@broadcast and @min_items > 1 are contradictory: a broadcast scalar stands in for the whole array."
      )
    }
    if (self@tune_on_null && !self@nullable) {
      return("@tune_on_null requires @nullable: NULL is the signal.")
    }
    if (self@default_on_null && !self@nullable) {
      return("@default_on_null requires @nullable: NULL is the signal.")
    }
    if (self@default_on_null && self@tune_on_null) {
      return(
        "@default_on_null and @tune_on_null are mutually exclusive: NULL means one thing."
      )
    }
    if (self@constant) {
      if (is.null(self@default)) {
        return("@default must hold the value when @constant is TRUE.")
      }
      if (self@tunable) {
        return("@constant and @tunable are mutually exclusive.")
      }
    }
    if (!is.null(self@applies_when)) {
      if (!self@nullable) {
        return(
          "@applies_when requires @nullable: NULL is how a property that does not apply is expressed."
        )
      }
      if (self@constant) {
        return(
          "@applies_when and @constant are mutually exclusive: a constant is not settable, so gating it has no effect."
        )
      }
      if (length(self@applies_when) == 0L) {
        return("@applies_when must name at least one sibling property.")
      }
      nms <- names(self@applies_when)
      if (is.null(nms) || any(!nzchar(nms))) {
        return("@applies_when must be fully named.")
      }
      if (anyDuplicated(nms) > 0L) {
        return("@applies_when must have unique names.")
      }
      for (nm in nms) {
        allowed <- self@applies_when[[nm]]
        # NA marks a gated-off cell in the tuning grid, so it is reserved.
        if (!is.atomic(allowed) || length(allowed) == 0L || anyNA(allowed)) {
          return(paste0(
            "@applies_when[['",
            nm,
            "']] must be a non-empty atomic vector of allowed values, without NA."
          ))
        }
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
      if (self@data_bound %in% NAME_BOUNDS && self@type != "string") {
        return(paste0(
          "@data_bound '",
          self@data_bound,
          "' is only supported for type 'string'."
        ))
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
    fields <- spec_fields(self)
    if (!is.null(self@default)) {
      type_ok <- switch(
        spec_r_kind(fields),
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
    default_msg <- validate_with_spec(self@default, fields)
    if (!is.null(default_msg)) {
      return(paste0("@default ", default_msg))
    }
    NULL
  }
) # /rtemis::PropertySpec


# %% spec_fields ----
#' A PropertySpec's fields as a plain nested list
#'
#' The form a spec is *stored* in, on the property and in the property's
#' validator closure. An S7 object carries its class definition by value in its
#' `S7_class` attribute, so storing the `PropertySpec` itself would write a copy
#' of the whole class object -- tens of KB -- into the package's lazy-load
#' database once per property, and S7 composes class objects by value, which
#' multiplies that cost through every class that declares or inherits such a
#' property. The fields alone are three orders of magnitude smaller.
#'
#' Nested `items` and `members` specs are converted too: the validators reach
#' them recursively and must find the same representation at every level.
#'
#' @param spec `PropertySpec` object.
#'
#' @return Named list of spec fields, with any nested specs likewise converted.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_fields <- function(spec) {
  fields <- props(spec)
  if (!is.null(fields[["items"]])) {
    fields[["items"]] <- spec_fields(fields[["items"]])
  }
  if (!is.null(fields[["members"]])) {
    fields[["members"]] <- lapply(fields[["members"]], spec_fields)
  }
  fields
} # /rtemis::spec_fields


# %% spec_object ----
#' Rebuild a PropertySpec from its stored fields
#'
#' Inverse of `spec_fields()`. The schema emitters and prop audits read spec
#' fields with `@`, so they are handed an object; only the validators, which run
#' on every property set, read the stored list directly.
#'
#' @param fields Named list produced by `spec_fields()`.
#'
#' @return `PropertySpec` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_object <- function(fields) {
  if (!is.null(fields[["items"]])) {
    fields[["items"]] <- spec_object(fields[["items"]])
  }
  if (!is.null(fields[["members"]])) {
    fields[["members"]] <- lapply(fields[["members"]], spec_object)
  }
  do.call(PropertySpec, fields)
} # /rtemis::spec_object


# %% spec_r_kind ----
#' The kind of R value a spec describes: "matrix", "list", or "atomic"
#'
#' A container of scalars is a plain (possibly named) R vector; only a container
#' whose elements are themselves containers needs a list. A matrix is its own
#' kind: the same JSON shape as a nested array, a different R class. So is a
#' table, whose columns each carry their own type.
#'
#' @param fields Named list of spec fields, from `spec_fields()`.
#'
#' @return Character: "matrix", "table", "factor", "list", or "atomic".
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_r_kind <- function(fields) {
  container <- fields[["container"]]
  if (container == "matrix") {
    return("matrix")
  }
  if (container == "table") {
    return("table")
  }
  if (container == "factor") {
    return("factor")
  }
  if (container == "struct") {
    return("list")
  }
  nested <- container != "none" &&
    !is.null(fields[["items"]]) &&
    fields[["items"]][["container"]] != "none"
  if (nested) "list" else "atomic"
} # /rtemis::spec_r_kind


# %% validate_array_arity ----
#' Check an array container's length and element uniqueness
#'
#' The R half of the `minItems` / `uniqueItems` keywords `spec_to_schema()`
#' publishes. A no-op for any other container, and for the default arity, so it
#' never competes with the emptiness message raised upstream.
#'
#' @param value Property value being set, in container form.
#' @param fields Named list of spec fields, from `spec_fields()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_array_arity <- function(value, fields) {
  if (fields[["container"]] != "array") {
    return(NULL)
  }
  min_items <- fields[["min_items"]]
  if (min_items > 1L && length(value) < min_items) {
    return(paste0(
      "must have at least ",
      min_items,
      " elements, but ",
      length(value),
      " given."
    ))
  }
  if (fields[["unique_items"]] && anyDuplicated(value) > 0L) {
    return("must not contain duplicate values.")
  }
  NULL
} # /rtemis::validate_array_arity


# %% validate_table_column ----
#' Check one column of a `table` container against its cell spec
#'
#' The column is a vector of cells, so the cell spec's bounds and enum apply
#' elementwise. A cell spec's `nullable` decides whether NA is allowed: a
#' metric that is undefined for a row (sensitivity for a class with no cases)
#' is a real NA, not a malformed one.
#'
#' @param column Vector: One column of the data frame.
#' @param fields Named list of the cell spec's fields, from `spec_fields()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_table_column <- function(column, fields) {
  type <- fields[["type"]]
  ok <- switch(
    type,
    boolean = is.logical(column),
    integer = is.integer(column),
    number = is.numeric(column),
    string = is.character(column),
    object = is.list(column)
  )
  if (!ok) {
    return(paste0("must be of type '", type, "'."))
  }
  if (!fields[["nullable"]] && anyNA(column)) {
    return("must not contain missing values.")
  }
  present <- column[!is.na(column)]
  minimum <- fields[["minimum"]]
  maximum <- fields[["maximum"]]
  exclusive_minimum <- fields[["exclusive_minimum"]]
  exclusive_maximum <- fields[["exclusive_maximum"]]
  enum <- fields[["enum"]]
  if (!is.null(minimum) && any(present < minimum)) {
    return(paste0("must be >= ", minimum, "."))
  }
  if (!is.null(maximum) && any(present > maximum)) {
    return(paste0("must be <= ", maximum, "."))
  }
  if (!is.null(exclusive_minimum) && any(present <= exclusive_minimum)) {
    return(paste0("must be > ", exclusive_minimum, "."))
  }
  if (!is.null(exclusive_maximum) && any(present >= exclusive_maximum)) {
    return(paste0("must be < ", exclusive_maximum, "."))
  }
  if (!is.null(enum) && !all(present %in% enum)) {
    return(paste0(
      "must be one of ",
      paste0("'", enum, "'", collapse = ", "),
      "."
    ))
  }
  NULL
} # /rtemis::validate_table_column


# %% validate_member_names ----
#' Check the names present against a declared object shape
#'
#' Shared by the two containers built on `@members`. Every name present must be
#' declared -- an undeclared one is a typo or a field nothing downstream knows
#' how to read -- and every required one must be present. An optional declared
#' member may be absent, which is how a conditional metric reports "not computed
#' for this task".
#'
#' @param present Character: Names the value actually carries.
#' @param fields Named list of spec fields with `members` set, from
#'   `spec_fields()`.
#' @param noun Character: What one member is called in messages, "column" for a
#'   table and "field" for a struct.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_member_names <- function(present, fields, noun) {
  declared <- names(fields[["members"]])
  unknown <- setdiff(present, declared)
  if (length(unknown) > 0L) {
    return(paste0(
      "has undeclared ",
      noun,
      "(s) ",
      paste0("'", unknown, "'", collapse = ", "),
      "; declared ",
      noun,
      "s are ",
      paste0("'", declared, "'", collapse = ", "),
      "."
    ))
  }
  missing_required <- setdiff(
    fields[["required_members"]] %||% declared,
    present
  )
  if (length(missing_required) > 0L) {
    return(paste0(
      "is missing required ",
      noun,
      "(s) ",
      paste0("'", missing_required, "'", collapse = ", "),
      "."
    ))
  }
  NULL
} # /rtemis::validate_member_names


# %% validate_struct ----
#' Validate a `struct` container against its member specs
#'
#' A struct's members may themselves be containers, so each is checked by the
#' full `validate_with_spec()` rather than the elementwise column check.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields with `container = "struct"`, from
#'   `spec_fields()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_struct <- function(value, fields) {
  if (!is.list(value) || is.data.frame(value)) {
    return("must be a named list.")
  }
  if (length(value) > 0L && is.null(names(value))) {
    return("must be named.")
  }
  msg <- validate_member_names(names(value), fields, "field")
  if (!is.null(msg)) {
    return(msg)
  }
  for (nm in names(value)) {
    msg <- validate_with_spec(value[[nm]], fields[["members"]][[nm]])
    if (!is.null(msg)) {
      return(paste0("field '", nm, "' ", msg))
    }
  }
  NULL
} # /rtemis::validate_struct


# %% validate_table ----
#' Validate a `table` container against its column specs
#'
#' Every column present must be declared -- an undeclared one is a typo or a
#' field nothing downstream knows how to read -- and every required column must
#' be present. An optional declared column may be absent, which is how a
#' conditional metric reports "not computed for this task".
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields with `container = "table"`, from
#'   `spec_fields()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_table <- function(value, fields) {
  if (!is.data.frame(value)) {
    return("must be a data frame.")
  }
  msg <- validate_member_names(names(value), fields, "column")
  if (!is.null(msg)) {
    return(msg)
  }
  present <- names(value)
  for (nm in present) {
    msg <- validate_table_column(value[[nm]], fields[["members"]][[nm]])
    if (!is.null(msg)) {
      return(paste0("column '", nm, "' ", msg))
    }
  }
  NULL
} # /rtemis::validate_table


# %% validate_candidates ----
#' Check a hyperparameter domain against the spec it was assigned to
#'
#' Every candidate must be a valid *value* of the property, so each is checked
#' against the same spec with `tunable` cleared -- a domain of domains is not a
#' thing.
#'
#' One combination cannot be inferred at the call site and is caught here: a
#' single bare vector on a vector-valued hyperparameter, which is how one value
#' of it is written. `tune_over()` records that reading so it can be corrected
#' rather than silently taken as one candidate per element.
#'
#' @param value `HyperparameterCandidates` object.
#' @param fields Named list of spec fields, from `spec_fields()`.
#'
#' @return Character message, or NULL when valid.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_candidates <- function(value, fields) {
  if (!fields[["tunable"]]) {
    return("is not tunable, so it accepts a value rather than `tune_over()`.")
  }
  candidates <- value@candidates
  if (value@from_vector && fields[["container"]] != "none") {
    # A bare vector is how one value of this hyperparameter is written, so it
    # cannot also be read as a list of candidates.
    return(paste0(
      "was given one vector, which is a single value for this hyperparameter ",
      "rather than a set of candidates.\n",
      "Pass each candidate as its own argument -- ",
      "`tune_over(c(12L, 6L), c(24L, 12L))` -- or as a list."
    ))
  }
  candidate <- fields
  candidate[["tunable"]] <- FALSE
  for (i in seq_along(candidates)) {
    msg <- validate_with_spec(candidates[[i]], candidate)
    if (!is.null(msg)) {
      return(paste0("candidate ", i, " ", msg))
    }
  }
  NULL
} # /rtemis::validate_candidates


# %% validate_with_spec ----
#' Validate a property value against its PropertySpec
#'
#' Shared validator body for all factory-generated properties. Returns NULL if
#' valid, otherwise a character message (the S7 validator contract). The
#' property's S7 class (set by the factory) already enforces the base type;
#' this checks arity, missingness, bounds, and enum membership.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields, from `spec_fields()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_with_spec <- function(value, fields) {
  nullable <- fields[["nullable"]]
  container <- fields[["container"]]
  if (is.null(value)) {
    return(if (nullable) NULL else "must not be NULL.")
  }
  if (is_candidates(value)) {
    return(validate_candidates(value, fields))
  }
  if (fields[["tunable"]] && container == "none" && length(value) > 1L) {
    # A hyperparameter takes one value, so say what to write instead of only
    # what is wrong. `deparse()` rather than `format()`: the suggestion is meant
    # to be pasted back into source, and an integer written `3` instead of `3L`
    # would not reproduce the value it came from.
    shown <- vapply(
      utils::head(value, 3L),
      function(v) paste(deparse(v), collapse = ""),
      character(1L)
    )
    return(paste0(
      "was given ",
      length(value),
      " values, but a hyperparameter takes one.\n",
      "To search over them, mark them: `tune_over(",
      paste(shown, collapse = ", "),
      if (length(value) > 3L) ", ..." else "",
      ")`.\n",
      "A bare vector is a value, so it does not mean a search space."
    ))
  }
  if (fields[["type"]] == "object" && container == "none") {
    # One named list is a single value however many keys it holds, and its
    # contents are the backend's contract, not ours.
    return(if (is.list(value)) NULL else "must be a list.")
  }
  if (container == "matrix") {
    if (!is.matrix(value)) {
      return("must be a matrix.")
    }
    if (anyNA(value)) {
      return("must not contain missing values.")
    }
    return(NULL)
  }
  if (container == "table") {
    return(validate_table(value, fields))
  }
  if (container == "struct") {
    return(validate_struct(value, fields))
  }
  if (container == "factor" && !is.factor(value)) {
    # The generic checks below then apply to the labels: emptiness, missingness
    # and, where declared, enum membership.
    return("must be a factor.")
  }
  if (container == "map" && is.null(names(value))) {
    return("must be named.")
  }
  items <- fields[["items"]]
  if (!is.null(items) && items[["container"]] != "none") {
    # Elements are themselves containers, so each is validated against `items`.
    # A container of *scalars* is a plain (possibly named) R vector and falls
    # through to the generic checks below.
    if (fields[["broadcast"]] && !is.list(value)) {
      # A bare element stands in for the whole container.
      return(validate_with_spec(value, items))
    }
    if (!is.list(value)) {
      return("must be a list.")
    }
    msg <- validate_array_arity(value, fields)
    if (!is.null(msg)) {
      return(msg)
    }
    for (i in seq_along(value)) {
      msg <- validate_with_spec(value[[i]], items)
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
      if (nullable) {
        "must not be empty (use NULL to leave it unset)."
      } else {
        "must not be empty."
      }
    )
  }
  if (length(value) > 1L && !fields[["tunable"]] && container == "none") {
    return("must be a single value (not tunable, no search values allowed).")
  }
  msg <- validate_array_arity(value, fields)
  if (!is.null(msg)) {
    return(msg)
  }
  if (anyNA(value)) {
    return("must not contain missing values.")
  }
  minimum <- fields[["minimum"]]
  maximum <- fields[["maximum"]]
  exclusive_minimum <- fields[["exclusive_minimum"]]
  exclusive_maximum <- fields[["exclusive_maximum"]]
  enum <- fields[["enum"]]
  if (!is.null(minimum) && any(value < minimum)) {
    return(paste0("must be >= ", minimum, "."))
  }
  if (!is.null(maximum) && any(value > maximum)) {
    return(paste0("must be <= ", maximum, "."))
  }
  if (!is.null(exclusive_minimum) && any(value <= exclusive_minimum)) {
    return(paste0("must be > ", exclusive_minimum, "."))
  }
  if (!is.null(exclusive_maximum) && any(value >= exclusive_maximum)) {
    return(paste0("must be < ", exclusive_maximum, "."))
  }
  if (!is.null(enum) && !all(value %in% enum)) {
    return(paste0(
      "must be one of ",
      paste0("'", enum, "'", collapse = ", "),
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
  fields <- spec_fields(spec)
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
    spec_r_kind(fields),
    matrix = S7::new_S3_class("matrix"),
    table = class_data.frame,
    factor = class_factor,
    list = class_list,
    atomic_class
  )
  if (spec@tunable) {
    # A tunable hyperparameter holds either a value or the domain a tuner
    # chooses from. `spec_r_kind()` names the value's shape, so the union is
    # added here rather than there; the schema emits the same two shapes from
    # the same spec, as the nesting rule (see `spec_to_schema()`).
    base_class <- base_class | HyperparameterCandidates
  }
  p <- new_property(
    class = if (spec@nullable) NULL | base_class else base_class,
    default = spec@default,
    validator = spec_validator(fields)
  )
  p[["spec"]] <- fields
  p
} # /rtemis::make_prop


# %% spec_validator ----
#' Build a property's validator over its spec fields
#'
#' A factory rather than an inline closure so that the validator's environment
#' holds the fields and nothing else. An inline closure would capture the whole
#' calling frame, and anything reachable from it is written to the lazy-load
#' database alongside the validator.
#'
#' @param fields Named list of spec fields, from `spec_fields()`.
#'
#' @return Function of one argument, suitable as an S7 property validator.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_validator <- function(fields) {
  force(fields)
  function(value) validate_with_spec(value, fields)
} # /rtemis::spec_validator


# %% prop_boolean ----
#' Logical (boolean) S7 property with attached PropertySpec
#'
#' @param default Logical: Default value (NULL only if `nullable`).
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param applies_when Optional named list: Sibling properties this one is only
#'   in effect for, mapped to the values that put it in effect. Requires
#'   `nullable`.
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
  applies_when = NULL,
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
    applies_when = applies_when,
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
#' @param tune_on_null Logical: If TRUE, a NULL value means "determine by
#'   tuning". Requires `nullable`.
#' @param default_on_null Logical: If TRUE, a NULL value means "apply the
#'   default for this task type". Requires `nullable`; mutually exclusive with
#'   `tune_on_null`.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param min_items Integer [1, Inf): Fewest elements a `vector` value may
#'   hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset -- `data_bound` names the dimension -- so a form should not prompt
#'   for it. An annotation only; it does not affect serialization.
#' @param applies_when Optional named list: Sibling properties this one is only
#'   in effect for, mapped to the values that put it in effect. Requires
#'   `nullable`.
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
  tune_on_null = FALSE,
  default_on_null = FALSE,
  vector = FALSE,
  broadcast = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  applies_when = NULL,
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
    tune_on_null = tune_on_null,
    default_on_null = default_on_null,
    container = if (vector) "array" else "none",
    broadcast = broadcast,
    min_items = min_items,
    unique_items = unique_items,
    data_bound = data_bound,
    data_dependent = data_dependent,
    applies_when = applies_when,
    description = description
  ))
} # /rtemis::prop_integer


# %% prop_float ----
#' Numeric (floating-point) S7 property with attached PropertySpec
#'
#' The only factory whose name differs from its JSON Schema type: it emits
#' type "number" (which in JSON Schema includes integers), but is named
#' `prop_float` because declarers think in the integer/float pairing --
#' "number" next to `prop_integer` invites the same ambiguity as R's
#' "numeric". Accepts R integer values too (`class_numeric`): JSON numbers
#' parse to double anyway, and floats are a superset of integers.
#'
#' @param default Numeric: Default value (NULL only if `nullable`).
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param tunable Logical: If TRUE, accepts a vector of search values.
#' @param tune_on_null Logical: If TRUE, a NULL value means "determine by
#'   tuning". Requires `nullable`.
#' @param default_on_null Logical: If TRUE, a NULL value means "apply the
#'   default for this task type". Requires `nullable`; mutually exclusive with
#'   `tune_on_null`.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param min_items Integer [1, Inf): Fewest elements a `vector` value may
#'   hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param data_bound Character or NULL: Training-data dimension constraining
#'   this value \{"n_features", "n_cases", "n_classes"\}. Scalar properties are
#'   bounded above by it; `vector` properties must have exactly that length.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset -- `data_bound` names the dimension -- so a form should not prompt
#'   for it. An annotation only; it does not affect serialization.
#' @param applies_when Optional named list: Sibling properties this one is only
#'   in effect for, mapped to the values that put it in effect. Requires
#'   `nullable`.
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
  tune_on_null = FALSE,
  default_on_null = FALSE,
  vector = FALSE,
  broadcast = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  applies_when = NULL,
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
    tune_on_null = tune_on_null,
    default_on_null = default_on_null,
    container = if (vector) "array" else "none",
    broadcast = broadcast,
    min_items = min_items,
    unique_items = unique_items,
    data_bound = data_bound,
    data_dependent = data_dependent,
    applies_when = applies_when,
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
#' @param tune_on_null Logical: If TRUE, a NULL value means "determine by
#'   tuning". Requires `nullable`.
#' @param default_on_null Logical: If TRUE, a NULL value means "apply the
#'   default for this task type". Requires `nullable`; mutually exclusive with
#'   `tune_on_null`.
#' @param vector Logical: If TRUE, the value is vector-valued (JSON array);
#'   mutually exclusive with `tunable`.
#' @param broadcast Logical: If TRUE, a bare scalar is accepted in place of the
#'   vector, meaning "this value for every element". Requires `vector`.
#' @param min_items Integer [1, Inf): Fewest elements a `vector` value may
#'   hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param data_bound Character or NULL: Only "feature_names" is meaningful for
#'   a string property: values must be a subset of the training features.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset -- `data_bound` names the dimension -- so a form should not prompt
#'   for it. An annotation only; it does not affect serialization.
#' @param applies_when Optional named list: Sibling properties this one is only
#'   in effect for, mapped to the values that put it in effect. Requires
#'   `nullable`.
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
  tune_on_null = FALSE,
  default_on_null = FALSE,
  vector = FALSE,
  broadcast = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  applies_when = NULL,
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
    tune_on_null = tune_on_null,
    default_on_null = default_on_null,
    container = if (vector) "array" else "none",
    broadcast = broadcast,
    min_items = min_items,
    unique_items = unique_items,
    data_bound = data_bound,
    data_dependent = data_dependent,
    applies_when = applies_when,
    description = description
  ))
} # /rtemis::prop_string


# %% prop_map ----
#' String-keyed map S7 property with attached PropertySpec
#'
#' A named R vector (or named list, when `values` is itself a container) whose
#' keys are data-dependent -- per-feature scaling values, per-feature one-hot
#' levels. Maps to a JSON object with `additionalProperties` describing the
#' value.
#'
#' @param values S7 property built by a `prop_*` factory: Describes one value of
#'   the map. Its own `default` is unused.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param data_bound Character or NULL: Training-data dimension the number of
#'   entries is tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the keys come from one dataset --
#'   feature names, unless `data_bound` names another dimension -- so a form
#'   should not prompt for it. An annotation only; it does not affect
#'   serialization.
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
#' An R list whose elements are themselves containers -- one weight vector per
#' tree, one in-bag count vector per tree. For a flat vector of scalars use the
#' `vector = TRUE` argument of the scalar factories instead.
#'
#' @param items S7 property built by a `prop_*` factory: Describes one element.
#'   Its own `default` is unused.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param broadcast Logical: If TRUE, a bare element is accepted in place of the
#'   list, meaning "this element for every position".
#' @param min_items Integer [1, Inf): Fewest elements the list may hold.
#' @param unique_items Logical: If TRUE, the elements must be distinct.
#' @param data_bound Character or NULL: Training-data dimension the number of
#'   elements is tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset -- `data_bound` names the dimension -- so a form should not prompt
#'   for it. An annotation only; it does not affect serialization.
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
  min_items = 1L,
  unique_items = FALSE,
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
    min_items = min_items,
    unique_items = unique_items,
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
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset -- `data_bound` names the dimension its rows follow -- so a form
#'   should not prompt for it. An annotation only; it does not affect
#'   serialization.
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


# %% prop_factor ----
#' Factor S7 property with attached PropertySpec
#'
#' An R factor -- a classification outcome or a predicted class. Serializes as
#' `{levels, codes}`: the levels in order, and a 1-based index into them per
#' case. That is what every categorical type stores -- an R factor, an Arrow
#' dictionary, a pandas Categorical -- and an array of labels is not a
#' substitute: it loses the level *order*, which is what decides the positive
#' class in binary classification, and any level with no cases.
#'
#' The levels are the outcome's own, so they travel with the value rather than
#' being declared; pass `enum` only where the permitted labels are fixed by the
#' class, which then constrains them.
#'
#' @param enum Character or NULL: Allowed labels.
#' @param nullable Logical: If TRUE, NULL is a valid value. Must be TRUE: a
#'   spec's default has to validate, and there is no factor a class could
#'   default to -- the same constraint `prop_matrix()` and `prop_table()` carry.
#' @param data_bound Character or NULL: Training-data dimension the length is
#'   tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset (one entry per case), so a form should not prompt for it. An
#'   annotation only; it does not affect serialization.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_factor <- function(
  enum = NULL,
  nullable = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  description = ""
) {
  make_prop(PropertySpec(
    type = "string",
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = enum,
    nullable = nullable,
    tunable = FALSE,
    container = "factor",
    items = NULL,
    broadcast = FALSE,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_factor


# %% prop_table ----
#' Data frame (table) S7 property with attached PropertySpec
#'
#' A data frame with declared, heterogeneous columns -- a metrics table, one row
#' per class or per resample. Serializes row-oriented, as an array of objects,
#' which pandas (`orient="records"`), polars and DataFrames.jl all read
#' natively. Use `prop_matrix()` instead for a homogeneous numeric grid with no
#' column identities.
#'
#' Declare every column that can ever appear and name only the always-present
#' ones in `required`; an optional column's absence then reads as "not computed
#' for this task" rather than as an invalid table.
#'
#' @param columns Named list of S7 properties built by `prop_*` factories: One
#'   per column, each describing a *cell*. Their own defaults are unused.
#' @param required Character, optional: Names of the always-present columns.
#'   NULL means all of them.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param data_bound Character or NULL: Training-data dimension the row count is
#'   tied to; see `DATA_BOUNDS`.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset, so a form should not prompt for it. An annotation only; it does
#'   not affect serialization.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_table <- function(
  columns,
  required = NULL,
  nullable = FALSE,
  data_bound = NULL,
  data_dependent = FALSE,
  description = ""
) {
  column_specs <- member_specs(columns, "columns")
  # Resolved here rather than left NULL so that the published `required` and
  # the spec read back from it name the same set.
  required <- required %||% names(column_specs)
  make_prop(PropertySpec(
    # A table has no single leaf type; each column carries its own. "object"
    # names the row, which is what the row-oriented encoding emits.
    type = "object",
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "table",
    items = NULL,
    members = column_specs,
    required_members = required,
    broadcast = FALSE,
    data_bound = data_bound,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_table


# %% member_specs ----
#' Extract the PropertySpec of each member of a declared object shape
#'
#' @param members Named list of S7 properties built by `prop_*` factories.
#' @param what Character: Name of the calling factory's argument, for errors.
#'
#' @return Named list of `PropertySpec` objects.
#'
#' @author EDG
#' @keywords internal
#' @noRd
member_specs <- function(members, what) {
  if (!is.list(members) || length(members) == 0L || is.null(names(members))) {
    rtemis.core::abort(
      "`",
      what,
      "` must be a non-empty named list of prop_* properties.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  out <- lapply(names(members), function(nm) {
    spec <- get_spec(members[[nm]])
    if (is.null(spec)) {
      rtemis.core::abort(
        "`",
        what,
        "[['",
        nm,
        "']]` must be a property built by a prop_* factory.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    spec
  })
  names(out) <- names(members)
  out
} # /rtemis::member_specs


# %% prop_struct ----
#' Declared-object (struct) S7 property with attached PropertySpec
#'
#' A named list whose members are *declared* and heterogeneous -- the metrics
#' payload of a classification result, which holds an `overall` table, a
#' per-class table, and a scalar. Maps to a JSON object with `properties`, the
#' counterpart of `prop_map()`'s `additionalProperties`: use a map when the keys
#' are data (one per feature) and the values homogeneous, a struct when the keys
#' are part of the contract.
#'
#' Unlike a table's columns, a struct's members may themselves be containers.
#'
#' @param members Named list of S7 properties built by `prop_*` factories: One
#'   per field. Their own defaults are unused.
#' @param required Character, optional: Names of the always-present fields.
#'   NULL means all of them.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param data_dependent Logical: If TRUE, the value's shape follows one
#'   dataset, so a form should not prompt for it. An annotation only; it does
#'   not affect serialization.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_struct <- function(
  members,
  required = NULL,
  nullable = FALSE,
  data_dependent = FALSE,
  description = ""
) {
  specs <- member_specs(members, "members")
  make_prop(PropertySpec(
    # "object" names the shape the struct emits.
    type = "object",
    default = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    enum = NULL,
    nullable = nullable,
    tunable = FALSE,
    container = "struct",
    items = NULL,
    members = specs,
    # Resolved here rather than left NULL so that the published `required` and
    # the spec read back from it name the same set.
    required_members = required %||% names(specs),
    broadcast = FALSE,
    data_bound = NULL,
    data_dependent = data_dependent,
    description = description
  ))
} # /rtemis::prop_struct


# %% prop_const ----
#' Constant S7 property with attached PropertySpec
#'
#' A value determined by the class rather than chosen by the user -- LightRF's
#' `boosting_type = "rf"`, LinearSVM's `kernel = "linear"`. It is what makes
#' the class that class, so it is declared the same way as the constant
#' `algorithm` discriminator: a computed property with no setter, hence
#' immutable by construction.
#'
#' Distinct from a **fixed** property, which the user *does* set but cannot
#' tune. Fixed is about tunability; constant is about settability.
#'
#' Emits `{"const": value}`, which is an assertion -- a config supplying any
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
  p[["spec"]] <- spec_fields(spec)
  p
} # /rtemis::prop_const


# %% tune_on_null_spec_names ----
#' Names of properties whose NULL value means "determine by tuning"
#'
#' @param x S7 class.
#'
#' @return Character vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tune_on_null_spec_names <- function(x) {
  names(Filter(
    function(p) {
      s <- get_spec(p)
      !is.null(s) && s@tune_on_null
    },
    x@properties
  ))
} # /rtemis::tune_on_null_spec_names


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


# %% applies_when_spec_names ----
#' Names of an S7 class's properties whose applicability a sibling gates
#'
#' @param x S7 class.
#'
#' @return Character vector: Names of properties whose `PropertySpec` carries an
#'   `applies_when`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
applies_when_spec_names <- function(x) {
  names(Filter(
    function(p) {
      fields <- get_spec_fields(p)
      !is.null(fields) && !is.null(fields[["applies_when"]])
    },
    x@properties
  ))
} # /rtemis::applies_when_spec_names


# %% format_allowed ----
#' Format a gate's allowed values for an error message
#'
#' @param values Atomic vector.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_allowed <- function(values) {
  values <- format(values, trim = TRUE)
  if (length(values) == 1L) {
    return(values)
  }
  paste0(
    paste(values[-length(values)], collapse = ", "),
    " or ",
    values[length(values)]
  )
} # /rtemis::format_allowed


# %% check_applies_when ----
#' Check an object's gated properties against the siblings that gate them
#'
#' A gated property is set only where it has an effect. Because a tunable
#' property holds *search values*, the gate passes when **any** of the gating
#' property's values opens it: the combination is then a conditional search, and
#' `tuning_grid()` drops the gated property from the cells that cannot use it.
#' Only a search no value of which opens the gate is rejected, since there the
#' gated value would be silently ignored in every cell.
#'
#' Call from a class validator. The declaration itself (gates naming real,
#' ungated siblings) is audited in the test suite, not here.
#'
#' @param object S7 object whose class declares the properties.
#'
#' @return Character scalar (the validator message) or NULL if every gate holds.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_applies_when <- function(object) {
  cls <- S7_class(object)
  for (nm in applies_when_spec_names(cls)) {
    if (is.null(prop(object, nm))) {
      next
    }
    gate <- get_spec_fields(cls@properties[[nm]])[["applies_when"]]
    for (gate_name in names(gate)) {
      allowed <- gate[[gate_name]]
      # The gate opens when any value the gating hyperparameter can take is
      # listed, so a domain contributes all of its candidates.
      gate_values <- prop(object, gate_name)
      if (is_candidates(gate_values)) {
        gate_values <- unlist(gate_values@candidates, use.names = FALSE)
      }
      if (!any(gate_values %in% allowed)) {
        return(paste0(
          "@",
          nm,
          " applies only when @",
          gate_name,
          " is ",
          format_allowed(allowed),
          ", and no value of @",
          gate_name,
          " is. Set @",
          gate_name,
          " accordingly, or leave @",
          nm,
          " NULL."
        ))
      }
    }
  }
  NULL
} # /rtemis::check_applies_when


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
  fields <- get_spec_fields(prop)
  if (is.null(fields)) {
    return(NULL)
  }
  spec_object(fields)
} # /rtemis::get_spec


# %% get_spec_fields ----
#' Get the stored spec fields of an S7 property, or NULL
#'
#' The stored form, without rebuilding a `PropertySpec`. Use this where only a
#' field or two is needed; use `get_spec()` where the caller reads fields with
#' `@`.
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Named list of spec fields, or NULL if the property was not built by a
#'   `prop_*` factory.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_spec_fields <- function(prop) {
  # An S7 property is a named list. Anything else was not built by a factory,
  # which the callers that accept a property as an *argument* must be able to
  # tell without tripping over `[[` on an atomic vector.
  if (!is.list(prop)) {
    return(NULL)
  }
  prop[["spec"]]
} # /rtemis::get_spec_fields


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
# Two orthogonal axes, deliberately not one:
#
# `role` -- WHO writes the value, which decides what the schema says.
#
# - "config"   Supplied by the user. Built by a `prop_*` factory, so it carries
#              a `PropertySpec`. The common case; inferred, never declared.
# - "state"    Written by the run, not the user (GLMNET's `lambda.min`, the
#              centers `preprocess()` learns). Declared with `prop_state()`,
#              and emitted `readOnly`: a reader needs the field to reconstruct
#              the class, but must not prompt for it.
# - "computed" A view derived from other published fields, never stored
#              (`DataFingerprint@portability` is a function of `@method`).
#              Declared with `prop_computed()`. Absent from the schema and from
#              a written config: publishing it would be a second representation
#              of something already there, free to disagree with the first.
# - "r_only"   An R value with no wire form at all: a fitted backend model (an
#              `rpart` tree, an `lgb.Booster`), or a `sessionInfo()`. Declared
#              with `prop_r_only()`, and likewise absent from the schema.
#
# The line between "computed" and "r_only" is what a consumer can do about it.
# A computed field is *recoverable* -- everything it derives from is published,
# so a port loses nothing by its absence. An r_only field is not: the value
# exists only inside R, and the `.rds` is its only carrier. Declaring it opaque
# instead would promise a field that never arrives, which is why publishing an
# escape hatch was rejected here as it was for inputs.
#
# State is never written to a config: `lambda.min` and `best_iter` are copied
# off the fitted model, which is the carrier, so the copy is re-derived on read.
# A *record* does carry them, but that is the record generator's business, not a
# per-property flag.
#
# `prop_serialized()` answers the question for both roles, and every family's
# `serializable_props` goes through it -- a flat config must not serialize a
# field a nested one drops just because it has no method of its own.
#
# A spec-less property with no role is drift: it is neither a declared input
# nor declared state, and schema generation aborts rather than quietly emitting
# an incomplete contract.
#
# `data_dependent` is a third axis and a *pure annotation*: the value is shaped
# by one dataset (per-case IDs, an initial embedding, per-feature centers), so a
# form should not prompt for it. It does **not** gate serialization -- every
# data-dependent property is a settable input, and dropping a value the user
# supplied would lose it silently.

# %% prop_state ----
#' S7 property holding run state rather than configuration
#'
#' Written by the run, not the user. Appears in the generated schema marked
#' `readOnly` -- a reader needs the field to reconstruct the class, and a run
#' record carries it -- so a form builder shows it without prompting for it.
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


# %% prop_serialized ----
#' Whether `write_config()` emits a property's value
#'
#' Everything a user can set is written; state is not, being re-derived on read.
#' Independent of `prop_role()` only in emphasis: the role decides what the
#' schema says, this decides what a written config contains.
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_serialized <- function(prop) {
  role <- prop_role(prop)
  if (role %in% c("state", "computed", "r_only")) {
    return(FALSE)
  }
  spec <- get_spec(prop)
  if (is.null(spec)) {
    # A spec-less, role-less property is machinery (a computed payload list, a
    # discriminator); its family's `serializable_props` decides.
    return(TRUE)
  }
  # Everything a user can set is written back, including the data-shaped values
  # (`id_strat`, `Y_init`, learned scaling centers): dropping a value the user
  # supplied would lose it silently. Only a constant is omitted, being implied
  # by the algorithm.
  !spec@constant
} # /rtemis::prop_serialized


# %% prop_computed ----
#' S7 property that is a derived view, not part of the contract
#'
#' Marks a computed property as *derivable from other published fields*, so it
#' is omitted from the generated schema and from a written config rather than
#' aborting generation as undeclared drift.
#'
#' Unlike `prop_state()` this takes a plain S7 property: a view has a getter and
#' no `PropertySpec`, there being nothing to validate -- its value is a function
#' of fields that are themselves validated.
#'
#' @param property S7 property (typically one with a `getter`).
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_computed <- function(property) {
  property[["role"]] <- "computed"
  property
} # /rtemis::prop_computed


# %% prop_r_only ----
#' S7 property that exists only in R
#'
#' Marks a property with no wire form at all -- a fitted backend model, a
#' `sessionInfo()` -- so it is omitted from the generated schema and from
#' serialization rather than aborting generation as undeclared drift.
#'
#' Reach for it only when the value genuinely exists and cannot travel. A slot
#' that holds nothing wants deleting, not declaring: the marker exists so that
#' forgetting to declare a property fails loudly, and it cannot tell you that a
#' property should not be there at all.
#'
#' Distinct from `prop_computed()`: a computed view is recoverable from fields
#' that *are* published, so its absence costs a consumer nothing, while an
#' r_only value exists only inside R and the saved `.rds` is its only carrier.
#' The marker is required rather than inferred, so that adding a property and
#' forgetting to declare it still fails loudly.
#'
#' @param property S7 property.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_r_only <- function(property) {
  property[["role"]] <- "r_only"
  property
} # /rtemis::prop_r_only


# %% prop_role ----
#' Role of an S7 property
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Character: "config", "state", "computed", "r_only", or `NA_character_` for a
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


# %% prop_published ----
#' Whether a property is part of the published contract
#'
#' True for everything the generated schema declares: configuration and run
#' state alike. False for a computed view (recoverable from published fields)
#' and for an r_only value (no wire form at all), which is why both are also
#' absent from [to_json].
#'
#' Distinct from `prop_serialized()`, which answers the narrower question of
#' whether `write_config()` emits the value: state is published but never
#' written to a config, being re-derived on read.
#'
#' A spec-less property with no role counts as published, so a class that has
#' not been migrated to the factories serializes as it always did; schema
#' generation is where that drift is caught.
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_published <- function(prop) {
  !prop_role(prop) %in% c("computed", "r_only")
} # /rtemis::prop_published


# %% published_prop_names ----
#' Names of an S7 class's published properties
#'
#' @param x S7 class.
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
published_prop_names <- function(x) {
  names(Filter(prop_published, x@properties))
} # /rtemis::published_prop_names


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
#' Membership is decided by `prop_serialized()`, so this and a flat config's
#' `serializable_props` answer the question the same way. Dropped: **constants**,
#' which the algorithm already implies, and **state** whose value the fitted
#' model already carries (GLMNET `lambda.min`, LightGBM `best_iter`), which is
#' re-derived on read. Everything a user can set is kept, data-shaped values
#' included -- see "Property roles".
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
      # A nested config serializes as its own schema, whatever its role here.
      S7_inherits(values[[nm]]) || prop_serialized(props[[nm]])
    },
    logical(1L)
  )
  values <- values[keep]
  for (nm in names(values)) {
    values[[nm]] <- wire_value(values[[nm]], props[[nm]])
  }
  values
} # /rtemis::config_prop_values


# %% wire_value ----
#' A property value in the shape its JSON Schema declares
#'
#' One conversion, for one mismatch: a `map` container publishes
#' `type: object`, but its R value for a scalar leaf is a *named atomic vector*,
#' and `jsonlite::toJSON()` drops names on atomic vectors -- emitting an array,
#' which its own schema rejects. Handing it a list restores the object.
#'
#' Deliberately spec-driven rather than "name any named vector": Ranger's
#' `class_weights` is a named numeric too, but declares an `array`, and naming
#' it must not change its wire type.
#'
#' @param value Property value.
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return The value, as a list when the spec declares a map.
#'
#' @author EDG
#' @keywords internal
#' @noRd
wire_value <- function(value, prop) {
  if (is.null(value)) {
    return(value)
  }
  spec <- get_spec(prop)
  if (is.null(spec)) {
    return(value)
  }
  if (is_candidates(value)) {
    # Tagged, so a reader tells a search space from a value without knowing the
    # property's declared type. A scalar hyperparameter's candidates flatten to
    # an array; a vector-valued one's stay a list, so each candidate keeps its
    # own array.
    return(list(
      candidates = if (spec@container == "none") {
        unlist(value@candidates, use.names = FALSE)
      } else {
        value@candidates
      }
    ))
  }
  if (spec@container == "map" && is.atomic(value)) {
    return(as.list(value))
  }
  if (spec@container == "factor" && is.factor(value)) {
    # `toJSON()` on a factor emits its labels and drops the levels attribute,
    # losing both their order -- which is what decides the positive class --
    # and any level with no cases.
    return(list(levels = levels(value), codes = as.integer(value)))
  }
  value
} # /rtemis::wire_value


# %% from_wire ----
#' Restore R-side shapes from values read back from JSON
#'
#' The inverse of `wire_value()`, and the single wire -> R translation: every
#' `.list_to_*()` reconstructor calls it, so a shape that needs rebuilding is
#' handled once rather than per config kind. Three shapes differ between the
#' wire and R, each decided by the property's own spec:
#'
#' - A **map** over a scalar leaf is a named atomic vector in R and a JSON
#'   object, which parses to a named list the property's class check rejects.
#' - A **factor** travels as `{levels, codes}` and must be rebuilt, levels and
#'   their order included.
#' - A **domain** is tagged, since JSON has no function calls and so no
#'   `tune_over()`. `{"candidates": [...]}` is a search space and anything else
#'   is a value, which takes no reference to the declared type at all.
#'
#' @param x Named list parsed from JSON.
#' @param cls S7 class the list reconstructs.
#'
#' @return `x`, with each of those shapes restored to its R form.
#'
#' @author EDG
#' @keywords internal
#' @noRd
from_wire <- function(x, cls) {
  props <- cls@properties
  for (nm in intersect(names(x), names(props))) {
    fields <- get_spec_fields(props[[nm]])
    if (is.null(fields)) {
      next
    }
    container <- fields[["container"]]
    is_scalar_map <- container == "map" && spec_r_kind(fields) == "atomic"
    if (is_scalar_map && is.list(x[[nm]])) {
      x[[nm]] <- unlist(x[[nm]])
    }
    if (container == "factor" && is.list(x[[nm]])) {
      x[[nm]] <- from_wire_factor(x[[nm]])
    }
    if (is_wire_candidates(x[[nm]])) {
      x[[nm]] <- HyperparameterCandidates(
        candidates = as.list(x[[nm]][["candidates"]])
      )
    }
  }
  x
} # /rtemis::from_wire


# %% from_wire_factor ----
#' Rebuild a factor from its `{levels, codes}` wire form
#'
#' @param x Named list with `levels` and `codes`, as parsed from JSON.
#'
#' @return Factor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
from_wire_factor <- function(x) {
  levels <- as.character(x[["levels"]])
  codes <- as.integer(x[["codes"]])
  if (length(codes) > 0L && max(codes) > length(levels)) {
    rtemis.core::abort(
      "Factor codes index past the declared levels: `codes` are 1-based positions in `levels`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  factor(levels[codes], levels = levels)
} # /rtemis::from_wire_factor


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


# %% data_bound_note ----
#' The sentence describing a `data_bound` constraint
#'
#' JSON Schema has no view of the training data, so a `data_bound` cannot be
#' expressed structurally and is carried in the property description instead.
#' Both directions share this one definition: `spec_to_schema()` appends it,
#' `schema_to_spec()` strips it back off. Two copies would let the reader fail
#' to recognize a sentence the writer had changed.
#'
#' @param data_bound Character: The bound; see `DATA_BOUNDS`.
#' @param container Character \{"none", "array", "map", "matrix", "table"\}: How
#' values are wrapped.
#' @param broadcast Logical: Whether a bare scalar stands in for the container.
#'
#' @return Character: The sentence appended to the description.
#'
#' @author EDG
#' @keywords internal
#' @noRd
data_bound_note <- function(data_bound, container, broadcast) {
  # A name bound is a membership rule, not a length rule, so it has no noun in
  # the table.
  if (data_bound == "feature_names") {
    return("Values must name training features.")
  }
  if (data_bound == "numeric_feature_names") {
    return("Values must name numeric training features.")
  }
  noun <- DATA_BOUND_NOUN[[data_bound]]
  if (container %in% c("matrix", "table")) {
    paste0("Must have one row per ", noun, ".")
  } else if (container == "map") {
    paste0("Must have one entry per ", noun, ".")
  } else if (container != "none" && broadcast) {
    # A scalar is explicitly allowed, so the length rule binds only the vector
    # form.
    paste0("A vector must have one value per ", noun, ".")
  } else if (container != "none") {
    paste0("Must have one value per ", noun, ".")
  } else {
    paste0(
      "Cannot exceed the number of ",
      DATA_BOUND_NOUN_PLURAL[[data_bound]],
      " in the training data."
    )
  }
} # /rtemis::data_bound_note


# %% data_dependent_comment ----
#' The `$comment` describing a data-dependent property
#'
#' Names the dimension *this* property follows, so a reader is told which of
#' cases, features or classes decides its shape rather than being handed the
#' union of them. The dimension comes from `data_bound` where one is declared;
#' a `map` with none is keyed by feature name (see `prop_map()`), the one shape
#' whose dependence the container states on its own. Anything else falls back to
#' the bare fact, since nothing in the declaration says more.
#'
#' @param spec `PropertySpec`.
#'
#' @return Character: The sentence emitted as the schema's `$comment`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
data_dependent_comment <- function(spec) {
  bound <- spec@data_bound
  container <- spec@container
  shape <- if (identical(bound, "feature_names")) {
    "its values name training features"
  } else if (identical(bound, "numeric_feature_names")) {
    "its values name numeric training features"
  } else if (!is.null(bound) && container != "none") {
    noun <- DATA_BOUND_NOUN[[bound]]
    if (container %in% c("matrix", "table")) {
      paste0("one row per ", noun)
    } else if (container == "map") {
      paste0("one entry per ", noun)
    } else if (spec@broadcast) {
      # A scalar stands in for the container, so the arity rule binds only the
      # vector form.
      paste0("one value per ", noun, " when given as a vector")
    } else {
      paste0("one value per ", noun)
    }
  } else if (container == "map") {
    "one entry per feature, keyed by feature name"
  } else {
    "its shape is decided by the training data"
  }
  paste0(
    "Data-dependent: ",
    shape,
    ", so it cannot be filled in without the data."
  )
} # /rtemis::data_dependent_comment


# %% candidates_schema ----
#' The JSON Schema object a hyperparameter domain emits
#'
#' A search space is tagged rather than distinguished by nesting depth, so a
#' reader can tell it from a value without consulting the property's declared
#' type. That matters because depth alone is not decisive: a broadcast array of
#' arrays and a container tunable's search space are the same shape, and only
#' the annotation separated them.
#'
#' The tag also leaves room to grow. A domain that is not enumerable -- a range
#' a random or Bayesian tuner samples -- is a sibling key here, where a bare
#' nested array has nowhere to put one.
#'
#' @param value_schema Named list: The schema of one value of the property,
#' which is what one candidate is.
#'
#' @return Named list (JSON Schema object).
#'
#' @author EDG
#' @keywords internal
#' @noRd
candidates_schema <- function(value_schema) {
  list(
    type = "object",
    properties = list(
      candidates = list(
        type = "array",
        items = value_schema,
        minItems = 2L,
        description = "Values to tune over; the tuner selects one."
      )
    ),
    required = I("candidates"),
    additionalProperties = FALSE
  )
} # /rtemis::candidates_schema


# %% applies_when_note ----
#' The sentence describing an `applies_when` gate
#'
#' The gate is emitted structurally in `x-rtemis`, but a reader that renders
#' only descriptions would otherwise show a conditional value as an
#' unconditional one. Both directions share this one definition:
#' `spec_to_schema()` appends it, `schema_to_spec()` strips it back off. Two
#' copies would let the reader fail to recognize a sentence the writer changed.
#'
#' @param applies_when Named list: The gate; see `PropertySpec`.
#'
#' @return Character: The sentence appended to the description.
#'
#' @author EDG
#' @keywords internal
#' @noRd
applies_when_note <- function(applies_when) {
  clauses <- vapply(
    names(applies_when),
    function(nm) paste0(nm, " is ", format_allowed(applies_when[[nm]])),
    character(1L)
  )
  paste0("Applies only when ", paste(clauses, collapse = " and "), ".")
} # /rtemis::applies_when_note


# %% members_schema ----
#' The JSON Schema object a declared shape emits
#'
#' Shared by the two containers built on `@members`: a `struct` emits this
#' directly, a `table` emits an array of it. Every declared member appears in
#' `properties`, but only the always-present ones in `required`, so an optional
#' member simply does not appear where it was not computed.
#' `additionalProperties: false` makes an undeclared one an error rather than
#' something a reader silently drops.
#'
#' Takes the members directly rather than the owning spec, so that record
#' *structure* -- a tuning table's rows, which belong to no class -- can be
#' built from the same declarations as a class property's.
#'
#' @param members Named list of `PropertySpec` objects, one per member.
#' @param required Character or NULL: The always-present members. NULL means
#'   all of them.
#'
#' @return Named list (JSON Schema object).
#'
#' @author EDG
#' @keywords internal
#' @noRd
members_schema <- function(members, required = NULL) {
  out <- list(
    type = "object",
    properties = lapply(members, spec_to_schema),
    additionalProperties = FALSE
  )
  required <- required %||% names(members)
  if (length(required) > 0L) {
    out[["required"]] <- I(required)
  }
  out
} # /rtemis::members_schema


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
#' @param read_only Logical: If TRUE, the property is run state -- marked
#'   `readOnly` and annotated `role: "state"`.
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
    arr <- Filter(
      Negate(is.null),
      list(
        type = if (spec@nullable) I(c("array", "null")) else "array",
        items = element,
        minItems = spec@min_items,
        uniqueItems = if (spec@unique_items) TRUE else NULL
      )
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
    } else if (spec@tunable) {
      # The value is the array; a search space is the tagged object, whose
      # candidates are each one whole array.
      arr[["type"]] <- "array"
      branches <- list(arr, candidates_schema(arr))
      if (spec@nullable) {
        branches <- c(list(list(type = "null")), branches)
      }
      list(oneOf = branches)
    } else {
      arr
    }
  } else if (spec@container == "factor") {
    # Levels and per-case codes, the representation every categorical type
    # uses. The levels are the outcome's own, so they travel with the value
    # rather than being declared -- except where `enum` fixes the vocabulary,
    # which constrains them here.
    list(
      type = if (spec@nullable) I(c("object", "null")) else "object",
      properties = list(
        levels = list(
          type = "array",
          items = Filter(
            Negate(is.null),
            list(type = "string", enum = if (!is.null(spec@enum)) I(spec@enum))
          ),
          minItems = 1L,
          uniqueItems = TRUE,
          description = "Levels, in order."
        ),
        codes = list(
          type = "array",
          items = list(type = "integer", minimum = 1L),
          description = "One 1-based index into `levels` per case."
        )
      ),
      required = I(c("levels", "codes")),
      additionalProperties = FALSE
    )
  } else if (spec@container == "matrix") {
    row <- list(type = "array", items = scalar, minItems = 1L)
    list(
      type = if (spec@nullable) I(c("array", "null")) else "array",
      items = row,
      minItems = 1L
    )
  } else if (spec@container == "table") {
    # Row-oriented: an array of the declared object shape, one per row.
    list(
      type = if (spec@nullable) I(c("array", "null")) else "array",
      items = members_schema(spec@members, spec@required_members)
    )
  } else if (spec@container == "struct") {
    obj <- members_schema(spec@members, spec@required_members)
    if (spec@nullable) {
      obj[["type"]] <- I(c("object", "null"))
    }
    obj
  } else if (spec@container == "map") {
    # A string-keyed object of homogeneous values (per-feature centers).
    list(
      type = if (spec@nullable) I(c("object", "null")) else "object",
      additionalProperties = element
    )
  } else if (spec@tunable) {
    # The value, or the tagged domain the Tuner chooses from.
    branches <- list(scalar, candidates_schema(scalar))
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
    note <- data_bound_note(spec@data_bound, spec@container, spec@broadcast)
    description <- if (nzchar(description)) {
      paste(description, note)
    } else {
      note
    }
  }
  # Carries the gate to a reader that renders only descriptions; it is also
  # emitted structurally below.
  if (!is.null(spec@applies_when)) {
    note <- applies_when_note(spec@applies_when)
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
      tune_on_null = if (spec@tune_on_null) TRUE else NULL,
      default_on_null = if (spec@default_on_null) TRUE else NULL,
      data_bound = spec@data_bound,
      data_dependent = if (spec@data_dependent) TRUE else NULL,
      # A gate over *search values*: it opens when any one of them is listed,
      # and the tuning grid drops the property from the cells where it is not.
      applies_when = spec@applies_when
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
    # skips these rather than asking for a value whose shape the data decides.
    # It does not say the value is derived -- these are settable inputs, and a
    # supplied one is used in place of computing it.
    out[["$comment"]] <- data_dependent_comment(spec)
  }
  out
} # /rtemis::spec_to_schema


# %% VALUE_ORIGINS ----
# Where a value in a *record* came from. A record states what a run used; this
# states how each value got there, so reading one document answers "did I choose
# this, or did the run?" without diffing against the input config.
#
# - "user"     supplied in the config
# - "default"  neither supplied nor computed: the `setup_*` default applied
# - "derived"  computed by the run from the data (LightGBM's `objective` from
#              the outcome type, the centers `preprocess()` learns)
# - "tuned"    selected by the Tuner from a search space
# - "unset"    the run never determined it -- it failed or was canceled first.
#              The value is `null`, and saying so is what lets an incomplete
#              record still state something true about every field, instead of
#              making completeness a conditional on `outcome`.
#
# "default" is kept distinct from "user" because folding them would claim
# somebody chose `strat_n_bins = 4`; "tuned" from "derived" because a value
# cross-validation selected is a different fact from one read off the data.
VALUE_ORIGINS <- c("user", "default", "derived", "tuned", "unset")


# %% origin_schema ----
#' The `origin` block of a record schema
#'
#' A parallel map rather than per-field wrappers: values keep their plain shape,
#' so a record stays diffable against a config and every reader of one can read
#' the other. This is the same "flat + annotate" choice the property schemas
#' make (see the governing principle in `plan/rtemis-types.md`).
#'
#' Each field's permitted origins are narrowed by what it is: run state can only
#' have been computed, and a value cannot be `"tuned"` unless it is tunable. The
#' schema therefore rejects a record claiming a user supplied `lambda.min`.
#'
#' Nested config properties (`$ref`) are excluded: each carries its own `origin`
#' block, so the parent would be duplicating it.
#'
#' @param props Named list of S7 properties the record declares.
#'
#' @return Named list: the `origin` property schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
origin_schema <- function(props) {
  entries <- lapply(names(props), function(nm) {
    spec <- get_spec(props[[nm]])
    # "unset" is always permitted: any field can be one the run never reached.
    allowed <- if (!is.null(spec) && spec@default_on_null) {
      # NULL applies the task-type default, which is a restatement of the
      # question rather than anything measured or searched.
      c("user", "default", "unset")
    } else if (identical(prop_role(props[[nm]]), "state")) {
      # Only a run writes it, so it was computed one way or the other.
      c("derived", "tuned", "unset")
    } else if (!is.null(spec) && (spec@tunable || spec@tune_on_null)) {
      # `tune_on_null` is the declaration that NULL means "determine this by
      # tuning" (GLMNET's `lambda`), so such a field can be tuned without
      # carrying a search space itself.
      VALUE_ORIGINS
    } else {
      setdiff(VALUE_ORIGINS, "tuned")
    }
    list(type = "string", enum = I(allowed))
  })
  names(entries) <- names(props)
  list(
    type = "object",
    description = paste(
      "Where each value came from: supplied in the config, left at its",
      "default, computed from the data, or selected by tuning."
    ),
    properties = entries,
    required = I(names(props)),
    additionalProperties = FALSE
  )
} # /rtemis::origin_schema


# %% folds_schema ----
#' The `folds` block of a supervised record schema
#'
#' A record's top level says what was *asked for*; `folds` says what *ran*, once
#' per outer resample. They are separate because a resampled run resolves
#' different values in each fold -- early stopping picks a different `nrounds`
#' every time -- so a single resolved value at the top level would be a claim
#' the run never made.
#'
#' A single fit is one fold rather than a second shape, so a position never
#' changes meaning between records.
#'
#' Built here rather than declared in the registry for the same reason
#' `origin_schema()` is: it is record *structure*, not a property of any class,
#' and the tree carries no hand-written JSON.
#'
#' @param refs Named character: record-schema URLs for the per-fold blocks
#'   (`hyperparameters`, and optionally `preprocessor_config` /
#'   `decomposition_config`).
#' @param metrics_refs Named character or NULL: schema URLs for the regression
#'   and classification metrics classes, `$ref`d by each fold's `metrics`.
#'
#' @return Named list: the `folds` property schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
folds_schema <- function(refs, metrics_refs = NULL) {
  nullable_ref <- function(url) {
    list(oneOf = list(list(type = "null"), list(`$ref` = url)))
  }
  properties <- list(
    index = list(
      type = "integer",
      minimum = 1L,
      description = "1-based outer resample this fold trained on."
    )
  )
  for (nm in c("preprocessor_config", "decomposition_config")) {
    if (!is.null(refs[[nm]])) {
      properties[[nm]] <- nullable_ref(refs[[nm]])
    }
  }
  properties[["hyperparameters"]] <- list(`$ref` = refs[["hyperparameters"]])
  # Null when the fold ran no tuning; otherwise the search, fully declared.
  properties[["tuning"]] <- list(
    oneOf = list(list(type = "null"), tuning_schema())
  )
  if (!is.null(metrics_refs)) {
    # Which metrics class applies follows from the outcome, not from anything
    # the record declares, so both are admitted and the reader takes whichever
    # validates.
    sample_schema <- list(
      oneOf = c(
        list(list(type = "null")),
        lapply(unname(metrics_refs), function(url) list(`$ref` = url))
      )
    )
    entries <- lapply(SUPERVISED_SAMPLES, function(...) sample_schema)
    names(entries) <- SUPERVISED_SAMPLES
    properties[["metrics"]] <- list(
      type = "object",
      description = "What this fold scored, in full, per sample.",
      properties = entries,
      additionalProperties = FALSE
    )
  }
  list(
    type = "array",
    minItems = 1L,
    description = paste(
      "What ran, once per outer resample. A single fit is one fold."
    ),
    items = list(
      type = "object",
      properties = properties,
      required = I(c("index", "hyperparameters")),
      additionalProperties = FALSE
    )
  )
} # /rtemis::folds_schema


# %% tuning_schema ----
#' The `tuning` block of one fold in a supervised record schema
#'
#' What a fold's inner tuning searched and found, as `Tuner@tuning_results`
#' holds it: the candidate grid, each candidate's training and validation
#' scores, and the winner. The three tables join on `param_combo_id`.
#'
#' Two of the shapes here have **data-dependent keys** and say so rather than
#' pretending otherwise: a `param_grid` row carries one column per
#' *hyperparameter being tuned*, and `best` is keyed the same way, so neither
#' set can be declared without a schema per algorithm. They are declared as far
#' as they can be -- the joining id, and the fact that every other value is a
#' scalar -- which is the same treatment `prop_map()` gives per-feature values.
#'
#' The score tables are fully declared, reusing the metric columns the metrics
#' classes carry, so a candidate's score is bounded exactly as the final score
#' is. Which of the two applies follows from the outcome, as it does for
#' `folds[i].metrics`.
#'
#' @return Named list: the `tuning` property schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tuning_schema <- function() {
  combo_id <- prop_integer(
    1L,
    min = 1L,
    description = "Candidate this row belongs to; joins the three tables."
  )
  # Anything a hyperparameter can be, once tuning has narrowed it to one value.
  scalar <- list(type = I(c("number", "string", "boolean", "null")))
  scores <- function(sample) {
    rows <- function(columns, required) {
      list(
        type = "array",
        minItems = 1L,
        items = members_schema(
          member_specs(c(list(param_combo_id = combo_id), columns), "columns"),
          c("param_combo_id", required)
        )
      )
    }
    list(
      description = paste0(
        "Each candidate's ",
        sample,
        " metrics, one row per candidate per inner resample."
      ),
      oneOf = list(
        rows(
          regression_metric_columns(),
          names(regression_metric_columns())
        ),
        rows(
          classification_overall_columns(),
          CLASSIFICATION_OVERALL_REQUIRED
        )
      )
    )
  }
  list(
    type = "object",
    description = paste(
      "What this fold's inner tuning searched and found: the candidate grid,",
      "the per-candidate metrics, and the winning combination."
    ),
    properties = list(
      param_grid = list(
        type = "array",
        minItems = 1L,
        description = paste(
          "The candidates searched, one row each. Columns beyond the id are",
          "the hyperparameters being tuned, so their names are data."
        ),
        items = list(
          type = "object",
          properties = list(param_combo_id = prop_to_schema(combo_id)),
          required = I("param_combo_id"),
          additionalProperties = scalar
        )
      ),
      training = scores("training"),
      validation = scores("validation"),
      best = list(
        type = "object",
        description = "The winning combination, keyed by hyperparameter.",
        additionalProperties = scalar
      )
    ),
    required = I(c("param_grid", "training", "validation", "best")),
    additionalProperties = FALSE
  )
} # /rtemis::tuning_schema


# %% metrics_schema ----
#' A record's headline-scores block
#'
#' One entry per sample, each a flat map of metric name to value: the mean
#' across folds, and beside it the standard deviation, which is null for a
#' single fit because one model has no dispersion.
#'
#' Typed loosely on purpose. The metric set differs between regression and
#' classification, and the authoritative, per-metric-bounded declaration is the
#' metrics schema each fold's `metrics` block `$ref`s. This block exists so that
#' "how did this run do?" is one lookup in a file, with no averaging and no R --
#' which is what makes a directory of records rankable.
#'
#' @param sd Logical: If TRUE, describe the dispersion block.
#'
#' @return Named list: the `metrics` (or `metrics_sd`) property schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
metrics_schema <- function(sd = FALSE) {
  sample_schema <- list(
    type = I(c("object", "null")),
    additionalProperties = list(type = I(c("number", "null")))
  )
  entries <- lapply(SUPERVISED_SAMPLES, function(...) sample_schema)
  names(entries) <- SUPERVISED_SAMPLES
  list(
    type = "object",
    description = if (sd) {
      paste(
        "Standard deviation of each metric across outer resamples, per sample.",
        "Null for a single fit, which has no dispersion."
      )
    } else {
      paste(
        "Headline score of each sample: the metric a run is judged on, averaged",
        "across outer resamples. Per-fold detail is in `folds`."
      )
    },
    properties = entries,
    additionalProperties = FALSE
  )
} # /rtemis::metrics_schema


# %% prop_to_schema ----
#' The JSON Schema for one S7 property
#'
#' Reads the two role axes off the property and hands them to
#' `spec_to_schema()`, so a leaf and a dispatcher's base field cannot annotate
#' the same declaration differently.
#'
#' @param prop S7 property (an element of `Class@properties`).
#'
#' @return Named list (JSON Schema property).
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_to_schema <- function(prop) {
  spec_to_schema(get_spec(prop), identical(prop_role(prop), "state"))
} # /rtemis::prop_to_schema


# %% S7_to_JSONSchema ----
#' Convert an S7 class built with `prop_*` factories to a JSON Schema
#'
#' Walks the class's properties, reads each attached `PropertySpec`, and
#' assembles a draft 2020-12 JSON Schema. Which properties take part is decided
#' by their declared role (see `prop_role()`), not by a list kept here:
#' `"config"` and `"state"` properties are generated from their spec, state
#' being marked `readOnly`, while `"computed"` and `"r_only"` properties are
#' omitted -- the first because everything it derives from is published, the
#' second because it has no wire form at all. A spec-less property with no role
#' is an error, so a class that drifts from the factory vocabulary fails loudly
#' instead of emitting a wrong schema.
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
#'   (matching [write_config]'s compaction). Ignored when `record` is TRUE.
#' @param fold_refs Named character or NULL: If set (and `record` is TRUE), adds
#'   a required `folds` array whose entries reference these record schemas. Only
#'   a record of a run that fits models per resample carries one.
#' @param metrics_refs Named character or NULL: If set (and `record` is TRUE),
#'   adds the required `metrics` / `metrics_sd` headline blocks, and references
#'   these metrics schemas from each fold's own `metrics`.
#' @param metrics_ref Character or NULL: If set (and `record` is TRUE), adds a
#'   required, nullable `metrics` property referencing that one schema. For a
#'   run whose result is a single metrics object rather than a per-sample map;
#'   mutually exclusive with `metrics_refs`.
#' @param provenance_url Character or NULL: If set (and `record` is TRUE), adds
#'   a required `provenance` property referencing that schema. Only a top-level
#'   record carries it; a nested one inherits its parent's.
#' @param record Logical: If TRUE, emit the **record** form of the schema: the
#'   same properties, but every one required. A record states what a run
#'   actually used, so nothing in it may fall back to a reader's defaults -- an
#'   unset value is written as an explicit `null` rather than omitted. The
#'   difference between an input schema and a record schema is exactly this;
#'   membership is identical.
#' @param extra Named list merged into the schema after generation, for
#'   cross-field constraints that are not per-property (e.g. an `allOf` of
#'   if/then clauses for kernel-specific SVM hyperparameters).
#' @param refs Named character: Properties holding a nested config object,
#'   mapped to the `$id` of the schema for that config. Each emits a `$ref` (or
#'   `oneOf: [null, $ref]` when the property accepts NULL, detected from its
#'   S7 union), instead of requiring a `PropertySpec`. Names must match
#'   existing properties.
#' @param array_refs Named character: As `refs`, for a property holding a *list*
#'   of such objects -- one metrics object per resample, one model per fold.
#'   Each emits an array whose `items` are the `$ref`.
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
  record = FALSE,
  provenance_url = NULL,
  fold_refs = NULL,
  metrics_refs = NULL,
  metrics_ref = NULL,
  extra = NULL,
  refs = NULL,
  array_refs = NULL,
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
  # Run state is part of the class, so it is part of the schema -- marked
  # `readOnly` by `prop_to_schema()`, since a user never supplies it. Whether it
  # is also written to a config is the separate `serialize` axis.
  for (arg in c("refs", "array_refs")) {
    named <- if (arg == "refs") refs else array_refs
    unknown <- setdiff(names(named), names(props))
    if (length(unknown) > 0L) {
      rtemis.core::abort(
        "`",
        arg,
        "` names no such (or omitted) propert",
        if (length(unknown) == 1L) "y: " else "ies: ",
        paste(unknown, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  referenced <- c(names(refs), names(array_refs))
  ref_props <- props[names(props) %in% referenced]
  props <- props[!names(props) %in% referenced]
  # A derived view is not part of the contract: it is a function of fields the
  # schema already declares, so publishing it would let the two disagree.
  props <- props[
    !vapply(
      props,
      function(p) prop_role(p) %in% c("computed", "r_only"),
      logical(1L)
    )
  ]
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
    function(nm) prop_to_schema(props[[nm]])
  )
  names(properties) <- names(props)
  # Nested config properties reference their own schema. A property whose S7
  # class is a union containing NULL is optional, so it also admits null.
  for (nm in names(ref_props)) {
    plural <- nm %in% names(array_refs)
    target <- unname(if (plural) array_refs[[nm]] else refs[[nm]])
    if (record) {
      # A record nests records: the input schemas are closed and do not declare
      # `origin`, so pointing at one would reject the very block it describes.
      target <- sub("/schema\\.json$", "/record.json", target)
    }
    ref <- list(`$ref` = target)
    if (plural) {
      ref <- list(type = "array", items = ref)
    }
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
  if (record) {
    # Every emitted property, `$schema` excluded: it identifies the document
    # rather than recording anything the run did. Constants are excluded too --
    # the algorithm implies them, `prop_serialized()` keeps them out of a
    # written record, and requiring what is never written would reject every
    # record rtemis produces.
    constants <- names(Filter(
      function(p) {
        spec <- get_spec(p)
        !is.null(spec) && spec@constant
      },
      props
    ))
    required <- setdiff(names(properties), c("$schema", constants))
    # A nested config carries its own `origin`, so it is not covered here.
    origin_props <- props[intersect(required, names(props))]
    origin_props <- origin_props[setdiff(names(origin_props), constants)]
    if (length(origin_props) > 0L) {
      properties[["origin"]] <- origin_schema(origin_props)
      required <- c(required, "origin")
    }
    # What produced the record, `$ref`d rather than restated in all 41 of them.
    # Nested records (a `preprocessor_config` inside a supervised record) get it
    # from their parent, so only a top-level record carries the block.
    if (!is.null(fold_refs)) {
      properties[["folds"]] <- folds_schema(fold_refs, metrics_refs)
      required <- c(required, "folds")
    }
    # What the run scored. A record that states the config and the provenance
    # but not the result cannot answer the question it is opened for.
    if (!is.null(metrics_refs)) {
      properties[["metrics"]] <- metrics_schema()
      properties[["metrics_sd"]] <- metrics_schema(sd = TRUE)
      required <- c(required, "metrics", "metrics_sd")
    }
    # A run that scores one metrics object rather than a map of samples: the
    # block is that object. Nullable, because a run can fail before scoring.
    if (!is.null(metrics_ref)) {
      properties[["metrics"]] <- list(
        description = "What the run scored.",
        oneOf = list(list(type = "null"), list(`$ref` = metrics_ref))
      )
      required <- c(required, "metrics")
    }
    if (!is.null(provenance_url)) {
      properties[["provenance"]] <- list(`$ref` = provenance_url)
      required <- c(required, "provenance")
    }
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


# %% base_schema_properties ----
#' JSON Schema properties for a family base class's shared fields
#'
#' The properties a family base class declares with the `prop_*` factories are
#' shared by every variant, so they are published on the dispatcher rather than
#' repeated on each leaf -- [S7_to_JSONSchema] subtracts them from the leaves
#' via its `base` argument. Spec-less base properties are class machinery (the
#' computed payload list, the discriminator, run state such as `tuned`) and have
#' no schema form, so they are skipped rather than erroring: unlike a leaf, a
#' base class is expected to carry them.
#'
#' @param base S7 class or NULL: The family base class. NULL yields no
#'   properties.
#' @param skip Character: Property names the dispatcher emits itself (the
#'   discriminator and the payload).
#'
#' @return Named list of JSON Schema properties, in declaration order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
base_schema_properties <- function(base, skip = character()) {
  if (is.null(base)) {
    return(list())
  }
  if (!inherits(base, "S7_class")) {
    rtemis.core::abort(
      "`base` must be an S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  props <- base@properties[setdiff(names(base@properties), skip)]
  props <- Filter(function(p) !is.null(get_spec(p)), props)
  out <- lapply(props, function(p) {
    prop_to_schema(p)
  })
  out
} # /rtemis::base_schema_properties


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
#' `.../<family>/<tolower(A)>/v1/schema.json` -- matching [S7_to_JSONSchema]'s
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
#' @param base Optional S7 class: The family base class. Its own
#'   `prop_*`-declared properties are shared by every variant, so they are
#'   emitted here rather than on any leaf (see Details).
#' @param title Optional Character: Schema title.
#' @param description Character: Schema description. If empty, omitted.
#' @param discriminator_description Character: Description of the
#'   discriminator property.
#' @param record Logical: If TRUE, dispatch to the variants' **record**
#'   schemas (`<family>/<variant>/v1/record.json`) rather than their input
#'   schemas. The discriminator and payload are required either way; what
#'   changes is which leaf each `if/then` branch applies.
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
#' `base` closes the loop with [S7_to_JSONSchema]'s `base` argument, which
#' *subtracts* the family base's properties from every leaf: the dispatcher
#' adds them back at the top level, from the same `PropertySpec`, so the shared
#' fields are declared once. Base properties carrying no spec are class
#' machinery (the computed payload list, run state) and are skipped, as is the
#' discriminator, which is generated from the variant enum.
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
  base = NULL,
  title = NULL,
  description = "",
  discriminator_description = "Algorithm name.",
  record = FALSE,
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
  leaf_file <- if (record) "record.json" else "schema.json"
  family_base <- sub("/v1/(schema|record)\\.json$", "", id)
  leaf_id <- function(variant) {
    paste0(family_base, "/", tolower(variant), "/v1/", leaf_file)
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
  # Generated from a spec like every other property, so it carries the same
  # `x-rtemis` annotation. The default is required by the factory but never
  # reaches the schema (no `default` keyword is emitted); the first variant is
  # the one value guaranteed to satisfy the enum.
  properties[[discriminator]] <- spec_to_schema(get_spec(prop_string(
    variants[[1L]],
    enum = variants,
    description = discriminator_description
  )))
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
  properties <- c(
    properties,
    base_schema_properties(base, skip = c(discriminator, payload))
  )
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
