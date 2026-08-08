# test_Props_read.R
# ::rtemis::
# 2026- EDG rtemis.org

# The acceptance test for the whole props/schema layer: every class that
# declares properties with the `prop_*` factories is emitted as a JSON Schema,
# read back with `JSONSchema_to_S7()`, and compared spec by spec. A declaration
# axis that the schema cannot carry shows up here as a mismatch.

spec_of <- function(cls, nm) rtemis:::get_spec(cls@properties[[nm]])


# Every S7 class in the namespace with at least one spec'd property, and no
# property lacking a role (which is drift that `S7_to_JSONSchema()` rejects
# outright). Discovered rather than listed, so a new class is covered without
# editing this file.
schema_classes <- function() {
  ns <- asNamespace("rtemis")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE)
  objs <- Filter(function(o) inherits(o, "S7_class"), objs)
  Filter(
    function(o) {
      own <- own_prop_names_for(o)
      length(own) > 0L &&
        any(vapply(
          o@properties[own],
          function(p) !is.null(rtemis:::get_spec(p)),
          logical(1L)
        )) &&
        all(
          !is.na(vapply(o@properties[own], rtemis:::prop_role, character(1L)))
        )
    },
    objs
  )
}


# Properties the class declares itself: a family leaf's inherited machinery is
# excluded from its schema by the `base` argument, so it is excluded here too.
is_base_parent <- function(cls) {
  identical(cls@parent@name, "S7_object")
}


own_prop_names_for <- function(cls) {
  if (is_base_parent(cls)) {
    return(names(cls@properties))
  }
  setdiff(names(cls@properties), names(cls@parent@properties))
}


base_of <- function(cls) {
  if (is_base_parent(cls)) NULL else cls@parent
}


# Defaults reach a reader through the published artifact, i.e. through JSON.
# Round-tripping them here exercises the coercion that restores what JSON drops
# (integer vs double, most importantly).
via_json <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA),
    simplifyVector = TRUE
  )
}


# %% JSONSchema_to_S7() round-trips every declared class ----
test_that("every spec'd class round-trips through JSON Schema unchanged", {
  classes <- schema_classes()
  expect_gt(length(classes), 20L)

  for (cls_name in names(classes)) {
    cls <- classes[[cls_name]]
    own <- own_prop_names_for(cls)
    schema <- S7_to_JSONSchema(
      cls,
      id = paste0(
        "https://schema.rtemis.org/test/",
        cls_name,
        "/v1/schema.json"
      ),
      base = base_of(cls)
    )
    # The schema is what a consumer actually receives: JSON, not an R list.
    schema <- jsonlite::fromJSON(
      jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null", digits = NA),
      simplifyVector = FALSE
    )
    specs <- Filter(
      Negate(is.null),
      lapply(cls@properties[own], rtemis:::get_spec)
    )
    defaults <- lapply(specs, function(s) via_json(s@default))
    rt <- JSONSchema_to_S7(schema, defaults = defaults, name = cls_name)

    expect_setequal(names(rt@properties), names(specs))
    for (nm in names(specs)) {
      original <- specs[[nm]]
      restored <- spec_of(rt, nm)
      label <- paste0(cls_name, "@", nm)
      expect_equal(restored@type, original@type, label = label)
      expect_equal(restored@minimum, original@minimum, label = label)
      expect_equal(restored@maximum, original@maximum, label = label)
      expect_equal(
        restored@exclusive_minimum,
        original@exclusive_minimum,
        label = label
      )
      expect_equal(
        restored@exclusive_maximum,
        original@exclusive_maximum,
        label = label
      )
      expect_equal(restored@enum, original@enum, label = label)
      expect_equal(restored@nullable, original@nullable, label = label)
      expect_equal(restored@tunable, original@tunable, label = label)
      expect_equal(restored@container, original@container, label = label)
      expect_equal(restored@broadcast, original@broadcast, label = label)
      expect_equal(restored@min_items, original@min_items, label = label)
      expect_equal(restored@unique_items, original@unique_items, label = label)
      expect_equal(restored@constant, original@constant, label = label)
      expect_equal(restored@tune_on_null, original@tune_on_null, label = label)
      expect_equal(
        restored@default_on_null,
        original@default_on_null,
        label = label
      )
      expect_equal(restored@data_bound, original@data_bound, label = label)
      expect_equal(
        restored@data_dependent,
        original@data_dependent,
        label = label
      )
      expect_equal(
        restored@applies_when,
        original@applies_when,
        label = label
      )
      expect_equal(restored@description, original@description, label = label)
      # `@items` is a nested spec; compare the axes that make it one.
      if (is.null(original@items)) {
        expect_null(restored@items, label = label)
      } else {
        expect_equal(restored@items@type, original@items@type, label = label)
        expect_equal(
          restored@items@container,
          original@items@container,
          label = label
        )
      }
    }
  }
})


# %% the restored class validates like the original ----
test_that("a restored class enforces the same constraints", {
  schema <- S7_to_JSONSchema(
    rtemis:::CARTHyperparameters,
    id = "https://schema.rtemis.org/test/cart/v1/schema.json",
    base = rtemis:::Hyperparameters
  )
  specs <- Filter(
    Negate(is.null),
    lapply(
      rtemis:::CARTHyperparameters@properties[own_prop_names_for(
        rtemis:::CARTHyperparameters
      )],
      rtemis:::get_spec
    )
  )
  cls <- JSONSchema_to_S7(
    schema,
    defaults = lapply(specs, function(s) s@default),
    name = "CARTRestored"
  )
  # `maxdepth` is bounded [1, 30] in the declaration; the restored class must
  # reject the same values, or the schema did not carry the constraint.
  expect_error(cls(maxdepth = 0L))
  expect_error(cls(maxdepth = 99L))
  expect_no_error(cls(maxdepth = 10L))
  # Type is enforced too: JSON's single number type is not an excuse.
  expect_error(cls(maxdepth = "ten"))
})


# %% defaults are required, not invented ----
test_that("JSONSchema_to_S7() names non-nullable properties with no default", {
  schema <- S7_to_JSONSchema(
    rtemis:::CARTHyperparameters,
    id = "https://schema.rtemis.org/test/cart/v1/schema.json",
    base = rtemis:::Hyperparameters
  )
  expect_error(
    JSONSchema_to_S7(schema),
    class = "rtemis_value_error"
  )
})


# %% array arity survives the round trip ----
# `DecompositionConfig` is the only class declaring `min_items`/`unique_items`,
# and it is not reached by the discovery above (its own `algorithm` property
# carries no role), so its axes are checked directly. They travel as standard
# keywords, which for a broadcast property sit on the trailing `oneOf` branch
# rather than at the top level.
test_that("min_items and unique_items round-trip", {
  original <- rtemis:::get_spec(
    rtemis:::DecompositionConfig@properties[["features"]]
  )
  restored <- rtemis:::schema_to_spec(
    rtemis:::spec_to_schema(original),
    default = NULL
  )
  expect_identical(restored@min_items, 2L)
  expect_true(restored@unique_items)
  expect_identical(restored@container, "array")

  broadcast <- rtemis:::get_spec(prop_float(
    NULL,
    min = 0,
    nullable = TRUE,
    vector = TRUE,
    broadcast = TRUE,
    unique_items = TRUE
  ))
  rt <- rtemis:::schema_to_spec(
    rtemis:::spec_to_schema(broadcast),
    default = NULL
  )
  expect_true(rt@broadcast)
  expect_true(rt@unique_items)
  expect_identical(rt@min_items, 1L)
})


# %% a hand-written schema without x-rtemis is rejected ----
test_that("schema_to_spec() rejects a property with no x-rtemis", {
  expect_error(
    rtemis:::schema_to_spec(list(type = "integer", minimum = 1L)),
    class = "rtemis_value_error"
  )
})


# %% $ref properties need their class ----
test_that("JSONSchema_to_S7() names unresolved `$ref` properties", {
  schema <- list(
    title = "WithRef",
    properties = list(
      nested = list(
        oneOf = list(
          list(type = "null"),
          list(`$ref` = "https://schema.rtemis.org/execution/v1/schema.json")
        )
      )
    )
  )
  expect_error(JSONSchema_to_S7(schema), class = "rtemis_value_error")

  # A nested config resolves to a class the reader built from its own schema,
  # which is how a port composes a whole config tree.
  exec_schema <- S7_to_JSONSchema(
    rtemis:::ExecutionConfig,
    id = "https://schema.rtemis.org/execution/v1/schema.json"
  )
  exec_specs <- Filter(
    Negate(is.null),
    lapply(rtemis:::ExecutionConfig@properties, rtemis:::get_spec)
  )
  exec <- JSONSchema_to_S7(
    exec_schema,
    defaults = lapply(exec_specs, function(s) s@default),
    name = "ExecutionRestored"
  )
  cls <- JSONSchema_to_S7(schema, refs = list(nested = exec))
  expect_true(inherits(cls, "S7_class"))
  expect_null(cls()@nested)
  expect_true(inherits(cls(nested = exec())@nested, "ExecutionRestored"))
})
