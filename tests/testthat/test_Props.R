# test_Props.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# %% Pilot class ----
# LightRF hyperparameters declared with the prop_* factories: types, defaults,
# bounds, tunability, and descriptions in ONE place, from which both the S7
# validators and the JSON Schema are generated. Defined here (not in R/) until
# the migration of 070_Hyperparameters.R.
LightRFProps <- S7::new_class(
  name = "LightRFProps",
  package = NULL,
  properties = list(
    nrounds = prop_integer(
      500L,
      min = 1L,
      tunable = TRUE,
      description = "Number of boosting rounds (trees)."
    ),
    num_leaves = prop_integer(
      4096L,
      min = 1L,
      tunable = TRUE,
      description = "Maximum number of leaves per tree."
    ),
    max_depth = prop_integer(
      -1L,
      tunable = TRUE,
      description = "Maximum tree depth. -1 = no limit."
    ),
    feature_fraction = prop_float(
      0.7,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of features sampled per tree."
    ),
    subsample = prop_float(
      0.623,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of cases sampled per tree (bagging fraction)."
    ),
    lambda_l1 = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "L1 regularization."
    ),
    lambda_l2 = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "L2 regularization."
    ),
    max_cat_threshold = prop_integer(
      32L,
      min = 1L,
      tunable = TRUE,
      description = "Maximum number of split points for categorical features."
    ),
    min_data_per_group = prop_integer(
      32L,
      min = 1L,
      tunable = TRUE,
      description = "Minimum number of cases per categorical group."
    ),
    linear_tree = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Fit linear models at leaves."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse frequency weighting of outcome classes."
    ),
    objective = prop_string(
      NULL,
      nullable = TRUE,
      description = "LightGBM objective. NULL = set from outcome type."
    ),
    device_type = prop_string(
      "cpu",
      enum = c("cpu", "gpu", "cuda"),
      description = "Compute device."
    ),
    tree_learner = prop_string(
      "serial",
      enum = c("serial", "feature", "data", "voting"),
      description = "Tree learner type."
    ),
    force_col_wise = prop_boolean(
      TRUE,
      description = "Force column-wise histogram building."
    )
  )
)

# %% Defaults ----
testthat::test_that("factory defaults populate a bare instance", {
  x <- LightRFProps()
  testthat::expect_identical(x@nrounds, 500L)
  testthat::expect_identical(x@num_leaves, 4096L)
  testthat::expect_identical(x@feature_fraction, 0.7)
  # Nullable prop without default reads as NULL: factories declare nullable
  # props as `NULL | <base>`, so S7 prototypes them to NULL rather than to the
  # base class's empty vector (see validate_with_spec).
  testthat::expect_null(x@objective)
  testthat::expect_identical(x@device_type, "cpu")
  testthat::expect_true(x@force_col_wise)
})

# %% Validation ----
testthat::test_that("spec-generated validators enforce bounds, enum, arity", {
  # Bounds.
  testthat::expect_error(LightRFProps(nrounds = 0L))
  testthat::expect_error(LightRFProps(feature_fraction = 0)) # exclusive min
  testthat::expect_error(LightRFProps(feature_fraction = 1.1))
  testthat::expect_no_error(LightRFProps(feature_fraction = 1)) # inclusive max
  testthat::expect_error(LightRFProps(lambda_l1 = -0.1))
  # Enum.
  testthat::expect_error(LightRFProps(device_type = "tpu"))
  testthat::expect_no_error(LightRFProps(device_type = "gpu"))
  # Type (enforced by the property's S7 class).
  testthat::expect_error(LightRFProps(nrounds = "many"))
  # NA.
  testthat::expect_error(LightRFProps(subsample = NA_real_))
  # Nullability.
  testthat::expect_no_error(LightRFProps(objective = NULL))
  testthat::expect_error(LightRFProps(device_type = NULL))
})

testthat::test_that("NULL is the only unset value; zero-length is rejected", {
  # A nullable prop is NULL whether it was defaulted or set explicitly, and an
  # empty vector is a real (invalid) value rather than a second spelling of
  # "unset" — this is what keeps `!is.null()` guards meaningful downstream.
  testthat::expect_null(LightRFProps()@objective)
  testthat::expect_null(LightRFProps(objective = NULL)@objective)
  testthat::expect_error(
    LightRFProps(objective = character(0)),
    "must not be empty"
  )
  testthat::expect_error(
    LightRFProps(nrounds = integer(0)),
    "must not be empty"
  )
  # The empty-value message may only point at NULL where NULL is accepted.
  err <- testthat::expect_error(LightRFProps(objective = character(0)))
  testthat::expect_match(conditionMessage(err), "use NULL", fixed = TRUE)
  err <- testthat::expect_error(LightRFProps(nrounds = integer(0)))
  testthat::expect_no_match(conditionMessage(err), "use NULL", fixed = TRUE)
  # A non-nullable prop's class is the bare base class rather than a union
  # with NULL, so S7's own type check rejects NULL before the spec validator
  # is reached. (The validator's NULL branch guards the factory-time check of
  # a spec's own default; see "bad defaults fail at factory time" below.)
  testthat::expect_error(LightRFProps(nrounds = NULL), "not <NULL>")
})

testthat::test_that("tunable properties accept search vectors; fixed do not", {
  x <- LightRFProps(num_leaves = c(1024L, 4096L, 16384L))
  testthat::expect_length(x@num_leaves, 3L)
  # Every search value is bounds-checked.
  testthat::expect_error(LightRFProps(num_leaves = c(1024L, 0L)))
  # Fixed hyperparameters reject vectors.
  testthat::expect_error(LightRFProps(device_type = c("cpu", "gpu")))
  testthat::expect_error(LightRFProps(force_col_wise = c(TRUE, FALSE)))
})

# %% Factory-time default validation ----
testthat::test_that("bad defaults fail at factory time, not first instantiation", {
  # Default below its own bound.
  testthat::expect_error(prop_integer(0L, min = 1L), "default")
  # Double default for an integer property.
  testthat::expect_error(prop_integer(500, min = 1L), "default")
  # Default outside its own enum.
  testthat::expect_error(prop_string("x", enum = c("a", "b")), "default")
  # Default above inclusive max.
  testthat::expect_error(prop_float(1.5, max = 1), "default")
  # NULL default requires nullable.
  testthat::expect_error(prop_string(NULL, nullable = FALSE), "default")
  testthat::expect_no_error(prop_string(NULL, nullable = TRUE))
})

# %% Vector-valued props ----
# %% Arity axes: container / items / broadcast ----
# Hand-built specs for shapes the factories do not yet expose (nested items,
# maps, broadcast). `default` is required whenever `nullable` is FALSE, so the
# helper supplies one.
mk_spec <- function(...) {
  args <- list(
    type = "number",
    default = 0,
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
    data_dependent = FALSE,
    description = ""
  )
  do.call(PropertySpec, utils::modifyList(args, list(...)))
}


testthat::test_that("`vector = TRUE` is sugar for container 'array'", {
  spec <- get_spec(prop_float(NULL, min = 0, nullable = TRUE, vector = TRUE))
  testthat::expect_identical(spec@container, "array")
  testthat::expect_null(spec@items)
  testthat::expect_false(spec@broadcast)
  testthat::expect_identical(get_spec(prop_float(1))@container, "none")
})


testthat::test_that("a nested `items` spec produces a nested array schema", {
  # A matrix is an array whose items are an array -- a shape `vector = TRUE`
  # alone cannot express.
  s <- spec_to_schema(mk_spec(
    container = "array",
    items = mk_spec(container = "array", default = c(0, 1)),
    default = list(c(0, 1))
  ))
  testthat::expect_identical(s[["type"]], "array")
  testthat::expect_identical(s[["items"]][["type"]], "array")
  testthat::expect_identical(s[["items"]][["items"]][["type"]], "number")
})


testthat::test_that("container 'map' produces additionalProperties", {
  s <- spec_to_schema(mk_spec(
    container = "map",
    items = mk_spec(),
    default = c(a = 0)
  ))
  testthat::expect_identical(s[["type"]], "object")
  testthat::expect_identical(s[["additionalProperties"]][["type"]], "number")
})


testthat::test_that("broadcast emits scalar-or-array, distinct from tunable", {
  s <- spec_to_schema(mk_spec(container = "array", broadcast = TRUE))
  testthat::expect_length(s[["oneOf"]], 2L)
  testthat::expect_identical(s[["oneOf"]][[1L]][["type"]], "number")
  testthat::expect_identical(s[["oneOf"]][[2L]][["type"]], "array")
  # Structurally the same shape as a tunable oneOf, semantically unrelated:
  # one broadcasts a value, the other declares a search space. Only the
  # declaration tells them apart -- which is why `x-rtemis` must carry it.
  tun <- spec_to_schema(get_spec(prop_float(1, tunable = TRUE)))
  testthat::expect_length(tun[["oneOf"]], 2L)
})


testthat::test_that("the arity axes are validated against each other", {
  testthat::expect_error(mk_spec(container = "bogus"), "container")
  # A container holds values; a tunable array holds search values.
  testthat::expect_error(
    mk_spec(container = "array", tunable = TRUE),
    "tunable"
  )
  # `items` is meaningless without a container, and required for a map.
  testthat::expect_error(mk_spec(items = mk_spec()), "items")
  testthat::expect_error(mk_spec(container = "map"), "items")
  testthat::expect_error(
    mk_spec(container = "array", items = 1),
    "PropertySpec"
  )
  # Nothing to broadcast into.
  testthat::expect_error(mk_spec(broadcast = TRUE), "broadcast")
})


testthat::test_that("vector props accept vectors, map to array schemas", {
  Vec <- S7::new_class(
    name = "Vec",
    package = NULL,
    properties = list(
      w = prop_float(
        NULL,
        min = 0,
        nullable = TRUE,
        vector = TRUE,
        description = "Per-feature weights."
      ),
      nm = prop_string(NULL, nullable = TRUE, vector = TRUE)
    )
  )
  x <- Vec(w = c(1, 2, 0.5), nm = c("a", "b"))
  testthat::expect_length(x@w, 3L)
  # Element-wise bounds still apply.
  testthat::expect_error(Vec(w = c(1, -1)))
  # NULL allowed when nullable.
  testthat::expect_no_error(Vec())
  # vector and tunable are mutually exclusive at factory time.
  testthat::expect_error(prop_float(1, vector = TRUE, tunable = TRUE))
  # Schema: array type (with null), items carry the scalar constraints.
  s <- spec_to_schema(get_spec(Vec@properties[["w"]]))
  testthat::expect_identical(as.character(s[["type"]]), c("array", "null"))
  testthat::expect_identical(s[["items"]][["type"]], "number")
  testthat::expect_identical(s[["items"]][["minimum"]], 0)
  # Defaults are published separately, so no schema carries the keyword.
  s1 <- spec_to_schema(get_spec(prop_string("a", vector = TRUE)))
  testthat::expect_false("default" %in% names(s1))
})

# %% Array arity: min_items / unique_items ----
testthat::test_that("min_items and unique_items are enforced and published", {
  p <- prop_string(
    NULL,
    nullable = TRUE,
    vector = TRUE,
    min_items = 2L,
    unique_items = TRUE
  )
  Sel <- S7::new_class("Sel", properties = list(cols = p))
  testthat::expect_error(Sel(cols = "a"), "at least 2 elements, but 1 given")
  testthat::expect_error(Sel(cols = c("a", "a")), "duplicate values")
  testthat::expect_no_error(Sel(cols = c("a", "b")))
  # NULL is still unset, not an empty selection.
  testthat::expect_no_error(Sel())

  s <- spec_to_schema(get_spec(p))
  testthat::expect_identical(s[["minItems"]], 2L)
  testthat::expect_true(s[["uniqueItems"]])
  # Standard keywords carry both, so neither is duplicated into `x-rtemis`.
  testthat::expect_false("minItems" %in% names(s[["x-rtemis"]]))
  testthat::expect_false("uniqueItems" %in% names(s[["x-rtemis"]]))
})


testthat::test_that("the default arity is published as before", {
  s <- spec_to_schema(get_spec(prop_float(
    NULL,
    nullable = TRUE,
    vector = TRUE
  )))
  testthat::expect_identical(s[["minItems"]], 1L)
  testthat::expect_false("uniqueItems" %in% names(s))
})


testthat::test_that("array arity is rejected on non-array containers", {
  # A scalar, a map and a matrix have no `minItems`/`uniqueItems` form, so a
  # non-default value there would publish a constraint nothing enforces.
  testthat::expect_error(prop_float(1, min_items = 2L), "only meaningful")
  testthat::expect_error(
    prop_string("a", unique_items = TRUE),
    "only meaningful"
  )
  testthat::expect_error(
    mk_spec(container = "map", items = mk_spec(), min_items = 2L),
    "only meaningful"
  )
  testthat::expect_error(
    mk_spec(container = "matrix", default = matrix(0), unique_items = TRUE),
    "only meaningful"
  )
  # A broadcast scalar stands in for the whole array, so it cannot also be
  # required to hold several elements.
  testthat::expect_error(
    prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      broadcast = TRUE,
      min_items = 2L
    ),
    "contradictory"
  )
  testthat::expect_error(prop_float(1, min_items = 0L), "single value >= 1")
})


# %% Spec introspection ----
testthat::test_that("specs ride along on properties and derive tunability", {
  props <- LightRFProps@properties
  spec <- get_spec(props[["num_leaves"]])
  testthat::expect_s7_class(spec, PropertySpec)
  testthat::expect_identical(spec@type, "integer")
  testthat::expect_identical(spec@minimum, 1L)
  testthat::expect_true(spec@tunable)
  # tunable/fixed vectors are now derivable — no hand-maintained constants.
  tunable <- names(Filter(function(p) get_spec(p)@tunable, props))
  testthat::expect_true(all(
    c("nrounds", "num_leaves", "feature_fraction", "ifw") %in% tunable
  ))
  testthat::expect_false(any(
    c("objective", "device_type", "tree_learner", "force_col_wise") %in% tunable
  ))
})

# %% spec_to_schema ----
testthat::test_that("spec_to_schema maps bounds, nullability, tunability", {
  props <- LightRFProps@properties
  # Tunable numeric with exclusive bound -> oneOf [scalar, array].
  s <- spec_to_schema(get_spec(props[["feature_fraction"]]))
  testthat::expect_named(s, c("oneOf", "description", "x-rtemis"))
  testthat::expect_identical(s[["oneOf"]][[1L]][["type"]], "number")
  testthat::expect_identical(s[["oneOf"]][[1L]][["exclusiveMinimum"]], 0)
  testthat::expect_identical(s[["oneOf"]][[1L]][["maximum"]], 1)
  testthat::expect_identical(s[["oneOf"]][[2L]][["type"]], "array")
  testthat::expect_identical(s[["oneOf"]][[2L]][["minItems"]], 1L)
  # Fixed enum string -> flat schema with enum.
  s <- spec_to_schema(get_spec(props[["device_type"]]))
  testthat::expect_identical(s[["type"]], "string")
  testthat::expect_identical(as.character(s[["enum"]]), c("cpu", "gpu", "cuda"))
  # Nullable fixed string -> type union with null.
  s <- spec_to_schema(get_spec(props[["objective"]]))
  testthat::expect_identical(as.character(s[["type"]]), c("string", "null"))
})

# %% S7_to_JSONSchema ----
schema <- S7_to_JSONSchema(
  LightRFProps,
  id = "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json",
  title = "rtemis LightRFHyperparameters",
  description = "Hyperparameters for the LightRF algorithm (LightGBM random forest mode).",
  instance_schema_url = "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json"
)

testthat::test_that("S7_to_JSONSchema assembles a complete schema", {
  testthat::expect_identical(
    schema[["$schema"]],
    "https://json-schema.org/draft/2020-12/schema"
  )
  testthat::expect_false(schema[["additionalProperties"]])
  # $schema const + all 15 declared properties.
  testthat::expect_length(schema[["properties"]], 16L)
  testthat::expect_identical(
    schema[["properties"]][["$schema"]][["const"]],
    "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json"
  )
  testthat::expect_false(
    "default" %in% names(schema[["properties"]][["nrounds"]])
  )
})

testthat::test_that("empty schema description is omitted", {
  Tiny <- S7::new_class(
    name = "Tiny",
    package = NULL,
    properties = list(a = prop_boolean(TRUE))
  )
  s <- S7_to_JSONSchema(Tiny, id = "https://example.org/tiny.json")
  testthat::expect_false("description" %in% names(s))
  s <- S7_to_JSONSchema(
    Tiny,
    id = "https://example.org/tiny.json",
    description = "A tiny config."
  )
  testthat::expect_identical(s[["description"]], "A tiny config.")
})

testthat::test_that("a property with no declared role is an error", {
  Mixed <- S7::new_class(
    name = "Mixed",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = S7::class_integer
    )
  )
  testthat::expect_error(
    S7_to_JSONSchema(Mixed, id = "https://example.org/x.json"),
    "no declared role"
  )
  # `prop_state()` keeps it in the contract, marked readOnly: the class has
  # the field, but a user never supplies it.
  Stated <- S7::new_class(
    name = "Stated",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_state(prop_integer(0L, min = 0L))
    )
  )
  s <- S7_to_JSONSchema(Stated, id = "https://example.org/x.json")
  testthat::expect_identical(names(s[["properties"]]), c("a", "b"))
  testthat::expect_null(s[["properties"]][["a"]][["readOnly"]])
  testthat::expect_true(s[["properties"]][["b"]][["readOnly"]])
})

testthat::test_that("an open-object property generates without `extra`", {
  Bag <- S7::new_class(
    name = "Bag",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_bag(description = "Backend parameters.")
    )
  )
  s <- S7_to_JSONSchema(Bag, id = "https://example.org/x.json")
  testthat::expect_setequal(names(s[["properties"]]), c("a", "b"))
  testthat::expect_identical(s[["properties"]][["b"]][["type"]], "object")
})

testthat::test_that("prop_role classifies each declaration style", {
  Roles <- S7::new_class(
    name = "Roles",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_bag(),
      c = prop_float(0, vector = TRUE, data_dependent = TRUE),
      d = prop_state(prop_integer(0L)),
      e = S7::class_integer
    )
  )
  testthat::expect_identical(role_prop_names(Roles, "config"), c("a", "b", "c"))
  testthat::expect_identical(role_prop_names(Roles, "state"), "d")
  # Data-dependence is orthogonal to role: `c` is a declared config input whose
  # value is tied to one dataset, so it is not written to a portable config.
  testthat::expect_identical(data_dependent_prop_names(Roles), "c")
  testthat::expect_true(is.na(prop_role(Roles@properties[["e"]])))
})

# %% What is written to a config ----
testthat::test_that("everything settable is written; state is not", {
  Axes <- S7::new_class(
    name = "Axes",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      # Data-shaped but still a settable input, so it is written: dropping a
      # value the user supplied would lose it silently.
      b = prop_float(0, vector = TRUE, data_dependent = TRUE),
      # Written by the run and carried by the fitted model, so re-derived
      # on read rather than stored.
      c = prop_state(prop_integer(0L)),
      # Determined by the class, so the algorithm already implies it.
      d = prop_const(1L)
    )
  )
  p <- Axes@properties
  testthat::expect_true(prop_serialized(p[["a"]]))
  testthat::expect_true(prop_serialized(p[["b"]]))
  testthat::expect_false(prop_serialized(p[["c"]]))
  testthat::expect_false(prop_serialized(p[["d"]]))
  # `data_dependent` is a pure annotation: it marks a field a form should not
  # prompt for, and says nothing about serialization.
  s <- S7_to_JSONSchema(Axes, id = "https://example.org/x.json")
  testthat::expect_true(s[["properties"]][["b"]][["x-rtemis"]][[
    "data_dependent"
  ]])
  testthat::expect_null(s[["properties"]][["b"]][["readOnly"]])
  testthat::expect_true(s[["properties"]][["c"]][["readOnly"]])
})

testthat::test_that("a flat config answers the same question as a family", {
  # `PreprocessorConfig` has no `serializable_props` method of its own, so the
  # default must apply the same rule rather than emitting whatever it holds.
  cfg <- setup_Preprocessor(scale = TRUE, center = TRUE)
  cfg@scale_centers <- c(a = 1.5, b = 2.5)
  out <- serializable_props(cfg)
  # `scale_centers` is a settable input `preprocess()` uses in place of
  # computing, so it round-trips.
  testthat::expect_true("scale_centers" %in% names(out))
  testthat::expect_true("center" %in% names(out))
})

testthat::test_that("state the model carries is not written to a config", {
  h <- setup_GLMNET()
  h@`lambda.min` <- 0.03
  testthat::expect_false(
    "lambda.min" %in% names(serializable_props(h)[["hyperparameters"]])
  )
  # A data-shaped value the user supplied *is* written: it is an input, and
  # `id_strat` decides which cases stay together, so losing it changes results.
  r <- setup_Resampler(
    type = "StratSub",
    n_resamples = 2L,
    id_strat = c("a", "b", "a")
  )
  testthat::expect_identical(
    serializable_props(r)[["id_strat"]],
    c("a", "b", "a")
  )
})

# %% JSON round-trip ----
testthat::test_that("schema serializes to JSON and round-trips", {
  testthat::skip_if_not_installed("jsonlite")
  tmpfile <- file.path(tempdir(), "lightrf.schema.json")
  write_JSONSchema(schema, tmpfile, overwrite = TRUE, verbosity = 0L)
  parsed <- jsonlite::fromJSON(tmpfile, simplifyVector = FALSE)
  testthat::expect_identical(
    parsed[["$id"]],
    "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json"
  )
  # enum stays an array even though scalar-unboxing is on.
  testthat::expect_identical(
    unlist(parsed[["properties"]][["device_type"]][["enum"]]),
    c("cpu", "gpu", "cuda")
  )
  # tunable property round-trips as oneOf with an array branch.
  ff <- parsed[["properties"]][["feature_fraction"]]
  testthat::expect_length(ff[["oneOf"]], 2L)
  testthat::expect_identical(ff[["oneOf"]][[2L]][["type"]], "array")
  # nullable objective: type union survives.
  testthat::expect_identical(
    unlist(parsed[["properties"]][["objective"]][["type"]]),
    c("string", "null")
  )
})


# %% data_bound ----------------------------------------------------------------
# `data_bound` declares that a value's valid range depends on the training data.
# It is deliberately NOT enforced at construction time - there is no data yet -
# but by check_data_bounds() via validate_hyperparameters(), which train() calls
# before tuning. See DATA_BOUNDS in 010_Props.R.

test_that("data_bound rejects names outside the vocabulary", {
  expect_error(prop_integer(1L, data_bound = "n_bananas"))
  expect_error(prop_float(1, data_bound = "n_bananas"))
})

test_that("feature_names is restricted to string properties", {
  # A length bound applies to any type: a character vector of per-case IDs is
  # bound by "n_cases".
  expect_no_error(prop_string("a", vector = TRUE, data_bound = "n_cases"))
  expect_error(prop_integer(1L, data_bound = "feature_names"))
})

test_that("data_bound does not constrain construction", {
  # 100 is nonsense for any real dataset, but there is no dataset here.
  expect_s7_class(setup_Ranger(mtry = 100L), RangerHyperparameters)
  expect_length(setup_CART(cost = c(1, 2))[["cost"]], 2L)
})

test_that("data_bound is surfaced in the generated schema description", {
  prop_schema <- function(cls, nm) {
    spec_to_schema(get_spec(cls@properties[[nm]]))
  }
  expect_match(
    prop_schema(RangerHyperparameters, "mtry")[["description"]],
    "Cannot exceed the number of features"
  )
  expect_match(
    prop_schema(RangerHyperparameters, "case_weights")[["description"]],
    "Must have one value per case"
  )
  expect_match(
    prop_schema(RangerHyperparameters, "always_split_variables")[[
      "description"
    ]],
    "must name training features"
  )
})

# check_data_bounds ----
n_cd <- 40L
datc_bounds <- data.frame(
  a = rnorm(n_cd),
  b = rnorm(n_cd),
  y = factor(sample(c("x", "z"), n_cd, replace = TRUE))
)
datr_bounds <- data.frame(a = rnorm(n_cd), b = rnorm(n_cd), y = rnorm(n_cd))

test_that("check_data_bounds() bounds a scalar above by the dimension", {
  expect_error(
    check_data_bounds(setup_Ranger(mtry = 100L), datr_bounds),
    class = "rtemis_range_error"
  )
  # Tunable hyperparameters carry the whole search space at this point, so a
  # single bad value anywhere in it must abort.
  expect_error(
    check_data_bounds(setup_Ranger(mtry = c(1L, 100L)), datr_bounds),
    class = "rtemis_range_error"
  )
  expect_invisible(check_data_bounds(setup_Ranger(mtry = 2L), datr_bounds))
})

test_that("check_data_bounds() requires vector properties to match the dimension", {
  expect_error(
    check_data_bounds(setup_CART(cost = c(1, 2, 3)), datr_bounds),
    class = "rtemis_length_error"
  )
  expect_invisible(check_data_bounds(setup_CART(cost = c(1, 2)), datr_bounds))
})

test_that("check_data_bounds() checks feature_names by membership", {
  expect_error(
    check_data_bounds(
      setup_Ranger(always_split_variables = c("a", "nope")),
      datr_bounds
    ),
    class = "rtemis_value_error"
  )
  expect_invisible(
    check_data_bounds(setup_Ranger(always_split_variables = "a"), datr_bounds)
  )
})

test_that("numeric_feature_names excludes non-numeric features", {
  # `features` is declared on DecompositionConfig, so this also exercises a
  # non-Hyperparameters config and the `prop()` accessor: a family's `[[`
  # routes into its computed payload list, which excludes base properties.
  feat <- data.frame(a = rnorm(6L), b = rnorm(6L), f = factor(letters[1:6]))
  expect_error(
    check_data_bounds(
      setup_PCA(k = 2L, features = c("a", "f")),
      feat,
      has_outcome = FALSE
    ),
    "must name numeric training features"
  )
  expect_error(
    check_data_bounds(
      setup_PCA(k = 2L, features = c("a", "nope")),
      feat,
      has_outcome = FALSE
    ),
    class = "rtemis_value_error"
  )
  expect_invisible(
    check_data_bounds(
      setup_PCA(k = 2L, features = c("a", "b")),
      feat,
      has_outcome = FALSE
    )
  )
  # Unset is skipped: NULL means "all numeric features", resolved later.
  expect_invisible(
    check_data_bounds(setup_PCA(k = 2L), feat, has_outcome = FALSE)
  )
})

test_that("has_outcome = FALSE keeps the last column as a feature", {
  # The supervised convention would silently drop `b` from every dimension.
  feat <- data.frame(a = rnorm(4L), b = rnorm(4L))
  expect_identical(
    rtemis:::resolve_data_bounds(feat, has_outcome = FALSE)[["feature_names"]],
    c("a", "b")
  )
  expect_identical(
    rtemis:::resolve_data_bounds(feat)[["feature_names"]],
    "a"
  )
  expect_null(rtemis:::resolve_data_bounds(feat, has_outcome = FALSE)[[
    "n_classes"
  ]])
})

test_that("a name bound is restricted to string properties", {
  expect_error(
    prop_integer(1L, data_bound = "numeric_feature_names"),
    "only supported for type 'string'"
  )
})

test_that("check_data_bounds() skips unset values and n_classes in regression", {
  expect_invisible(check_data_bounds(setup_Ranger(), datr_bounds))
  # class_weights is bound to n_classes, which is undefined for a numeric
  # outcome: the declaration does not apply rather than erroring.
  expect_invisible(
    check_data_bounds(setup_Ranger(class_weights = c(1, 2, 3)), datr_bounds)
  )
  expect_error(
    check_data_bounds(setup_Ranger(class_weights = c(1, 2, 3)), datc_bounds),
    class = "rtemis_length_error"
  )
})


# %% Map containers on the wire ----
test_that("a map property round-trips as a JSON object, not an array", {
  # `jsonlite` drops names on atomic vectors, so a named numeric would emit an
  # array and fail its own schema, which declares `type: object`.
  cfg <- setup_Preprocessor(scale = TRUE)
  cfg@scale_centers <- c(a = 1.5, b = 2.5)
  wire <- S7_to_list(serializable_props(cfg))[["scale_centers"]]
  expect_type(wire, "list")
  expect_identical(names(wire), c("a", "b"))
  expect_match(
    as.character(jsonlite::toJSON(wire, auto_unbox = TRUE)),
    "^\\{",
    info = "must serialize as an object"
  )
  # And back: JSON objects parse to named lists, but the property is a named
  # atomic vector.
  restored <- .list_to_PreprocessorConfig(list(
    scale = TRUE,
    scale_centers = list(a = 1.5, b = 2.5)
  ))
  expect_identical(restored@scale_centers, c(a = 1.5, b = 2.5))
})

test_that("wire_value only reshapes maps, not any named vector", {
  # Ranger's class_weights is a named numeric declaring an *array*: naming it
  # must not change its wire type.
  arr <- RangerHyperparameters@properties[["class_weights"]]
  named <- c(setosa = 1, versicolor = 2)
  expect_identical(wire_value(named, arr), named)
  map <- PreprocessorConfig@properties[["scale_centers"]]
  expect_type(wire_value(named, map), "list")
})


# %% table container ----

# One spec reused across the table tests: an always-present numeric column, an
# optional one (a metric computed only for some tasks), and a label.
demo_table_prop <- function(...) {
  prop_table(
    columns = list(
      accuracy = prop_float(0, min = 0, max = 1, description = "Accuracy."),
      auc = prop_float(0, min = 0, max = 1, nullable = TRUE),
      label = prop_string("")
    ),
    required = c("accuracy", "label"),
    nullable = TRUE,
    description = "One row per resample.",
    ...
  )
}


demo_table_class <- function() {
  new_class("DemoTable", properties = list(overall = demo_table_prop()))
}


test_that("a table property accepts a conforming data frame", {
  Demo <- demo_table_class()
  df <- data.frame(
    accuracy = c(0.9, 0.8),
    auc = c(0.81, NA),
    label = c("f1", "f2")
  )
  expect_identical(Demo(overall = df)@overall, df)
  # An optional column may be absent entirely: the metric was not computed for
  # this task, which is not the same as being invalid.
  expect_no_error(Demo(overall = data.frame(accuracy = 0.9, label = "f1")))
})


test_that("a table property rejects malformed frames", {
  Demo <- demo_table_class()
  expect_error(
    Demo(overall = data.frame(accuracy = 1.4, label = "x")),
    "must be <= 1"
  )
  expect_error(
    Demo(overall = data.frame(accuracy = 0.9, label = "x", oops = 1)),
    "undeclared column"
  )
  expect_error(
    Demo(overall = data.frame(accuracy = 0.9)),
    "missing required column"
  )
  expect_error(
    Demo(overall = data.frame(accuracy = "high", label = "x")),
    "must be of type 'number'"
  )
  # NA is allowed only where the cell spec says so.
  expect_error(
    Demo(overall = data.frame(accuracy = NA_real_, label = "x")),
    "must not contain missing values"
  )
  expect_no_error(
    Demo(overall = data.frame(accuracy = 0.9, auc = NA_real_, label = "x"))
  )
  # S7's own class check fires before the validator for a non-frame.
  expect_error(Demo(overall = list(accuracy = 0.9)), "data\\.frame")
})


test_that("a table declaration is itself checked", {
  expect_error(prop_table(columns = list()), "non-empty named list")
  expect_error(
    prop_table(columns = list(prop_float(0))),
    "non-empty named list"
  )
  expect_error(
    prop_table(columns = list(a = "not a prop")),
    "must be a property built by a prop_\\* factory"
  )
  # A cell is a scalar: a column of containers has no row-oriented encoding.
  expect_error(
    prop_table(columns = list(a = prop_float(0, vector = TRUE))),
    "must describe a scalar cell"
  )
  expect_error(
    prop_table(columns = list(a = prop_float(0)), required = "b"),
    "undeclared members"
  )
})


test_that("a table emits a row-oriented schema", {
  spec <- get_spec(demo_table_prop())
  sch <- spec_to_schema(spec)
  expect_identical(as.character(sch[["type"]]), c("array", "null"))
  row <- sch[["items"]]
  expect_identical(row[["type"]], "object")
  expect_false(row[["additionalProperties"]])
  # Every declared column is published; only the always-present ones are
  # required, which is what makes an absent optional column legal.
  expect_identical(names(row[["properties"]]), c("accuracy", "auc", "label"))
  expect_identical(as.character(row[["required"]]), c("accuracy", "label"))
  expect_identical(row[["properties"]][["accuracy"]][["maximum"]], 1)
  expect_identical(
    as.character(row[["properties"]][["auc"]][["type"]]),
    c("number", "null")
  )
  expect_identical(sch[["x-rtemis"]][["container"]], "table")
})


test_that("a table round-trips through its schema", {
  orig <- get_spec(demo_table_prop())
  back <- schema_to_spec(spec_to_schema(orig))
  expect_identical(back@container, "table")
  expect_identical(back@nullable, orig@nullable)
  expect_identical(back@required_members, orig@required_members)
  expect_identical(names(back@members), names(orig@members))
  for (nm in names(orig@members)) {
    a <- orig@members[[nm]]
    b <- back@members[[nm]]
    expect_identical(b@type, a@type)
    expect_identical(b@minimum, a@minimum)
    expect_identical(b@maximum, a@maximum)
    expect_identical(b@nullable, a@nullable)
  }
})


test_that("a class rebuilt from a published schema validates tables alike", {
  Demo <- demo_table_class()
  sch <- S7_to_JSONSchema(
    Demo,
    id = "https://schema.rtemis.org/r/demo_table/v1/schema.json"
  )
  Rebuilt <- JSONSchema_to_S7(sch, name = "DemoTableRebuilt")
  expect_no_error(Rebuilt(overall = data.frame(accuracy = 0.9, label = "f1")))
  expect_error(
    Rebuilt(overall = data.frame(accuracy = 2, label = "x")),
    "must be <= 1"
  )
  expect_error(
    Rebuilt(overall = data.frame(accuracy = 0.9)),
    "missing required column"
  )
})


test_that("a table serializes row-oriented, keeping absent and null distinct", {
  df <- data.frame(
    accuracy = c(0.9, 0.8),
    auc = c(0.81, NA),
    label = c("f1", "f2")
  )
  # The write path's options, which decide this: an undefined cell must reach
  # the wire as `null`, not as a missing key, or it would be indistinguishable
  # from a column that was never computed.
  json <- as.character(jsonlite::toJSON(df, auto_unbox = TRUE, na = "null"))
  expect_match(json, '^\\[\\{"accuracy":0\\.9')
  expect_match(json, '"auc":null')
  restored <- jsonlite::fromJSON(json)
  expect_identical(restored, df)
  # A column never computed is simply absent from every row.
  absent <- as.character(jsonlite::toJSON(
    data.frame(accuracy = 0.9, label = "f1"),
    auto_unbox = TRUE,
    na = "null"
  ))
  expect_false(grepl("auc", absent, fixed = TRUE))
})


# %% struct container ----

demo_struct_prop <- function() {
  prop_struct(
    members = list(
      overall = prop_table(
        columns = list(
          balanced_accuracy = prop_float(0, min = 0, max = 1),
          auc = prop_float(0, min = 0, max = 1, nullable = TRUE)
        ),
        required = "balanced_accuracy",
        nullable = TRUE
      ),
      positive_class = prop_string(NULL, nullable = TRUE)
    ),
    nullable = TRUE,
    description = "Classification metrics payload."
  )
}


demo_struct_class <- function() {
  new_class("DemoStruct", properties = list(metrics = demo_struct_prop()))
}


test_that("a struct property validates its declared fields", {
  Demo <- demo_struct_class()
  ok <- list(
    overall = data.frame(balanced_accuracy = 0.9, auc = 0.8),
    positive_class = "a"
  )
  expect_no_error(Demo(metrics = ok))
  expect_error(
    Demo(metrics = list(overall = ok[["overall"]])),
    "positive_class"
  )
  expect_error(
    Demo(metrics = c(ok, list(oops = 1))),
    "undeclared field\\(s\\) 'oops'"
  )
  expect_error(Demo(metrics = "not a list"), "list")
})


test_that("a struct's members may themselves be containers", {
  # This is what separates a struct from a table: a field can be a whole table,
  # and the error names the path to the offending cell.
  Demo <- demo_struct_class()
  expect_error(
    Demo(
      metrics = list(
        overall = data.frame(balanced_accuracy = 1.5),
        positive_class = "a"
      )
    ),
    "field 'overall' column 'balanced_accuracy' must be <= 1"
  )
  expect_error(
    Demo(
      metrics = list(
        overall = data.frame(auc = 0.5),
        positive_class = "a"
      )
    ),
    "field 'overall' is missing required column\\(s\\) 'balanced_accuracy'"
  )
})


test_that("a struct declaration is itself checked", {
  expect_error(prop_struct(members = list()), "non-empty named list")
  expect_error(
    prop_struct(members = list(a = "not a prop")),
    "must be a property built by a prop_\\* factory"
  )
  expect_error(
    prop_struct(members = list(a = prop_float(0)), required = "b"),
    "undeclared members"
  )
})


test_that("a struct emits an object schema and round-trips", {
  orig <- get_spec(demo_struct_prop())
  sch <- spec_to_schema(orig)
  expect_identical(as.character(sch[["type"]]), c("object", "null"))
  expect_false(sch[["additionalProperties"]])
  expect_identical(
    as.character(sch[["required"]]),
    c("overall", "positive_class")
  )
  expect_identical(sch[["x-rtemis"]][["container"]], "struct")
  # The nested table keeps its own container annotation.
  expect_identical(
    sch[["properties"]][["overall"]][["x-rtemis"]][["container"]],
    "table"
  )
  back <- schema_to_spec(sch)
  expect_identical(back@container, "struct")
  expect_identical(back@required_members, orig@required_members)
  expect_identical(names(back@members), names(orig@members))
  expect_identical(back@members[["overall"]]@container, "table")
  expect_identical(
    back@members[["overall"]]@required_members,
    orig@members[["overall"]]@required_members
  )
})


test_that("a class rebuilt from a published schema validates structs alike", {
  Demo <- demo_struct_class()
  sch <- S7_to_JSONSchema(
    Demo,
    id = "https://schema.rtemis.org/r/demo_struct/v1/schema.json"
  )
  Rebuilt <- JSONSchema_to_S7(sch, name = "DemoStructRebuilt")
  expect_no_error(Rebuilt(
    metrics = list(
      overall = data.frame(balanced_accuracy = 0.9),
      positive_class = "a"
    )
  ))
  expect_error(
    Rebuilt(
      metrics = list(
        overall = data.frame(balanced_accuracy = 3),
        positive_class = "a"
      )
    ),
    "field 'overall' column 'balanced_accuracy' must be <= 1"
  )
})


# %% r_only role ----

test_that("an r_only property is neither serialized nor published", {
  p <- prop_r_only(new_property(class_any))
  expect_identical(prop_role(p), "r_only")
  expect_false(prop_serialized(p))
  # The real ones: a fitted backend model has no wire form, and unlike a
  # computed view nothing published can reconstruct it.
  for (nm in c("model", "session_info")) {
    expect_identical(
      prop_role(Supervised@properties[[nm]]),
      "r_only",
      info = nm
    )
    expect_false(prop_serialized(Supervised@properties[[nm]]), info = nm)
  }
})


test_that("r_only is marked, never inferred", {
  # A spec-less, role-less property is drift and must still fail loudly: the
  # marker exists so that adding a property and forgetting to declare it is not
  # silently treated as "R-only".
  Drifty <- new_class("Drifty", properties = list(mystery = class_any))
  expect_error(
    S7_to_JSONSchema(
      Drifty,
      id = "https://schema.rtemis.org/r/drifty/v1/schema.json"
    ),
    "mystery"
  )
})


# %% Published properties ----

test_that("prop_published() is true for config and state, false for the rest", {
  expect_true(prop_published(prop_float(0)))
  expect_true(prop_published(prop_state(prop_float(0))))
  expect_false(prop_published(prop_computed(new_property(class_any))))
  expect_false(prop_published(prop_r_only(new_property(class_any))))
  # A property that has not been migrated to the factories keeps serializing as
  # it always did; schema generation is where that drift is caught.
  expect_true(prop_published(new_property(class_any)))
})


test_that("the default to_json omits computed and r_only properties", {
  keys <- names(to_json(data_fingerprint(iris)))
  expect_true("method" %in% keys)
  # `portability` is a function of `method`, so a consumer loses nothing.
  expect_false("portability" %in% keys)
})


# %% factor container ----

demo_factor_class <- function(...) {
  new_class(
    "DemoFactor",
    properties = list(y = prop_factor(nullable = TRUE, ...))
  )
}


test_that("a factor property accepts a factor and rejects its labels", {
  Demo <- demo_factor_class()
  y <- factor(c("a", "b", "a"))
  expect_identical(Demo(y = y)@y, y)
  # A character vector is not a factor: the two carry different information
  # (the level set), so accepting one for the other would lose it silently.
  expect_error(Demo(y = c("a", "b")), "factor")
  expect_error(Demo(y = factor(c("a", NA))), "missing values")
  expect_null(Demo()@y)
})


test_that("a factor property honors a declared level set", {
  Demo <- demo_factor_class(enum = c("no", "yes"))
  expect_no_error(Demo(y = factor(c("yes", "no"))))
  expect_error(Demo(y = factor(c("maybe"))), "must be one of")
})


test_that("a factor emits its levels and codes, and the spec round-trips", {
  spec <- get_spec(prop_factor(nullable = TRUE, description = "Outcome."))
  sch <- spec_to_schema(spec)
  expect_identical(as.character(sch[["type"]]), c("object", "null"))
  expect_identical(as.character(sch[["required"]]), c("levels", "codes"))
  expect_true(sch[["properties"]][["levels"]][["uniqueItems"]])
  # Codes are 1-based positions, which the schema asserts rather than leaving
  # to prose: a 0-based reader would shift every label by one.
  expect_identical(sch[["properties"]][["codes"]][["items"]][["minimum"]], 1L)
  expect_identical(sch[["x-rtemis"]][["container"]], "factor")
  back <- schema_to_spec(sch)
  expect_identical(back@container, "factor")
  expect_identical(back@type, "string")
  expect_true(back@nullable)
})


test_that("a declared level set constrains the levels in the schema", {
  spec <- get_spec(prop_factor(enum = c("no", "yes"), nullable = TRUE))
  sch <- spec_to_schema(spec)
  expect_identical(
    as.character(sch[["properties"]][["levels"]][["items"]][["enum"]]),
    c("no", "yes")
  )
})


test_that("a factor property must be nullable, having no prototype value", {
  # The same constraint `prop_matrix()` and `prop_table()` carry: a spec's
  # default must validate, and there is no factor a class could default to.
  expect_error(prop_factor(), "default")
})


test_that("a factor survives the wire with its level order and empty levels", {
  prop <- prop_factor(nullable = TRUE)
  # Level order is not cosmetic: `binclasspos = 2L` means the positive class is
  # the *second* level, so a realphabetised factor has swapped which class is
  # positive.
  reordered <- factor(c("b", "a", "b"), levels = c("b", "a"))
  wire <- wire_value(reordered, prop)
  expect_identical(wire[["levels"]], c("b", "a"))
  expect_identical(wire[["codes"]], c(1L, 2L, 1L))
  expect_identical(from_wire_factor(wire), reordered)
  # A level with no cases disappears entirely from an array of labels.
  unobserved <- factor(c("a", "a"), levels = c("a", "b", "c"))
  expect_identical(
    from_wire_factor(wire_value(unobserved, prop)),
    unobserved
  )
})


test_that("factor codes that index past the levels are rejected", {
  expect_error(
    from_wire_factor(list(levels = c("a", "b"), codes = c(1L, 3L))),
    "1-based"
  )
})


test_that("from_wire_maps rebuilds a factor property", {
  Demo <- demo_factor_class()
  parsed <- list(y = list(levels = c("b", "a"), codes = c(2L, 1L)))
  restored <- from_wire_maps(parsed, Demo)
  expect_identical(restored[["y"]], factor(c("a", "b"), levels = c("b", "a")))
  expect_no_error(Demo(y = restored[["y"]]))
})


# %% array_refs ----

test_that("array_refs emits an array of $ref", {
  Demo <- new_class(
    "DemoArrayRefs",
    properties = list(parts = new_property(class_list))
  )
  target <- "https://schema.rtemis.org/regressionmetrics/v1/schema.json"
  sch <- S7_to_JSONSchema(
    Demo,
    id = "https://schema.rtemis.org/r/demoarrayrefs/v1/schema.json",
    array_refs = c(parts = target)
  )
  parts <- sch[["properties"]][["parts"]]
  expect_identical(parts[["type"]], "array")
  expect_identical(parts[["items"]][["$ref"]], target)
})


test_that("array_refs must name existing properties", {
  Demo <- new_class(
    "DemoArrayRefs2",
    properties = list(parts = new_property(class_list))
  )
  expect_error(
    S7_to_JSONSchema(
      Demo,
      id = "https://schema.rtemis.org/r/demoarrayrefs2/v1/schema.json",
      array_refs = c(nope = "https://example.org/x.json")
    ),
    "array_refs"
  )
})
