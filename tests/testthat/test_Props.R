# test_Props.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

# %% Pilot class ----
# LightRF hyperparameters declared with the prop_* factories: types, defaults,
# bounds, tunability, and descriptions in ONE place, from which both the S7
# validators and the JSON Schema are generated. Defined here (not in R/) until
# the migration of 02_Hyperparameters.R.
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
  # A length-1 default on an array-typed property must still serialize as an
  # array, or the emitted default contradicts the emitted type.
  testthat::skip_if_not_installed("jsonlite")
  s1 <- spec_to_schema(get_spec(prop_string("a", vector = TRUE)))
  testthat::expect_identical(
    as.character(jsonlite::toJSON(s1[["default"]], auto_unbox = TRUE)),
    "[\"a\"]"
  )
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
  testthat::expect_named(s, c("oneOf", "default", "description"))
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
  testthat::expect_identical(
    schema[["properties"]][["nrounds"]][["default"]],
    500L
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
  # `prop_state()` declares it out of the contract entirely.
  Stated <- S7::new_class(
    name = "Stated",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_state(S7::class_integer, default = 0L)
    )
  )
  s <- S7_to_JSONSchema(Stated, id = "https://example.org/x.json")
  testthat::expect_identical(names(s[["properties"]]), "a")
})

testthat::test_that("prop_external() requires `extra` to supply its schema", {
  Ext <- S7::new_class(
    name = "Ext",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_external(S7::class_list, default = list())
    )
  )
  testthat::expect_error(
    S7_to_JSONSchema(Ext, id = "https://example.org/x.json"),
    "not supplied by `extra`"
  )
  s <- S7_to_JSONSchema(
    Ext,
    id = "https://example.org/x.json",
    extra = list(properties = list(b = list(type = "object")))
  )
  testthat::expect_setequal(names(s[["properties"]]), c("a", "b"))
})

testthat::test_that("prop_role classifies each declaration style", {
  Roles <- S7::new_class(
    name = "Roles",
    package = NULL,
    properties = list(
      a = prop_boolean(TRUE),
      b = prop_external(S7::class_list, default = list()),
      c = prop_external(S7::class_numeric, data_dependent = TRUE),
      d = prop_state(S7::class_integer, default = 0L),
      e = S7::class_integer
    )
  )
  testthat::expect_identical(role_prop_names(Roles, "config"), "a")
  testthat::expect_identical(role_prop_names(Roles, "external"), c("b", "c"))
  testthat::expect_identical(role_prop_names(Roles, "state"), "d")
  testthat::expect_identical(data_dependent_prop_names(Roles), "c")
  testthat::expect_true(is.na(prop_role(Roles@properties[["e"]])))
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
# before tuning. See DATA_BOUNDS in 00_Props.R.

test_that("data_bound rejects names outside the vocabulary", {
  expect_error(prop_integer(1L, data_bound = "n_bananas"))
  expect_error(prop_external(NULL | class_numeric, data_bound = "n_bananas"))
})

test_that("data_bound on a string property must be feature_names", {
  expect_error(prop_string("a", data_bound = "n_features"))
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
