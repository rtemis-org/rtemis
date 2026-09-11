# test_Defaults.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("default declarations preserve null and nested member metadata", {
  property <- prop_struct(list(status = prop_string("unsupported", enum = c("computed", "unsupported"))), nullable = TRUE)
  original <- get_spec(property)
  schema <- prop_to_schema(property)
  declarations <- default_declarations(original, schema, "/properties/result")
  encoded <- jsonlite::toJSON(declarations, auto_unbox = TRUE, null = "null", digits = NA)
  declarations <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)
  restored <- schema_to_spec(schema, declarations = declarations, path = "/properties/result")
  expect_identical(spec_fields(restored), spec_fields(original))
  expect_null(restored@default)
  expect_identical(restored@members[["status"]]@default, "unsupported")
  expect_error(schema_to_spec(schema, declarations = list(), path = "/properties/result"), "Missing default declaration")
})


test_that("singleton and empty collection defaults retain wire shape", {
  for (value in list("")) {
    property <- prop_string(value, vector = TRUE)
    schema <- prop_to_schema(property)
    declaration <- default_declarations(get_spec(property), schema, "/properties/x")
    json <- jsonlite::toJSON(declaration, auto_unbox = TRUE, null = "null", digits = NA)
    decoded <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_type(decoded[["/properties/x"]][["value"]], "list")
    restored <- schema_to_spec(schema, declarations = decoded, path = "/properties/x")
    expect_identical(restored@default, value)
  }
  empty <- prop_collection(Hyperparameters, container = "array", min_items = 0L, default = list())
  encoded <- jsonlite::toJSON(default_wire_value(list(), get_spec_fields(empty)), auto_unbox = TRUE)
  expect_identical(as.character(encoded), "[]")
  expect_identical(as.character(jsonlite::toJSON(default_wire_value(list(), get_spec_fields(prop_bag())), auto_unbox = TRUE)), "{}")
})


test_that("a declaration without a default does not invent a value", {
  schema <- prop_to_schema(prop_integer(3L, min = 1L))
  spec <- schema_to_spec(schema)
  expect_false(spec@default_present)
  expect_null(spec@default)
  cls <- schema_class("RequiredDefaultInput", properties = list(x = make_prop(spec)))
  expect_error(cls(), class = "rtemis_input_error")
  expect_identical(cls(x = 4L)@x, 4L)
  expect_error(cls(x = 0L))
  expect_error(default_from_wire("3", schema), "wrong JSON type")
  expect_error(default_from_wire(3.2, schema), "whole numbers")
})


test_that("document defaults agree with setup behavior and preserve overrides", {
  for (scale in c(FALSE, TRUE)) {
    out <- resolve_class_defaults(PreprocessorConfig, list(scale = scale))
    expect_identical(out[["values"]][["center"]], scale)
    expect_identical(setup_Preprocessor(scale = scale)@center, scale)
    expect_identical(setup_Preprocessor(scale = scale, center = !scale)@center, !scale)
  }
  for (replace in c(FALSE, TRUE)) {
    expected <- if (replace) 1 else 0.632
    out <- resolve_class_defaults(RangerHyperparameters, list(replace = replace))
    expect_identical(out[["values"]][["sample_fraction"]], expected)
    expect_identical(setup_Ranger(replace = replace)@sample_fraction, expected)
  }
  expect_identical(setup_Ranger(sample_fraction = 0.75)@sample_fraction, 0.75)
  for (initial in list(NULL, matrix(0, 2L, 2L))) {
    input <- list(Y_init = initial)
    out <- resolve_class_defaults(tSNEConfig, input)
    expected <- if (is.null(initial)) 250L else 0L
    expect_identical(out[["values"]][["stop_lying_iter"]], expected)
    expect_identical(setup_tSNE(Y_init = initial)@mom_switch_iter, expected)
  }
})


test_that("runtime defaults remain pending without explicit context", {
  out <- resolve_class_defaults(FutureExecutionConfig, list())
  expect_true(all(c("n_workers", "future_plan", "seed") %in% names(out[["pending"]])))
  expect_false("seed" %in% names(out[["values"]]))
  context <- list(n_workers = 2L, future_plan = "sequential", seed = 19L)
  resolved <- resolve_class_defaults(FutureExecutionConfig, list(), context)
  expect_length(resolved[["pending"]], 0L)
  expect_identical(resolved[["values"]][["seed"]], 19L)
  repeated <- resolve_class_defaults(FutureExecutionConfig, resolved[["values"]], context)
  expect_identical(repeated[["values"]], resolved[["values"]])
  expect_identical(resolve_class_defaults(FutureExecutionConfig, list(seed = NULL), context)[["values"]][["seed"]], 19L)
})


test_that("default expressions reject ambiguity and cycles", {
  expect_error(DefaultPolicy(kind = "expression", expression = list(system = "anything")), "Unsupported default operation")
  candidate <- resolve_class_defaults(RangerHyperparameters, list(replace = tune_over(FALSE, TRUE)))
  expect_true("sample_fraction" %in% names(candidate[["pending"]]))
  expect_error(setup_Ranger(replace = tune_over(FALSE, TRUE)), class = "rtemis_defaults_pending")
  expect_error(schema_class("CyclicDefaults", properties = list(x = prop_integer(1L), y = prop_integer(1L)),
    defaults = list(x = DefaultPolicy(kind = "expression", expression = list(var = "y")),
      y = DefaultPolicy(kind = "expression", expression = list(var = "x")))), "Cyclic default dependency")
})


test_that("class metadata and inherited construction are distinct", {
  parent <- schema_class("DefaultParent", properties = list(x = prop_integer(1L)))
  child <- schema_class("DefaultChild", parent = parent, properties = list(x = prop_integer(2L)))
  expect_identical(child()@x, 1L)
  expect_identical(resolve_class_defaults(child, list())[["values"]][["x"]], 2L)
  expect_identical(child(x = 2L)@x, 2L)
  expect_null(get_spec(SuperConfigPaths@properties[["outdir"]])@default)
  expect_identical(resolve_class_defaults(SuperConfigPaths, list())[["values"]][["outdir"]], "results/")
})
