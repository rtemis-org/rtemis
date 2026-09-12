# test_Defaults.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("default declarations preserve null and nested member metadata", {
  property <- prop_struct(
    list(
      status = prop_string("unsupported", enum = c("computed", "unsupported"))
    ),
    nullable = TRUE
  )
  original <- get_spec(property)
  schema <- prop_to_schema(property)
  declarations <- default_declarations(original, schema, "/properties/result")
  encoded <- jsonlite::toJSON(
    declarations,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  declarations <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)
  restored <- schema_to_spec(
    schema,
    declarations = declarations,
    path = "/properties/result"
  )
  expect_identical(spec_fields(restored), spec_fields(original))
  expect_null(restored@default)
  expect_identical(restored@members[["status"]]@default, "unsupported")
  expect_error(
    schema_to_spec(schema, declarations = list(), path = "/properties/result"),
    "Missing default declaration"
  )
})


test_that("singleton and empty collection defaults retain wire shape", {
  for (value in list("")) {
    property <- prop_string(value, vector = TRUE)
    schema <- prop_to_schema(property)
    declaration <- default_declarations(
      get_spec(property),
      schema,
      "/properties/x"
    )
    json <- jsonlite::toJSON(
      declaration,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    decoded <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_type(decoded[["/properties/x"]][["value"]], "list")
    restored <- schema_to_spec(
      schema,
      declarations = decoded,
      path = "/properties/x"
    )
    expect_identical(restored@default, value)
  }
  empty <- prop_collection(
    Hyperparameters,
    container = "array",
    min_items = 0L,
    default = list()
  )
  encoded <- jsonlite::toJSON(
    default_wire_value(list(), get_spec_fields(empty)),
    auto_unbox = TRUE
  )
  expect_identical(as.character(encoded), "[]")
  expect_identical(
    as.character(jsonlite::toJSON(
      default_wire_value(list(), get_spec_fields(prop_bag())),
      auto_unbox = TRUE
    )),
    "{}"
  )
})


test_that("input policies preserve typed values through JSON", {
  policies <- list(
    DefaultPolicy(kind = "literal", value = "NA"),
    DefaultPolicy(
      kind = "runtime",
      requires = "data",
      reason = "Read data first."
    )
  )
  for (policy in policies) {
    property <- prop_default(prop_string("", vector = TRUE), policy)
    schema <- prop_to_schema(property)
    declarations <- jsonlite::fromJSON(
      jsonlite::toJSON(
        default_declarations(get_spec(property), schema, "/properties/x"),
        auto_unbox = TRUE,
        null = "null"
      ),
      simplifyVector = FALSE
    )
    restored <- schema_to_spec(
      schema,
      declarations = declarations,
      path = "/properties/x"
    )
    expect_identical(props(restored@default_policy), props(policy))
    wire_policy <- declarations[["/properties/x"]][["policy"]]
    expect_type(
      wire_policy[[if (policy@kind == "literal") "value" else "requires"]],
      "list"
    )
  }

  id <- "https://example.test/vector/schema.json"
  property <- prop_string("", vector = TRUE)
  schema <- list(
    `$id` = id,
    title = "ArtifactVector",
    properties = list(x = prop_to_schema(property)),
    `x-rtemis` = list(
      publication = list(
        class = "artifact::ArtifactVector",
        role = "document",
        kind = "config"
      )
    )
  )
  artifact <- list(
    format_version = 1L,
    declarations = stats::setNames(
      list(default_declarations(
        get_spec(property),
        schema[["properties"]][["x"]],
        "/properties/x"
      )),
      id
    ),
    resolution = stats::setNames(
      list(list(x = list(kind = "literal", value = list("NA")))),
      id
    )
  )
  artifact <- jsonlite::fromJSON(
    jsonlite::toJSON(artifact, auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  cls <- JSONSchema_to_S7(
    schema,
    defaults = artifact,
    schemas = stats::setNames(list(schema), id)
  )
  expect_identical(cls()@x, "")
  expect_identical(resolve_class_defaults(cls, list())[["values"]][["x"]], "NA")
  expect_identical(
    resolve_class_defaults(cls, list(x = "custom"))[["values"]][["x"]],
    "custom"
  )
  for (version in list(2L, 3L, "1", 1.5)) {
    invalid <- artifact
    invalid[["format_version"]] <- version
    expect_error(
      JSONSchema_to_S7(schema, defaults = invalid),
      "Unsupported defaults artifact version"
    )
    expect_error(
      JSONSchema_to_S7(
        schema,
        defaults = invalid,
        schemas = stats::setNames(list(schema), id)
      ),
      "Unsupported defaults artifact version"
    )
  }
})


test_that("a declaration without a default does not invent a value", {
  schema <- prop_to_schema(prop_integer(3L, min = 1L))
  spec <- schema_to_spec(schema)
  expect_false(spec@default_present)
  expect_null(spec@default)
  cls <- schema_class(
    "RequiredDefaultInput",
    properties = list(x = make_prop(spec))
  )
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
    expect_identical(
      setup_Preprocessor(scale = scale, center = !scale)@center,
      !scale
    )
  }
  for (replace in c(FALSE, TRUE)) {
    expected <- if (replace) 1 else 0.632
    out <- resolve_class_defaults(
      RangerHyperparameters,
      list(replace = replace)
    )
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
  expect_true(all(
    c("n_workers", "future_plan", "seed") %in% names(out[["pending"]])
  ))
  expect_false("seed" %in% names(out[["values"]]))
  context <- list(n_workers = 2L, future_plan = "sequential", seed = 19L)
  resolved <- resolve_class_defaults(FutureExecutionConfig, list(), context)
  expect_length(resolved[["pending"]], 0L)
  expect_identical(resolved[["values"]][["seed"]], 19L)
  repeated <- resolve_class_defaults(
    FutureExecutionConfig,
    resolved[["values"]],
    context
  )
  expect_identical(repeated[["values"]], resolved[["values"]])
  expect_identical(
    resolve_class_defaults(FutureExecutionConfig, list(seed = NULL), context)[[
      "values"
    ]][["seed"]],
    19L
  )
})


test_that("default expressions reject ambiguity and cycles", {
  expect_error(
    DefaultPolicy(kind = "expression", expression = list(system = "anything")),
    "Unsupported default operation"
  )
  candidate <- resolve_class_defaults(
    RangerHyperparameters,
    list(replace = tune_over(FALSE, TRUE))
  )
  expect_true("sample_fraction" %in% names(candidate[["pending"]]))
  expect_error(
    setup_Ranger(replace = tune_over(FALSE, TRUE)),
    class = "rtemis_defaults_pending"
  )
  expect_error(
    schema_class(
      "CyclicDefaults",
      properties = list(x = prop_integer(1L), y = prop_integer(1L)),
      defaults = list(
        x = DefaultPolicy(kind = "expression", expression = list(var = "y")),
        y = DefaultPolicy(kind = "expression", expression = list(var = "x"))
      )
    ),
    "Cyclic default dependency"
  )
})


test_that("class metadata and inherited construction are distinct", {
  parent <- schema_class(
    "DefaultParent",
    properties = list(x = prop_integer(1L))
  )
  child <- schema_class(
    "DefaultChild",
    parent = parent,
    properties = list(x = prop_integer(2L))
  )
  expect_identical(child()@x, 1L)
  expect_identical(resolve_class_defaults(child, list())[["values"]][["x"]], 2L)
  expect_identical(child(x = 2L)@x, 2L)
  expect_null(get_spec(SuperConfigPaths@properties[["outdir"]])@default)
  expect_identical(
    resolve_class_defaults(SuperConfigPaths, list())[["values"]][["outdir"]],
    "results/"
  )
})


test_that("artifact references reconstruct without native readers", {
  leaf_id <- "https://example.test/leaf/schema.json"
  root_id <- "https://example.test/root/schema.json"
  leaf <- schema_class(
    "ArtifactLeaf",
    package = "artifact",
    properties = list(value = prop_integer(7L, min = 1L))
  )
  reference <- prop_object(leaf, default = leaf())
  leaf_schema <- list(
    `$id` = leaf_id,
    title = "ArtifactLeaf",
    properties = list(value = prop_to_schema(leaf@properties[["value"]])),
    `x-rtemis` = list(
      publication = list(
        class = "artifact::ArtifactLeaf",
        role = "document",
        kind = "config"
      )
    )
  )
  root_schema <- list(
    `$id` = root_id,
    title = "ArtifactRoot",
    properties = list(
      child = spec_to_schema(
        get_spec(reference),
        reference = leaf_id,
        reference_urls = c(`artifact::ArtifactLeaf` = leaf_id)
      )
    ),
    `x-rtemis` = list(
      publication = list(
        class = "artifact::ArtifactRoot",
        role = "document",
        kind = "config"
      )
    )
  )
  declarations <- stats::setNames(
    list(
      default_declarations(
        get_spec(leaf@properties[["value"]]),
        leaf_schema[["properties"]][["value"]],
        "/properties/value"
      ),
      default_declarations(
        get_spec(reference),
        root_schema[["properties"]][["child"]],
        "/properties/child"
      )
    ),
    c(leaf_id, root_id)
  )
  artifact <- jsonlite::fromJSON(
    jsonlite::toJSON(
      list(
        format_version = 1L,
        declarations = declarations,
        resolution = stats::setNames(
          list(list(
            child = list(kind = "literal", value = list(value = 9L))
          )),
          root_id
        )
      ),
      auto_unbox = TRUE,
      null = "null"
    ),
    simplifyVector = FALSE
  )
  schemas <- stats::setNames(
    list(leaf_schema, root_schema),
    c(leaf_id, root_id)
  )
  local_mocked_bindings(from_wire_object = function(...) {
    stop("Native reader must not run")
  })
  cls <- JSONSchema_to_S7(root_schema, defaults = artifact, schemas = schemas)
  expect_identical(cls()@child@value, 7L)
  expect_identical(
    resolve_class_defaults(cls, list())[["values"]][["child"]],
    list(value = 9L)
  )
  expect_error(cls(child = list(value = 7L)))
  child <- S7_class(cls()@child)
  expect_error(child(value = 0L))
  expect_error(
    JSONSchema_to_S7(
      root_schema,
      defaults = artifact,
      schemas = schemas[root_id]
    ),
    "Unavailable reference"
  )
  record <- root_schema
  record[["$id"]] <- "https://example.test/root/record.json"
  record[["properties"]][["observed"]] <- prop_to_schema(prop_integer(0L))
  mixed <- c(stats::setNames(list(record), record[["$id"]]), schemas)
  for (graph in list(mixed, rev(mixed))) {
    restored <- JSONSchema_to_S7(
      root_schema,
      defaults = artifact,
      schemas = graph
    )
    expect_identical(restored()@child@value, 7L)
    expect_false("observed" %in% names(restored@properties))
  }
  artifact[["resolution"]][[root_id]][["child"]]["value"] <- list(list())
  partial <- JSONSchema_to_S7(root_schema, defaults = artifact, schemas = mixed)
  expect_identical(
    resolve_class_defaults(partial, list())[["values"]][["child"]],
    list()
  )
  artifact[["resolution"]][[root_id]][["child"]][["value"]] <- NULL
  expect_error(
    JSONSchema_to_S7(root_schema, defaults = artifact, schemas = mixed),
    "lacks its value"
  )
})


test_that("artifact family policies do not invent discriminator defaults", {
  family_id <- "https://example.test/family/schema.json"
  leaf_id <- "https://example.test/one/schema.json"
  property <- prop_integer(1L)
  family <- list(
    `$id` = family_id,
    properties = list(
      mode = list(
        type = "string",
        enum = list("one"),
        `x-rtemis` = list(type = "string")
      ),
      x = prop_to_schema(property)
    ),
    allOf = list(list(
      `if` = list(properties = list(mode = list(const = "one"))),
      then = list(`$ref` = leaf_id)
    )),
    `x-rtemis` = list(
      publication = list(
        class = "artifact::PolicyFamily",
        role = "family",
        kind = "config"
      )
    )
  )
  leaf <- list(
    `$id` = leaf_id,
    properties = list(),
    `x-rtemis` = list(
      publication = list(
        class = "artifact::PolicyLeaf",
        role = "leaf",
        kind = "config"
      )
    )
  )
  artifact <- list(
    format_version = 1L,
    declarations = stats::setNames(
      list(
        default_declarations(
          get_spec(property),
          family[["properties"]][["x"]],
          "/properties/x"
        ),
        list()
      ),
      c(family_id, leaf_id)
    ),
    resolution = stats::setNames(
      list(
        list(x = list(kind = "literal", value = 1L)),
        list(x = list(kind = "literal", value = 2L))
      ),
      c(family_id, leaf_id)
    )
  )
  schemas <- stats::setNames(list(family, leaf), c(family_id, leaf_id))
  root <- JSONSchema_to_S7(family, defaults = artifact, schemas = schemas)
  branch <- JSONSchema_to_S7(leaf, defaults = artifact, schemas = rev(schemas))
  expect_identical(
    resolve_class_defaults(root, list())[["values"]],
    list(x = 1L)
  )
  expect_identical(
    resolve_class_defaults(branch, list())[["values"]],
    list(x = 2L)
  )
  expect_identical(branch()@mode, "one")
  expect_error(branch(mode = "wrong"))
})


test_that("policy result types are rejected when declared", {
  expect_error(
    schema_class(
      "BadDefaultType",
      properties = list(x = prop_integer(1L), y = prop_boolean(TRUE)),
      defaults = list(
        x = DefaultPolicy(kind = "expression", expression = list(var = "y"))
      )
    ),
    "incompatible result type"
  )
  expect_error(
    schema_class(
      "BadDefaultDependency",
      properties = list(x = prop_integer(1L)),
      defaults = list(
        x = DefaultPolicy(
          kind = "expression",
          expression = list(var = "missing")
        )
      )
    ),
    "unknown property"
  )
  declaration <- list(`/properties/x` = list(kind = "literal", value = 2L))
  expect_error(
    schema_to_spec(
      prop_to_schema(prop_const(1L)),
      declarations = declaration,
      path = "/properties/x"
    ),
    "contradicts"
  )
  expect_length(resolve_class_defaults(DataFingerprint, list())[["values"]], 0L)
})
