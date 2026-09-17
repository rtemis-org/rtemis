# test_ArtifactInheritance.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("artifact-only reconstruction preserves declared parent identity", {
  Parent <- schema_class(
    "ArtifactParent",
    package = "rtemis",
    properties = list(
      label = prop_string(description = "Observed label."),
      model = prop_runtime("Fitted native model used for prediction."),
      preprocessor = Supervised@properties[["preprocessor"]],
      preprocessor_internal = Supervised@properties[["preprocessor_internal"]],
      decomposition = Supervised@properties[["decomposition"]]
    ),
    publication = SchemaPublication(
      description = "Parent.",
      kind = "report",
      scope = "shared"
    )
  )
  Child <- schema_class(
    "ArtifactChild",
    package = "rtemis",
    parent = Parent,
    properties = list(score = prop_float(0, min = 0)),
    publication = SchemaPublication(
      description = "Child.",
      kind = "report",
      scope = "shared"
    )
  )
  classes <- list(Parent, Child)
  schemas <- lapply(classes, function(cls) {
    S7_to_JSONSchema(
      cls,
      id = paste0("https://example.test/", cls@name),
      asserted = TRUE
    )
  })
  names(schemas) <- vapply(schemas, `[[`, character(1L), "$id")
  declarations <- lapply(seq_along(classes), function(i) {
    unlist(
      lapply(published_prop_names(classes[[i]]), function(nm) {
        default_declarations(
          get_spec(classes[[i]]@properties[[nm]]),
          schemas[[i]][["properties"]][[nm]],
          paste0("/properties/", nm)
        )
      }),
      recursive = FALSE
    )
  })
  names(declarations) <- names(schemas)
  defaults <- list(format_version = 1L, declarations = declarations)
  graph <- default_artifact_graph(schemas, defaults)
  RestoredParent <- graph[["class"]](names(schemas)[[1L]])
  RestoredChild <- graph[["class"]](names(schemas)[[2L]])
  value <- RestoredChild(label = "observed", score = 0.75)
  expect_true(S7::S7_inherits(value, RestoredParent))
  expect_identical(value@label, "observed")
  runtime <- schemas[[2L]][["x-rtemis"]][["runtime_properties"]]
  for (nm in names(runtime)) {
    expect_null(prop(value, nm))
    native <- new.env(parent = emptyenv())
    prop(value, nm) <- native
    expect_identical(prop(value, nm), native)
    expect_false(nm %in% names(S7_to_list(value)))
    expect_false(nm %in% names(record_object(value)))
    expect_false(nm %in% names(schemas[[2L]][["properties"]]))
    expect_identical(runtime[[nm]][["kind"]], "opaque")
  }
  expect_error(RestoredChild(score = 0.75), "requires an explicit value")
  expect_error(RestoredChild(label = "observed", score = -1))
  expect_error(JSONSchema_to_S7(schemas[[2L]]), "published parent")
  schemas[[2L]][["x-rtemis"]][["publication"]][["parent"]] <- "rtemis::Missing"
  expect_error(
    default_artifact_graph(schemas, defaults)[["class"]](names(schemas)[[2L]]),
    "Unavailable artifact class"
  )
})


test_that("string declaration omission is distinct from literal null", {
  property <- prop_string(description = "No declaration default.")
  spec <- get_spec(property)
  expect_false(spec@default_present)
  schema <- prop_to_schema(property)
  declarations <- default_declarations(spec, schema, "/properties/value")
  expect_identical(declarations[["/properties/value"]][["kind"]], "none")
  restored <- schema_to_spec(
    schema,
    declarations = declarations,
    path = "/properties/value"
  )
  expect_identical(spec_fields(restored), spec_fields(spec))
  expect_error(prop_string(NULL))
  expect_true(get_spec(prop_string(NULL, nullable = TRUE))@default_present)
  expect_true(get_spec(prop_string(""))@default_present)
})
