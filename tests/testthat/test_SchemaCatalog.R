# test_SchemaCatalog.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("class publication preserves the frozen publication inventory", {
  original <- jsonlite::fromJSON(
    test_path("fixtures", "schema-publication.json"),
    simplifyVector = FALSE
  )
  catalog <- schema_catalog()
  derived <- list(
    families = lapply(catalog[["families"]], function(f) {
      list(
        base_class = f[["base_class"]]@name,
        title = f[["title"]],
        description = f[["description"]],
        discriminator_description = f[["discriminator_description"]],
        algorithms = lapply(f[["algorithms"]], function(a) {
          list(class = a[["cls"]]@name, description = a[["desc"]])
        })
      )
    }),
    documents = lapply(catalog[["flat_configs"]], function(f) {
      list(
        class = f[["cls"]]@name,
        title = f[["title"]],
        description = f[["description"]]
      )
    })
  )
  expect_identical(derived, original)
})


test_that("publication is explicit, non-inheriting, and does not change instances", {
  Parent <- schema_class(
    "PublicationParent",
    package = "rtemis",
    abstract = TRUE,
    properties = list(algorithm = S7::class_character),
    publication = SchemaPublication(
      role = "family",
      description = "Test family.",
      discriminator = "algorithm",
      discriminator_description = "Test algorithm."
    )
  )
  Child <- S7::new_class(
    "PublicationChild",
    package = "rtemis",
    parent = Parent,
    properties = list(algorithm = prop_algorithm("test"))
  )
  expect_null(schema_publication(Child))
  expect_null(attr(Child(), "rtemis_schema", exact = TRUE))
  expect_identical(names(S7::props(Child())), "algorithm")
  # The only descendant is unpublished: discovering the root cannot publish it.
  expect_error(
    schema_catalog(list(Parent, Child)),
    "without leaves",
    class = "rtemis_schema_error"
  )
})


test_that("catalog discovery deduplicates aliases without running constructors", {
  Document <- schema_class(
    "PublicationDocument",
    package = "rtemis",
    constructor = function() {
      stop("constructor must not run")
      S7::new_object(S7::S7_object())
    },
    publication = SchemaPublication(description = "Test document.")
  )
  catalog <- schema_catalog(list(Document, alias = Document))
  expect_named(catalog[["flat_configs"]], "publicationdocument")
  expect_identical(catalog[["flat_configs"]][[1L]][["cls"]], Document)
  expect_identical(
    schema_publication(unserialize(serialize(Document, NULL))),
    schema_publication(Document)
  )
})


test_that("catalog rejects orphan leaves and duplicate publication identities", {
  Orphan <- schema_class(
    "PublicationOrphan",
    package = "rtemis",
    publication = SchemaPublication(role = "leaf", description = "Orphan.")
  )
  expect_error(
    schema_catalog(list(Orphan)),
    "exactly one",
    class = "rtemis_schema_error"
  )
  A <- schema_class(
    "PublicationA",
    package = "rtemis",
    publication = SchemaPublication(slug = "same", description = "A.")
  )
  B <- schema_class(
    "PublicationB",
    package = "rtemis",
    publication = SchemaPublication(slug = "same", description = "B.")
  )
  expect_error(
    schema_catalog(list(A, B)),
    "Duplicate schema slug",
    class = "rtemis_schema_error"
  )
  OtherA <- schema_class(
    "PublicationA",
    package = "rtemis",
    publication = SchemaPublication(description = "Different A.")
  )
  expect_error(
    schema_catalog(list(A, OtherA)),
    "Ambiguous",
    class = "rtemis_schema_error"
  )
  expect_error(SchemaPublication(slug = "../invalid", description = "Invalid."))
  expect_error(SchemaPublication(
    role = "family",
    description = "Missing dispatch."
  ))
})


test_that("group and authorship round-trip independently of schema defaults", {
  Declared <- S7::new_class(
    "PolicyRoundtrip",
    properties = prop_group(
      c(
        prop_host_only(list(workers = prop_integer(2L, min = 1L))),
        list(state = prop_state(prop_string(NULL, nullable = TRUE)))
      ),
      "runtime"
    )
  )
  schema <- S7_to_JSONSchema(
    Declared,
    id = "https://example.org/policy/schema.json"
  )
  Restored <- JSONSchema_to_S7(
    schema,
    defaults = list(workers = 2L),
    authoring = list(workers = FALSE)
  )
  for (nm in names(Declared@properties)) {
    expect_identical(
      spec_fields(get_spec(Restored@properties[[nm]])),
      spec_fields(get_spec(Declared@properties[[nm]]))
    )
    expect_identical(
      prop_role(Restored@properties[[nm]]),
      prop_role(Declared@properties[[nm]])
    )
  }
  expect_error(
    JSONSchema_to_S7(schema, authoring = list(missing = FALSE)),
    "declared property"
  )
  expect_error(
    JSONSchema_to_S7(schema, authoring = list(workers = NA)),
    "logical scalars"
  )
})
