# test_SchemaCatalog.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("class publication preserves the frozen publication inventory", {
  original <- jsonlite::fromJSON(
    test_path("fixtures", "schema-publication.json"),
    simplifyVector = FALSE
  )
  catalog <- schema_catalog()
  identities <- names(schema_reference_urls(
    catalog,
    "https://schema.rtemis.org"
  ))
  expect_true(all(startsWith(identities, "rtemis::")))
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


test_that("publication separates generating language from contract scope", {
  shared <- schema_publication_annotation(Implementation)
  expect_identical(shared[["producer"]], "rtemis")
  expect_identical(shared[["language"]], "r")
  expect_identical(shared[["scope"]], "shared")
  expect_identical(shared[["domain"]], "ml")
  expect_identical(
    schema_publication_annotation(KNNHyperparameters)[["scope"]],
    "implementation"
  )
  expect_error(SchemaPublication(description = "Invalid.", scope = "python"))
  expect_error(SchemaPublication(description = "Invalid.", domain = "../ml"))
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


test_that("an authored document may not shadow a referenced family's discriminator", {
  shadowing <- list(
    algorithm = prop_string(NULL, nullable = TRUE, description = "A copy."),
    clustering_config = prop_object(
      ClusteringConfig,
      nullable = TRUE,
      description = "The family that owns the name."
    )
  )
  expect_error(
    schema_class(
      "ShadowingPipeline",
      package = "rtemis",
      properties = shadowing,
      publication = SchemaPublication(
        kind = "pipeline",
        description = "Shadowing."
      )
    ),
    "name the variant once",
    class = "rtemis_schema_error"
  )
  expect_error(
    schema_class(
      "ShadowingConfig",
      package = "rtemis",
      properties = shadowing,
      publication = SchemaPublication(
        kind = "config",
        description = "Shadowing."
      )
    ),
    class = "rtemis_schema_error"
  )
  # A report restates what ran; it is read, not authored, so it may carry the
  # algorithm beside the config that named it.
  expect_s3_class(
    schema_class(
      "RestatingReport",
      package = "rtemis",
      properties = shadowing,
      publication = SchemaPublication(
        kind = "report",
        description = "Restating."
      )
    ),
    "S7_class"
  )
  # A property that is not the discriminator is not a shadow.
  expect_s3_class(
    schema_class(
      "PlainPipeline",
      package = "rtemis",
      properties = list(
        label = prop_string(NULL, nullable = TRUE, description = "A label."),
        clustering_config = shadowing[["clustering_config"]]
      ),
      publication = SchemaPublication(kind = "pipeline", description = "Plain.")
    ),
    "S7_class"
  )
  # A leaf carries its own discriminator beside a same-family reference.
  expect_identical(schema_publication(SuperLearnerHyperparameters)@role, "leaf")
  expect_true(all(
    c("algorithm", "base_learners") %in%
      names(SuperLearnerHyperparameters@properties)
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
    defaults = list(workers = 2L, state = NULL),
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


test_that("implementation paths are language-qualified and shared paths are stable", {
  urls <- schema_reference_urls(schema_catalog(), "https://schema.rtemis.org")
  records <- schema_reference_urls(
    schema_catalog(),
    "https://schema.rtemis.org",
    record = TRUE
  )
  expect_identical(
    unname(urls[["rtemis::Hyperparameters"]]),
    "https://schema.rtemis.org/hyperparameters/r/v1/schema.json"
  )
  expect_identical(
    unname(urls[["rtemis::LightGBMHyperparameters"]]),
    "https://schema.rtemis.org/hyperparameters/r/lightgbm/v1/schema.json"
  )
  expect_identical(
    unname(records[["rtemis::LightGBMHyperparameters"]]),
    "https://schema.rtemis.org/hyperparameters/r/lightgbm/v1/record.json"
  )
  expect_identical(
    unname(urls[["rtemis::Supervised"]]),
    "https://schema.rtemis.org/supervisedresult/v1/schema.json"
  )
  expect_identical(
    unname(urls[["rtemis::Provenance"]]),
    "https://schema.rtemis.org/provenance/v1/schema.json"
  )
  entries <- default_catalog_entries(schema_catalog())
  expect_true(all(names(entries) %in% unname(urls)))
  expect_false(any(grepl("/hyperparameters/v1/", names(entries), fixed = TRUE)))
})


test_that("class closures enclose the namespace, not a definition frame", {
  skip_on_cran()
  ns <- asNamespace("rtemis")
  # Serialized size, writing source-reference files as a name: a package
  # loaded from source keeps them, an installed package does not.
  serialized_size <- function(x) {
    length(serialize(x, NULL, refhook = function(r) {
      if (inherits(r, "srcfile")) "srcfile"
    }))
  }
  # Names the closures of `cls` whose environment is neither `home` nor a
  # child of `home`; binds an S7 class other than the parent class that S7
  # binds for a subclass constructor, as a definition frame binds the class
  # it defines; or serializes larger than its bindings, as when an unforced
  # promise refers to the frame that created the closure.
  enclosing_frames <- function(cls, home) {
    closures <- c(
      list(constructor = cls@constructor, validator = cls@validator),
      unlist(
        lapply(cls@properties, function(p) p[c("getter", "setter")]),
        recursive = FALSE
      )
    )
    closures <- Filter(is.function, closures)
    parent_name <- if (inherits(cls@parent, "S7_class")) cls@parent@name
    bad <- vapply(
      names(closures),
      function(nm) {
        e <- environment(closures[[nm]])
        if (identical(e, home)) {
          return(FALSE)
        }
        if (!identical(parent.env(e), home)) {
          return(TRUE)
        }
        bindings <- mget(ls(e, all.names = TRUE), envir = e)
        bound <- names(Filter(function(x) inherits(x, "S7_class"), bindings))
        allowed <- if (nm == "constructor") parent_name
        length(setdiff(bound, allowed)) > 0L ||
          serialized_size(e) > serialized_size(bindings) + 1e4
      },
      logical(1L)
    )
    names(closures)[bad]
  }
  classes <- Filter(
    function(x) inherits(x, "S7_class"),
    mget(ls(ns, all.names = TRUE), envir = ns)
  )
  frames <- Filter(length, lapply(classes, enclosing_frames, home = ns))
  expect_identical(names(frames), character())

  # A class created inside a function frame encloses that frame.
  definer <- function() {
    payload <- seq_len(1e5) + 0
    S7::new_class("FrameProbe", properties = list(x = S7::class_integer))
  }
  expect_identical(enclosing_frames(definer(), environment()), "constructor")
  # An unforced argument of a closure factory refers to its caller's frame.
  validator_of <- function(v) function(self) v
  caller <- function() {
    payload <- seq_len(1e5) + 0
    validator_of(NULL)
  }
  probe <- S7::new_class("PromiseProbe", validator = caller())
  expect_identical(enclosing_frames(probe, environment()), "validator")
})


test_that("the catalog of published bindings equals a scan of every binding", {
  skip_on_cran()
  ns <- asNamespace("rtemis")
  bindings <- mget(ls(ns, all.names = TRUE), ns, inherits = FALSE)
  expect_setequal(
    ns[[".class_names"]],
    names(Filter(function(x) inherits(x, "S7_class"), bindings))
  )
  expect_identical(
    schema_catalog(),
    schema_catalog(mget(ls(ns, all.names = TRUE), ns, inherits = FALSE))
  )
})
