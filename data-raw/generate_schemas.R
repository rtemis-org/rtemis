# generate_schemas.R
# ::rtemis::
# 2026- EDG rtemis.org

# Single source of truth for the schema.rtemis.org algorithm-family schemas.
# Generates, per family, one leaf schema per algorithm (S7_to_JSONSchema) plus
# the `<family>/v1` dispatcher (S7_dispatcher_JSONSchema), and writes them to
# the schema repo in the uniform `<family>/v1` + `<family>/<algorithm>/v1`
# layout. Run with: Rscript data-raw/generate_schemas.R [SCHEMA_REPO]

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"
# Publication inventory is derived from the package classes.
catalog <- schema_catalog()
families <- catalog$families
flat_configs <- catalog$flat_configs

# The input-schema contract, asserted on every config schema before it is
# written: no required beyond the keys carrying the document's shape, no
# conditional branch demanding a key, no emitted defaults. See
# `rtemis.core::assert_config_contract()` for what each rule prevents.
# From rtemis.core, so rtemis and rtemis.draw publish into one registry under
# one contract. Attached rather than called with `::` at each site: the three
# call sites below read as the rule they are.
assert_config_contract <- rtemis.core::assert_config_contract
# The two prose rules on their own. `assert_config_contract()` applies them too,
# but it governs only documents a caller authors -- so a record and a result
# class, which are exempt from the rules about what a document may demand, would
# otherwise publish descriptions nothing checked. Prose is read by every
# implementation whatever the document asserts, so this runs on all of them.
assert_description_language <- rtemis.core::assert_description_language

# Generation ----------------------------------------------------------------
reference_urls <- list(
  schema = schema_reference_urls(catalog, base_url),
  record = schema_reference_urls(catalog, base_url, record = TRUE)
)
for (family in names(families)) {
  fam <- families[[family]]
  classes <- lapply(fam[["algorithms"]], `[[`, "cls")
  discriminator <- fam[["discriminator"]]
  # Leaves. Each is written twice: the input schema, and its `record.json`
  # sibling, which declares the same properties with every one required. A
  # record states what a run used, so nothing in it falls back to a reader's
  # defaults.
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    slug <- tolower(discriminator_value(cls, discriminator))
    dir <- file.path(schema_repo, family, slug, "v1")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    for (kind in c("schema", "record")) {
      id <- paste0(base_url, "/", family, "/", slug, "/v1/", kind, ".json")
      schema <- S7_to_JSONSchema(
        cls,
        id = id,
        title = algo[["title"]],
        description = algo[["desc"]],
        base = fam[["base_class"]],
        record = kind == "record",
        reference_urls = reference_urls[[kind]],
        # Open: the dispatcher composes the leaf into its own object and
        # closes the whole with `unevaluatedProperties`.
        closed = FALSE
      )
      assert_description_language(schema, id)
      if (kind == "schema") {
        # A leaf requires nothing: the dispatcher declares the discriminator.
        assert_config_contract(schema, id)
      }
      write_JSONSchema(
        schema,
        file.path(dir, paste0(kind, ".json")),
        overwrite = TRUE,
        verbosity = 0L
      )
    }
  }

  # Dispatcher, likewise in both kinds: the record dispatcher routes each
  # variant to its `record.json` rather than its `schema.json`.
  for (kind in c("schema", "record")) {
    dispatcher_id <- paste0(base_url, "/", family, "/v1/", kind, ".json")
    dispatcher <- S7_dispatcher_JSONSchema(
      classes = classes,
      id = dispatcher_id,
      discriminator = discriminator,
      base = fam[["base_class"]],
      record = kind == "record",
      title = fam[["title"]],
      description = fam[["description"]],
      discriminator_description = fam[["discriminator_description"]],
      instance_schema_url = dispatcher_id
    )
    assert_description_language(dispatcher, dispatcher_id)
    if (kind == "schema") {
      # The discriminator is the shape of a dispatched document, not a value a
      # user supplies: without it no variant's schema applies, and `.list_to_*`
      # rejects the document.
      assert_config_contract(
        dispatcher,
        dispatcher_id,
        structural = discriminator
      )
    }
    write_JSONSchema(
      dispatcher,
      file.path(schema_repo, family, "v1", paste0(kind, ".json")),
      overwrite = TRUE,
      verbosity = 0L
    )
  }
  cat(sprintf(
    "%-16s %d leaves + dispatcher (schema + record)\n",
    family,
    length(fam[["algorithms"]])
  ))
}

# Flat configs --------------------------------------------------------------
# Single-object configs (no algorithm discriminator, so no family base class):
# one schema per class, with constraints generated from its declarations.
for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  dir <- file.path(schema_repo, family, "v1")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # A record's own components have no record form: they *are* the record's
  # furniture, not configs a run resolves. Nor do results classes, which are
  # outputs with no input counterpart.
  kinds <- if (cfg[["kind"]] %in% c("component", "report")) {
    "schema"
  } else {
    c("schema", "record")
  }
  for (kind in kinds) {
    id <- paste0(base_url, "/", family, "/v1/", kind, ".json")
    arguments <- list(
      x = cfg[["cls"]],
      id = id,
      title = cfg[["title"]],
      description = cfg[["description"]],
      record = kind == "record",
      asserted = cfg[["kind"]] == "report",
      reference_urls = reference_urls[[kind]],
      instance_schema_url = if (cfg[["kind"]] != "report") id
    )
    if (kind == "record") {
      arguments <- c(
        arguments,
        schema_record_arguments(cfg[["cls"]], catalog, base_url)
      )
    }
    schema <- do.call(S7_to_JSONSchema, arguments)
    assert_description_language(schema, id)
    # The config contract governs documents a caller authors. A results class is
    # not one: its `required` states what rtemis always writes, which is the
    # record's rule rather than the config's.
    if (kind == "schema" && !(cfg[["kind"]] == "report")) {
      assert_config_contract(schema, id)
    }
    write_JSONSchema(
      schema,
      file.path(dir, paste0(kind, ".json")),
      overwrite = TRUE,
      verbosity = 0L
    )
  }
  cat(sprintf("%-16s %s\n", family, paste(kinds, collapse = " + ")))
}
