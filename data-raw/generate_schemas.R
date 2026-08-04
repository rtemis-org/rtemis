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
# `$ref`d by every top-level record. The two schemas that *make up* a record --
# provenance and the fingerprints it holds -- do not carry one themselves.
provenance_url <- paste0(base_url, "/provenance/v1/schema.json")
record_parts <- c("provenance", "datafingerprint")
# Results classes: what a run produced, not a config a run resolves. They have
# no input form, so no `record.json` either -- the whole document is a report.
result_classes <- c(
  "regressionmetrics",
  "classificationmetrics",
  "regressionmetricsres",
  "classificationmetricsres"
)
# Only a *pipeline* record represents a run, so only one carries provenance. A
# component config (preprocessor, execution, ...) has a record form too, but it
# appears nested inside a pipeline record and takes provenance from there;
# requiring its own would demand a second copy of the same block.
pipeline_records <- c("supervised", "decompose", "cluster")
# `folds` is supervised-specific: only a supervised run fits a model per outer
# resample, each resolving its own values.
fold_refs <- c(
  hyperparameters = paste0(base_url, "/hyperparameters/v1/record.json"),
  preprocessor_config = paste0(base_url, "/preprocessor/v1/record.json"),
  decomposition_config = paste0(base_url, "/decomposition/v1/record.json")
)
# What the run scored. Which of the two applies follows from the outcome, so a
# record admits either and the reader takes whichever validates.
metrics_refs <- c(
  regression = paste0(base_url, "/regressionmetrics/v1/schema.json"),
  classification = paste0(base_url, "/classificationmetrics/v1/schema.json")
)

# Registry ------------------------------------------------------------------
# Per family: the base class, the payload field name, the dispatcher's title and
# descriptions, and the per-algorithm classes with a one-line description, and
# an optional `extra` supplying cross-field constraints that are not
# per-property. Which properties take part is decided by their declared role
# (see `prop_role()`), not listed here: run state is dropped, and a spec-less
# property aborts generation.
source(file.path("data-raw", "schema_registry.R"))

# The input-schema contract, asserted on every config schema before it is
# written: no required beyond the keys carrying the document's shape, no
# conditional branch demanding a key, no emitted defaults. See
# `schema_contract.R` for what each rule prevents.
source(file.path("data-raw", "schema_contract.R"))

# Generation ----------------------------------------------------------------
for (family in names(families)) {
  fam <- families[[family]]
  classes <- lapply(fam[["algorithms"]], `[[`, "cls")
  discriminator <- if (is.null(fam[["discriminator"]])) {
    "algorithm"
  } else {
    fam[["discriminator"]]
  }
  # `payload = NULL` (top-level mode) needs open leaves so the dispatcher's
  # `unevaluatedProperties` can account for them.
  top_level <- !("payload" %in% names(fam)) || is.null(fam[["payload"]])
  payload <- if (top_level) NULL else fam[["payload"]]

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
        title = paste0("rtemis ", cls@name),
        description = algo[["desc"]],
        base = fam[["base_class"]],
        record = kind == "record",
        # A leaf is nested under its dispatcher, which carries the block.
        extra = algo[["extra"]],
        refs = algo[["refs"]],
        closed = !top_level
      )
      if (kind == "schema") {
        # A leaf carries neither: the dispatcher declares the discriminator
        # and, in nested-payload mode, holds the leaf under the payload key.
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
      payload = payload,
      base = fam[["base_class"]],
      record = kind == "record",
      title = fam[["title"]],
      description = fam[["description"]],
      discriminator_description = if (
        is.null(fam[["discriminator_description"]])
      ) {
        fam[["algorithm_description"]]
      } else {
        fam[["discriminator_description"]]
      },
      instance_schema_url = dispatcher_id
    )
    if (kind == "schema") {
      # `{discriminator, payload}` is the shape of a dispatched document, not a
      # value a user supplies: without the payload key there is nothing for the
      # selected variant's schema to apply to, and `.list_to_*` rejects the
      # document. Top-level mode has no payload, so the discriminator alone.
      assert_config_contract(
        dispatcher,
        dispatcher_id,
        structural = c(discriminator, payload)
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
# one schema per class. `extra` supplies cross-field constraints.
for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  dir <- file.path(schema_repo, family, "v1")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # A record's own components have no record form: they *are* the record's
  # furniture, not configs a run resolves. Nor do results classes, which are
  # outputs with no input counterpart.
  kinds <- if (family %in% c(record_parts, result_classes)) {
    "schema"
  } else {
    c("schema", "record")
  }
  for (kind in kinds) {
    id <- paste0(base_url, "/", family, "/v1/", kind, ".json")
    schema <- S7_to_JSONSchema(
      cfg[["cls"]],
      id = id,
      title = cfg[["title"]],
      description = cfg[["description"]],
      record = kind == "record",
      provenance_url = if (kind == "record" && family %in% pipeline_records) {
        provenance_url
      },
      fold_refs = if (kind == "record" && family == "supervised") fold_refs,
      metrics_refs = if (kind == "record" && family == "supervised") {
        metrics_refs
      },
      extra = cfg[["extra"]],
      refs = cfg[["refs"]],
      array_refs = cfg[["array_refs"]],
      # A config instance self-identifies with `$schema`; a results object does
      # not, being produced by rtemis rather than authored against a schema, so
      # declaring the field would put a key in the contract that nothing writes.
      instance_schema_url = if (!(family %in% result_classes)) id
    )
    if (kind == "schema") {
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

# `supervised/v1` is now generated from `SuperConfig` (with `$ref`s to the
# family schemas), so the hand-authored hyperparameters `allOf` and its
# drift check are retired: the references cannot drift from the classes.
