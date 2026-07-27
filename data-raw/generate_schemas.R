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

# Registry ------------------------------------------------------------------
# Per family: the base class, the payload field name, the dispatcher's title /
# descriptions / extra top-level properties, and the per-algorithm classes with
# a one-line description, and an optional `extra` supplying cross-field
# constraints that are not per-property. Which properties take part is decided
# by their declared role (see `prop_role()`), not listed here: run state is
# dropped, and a spec-less property aborts generation.
source(file.path("data-raw", "schema_registry.R"))

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

  # Leaves.
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    slug <- tolower(discriminator_value(cls, discriminator))
    id <- paste0(base_url, "/", family, "/", slug, "/v1/schema.json")
    schema <- S7_to_JSONSchema(
      cls,
      id = id,
      title = paste0("rtemis ", cls@name),
      description = algo[["desc"]],
      base = fam[["base_class"]],
      extra = algo[["extra"]],
      refs = algo[["refs"]],
      closed = !top_level
    )
    dir <- file.path(schema_repo, family, slug, "v1")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    write_JSONSchema(
      schema,
      file.path(dir, "schema.json"),
      overwrite = TRUE,
      verbosity = 0L
    )
  }

  # Dispatcher.
  dispatcher_id <- paste0(base_url, "/", family, "/v1/schema.json")
  dispatcher <- S7_dispatcher_JSONSchema(
    classes = classes,
    id = dispatcher_id,
    discriminator = discriminator,
    payload = payload,
    title = fam[["title"]],
    description = fam[["description"]],
    discriminator_description = if (
      is.null(fam[["discriminator_description"]])
    ) {
      fam[["algorithm_description"]]
    } else {
      fam[["discriminator_description"]]
    },
    extra_properties = if (is.null(fam[["extra_properties"]])) {
      list()
    } else {
      fam[["extra_properties"]]
    },
    # Translate `required_except` (property -> variants to skip) into the
    # per-variant `required` form the dispatcher generator consumes.
    variant_required = local({
      # The dispatcher keys `variant_required` by the raw discriminator value.
      values <- vapply(
        classes,
        function(cls) discriminator_value(cls, discriminator),
        character(1L)
      )
      req <- list()
      for (prop_name in names(fam[["required_except"]])) {
        except <- fam[["required_except"]][[prop_name]]
        for (v in setdiff(values, except)) {
          req[[v]] <- c(req[[v]], prop_name)
        }
      }
      req
    }),
    instance_schema_url = dispatcher_id
  )
  write_JSONSchema(
    dispatcher,
    file.path(schema_repo, family, "v1", "schema.json"),
    overwrite = TRUE,
    verbosity = 0L
  )
  cat(sprintf(
    "%-16s %d leaves + dispatcher\n",
    family,
    length(fam[["algorithms"]])
  ))
}

# Flat configs --------------------------------------------------------------
# Single-object configs (no algorithm discriminator, so no family base class):
# one schema per class. `extra` supplies cross-field constraints.
for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  id <- paste0(base_url, "/", family, "/v1/schema.json")
  schema <- S7_to_JSONSchema(
    cfg[["cls"]],
    id = id,
    title = cfg[["title"]],
    description = cfg[["description"]],
    extra = cfg[["extra"]],
    refs = cfg[["refs"]],
    instance_schema_url = id
  )
  dir <- file.path(schema_repo, family, "v1")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_JSONSchema(
    schema,
    file.path(dir, "schema.json"),
    overwrite = TRUE,
    verbosity = 0L
  )
  cat(sprintf("%-16s flat config schema\n", family))
}

# `supervised/v1` is now generated from `SuperConfig` (with `$ref`s to the
# family schemas), so the hand-authored hyperparameters `allOf` and its
# drift check are retired: the references cannot drift from the classes.
