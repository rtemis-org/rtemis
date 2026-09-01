# generate_authoring.R
# ::rtemis::
# 2026- EDG rtemis.org

# Emits the rtemis-wide authoring artifact: for every published schema, which
# properties (if any) are not agent-writable -- the host's to set, never part
# of the question an agent's config answers (`outdir`, `verbosity`).
#
# Deliberately not in the schemas themselves. Who may author a property is a
# fact about workflow policy, and policy can change (a delegated, sandboxed
# agent is a coherent future workflow where the answer flips) while a
# published schema version must not. Read directly from each class's own
# `PropertySpec@agent_writable` -- set only by `prop_host_only()` at class
# declaration -- never from `x-rtemis`, which does not carry it. See
# `PropertySpec@agent_writable`'s doc in `R/010_Props.R` for the full
# reasoning.
#
# Structurally this is `generate_defaults.R` for a different fact: same
# registry walk, same one-file-per-corpus output keyed by schema `$id`, same
# independent versioning. Simpler internals -- no `setup_*()` formals to
# evaluate, since this is read directly off the class.
#
# Run with: Rscript data-raw/generate_authoring.R [SCHEMA_REPO]

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"

source(file.path("data-raw", "schema_registry.R"))


# %% .schema_properties ----
# Property names a generated schema declares, read from the schema itself so
# the artifact can never mark a field the contract does not have.
.schema_properties <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  props <- jsonlite::fromJSON(path, simplifyVector = FALSE)[["properties"]]
  setdiff(names(props), "$schema")
} # /.schema_properties


# %% .host_only_properties ----
# Names of `cls`'s properties, restricted to `wanted`, whose spec is stamped
# `agent_writable = FALSE`. A property built without a `prop_*` factory (a
# nested config, `NULL | SomeConfig`) has no spec and is silently not
# host-only -- there is nothing to stamp, and nothing to mark.
.host_only_properties <- function(cls, wanted) {
  props <- cls@properties
  out <- character()
  for (nm in intersect(wanted, names(props))) {
    spec <- get_spec(props[[nm]])
    if (!is.null(spec) && isTRUE(spec@agent_writable == FALSE)) {
      out <- c(out, nm)
    }
  }
  out
} # /.host_only_properties


# %% .class_authoring ----
# The authoring entry for `schema_path`'s properties, or NULL when none of
# them are host-only.
.class_authoring <- function(cls, schema_path) {
  wanted <- .schema_properties(schema_path)
  if (length(wanted) == 0L) {
    return(NULL)
  }
  host_only <- .host_only_properties(cls, wanted)
  if (length(host_only) == 0L) {
    return(NULL)
  }
  out <- stats::setNames(as.list(rep(FALSE, length(host_only))), host_only)
  out
} # /.class_authoring


# Collect --------------------------------------------------------------------
authoring <- list()

for (family in names(families)) {
  fam <- families[[family]]
  discriminator <- if (is.null(fam[["discriminator"]])) {
    "algorithm"
  } else {
    fam[["discriminator"]]
  }
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    slug <- tolower(discriminator_value(cls, discriminator))
    id <- paste0(base_url, "/", family, "/", slug, "/v1/schema.json")
    path <- file.path(schema_repo, family, slug, "v1", "schema.json")
    a <- .class_authoring(cls, path)
    if (!is.null(a)) {
      authoring[[id]] <- a
    }
  }
  # A dispatcher publishes the discriminator plus any field declared on the
  # family base, which every variant also declares -- read off the first.
  dispatcher_id <- paste0(base_url, "/", family, "/v1/schema.json")
  dispatcher_path <- file.path(schema_repo, family, "v1", "schema.json")
  a <- .class_authoring(fam[["algorithms"]][[1L]][["cls"]], dispatcher_path)
  if (!is.null(a)) {
    authoring[[dispatcher_id]] <- a
  }
}

for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  id <- paste0(base_url, "/", family, "/v1/schema.json")
  path <- file.path(schema_repo, family, "v1", "schema.json")
  a <- .class_authoring(cfg[["cls"]], path)
  if (!is.null(a)) {
    authoring[[id]] <- a
  }
}


# Write ----------------------------------------------------------------------
out_dir <- file.path(schema_repo, "authoring", "v1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "authoring.json")

# Keys sorted so the file diffs cleanly when a single marking changes.
authoring <- authoring[order(names(authoring))]

json <- jsonlite::toJSON(
  list(
    `$id` = paste0(base_url, "/authoring/v1/authoring.json"),
    title = "rtemis authoring",
    description = paste0(
      "Which properties of each schema at schema.rtemis.org are not ",
      "agent-writable -- the host's to set (`outdir`, `verbosity`), never ",
      "part of the question a config answers. A property absent here, or a ",
      "schema absent entirely, has nothing marked: every property is ",
      "agent-writable by default. Workflow policy, not a type fact -- ",
      "versioned independently of the schemas, which are immutable once ",
      "published, and never encoded in a schema's own `x-rtemis` block."
    ),
    authoring = authoring
  ),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  digits = NA
)
write_lines(
  as.character(json),
  file = out_file,
  overwrite = TRUE,
  verbosity = 0L
)

cat(sprintf(
  "authoring markers for %d schemas -> %s\n",
  length(authoring),
  out_file
))
