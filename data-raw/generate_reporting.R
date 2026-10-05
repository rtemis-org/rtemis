# generate_reporting.R
# ::rtemis::
# 2026- EDG rtemis.org

# Emits the rtemis-wide reporting artifact: for every published hyperparameters
# leaf, the primary hyperparameters a writeup's main table lists for every fit.
#
# Which hyperparameters matter in a paper is an editorial judgment, partly
# dependent on the field and the data, so it is published beside the schemas
# and versioned independently of them, as the authoring artifact is. Read from
# each class's own `schema_class(reporting = )` declaration
# (`schema_reporting()`), which does not inherit.
#
# spec: rtemis/writeup#hyperparameter-tables
#
# Run with: Rscript data-raw/generate_reporting.R [SCHEMA_REPO]

suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/write_json.R")

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"

families <- schema_catalog()[["families"]]

reporting <- list()
for (family in names(families)) {
  fam <- families[[family]]
  namespace <- schema_namespace(family, fam[["base_class"]])
  discriminator <- fam[["discriminator"]]
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    declared <- schema_reporting(cls)
    if (is.null(declared)) {
      next
    }
    slug <- tolower(discriminator_value(cls, discriminator))
    id <- paste0(base_url, "/", namespace, "/", slug, "/v1/schema.json")
    # I(): a one-element list stays a JSON array.
    reporting[[id]] <- list(primary = I(declared[["primary"]]))
  }
}

# Keys sorted so the file diffs cleanly when a single declaration changes.
reporting <- reporting[order(names(reporting))]

out_file <- file.path(schema_repo, "reporting", "v1", "reporting.json")
write_json_document(
  list(
    `$id` = paste0(base_url, "/reporting/v1/reporting.json"),
    title = "rtemis reporting",
    description = paste0(
      "For each hyperparameters schema at schema.rtemis.org, the primary ",
      "hyperparameters a writeup's main hyperparameter table lists for every ",
      "fit, beside every hyperparameter that was tuned, supplied, or selected ",
      "during fitting. Editorial metadata, versioned independently of the ",
      "schemas."
    ),
    reporting = reporting
  ),
  out_file
)

cat(sprintf(
  "reporting for %d schemas -> %s\n",
  length(reporting),
  out_file
))
