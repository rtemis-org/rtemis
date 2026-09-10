# schema-roundtrip.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
artifact_dir <- args[[1L]]
report_file <- args[[2L]]
base_url <- "https://schema.rtemis.org"
catalog <- schema_catalog()
families <- catalog$families
flat_configs <- catalog$flat_configs


# %% read_artifact ----
#' Read a generated JSON artifact without simplifying its containers
#' @param path Character: Path relative to the artifact directory.
#' @return Named list.
#' @keywords internal
#' @noRd
read_artifact <- function(path) {
  jsonlite::fromJSON(file.path(artifact_dir, path), simplifyVector = FALSE)
}


# %% compare_fields ----
#' Compare every field, including nested element and member declarations
#' @param original,restored Named list: Property specification fields.
#' @param prefix Character: Field path prefix.
#' @return List of differing paths and their values.
#' @keywords internal
#' @noRd
compare_fields <- function(original, restored, prefix = "") {
  out <- list()
  for (nm in union(names(original), names(restored))) {
    path <- paste0(prefix, nm)
    a <- original[[nm]]
    b <- restored[[nm]]
    if (identical(a, b)) {
      next
    }
    if (is.list(a) && is.list(b) && !is.null(names(a)) && !is.null(names(b))) {
      out <- c(out, compare_fields(a, b, paste0(path, ".")))
    } else {
      out[[path]] <- list(
        original = paste(deparse(a), collapse = " "),
        restored = paste(deparse(b), collapse = " ")
      )
    }
  }
  out
}


# %% Audit ----
defaults <- read_artifact("defaults/v1/defaults.json")[["defaults"]]
authoring <- read_artifact("authoring/v1/authoring.json")[["authoring"]]
entries <- list()
for (family in names(families)) {
  fam <- families[[family]]
  base <- fam[["base_class"]]
  entries[[paste0(family, "/v1/schema.json")]] <- list(
    cls = base,
    properties = family_shared_names(base)
  )
  for (leaf in fam[["algorithms"]]) {
    cls <- leaf[["cls"]]
    slug <- tolower(discriminator_value(
      cls,
      fam[["discriminator"]]
    ))
    entries[[paste0(family, "/", slug, "/v1/schema.json")]] <- list(
      cls = cls,
      properties = own_prop_names(cls, base)
    )
  }
}
for (family in names(flat_configs)) {
  cls <- flat_configs[[family]][["cls"]]
  entries[[paste0(family, "/v1/schema.json")]] <- list(
    cls = cls,
    properties = names(cls@properties)
  )
}

rows <- list()
validators <- list()
for (path in sort(names(entries))) {
  entry <- entries[[path]]
  cls <- entry[["cls"]]
  schema <- read_artifact(path)
  id <- schema[["$id"]]
  ancestor <- cls
  while (
    inherits(ancestor, "S7_class") && !identical(ancestor, S7::S7_object)
  ) {
    native <- schema_native_validator(ancestor)
    if (!is.null(native)) {
      validators[[ancestor@name]] <- list(
        declaring_class = ancestor@name,
        body = paste(deparse(body(native)), collapse = "\n")
      )
    }
    ancestor <- ancestor@parent
  }
  for (nm in intersect(entry[["properties"]], names(schema[["properties"]]))) {
    prop <- cls@properties[[nm]]
    spec <- get_spec(prop)
    row <- list(class = cls@name, schema = id, property = nm)
    if (is.null(spec)) {
      row[["status"]] <- "no_property_spec"
    } else {
      restored <- tryCatch(
        schema_to_spec(schema[["properties"]][[nm]], defaults[[id]][[nm]]),
        error = identity
      )
      if (inherits(restored, "error")) {
        row[["status"]] <- "reader_error"
        row[["error"]] <- conditionMessage(restored)
      } else {
        # Policy is read from the artifact that owns it; the reader's lack of
        # an authoring argument is accounted for separately from lost data.
        restored@agent_writable <- authoring[[id]][[nm]]
        differences <- compare_fields(spec_fields(spec), spec_fields(restored))
        row[["status"]] <- if (length(differences)) "different" else "equal"
        if (length(differences)) row[["differences"]] <- differences
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
}
counts <- table(vapply(rows, `[[`, character(1L), "status"))
report <- list(
  property_count = length(rows),
  schema_count = length(entries),
  counts = as.list(counts),
  properties = rows,
  validators = validators[sort(names(validators))]
)
dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
writeLines(
  as.character(jsonlite::toJSON(
    report,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE,
    digits = NA
  )),
  report_file
)
print(counts)
cat(length(validators), "distinct declaring classes with validators\n")
