# generate_defaults.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
schema_repo <- path.expand(if (length(args)) args[[1L]] else "~/Schemas/schema")
base_url <- "https://schema.rtemis.org"
catalog <- schema_catalog()
entries <- default_catalog_entries(catalog, base_url)
declarations <- resolution <- list()
manifest <- list()
for (id in names(entries)) {
  entry <- entries[[id]]
  cls <- entry[["cls"]]
  path <- file.path(schema_repo, entry[["path"]])
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  manifest[[entry[["path"]]]] <- unclass(as.character(openssl::sha256(readBin(
    path,
    "raw",
    n = file.info(path)[["size"]]
  ))))
  declared <- list()
  for (nm in intersect(names(schema[["properties"]]), names(cls@properties))) {
    spec <- get_spec(cls@properties[[nm]])
    if (is.null(spec)) {
      next
    }
    declared <- c(
      declared,
      tryCatch(
        default_declarations(
          spec,
          schema[["properties"]][[nm]],
          paste0("/properties/", default_pointer(nm))
        ),
        error = function(e) {
          rtemis.core::abort(
            cls@name,
            "@",
            nm,
            ": ",
            conditionMessage(e),
            class = "rtemis_schema_error"
          )
        }
      )
    )
  }
  inline_declarations <- function(node, pointer = "") {
    if (!is.list(node)) {
      return(list())
    }
    result <- list()
    identity <- node[["x-rtemis"]][["publication"]][["class"]]
    if (!is.null(identity) && identity %in% names(catalog[["inline"]])) {
      inline_cls <- catalog[["inline"]][[identity]][["cls"]]
      for (nm in names(node[["properties"]])) {
        spec <- get_spec(inline_cls@properties[[nm]])
        if (!is.null(spec)) {
          result <- c(
            result,
            default_declarations(
              spec,
              node[["properties"]][[nm]],
              paste0(pointer, "/properties/", default_pointer(nm))
            )
          )
        }
      }
    }
    for (i in seq_along(node)) {
      key <- if (is.null(names(node))) {
        as.character(i - 1L)
      } else {
        default_pointer(names(node)[[i]])
      }
      result <- c(
        result,
        inline_declarations(node[[i]], paste0(pointer, "/", key))
      )
    }
    result
  }
  declared <- c(declared, inline_declarations(schema))
  declarations[[id]] <- if (length(declared)) {
    declared
  } else {
    stats::setNames(list(), character())
  }
  policies <- list()
  publication <- schema_publication(cls)
  if (publication@kind %in% c("config", "pipeline")) {
    class_policies <- class_default_policies(cls)
    for (nm in names(class_policies)) {
      policy <- class_policies[[nm]]
      fields <- get_spec_fields(cls@properties[[nm]])
      p <- props(policy)
      if (policy@kind == "declaration") {
        if (is.null(fields) || !fields[["default_present"]]) {
          rtemis.core::abort(
            cls@name,
            "@",
            nm,
            " has no declaration default.",
            class = "rtemis_schema_error"
          )
        }
        p[["kind"]] <- "literal"
        p["value"] <- list(default_wire_value(fields[["default"]], fields))
      } else if (policy@kind == "literal") {
        p["value"] <- list(default_wire_value(policy@value, fields))
      }
      if (!is.null(p[["requires"]])) {
        p[["requires"]] <- I(p[["requires"]])
      }
      policies[[nm]] <- p
    }
  }
  resolution[[id]] <- if (length(policies)) {
    policies
  } else {
    stats::setNames(list(), character())
  }
}
schema_files <- sort(list.files(
  schema_repo,
  pattern = "^(schema|record)[.]json$",
  recursive = TRUE
))
for (relative in schema_files) {
  path <- file.path(schema_repo, relative)
  manifest[[relative]] <- unclass(as.character(openssl::sha256(readBin(
    path,
    "raw",
    n = file.info(path)[["size"]]
  ))))
}
manifest <- manifest[sort(names(manifest))]
schemas <- lapply(
  file.path(schema_repo, schema_files),
  jsonlite::fromJSON,
  simplifyVector = FALSE
)
names(schemas) <- vapply(schemas, `[[`, character(1L), "$id")
for (id in names(resolution)) {
  literal <- Filter(
    function(p) identical(p[["kind"]], "literal"),
    resolution[[id]]
  )
  if (!length(literal)) {
    next
  }
  cls <- entries[[id]][["cls"]]
  properties <- schemas[[id]][["properties"]]
  base <- family_base(cls)
  if (!is.null(base)) {
    base_id <- schema_reference_urls(catalog, base_url)[[paste0(
      base@package,
      "::",
      base@name
    )]]
    properties <- c(properties, schemas[[base_id]][["properties"]])
  }
  nms <- intersect(names(literal), names(properties))
  bundle <- default_schema_bundle(
    schemas,
    list(type = "object", properties = properties[nms]),
    id
  )
  valid <- jsonvalidate::json_validate(
    jsonlite::toJSON(
      lapply(literal[nms], `[[`, "value"),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    jsonlite::toJSON(bundle, auto_unbox = TRUE, null = "null", digits = NA),
    engine = "ajv",
    verbose = TRUE
  )
  if (!isTRUE(valid)) {
    print(attr(valid, "errors"))
    rtemis.core::abort(
      "Invalid resolution literal in ",
      id,
      ".",
      class = "rtemis_schema_error"
    )
  }
}
out <- list(
  `$schema` = paste0(base_url, "/defaults/v1/format.schema.json"),
  `$id` = paste0(base_url, "/defaults/v1/defaults.json"),
  format_version = 1L,
  producer = list(
    package = "rtemis",
    version = as.character(utils::packageVersion("rtemis"))
  ),
  schemas = manifest,
  declarations = declarations,
  resolution = resolution
)
out_dir <- file.path(schema_repo, "defaults", "v1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  out,
  file.path(out_dir, "defaults.json"),
  auto_unbox = TRUE,
  null = "null",
  pretty = TRUE,
  digits = NA
)
invisible(file.copy(
  "data-raw/defaults-format.schema.json",
  file.path(out_dir, "format.schema.json"),
  overwrite = TRUE
))
valid <- jsonvalidate::json_validate(
  file.path(out_dir, "defaults.json"),
  "data-raw/defaults-format.schema.json",
  engine = "ajv",
  verbose = TRUE
)
if (!isTRUE(valid)) {
  print(attr(valid, "errors"))
  rtemis.core::abort(
    "Generated defaults artifact failed its format schema.",
    class = "rtemis_schema_error"
  )
}
cat(sprintf(
  "defaults v1: %d schemas, %d declaration nodes -> %s\n",
  length(declarations),
  sum(lengths(declarations)),
  out_dir
))
