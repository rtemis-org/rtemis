# schema-graph.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
artifact_dir <- args[[1L]]
report_file <- args[[2L]]
paths <- list.files(
  artifact_dir,
  pattern = "^(schema|record)\\.json$",
  recursive = TRUE
)
documents <- lapply(file.path(artifact_dir, paths), function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
})
ids <- vapply(documents, `[[`, character(1L), "$id")
stopifnot(!anyDuplicated(ids))
names(documents) <- ids
keys <- stats::setNames(paste0("s", seq_along(ids)), ids)


# %% collect_refs ----
#' Collect every reference in a generated document
#' @param node JSON value: Node to inspect.
#' @return Character vector of reference URIs.
#' @keywords internal
#' @noRd
collect_refs <- function(node) {
  if (!is.list(node)) {
    return(character())
  }
  c(node[["$ref"]], unlist(lapply(node, collect_refs), use.names = FALSE))
}


# %% reference_id ----
#' Resolve a reference to a document in the generated graph
#' @param ref Character: Reference URI.
#' @param owner Character: Containing document identity.
#' @return Character scalar document identity.
#' @keywords internal
#' @noRd
reference_id <- function(ref, owner) {
  if (startsWith(ref, "#")) owner else sub("#.*$", "", ref)
}


# %% localize ----
#' Relocate references into a bundle while retaining their full constraints
#' @param node JSON value: Node to relocate.
#' @param owner Character: Containing document identity.
#' @return JSON value with bundle-local references.
#' @keywords internal
#' @noRd
localize <- function(node, owner) {
  if (!is.list(node)) {
    return(node)
  }
  ref <- node[["$ref"]]
  if (!is.null(ref)) {
    target <- reference_id(ref, owner)
    if (!target %in% ids) {
      stop("Unresolved reference: ", ref, " in ", owner)
    }
    fragment <- if (grepl("#", ref, fixed = TRUE)) {
      sub("^[^#]*#", "", ref)
    } else {
      ""
    }
    if (nzchar(fragment) && !startsWith(fragment, "/")) {
      stop("Unsupported anchor: ", ref)
    }
    node[["$ref"]] <- paste0("#/$defs/", keys[[target]], fragment)
  }
  lapply(node, localize, owner = owner)
}


# %% validator_for ----
#' Compile a document with its actual transitive reference closure
#' @param id Character: Root schema identity.
#' @return Validation function backed by Ajv.
#' @keywords internal
#' @noRd
validator_for <- function(id) {
  closure <- character()
  pending <- id
  while (length(pending)) {
    owner <- pending[[1L]]
    pending <- pending[-1L]
    if (owner %in% closure) {
      next
    }
    if (!owner %in% ids) {
      stop("Unresolved document: ", owner)
    }
    closure <- c(closure, owner)
    refs <- collect_refs(documents[[owner]])
    pending <- unique(c(
      pending,
      vapply(refs, reference_id, character(1L), owner = owner)
    ))
  }
  defs <- lapply(closure, function(owner) {
    doc <- documents[[owner]]
    doc[["$id"]] <- NULL
    localize(doc, owner)
  })
  names(defs) <- keys[closure]
  jsonvalidate::json_validator(
    jsonlite::toJSON(
      list(
        `$schema` = "https://json-schema.org/draft/2020-12/schema",
        `$ref` = paste0("#/$defs/", keys[[id]]),
        `$defs` = defs
      ),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    engine = "ajv"
  )
}


# %% Graph audit ----
generated_documents <- length(ids)
edges <- lapply(ids, function(id) {
  refs <- collect_refs(documents[[id]])
  lapply(refs, function(ref) {
    list(source = id, target = reference_id(ref, id), reference = ref)
  })
})
edges <- unlist(edges, recursive = FALSE)
for (edge in edges) {
  stopifnot(edge$target %in% ids)
}
for (id in ids) {
  validator <- validator_for(id)
  rm(validator)
}
cat(
  length(ids),
  "generated schemas compile;",
  length(edges),
  "references resolve.\n"
)


# %% check_document ----
#' Check a real wire document or an isolated mutation
#' @param label Character: Case identity.
#' @param path Character: Schema path under the publication base URL.
#' @param value JSON-compatible value: Document to validate.
#' @param expected Logical: Expected acceptance.
#' @return Named list with case result and validator diagnostics.
#' @keywords internal
#' @noRd
check_document <- function(label, path, value, expected = TRUE) {
  validator <- validator_for(paste0("https://schema.rtemis.org/", path))
  json <- if (inherits(value, "json")) {
    value
  } else {
    jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )
  }
  result <- validator(json, verbose = TRUE)
  list(
    case = label,
    schema = path,
    expected = expected,
    actual = isTRUE(result),
    errors = attr(result, "errors")
  )
}


# %% Document corpus ----
cases <- list()
configs <- list(
  execution = setup_MiraiExecution(n_workers = 2L, seed = 1L),
  clustering = setup_KMeans(k = 3L),
  decomposition = setup_PCA(k = 2L),
  resampler = setup_KFold(3L),
  tuner = setup_GridSearch(resampler_config = setup_KFold(3L)),
  ingest = setup_DelimitedIngest(),
  partition = setup_RandomPartition(),
  conformal = setup_SplitConformal(),
  explanation = setup_SHAP(),
  hyperparameters = setup_SuperLearner(
    base_learners = list(clinical = setup_GLM(), imaging = setup_CART())
  )
)
for (family in names(configs)) {
  x <- configs[[family]]
  cases[[paste0(family, "_input")]] <- check_document(
    paste0(family, "_input"),
    paste0(family, "/v1/schema.json"),
    S7_to_list(x)
  )
  cases[[paste0(family, "_record")]] <- check_document(
    paste0(family, "_record"),
    paste0(family, "/v1/record.json"),
    nested_record(x, x)
  )
}
hp <- configs$hyperparameters
wire <- S7_to_list(hp)
stopifnot(identical(names(wire$base_learners), c("clinical", "imaging")))
restored <- .list_to_Hyperparameters(jsonlite::fromJSON(
  jsonlite::toJSON(wire, auto_unbox = TRUE, null = "null"),
  simplifyVector = FALSE
))
stopifnot(identical(names(restored@base_learners), names(hp@base_learners)))
mutant <- wire
mutant$base_learners <- mutant$base_learners[1L]
cases$library_short <- check_document(
  "library_short",
  "hyperparameters/v1/schema.json",
  mutant,
  FALSE
)
mutant <- wire
mutant$base_learners$imaging <- S7_to_list(setup_KMeans())
cases$library_wrong_family <- check_document(
  "library_wrong_family",
  "hyperparameters/v1/schema.json",
  mutant,
  FALSE
)
mutant <- wire
mutant$base_learners <- unname(mutant$base_learners)
cases$library_array <- check_document(
  "library_array",
  "hyperparameters/v1/schema.json",
  mutant,
  FALSE
)
mutant <- structure(
  sub(
    '"imaging":',
    '"":',
    jsonlite::toJSON(wire, auto_unbox = TRUE, null = "null"),
    fixed = TRUE
  ),
  class = "json"
)
cases$library_empty_key <- check_document(
  "library_empty_key",
  "hyperparameters/v1/schema.json",
  mutant,
  FALSE
)
mutant <- nested_record(hp, hp)
mutant$base_learners$imaging$origin <- NULL
cases$library_missing_origin <- check_document(
  "library_missing_origin",
  "hyperparameters/v1/record.json",
  mutant,
  FALSE
)
cases$diagnostics_empty <- check_document(
  "diagnostics_empty",
  "diagnostics/v1/schema.json",
  record_object(Diagnostics())
)
cases$diagnostics_wrong <- check_document(
  "diagnostics_wrong",
  "diagnostics/v1/schema.json",
  list(diagnostics = list(S7_to_list(setup_GLM()))),
  FALSE
)
set <- as_HyperparametersSet(list(
  cart = setup_LINAD(node_model = "constant"),
  linear = setup_LINAD()
))
pipeline <- setup_SuperConfig(
  hyperparameters = set,
  execution_config = setup_SerialExecution()
)
payload <- S7_to_list(pipeline)
cases$named_set <- check_document(
  "named_set",
  "supervised/v1/schema.json",
  payload
)
set_record <- nested_record(set, set)
stopifnot(
  identical(names(set_record), "variants"),
  identical(names(set_record$variants), c("cart", "linear"))
)
set_schema <- documents[[
  "https://schema.rtemis.org/supervised/v1/record.json"
]]$properties$hyperparameters
set_id <- "https://schema.rtemis.org/test/set-record/schema.json"
documents[[set_id]] <- list(
  `$id` = set_id,
  type = "object",
  properties = list(hyperparameters = set_schema)
)
ids <- c(ids, set_id)
keys[[set_id]] <- "set_record_probe"
cases$named_set_record <- check_document(
  "named_set_record",
  "test/set-record/schema.json",
  list(hyperparameters = set_record)
)
mutant <- payload
mutant$hyperparameters$variants$linear <- S7_to_list(setup_CART())
cases$set_mixed_algorithms <- check_document(
  "set_mixed_algorithms",
  "supervised/v1/schema.json",
  mutant,
  FALSE
)
mutant <- payload
mutant$hyperparameters$variants <- structure(list(), names = character())
cases$set_empty <- check_document(
  "set_empty",
  "supervised/v1/schema.json",
  mutant,
  FALSE
)
mutant <- payload
mutant$hyperparameters$algorithm <- "LINAD"
cases$set_extra_key <- check_document(
  "set_extra_key",
  "supervised/v1/schema.json",
  mutant,
  FALSE
)
decoded <- jsonlite::fromJSON(
  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
  simplifyVector = FALSE
)
restored <- .list_to_SuperConfig(decoded)
stopifnot(identical(
  names(restored@hyperparameters@variants),
  names(set@variants)
))

ok <- vapply(
  cases,
  function(case) identical(case$actual, case$expected),
  logical(1L)
)
report <- list(
  documents = generated_documents,
  references = edges,
  cases = cases,
  passed = sum(ok),
  failed = sum(!ok)
)
dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report,
  report_file,
  auto_unbox = TRUE,
  null = "null",
  pretty = TRUE
)
cat(
  sum(ok),
  "document cases agree;",
  sum(!ok),
  "failures. Report:",
  report_file,
  "\n"
)
if (!all(ok)) {
  stop("Document corpus failed: ", paste(names(cases)[!ok], collapse = ", "))
}
