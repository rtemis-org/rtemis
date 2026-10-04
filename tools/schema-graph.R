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
  stopifnot(edge[["target"]] %in% ids)
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
  execution = setup_MiraiExecution(
    n_workers = 2L,
    device = setup_CUDA(ids = 0:1),
    seed = 1L
  ),
  device = setup_CUDA(ids = 1L),
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
  namespace <- schema_namespace(
    family,
    schema_catalog()[["families"]][[family]][["base_class"]]
  )
  cases[[paste0(family, "_input")]] <- check_document(
    paste0(family, "_input"),
    paste0(namespace, "/v1/schema.json"),
    S7_to_list(x)
  )
  cases[[paste0(family, "_record")]] <- check_document(
    paste0(family, "_record"),
    paste0(namespace, "/v1/record.json"),
    nested_record(x, x)
  )
}
# A decompose pipeline carries its execution config; the record is a real
# `decomp()` run, written the way `decomp(outdir =)` writes it.
decompose <- setup_DecomposeConfig(
  decomposition_config = setup_PCA(k = 2L),
  execution_config = setup_SerialExecution(n_workers_algorithm = 2L, seed = 1L)
)
cases[["decompose_input"]] <- check_document(
  "decompose_input",
  "decompose/r/v1/schema.json",
  S7_to_list(decompose)
)
mutant <- S7_to_list(decompose)
mutant[["execution_config"]][["backend"]] <- "threads"
cases[["decompose_execution_wrong_backend"]] <- check_document(
  "decompose_execution_wrong_backend",
  "decompose/r/v1/schema.json",
  mutant,
  FALSE
)
decompose_record_file <- tempfile(fileext = ".json")
write_record(
  decomp(
    iris[, 1:4],
    config = setup_PCA(k = 2L),
    execution_config = setup_SerialExecution(seed = 1L),
    verbosity = 0L
  ),
  decompose_record_file,
  verbosity = 0L
)
decompose_record <- structure(
  paste(readLines(decompose_record_file, warn = FALSE), collapse = "\n"),
  class = "json"
)
stopifnot(grepl('"execution_config"', decompose_record, fixed = TRUE))
cases[["decompose_record"]] <- check_document(
  "decompose_record",
  "decompose/r/v1/record.json",
  decompose_record
)
# An autoencoder leaf publishes the settings its unpublished intermediate class
# declares, including those spliced from the torch factories MLP shares. Each
# mutant breaks one constraint of the leaf; the record is a real fit, whose
# derived widths and batch size the record states.
autoencoder_wire <- S7_to_list(setup_Autoencoder(
  k = 2L,
  hidden_units = c(8L, 4L),
  input_noise = 0.1,
  optimizer = "sgd",
  momentum = 0.9
))
cases[["autoencoder_input"]] <- check_document(
  "autoencoder_input",
  "decomposition/r/v1/schema.json",
  autoencoder_wire
)
autoencoder_mutants <- list(
  autoencoder_validation_fraction_one = list(validation_fraction = 1),
  autoencoder_classification_loss = list(loss = "cross_entropy"),
  autoencoder_no_hidden_layers = list(hidden_units = list()),
  autoencoder_momentum_under_adam = list(optimizer = "adam")
)
for (label in names(autoencoder_mutants)) {
  mutant <- autoencoder_wire
  mutant[names(autoencoder_mutants[[label]])] <- autoencoder_mutants[[label]]
  cases[[label]] <- check_document(
    label,
    "decomposition/r/v1/schema.json",
    mutant,
    FALSE
  )
}
if (
  !requireNamespace("torch", quietly = TRUE) || !torch::torch_is_installed()
) {
  stop("The autoencoder record case needs torch with libtorch installed.")
}
autoencoder_record_file <- tempfile(fileext = ".json")
write_record(
  decomp(
    iris[, 1:4],
    config = setup_Autoencoder(max_epochs = 2L),
    execution_config = setup_SerialExecution(seed = 1L),
    verbosity = 0L
  ),
  autoencoder_record_file,
  verbosity = 0L
)
autoencoder_record <- structure(
  paste(readLines(autoencoder_record_file, warn = FALSE), collapse = "\n"),
  class = "json"
)
stopifnot(grepl('"hidden_units": "derived"', autoencoder_record, fixed = TRUE))
cases[["autoencoder_record"]] <- check_document(
  "autoencoder_record",
  "decompose/r/v1/record.json",
  autoencoder_record
)
# The variational leaf shares every autoencoder setting and adds `beta`.
vae_wire <- S7_to_list(setup_VariationalAutoencoder(
  beta = 4,
  input_noise = 0.1
))
cases[["vae_input"]] <- check_document(
  "vae_input",
  "decomposition/r/v1/schema.json",
  vae_wire
)
mutant <- vae_wire
mutant[["beta"]] <- -1
cases[["vae_negative_beta"]] <- check_document(
  "vae_negative_beta",
  "decomposition/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- autoencoder_wire
mutant[["beta"]] <- 4
cases[["autoencoder_beta_undeclared"]] <- check_document(
  "autoencoder_beta_undeclared",
  "decomposition/r/v1/schema.json",
  mutant,
  FALSE
)
vae_record_file <- tempfile(fileext = ".json")
write_record(
  decomp(
    iris[, 1:4],
    config = setup_VariationalAutoencoder(max_epochs = 2L),
    execution_config = setup_SerialExecution(seed = 1L),
    verbosity = 0L
  ),
  vae_record_file,
  verbosity = 0L
)
cases[["vae_record"]] <- check_document(
  "vae_record",
  "decompose/r/v1/record.json",
  structure(
    paste(readLines(vae_record_file, warn = FALSE), collapse = "\n"),
    class = "json"
  )
)
# The distance-based leaves share one `dist_method` vocabulary, which leaves out
# vegdist's dataset-level methods; each mutant breaks one constraint, and each
# record is a real fit.
for (alg in c("MDS", "PCoA")) {
  key <- tolower(alg)
  setup_fn <- get(paste0("setup_", alg))
  wire <- S7_to_list(setup_fn(k = 3L, dist_method = "bray"))
  cases[[paste0(key, "_input")]] <- check_document(
    paste0(key, "_input"),
    "decomposition/r/v1/schema.json",
    wire
  )
  mutant <- wire
  mutant[["dist_method"]] <- "gower"
  label <- paste0(key, "_dataset_level_distance")
  cases[[label]] <- check_document(
    label,
    "decomposition/r/v1/schema.json",
    mutant,
    FALSE
  )
  record_file <- tempfile(fileext = ".json")
  write_record(
    decomp(
      iris[, 1:4],
      config = setup_fn(k = 2L),
      execution_config = setup_SerialExecution(seed = 1L),
      verbosity = 0L
    ),
    record_file,
    verbosity = 0L
  )
  cases[[paste0(key, "_record")]] <- check_document(
    paste0(key, "_record"),
    "decompose/r/v1/record.json",
    structure(
      paste(readLines(record_file, warn = FALSE), collapse = "\n"),
      class = "json"
    )
  )
}
mds_wire <- S7_to_list(setup_MDS())
mutant <- mds_wire
mutant[["model"]] <- "hybrid"
cases[["mds_hybrid_model"]] <- check_document(
  "mds_hybrid_model",
  "decomposition/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- S7_to_list(setup_PCoA())
mutant[["nstart"]] <- 5L
cases[["pcoa_nstart_undeclared"]] <- check_document(
  "pcoa_nstart_undeclared",
  "decomposition/r/v1/schema.json",
  mutant,
  FALSE
)
# The device is a nested family: an unknown type, and GPU ids on a device that
# has none, are both rejected by the schema itself.
execution_wire <- S7_to_list(setup_SerialExecution(device = "cuda", seed = 1L))
mutant <- execution_wire
mutant[["device"]] <- list(type = "tpu")
cases[["execution_device_unknown"]] <- check_document(
  "execution_device_unknown",
  "execution/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- execution_wire
mutant[["device"]] <- list(type = "mps", ids = list(0L))
cases[["execution_device_ids_on_mps"]] <- check_document(
  "execution_device_ids_on_mps",
  "execution/r/v1/schema.json",
  mutant,
  FALSE
)
cases[["execution_device_input"]] <- check_document(
  "execution_device_input",
  "execution/r/v1/schema.json",
  execution_wire
)
cluster_config <- setup_ClusterConfig(
  clustering_config = setup_KMeans(k = 3L),
  execution_config = setup_SerialExecution(seed = 1L)
)
cases[["cluster_input"]] <- check_document(
  "cluster_input",
  "cluster/r/v1/schema.json",
  S7_to_list(cluster_config)
)
cluster_record_file <- tempfile(fileext = ".json")
write_record(
  cluster(
    iris[, 1:4],
    config = setup_KMeans(k = 3L),
    execution_config = setup_SerialExecution(seed = 1L),
    verbosity = 0L
  ),
  cluster_record_file,
  verbosity = 0L
)
cases[["cluster_record"]] <- check_document(
  "cluster_record",
  "cluster/r/v1/record.json",
  structure(
    paste(readLines(cluster_record_file, warn = FALSE), collapse = "\n"),
    class = "json"
  )
)
preprocessor <- setup_Preprocessor(
  scale = TRUE,
  scale_centers = c(candidates = 1.5)
)
preprocessor_record <- nested_record(preprocessor, preprocessor)
cases[["candidate_named_map_record"]] <- check_document(
  "candidate_named_map_record",
  "preprocessor/r/v1/record.json",
  preprocessor_record
)
preprocessor_record[["scale_centers"]][["candidates"]] <- "invalid"
cases[["candidate_named_map_wrong_type"]] <- check_document(
  "candidate_named_map_wrong_type",
  "preprocessor/r/v1/record.json",
  preprocessor_record,
  FALSE
)
mlp <- setup_MLP(hidden_units = tune_over(c(12L, 6L), c(24L, 12L)))
mlp_wire <- S7_to_list(mlp)
mlp_restored <- .list_to_Hyperparameters(jsonlite::fromJSON(
  jsonlite::toJSON(mlp_wire, auto_unbox = TRUE, null = "null"),
  simplifyVector = FALSE
))
stopifnot(identical(
  mlp_restored@hidden_units@candidates,
  mlp@hidden_units@candidates
))
cases[["vector_candidate_input"]] <- check_document(
  "vector_candidate_input",
  "hyperparameters/r/v1/schema.json",
  mlp_wire
)
mlp_wire[["hidden_units"]][["candidates"]][[1L]][[1L]] <- 1.5
cases[["vector_candidate_wrong_type"]] <- check_document(
  "vector_candidate_wrong_type",
  "hyperparameters/r/v1/schema.json",
  mlp_wire,
  FALSE
)
hp <- configs[["hyperparameters"]]
wire <- S7_to_list(hp)
stopifnot(identical(names(wire[["base_learners"]]), c("clinical", "imaging")))
restored <- .list_to_Hyperparameters(jsonlite::fromJSON(
  jsonlite::toJSON(wire, auto_unbox = TRUE, null = "null"),
  simplifyVector = FALSE
))
stopifnot(identical(names(restored@base_learners), names(hp@base_learners)))
mutant <- wire
mutant[["base_learners"]] <- mutant[["base_learners"]][1L]
cases[["library_short"]] <- check_document(
  "library_short",
  "hyperparameters/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- wire
mutant[["base_learners"]][["imaging"]] <- S7_to_list(setup_KMeans())
cases[["library_wrong_family"]] <- check_document(
  "library_wrong_family",
  "hyperparameters/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- wire
mutant[["base_learners"]] <- unname(mutant[["base_learners"]])
cases[["library_array"]] <- check_document(
  "library_array",
  "hyperparameters/r/v1/schema.json",
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
cases[["library_empty_key"]] <- check_document(
  "library_empty_key",
  "hyperparameters/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- nested_record(hp, hp)
mutant[["base_learners"]][["imaging"]][["origin"]] <- NULL
cases[["library_missing_origin"]] <- check_document(
  "library_missing_origin",
  "hyperparameters/r/v1/record.json",
  mutant,
  FALSE
)
cases[["diagnostics_empty"]] <- check_document(
  "diagnostics_empty",
  "diagnostics/r/v1/schema.json",
  record_object(Diagnostics())
)
writeup_idx <- c(1:40, 51:90, 101:140)
writeup_record <- record_object(writeup(train(
  iris[writeup_idx, ],
  dat_test = iris[-writeup_idx, ],
  hyperparameters = setup_CART(maxdepth = tune_over(2L, 3L)),
  verbosity = 0L
)))
cases[["writeup_record"]] <- check_document(
  "writeup_record",
  "supervisedwriteup/v1/schema.json",
  writeup_record
)
mutant <- writeup_record
mutant[["sections"]][[1L]][["part"]] <- "discussion"
cases[["writeup_wrong_part"]] <- check_document(
  "writeup_wrong_part",
  "supervisedwriteup/v1/schema.json",
  mutant,
  FALSE
)
mutant <- writeup_record
mutant[["review"]][["baseline"]][["resamples_compared"]] <- NULL
mutant[["review"]][["algorithm"]] <- "NotAnAlgorithm"
cases[["writeup_review_wrong_algorithm"]] <- check_document(
  "writeup_review_wrong_algorithm",
  "supervisedwriteup/v1/schema.json",
  mutant,
  FALSE
)
cases[["diagnostics_wrong"]] <- check_document(
  "diagnostics_wrong",
  "diagnostics/r/v1/schema.json",
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
cases[["named_set"]] <- check_document(
  "named_set",
  "supervised/r/v1/schema.json",
  payload
)
set_record <- nested_record(set, set)
stopifnot(
  identical(names(set_record), "variants"),
  identical(names(set_record[["variants"]]), c("cart", "linear"))
)
set_schema <- documents[[
  "https://schema.rtemis.org/supervised/r/v1/record.json"
]][["properties"]][["hyperparameters"]]
set_id <- "https://schema.rtemis.org/test/set-record/schema.json"
documents[[set_id]] <- list(
  `$id` = set_id,
  type = "object",
  properties = list(hyperparameters = set_schema)
)
ids <- c(ids, set_id)
keys[[set_id]] <- "set_record_probe"
cases[["named_set_record"]] <- check_document(
  "named_set_record",
  "test/set-record/schema.json",
  list(hyperparameters = set_record)
)
mutant <- payload
mutant[["hyperparameters"]][["variants"]][[
  "linear"
]] <- S7_to_list(setup_CART())
cases[["set_mixed_algorithms"]] <- check_document(
  "set_mixed_algorithms",
  "supervised/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- payload
mutant[["hyperparameters"]][["variants"]] <- structure(
  list(),
  names = character()
)
cases[["set_empty"]] <- check_document(
  "set_empty",
  "supervised/r/v1/schema.json",
  mutant,
  FALSE
)
mutant <- payload
mutant[["hyperparameters"]][["algorithm"]] <- "LINAD"
cases[["set_extra_key"]] <- check_document(
  "set_extra_key",
  "supervised/r/v1/schema.json",
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
  function(case) identical(case[["actual"]], case[["expected"]]),
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
