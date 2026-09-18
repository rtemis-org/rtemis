# result_storage.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% result_walk ----
#' Walk portable properties while transforming declared external values
#' @param x Value to visit.
#' @param transform Function receiving a value and its property specification.
#' @param fields Optional property specification fields.
#' @param native Logical: Preserve native values and update S7 properties.
#' @return Wire-ready value.
#' @keywords internal
#' @noRd
result_walk <- function(x, transform, fields = NULL, native = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.null(fields[["alternatives"]])) {
    matches <- vapply(
      fields[["alternatives"]],
      function(spec) {
        is.null(validate_value(x, spec))
      },
      logical(1L)
    )
    if (!any(matches)) {
      stop("Value does not match a declared alternative.")
    }
    return(result_walk(
      x,
      transform,
      fields[["alternatives"]][[which(matches)[[1L]]]],
      native = native
    ))
  }
  if (isTRUE(fields[["external"]])) {
    return(transform(x, fields))
  }
  if (
    !is.null(fields[["items"]]) && fields[["container"]] %in% c("array", "map")
  ) {
    out <- lapply(
      x,
      result_walk,
      transform = transform,
      fields = fields[["items"]],
      native = native
    )
    return(if (fields[["container"]] == "array") unname(out) else out)
  }
  if (!native && !is.null(fields[["schema_choices"]])) {
    return(schema_choice_wire(x, fields))
  }
  if (S7_inherits(x)) {
    cls <- S7_class(x)
    if (native) {
      declared <- cls@properties
      artifact <- attr(cls, "rtemis_artifact_schema")
      names <- if (!is.null(artifact)) {
        names(artifact[["properties"]])
      } else {
        published_prop_names(cls)
      }
      names <- artifact_present_names(x, names)
      values <- lapply(names, function(nm) {
        result_walk(
          prop(x, nm),
          transform,
          get_spec_fields(declared[[nm]]),
          native = TRUE
        )
      })
      props(x) <- stats::setNames(values, names)
      return(x)
    }
    # Use the existing family serializers for configs with computed wire shapes.
    publication <- schema_publication(cls)
    artifact <- attr(cls, "rtemis_artifact_schema")
    observed <- !is.null(publication) && identical(publication@kind, "report")
    if (!observed && is.null(artifact)) {
      return(lapply(serializable_props(x), result_walk, transform = transform))
    }
    names <- if (!is.null(artifact)) {
      names(artifact[["properties"]])
    } else {
      published_prop_names(cls)
    }
    names <- artifact_present_names(x, names)
    out <- lapply(names, function(nm) {
      result_walk(prop(x, nm), transform, get_spec_fields(cls@properties[[nm]]))
    })
    return(stats::setNames(out, names))
  }
  if (!native && !is.null(fields)) {
    x <- wire_value(x, list(spec = fields))
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, result_walk, transform = transform, native = native)
  }
  x
}


# %% result_data_path ----
#' Resolve a contained relative payload path
#' @param base Character: Bundle directory.
#' @param relative Character: Relative path from a DataRef.
#' @return Existing normalized path.
#' @keywords internal
#' @noRd
result_data_path <- function(base, relative) {
  parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  if (
    !nzchar(relative) ||
      startsWith(relative, "/") ||
      grepl("[:\\\\]", relative) ||
      endsWith(relative, "/") ||
      any(parts %in% c("", ".", ".."))
  ) {
    stop(
      "DataRef.path must be a normalized relative path inside the result directory."
    )
  }
  base <- normalizePath(base, mustWork = TRUE)
  path <- normalizePath(file.path(base, relative), mustWork = TRUE)
  if (!startsWith(path, paste0(base, "/"))) {
    stop("DataRef.path escapes the result directory.")
  }
  path
}


# %% result_checked_file ----
#' Verify payload bytes against the reference
#' @param reference DataRef value.
#' @param base Character: Bundle directory.
#' @return Verified file path.
#' @keywords internal
#' @noRd
result_checked_file <- function(reference, base) {
  path <- result_data_path(base, reference@path)
  if (file.size(path) != reference@bytes) {
    stop("Parquet byte size differs from DataRef.bytes.")
  }
  if (!identical(.hash_file(path, reference@algorithm), reference@hash)) {
    stop("Parquet checksum differs from DataRef.hash.")
  }
  path
}


# %% result_inline ----
#' Check and serialize a materialized payload
#' @param x Native value.
#' @param fields Property specification fields.
#' @param native Logical: Retain native value after validation.
#' @return Wire-ready or native value.
#' @keywords internal
#' @noRd
result_inline <- function(x, fields, native = FALSE) {
  fields[["external"]] <- FALSE
  error <- validate_value(x, fields)
  if (!is.null(error)) {
    stop(error)
  }
  if (native) x else wire_value(x, list(spec = fields))
}


# %% result_load_payload ----
#' Load and validate a selected Parquet payload
#' @param reference DataRef value.
#' @param fields Property specification fields.
#' @param base Character: Bundle directory.
#' @param cache Environment: Per-operation verified files and loaded selections.
#' @param native Logical: Retain native payload after validation.
#' @return Wire-ready inline value.
#' @keywords internal
#' @noRd
result_load_payload <- function(
  reference,
  fields,
  base,
  cache,
  native = FALSE
) {
  error <- validate_external_reference(reference, fields)
  if (!is.null(error)) {
    stop(error)
  }
  key <- paste(
    reference@path,
    reference@algorithm,
    reference@hash,
    reference@bytes,
    sep = "\n"
  )
  if (!exists(key, cache, inherits = FALSE)) {
    path <- result_checked_file(reference, base)
    assign(key, nanoparquet::read_parquet_metadata(path), cache)
  }
  metadata <- get(key, cache, inherits = FALSE)
  columns <- metadata[["schema"]][["name"]][
    !is.na(metadata[["schema"]][["r_col"]])
  ]
  if (
    metadata[["file_meta_data"]][["num_rows"]] != reference@n_rows ||
      length(columns) != reference@n_cols
  ) {
    stop("Parquet dimensions differ from DataRef.")
  }
  if (anyDuplicated(columns)) {
    stop("Parquet column names must be distinct.")
  }
  if (!all(reference@columns %in% columns)) {
    stop("DataRef selects a missing Parquet column.")
  }
  selected <- paste0(key, "\n", jsonlite::toJSON(reference@columns))
  if (!exists(selected, cache, inherits = FALSE)) {
    path <- result_data_path(base, reference@path)
    data <- nanoparquet::read_parquet(path, col_select = reference@columns)
    assign(selected, data[reference@columns], cache)
  }
  data <- get(selected, cache, inherits = FALSE)
  x <- switch(
    fields[["container"]],
    array = data[[1L]],
    matrix = unname(as.matrix(data)),
    factor = {
      codes <- data[[1L]]
      if (
        !is.numeric(codes) ||
          inherits(codes, "integer64") ||
          any(
            !is.na(codes) &
              (!is.finite(codes) |
                codes != floor(codes) |
                codes < 1 |
                codes > length(reference@levels))
          )
      ) {
        stop("Categorical codes must index the declared levels.")
      }
      structure(as.integer(codes), levels = reference@levels, class = "factor")
    }
  )
  # Integer Parquet columns from Python use int64. Convert only exact values
  # within R's supported index range; never silently overflow a split index.
  type <- fields[["items"]][["type"]] %||% fields[["type"]]
  if (identical(type, "integer") && !is.factor(x) && !is.integer(x)) {
    if (!is.numeric(x) && !inherits(x, "integer64")) {
      stop("Integer payload must contain numeric integers.")
    }
    numeric <- as.numeric(x)
    if (
      any(
        !is.na(numeric) &
          (!is.finite(numeric) |
            numeric != floor(numeric) |
            abs(numeric) > .Machine[["integer.max"]])
      )
    ) {
      stop("Integer payload exceeds the supported index range.")
    }
    shape <- dim(x)
    x <- as.integer(numeric)
    if (!is.null(shape)) dim(x) <- shape
  }
  result_inline(x, fields, native = native)
}


# %% write_result ----
#' Write a portable result with optional Parquet payloads
#'
#' Eligible outcomes, predictions, probabilities and resampling indices can
#' stay inline or reference compressed Parquet files beside the JSON document.
#' The outer resample structure remains in JSON. Fitted runtime objects are
#' excluded. Move the JSON file and its accompanying directory together.
#'
#' @param x Published S7 report object.
#' @param file Character: Output JSON path.
#' @param storage Character: `"inline"`, `"parquet"`, or `"auto"`.
#' @param inline_max_bytes Numeric: In auto mode, maximum estimated uncompressed
#'   payload bytes kept inline: eight per numeric value or categorical code,
#'   one per Boolean, or UTF-8 string bytes plus eight per string.
#' @param base_dir Character: Directory resolving existing DataRef paths.
#'   Defaults to the output directory.
#' @param overwrite Logical: Replace an existing JSON file.
#'
#' @return `x`, invisibly.
#' @export
#' @examples
#' result <- resample(1:6, setup_KFold(n_resamples = 2L), verbosity = 0L)
#' directory <- tempfile("result-")
#' dir.create(directory)
#' write_result(result, file.path(directory, "result.json"), storage = "parquet")
#' unlink(directory, recursive = TRUE)
write_result <- function(
  x,
  file,
  storage = c("auto", "inline", "parquet"),
  inline_max_bytes = 65536,
  base_dir = dirname(file),
  overwrite = FALSE
) {
  storage <- match.arg(storage)
  if (
    !is.numeric(inline_max_bytes) ||
      length(inline_max_bytes) != 1L ||
      !is.finite(inline_max_bytes) ||
      inline_max_bytes < 0 ||
      inline_max_bytes != floor(inline_max_bytes)
  ) {
    stop("inline_max_bytes must be nonnegative and finite.")
  }
  if (!S7_inherits(x)) {
    stop("write_result requires a published report.")
  }
  cls <- S7_class(x)
  artifact <- attr(cls, "rtemis_artifact_schema")
  publication <- if (is.null(artifact)) schema_publication(cls) else NULL
  kind <- if (is.null(artifact)) {
    publication@kind
  } else {
    artifact[["x-rtemis"]][["publication"]][["kind"]]
  }
  if (!identical(kind, "report")) {
    stop("write_result requires a published report.")
  }
  identity <- if (is.null(artifact)) {
    unname(schema_reference_urls(
      schema_catalog(),
      "https://schema.rtemis.org"
    )[[paste0(cls@package, "::", cls@name)]])
  } else {
    artifact[["$id"]]
  }
  if (file.exists(file) && !overwrite) {
    stop("File exists and overwrite is FALSE.")
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".rtemis-result-", tmpdir = dirname(file))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  destination <- paste0(tools::file_path_sans_ext(basename(file)), ".data")
  if (grepl("[:\\\\]", destination)) {
    stop("Output name must form a portable relative path.")
  }
  target_dir <- file.path(dirname(file), destination)
  if (
    dir.exists(target_dir) &&
      !startsWith(
        normalizePath(target_dir),
        paste0(normalizePath(dirname(file)), "/")
      )
  ) {
    stop("Output data directory escapes the result directory.")
  }
  cache <- new.env(parent = emptyenv())
  transform <- function(value, fields) {
    if (inherits(value, "rtemis::DataRef")) {
      if (storage == "inline") {
        return(result_load_payload(value, fields, base_dir, cache))
      }
      path <- result_checked_file(value, base_dir)
      hash <- .hash_file(path, "sha256")
      name <- paste0(hash, ".parquet")
      if (
        !file.exists(file.path(stage, name)) &&
          !file.copy(path, file.path(stage, name))
      ) {
        stop("Could not stage Parquet file.")
      }
      value@path <- paste0(destination, "/", name)
      value@algorithm <- "sha256"
      value@hash <- hash
      return(S7_to_list(value))
    }
    if (storage == "inline") {
      return(result_inline(value, fields))
    }
    if (storage == "auto") {
      size <- if (is.character(value)) {
        sum(nchar(enc2utf8(value), type = "bytes"), na.rm = TRUE) +
          8 * length(value)
      } else {
        length(value) * if (is.logical(value)) 1 else 8
      }
      if (size <= inline_max_bytes) return(result_inline(value, fields))
    }
    layout <- fields[["container"]]
    table <- if (layout == "matrix") {
      as.data.frame(value)
    } else {
      data.frame(v0 = if (layout == "factor") as.integer(value) else value)
    }
    names(table) <- paste0("v", seq_len(ncol(table)) - 1L)
    temporary <- file.path(stage, "payload.tmp")
    write_parquet_table(table, temporary)
    hash <- .hash_file(temporary, "sha256")
    name <- paste0(hash, ".parquet")
    if (!file.rename(temporary, file.path(stage, name))) {
      stop("Could not stage Parquet file.")
    }
    S7_to_list(DataRef(
      path = paste0(destination, "/", name),
      hash = hash,
      bytes = file.size(file.path(stage, name)),
      n_rows = nrow(table),
      n_cols = ncol(table),
      layout = layout,
      columns = names(table),
      levels = if (layout == "factor") levels(value) else NULL
    ))
  }
  document <- result_walk(x, transform)
  document[["$schema"]] <- identity
  manifest <- file.path(stage, "result.json")
  jsonlite::write_json(
    document,
    manifest,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
  files <- list.files(stage, pattern = "[.]parquet$", full.names = TRUE)
  if (length(files)) {
    dir.create(file.path(dirname(file), destination), showWarnings = FALSE)
  }
  for (source in files) {
    target <- file.path(dirname(file), destination, basename(source))
    if (file.exists(target)) {
      target <- result_data_path(
        dirname(file),
        paste0(destination, "/", basename(source))
      )
      if (
        .hash_file(target, "sha256") !=
          tools::file_path_sans_ext(basename(source))
      ) {
        stop("Existing content-addressed Parquet file is corrupt.")
      }
    } else {
      if (!file.link(source, target)) {
        stop("Could not publish Parquet file.")
      }
    }
  }
  # Published payloads may already belong to a concurrent successful writer.
  # Retain them even when this manifest cannot be published.
  if (overwrite) {
    if (!file.rename(manifest, file)) stop("Could not replace result JSON.")
  } else if (!file.link(manifest, file)) {
    stop("Could not publish result JSON.")
  }
  invisible(x)
}


# %% read_result ----
#' Read a portable result from a local schema registry
#'
#' Reconstruct the report's classes from the supplied schema and defaults
#' artifacts. Parsing does not open referenced files. With `load_data = TRUE`,
#' verify file integrity and dimensions, then validate the materialized values.
#' Native fitted objects are not restored.
#'
#' @param file Character: Result JSON path.
#' @param registry Character: Local registry directory containing versioned
#'   schema documents and `defaults/v1/defaults.json`.
#' @param load_data Logical: Materialize Parquet references.
#'
#' @return An S7 report reconstructed from registry declarations.
#' @export
#' @examples
#' # A local registry is supplied explicitly; reading never fetches schemas.
#' reader <- function(file, registry) read_result(file, registry, load_data = TRUE)
read_result <- function(file, registry, load_data = FALSE) {
  # Union reconstruction may catch rejected branches. Only the final error is
  # actionable; do not print diagnostic banners for successful reads.
  verbosity <- options(rtemis.verbosity = 0L)
  on.exit(options(verbosity), add = TRUE)
  if (!file.exists(file)) {
    stop("Result JSON file does not exist.")
  }
  if (!dir.exists(registry)) {
    stop("Registry must name a local directory.")
  }
  read <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
  paths <- list.files(
    registry,
    pattern = "^(schema|record)[.]json$",
    recursive = TRUE,
    full.names = TRUE
  )
  schemas <- lapply(paths, read)
  names(schemas) <- vapply(schemas, `[[`, character(1L), "$id")
  document <- read(file)
  identity <- document[["$schema"]]
  if (
    !is.character(identity) ||
      length(identity) != 1L ||
      !identity %in% names(schemas)
  ) {
    stop("Result must identify a supported schema with $schema.")
  }
  publication <- schemas[[identity]][["x-rtemis"]][["publication"]]
  if (!identical(publication[["kind"]], "report")) {
    stop("Result must identify a report schema.")
  }
  bundle <- default_schema_bundle(schemas, list(`$ref` = identity), identity)
  validator <- jsonvalidate::json_validator(
    jsonlite::toJSON(bundle, auto_unbox = TRUE, null = "null", digits = NA),
    engine = "ajv"
  )
  validator(file, error = TRUE)
  graph <- default_artifact_graph(
    schemas,
    lapply(
      unique(c(
        "https://schema.rtemis.org/defaults/v1/defaults.json",
        vapply(
          Filter(
            function(schema) {
              !is.null(schema[["x-rtemis"]][["defaults"]])
            },
            schemas
          ),
          function(schema) schema[["x-rtemis"]][["defaults"]],
          character(1L)
        )
      )),
      function(id) {
        prefix <- "https://schema.rtemis.org/"
        if (!startsWith(id, prefix)) {
          stop("Defaults must identify a registry artifact.")
        }
        relative <- substring(id, nchar(prefix) + 1L)
        read(result_data_path(registry, relative))
      }
    )
  )
  result <- graph[["decode"]](document, publication[["class"]])
  if (load_data) {
    cache <- new.env(parent = emptyenv())
    result <- result_walk(
      result,
      function(value, fields) {
        if (inherits(value, "rtemis::DataRef")) {
          result_load_payload(
            value,
            fields,
            dirname(file),
            cache,
            native = TRUE
          )
        } else {
          value
        }
      },
      native = TRUE
    )
  }
  result
}
