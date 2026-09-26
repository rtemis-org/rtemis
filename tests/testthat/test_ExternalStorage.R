# test_ExternalStorage.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% storage_fixture ----
#' Construct a small artifact-only report from source declarations
#' @param directory Character: Temporary registry directory.
#' @return Typed report with array, matrix and categorical values.
#' @keywords internal
#' @noRd
storage_fixture <- function(directory) {
  Report <- schema_class(
    "StorageFixture",
    package = "rtemis",
    properties = list(
      values = prop_external(prop_array(prop_float(NULL, nullable = TRUE))),
      indices = prop_external(prop_integer(min = 1L, vector = TRUE)),
      categories = prop_external(prop_factor(allow_missing = TRUE)),
      probabilities = prop_external(prop_matrix(
        items = prop_float(NULL, nullable = TRUE, min = 0, max = 1)
      ))
    ),
    publication = SchemaPublication(
      kind = "report",
      scope = "shared",
      description = "Storage fixture."
    )
  )
  classes <- list(Report, DataRef)
  ids <- c(
    "https://example.test/report/schema.json",
    "https://schema.rtemis.org/dataref/v1/schema.json"
  )
  urls <- c("rtemis::StorageFixture" = ids[[1L]], "rtemis::DataRef" = ids[[2L]])
  schemas <- lapply(seq_along(classes), function(i) {
    S7_to_JSONSchema(
      classes[[i]],
      id = ids[[i]],
      instance_schema_url = ids[[i]],
      asserted = i == 1L,
      reference_urls = urls
    )
  })
  names(schemas) <- ids
  declarations <- lapply(seq_along(classes), function(i) {
    unlist(
      lapply(published_prop_names(classes[[i]]), function(nm) {
        default_declarations(
          get_spec(classes[[i]]@properties[[nm]]),
          schemas[[i]][["properties"]][[nm]],
          paste0("/properties/", nm)
        )
      }),
      recursive = FALSE
    )
  })
  names(declarations) <- ids
  defaults <- list(format_version = 1L, declarations = declarations)
  for (i in seq_along(schemas)) {
    dir <- file.path(directory, if (i == 1L) "report" else "dataref", "v1")
    dir.create(dir, recursive = TRUE)
    jsonlite::write_json(
      schemas[[i]],
      file.path(dir, "schema.json"),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
  }
  dir.create(file.path(directory, "defaults/v1"), recursive = TRUE)
  jsonlite::write_json(
    defaults,
    file.path(directory, "defaults/v1/defaults.json"),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  graph <- default_artifact_graph(schemas, defaults)
  report <- Report(
    values = c(1, NA, 3),
    indices = c(1L, 2L, 3L),
    categories = factor(c("b", NA, "a"), levels = c("b", "a", "unused")),
    probabilities = matrix(c(0.1, NA, 0.3, 0.9, NA, 0.7), ncol = 2L)
  )
  graph[["decode"]](
    jsonlite::fromJSON(
      jsonlite::toJSON(
        S7_to_list(report),
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      ),
      simplifyVector = FALSE
    ),
    "rtemis::StorageFixture"
  )
}


test_that("external property axes roundtrip with absent and present defaults", {
  properties <- list(
    prop_integer(min = 1L, vector = TRUE),
    prop_array(prop_float(NULL, nullable = TRUE)),
    prop_factor(allow_missing = TRUE),
    prop_matrix(items = prop_float(NULL, nullable = TRUE, min = 0, max = 1))
  )
  for (property in properties) {
    spec <- get_spec(prop_external(property))
    schema <- spec_to_schema(spec)
    declarations <- default_declarations(spec, schema, "/properties/value")
    restored <- schema_to_spec(
      schema,
      declarations = declarations,
      path = "/properties/value"
    )
    expect_identical(spec_fields(restored), spec_fields(spec))
    expect_true(schema[["x-rtemis"]][["external"]])
    expect_identical(
      schema[["anyOf"]][[2L]][["properties"]][["layout"]][["const"]],
      spec@container
    )
  }
  expect_error(prop_external(prop_string()), "External")
  expect_error(prop_external(prop_object(DataRef)), "External")
})


test_that("Parquet reports load lazily and materialize without value loss", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  registry <- file.path(directory, "registry")
  report <- storage_fixture(registry)
  report@values <- c(pi, NA_real_, sqrt(2))
  file <- file.path(directory, "result.json")
  expected <- S7_to_list(report)
  for (storage in c("inline", "parquet", "auto")) {
    write_result(
      report,
      file,
      storage = storage,
      inline_max_bytes = 0,
      overwrite = TRUE
    )
    loaded <- read_result(file, registry, load_data = TRUE)
    expect_equal(S7_to_list(loaded), expected)
    lazy <- read_result(file, registry)
    if (storage != "inline") {
      expect_identical(loaded@values, report@values)
      expect_identical(loaded@categories, report@categories)
      expect_identical(loaded@probabilities, report@probabilities)
      expect_true(inherits(lazy@values, "rtemis::DataRef"))
      expect_identical(lazy@categories@levels, c("b", "a", "unused"))
      expect_identical(lazy@probabilities@columns, c("v0", "v1"))
    }
  }
  expect_error(write_result(report, file), "exists")
  lazy <- read_result(file, registry)
  payload <- file.path(directory, lazy@values@path)
  writeBin(as.raw(0:3), payload)
  expect_true(inherits(read_result(file, registry)@values, "rtemis::DataRef"))
  expect_error(read_result(file, registry, load_data = TRUE), "byte size")
})


test_that("Parquet references enforce metadata and original value constraints", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  registry <- file.path(directory, "registry")
  report <- storage_fixture(registry)
  file <- file.path(directory, "result.json")
  write_result(report, file, storage = "parquet")
  original <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  write <- function(document) {
    jsonlite::write_json(
      document,
      file,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
  }
  for (change in list(
    list(layout = "factor"),
    list(columns = list("v0", "v0")),
    list(bytes = 1.5),
    list(levels = list("a"))
  )) {
    document <- original
    for (nm in names(change)) {
      document[["values"]][nm] <- change[nm]
    }
    write(document)
    expect_error(
      read_result(file, registry),
      info = jsonlite::toJSON(
        document[["values"]],
        auto_unbox = TRUE,
        null = "null"
      )
    )
  }
  document <- original
  document[["values"]][["bytes"]] <- NULL
  write(document)
  expect_error(
    read_result(file, registry),
    info = jsonlite::toJSON(
      document[["values"]],
      auto_unbox = TRUE,
      null = "null"
    )
  )
  document <- original
  document[["values"]][["path"]] <- "../outside.parquet"
  write(document)
  expect_true(inherits(read_result(file, registry)@values, "rtemis::DataRef"))
  expect_error(read_result(file, registry, load_data = TRUE), "relative path")
  document <- original
  reference <- document[["probabilities"]]
  payload <- file.path(directory, reference[["path"]])
  write_parquet_table(
    data.frame(v0 = c(1.2, 0.2, 0.3), v1 = c(0.8, 0.8, 0.7)),
    payload
  )
  document[["probabilities"]][["bytes"]] <- file.size(payload)
  document[["probabilities"]][["hash"]] <- .hash_file(payload, "sha256")
  write(document)
  expect_error(read_result(file, registry, load_data = TRUE), "<= 1")
})


test_that("integer payloads reject logical and character columns without coercion", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  registry <- file.path(directory, "registry")
  report <- storage_fixture(registry)
  file <- file.path(directory, "result.json")
  write_result(report, file, storage = "parquet")
  document <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  payload <- file.path(directory, document[["indices"]][["path"]])
  for (values in list(c(TRUE, FALSE, TRUE), c("1", "2", "3"), c(1, 2.5, 3))) {
    write_parquet_table(data.frame(v0 = values), payload)
    document[["indices"]][["bytes"]] <- file.size(payload)
    document[["indices"]][["hash"]] <- .hash_file(payload, "sha256")
    jsonlite::write_json(
      document,
      file,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    expect_error(
      read_result(file, registry, load_data = TRUE),
      "Integer payload"
    )
  }
})


test_that("external descriptors support files larger than the native integer limit", {
  fields <- spec_fields(get_spec(prop_external(prop_integer(
    min = 1L,
    vector = TRUE
  ))))
  reference <- DataRef(
    path = "large.parquet",
    hash = strrep("0", 64),
    bytes = 3 * 1024^3,
    n_rows = 1L,
    n_cols = 1L,
    layout = "array",
    columns = "v0"
  )
  expect_null(validate_external_reference(reference, fields))
})
