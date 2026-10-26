# 047_IngestConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# Settings for reading a data file and writing it to Parquet, and `ingest()`,
# which executes them.
#
# `IngestConfig` is an abstract family discriminated by `format`. Each leaf
# holds the settings of one file format: a delimited file has a separator and a
# quote character, a spreadsheet has a sheet number, and Parquet, RDS, Stata,
# and ARFF files have no format-specific settings beyond the reader.
#
# `format` is required in a config document. `ingest()` derives the format from
# the file extension and raises an error when the config states another.

# Column types a config can declare: the profile dtype vocabulary without
# `other`, which describes types outside the vocabulary (a BLOB, a struct) and
# has no conversion.
INGEST_DTYPES <- setdiff(PROFILE_DTYPES, "other")


# %% IngestConfig ----
#' IngestConfig Class
#'
#' @description
#' Abstract base for the ingest family. Holds the settings shared by every
#' format: declared column types and the operations applied after reading.
#'
#' @author EDG
#' @noRd
IngestConfig <- schema_class(
  name = "IngestConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    format = class_character,
    # Declared types, keyed by column name. `apply_declared_types()` converts
    # each named column after reading.
    columns = prop_map(
      prop_string(INGEST_DTYPES[[1L]], enum = INGEST_DTYPES),
      nullable = TRUE,
      description = paste0(
        "Declared column types, keyed by column name, in the `profile/r/v1` ",
        "vocabulary. A column named here is converted to the declared type ",
        "after reading; other columns keep the type the reader assigns."
      )
    ),
    # Applied after reading, for every format.
    character2factor = prop_boolean(
      TRUE,
      description = "Convert string columns to categorical columns."
    ),
    clean_colnames = prop_boolean(
      TRUE,
      description = "Normalize column names on read."
    ),
    remove_duplicates = prop_boolean(
      FALSE,
      description = "Drop rows that repeat an earlier row exactly."
    )
  ),
  publication = SchemaPublication(
    role = "family",
    slug = "ingest",
    title = "rtemis IngestConfig",
    description = "Language-independent config for reading a data file and writing it to Parquet. `format` names the file format and selects the settings that apply to it; a config whose `format` differs from the format of the file is an error. `columns` declares column types, which replace the types the reader assigns.",
    discriminator = "format",
    discriminator_description = "What the file is.",
    order = 3L
  )
) # /rtemis::IngestConfig


# %% DelimitedIngestConfig ----
#' DelimitedIngestConfig Class
#'
#' @description
#' Settings for reading a delimited file: the reader library, field separator,
#' quote character, and missing-value strings.
#'
#' @author EDG
#' @noRd
DelimitedIngestConfig <- schema_class(
  name = "DelimitedIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(
    format = prop_algorithm("delimited"),
    # Readers differ in type inference and in what they read as missing, so the
    # config names the reader.
    reader = prop_string(
      "data.table",
      enum = c("data.table", "vroom", "duckdb", "arrow"),
      description = "Library used to read the file."
    ),
    sep = prop_string(
      NULL,
      nullable = TRUE,
      description = "Field separator. Unset lets the reader detect it."
    ),
    quote = prop_string("\"", description = "Quote character."),
    na_strings = prop_string(
      "",
      vector = TRUE,
      description = "Strings to read as missing."
    )
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "A delimited file (csv, tsv, ...).",
    order = 1L
  )
) # /rtemis::DelimitedIngestConfig


# %% ParquetIngestConfig ----
#' ParquetIngestConfig Class
#'
#' @description
#' Settings for reading a Parquet file, which stores its column types: the
#' reader library.
#'
#' @author EDG
#' @noRd
ParquetIngestConfig <- schema_class(
  name = "ParquetIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(
    format = prop_algorithm("parquet"),
    reader = prop_string(
      "arrow",
      enum = c("arrow", "nanoparquet"),
      description = "Library used to read the file."
    )
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "A Parquet file, which declares its own types.",
    order = 2L
  )
) # /rtemis::ParquetIngestConfig


# %% XLSXIngestConfig ----
#' XLSXIngestConfig Class
#'
#' @description
#' Settings for reading a spreadsheet: the sheet number and missing-value
#' strings.
#'
#' @author EDG
#' @noRd
XLSXIngestConfig <- schema_class(
  name = "XLSXIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(
    format = prop_algorithm("xlsx"),
    sheet = prop_integer(
      1L,
      min = 1L,
      description = "Sheet number to read."
    ),
    na_strings = prop_string(
      "",
      vector = TRUE,
      description = "Strings to read as missing."
    )
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "A spreadsheet.",
    order = 3L
  )
) # /rtemis::XLSXIngestConfig


# %% RDSIngestConfig ----
#' RDSIngestConfig Class
#'
#' @description
#' Settings for reading an RDS file, which stores R types. The format has no
#' settings of its own; the leaf states the format.
#'
#' @author EDG
#' @noRd
RDSIngestConfig <- schema_class(
  name = "RDSIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("rds")),
  publication = SchemaPublication(
    role = "leaf",
    description = "An RDS file.",
    order = 4L
  )
) # /rtemis::RDSIngestConfig


# %% DTAIngestConfig ----
#' DTAIngestConfig Class
#'
#' @description
#' Settings for reading a Stata file, which stores its column types. The format
#' has no settings of its own.
#'
#' @author EDG
#' @noRd
DTAIngestConfig <- schema_class(
  name = "DTAIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("dta")),
  publication = SchemaPublication(
    role = "leaf",
    description = "A Stata file.",
    order = 5L
  )
) # /rtemis::DTAIngestConfig


# %% ARFFIngestConfig ----
#' ARFFIngestConfig Class
#'
#' @description
#' Settings for reading an ARFF file, whose header declares the attribute types.
#' The format has no settings of its own.
#'
#' @author EDG
#' @noRd
ARFFIngestConfig <- schema_class(
  name = "ARFFIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("arff")),
  publication = SchemaPublication(
    role = "leaf",
    description = "An ARFF file.",
    order = 6L
  )
) # /rtemis::ARFFIngestConfig


# The leaf class and constructor of each format. `ingest()` selects the class
# for the format it derives, and `.list_to_IngestConfig()` dispatches a parsed
# document through the matching `setup_*()`.
INGEST_CLASSES <- list(
  delimited = DelimitedIngestConfig,
  parquet = ParquetIngestConfig,
  xlsx = XLSXIngestConfig,
  rds = RDSIngestConfig,
  dta = DTAIngestConfig,
  arff = ARFFIngestConfig
)

INGEST_SETUP <- c(
  delimited = "setup_DelimitedIngest",
  parquet = "setup_ParquetIngest",
  xlsx = "setup_XLSXIngest",
  rds = "setup_RDSIngest",
  dta = "setup_DTAIngest",
  arff = "setup_ARFFIngest"
)


# The arguments every format takes, documented in full on each constructor:
#
# @param columns Optional Named vector: Declared column types, keyed by column
# name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
# here is converted to the declared type after reading, replacing the type the
# reader assigned.
# @param character2factor Logical: If TRUE, read character columns as factors.
# @param clean_colnames Logical: If TRUE, normalize column names.
# @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.

# %% setup_DelimitedIngest ----
#' Set up a `DelimitedIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read a delimited file.
#'
#' Each format has its own constructor, which takes the settings of that format
#' only; an argument of another format is an unused-argument error.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#' @param reader Character \{"data.table", "vroom", "duckdb", "arrow"\}: Library
#' used to read the file.
#' @param sep Optional Character: Field separator. If `NULL`, the reader detects it.
#' @param quote Character: Quote character.
#' @param na_strings Character vector: Strings to read as missing.
#'
#' @return `DelimitedIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_DelimitedIngest(sep = ";")
setup_DelimitedIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE,
  reader = "data.table",
  sep = NULL,
  quote = "\"",
  na_strings = ""
) {
  apply_setup_defaults(DelimitedIngestConfig)
  DelimitedIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates,
    reader = reader,
    sep = sep,
    quote = quote,
    na_strings = na_strings
  )
} # /rtemis::setup_DelimitedIngest


# %% setup_ParquetIngest ----
#' Set up a `ParquetIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read a Parquet file. Parquet stores
#' its column types, and the only format-specific setting is the reader library.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#' @param reader Character \{"arrow", "nanoparquet"\}: Library used to read the
#' file.
#'
#' @return `ParquetIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_ParquetIngest()
setup_ParquetIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE,
  reader = "arrow"
) {
  apply_setup_defaults(ParquetIngestConfig)
  ParquetIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates,
    reader = reader
  )
} # /rtemis::setup_ParquetIngest


# %% setup_XLSXIngest ----
#' Set up an `XLSXIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read a spreadsheet.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#' @param sheet Integer [1, Inf): Sheet number to read.
#' @param na_strings Character vector: Strings to read as missing.
#'
#' @return `XLSXIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_XLSXIngest(sheet = 2L)
setup_XLSXIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE,
  sheet = 1L,
  na_strings = ""
) {
  apply_setup_defaults(XLSXIngestConfig)
  XLSXIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates,
    sheet = clean_posint(sheet),
    na_strings = na_strings
  )
} # /rtemis::setup_XLSXIngest


# %% setup_RDSIngest ----
#' Set up an `RDSIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read an RDS file. RDS stores R types,
#' and the format has no settings of its own.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#'
#' @return `RDSIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_RDSIngest()
setup_RDSIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE
) {
  apply_setup_defaults(RDSIngestConfig)
  RDSIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates
  )
} # /rtemis::setup_RDSIngest


# %% setup_DTAIngest ----
#' Set up a `DTAIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read a Stata file. Stata files store
#' their column types, and the format has no settings of its own.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#'
#' @return `DTAIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_DTAIngest()
setup_DTAIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE
) {
  apply_setup_defaults(DTAIngestConfig)
  DTAIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates
  )
} # /rtemis::setup_DTAIngest


# %% setup_ARFFIngest ----
#' Set up an `ARFFIngestConfig`
#'
#' @description
#' Creates the settings `ingest()` uses to read an ARFF file. The ARFF header
#' declares the attribute types, and the format has no settings of its own.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/r/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted to the declared type after reading, replacing the type the
#' reader assigned.
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#'
#' @return `ARFFIngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_ARFFIngest()
setup_ARFFIngest <- function(
  columns = NULL,
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE
) {
  apply_setup_defaults(ARFFIngestConfig)
  ARFFIngestConfig(
    columns = columns,
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates
  )
} # /rtemis::setup_ARFFIngest


# %% ingest_format ----
#' The ingest format a file's extension names
#'
#' @param path Character: Path to the file, or an HTTP or HTTPS URL, whose
#' extension is read from the last segment of the URL path.
#'
#' @return Character, or NULL where the extension names no supported format.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ingest_format <- function(path) {
  if (is_url(path)) {
    path <- url_filename(path)
  }
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    parquet = "parquet",
    rds = "rds",
    xlsx = "xlsx",
    dta = "dta",
    arff = "arff",
    # `read()`'s own fallthrough: anything else is handed to the delimited
    # reader, which detects the separator.
    csv = "delimited",
    tsv = "delimited",
    txt = "delimited",
    gz = "delimited",
    NULL
  )
} # /rtemis::ingest_format


# %% ingest_display_name ----
#' The file name of an ingest input, for messages
#'
#' @param path Character: Path to the file, or an HTTP or HTTPS URL.
#'
#' @return Character: the base name of a local path, or `url_filename()` of a
#' URL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ingest_display_name <- function(path) {
  if (is_url(path)) url_filename(path) else basename(path)
} # /rtemis::ingest_display_name


# %% apply_declared_types ----
#' Convert the columns the config declares types for
#'
#' Converts each column named in `columns` to its declared type. Runs after
#' reading, so a declared type replaces the type the reader assigned.
#'
#' @param x `data.table`: The frame as read.
#' @param columns Named character or NULL: Declared types by column name.
#'
#' @return The frame, converted.
#'
#' @author EDG
#' @keywords internal
#' @noRd
apply_declared_types <- function(x, columns) {
  if (is.null(columns) || length(columns) == 0L) {
    return(x)
  }
  missing_cols <- setdiff(names(columns), names(x))
  if (length(missing_cols) > 0L) {
    rtemis.core::abort(
      "The config declares types for columns that are not in the data: ",
      paste0("'", missing_cols, "'", collapse = ", "),
      ". Columns are: ",
      paste0("'", names(x), "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  for (nm in names(columns)) {
    x[[nm]] <- switch(
      columns[[nm]],
      number = as.numeric(x[[nm]]),
      integer = as.integer(x[[nm]]),
      categorical = as.factor(x[[nm]]),
      string = as.character(x[[nm]]),
      boolean = as.logical(x[[nm]]),
      temporal = as.Date(x[[nm]]),
      x[[nm]]
    )
  }
  x
} # /rtemis::apply_declared_types


# %% ingest ----
#' Read a data file and normalize it to Parquet
#'
#' @description
#' Reads a data file with the settings of an `IngestConfig`, converts the
#' columns it declares types for, writes the result to a Parquet file, and
#' returns a manifest of the operation.
#'
#' The file extension names the format. A `config` whose `format` differs from
#' it is an error.
#'
#' `path` may be an `http://` or `https://` URL or an `hf://datasets/` path, as
#' `read()` accepts them, downloaded to a temporary file. The manifest records
#' the path as given in `input`, the time the download started in
#' `retrieved_at`, and, for an `hf://` path, the path pinned to the resolved
#' commit and file in `input_resolved`.
#'
#' The manifest also records the input and output files, the SHA-256 of the
#' input, a `DataFingerprint` of the data, the config, and the engine that
#' produced the Parquet file.
#'
#' @param path Character: Path to the input file, an `http://` or `https://`
#' URL, or an `hf://datasets/` path.
#' @param outfile Character: Path to write the Parquet file to.
#' @param config Optional `IngestConfig`: How to read it. If `NULL`, the
#' defaults for the file's own format.
#' @param overwrite Logical: If TRUE, overwrite `outfile` if it exists.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: the ingest manifest, with `input`, `input_resolved`
#' (`hf://datasets/<repo>@<commit>/<file>` for an `hf://` input, otherwise
#' NULL), `input_sha256` (SHA-256 of the input file's bytes), `retrieved_at`
#' (ISO 8601 UTC time the download started, or NULL for a local file), `output`,
#' `format`, `config`, `engine`, `rtemis_version`, `data_input`, `n_rows`, and
#' `n_cols`.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' ingest("data.csv", "data.parquet")
#' ingest("data.csv", "data.parquet", setup_DelimitedIngest(sep = ";"))
#' ingest("https://example.org/data/iris.csv", "iris.parquet")
#' ingest("hf://datasets/rtemis/tcga-pancan-hiseq", "tcga.parquet")
#' }
ingest <- function(
  path,
  outfile,
  config = NULL,
  overwrite = FALSE,
  verbosity = 1L
) {
  check_dependencies("arrow")
  remote <- is_url(path) || is_hf(path)
  if (!remote) {
    path <- sanitize_path(path, must_exist = TRUE)
  }
  outfile <- sanitize_path(outfile, must_exist = FALSE, normalize = FALSE)
  if (file.exists(outfile) && !overwrite) {
    rtemis.core::abort(
      "`outfile` exists: ",
      outfile,
      ". Pass `overwrite = TRUE` to replace it.",
      class = c("rtemis_file_error", "rtemis_input_error")
    )
  }

  retrieved_at <- if (remote) iso8601(Sys.time())
  # An `hf://` path is resolved to a commit and a file before the format check:
  # a path naming the repository only carries no extension.
  hf <- if (is_hf(path)) resolve_hf(path, verbosity = verbosity)
  name <- if (is.null(hf)) {
    ingest_display_name(path)
  } else {
    basename(hf[["file"]])
  }
  format <- ingest_format(name)
  if (is.null(format)) {
    rtemis.core::abort(
      "Cannot ingest '",
      name,
      "': its extension names no supported format.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  config <- config %||% INGEST_CLASSES[[format]]()
  check_is_S7(config, IngestConfig)
  if (!identical(config@format, format)) {
    rtemis.core::abort(
      "The config is for a '",
      config@format,
      "' file, but '",
      name,
      "' is a '",
      format,
      "' file. Pass a config with `format = \"",
      format,
      "\"`, or omit `config`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Every reading setting passed to `read()` comes from the config, so the
  # manifest reports each one. Only the settings of this format are passed.
  local_path <- if (!is.null(hf)) {
    download_hf(hf, verbosity = verbosity)
  } else if (remote) {
    download_url(path, verbosity = verbosity)
  } else {
    path
  }
  if (remote) {
    on.exit(unlink(dirname(local_path), recursive = TRUE), add = TRUE)
  }
  input_sha256 <- file_sha256(local_path)
  args <- list(
    local_path,
    character2factor = config@character2factor,
    clean_colnames = config@clean_colnames,
    remove_duplicates = config@remove_duplicates,
    verbosity = verbosity
  )
  per_format <- switch(
    format,
    delimited = list(
      delim_reader = config@reader,
      sep = config@sep,
      quote = config@quote,
      na_strings = config@na_strings
    ),
    parquet = list(parquet_reader = config@reader),
    xlsx = list(xlsx_sheet = config@sheet, na_strings = config@na_strings),
    list()
  )
  dat <- apply_declared_types(
    do.call(read, c(args, per_format)),
    config@columns
  )
  arrow::write_parquet(dat, outfile)

  msg0(
    bold(highlight("\U25B6")),
    " Ingested ",
    highlight(name),
    " to ",
    highlight(basename(outfile)),
    verbosity = verbosity
  )

  # Returned as a manifest; an ingest node has no published record schema.
  list(
    input = path,
    input_resolved = if (!is.null(hf)) {
      paste0(
        "hf://datasets/",
        hf[["repo"]],
        "@",
        hf[["commit"]],
        "/",
        hf[["file"]]
      )
    },
    input_sha256 = input_sha256,
    retrieved_at = retrieved_at,
    output = outfile,
    format = format,
    config = S7_to_list(config),
    engine = "R",
    rtemis_version = as.character(utils::packageVersion("rtemis")),
    data_input = S7_to_list(data_fingerprint(dat)),
    n_rows = as.integer(NROW(dat)),
    n_cols = as.integer(NCOL(dat))
  )
} # /rtemis::ingest


# %% .list_to_IngestConfig ----
#' Rebuild an `IngestConfig` from a parsed config document
#'
#' `format` selects the constructor, and every other key is passed as one of
#' its arguments. A key of another format is an unused-argument error that
#' names the key.
#'
#' @param x Named list: The parsed document.
#'
#' @return An `IngestConfig` subclass.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_IngestConfig <- function(x) {
  # Every key any format admits: the base's, plus each leaf's own. A key of
  # another format reaches the constructor of this one, which raises an
  # unused-argument error naming it.
  keys <- unique(unlist(c(
    list(names(IngestConfig@properties)),
    lapply(INGEST_CLASSES, function(cls) names(cls@properties))
  )))
  check_wire_keys(x, keys, "ingest config")
  args <- .drop_meta_keys(x)
  # A JSON object parses to a list; `columns` is a map of scalars, so it is the
  # named character vector the property declares.
  if (!is.null(args[["columns"]])) {
    args[["columns"]] <- unlist(args[["columns"]])
  }
  format <- args[["format"]]
  if (is.null(format) || !format %in% names(INGEST_SETUP)) {
    rtemis.core::abort(
      "An ingest config needs a `format`, one of: ",
      paste0("'", names(INGEST_SETUP), "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  args[["format"]] <- NULL
  do.call(INGEST_SETUP[[format]], args)
} # /rtemis::.list_to_IngestConfig
