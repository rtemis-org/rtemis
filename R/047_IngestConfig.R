# 047_IngestConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# How a data file is read and normalized to Parquet -- the first step of any
# pipeline, and the only one that decides types.
#
# A family rather than one flat config, for the reason every other rtemis family
# is one: a delimited file has a separator and a quote character, a spreadsheet
# has a sheet number, and a Parquet has neither. A single config carrying all of
# them offers a user settings that do nothing, and offers an agent settings it
# can put in a document that will never take effect. `format` is the
# discriminator and the leaf holds what that format actually has.
#
# `format` has no default. It is not a preference -- it is what the file *is* --
# so a config either states it, and can be checked against the file, or the
# reader guesses and no one can tell what was read. Stating it is what makes a
# mismatch a finding rather than a silent reinterpretation.

# The dtypes a user can *declare*. `other` is the fallback for what nothing else
# matches -- a BLOB, a struct -- and is a description, never a conversion
# target. Derived from the profile vocabulary so a dtype added there arrives
# here without a second list to update.
INGEST_DTYPES <- setdiff(PROFILE_DTYPES, "other")


# %% IngestConfig ----
#' IngestConfig Class
#'
#' @description
#' Abstract base for the ingest family. Holds what applies whatever the file is:
#' the operations performed *after* reading, and the types the user declares.
#'
#' @author EDG
#' @noRd
IngestConfig <- new_class(
  name = "IngestConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    format = class_character,
    # Declared types, keyed by column name. Inference is a guess made from the
    # values a file happens to contain -- an all-integer column of codes reads
    # as a number, a column of "0"/"1" as an integer -- and the guess decides
    # what a learner is handed. Where the user knows, saying so beats every
    # heuristic, and the record then reports what was declared rather than what
    # was inferred.
    columns = prop_map(
      prop_string(INGEST_DTYPES[[1L]], enum = INGEST_DTYPES),
      nullable = TRUE,
      description = paste0(
        "Declared column types, keyed by column name, in the `profile/v1` ",
        "vocabulary. A column named here is converted after reading rather ",
        "than inferred; one absent is left as the reader gave it."
      )
    ),
    # Applied after reading, so they belong to every format.
    character2factor = prop_boolean(
      TRUE,
      description = paste0(
        "Read character columns as factors. Supervised learning needs ",
        "categorical predictors as factors, and most file formats cannot say ",
        "which columns those are."
      )
    ),
    clean_colnames = prop_boolean(
      TRUE,
      description = "Normalize column names on read."
    ),
    remove_duplicates = prop_boolean(
      FALSE,
      description = "Drop rows that repeat an earlier row exactly."
    )
  )
) # /rtemis::IngestConfig


# %% DelimitedIngestConfig ----
#' DelimitedIngestConfig Class
#'
#' @description
#' Reading a delimited file: the format that declares nothing, and therefore the
#' one with the most to say.
#'
#' @author EDG
#' @noRd
DelimitedIngestConfig <- new_class(
  name = "DelimitedIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(
    format = prop_algorithm("delimited"),
    # Readers disagree -- on type inference, on what counts as missing -- so a
    # record that did not name the reader could not account for its own numbers.
    reader = prop_string(
      "data.table",
      enum = c("data.table", "vroom", "duckdb", "arrow"),
      description = "Library used to read the file."
    ),
    sep = prop_string(
      NULL,
      nullable = TRUE,
      description = "Field separator. NULL lets the reader detect it."
    ),
    quote = prop_string("\"", description = "Quote character."),
    na_strings = prop_string(
      "",
      vector = TRUE,
      description = "Strings to read as missing."
    )
  )
) # /rtemis::DelimitedIngestConfig


# %% ParquetIngestConfig ----
#' ParquetIngestConfig Class
#'
#' @description
#' Reading a Parquet file. It declares its own types, so there is nothing to
#' tell the reader except which reader to be.
#'
#' @author EDG
#' @noRd
ParquetIngestConfig <- new_class(
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
  )
) # /rtemis::ParquetIngestConfig


# %% XLSXIngestConfig ----
#' XLSXIngestConfig Class
#'
#' @description
#' Reading a spreadsheet. A workbook holds several tables, so which one is the
#' only thing that must be said.
#'
#' @author EDG
#' @noRd
XLSXIngestConfig <- new_class(
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
  )
) # /rtemis::XLSXIngestConfig


# %% RDSIngestConfig ----
#' RDSIngestConfig Class
#'
#' @description
#' Reading an RDS file. R's own serialization carries its types, so the format
#' has no settings -- the leaf exists so that saying "this is an RDS" is
#' expressible, not because there is anything to configure.
#'
#' @author EDG
#' @noRd
RDSIngestConfig <- new_class(
  name = "RDSIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("rds"))
) # /rtemis::RDSIngestConfig


# %% DTAIngestConfig ----
#' DTAIngestConfig Class
#'
#' @description
#' Reading a Stata file. Its types are declared in the file.
#'
#' @author EDG
#' @noRd
DTAIngestConfig <- new_class(
  name = "DTAIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("dta"))
) # /rtemis::DTAIngestConfig


# %% ARFFIngestConfig ----
#' ARFFIngestConfig Class
#'
#' @description
#' Reading an ARFF file. Its header declares the attribute types.
#'
#' @author EDG
#' @noRd
ARFFIngestConfig <- new_class(
  name = "ARFFIngestConfig",
  parent = IngestConfig,
  package = "rtemis",
  properties = list(format = prop_algorithm("arff"))
) # /rtemis::ARFFIngestConfig


# The leaf per format. Stated once: `ingest()` picks the class for the format it
# derived, and `.list_to_IngestConfig()` dispatches a parsed document through
# the matching `setup_*`, so a format added here arrives in both.
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


# The arguments every format takes, documented once per constructor because each
# is a user-facing contract in its own right and the audit compares them class
# by class.
#
# @param columns Optional Named vector: Declared column types, keyed by column
# name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
# here is converted after reading rather than inferred.
# @param character2factor Logical: If TRUE, read character columns as factors.
# @param clean_colnames Logical: If TRUE, normalize column names.
# @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.

# %% setup_DelimitedIngest ----
#' Set up a `DelimitedIngestConfig`
#'
#' @description
#' Reading a delimited file -- the format that declares nothing, and therefore
#' the one with the most to say.
#'
#' One constructor per format rather than one for all of them: a Parquet has no
#' separator, so `sep` is not a setting it ignores, it is not a setting. R
#' refusing an argument that does not exist says that better than any check
#' could, and each format's own enums and defaults can then be documented
#' against the class they belong to.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' Reading a Parquet file. It declares its own types, so there is nothing to
#' tell the reader except which reader to be.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' Reading a spreadsheet. A workbook holds several tables, so which one is the
#' only thing that must be said.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' Reading an RDS file. R's own serialization carries its types, so the format
#' has no settings of its own.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' Reading a Stata file. Its types are declared in the file.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' Reading an ARFF file. Its header declares the attribute types.
#'
#' @param columns Optional Named vector: Declared column types, keyed by column
#' name, in the `profile/v1` vocabulary -- `c(Age = "integer")`. A column named
#' here is converted after reading rather than inferred, which is what makes a
#' known type beat a heuristic.
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
#' @param path Character: Path to the file.
#'
#' @return Character, or NULL where the extension names no supported format.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ingest_format <- function(path) {
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


# %% apply_declared_types ----
#' Convert the columns the config declares types for
#'
#' Runs after reading, so it overrides whatever the reader inferred -- which is
#' the point: inference is a guess made from the values a file happens to hold,
#' and where the user knows, saying so beats every heuristic.
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
#' The first step of any pipeline. A delimited file or a spreadsheet carries no
#' usable type information; Parquet does. Ingesting once means nothing
#' downstream infers a type a second time, and the decisions that were made are
#' in the config rather than in a reader's defaults.
#'
#' The config's `format` is checked against the file. They disagreeing is an
#' error rather than a silent reinterpretation: a config written for a delimited
#' file says things a Parquet reader would ignore, and ignoring them is how a
#' run does something other than what it was asked.
#'
#' Returns a manifest describing what happened: the file in, the file out, a
#' `DataFingerprint` of the data, the config, and which engine ran it. The
#' engine is named from the start because it is unrecoverable later -- once a
#' second implementation exists, nothing else could say which produced a given
#' Parquet.
#'
#' @param path Character: Path to the input file.
#' @param outfile Character: Path to write the Parquet file to.
#' @param config Optional `IngestConfig`: How to read it. If `NULL`, the
#' defaults for the file's own format.
#' @param overwrite Logical: If TRUE, overwrite `outfile` if it exists.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: the ingest manifest.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' ingest("data.csv", "data.parquet")
#' ingest("data.csv", "data.parquet", setup_DelimitedIngest(sep = ";"))
#' }
ingest <- function(
  path,
  outfile,
  config = NULL,
  overwrite = FALSE,
  verbosity = 1L
) {
  check_dependencies("arrow")
  path <- sanitize_path(path, must_exist = TRUE)
  outfile <- sanitize_path(outfile, must_exist = FALSE, normalize = FALSE)
  if (file.exists(outfile) && !overwrite) {
    rtemis.core::abort(
      "`outfile` exists: ",
      outfile,
      ". Pass `overwrite = TRUE` to replace it.",
      class = c("rtemis_file_error", "rtemis_input_error")
    )
  }

  format <- ingest_format(path)
  if (is.null(format)) {
    rtemis.core::abort(
      "Cannot ingest '",
      basename(path),
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
      "' file but '",
      basename(path),
      "' is ",
      format,
      ". Its settings would not apply.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Every reading decision comes from the config. `read()`'s own defaults are
  # deliberately not relied on: a default that is not in the config is a
  # decision the record cannot report. Only the arguments this format has are
  # passed, which is what makes the family worth having.
  args <- list(
    path,
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
    bold(highlight("▶")),
    " Ingested ",
    highlight(basename(path)),
    " to ",
    highlight(basename(outfile)),
    verbosity = verbosity
  )

  # Not yet a published record: what an ingest *node* records -- and whether it
  # is its own record or a block in the run's -- is still open. Returned as a
  # manifest so the information exists to shape it from, rather than a schema
  # being invented ahead of the decision.
  list(
    input = path,
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
#' `format` picks the constructor, and every other key is one of its arguments
#' -- so a key belonging to a different format fails there as an unused
#' argument, naming it, rather than being dropped.
#'
#' @param x Named list: The parsed document.
#'
#' @return An `IngestConfig` subclass.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_IngestConfig <- function(x) {
  # Every key any format admits: the base's, plus each leaf's own. A key that
  # belongs to a *different* format reaches its constructor and is refused as an
  # unused argument, which names it.
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
