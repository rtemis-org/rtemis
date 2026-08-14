# read.R
# ::rtemis::
# 2022- EDG rtemis.org

# %% read ----
#' Read tabular data from a variety of formats
#'
#' Read data and optionally clean column names, keep unique rows, and convert
#' characters to factors
#'
#' @details
#' `read` is a convenience function to read:
#'
#' - **Delimited** files using `data.table:fread()`, `arrow:read_delim_arrow()`,
#'   `vroom::vroom()`, or `duckdb::duckdb_read_csv()`
#' - **ARFF** files using `farff::readARFF()`
#' - **Parquet** files using `arrow::read_parquet()` or `nanoparquet::read_parquet`
#' - **XLSX** files using `readxl::read_excel()`
#' - **DTA** files from Stata using `haven::read_dta()`
#' - **FASTA** files using `seqinr::read.fasta()`
#' - **RDS** files using `readRDS()`
#'
#' @param filename Character: filename or full path if `datadir = NULL`.
#' @param datadir Character: Optional path to directory where `filename`
#' is located. If not specified, `filename` must be the full path.
#' @param remove_duplicates Logical: If TRUE, keep unique rows only.
#' @param character2factor Logical: If TRUE, convert character variables to
#' factors.
#' @param clean_colnames Logical: If TRUE, clean columns names using
#' [clean_colnames].
#' @param delim_reader Character: package to use for reading delimited data.
#' @param parquet_reader Character: package to use for reading Parquet files. If undefined and
#'   system is WASM, uses "nanoparquet", otherwise "arrow".
#' @param xlsx_sheet Integer or character: Name or number of XLSX sheet to read.
#' @param sep Single character: field separator. If `delim_reader = "fread"`
#' and `sep = NULL`, this is "auto", otherwise ",".
#' @param quote Single character: quote character.
#' @param na_strings Character vector: Strings to be interpreted as NA values.
#' For `delim_reader = "duckdb"`, this must be a single string.
#' @param output Character: "default" or "data.table", If default, return the delim_reader's
#' default data structure, otherwise convert to data.table.
#' @param attr Character: Attribute to set (Optional).
#' @param value Character: Value to set (if `attr` is not NULL).
#' @param verbosity Integer: Verbosity level.
#' @param fread_verbosity Integer: Verbosity level. Passed to `data.table::fread`
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional arguments to pass to `data.table::fread`,
#' `arrow::read_delim_arrow()`, `vroom::vroom()`,
#' or `readxl::read_excel()`.
#'
#' @return data.frame, data.table, or tibble.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' # Replace with your own data directory and filename
#' datadir <- "/Data"
#' dat <- read("iris.csv", datadir)
#' }
read <- function(
  filename,
  datadir = NULL,
  remove_duplicates = FALSE,
  character2factor = FALSE,
  clean_colnames = TRUE,
  delim_reader = c("data.table", "vroom", "duckdb", "arrow"),
  parquet_reader = c("arrow", "nanoparquet"),
  xlsx_sheet = 1,
  sep = NULL,
  quote = "\"",
  na_strings = c(""),
  output = c("data.table", "tibble", "data.frame"),
  attr = NULL,
  value = NULL,
  verbosity = 1L,
  fread_verbosity = 0L,
  timed = verbosity > 0L,
  ...
) {
  check_dependencies("data.table")
  if (timed) {
    start_time <- intro(verbosity = 0L)
  }
  delim_reader <- match.arg(delim_reader)
  output <- match.arg(output)
  if (output == "tibble") {
    check_dependencies("tibble")
  }
  ext <- tools::file_ext(filename)
  path <- if (is.null(datadir)) {
    filename
  } else {
    file.path(datadir, filename)
  }

  # Sanitize path for security
  path <- sanitize_path(path, must_exist = TRUE)

  if (ext == "parquet") {
    if (
      length(parquet_reader) > 1L &&
        substr(base::R.Version()[["arch"]], 1, 4) == "wasm"
    ) {
      parquet_reader <- "nanoparquet"
    }
    parquet_reader <- match.arg(parquet_reader)
    if (parquet_reader == "arrow") {
      check_dependencies("arrow")
    } else {
      check_dependencies("nanoparquet")
    }
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using ",
      parquet_reader,
      "...",
      verbosity = verbosity
    )
    .dat <- switch(
      parquet_reader,
      "arrow" = as.data.frame(materialize_arrow_views(
        arrow::read_parquet(path, as_data_frame = FALSE, ...)
      )),
      "nanoparquet" = nanoparquet::read_parquet(path, ...)
    )
  } else if (ext == "rds") {
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      "...",
      verbosity = verbosity
    )
    .dat <- readRDS(path)
  } else if (ext == "xlsx") {
    check_dependencies("readxl")
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using readxl::read_excel()...",
      verbosity = verbosity
    )
    .dat <- readxl::read_excel(
      path,
      sheet = xlsx_sheet,
      na = na_strings,
      ...
    )
  } else if (ext == "dta") {
    check_dependencies("haven")
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using haven::read_dta()...",
      verbosity = verbosity
    )
    .dat <- haven::read_dta(path, ...)
  } else if (ext == "fasta") {
    check_dependencies("seqinr")
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using seqinr::read.fasta()...",
      verbosity = verbosity
    )
    .dat <- seqinr::read.fasta(path, ...)
    # if single sequence, return as character
    if (length(.dat) == 1) {
      .dat <- as.character(.dat[[1]])
    }
    return(.dat)
  } else if (ext == "arff") {
    check_dependencies("farff")
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using farff::readARFF()...",
      verbosity = verbosity
    )
    .dat <- farff::readARFF(path, ...)
  } else {
    msg0(
      bold(highlight("\u25B6")),
      " Reading ",
      highlight(basename(path)),
      " using ",
      delim_reader,
      "...",
      verbosity = verbosity
    )
    if (delim_reader == "data.table") {
      if (is.null(sep)) {
        sep <- "auto"
      }
      .dat <- data.table::fread(
        path,
        sep = sep,
        quote = quote,
        na.strings = na_strings,
        verbose = fread_verbosity > 0L,
        ...
      )
    } else if (delim_reader == "duckdb") {
      check_dependencies("DBI", "duckdb")
      if (is.null(sep)) {
        sep <- ","
      }
      if (length(na_strings) > 1) {
        msg(
          "Note: 'na_strings' must be a single string for duckdb; setting to '",
          na_strings[1],
          "'"
        )
        na_strings <- na_strings[1]
      }
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      duckdb::duckdb_read_csv(
        con,
        "data",
        path,
        header = TRUE,
        na.strings = na_strings,
        nrow.check = 500,
        delim = sep,
        quote = quote,
        ...
      )
      .dat <- DBI::dbReadTable(con, "data")
    } else if (delim_reader == "arrow") {
      check_dependencies("arrow")
      if (is.null(sep)) {
        sep <- ","
      }
      .dat <- arrow::read_delim_arrow(
        path,
        delim = sep,
        quote = quote,
        na = na_strings,
        ...
      )
    } else {
      check_dependencies("vroom")
      .dat <- vroom::vroom(
        path,
        delim = sep,
        quote = quote,
        na = na_strings,
        progress = verbosity > 0L,
        ...
      )
    }
  }

  .nrow <- nrow(.dat)
  .ncol <- ncol(.dat)
  msg(
    "Read in",
    highlightbig(.nrow),
    "x",
    highlightbig(.ncol),
    verbosity = verbosity
  )

  if (clean_colnames) {
    setnames(.dat, names(.dat), clean_colnames(.dat))
  }

  if (remove_duplicates || character2factor) {
    .dat <- preprocess(
      .dat,
      setup_Preprocessor(
        character2factor = character2factor,
        remove_duplicates = remove_duplicates
      ),
      verbosity = verbosity
    )[["preprocessed"]]
  }

  if (!is.null(attr) && !is.null(value)) {
    for (i in seq_len(ncol(.dat))) {
      setattr(.dat[[i]], attr, value)
    }
  }

  if (timed) {
    outro(start_time)
  }

  # Set output structure
  if (output == "data.table") {
    if (!is.data.table(.dat)) setDT(.dat)
  } else if (output == "tibble") {
    .dat <- tibble::as_tibble(.dat)
  } else if (output == "data.frame") {
    if (!is.data.frame(.dat)) {
      .dat <- as.data.frame(.dat)
    } else {
      setDF(.dat)
    }
  }

  .dat
} # /rtemis::read


# %% materialize_arrow_views ----
#' Cast an Arrow table's view types to their materialized equivalents
#'
#' `arrow` has no R converter for the Arrow view types, so a Table carrying one
#' reads fine and then fails to convert with
#' `cannot handle Array of type <utf8_view>`. Polars writes every string column
#' as `string_view` and its parquet writer fixes the compatibility level, so a
#' polars-written file cannot avoid them and the cast has to happen here.
#'
#' Only top-level fields are cast: a view type nested inside a list or struct
#' column is left as it is, since such a column is not the tabular data `read()`
#' returns.
#'
#' @param x Arrow Table.
#'
#' @return `x` with each `string_view` field cast to `utf8` and each
#' `binary_view` field to `binary`, returned unchanged when it carries neither.
#'
#' @author EDG
#' @keywords internal
#' @noRd
materialize_arrow_views <- function(x) {
  # Two accessors, because arrow overloads `[[` and this package overloads `$`.
  # `[[` reads an arrow object's *contents* where arrow defines it to -- a
  # Table's column, a Schema's field -- and is plain environment access
  # elsewhere, so it serves on a Field or a DataType but cannot reach a Table's
  # schema or a Schema's metadata. `$` would, but the package's own `$` methods
  # make it a closure, under which static analysis reads every member name as an
  # unbound global: an `R CMD check` NOTE. Both are active bindings on an R6
  # object, which is an environment, so `get()` reaches them. `$<-` and
  # `as_arrow_table(schema = )` are clear of all this.
  schema <- get("schema", envir = x)
  fields <- lapply(seq_along(schema), function(i) schema[[i]])
  materialized <- list(
    string_view = arrow::utf8(),
    binary_view = arrow::binary()
  )
  targets <- lapply(fields, function(field) {
    materialized[[field[["type"]][["ToString"]]()]]
  })
  is_view <- !vapply(targets, is.null, logical(1L))
  if (!any(is_view)) {
    return(x)
  }
  fields[is_view] <- lapply(which(is_view), function(i) {
    arrow::field(
      fields[[i]][["name"]],
      targets[[i]],
      nullable = fields[[i]][["nullable"]]
    )
  })
  target_schema <- do.call(arrow::schema, fields)
  # The cast reaches the data through a schema built from scratch, which carries
  # no key-value metadata; converting to a data.frame reads the "r" entry to
  # restore R attributes, so the file's metadata has to be carried across.
  target_schema$metadata <- get("metadata", envir = schema)
  arrow::as_arrow_table(x, schema = target_schema)
} # /rtemis::materialize_arrow_views
