# read.R
# ::rtemis::
# 2022- EDG rtemis.org

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
#' - **Parquet** files using `arrow::read_parquet()`
#' - **XLSX** files using `readxl::read_excel()`
#' - **DTA** files from Stata using `haven::read_dta()`
#' - **FASTA** files using `seqinr::read.fasta()`
#' - **RDS** files using `readRDS()`
#'
#' @param filename Character: filename or full path if `datadir = NULL`
#' @param datadir Character: Optional path to directory where `filename`
#' is located. If not specified, `filename` must be the full path.
#' @param make_unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to
#' factors
#' @param clean_colnames Logical: If TRUE, clean columns names using
#' [clean_colnames]
#' @param delim_reader Character: package to use for reading delimited data
#' @param xlsx_sheet Integer or character: Name or number of XLSX sheet to read
#' @param sep Single character: field separator. If `delim_reader = "fread"`
#' and `sep = NULL`, this defaults to "auto", otherwise defaults to ","
#' @param quote Single character: quote character
#' @param na_strings Character vector: Strings to be interpreted as NA values.
#' For `delim_reader = "duckdb"`, this must be a single string.
#' @param output Character: "default" or "data.table", If default, return the delim_reader's
#' default data structure, otherwise convert to data.table
#' @param attr Character: Attribute to set (Optional)
#' @param value Character: Value to set (if `attr` is not NULL)
#' @param verbosity Integer: Verbosity level.
#' @param fread_verbosity Integer: Verbosity level. Passed to `data.table::fread`
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional arguments to pass to `data.table::fread`,
#' `arrow::read_delim_arrow()`, `vroom::vroom()`,
#' or `readxl::read_excel()`
#'
#' @return data.frame, data.table, or tibble.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' datadir <- "/Data"
#' dat <- read("iris.csv", datadir)
#' }
read <- function(
  filename,
  datadir = NULL,
  make_unique = TRUE,
  character2factor = FALSE,
  clean_colnames = TRUE,
  delim_reader = c("data.table", "vroom", "duckdb", "arrow"),
  xlsx_sheet = 1,
  sep = NULL,
  quote = "\"",
  na_strings = c(""),
  output = c("data.table", "default"),
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
  ext <- tools::file_ext(filename)
  path <- if (is.null(datadir)) {
    filename
  } else {
    file.path(datadir, filename)
  }
  path <- path.expand(path)

  if (ext == "parquet") {
    check_dependencies("arrow")
    if (verbosity > 0L) {
      msg0(
        highlight("\u25B6"),
        " Reading ",
        highlight(basename(path)),
        " using arrow::read_parquet()..."
      )
    }
    .dat <- arrow::read_parquet(path, ...)
    if (output == "data.table") setDT(.dat)
  } else if (ext == "rds") {
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        "..."
      )
    }
    .dat <- readRDS(path)
  } else if (ext == "xlsx") {
    check_dependencies("openxlsx")
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        " using readxl::read_excel()..."
      )
    }
    .dat <- readxl::read_excel(
      filename,
      sheet = xlsx_sheet,
      na = na_strings,
      ...
    )
    if (output == "data.table") setDT(.dat)
  } else if (ext == "dta") {
    check_dependencies("haven")
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        " using haven::read_dta()..."
      )
    }
    .dat <- haven::read_dta(path, ...)
    if (output == "data.table") setDT(.dat)
  } else if (ext == "fasta") {
    check_dependencies("seqinr")
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        " using seqinr::read.fasta()..."
      )
    }
    .dat <- seqinr::read.fasta(path, ...)
    # if single sequence, return as character
    if (length(.dat) == 1) {
      .dat <- as.character(.dat[[1]])
    }
    return(.dat)
  } else if (ext == "arff") {
    check_dependencies("farff")
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        " using farff::readARFF()..."
      )
    }
    .dat <- farff::readARFF(path, ...)
    if (output == "data.table") setDT(.dat)
  } else {
    if (verbosity > 0L) {
      msg0(
        bold(highlight("\u25B6")),
        " Reading ",
        highlight(basename(path)),
        " using ",
        delim_reader,
        "..."
      )
    }
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
      if (output == "data.table") {
        setDT(.dat)
      }
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
      if (output == "data.table") {
        setDT(.dat)
      }
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
      if (output == "data.table") {
        setDT(.dat)
      }
    }
  }

  .nrow <- nrow(.dat)
  .ncol <- ncol(.dat)
  if (verbosity > 0L) {
    msg(
      "Read in",
      highlightbig(.nrow),
      "x",
      highlightbig(.ncol)
    )
  }
  if (make_unique) {
    .dat <- unique(.dat)
    .nrowp <- nrow(.dat)
    .dup <- .nrow - .nrowp
    if (verbosity > 0L && .dup > 0) {
      msg(
        "Removed",
        bold(orange(format(.dup, big.mark = ","))),
        "duplicate",
        paste0(ngettext(.dup, "row", "rows"), ".")
      )
      msg(
        "New dimensions:",
        highlightbig(.nrowp),
        "x",
        highlightbig(.ncol)
      )
    }
  }

  if (clean_colnames) {
    setnames(.dat, names(.dat), clean_colnames(.dat))
  }

  if (character2factor) {
    .dat <- preprocess(
      .dat,
      setup_Preprocessor(character2factor = TRUE)
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

  .dat
} # /rtemis::read


#' Msg for data reading
#'
#' @author EDG
#' @keywords internal
#' @noRd
msgread <- function(x, caller = "", use_basename = TRUE) {
  if (use_basename) {
    x <- basename(x)
  }
  msg0(
    bold(highlight("\u25B6")),
    " Reading ",
    highlight(x),
    "...",
    caller = caller
  )
} # /rtemis::msgread
