# ddb_ops.R
# ::rtemis::
# 2022-3 EDG rtemis.org

#' Read CSV using DuckDB
#'
#' Lazy-read a CSV file, optionally filter rows, remove duplicates,
#' clean column names, convert character to factor, and collect.
#'
#' @param filename Character: file name; either full path or just the file name,
#' if `datadir` is also provided.
#' @param datadir Character: Optional path if `filename` is not full path.
#' @param sep Character: Field delimiter/separator.
#' @param header Logical: If TRUE, first line will be read as column names.
#' @param quotechar Character: Quote character.
#' @param ignore_errors Logical: If TRUE, ignore parsing errors (sometimes it's
#' either this or no data, so).
#' @param make_unique Logical: If TRUE, keep only unique rows.
#' @param select_columns Character vector: Column names to select.
#' @param filter_column Character: Name of column to filter on, e.g. "ID".
#' @param filter_vals Numeric or Character vector: Values in `filter_column` to keep.
#' `filter_column` to keep.
#' @param character2factor Logical: If TRUE, convert character columns to
#' factors.
#' @param collect Logical: If TRUE, collect data and return structure class
#' as defined by `returnobj`.
#' @param progress Logical: If TRUE, print progress (no indication this works).
#' @param returnobj Character: "data.frame" or "data.table" object class to
#' return. If "data.table", data.frame object returned from
#' `DBI::dbGetQuery` is passed to `data.table::setDT`; will add to
#' execution time if very large, but then that's when you need a data.table.
#' @param data.table.key Character: If set, this corresponds to a column name in the
#' dataset. This column will be set as key in the data.table output.
#' @param clean_colnames Logical: If TRUE, clean colnames with
#' [clean_colnames].
#' @param verbosity Integer: Verbosity level.
#'
#' @return data.frame or data.table.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' ir <- ddb_data("/Data/massive_dataset.csv",
#'   filter_column = "ID",
#'   filter_vals = 8001:9999
#' )
#' }
ddb_data <- function(
  filename,
  datadir = NULL,
  sep = ",",
  header = TRUE,
  quotechar = "",
  ignore_errors = TRUE,
  make_unique = TRUE,
  select_columns = NULL,
  filter_column = NULL,
  filter_vals = NULL,
  character2factor = FALSE,
  collect = TRUE,
  progress = TRUE,
  returnobj = c("data.table", "data.frame"),
  data.table.key = NULL,
  clean_colnames = TRUE,
  verbosity = 1L
) {
  # Intro ----
  check_dependencies("DBI", "duckdb")
  returnobj <- match.arg(returnobj)
  if (!is.null(data.table.key)) {
    returnobj <- "data.table"
  }
  path <- if (is.null(datadir)) {
    normalizePath(filename)
  } else {
    file.path(normalizePath(datadir), filename)
  }
  check_files(path, verbosity = 0L)
  fileext <- tools::file_ext(path)

  out <- paste(
    bold(highlight("\u25B6")),
    ifelse(collect, "Reading", "Lazy-reading"),
    highlight(basename(path))
  )
  if (!is.null(filter_column)) {
    out <- paste(
      out,
      bold(highlight("\u29e8")),
      "filtering on",
      bold(filter_column)
    )
  }
  start_time <- intro(out, verbosity = verbosity)
  distinct <- ifelse(make_unique, "DISTINCT ", NULL)
  select <- if (!is.null(select_columns)) {
    ls2sel(select_columns)
  } else {
    "*"
  }

  # SQL ----
  sql <- if (fileext == "parquet") {
    paste0(
      "SELECT ",
      paste0(distinct, select),
      " FROM read_parquet('",
      path,
      "')"
    )
  } else {
    paste0(
      "SELECT ",
      paste0(distinct, select),
      " FROM read_csv_auto('",
      path,
      "',
            sep='",
      sep,
      "', quote='",
      quotechar,
      "',
            header=",
      header,
      ", ignore_errors=",
      ignore_errors,
      ")"
    )
  }

  sql <- if (!is.null(filter_column)) {
    vals <- if (is.numeric(filter_vals)) {
      paste0(filter_vals, collapse = ", ")
    } else {
      paste0("'", paste0(filter_vals, collapse = "', '"), "'")
    }
    paste(
      sql,
      "WHERE",
      filter_column,
      "in (",
      vals,
      ");"
    )
  } else {
    paste0(sql, ";")
  }

  # Collect ----
  if (collect) {
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
    # on.exit(
    #     tryCatch(DBI::dbRollback(conn), error = function(e) {
    # }))
    if (progress) {
      DBI::dbExecute(conn, "PRAGMA enable_progress_bar;")
    }
    out <- DBI::dbGetQuery(conn, sql)
    if (clean_colnames) {
      names(out) <- clean_colnames(out)
    }
    if (returnobj == "data.table") {
      data.table::setDT(out)
      if (!is.null(data.table.key)) {
        data.table::setkeyv(out, data.table.key)
      }
    }
    if (character2factor) {
      out <- preprocess(out, character2factor = TRUE)
    }
  } else {
    out <- sql
  }

  # Outro ----
  outro(start_time, verbosity = verbosity)
  out
} # /rtemis::ddb_data


# output: '"alpha", "beta", "gamma"'
ls2sel <- function(x) {
  paste0(
    '"',
    paste0(x, collapse = '", "'),
    '"'
  )
}


#' Collect a lazy-read duckdb table
#'
#' Collect a table read with `ddb_data(x, collect = FALSE)`
#'
#' @param sql Character: DuckDB SQL query, usually output of
#' [ddb_data] with `collect = FALSE`
#' @param progress Logical: If TRUE, show progress bar
#' @param returnobj Character: data.frame or data.table: class of object to return
#'
#' @return `data.frame` or `data.table`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' sql <- ddb_data("/Data/iris.csv", collect = FALSE)
#' ir <- ddb_ollect(sql)
#' }
ddb_collect <- function(
  sql,
  progress = TRUE,
  returnobj = c("data.frame", "data.table")
) {
  returnobj <- match.arg(returnobj)
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  if (progress) {
    DBI::dbExecute(conn, "PRAGMA enable_progress_bar;")
  }
  out <- DBI::dbGetQuery(conn, sql)
  if (returnobj == "data.table") {
    setDT(out)
  }
  out
} # /rtemis::ddb_collect
