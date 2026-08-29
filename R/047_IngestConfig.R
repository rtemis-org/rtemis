# 047_IngestConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% IngestConfig ----
#' IngestConfig Class
#'
#' @description
#' How a data file is read and normalized to Parquet -- the first step of any
#' pipeline, and the only one that decides types.
#'
#' A delimited file, a spreadsheet and a Stata file carry no usable type
#' information, so *something* has to decide that a column of labels is a factor
#' rather than text, and where the missing values are. Left to each reader's
#' defaults, that decision is invisible: it does not appear in the config, the
#' record cannot report it, and two readers of the same file disagree for
#' reasons no one can inspect. Stating it here makes it a property of the run.
#'
#' Once ingested, everything downstream reads Parquet, which *declares* its
#' types -- so nothing infers them a second time.
#'
#' @author EDG
#' @noRd
IngestConfig <- new_class(
  name = "IngestConfig",
  package = "rtemis",
  properties = list(
    # The one property that changes what a learner sees rather than merely how
    # the file is parsed: a column of labels is a factor or it is unusable text,
    # and `check_supervised()` rejects the latter outright.
    character2factor = prop_boolean(
      TRUE,
      description = paste0(
        "Read character columns as factors. Supervised learning needs ",
        "categorical predictors as factors, and a delimited file cannot say ",
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
    ),
    # Which library reads the file. It belongs in the config because readers
    # disagree -- on type inference, on what counts as missing -- so a record
    # that did not name the reader could not account for its own numbers.
    delim_reader = prop_string(
      "data.table",
      enum = c("data.table", "vroom", "duckdb", "arrow"),
      description = "Library used to read a delimited file."
    ),
    parquet_reader = prop_string(
      "arrow",
      enum = c("arrow", "nanoparquet"),
      description = "Library used to read a Parquet file."
    ),
    xlsx_sheet = prop_integer(
      1L,
      min = 1L,
      description = "Sheet number to read from a spreadsheet."
    ),
    # Delimited-file parsing. `sep` is nullable because the readers guess it,
    # and a guess that was never overridden is worth recording as a guess.
    sep = prop_string(
      NULL,
      nullable = TRUE,
      description = "Field separator. NULL lets the reader detect it."
    ),
    quote = prop_string(
      "\"",
      description = "Quote character."
    ),
    na_strings = prop_string(
      "",
      vector = TRUE,
      description = "Strings to read as missing."
    )
  )
) # /rtemis::IngestConfig


# %% repr.IngestConfig ----
#' repr IngestConfig
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, IngestConfig) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name("IngestConfig", pad = pad, output_type = output_type),
    repr_ls(props(x), pad = pad, output_type = output_type)
  )
} # /rtemis::repr.IngestConfig


# %% print.IngestConfig ----
#' Print IngestConfig
#'
#' @param x `IngestConfig` object.
#' @param pad Integer: Left padding.
#' @param output_type Character: Output type.
#' @param ... Not used.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(print, IngestConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type), "\n")
  invisible(x)
} # /rtemis::print.IngestConfig


# %% setup_Ingest ----
#' Set up an `IngestConfig`
#'
#' @description
#' How a data file is read and normalized to Parquet. Every argument is a
#' decision that changes what the run trains on, which is why they are a config
#' rather than arguments to a reader: the record reports them, and
#' `validate_config()` can read them.
#'
#' @param character2factor Logical: If TRUE, read character columns as factors.
#' @param clean_colnames Logical: If TRUE, normalize column names.
#' @param remove_duplicates Logical: If TRUE, drop rows repeating an earlier row.
#' @param delim_reader Character \{"data.table", "vroom", "duckdb", "arrow"\}:
#' Library used to read a delimited file.
#' @param parquet_reader Character \{"arrow", "nanoparquet"\}: Library used to
#' read a Parquet file.
#' @param xlsx_sheet Integer [1, Inf): Sheet number to read from a spreadsheet.
#' @param sep Optional Character: Field separator. If `NULL`, the reader detects it.
#' @param quote Character: Quote character.
#' @param na_strings Character vector: Strings to read as missing.
#'
#' @return `IngestConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_Ingest(character2factor = FALSE)
setup_Ingest <- function(
  character2factor = TRUE,
  clean_colnames = TRUE,
  remove_duplicates = FALSE,
  delim_reader = "data.table",
  parquet_reader = "arrow",
  xlsx_sheet = 1L,
  sep = NULL,
  quote = "\"",
  na_strings = ""
) {
  IngestConfig(
    character2factor = character2factor,
    clean_colnames = clean_colnames,
    remove_duplicates = remove_duplicates,
    delim_reader = delim_reader,
    parquet_reader = parquet_reader,
    xlsx_sheet = clean_posint(xlsx_sheet),
    sep = sep,
    quote = quote,
    na_strings = na_strings
  )
} # /rtemis::setup_Ingest


# %% ingest ----
#' Read a data file and normalize it to Parquet
#'
#' @description
#' The first step of any pipeline. A delimited file, a spreadsheet or a Stata
#' file carries no usable type information; Parquet does. Ingesting once means
#' nothing downstream infers a type a second time, and the decisions that were
#' made are in the config rather than in a reader's defaults.
#'
#' Returns a manifest describing what happened: the file in, the file out, a
#' `DataFingerprint` of each, the config, and which engine ran it. The engine is
#' named from the start because it is unrecoverable later -- once a second
#' implementation exists, nothing else could say which produced a given Parquet.
#'
#' @param path Character: Path to the input file.
#' @param outfile Character: Path to write the Parquet file to.
#' @param config Optional `IngestConfig`: How to read it. If `NULL`, defaults.
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
#' }
ingest <- function(
  path,
  outfile,
  config = NULL,
  overwrite = FALSE,
  verbosity = 1L
) {
  check_dependencies("arrow")
  config <- config %||% setup_Ingest()
  check_is_S7(config, IngestConfig)
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

  # Every reading decision comes from the config. `read()`'s own defaults are
  # deliberately not relied on: a default that is not in the config is a
  # decision the record cannot report.
  dat <- read(
    path,
    character2factor = config@character2factor,
    clean_colnames = config@clean_colnames,
    remove_duplicates = config@remove_duplicates,
    delim_reader = config@delim_reader,
    parquet_reader = config@parquet_reader,
    xlsx_sheet = config@xlsx_sheet,
    sep = config@sep,
    quote = config@quote,
    na_strings = config@na_strings,
    verbosity = verbosity
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
  # is its own record or a block in the run's -- is step 8's remaining decision.
  # Returned as a manifest so the information exists to shape it from, rather
  # than a schema being invented ahead of the decision.
  list(
    input = path,
    output = outfile,
    config = S7_to_list(config),
    # Which implementation read the file. R today; a native executor later, and
    # the two are held to the same output by a fixture.
    engine = "R",
    rtemis_version = as.character(utils::packageVersion("rtemis")),
    data_input = S7_to_list(data_fingerprint(dat)),
    n_rows = as.integer(NROW(dat)),
    n_cols = as.integer(NCOL(dat))
  )
} # /rtemis::ingest
