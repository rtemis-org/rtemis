# 061_DataRef.R
# ::rtemis::
# 2026- EDG rtemis.org

# A reference from a run record to a file written beside it.
#
# Not everything a run produces belongs inside a JSON document. Most of it is
# tabular -- the execution graph, and later the per-fold predictions, the
# grid-search results, the variable importances, the calibration points -- and a
# table belongs in a columnar file, typed and compressed and readable by any
# data tool. Some of it is not tabular at all: `rt_save()` already writes the
# fitted object beside the record and `train()` writes a log, neither of which
# the record names today.
#
# So this is a reference to a *file*, not to a table. `n_rows` and `n_cols` are
# what a table has and are unset for anything else; `path`, `encoding`, `bytes`
# and the digest are what every referenced file has.
#
# The record's integrity still covers the file. The hash is inside the JSON, so
# a changed sidecar contradicts the record, and a record edited to agree
# contradicts whatever digest the store holds over it. The pair is one piece of
# evidence in two files.
#
# Declared with the `prop_*` factories like every other class, so its schema is
# generated and a port reads it from the published contract. `$ref`d wherever a
# record names a sidecar, rather than restated per field.

# %% DATA_ENCODINGS ----
# How a referenced file is written. Parquet for a table, because a record is
# *storage*: it is
# already this project's storage format (`ingest` writes it, `profile/v1`
# fixtures publish it), every data tool reads it, and row-group statistics are
# what a later index queries. Arrow IPC stays the *wire* format, which is a
# different job and is unaffected.
#
# One value because one kind of file is referenced today. The set grows when a
# record names something else, not in anticipation of it: an encoding nothing
# writes is a value every reader must handle and none will ever see.
DATA_ENCODINGS <- c("parquet")


# %% DataRef ----
#' DataRef
#'
#' @description
#' A reference to a file written beside a record: where it is, how it is
#' encoded, how big it is, and the digest that ties it to the record naming it.
#'
#' @details
#' The digest is over the file's bytes. A canonical logical form would be needed
#' if two implementations had to agree on the digest for one table, as they must
#' for the schema corpus; a sidecar is produced once by the engine that ran, so
#' there is no second writer to agree with.
#'
#' @field path Character: Path to the file, relative to the record naming it.
#' @field encoding Character \{"parquet"\}: How the file is written.
#' @field algorithm Character \{"sha256", "sha512", "sha384", "sha224",
#'   "sha3-256", "sha3-512", "blake2b", "blake2s", "sha1", "md5"\}: Hash
#'   algorithm.
#' @field hash Character: Hash digest of the file's bytes, as hex.
#' @field bytes Integer [0, Inf): Size of the file.
#' @field n_rows Optional Integer [0, Inf): Rows, when the file is a table.
#' @field n_cols Optional Integer [0, Inf): Columns, when the file is a table.
#'
#' @author EDG
#' @noRd
DataRef <- new_class(
  name = "DataRef",
  package = "rtemis",
  properties = list(
    # Relative, so a study is a directory that can be moved, copied or handed to
    # a colleague without rewriting what it says about itself.
    path = prop_string(
      "",
      description = "Path to the file, relative to the record that names it."
    ),
    encoding = prop_string(
      "parquet",
      enum = DATA_ENCODINGS,
      description = "How the file is written. Two digests are comparable only if they share one."
    ),
    algorithm = prop_string(
      "sha256",
      enum = DATA_HASH_ALGORITHMS,
      description = "Hash algorithm, recorded so a hash is verifiable rather than merely comparable."
    ),
    # No meaningful default, for the reason `DataFingerprint@hash` has none: a
    # reference that cannot be checked is not a reference.
    hash = prop_string(
      "",
      description = "Hash digest of the file's bytes, as hex."
    ),
    # Every referenced file has a size, and a reader deciding whether to fetch
    # one needs it before it does. A second integrity signal beside the digest.
    bytes = prop_integer(
      0L,
      min = 0L,
      description = "Size of the file."
    ),
    # Unset for a file that is not a table. Nullable rather than zero: an empty
    # table and a file with no rows to speak of are different facts, and only
    # one of them is sayable as 0.
    n_rows = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Rows, when the referenced file is a table."
    ),
    n_cols = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Columns, when the referenced file is a table."
    )
  ),
  validator = function(self) {
    if (!nzchar(self@path)) {
      return("@path must name a file.")
    }
    if (!nzchar(self@hash)) {
      return("@hash must be the digest of that file's bytes.")
    }
    NULL
  }
) # /rtemis::DataRef


# %% repr.DataRef ----
method(repr, DataRef) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("DataRef", pad = pad, output_type = output_type)
  paste0(
    out,
    strrep(" ", pad + 2L),
    x@path,
    gray(
      paste0(
        "  ",
        if (is.null(x@n_rows)) {
          paste0(x@bytes, " B")
        } else {
          paste0(x@n_rows, " x ", x@n_cols)
        },
        "  ",
        x@algorithm,
        ":",
        substr(x@hash, 1L, 12L)
      ),
      output_type = output_type
    ),
    "\n"
  )
} # /rtemis::repr.DataRef


# %% print.DataRef ----
method(print, DataRef) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.DataRef
