# 040_DataFingerprint.R
# ::rtemis::
# 2026- EDG rtemis.org

# Dataset identity for provenance records: what data a run actually used, so
# that across a batch of experiments a user can tell whether runs shared an
# input or drifted undocumented. A path alone does not establish this -- the
# file at that path can change -- so a `DataFingerprint` pairs a content hash
# with the cheap structural facts (dimensions, column names) that make a mismatch
# *diagnosable* rather than merely detectable.
#
# Three hash methods, answering three different questions:
# - "file"    raw bytes of the source file. Answers "the same file?", and is
#             sensitive to changes that do not affect the data (line endings,
#             trailing whitespace).
# - "object"  the serialized R object. The default: cheap, always available,
#             but sensitive to R-internal representation: the container, the
#             order attributes are stored in, factor level order, integer vs
#             double storage, and whether a column is still an ALTREP compact
#             sequence or has been materialized. Two tables that are
#             `identical()` can differ.
# - "table"   canonical logical content via Arrow IPC. Strongest and most
#             portable, but requires the suggested `arrow` package.
#
# `method` is the question asked; `encoding` is the answer's byte recipe, and
# the only field that makes two digests comparable. See `DATA_HASH_ENCODINGS`.
#
# `portability` names the language runtime that can recompute a digest -- "any"
# where the hashed form is defined outside any runtime, "R" for R's own
# serialization, "unknown" for a token written elsewhere. It is computed from
# `encoding` rather than stored, so it cannot contradict it.

# %% Constants ----
# Hash methods, ordered weakest to strongest guarantee.
DATA_HASH_METHODS <- c("file", "object", "table")

# Hash algorithms accepted for `DataFingerprint@algorithm`, each dispatched to
# its `openssl` implementation by `.hash_bytes()`. Adding a name here without
# adding its branch there is caught by the switch's own error.
DATA_HASH_ALGORITHMS <- c(
  "sha256",
  "sha512",
  "sha384",
  "sha224",
  "sha3-256",
  "sha3-512",
  "blake2b",
  "blake2s",
  "sha1",
  "md5"
)

# Pinned so that "object" hashes are reproducible: the bytes `serialize()`
# produces depend on the serialization format version, so leaving this to the
# default would make those hashes irreproducible across R versions that change
# it. Changing this value is a BREAKING change to fingerprint comparability.
DATA_HASH_SERIALIZE_VERSION <- 3L

# One row per method, carrying both facts derived from it, so a method cannot
# acquire an encoding without also declaring who can reproduce it.
#
# `method` says what kind of identity was asked for -- the same file, the same
# object, the same logical table -- and means that in any implementation.
# `encoding` names the exact byte recipe, which is what two digests must share
# to be comparable at all: "object" is a family, not a definition, so an R
# fingerprint and a Python one would both say "object" while hashing entirely
# different bytes. Comparing encodings makes that "not comparable" instead of a
# confident "different data".
#
# A token carries its format version where the format has one, so bumping
# `DATA_HASH_SERIALIZE_VERSION` renames the encoding and the fingerprints it
# affects report as not comparable rather than as different data. Which ones it
# affects is not obvious: v2 and v3 payloads coincide for a plain data.frame
# and diverge for any ALTREP column, which v3 writes compactly -- exactly the
# kind of silent, input-dependent change the token exists to make visible.
DATA_HASH_ENCODINGS <- data.frame(
  method = c("file", "object", "table"),
  encoding = c(
    "file-bytes",
    paste0("r-serialize-v", DATA_HASH_SERIALIZE_VERSION),
    "arrow-ipc"
  ),
  portability = c("any", "R", "any"),
  stringsAsFactors = FALSE
)

# Characters of the hash shown in `repr()`. Enough to compare at a glance.
DATA_HASH_DISPLAY_CHARS <- 12L


# %% DataFingerprint ----
#' DataFingerprint Class
#'
#' @description
#' Identity of a dataset used by a run: a content hash plus the structural facts
#' needed to diagnose a mismatch. Created by `data_fingerprint()`.
#'
#' Every property but `method`, `algorithm` and `source` is measured from the
#' data rather than chosen, so this class is documented here with `@field`
#' rather than through a `setup_*` function.
#'
#' @field method Character \{"file", "object", "table"\}: What was hashed.
#' @field algorithm Character \{"sha256", "sha512", "sha384", "sha224", "sha3-256", "sha3-512", "blake2b", "blake2s", "sha1", "md5"\}: Hash algorithm.
#' @field encoding Character: The exact byte recipe hashed, named so that it
#'   identifies itself outside R. Two digests are comparable only if they share
#'   one.
#' @field hash Character: Hash digest, as hex.
#' @field n_rows Integer [0, Inf): Number of rows (cases).
#' @field n_cols Integer [0, Inf): Number of columns.
#' @field column_names Optional Character vector: Column names, in order.
#' @field source Optional Character: Path the data was read from.
#' @field portability Character: Computed from `encoding`; which language
#'   runtime can recompute and compare this hash. "any" when the hashed form is
#'   defined outside any runtime, the language that defines it otherwise, and
#'   "unknown" for an encoding written by an implementation this build does not
#'   know.
#'
#' @author EDG
#' @noRd
DataFingerprint <- new_class(
  name = "DataFingerprint",
  package = "rtemis",
  properties = list(
    method = prop_string(
      "object",
      enum = DATA_HASH_METHODS,
      description = "What was hashed: the source file's bytes, the serialized R object, or the canonical Arrow table."
    ),
    algorithm = prop_string(
      "sha256",
      enum = DATA_HASH_ALGORITHMS,
      description = "Hash algorithm, recorded so a hash is verifiable rather than merely comparable."
    ),
    # Stored rather than computed, because a computed property is absent from
    # the wire and this is the one field a foreign reader cannot do without: it
    # is recoverable from `method` only by an implementation that already knows
    # which language wrote the record. Left free-form for the same reason -- a
    # token from another implementation must be readable here, not rejected,
    # and an unrecognized one correctly means "not comparable".
    encoding = prop_string(
      "",
      description = "The exact byte recipe hashed. Two digests are comparable only if they share one."
    ),
    # No meaningful default: a fingerprint without a hash is not a fingerprint,
    # so the class validator rejects the empty default and bare construction
    # fails with an informative message.
    hash = prop_string("", description = "Hash digest, as hex."),
    n_rows = prop_integer(
      0L,
      min = 0L,
      description = "Number of rows (cases)."
    ),
    n_cols = prop_integer(
      0L,
      min = 0L,
      description = "Number of columns."
    ),
    column_names = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Column names, in order."
    ),
    source = prop_string(
      NULL,
      nullable = TRUE,
      description = "Path the data was read from. NULL for in-memory input."
    ),
    # Computed: which runtime can recompute and compare this hash. Derived from
    # `encoding` rather than `method` so that a foreign record is described by
    # what it actually hashed -- a Python fingerprint says `method = "object"`
    # too, and deriving from that would claim R could reproduce it.
    portability = prop_computed(new_property(
      class_character,
      getter = function(self) hash_portability(self@encoding)
    ))
  ),
  validator = function(self) {
    if (!nzchar(self@hash)) {
      return("@hash must not be empty.")
    }
    if (!nzchar(self@encoding)) {
      return("@encoding must not be empty.")
    }
    if (self@method == "file" && is.null(self@source)) {
      return("@source must be set when @method is 'file'.")
    }
    if (
      !is.null(self@column_names) && length(self@column_names) != self@n_cols
    ) {
      return("@column_names must have one entry per column (@n_cols).")
    }
    NULL
  }
) # /rtemis::DataFingerprint


# %% hash_encoding ----
#' The byte recipe a hash method produces
#'
#' @param method Character: One of `DATA_HASH_METHODS`.
#'
#' @return Character: Encoding token.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hash_encoding <- function(method) {
  i <- match(method, DATA_HASH_ENCODINGS[["method"]])
  if (is.na(i)) {
    rtemis.core::abort(
      "Unknown hash method: ",
      method,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  DATA_HASH_ENCODINGS[["encoding"]][[i]]
} # /rtemis::hash_encoding


# %% hash_portability ----
#' Which runtime can recompute and compare a digest
#'
#' @param encoding Character: Encoding token.
#'
#' @return Character: "any" where the hashed form is defined outside any
#' runtime, the language that defines it otherwise, and "unknown" for a token
#' this build does not know.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hash_portability <- function(encoding) {
  i <- match(encoding, DATA_HASH_ENCODINGS[["encoding"]])
  # A token written by another implementation, or by a build with a different
  # pinned serialization version. Refusing to guess is the whole point: this
  # build cannot say who reproduces bytes it has never produced.
  if (is.na(i)) {
    return("unknown")
  }
  DATA_HASH_ENCODINGS[["portability"]][[i]]
} # /rtemis::hash_portability


# %% data_fingerprint ----
#' Fingerprint a dataset
#'
#' Computes a `DataFingerprint`: a content hash of `x` plus the structural facts
#' (dimensions, column names) that make a mismatch between two runs diagnosable.
#' Used to record which data a run actually used, so undocumented drift between
#' experiments is visible.
#'
#' @param x tabular data: The dataset. Required for every method, so that
#'   dimensions and column names are always recorded, even when hashing file
#'   bytes.
#' @param method Character \{"file", "object", "table"\}: What to hash. "object"
#'   hashes the serialized R object (cheap, R-only). "file" hashes the raw bytes
#'   of `source`. "table" hashes the canonical Arrow IPC representation
#'   (cross-language; requires the `arrow` package), and is the only method
#'   under which one table held as a data.frame, a data.table, a tibble or a
#'   matrix gives one hash.
#' @param algorithm Character \{"sha256", "sha512", "sha384", "sha224",
#'   "sha3-256", "sha3-512", "blake2b", "blake2s", "sha1", "md5"\}: Hash
#'   algorithm. "blake2b" and "blake2s" are the fast options; "sha1" and "md5"
#'   are for interoperability with systems that record those.
#' @param source Optional Character: Path `x` was read from. Required when
#'   `method` is "file".
#'
#' @return `DataFingerprint` object.
#'
#' @author EDG
#' @export
#' @examples
#' fp <- data_fingerprint(iris)
#' fp
#' # A logically identical table in a different R container hashes differently
#' # under "object", which is why its portability is "R". FALSE:
#' data_fingerprint(iris)@hash ==
#'   data_fingerprint(data.table::as.data.table(iris))@hash
data_fingerprint <- function(
  x,
  method = "object",
  algorithm = "sha256",
  source = NULL
) {
  check_character(method, allow_null = FALSE)
  check_character(algorithm, allow_null = FALSE)
  if (!method %in% DATA_HASH_METHODS) {
    rtemis.core::abort(
      "`method` must be one of ",
      paste0("'", DATA_HASH_METHODS, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!algorithm %in% DATA_HASH_ALGORITHMS) {
    rtemis.core::abort(
      "`algorithm` must be one of ",
      paste0("'", DATA_HASH_ALGORITHMS, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  hash <- switch(
    method,
    file = .hash_file(source, algorithm),
    object = .hash_object(x, algorithm),
    table = .hash_table(x, algorithm)
  )
  DataFingerprint(
    method = method,
    encoding = hash_encoding(method),
    algorithm = algorithm,
    hash = hash,
    n_rows = as.integer(NROW(x)),
    n_cols = as.integer(NCOL(x)),
    column_names = colnames(x),
    source = source
  )
} # /rtemis::data_fingerprint


# %% .hash_bytes ----
#' Hash a raw vector or a connection with the named algorithm
#'
#' The single point where an algorithm name becomes an implementation, so the
#' three hash methods cannot drift apart in which algorithms they accept.
#'
#' @param x Raw vector, or a connection to be streamed.
#' @param algorithm Character: One of `DATA_HASH_ALGORITHMS`.
#'
#' @return Character: Hash digest, as hex.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.hash_bytes <- function(x, algorithm) {
  hasher <- switch(
    algorithm,
    sha256 = openssl::sha256,
    sha512 = openssl::sha512,
    sha384 = openssl::sha384,
    sha224 = openssl::sha224,
    `sha3-256` = function(x) openssl::sha3(x, size = 256L),
    `sha3-512` = function(x) openssl::sha3(x, size = 512L),
    blake2b = openssl::blake2b,
    blake2s = openssl::blake2s,
    sha1 = openssl::sha1,
    md5 = openssl::md5,
    rtemis.core::abort(
      "Unknown hash algorithm: ",
      algorithm,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
  # `as.character()` on an `openssl` hash keeps its c("hash", "<algorithm>")
  # class. That class rides into `@hash` unnoticed -- `prop_string` checks the
  # type and a classed character is still a character -- and surfaces much later
  # as "No method asJSON S3 class: sha256" when a run record is written.
  # `as.vector()` drops it.
  as.vector(as.character(hasher(x)))
} # /rtemis::.hash_bytes


# %% .hash_file ----
#' Hash the raw bytes of a file
#'
#' Hashes a connection rather than the file's contents: `openssl` streams it in
#' chunks, so a file larger than memory is fingerprintable.
#'
#' @param path Character: File path.
#' @param algorithm Character: Hash algorithm.
#'
#' @return Character: Hash digest.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.hash_file <- function(path, algorithm) {
  if (is.null(path)) {
    rtemis.core::abort(
      "`source` is required when `method` is 'file'.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!file.exists(path)) {
    rtemis.core::abort(
      "File not found: ",
      path,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  .hash_bytes(con, algorithm)
} # /rtemis::.hash_file


# %% .serialization_offset ----
#' Where the object starts in a serialization stream
#'
#' A serialization stream opens with a header describing the R that wrote it,
#' not the object: hashing it would make identical data fingerprint differently
#' on another R build or in another locale. A version-3 header is "X\\n", then
#' three 4-byte big-endian integers (format version, writing R version, minimum
#' reading R version), then a 4-byte length and that many bytes naming the
#' native encoding. Everything after that is the object.
#'
#' @param bytes Raw vector: A version-3 serialization stream.
#'
#' @return Integer: Number of leading bytes that describe the writer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.serialization_offset <- function(bytes) {
  encoding_nchar <- readBin(
    bytes[15L:18L],
    what = "integer",
    n = 1L,
    size = 4L,
    endian = "big"
  )
  18L + encoding_nchar
} # /rtemis::.serialization_offset


# %% .hash_object ----
#' Hash the serialized R object
#'
#' The serialization version is pinned (see `DATA_HASH_SERIALIZE_VERSION`) and
#' the header skipped (see `.serialization_offset()`), so the hash depends on
#' the object alone rather than on the R that hashed it.
#'
#' The header is skipped by seeking a connection over the stream rather than by
#' dropping the bytes from it. Both give the same digest, but `bytes[-seq_len()]`
#' allocates a second copy of the whole stream, and negative indexing pays for
#' the complement of the index it was given: on an 80 MB frame that is around
#' 0.36 s and 80 MB to remove 23 bytes, which is most of the cost of
#' fingerprinting. `openssl` reads a connection from its current position, so
#' seeking costs neither.
#'
#' @param x R object.
#' @param algorithm Character: Hash algorithm.
#'
#' @return Character: Hash digest.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.hash_object <- function(x, algorithm) {
  bytes <- serialize(
    x,
    connection = NULL,
    version = DATA_HASH_SERIALIZE_VERSION
  )
  con <- rawConnection(bytes, open = "rb")
  on.exit(close(con), add = TRUE)
  seek(con, .serialization_offset(bytes))
  .hash_bytes(con, algorithm)
} # /rtemis::.hash_object


# %% .hash_table ----
#' Hash the canonical Arrow IPC representation of a table
#'
#' Arrow's IPC stream is a language-neutral binary form whose normalization the
#' Arrow spec fixes, so a Python implementation reading the same table produces
#' the same digest. That is what gives this method portability "any" where
#' `.hash_object()` is not.
#'
#' @param x tabular data.
#' @param algorithm Character: Hash algorithm.
#'
#' @return Character: Hash digest.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.hash_table <- function(x, algorithm) {
  check_dependencies("arrow")
  # `as_arrow_table()` has no method for a matrix, which the other two methods
  # take without complaint -- and `decomp()` fingerprints one. The conversion is
  # exact: a matrix and the data.frame of it produce the same Arrow table and so
  # the same digest, so this keeps the three methods interchangeable for a given
  # input rather than rejecting it with arrow's own error.
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  tbl <- arrow::as_arrow_table(x)
  # arrow stores R attributes and class under the schema metadata key "r" so it
  # can round-trip data.table/tibble-ness. That metadata differs between a
  # data.frame and a data.table holding identical data, which would make this
  # method hash them differently -- defeating its whole purpose and making the
  # portability "any" claim false. Strip it, so only the logical table is hashed.
  # (`$` and not `[[`: an arrow Table is an R6 object whose `metadata` is an
  # active binding; `[[<-` would try to drop a *column* of that name.)
  tbl$metadata <- NULL
  buffer <- arrow::write_to_raw(tbl, format = "stream")
  .hash_bytes(buffer, algorithm)
} # /rtemis::.hash_table


# %% same_data ----
#' Do two fingerprints identify the same dataset?
#'
#' Two fingerprints are comparable only if they were produced the same way, so
#' this is FALSE when `method` or `algorithm` differ -- an unequal comparison of
#' incomparable values would be worse than no answer.
#'
#' @param x,y `DataFingerprint` objects.
#'
#' @return Logical.
#'
#' @author EDG
#' @export
#' @examples
#' same_data(data_fingerprint(iris), data_fingerprint(iris))
same_data <- function(x, y) {
  if (!S7_inherits(x, DataFingerprint) || !S7_inherits(y, DataFingerprint)) {
    rtemis.core::abort(
      "`x` and `y` must be DataFingerprint objects.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  # `encoding` rather than `method`: two implementations both call R's
  # `serialize()` and Python's `pickle` "object", so matching methods do not
  # make matching digests, while matching encodings do.
  identical(x@encoding, y@encoding) &&
    identical(x@algorithm, y@algorithm) &&
    identical(x@hash, y@hash)
} # /rtemis::same_data


# %% fingerprint_diff ----
#' Describe how two fingerprints differ, for an informative message
#'
#' A hash alone says "different"; the structural facts say *how*. Returns the
#' most specific difference available, so a warning can tell a user whether they
#' have different rows, a different shape, or renamed columns.
#'
#' @param x,y `DataFingerprint` objects.
#'
#' @return Character: One-line description, or `NULL` if the fingerprints match.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fingerprint_diff <- function(x, y) {
  if (same_data(x, y)) {
    return(NULL)
  }
  if (
    !identical(x@encoding, y@encoding) || !identical(x@algorithm, y@algorithm)
  ) {
    return(paste0(
      "not comparable (",
      x@encoding,
      "/",
      x@algorithm,
      " vs ",
      y@encoding,
      "/",
      y@algorithm,
      ")"
    ))
  }
  if (!identical(x@n_rows, y@n_rows)) {
    return(paste0(
      "different number of rows (",
      x@n_rows,
      " vs ",
      y@n_rows,
      ")"
    ))
  }
  if (!identical(x@n_cols, y@n_cols)) {
    return(paste0(
      "different number of columns (",
      x@n_cols,
      " vs ",
      y@n_cols,
      ")"
    ))
  }
  if (!identical(x@column_names, y@column_names)) {
    return("same shape, different column names")
  }
  # Reached by elimination: comparable fingerprints, same shape, same column
  # names, different hash. Only "table" hashes a canonical form, so only there
  # does that isolate the contents. "object" and "file" hash a representation
  # too -- integer against double storage, factor level order, row order,
  # attribute order, line endings -- and each of those differs while the data
  # does not, so naming the values alone would send a reader hunting for a
  # change that never happened.
  if (identical(x@encoding, "arrow-ipc")) {
    "same shape and column names, different values"
  } else {
    "same shape and column names, different values or representation"
  }
} # /rtemis::fingerprint_diff


# %% warn_fingerprint_mismatch ----
#' Notify when a set of models was not trained on the same data
#'
#' Comparing metrics across models trained on different inputs can be a silent
#' failure -- the numbers look comparable and may not be. Every model that
#' carries a fingerprint is compared against the first; models without one
#' (trained before fingerprinting, or as a nested sub-model) are skipped rather
#' than reported, so the check degrades quietly instead of crying wolf.
#'
#' Printed rather than a real R warning, and never an error: comparing models
#' across different data is frequently deliberate -- feature selection,
#' ablations, differing preprocessing -- so it must not be escalatable to a
#' failure in a legitimate workflow. It reports a fact; the user judges it.
#'
#' @param models List of `Supervised` / `SupervisedRes` objects.
#'
#' @return `TRUE` if a mismatch was reported, otherwise `FALSE`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
warn_fingerprint_mismatch <- function(models) {
  fps <- lapply(models, function(m) prop(m, "data_fingerprint"))
  named <- names(models)
  keep <- !vapply(fps, is.null, logical(1L))
  if (sum(keep) < 2L) {
    return(invisible(FALSE))
  }
  fps <- fps[keep]
  labels <- if (is.null(named)) {
    vapply(models[keep], function(m) m@algorithm, character(1L))
  } else {
    named[keep]
  }
  reference <- fps[[1L]]
  diffs <- character()
  for (i in seq_along(fps)[-1L]) {
    d <- fingerprint_diff(reference, fps[[i]])
    if (!is.null(d)) {
      diffs <- c(diffs, paste0("  ", labels[[i]], ": ", d))
    }
  }
  if (length(diffs) == 0L) {
    return(invisible(FALSE))
  }
  # A printed notice, deliberately NOT a real R warning: comparing models
  # trained on different data is often entirely legitimate (feature selection,
  # ablations, different preprocessing), so this must not be escalatable to an
  # error by `options(warn = 2)` in a valid workflow. It informs; it does not
  # accuse.
  rtemis.core::warn(
    "Models were not all trained on the same data. Compared to '",
    labels[[1L]],
    "':\n",
    paste(diffs, collapse = "\n")
  )
  invisible(TRUE)
} # /rtemis::warn_fingerprint_mismatch


# %% repr.DataFingerprint ----
#' repr DataFingerprint
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, DataFingerprint) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name("DataFingerprint", pad = pad, output_type = output_type),
    repr_ls(
      list(
        hash = paste0(
          substr(x@hash, 1L, DATA_HASH_DISPLAY_CHARS),
          "\u2026 (",
          x@algorithm,
          ", ",
          x@method,
          ")"
        ),
        dim = paste0(x@n_rows, " x ", x@n_cols),
        source = x@source,
        portability = x@portability
      ),
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.DataFingerprint


# %% print.DataFingerprint ----
#' Print DataFingerprint
#'
#' @param x `DataFingerprint` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, DataFingerprint) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.DataFingerprint
