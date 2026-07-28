# 040_DataFingerprint.R
# ::rtemis::
# 2026- EDG rtemis.org

# Dataset identity for provenance records: what data a run actually used, so
# that across a batch of experiments a user can tell whether runs shared an
# input or drifted undocumented. A path alone does not establish this — the file
# at that path can change — so a `DataFingerprint` pairs a content hash with the
# cheap structural facts (dimensions, column names) that make a mismatch
# *diagnosable* rather than merely detectable.
#
# Three hash methods, answering three different questions:
# - "file"    raw bytes of the source file. Answers "the same file?", and is
#             sensitive to changes that do not affect the data (line endings,
#             trailing whitespace).
# - "object"  the serialized R object. The default: cheap, always available,
#             but sensitive to R-internal representation (data.frame vs
#             data.table, factor level order, integer vs double storage), so
#             two logically identical tables can differ.
# - "table"   canonical logical content via Arrow IPC. Strongest and most
#             portable, but requires the suggested `arrow` package.
#
# `portability` is computed from `method` rather than stored, so it cannot
# contradict it.

# %% Constants ----
# Hash methods, ordered weakest to strongest guarantee.
DATA_HASH_METHODS <- c("file", "object", "table")

# `digest` algorithms accepted for `DataFingerprint@algorithm`.
DATA_HASH_ALGORITHMS <- c(
  "sha256",
  "sha512",
  "sha1",
  "md5",
  "xxhash64",
  "xxhash32",
  "xxh3_64",
  "xxh3_128",
  "blake3",
  "spookyhash",
  "murmur32",
  "crc32",
  "crc32c"
)

# Pinned so that "object" hashes are reproducible. `digest()` hashes
# `serialize()` output, whose bytes depend on the serialization format version,
# so leaving this to the default would make those hashes irreproducible across R
# versions that change it. Changing this value is a BREAKING change to
# fingerprint comparability.
DATA_HASH_SERIALIZE_VERSION <- 3L

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
#' @field algorithm Character \{"sha256", "sha512", "sha1", "md5", "xxhash64", "xxhash32", "xxh3_64", "xxh3_128", "blake3", "spookyhash", "murmur32", "crc32", "crc32c"\}: Hash algorithm.
#' @field hash Character: Hash digest, as hex.
#' @field n_rows Integer [0, Inf): Number of rows (cases).
#' @field n_cols Integer [0, Inf): Number of columns.
#' @field column_names Optional Character vector: Column names, in order.
#' @field source Optional Character: Path the data was read from.
#' @field portability Character: Computed from `method`; which contexts can
#'   recompute and compare this hash.
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
    # Computed: which contexts can recompute and compare this hash. Derived from
    # `method` so it cannot contradict it.
    portability = prop_computed(new_property(
      class_character,
      getter = function(self) hash_portability(self@method)
    ))
  ),
  validator = function(self) {
    if (!nzchar(self@hash)) {
      return("@hash must not be empty.")
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


# %% hash_portability ----
#' Contexts in which a hash method can be recomputed and compared
#'
#' @param method Character: One of `DATA_HASH_METHODS`.
#'
#' @return Character: "cross_language" or "single_language".
#'
#' @author EDG
#' @keywords internal
#' @noRd
hash_portability <- function(method) {
  switch(
    method,
    file = "cross_language",
    table = "cross_language",
    # R `serialize()` output, so R-only, and additionally sensitive to the
    # pinned serialization version and to the package defining the object's
    # class (data.table vs data.frame vs tibble).
    object = "single_language",
    rtemis.core::abort(
      "Unknown hash method: ",
      method,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
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
#'   (cross-language; requires the `arrow` package).
#' @param algorithm Character: Hash algorithm passed to [digest::digest]. See
#'   `DATA_HASH_ALGORITHMS`.
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
#' # A logically identical table with a different R representation hashes
#' # differently under "object", which is why it is "single_language":
#' data_fingerprint(iris)@hash == data_fingerprint(as.data.frame(iris))@hash
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
    algorithm = algorithm,
    hash = hash,
    n_rows = as.integer(NROW(x)),
    n_cols = as.integer(NCOL(x)),
    column_names = colnames(x),
    source = source
  )
} # /rtemis::data_fingerprint


# %% .hash_file ----
#' Hash the raw bytes of a file
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
  digest::digest(file = path, algo = algorithm)
} # /rtemis::.hash_file


# %% .hash_object ----
#' Hash the serialized R object
#'
#' `serializeVersion` is pinned (see `DATA_HASH_SERIALIZE_VERSION`) so the
#' digest is reproducible across R versions that change the serialization
#' default.
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
  digest::digest(
    x,
    algo = algorithm,
    serializeVersion = DATA_HASH_SERIALIZE_VERSION
  )
} # /rtemis::.hash_object


# %% .hash_table ----
#' Hash the canonical Arrow IPC representation of a table
#'
#' Arrow's IPC stream is a language-neutral binary form whose normalization the
#' Arrow spec fixes, so a Python implementation reading the same table produces
#' the same digest. That is what makes this method "cross_language" where
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
  tbl <- arrow::as_arrow_table(x)
  # arrow stores R attributes and class under the schema metadata key "r" so it
  # can round-trip data.table/tibble-ness. That metadata differs between a
  # data.frame and a data.table holding identical data, which would make this
  # method hash them differently -- defeating its whole purpose and making the
  # "cross_language" claim false. Strip it, so only the logical table is hashed.
  # (`$` and not `[[`: an arrow Table is an R6 object whose `metadata` is an
  # active binding; `[[<-` would try to drop a *column* of that name.)
  tbl$metadata <- NULL
  buffer <- arrow::write_to_raw(tbl, format = "stream")
  digest::digest(
    buffer,
    algo = algorithm,
    serialize = FALSE
  )
} # /rtemis::.hash_table


# %% same_data ----
#' Do two fingerprints identify the same dataset?
#'
#' Two fingerprints are comparable only if they were produced the same way, so
#' this is FALSE when `method` or `algorithm` differ — an unequal comparison of
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
  identical(x@method, y@method) &&
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
  if (!identical(x@method, y@method) || !identical(x@algorithm, y@algorithm)) {
    return(paste0(
      "not comparable (",
      x@method,
      "/",
      x@algorithm,
      " vs ",
      y@method,
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
  "same shape and column names, different values"
} # /rtemis::fingerprint_diff


# %% warn_fingerprint_mismatch ----
#' Notify when a set of models was not trained on the same data
#'
#' Comparing metrics across models trained on different inputs can be a silent
#' failure — the numbers look comparable and may not be. Every model that
#' carries a fingerprint is compared against the first; models without one
#' (trained before fingerprinting, or as a nested sub-model) are skipped rather
#' than reported, so the check degrades quietly instead of crying wolf.
#'
#' Printed rather than a real R warning, and never an error: comparing models
#' across different data is frequently deliberate — feature selection,
#' ablations, differing preprocessing — so it must not be escalatable to a
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
          "… (",
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
