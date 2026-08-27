# 045_DataProfile.R
# ::rtemis::
# 2026- EDG rtemis.org

# What a dataset *is*, in the facts a validator needs and nothing more.
#
# The speech act is new. The three that came before it are a **request** (a
# config), an **assertion** (a record) and an **annotation** (defaults); this
# one is a **description**. It states measured facts about one dataset and makes
# no claim about what should be done with them.
#
# It exists so that validating a config against data does not require the data.
# Every data check rtemis performs reads summary statistics -- row count, column
# types, distinct counts, missing counts, outcome level counts -- and each is a
# single pass in any language. Publishing the description means the rtemis CLI
# computes it in polars and evaluates the same rules, instead of paying ~570ms
# to start R for a check that takes under a millisecond.
#
# Two shapes are deliberate:
#
# - **Arrays of records, not maps.** `columns` is a table with a `name` column
#   rather than an object keyed by name. Small expression languages iterate
#   arrays and cannot iterate objects, so a rule asking "which columns never
#   vary" is a `filter` over this and is not expressible over a map.
# - **Levels in long form**, one row per (column, level), as
#   `ClassificationMetrics@confusion_long` already does. A table's cells are
#   scalars, so the per-column level counts cannot nest inside `columns`; and
#   long form is what a `filter` reads anyway.

# %% PROFILE_DTYPES ----
# The column types a profile distinguishes, chosen so that every check can be
# answered from the token alone and so that R and polars map onto it without
# either dictating the vocabulary.
#
# Five are `PROP_TYPES`' own words, which is why they are spelled that way;
# "categorical" and "temporal" are added because a data column can be either and
# a property cannot.
#
# What the checks need from it:
# - "categorical" decides Classification, and contributes one encoded column per
#   level to the width `DIM_P_GT_N` compares.
# - "number" and "integer" decide Regression and contribute one column each.
#   They are kept apart although no check separates them: a binary outcome
#   stored as 0/1 is an integer with two distinct values, and that is the shape
#   an agent needs to see to ask whether classification was meant.
# - everything else is a type `train()` refuses as a predictor, which is
#   `FEATURE_TYPE_UNSUPPORTED`.
#
# Croissant interoperability. A Croissant 1.1 `cr:Field` carries a `dataType`
# that is a *list*: an atomic type plus, optionally, semantic ontology classes
# ("represented as text, means a city"). These tokens are only the atomic half,
# which is why they are not named `dataType` and are not expected to round-trip
# a Croissant field's full meaning. The atomic mapping a serializer would use:
#
#   number       sc:Float
#   integer      sc:Integer
#   boolean      sc:Boolean
#   string       sc:Text
#   temporal     sc:Date / sc:DateTime
#   categorical  sc:Text, plus a reference to an `sc:Enumeration` RecordSet
#   other        (none -- Croissant has no atomic type for it)
#
# Note the direction that does not invert: `categorical` and `string` both go to
# `sc:Text`, and only the enumeration reference tells them apart. A serializer
# reading Croissant back must consult that reference, not `dataType` alone.
# `PROFILE_MAX_LEVELS` also means a categorical may arrive with no level counts
# at all, which is a deliberate omission rather than an empty domain.
PROFILE_DTYPES <- c(
  "number",
  "integer",
  "categorical",
  "string",
  "boolean",
  "temporal",
  "other"
)

# Level counts are carried for a categorical column with at most this many
# levels. An identifier column read as a category would otherwise put one row
# per case into a document whose whole purpose is to be small.
PROFILE_MAX_LEVELS <- 64L


# %% DataProfile ----
#' DataProfile Class
#'
#' @description
#' Measured facts about one dataset: its size, its columns and their types, how
#' much of each is missing, how many distinct values each takes, and the level
#' counts of its low-cardinality categorical columns. Created by
#' [data_profile].
#'
#' Enough to answer every data check rtemis makes about a config, and small
#' enough to travel: bounded by the number of columns rather than the number of
#' rows, with one exception under the control of `PROFILE_MAX_LEVELS`.
#'
#' @field n_rows Integer [0, Inf): Number of rows (cases).
#' @field columns Optional List vector: One row per column of the dataset, as a
#'   `data.frame`, with its `name`, `dtype`, `n_distinct` (observed non-missing
#'   values) and `n_missing`.
#' @field level_counts Optional List vector: One row per (`column`, `level`)
#'   with its count `n`, as a `data.frame`, for categorical columns of at most
#'   `PROFILE_MAX_LEVELS` levels. Observed levels only -- named `level_counts`
#'   rather than `levels` because R's `levels()` means the *declared* set, and
#'   Croissant's `sc:Enumeration` likewise describes a complete domain, while
#'   this is what the rows actually contain.
#' @field n_complete_cases Integer [0, Inf): Rows with no missing value in any
#'   column.
#' @field n_duplicates Optional Integer [0, Inf): Rows that repeat an earlier
#'   row exactly. NULL when not counted.
#' @field fingerprint Optional `DataFingerprint`: Identity of the dataset
#'   described. NULL when not computed -- hashing is the one expensive thing
#'   here, and a profile is often made and read in one breath. Not named `data`:
#'   Croissant's `RecordSet.data` means embedded records, and this document is
#'   meant to be readable beside one.
#'
#' @author EDG
#' @noRd
DataProfile <- new_class(
  name = "DataProfile",
  package = "rtemis",
  properties = list(
    n_rows = prop_integer(
      0L,
      min = 0L,
      description = "Number of rows (cases)."
    ),
    columns = prop_table(
      columns = list(
        name = prop_string("", description = "Column name."),
        dtype = prop_string(
          PROFILE_DTYPES[[1L]],
          enum = PROFILE_DTYPES,
          description = "Column type, in the profile's own vocabulary."
        ),
        n_distinct = prop_integer(
          0L,
          min = 0L,
          description = "Distinct observed values, missing values excluded."
        ),
        n_missing = prop_integer(
          0L,
          min = 0L,
          description = "Missing values in this column."
        )
      ),
      nullable = TRUE,
      data_dependent = TRUE,
      description = "One row per column of the dataset, in column order."
    ),
    level_counts = prop_table(
      columns = list(
        column = prop_string("", description = "Column the level belongs to."),
        level = prop_string("", description = "Level, as a label."),
        n = prop_integer(0L, min = 0L, description = "Cases taking this level.")
      ),
      nullable = TRUE,
      data_dependent = TRUE,
      description = "Observed level counts in long form, one row per column and level, for categorical columns with at most 64 levels."
    ),
    n_complete_cases = prop_integer(
      0L,
      min = 0L,
      description = "Rows with no missing value in any column."
    ),
    n_duplicates = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Rows that repeat an earlier row exactly. Null when not counted."
    ),
    fingerprint = NULL | DataFingerprint
  )
) # /rtemis::DataProfile


# %% profile_dtype ----
#' The profile's type token for one column
#'
#' The R half of the vocabulary. A second implementation maps its own types onto
#' the same tokens; what must not happen is a token appearing here that
#' `PROFILE_DTYPES` does not declare, which is why the fallback is "other"
#' rather than the class name.
#'
#' @param x Vector: One column.
#'
#' @return Character: One of `PROFILE_DTYPES`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
profile_dtype <- function(x) {
  if (is.factor(x)) {
    return("categorical")
  }
  # Before `is.numeric()`: a Date is numeric underneath, and calling one a
  # number would let `FEATURE_TYPE_UNSUPPORTED` pass a column `train()` rejects.
  if (inherits(x, c("Date", "IDate", "POSIXct", "POSIXlt", "difftime"))) {
    return("temporal")
  }
  if (is.logical(x)) {
    return("boolean")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.numeric(x)) {
    return("number")
  }
  if (is.character(x)) {
    return("string")
  }
  "other"
} # /rtemis::profile_dtype


# %% data_profile ----
#' Profile a dataset
#'
#' Measures the facts [validate_config] checks a config against: dimensions,
#' column types, distinct and missing counts per column, level counts for
#' low-cardinality categorical columns, and the number of complete cases.
#'
#' One pass per column and no copy of the data, so profiling is cheap enough to
#' do per validation rather than caching it.
#'
#' @param x tabular data: The dataset.
#' @param n_duplicates Logical: If TRUE, count rows that repeat an earlier row.
#'   The one whole-table operation here; FALSE leaves the field NULL.
#' @param fingerprint Logical: If TRUE, record a [data_fingerprint] of `x`.
#'   Hashes the whole dataset, so it is off by default.
#'
#' @return `DataProfile` object.
#'
#' @author EDG
#' @export
#' @examples
#' p <- data_profile(iris)
#' p@columns
#' p@level_counts
data_profile <- function(x, n_duplicates = TRUE, fingerprint = FALSE) {
  check_tabular(x)
  check_logical(n_duplicates)
  check_logical(fingerprint)
  dat <- as.data.table(x)
  nms <- names(dat)
  dtypes <- vapply(dat, profile_dtype, character(1L))
  columns <- data.frame(
    name = nms,
    dtype = unname(dtypes),
    n_distinct = vapply(
      dat,
      function(v) as.integer(uniqueN(v, na.rm = TRUE)),
      integer(1L)
    ),
    n_missing = vapply(dat, function(v) sum(is.na(v)), integer(1L)),
    stringsAsFactors = FALSE
  )
  rownames(columns) <- NULL

  # Long form, and only where the level set is small enough to be a description
  # rather than a copy of the column.
  wanted <- nms[
    dtypes == "categorical" & columns[["n_distinct"]] <= PROFILE_MAX_LEVELS
  ]
  level_counts_df <- if (length(wanted) == 0L) {
    data.frame(
      column = character(),
      level = character(),
      n = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(
      rbind,
      lapply(wanted, function(nm) {
        # Observed levels only. `table()` on a factor reports every *declared*
        # level, including one no case takes -- but `n_distinct` counts what is
        # there, and a second implementation reading a CSV has no notion of a
        # declared-but-unused level at all. Reporting one would make the two
        # fields disagree and be unreproducible outside R.
        counts <- table(droplevels(dat[[nm]]))
        data.frame(
          column = nm,
          level = names(counts),
          n = as.integer(counts),
          stringsAsFactors = FALSE
        )
      })
    )
  }
  rownames(level_counts_df) <- NULL

  DataProfile(
    n_rows = as.integer(NROW(dat)),
    columns = columns,
    level_counts = level_counts_df,
    n_complete_cases = as.integer(sum(complete.cases(dat))),
    n_duplicates = if (n_duplicates) {
      as.integer(NROW(dat) - uniqueN(dat))
    },
    fingerprint = if (fingerprint) data_fingerprint(x)
  )
} # /rtemis::data_profile


# %% repr.DataProfile ----
#' repr DataProfile
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, DataProfile) <- function(x, pad = 0L, output_type = NULL) {
  types <- table(x@columns[["dtype"]])
  paste0(
    repr_S7name("DataProfile", pad = pad, output_type = output_type),
    repr_ls(
      list(
        dim = paste0(x@n_rows, " x ", NROW(x@columns)),
        types = paste0(names(types), ": ", as.integer(types), collapse = ", "),
        complete_cases = x@n_complete_cases,
        duplicates = x@n_duplicates,
        missing = sum(x@columns[["n_missing"]])
      ),
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.DataProfile


# %% print.DataProfile ----
#' Print `DataProfile`
#'
#' @param x `DataProfile` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, DataProfile) <- function(x, pad = 0L, output_type = NULL, ...) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.DataProfile
