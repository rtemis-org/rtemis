# 048_PartitionConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# How a held-out set is produced, as its own auditable step -- not a field on
# `SuperConfig`. A split is a decision (which rows train, which are held out,
# and by what rule), and a decision the record cannot report is a decision
# nothing downstream can audit. Mirrors `IngestConfig`'s shape exactly: an
# abstract base holding the discriminator, one leaf per method, each a
# `setup_*()` away, and a free function (`partition()`, alongside `ingest()`)
# that executes the config against real data and returns a manifest.
#
# Deliberately not a `SuperConfig` property. `SuperConfig` has always only
# *referenced* data; a split field would be the first data-manipulation
# operation in it, and once one exists there is no principled place to stop
# before a filter, then a join, then a derived column. `partition()` produces
# ordinary Parquet files (or, called directly on in-memory data, ordinary
# frames); a `SuperConfig` names the result the way it already names any
# other input.

# %% PartitionConfig ----
#' PartitionConfig Class
#'
#' @description
#' Abstract base for the partition family: how a dataset is divided into a
#' training set and a held-out test set. `method` is the discriminator and the
#' leaf holds what that method actually needs.
#'
#' @author EDG
#' @noRd
PartitionConfig <- new_class(
  name = "PartitionConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    method = class_character
  )
) # /rtemis::PartitionConfig


# %% RandomPartitionConfig ----
#' RandomPartitionConfig Class
#'
#' @description
#' A uniformly random split -- the right default when nothing about row order
#' or grouping is meaningful.
#'
#' @author EDG
#' @noRd
RandomPartitionConfig <- new_class(
  name = "RandomPartitionConfig",
  parent = PartitionConfig,
  package = "rtemis",
  properties = list(
    method = prop_algorithm("random"),
    train_p = prop_float(
      0.75,
      exclusive_min = 0,
      exclusive_max = 1,
      description = "Training set fraction."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::RandomPartitionConfig


# %% TimePartitionConfig ----
#' TimePartitionConfig Class
#'
#' @description
#' A split by time order: the earliest-ordered cases train, the
#' latest-ordered are held out. The natural split for data where a model
#' trained on the future to predict the past would overstate what it can do.
#'
#' @author EDG
#' @noRd
TimePartitionConfig <- new_class(
  name = "TimePartitionConfig",
  parent = PartitionConfig,
  package = "rtemis",
  properties = list(
    method = prop_algorithm("time"),
    # No default: a time split with no column to order by is not a request
    # anything can act on, the same reason `IngestConfig@format` has none --
    # but a config schema may not require a leaf property (only the
    # discriminator may be required), so this stays nullable and
    # `setup_TimePartition()` aborts on a NULL, the way `setup_Custom()`
    # already does for `resamples`.
    column = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the column to order cases by before splitting."
    ),
    train_p = prop_float(
      0.75,
      exclusive_min = 0,
      exclusive_max = 1,
      description = "Fraction of the earliest-ordered cases assigned to training."
    )
  )
) # /rtemis::TimePartitionConfig


# %% GroupPartitionConfig ----
#' GroupPartitionConfig Class
#'
#' @description
#' A split that keeps every case sharing a group ID on the same side, so a
#' subject with repeated measurements cannot appear in both the training and
#' the test set.
#'
#' @author EDG
#' @noRd
GroupPartitionConfig <- new_class(
  name = "GroupPartitionConfig",
  parent = PartitionConfig,
  package = "rtemis",
  properties = list(
    method = prop_algorithm("group"),
    # See `TimePartitionConfig@column` for why this is nullable rather than
    # required.
    column = prop_string(
      NULL,
      nullable = TRUE,
      description = paste0(
        "Name of the column holding per-case group IDs; cases sharing an ID ",
        "stay in the same partition."
      )
    ),
    train_p = prop_float(
      0.75,
      exclusive_min = 0,
      exclusive_max = 1,
      description = paste0(
        "Approximate training set fraction, honored at the group level: ",
        "groups are assigned whole, so the realized fraction of rows varies ",
        "with group sizes."
      )
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::GroupPartitionConfig


# %% PredefinedPartitionConfig ----
#' PredefinedPartitionConfig Class
#'
#' @description
#' A split already recorded in the data itself -- a column naming, per case,
#' which side it belongs to. The right method when the partition was decided
#' outside rtemis (a prior study, a regulatory submission) and must be
#' reproduced exactly rather than re-derived.
#'
#' @author EDG
#' @noRd
PredefinedPartitionConfig <- new_class(
  name = "PredefinedPartitionConfig",
  parent = PartitionConfig,
  package = "rtemis",
  properties = list(
    method = prop_algorithm("predefined"),
    # See `TimePartitionConfig@column` for why this is nullable rather than
    # required.
    column = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the column holding each case's partition label."
    ),
    training_value = prop_string(
      "train",
      description = "Value in `column` identifying training cases."
    ),
    test_value = prop_string(
      "test",
      description = "Value in `column` identifying held-out test cases."
    )
  )
) # /rtemis::PredefinedPartitionConfig


# The leaf per method. Stated once, the way `INGEST_CLASSES`/`INGEST_SETUP`
# are: a method added here reaches `partition()` and `.list_to_PartitionConfig()`
# both.
PARTITION_CLASSES <- list(
  random = RandomPartitionConfig,
  time = TimePartitionConfig,
  group = GroupPartitionConfig,
  predefined = PredefinedPartitionConfig
)

PARTITION_SETUP <- c(
  random = "setup_RandomPartition",
  time = "setup_TimePartition",
  group = "setup_GroupPartition",
  predefined = "setup_PredefinedPartition"
)


# %% setup_RandomPartition ----
#' Set up a `RandomPartitionConfig`
#'
#' @description
#' A uniformly random split -- the right default when nothing about row order
#' or grouping is meaningful.
#'
#' @param train_p Numeric (0, 1): Training set fraction.
#' @param seed Optional Integer [0, Inf): Random seed.
#'
#' @return `RandomPartitionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_RandomPartition(train_p = 0.8, seed = 2026L)
setup_RandomPartition <- function(train_p = 0.75, seed = NULL) {
  RandomPartitionConfig(train_p = train_p, seed = clean_int(seed))
} # /rtemis::setup_RandomPartition


# %% setup_TimePartition ----
#' Set up a `TimePartitionConfig`
#'
#' @description
#' A split by time order: the earliest-ordered cases train, the
#' latest-ordered are held out.
#'
#' @param column Optional Character: Name of the column to order cases by before
#' splitting.
#' @param train_p Numeric (0, 1): Fraction of the earliest-ordered cases
#' assigned to training.
#'
#' @return `TimePartitionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_TimePartition(column = "visit_date", train_p = 0.8)
setup_TimePartition <- function(column = NULL, train_p = 0.75) {
  if (is.null(column)) {
    rtemis.core::abort(
      "A time split needs a `column` to order cases by.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  TimePartitionConfig(column = column, train_p = train_p)
} # /rtemis::setup_TimePartition


# %% setup_GroupPartition ----
#' Set up a `GroupPartitionConfig`
#'
#' @description
#' A split that keeps every case sharing a group ID on the same side.
#'
#' @param column Optional Character: Name of the column holding per-case group IDs.
#' @param train_p Numeric (0, 1): Approximate training set fraction, honored
#' at the group level.
#' @param seed Optional Integer [0, Inf): Random seed.
#'
#' @return `GroupPartitionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_GroupPartition(column = "subject_id", train_p = 0.8, seed = 2026L)
setup_GroupPartition <- function(column = NULL, train_p = 0.75, seed = NULL) {
  if (is.null(column)) {
    rtemis.core::abort(
      "A group split needs a `column` naming the per-case group IDs.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  GroupPartitionConfig(
    column = column,
    train_p = train_p,
    seed = clean_int(seed)
  )
} # /rtemis::setup_GroupPartition


# %% setup_PredefinedPartition ----
#' Set up a `PredefinedPartitionConfig`
#'
#' @description
#' A split already recorded in the data itself.
#'
#' @param column Optional Character: Name of the column holding each case's partition
#' label.
#' @param training_value Character: Value in `column` identifying training
#' cases.
#' @param test_value Character: Value in `column` identifying held-out test
#' cases.
#'
#' @return `PredefinedPartitionConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_PredefinedPartition(column = "split")
setup_PredefinedPartition <- function(
  column = NULL,
  training_value = "train",
  test_value = "test"
) {
  if (is.null(column)) {
    rtemis.core::abort(
      "A predefined split needs a `column` holding each case's partition label.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  PredefinedPartitionConfig(
    column = column,
    training_value = training_value,
    test_value = test_value
  )
} # /rtemis::setup_PredefinedPartition


# %% .partition_indices ----
#' Row indices for each side of a split
#'
#' @param dat data.frame or data.table: The dataset to split.
#' @param config `PartitionConfig`: The split rule.
#'
#' @return Named list: `training` and `test`, each an integer vector of row
#'   indices into `dat`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.partition_indices <- function(dat, config) {
  n <- NROW(dat)
  if (S7_inherits(config, RandomPartitionConfig)) {
    if (!is.null(config@seed)) {
      set.seed(config@seed)
    }
    n_train <- round(config@train_p * n)
    train_idi <- sample(n, n_train)
    return(list(
      training = sort(train_idi),
      test = sort(setdiff(seq_len(n), train_idi))
    ))
  }
  if (S7_inherits(config, TimePartitionConfig)) {
    .check_partition_column(dat, config@column)
    ord <- order(dat[[config@column]])
    n_train <- round(config@train_p * n)
    return(list(
      training = sort(ord[seq_len(n_train)]),
      test = sort(ord[seq_len(n) > n_train])
    ))
  }
  if (S7_inherits(config, GroupPartitionConfig)) {
    .check_partition_column(dat, config@column)
    if (!is.null(config@seed)) {
      set.seed(config@seed)
    }
    groups <- dat[[config@column]]
    group_sizes <- table(groups)
    shuffled <- sample(names(group_sizes))
    cum_n <- cumsum(group_sizes[shuffled])
    n_train <- config@train_p * n
    # The first groups, in shuffled order, whose cumulative size reaches the
    # target fraction -- at least one group trains even if it alone exceeds it.
    train_groups <- shuffled[seq_len(max(1L, sum(cum_n <= n_train)))]
    is_train <- groups %in% train_groups
    return(list(
      training = sort(which(is_train)),
      test = sort(which(!is_train))
    ))
  }
  if (S7_inherits(config, PredefinedPartitionConfig)) {
    .check_partition_column(dat, config@column)
    labels <- dat[[config@column]]
    unexpected <- setdiff(
      unique(labels),
      c(config@training_value, config@test_value)
    )
    if (length(unexpected) > 0L) {
      rtemis.core::abort(
        "`",
        config@column,
        "` holds values other than `training_value`/`test_value`: ",
        paste0("'", unexpected, "'", collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
    return(list(
      training = sort(which(labels == config@training_value)),
      test = sort(which(labels == config@test_value))
    ))
  }
  rtemis.core::abort(
    "Unsupported partition method: ",
    config@method,
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /rtemis::.partition_indices


# %% .check_partition_column ----
#' Confirm a partition config's column names a real column
#'
#' @param dat data.frame or data.table: The dataset to split.
#' @param column Character or NULL: Column name to check.
#'
#' @return Invisible NULL. Aborts if `column` is missing from `dat`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_partition_column <- function(dat, column) {
  if (!column %in% names(dat)) {
    rtemis.core::abort(
      "`",
      column,
      "` is not a column of the data. Columns are: ",
      paste0("'", names(dat), "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  invisible(NULL)
} # /rtemis::.check_partition_column


# %% partition ----
#' Split a dataset into training and held-out test partitions
#'
#' @description
#' A held-out split is a decision -- which rows train, which are held out, and
#' by what rule -- and a decision the record cannot report is one nothing
#' downstream can audit. `partition()` makes it a first-class, executed step,
#' the same way `ingest()` makes reading a file one: the config states the
#' rule, this function applies it and returns a manifest naming exactly what
#' happened, including a `DataFingerprint` of each side.
#'
#' Not a `SuperConfig` property. `SuperConfig`'s `dat_training_path` and
#' `dat_test_path` simply name whatever `partition()` (or any other step)
#' produced -- the same relationship they already have with `ingest()`'s
#' output.
#'
#' @param dat data.frame or data.table: The dataset to split.
#' @param config `PartitionConfig`: The split rule.
#' @param outdir Optional Character: Directory to write `training.parquet` and
#' `test.parquet` to; created if it does not exist. If `NULL`, nothing is
#' written and each `outputs` entry carries its fingerprint and row count
#' without a `path`.
#' @param overwrite Logical: If TRUE, overwrite existing output files.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: the partition manifest -- `config`, `source` (a
#' `DataFingerprint` of `dat`), and `outputs` (a list with `training` and
#' `test`, each holding a `fingerprint` and, when `outdir` is given, the `path`
#' written to).
#'
#' @author EDG
#' @export
#' @examples
#' partition(iris, setup_RandomPartition(train_p = 0.8, seed = 2026L))
partition <- function(
  dat,
  config,
  outdir = NULL,
  overwrite = FALSE,
  verbosity = 1L
) {
  check_is_S7(config, PartitionConfig)
  idi <- .partition_indices(dat, config)
  training <- dat[idi[["training"]], , drop = FALSE]
  test <- dat[idi[["test"]], , drop = FALSE]

  if (!is.null(outdir)) {
    check_dependencies("arrow")
    outdir <- sanitize_path(
      outdir,
      must_exist = FALSE,
      type = "any",
      normalize = FALSE
    )
    if (
      !dir.exists(outdir) &&
        !dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    ) {
      rtemis.core::abort(
        "Cannot create output directory: ",
        outdir,
        ".",
        class = c("rtemis_file_error", "rtemis_io_error")
      )
    }
  }

  write_side <- function(side_dat, name) {
    if (is.null(outdir)) {
      return(list(fingerprint = data_fingerprint(side_dat)))
    }
    path <- file.path(outdir, paste0(name, ".parquet"))
    if (file.exists(path) && !overwrite) {
      rtemis.core::abort(
        "Output file exists: ",
        path,
        ". Pass `overwrite = TRUE` to replace it.",
        class = c("rtemis_file_error", "rtemis_input_error")
      )
    }
    arrow::write_parquet(side_dat, path)
    list(path = path, fingerprint = data_fingerprint(side_dat))
  }

  training_out <- write_side(training, "training")
  test_out <- write_side(test, "test")

  msg0(
    bold(highlight("\U25B6")),
    " Partitioned ",
    highlight(NROW(dat)),
    " cases into ",
    highlight(NROW(training)),
    " training and ",
    highlight(NROW(test)),
    " test (",
    config@method,
    ").",
    verbosity = verbosity
  )

  # Not yet a published record, the same reason `ingest()`'s manifest is not:
  # what a partition *node* records, and whether it is its own record or a
  # block in the run's, is a question for whichever architecture ends up
  # orchestrating multi-step pipelines. Returned as a manifest so that
  # information exists to shape it from.
  list(
    config = S7_to_list(config),
    source = S7_to_list(data_fingerprint(dat)),
    outputs = list(
      training = list(
        path = training_out[["path"]],
        fingerprint = S7_to_list(training_out[["fingerprint"]]),
        n_rows = as.integer(NROW(training))
      ),
      test = list(
        path = test_out[["path"]],
        fingerprint = S7_to_list(test_out[["fingerprint"]]),
        n_rows = as.integer(NROW(test))
      )
    )
  )
} # /rtemis::partition


# %% .list_to_PartitionConfig ----
#' Rebuild a `PartitionConfig` from a parsed config document
#'
#' `method` picks the constructor, and every other key is one of its
#' arguments -- so a key belonging to a different method fails there as an
#' unused argument, naming it, rather than being dropped.
#'
#' @param x Named list: The parsed document.
#'
#' @return A `PartitionConfig` subclass.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_PartitionConfig <- function(x) {
  keys <- unique(unlist(c(
    list(names(PartitionConfig@properties)),
    lapply(PARTITION_CLASSES, function(cls) names(cls@properties))
  )))
  check_wire_keys(x, keys, "partition config")
  args <- .drop_meta_keys(x)
  method <- args[["method"]]
  if (is.null(method) || !method %in% names(PARTITION_SETUP)) {
    rtemis.core::abort(
      "A partition config needs a `method`, one of: ",
      paste0("'", names(PARTITION_SETUP), "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  args[["method"]] <- NULL
  do.call(PARTITION_SETUP[[method]], args)
} # /rtemis::.list_to_PartitionConfig
