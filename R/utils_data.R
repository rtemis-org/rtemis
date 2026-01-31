# utils_data.R
# ::rtemis::
# EDG rtemis.org

#' @name get_factor_levels
#'
#' @title
#' Get factor levels from data.frame or similar
#'
#' @usage
#' get_factor_levels(x)
#'
#' @param x data.frame or data.table.
#'
#' @return Named list of factor levels. Names correspond to column names.
#'
#' @author EDG
#' @export
get_factor_levels <- new_generic(
  "get_factor_levels",
  "x",
  function(x) S7_dispatch()
)
method(get_factor_levels, class_data.frame) <- function(x) {
  factor_index <- which(sapply(x, is.factor))
  lapply(x[, factor_index, drop = FALSE], levels)
}
method(get_factor_levels, class_data.table) <- function(x) {
  factor_index <- which(sapply(x, is.factor))
  lapply(x[, factor_index, with = FALSE], levels)
}


#' Describe factor
#'
#' Outputs a single character with names and counts of each level of the input factor.
#'
#' @param x factor.
#' @param max_n Integer: Return counts for up to this many levels.
#' @param return_ordered Logical: If TRUE, return levels ordered by count, otherwise
#' return in level order.
#'
#' @return Character with level counts.
#'
#' @author EDG
#'
#' @export
#' @examples
#' \dontrun{
#' # Small number of levels
#' fct_describe(iris$Species)
#'
#' # Large number of levels: show top n by count
#' x <- factor(sample(letters, 1000, TRUE))
#' fct_describe(x)
#' fct_describe(x, 3)
#' }
fct_describe <- function(x, max_n = 5, return_ordered = TRUE) {
  x <- factor(x)
  x_levels <- levels(x)
  n_unique <- length(x_levels)
  x_freqs <- as.integer(table(x))
  if (return_ordered) {
    idi <- order(x_freqs, decreasing = TRUE)
  }

  if (n_unique <= max_n) {
    if (return_ordered) {
      paste(x_levels[idi], x_freqs[idi], sep = ": ", collapse = "; ")
    } else {
      paste(x_levels, x_freqs, sep = ": ", collapse = "; ")
    }
  } else {
    idi <- order(x_freqs, decreasing = TRUE)
    if (return_ordered) {
      idi <- idi[seq_len(max_n)]
      paste0(
        "(Top ",
        max_n,
        " of ",
        n_unique,
        ") ",
        paste(x_levels[idi], x_freqs[idi], sep = ": ", collapse = "; ")
      )
    } else {
      paste0(
        "(First ",
        max_n,
        " of ",
        n_unique,
        ") ",
        paste(x_levels, x_freqs, sep = ": ", collapse = "; ")
      )
    }
  }
} # /rtemis::fct_describe


#' Merge panel data treatment and outcome data
#'
#' Merge long format treatment and outcome data from multiple sources with possibly hierarchical
#' matching IDs using **data.table**
#'
#' @param x Named list: Long form datasets to merge. Will be converted to `data.table`
#' @param group_varnames Vector, character: Variable names to merge by, in order. If first is present on
#' a given pair of datasets, merge on that, otherwise try the next in line.
#' @param time_varname Character: Name of column that should be present in all datasets containing
#' time information.
#' @param start_date Date or character: Start date for final dataset in format "YYYY-MM-DD"
#' @param end_date Date or character: End dat for final dataset in format "YYYY-MM-DD"
#' @param interval_days Integer: Starting with `start_date` create timepoints every this many
#' days.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `data.table` with merged data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
merge_long_treatment <- function(
  x,
  group_varnames,
  time_varname = "Date",
  start_date,
  end_date,
  interval_days = 14L,
  verbosity = 1L
) {
  # Arguments ----
  if (!is.list(x)) {
    cli::cli_abort("x must be a named list")
  }
  n_sets <- length(x)
  if (is.null(names(x))) {
    names(x) <- paste0("Dataset", seq(x))
  }
  .names <- names(x)

  # Check there are at least 2 inputs
  if (n_sets < 2) {
    cli::cli_abort("Please provide at least 2 datasets as a named list in 'x'")
  }

  # Check all inputs contain at least one of group_varname and the time_varname
  for (i in seq(x)) {
    .names <- names(x[[i]])
    if (!time_varname %in% .names) {
      cli::cli_abort(
        "dataset",
        .names[i],
        "does not include time variable",
        time_varname
      )
    }
    if (any(!group_varnames %in% .names)) {
      cli::cli_abort(
        "Dataset",
        .names[i],
        "does not include any variable named",
        paste(group_varnames, collapse = " or ")
      )
    }
  }

  # Print input summary ----
  if (verbosity > 0L) {
    msg("There are", n_sets, "input datasets:")
    .summary <- t(data.frame(sapply(
      x,
      function(i) paste(NROW(i), "x", NCOL(i))
    )))
    printdf1(.summary, pad = 4)
  }

  # Base dataset ----
  # Contains final number of rows,
  # with "Date" and "ID" columns.
  # Each merge will add columns (not rows) by rolling joins
  dat <- data.table::as.data.table(expand.grid(
    Date = seq(
      as.Date(start_date),
      as.Date(end_date),
      interval_days
    ),
    ID = group_varnames[1]
  ))

  # Merges ----
  for (i in seq(x)) {
    .key <- group_varnames[min(which(group_varnames %in% names(x[[i]])))]
    setkeyv(dat, c(.key, time_varname))
    setkeyv(x[[i]], c(.key, time_varname))
    if (verbosity > 0L) {
      msg0(
        "Merge ",
        orange(i),
        " of ",
        orange(n_sets),
        ": Using keys ",
        paste0(highlight(.key), ", ", highlight(time_varname))
      )
    }
    # if (try({
    dat <- x[[i]][dat, roll = TRUE]
    # })) msg0("Successfully merged ", .names[i], ":")
    if (verbosity > 0L) {
      msg(
        "Merged dataset now contains",
        highlight(NROW(dat)),
        "rows and",
        highlight(NCOL(dat)),
        "columns"
      )
    }
  }

  dat
} # /rtemis::merge_long_treatment


#' Read all sheets of an XLSX file into a list
#'
#' @param x Character: path or URL to XLSX file
#' @param sheet Integer, vector: Sheet(s) to read. If NULL, will read all
#' sheets in `x`
#' @param startRow Integer, vector: First row to start reading. Will be
#' recycled as needed for all sheets
#' @param colNames Logical: If TRUE, use the first row of data
#' @param na.strings Character vector: stringd to be interpreted as NA
#' @param detectDates Logical: If TRUE, try to automatically detect dates
#' @param skipEmptyRows Logical: If TRUE, skip empty rows
#' @param skipEmptyCols Logical: If TRUE, skip empty columns
#'
#' @return List of data.frames
#'
#' @author EDG
#' @export
xlsx2list <- function(
  x,
  sheet = NULL,
  startRow = 1,
  colNames = TRUE,
  na.strings = "NA",
  detectDates = TRUE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE
) {
  if (is.null(sheet)) {
    sheet <- openxlsx::getSheetNames(x)
  }

  if (length(startRow) != length(sheet)) {
    startRow <- recycle(startRow, sheet)
  }

  out <- lapply(seq_along(sheet), \(i) {
    openxlsx::read.xlsx(
      x,
      sheet = i,
      startRow = startRow[i],
      colNames = colNames,
      na.strings = na.strings,
      detectDates = detectDates,
      skipEmptyRows = skipEmptyRows,
      skipEmptyCols = skipEmptyCols
    )
  })

  names(out) <- sheet

  out
} # /rtemis::xlsx2list


#' Match cases by covariates
#'
#' Find one or more cases from a `pool` data.frame that match cases in a target
#' data.frame. Match exactly and/or by distance (sum of squared distances).
#'
#' @param target data.frame you are matching against
#' @param pool data.frame you are looking for matches from
#' @param n_matches Integer: Number of matches to return
#' @param target_id Character: Column name in `target` that holds unique
#' cases IDs. Default = NULL, in which case integer case numbers will be used
#' @param pool_id Character: Same as `target_id` for `pool`
#' @param exactmatch_factors Logical: If TRUE, selected cases will have to
#' exactly match factors
#' available in `target`
#' @param exactmatch_cols Character: Names of columns that should be matched
#' exactly
#' @param distmatch_cols Character: Names of columns that should be
#' distance-matched
#' @param norepeats Logical: If TRUE, cases in `pool` can only be chosen
#' once.
#' @param ignore_na Logical: If TRUE, ignore NA values during exact matching.
#' @param verbosity Integer: Verbosity level.
#'
#' @return data.frame
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' set.seed(2021)
#' cases <- data.frame(
#'   PID = paste0("PID", seq(4)),
#'   Sex = factor(c(1, 1, 0, 0)),
#'   Handedness = factor(c(1, 1, 0, 1)),
#'   Age = c(21, 27, 39, 24),
#'   Var = c(.7, .8, .9, .6),
#'   Varx = rnorm(4)
#' )
#' controls <- data.frame(
#'   CID = paste0("CID", seq(50)),
#'   Sex = factor(sample(c(0, 1), 50, TRUE)),
#'   Handedness = factor(sample(c(0, 1), 50, TRUE, c(.1, .9))),
#'   Age = sample(16:42, 50, TRUE),
#'   Var = rnorm(50),
#'   Vary = rnorm(50)
#' )
#'
#' mc <- matchcases(cases, controls, 2, "PID", "CID")
#' }
matchcases <- function(
  target,
  pool,
  n_matches = 1,
  target_id = NULL,
  pool_id = NULL,
  exactmatch_factors = TRUE,
  exactmatch_cols = NULL,
  distmatch_cols = NULL,
  norepeats = TRUE,
  ignore_na = FALSE,
  verbosity = 1L
) {
  ntarget <- nrow(target)
  npool <- nrow(pool)

  # Get IDs ----
  if (is.null(target_id)) {
    targetID <- seq(ntarget)
  } else {
    targetID <- target[, target_id]
    target[, target_id] <- NULL
  }
  if (is.null(pool_id)) {
    poolID <- seq(npool)
  } else {
    poolID <- pool[, pool_id]
    pool[, pool_id] <- NULL
  }

  # exact- & dist-matched column names
  if (is.null(exactmatch_cols) && exactmatch_factors) {
    exactmatch_cols <- colnames(target)[sapply(target, is.factor)]
  }
  # Keep exactmatch_cols present in pool
  exactmatch_cols <- exactmatch_cols[exactmatch_cols %in% colnames(pool)]

  if (is.null(distmatch_cols)) {
    distmatch_cols <- colnames(target)[!colnames(target) %in% exactmatch_cols]
  }
  # Keep distmatch_cols present in pool
  distmatch_cols <- distmatch_cols[distmatch_cols %in% colnames(pool)]

  # Remove unused columns, if any
  .remove <- colnames(target)[
    !colnames(target) %in% c(exactmatch_cols, distmatch_cols)
  ]
  target[, .remove] <- NULL
  .remove <- colnames(pool)[
    !colnames(pool) %in% c(exactmatch_cols, distmatch_cols)
  ]
  pool[, .remove] <- NULL

  # Convert all non-exact-matching to numeric
  # index_num <- which(sapply(target, is.numeric))
  tonumeric <- distmatch_cols[!sapply(target[, distmatch_cols], is.numeric)]
  if (length(tonumeric) > 0) {
    target[, tonumeric] <- lapply(target[, tonumeric, drop = FALSE], as.numeric)
  }
  tonumeric <- distmatch_cols[!sapply(pool[, distmatch_cols], is.numeric)]
  if (length(tonumeric) > 0) {
    pool[, tonumeric] <- lapply(pool[, tonumeric, drop = FALSE], as.numeric)
  }

  # Normalize all
  vcat <- rbind(target, pool)
  vcat[, distmatch_cols] <- lapply(vcat[, distmatch_cols, drop = FALSE], scale)
  target_s <- cbind(targetID = targetID, vcat[seq(ntarget), ])
  pool_s <- cbind(poolID = poolID, vcat[-seq(ntarget), ])
  rm(vcat)

  # For each target, select matches on categoricals,
  # then order pool by distance.
  mc <- data.frame(targetID = targetID, match = matrix(NA, ntarget, n_matches))
  for (i in seq(ntarget)) {
    if (verbosity > 0L) {
      msg("Working on case", i, "of", ntarget)
    }
    if (is.null(exactmatch_cols)) {
      subpool <- pool_s
    } else {
      ind <- sapply(seq_len(nrow(pool_s)), function(j) {
        all(
          target_s[i, exactmatch_cols] == pool_s[j, exactmatch_cols],
          na.rm = ignore_na
        )
      })
      subpool <- pool_s[ind, , drop = FALSE]
    }
    # distord <- order(sapply(seq(nrow(subpool)),
    #                           function(j) sum((target_s[i, distmatch_cols] - subpool[j, distmatch_cols])^2)))
    distord <- order(sapply(
      seq_len(nrow(subpool)),
      function(j) {
        mse(
          unlist(target_s[i, distmatch_cols]),
          unlist(subpool[j, distmatch_cols]),
          na.rm = ignore_na
        )
      }
    ))
    n_matched <- min(n_matches, nrow(subpool))
    mc[i, 2:(n_matched + 1)] <- subpool[, 1][distord[seq(n_matched)]]
    if (norepeats) {
      pool_s <- pool_s[!pool_s[, 1] %in% mc[i, 2:(n_matches + 1)], ]
    }
  }

  mc
} # /rtemis::matchcases
