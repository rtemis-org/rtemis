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
#' @keywords internal
#' @noRd
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
#' fct_describe(iris[["Species"]])
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


#' Match cases by covariates
#'
#' Find one or more cases from a `pool` data.frame that match cases in a target
#' data.frame. Match exactly and/or by distance (sum of squared distances).
#'
#' @param target data.frame you are matching against.
#' @param pool data.frame you are looking for matches from.
#' @param n_matches Integer: Number of matches to return.
#' @param target_id Character: Column name in `target` that holds unique
#' cases IDs. Default = NULL, in which case integer case numbers will be used.
#' @param pool_id Character: Same as `target_id` for `pool`.
#' @param exactmatch_factors Logical: If TRUE, selected cases will have to
#' exactly match factors available in `target`.
#' @param exactmatch_cols Character: Names of columns that should be matched
#' exactly.
#' @param distmatch_cols Character: Names of columns that should be
#' distance-matched.
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

  # Get IDs
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


#' Index columns by attribute name & value
#'
#' @param x data.frame or compatible
#' @param name Character: Name of attribute
#' @param value Character: Value of attribute
#'
#' @return Integer vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- data.table(
#'   id = 1:5,
#'   sbp = rnorm(5, 120, 15),
#'   dbp = rnorm(5, 80, 10),
#'   paO2 = rnorm(5, 90, 10),
#'   paCO2 = rnorm(5, 40, 5)
#' )
#' setattr(x[["sbp"]], "source", "outpatient")
#' setattr(x[["dbp"]], "source", "outpatient")
#' setattr(x[["paO2"]], "source", "icu")
#' setattr(x[["paCO2"]], "source", "icu")
#' dt_index_attr(x, "source", "icu")
index_col_by_attr <- function(x, name, value) {
  colattr <- unlist(sapply(x, \(i) attr(i, name)))
  which(colattr == value)
} # /rtemis::index_col_by_attr


#' Tabulate column attributes
#'
#' @param x data.table
#' @param attr Character: Attribute to get
#' @param useNA Character: Passed to `table`
#'
#' @return table.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- data.table(
#'   id = 1:5,
#'   sbp = rnorm(5, 120, 15),
#'   dbp = rnorm(5, 80, 10),
#'   paO2 = rnorm(5, 90, 10),
#'   paCO2 = rnorm(5, 40, 5)
#' )
#' setattr(x[["sbp"]], "source", "outpatient")
#' setattr(x[["dbp"]], "source", "outpatient")
#' setattr(x[["paO2"]], "source", "icu")
#' setattr(x[["paCO2"]], "source", "icu")
#' dt_get_column_attr(x, "source")
table_column_attr <- function(x, attr = "source", useNA = "always") {
  attrs <- sapply(x, \(i) {
    if (is.null(attr(i, attr, exact = TRUE))) {
      NA_character_
    } else {
      attr(i, attr, exact = TRUE)
    }
  })
  table(attrs, useNA = useNA)
} # /rtemis::table_column_attr


#' List column names by class
#'
#' @param x data.table
#' @param sorted Logical: If TRUE, sort the output
#' @param item_format Function: Function to format each item
#' @param maxlength Integer: Maximum number of items to print
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' names_by_class(iris)
names_by_class <- function(
  x,
  sorted = TRUE,
  item_format = highlight,
  maxlength = 24
) {
  classes <- sapply(x, class)
  vals <- unique(classes)
  out <- if (sorted) {
    sapply(vals, \(i) sort(names(x)[classes == i]))
  } else {
    sapply(vals, \(i) names(x)[classes == i])
  }
  cat(repr_ls(out, item_format = item_format, maxlength = maxlength))
  invisible()
} # /rtemis::names_by_class
