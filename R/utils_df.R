# dataops
# ::rtemis::
# 2021 EDG rtemis.org

#' Get names by string matching
#'
#' @param x object with `names()` method.
#' @param pattern Character: pattern to match anywhere in names of x.
#' @param starts_with Character: pattern to match in the beginning of names of x.
#' @param ends_with Character: pattern to match at the end of names of x.
#' @param ignore_case Logical: If TRUE, well, ignore case.
#'
#' @return Character vector of matched names.
#'
#' @author EDG
#' @export
getnames <- function(
  x,
  pattern = NULL,
  starts_with = NULL,
  ends_with = NULL,
  ignore_case = TRUE
) {
  .names <- if (is.character(x)) {
    x
  } else {
    names(x)
  }
  if (!is.null(pattern)) {
    .names[grep(pattern, .names, ignore.case = ignore_case)]
  } else if (!is.null(starts_with)) {
    .names[grep(paste0("^", starts_with), .names, ignore.case = ignore_case)]
  } else if (!is.null(ends_with)) {
    .names[grep(paste0(ends_with, "$"), .names, ignore.case = ignore_case)]
  }
}

#' Get names by string matching multiple patterns
#'
#' @param x Character vector or object with `names()` method.
#' @param pattern Character vector: pattern(s) to match anywhere in names of x.
#' @param starts_with Character: pattern to match in the beginning of names of x.
#' @param ends_with Character: pattern to match at the end of names of x.
#' @param ignore_case Logical: If TRUE, well, ignore case.
#' @param return_index Logical: If TRUE, return integer index of matches instead of names.
#'
#' @return Character vector of matched names or integer index.
#'
#' @author EDG
#' @export
mgetnames <- function(
  x,
  pattern = NULL,
  starts_with = NULL,
  ends_with = NULL,
  ignore_case = TRUE,
  return_index = FALSE
) {
  .names <- if (is.character(x)) x else names(x)
  idi <- numeric()
  if (!is.null(pattern)) {
    idi <- c(
      idi,
      unlist(lapply(
        pattern,
        function(p) grep(p, .names, ignore.case = ignore_case)
      ))
    )
  }
  if (!is.null(starts_with)) {
    idi <- c(idi, which(startsWith(.names, starts_with)))
  }
  if (!is.null(ends_with)) {
    idi <- c(idi, which(endsWith(.names, ends_with)))
  }
  idi <- unique(idi)
  if (return_index) {
    idi
  } else {
    .names[idi]
  }
}

#' Get factor/numeric/logical/character names from data.frame/data.table
#'
#' @name get-names
#' @param x data.frame or data.table (or data.frame-compatible object)
#'
#' @return Character vector of column names of x with the specified class.
#'
#' @author EDG
#' @export
getfactornames <- function(x) names(x)[sapply(x, is.factor)]

#' @rdname getnames
#' @export
getnumericnames <- function(x) names(x)[sapply(x, is.numeric)]

#' @rdname getnames
#' @export
getlogicalnames <- function(x) names(x)[sapply(x, is.logical)]

#' @rdname getnames
#' @export
getcharacternames <- function(x) names(x)[sapply(x, is.character)]

#' @rdname getnames
#' @export
getdatenames <- function(x) {
  date_id <- sapply(
    x,
    \(v) class(v)[1] %in% c("Date", "IDate", "POSIXct", "POSIXlt")
  )
  names(x)[date_id]
}

#' Get data.frame names and types
#'
#' @param x data.frame / data.table or similar
#' @return character vector of column names with attribute "type" holding the class of each
#' column
#'
#' @export
getnamesandtypes <- function(x) {
  xnames <- names(x)
  attr(xnames, "type") <- sapply(x, class)
  xnames
} # rtemis::namesandtypes


#' Unique values per feature
#'
#' Get number of unique values per features
#'
#' @param x matrix or data frame input
#' @param excludeNA Logical: If TRUE, exclude NA values from unique count.
#'
#' @return Vector, integer of length `NCOL(x)` with number of unique
#' values per column/feature
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' uniquevalsperfeat(iris)
#' }
uniquevalsperfeat <- function(x, excludeNA = FALSE) {
  if (excludeNA) {
    apply(x, 2, function(i) length(unique(na.exclude(i))))
  } else {
    apply(x, 2, function(i) length(unique(i)))
  }
} # rtemis::uniquevalsperfeat


# df_movecolumn.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Move data frame column
#'
#' @param x data.frame.
#' @param from String or Integer: Define which column holds the vector you want to move.
#' @param to Integer: Define which column number you want the vector to be moved to.
#' Default = `ncol(x)` i.e. the last column.
#'
#' @return data.frame
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars_hp <- df_movecolumn(mtcars, "hp")
#' }
df_movecolumn <- function(x, from, to = ncol(x)) {
  if (!is.data.frame(x)) {
    cli::cli_abort("Input must be data frame")
  }

  if (is.character(from)) {
    from_name <- from
    from <- grep(from, colnames(x))
    if (length(from) == 0) {
      cli::cli_abort("Did not find \"", from, "\" in input")
    }
  } else {
    from_name <- colnames(x)[from]
  }

  v <- x[, from, drop = FALSE]
  x[, from] <- NULL

  if (to == ncol(x)) {
    cbind(x, v)
  } else {
    cbind(x[, 1:(to - 1), drop = FALSE], v, x[, (to + 1):ncol(x), drop = FALSE])
  }
} # rtemis::df_movecolumn


#' Vector to data.frame
#'
#' Convert vector to 1-row data.frame, maintaining names if present
#'
#' @param x Vector.
#' @param col_names Character: Name of the vector.
#'
#' @return data.frame.
#'
#' @author EDG
#' @export
vec2df <- function(x, col_names = NULL) {
  if (!is.vector(x)) {
    cli::cli_abort("Input must be a vector")
  }
  if (!is.null(col_names)) {
    names(x) <- col_names
  }
  as.data.frame(t(x))
} # rtemis::vec2df
