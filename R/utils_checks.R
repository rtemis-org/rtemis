# utils_checks.R
# ::rtemis::
# 2024- EDG rtemis.org

# clean_* functions performm checks and return clean inputs.
# check_* functions perform checks (do not return a value).

#' Check type of object
#'
#' @param x Object to check
#' @param fn Function to check against, any `is.*` function, e.g. `is.character`
#'
#' @return Logical
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' is_check("papaya", is.character) # TRUE
#' is_check(c(1, 2.5, 3.2), is.integer) # FALSE
#' is_check(iris, is.list) # TRUE
#' }
is_check <- function(x, fn) {
  if (!fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    message(red(bold(input), "is not", bold(type)))
    return(FALSE)
  }
  TRUE
} # /rtemis::is_check


#' Test type of object
#'
#' @inheritParams is_check
#'
#' @return NULL (invisibly)
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
is_test <- function(x, fn) {
  if (!is.null(x) && !fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    cli::cli_abort(bold(input), " is not ", bold(type))
  }
  invisible()
} # /rtemis::is_test


#' Check class of object
#'
#' @param x Object to check
#' @param cl Character: class to check against
#'
#' @return Logical
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' check_inherits("papaya", "character") # TRUE
#' check_inherits(c(1, 2.5, 3.2), "integer") # FALSE
#' check_inherits(iris, "list") # FALSE, compare to is_check(iris, is.list)
#' }
test_inherits <- function(x, cl) {
  if (!inherits(x, cl)) {
    input <- deparse(substitute(x))
    message(red(bold(input), "is not", bold(cl)))
    return(FALSE)
  }
  TRUE
} # /rtemis::test_inherits


#' Test class of object
#'
#' @param x Object to check.
#' @param cl Character: class to check against.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_inherits <- function(
  x,
  cl,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!inherits(x, cl)) {
    cli::cli_abort(
      "{.var {xname}} must be of class {.cls {cl}}."
    )
  }

  invisible()
} # /rtemis::check_inherits


#' Function that returns object if it is of a certain class
#'
#' @param object Object to check and return
#' @param class Character vector: class(es) to check against
#' @param allow_null Logical: if TRUE, allows NULL objects
#'
#' @return Object
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' strict("papaya", "character") # "papaya"
#' strict(c(1, 2.5, 3.2), "integer") # Error
#' strict(iris, "list") # Error
#' }
strict <- function(
  object,
  class,
  allow_null = TRUE,
  xname = deparse(substitute(object))
) {
  if (allow_null && is.null(object)) {
    return(NULL)
  }
  if (inherits(object, class)) {
    return(object)
  } else {
    cli::cli_abort(xname, " must be ", bold(class))
  }
} # /rtemis::strict

#' Clean integer input
#'
#' @details
#' The goal is to return an integer vector.
#' If the input is integer, it is returned as is.
#' If the input is numeric, it is coerced to integer only if the numeric values are integers,
#' otherwise an error is thrown.
#'
#' @param x Double or integer vector to check.
#'
#' @return Integer vector
#' @author EDG
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' clean_int(6L)
#' clean_int(3)
#' clean_int(12.1) # Error
#' clean_int(c(3, 5, 7))
#' clean_int(c(3, 5, 7.01)) # Error
#' }
clean_int <- function(x, xname = deparse(substitute(x))) {
  if (is.integer(x)) {
    return(x)
  } else if (is.numeric(x)) {
    if (all(x %% 1 == 0)) {
      return(as.integer(x))
    } else {
      cli::cli_abort("{.var {xname}} must be integer.")
    }
  } else if (is.null(x)) {
    return(NULL)
  }
  cli::cli_abort("{.var {xname}} must be integer.")
} # /rtemis::clean_int


#' Match Arguments Ignoring Case
#'
#' @param x Character: Argument to match.
#' @param choices Character vector: Choices to match against.
#'
#' @return Character: Matched argument.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' match_arg("papaya", c("AppleExtreme", "SuperBanana", "PapayaMaster"))
#' }
match_arg <- function(x, choices) {
  out <- match.arg(tolower(x), tolower(choices))
  grep(out, choices, value = TRUE, ignore.case = TRUE)
} # /rtemis::match_arg


#' Check logical
#'
#' @param x Vector to check
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_logical <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }
  if (!is.logical(x)) {
    cli::cli_abort("{.var {xname}} must be logical.")
  }

  invisible()
} # /rtemis::check_logical


#' Check character
#'
#' @param x Vector to check
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_character <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }
  if (!is.character(x)) {
    cli::cli_abort("{.var {xname}} must be character.")
  }

  invisible()
} # /rtemis::check_character


#' Check positive float
#'
#' @details
#' Checking with `is.numeric()` allows integer inputs as well, which should be ok since it is
#' unlikely the function that consumes this will enforce double type only, but instead is most
#' likely to allow implicit coercion from integer to numeric.
#'
#' @param x Float vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail, otherwise invisible().
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_floatpos <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x <= 0)) {
    cli::cli_abort("{.var {xname}} must be greater than 0.")
  }

  invisible()
} # /rtemis::check_floatpos


#' Check float between 0 and 1, exclusive
#'
#' @param x Vector to check
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' check_float01exc(0.5)
check_float01exc <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x < 0 | x > 1)) {
    cli::cli_abort(
      "{.var {xname}} must be between 0 and 1, exclusive."
    )
  }

  invisible()
} # /rtemis::check_float01


#' Check float between 0 and 1, inclusive
#'
#' @param x Float vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' check_float01inc(0.5)
check_float01inc <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.", call. = FALSE)
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x < 0 | x > 1)) {
    cli::cli_abort("{.var {xname}} must be between 0 and 1, inclusive.")
  }

  invisible()
} # /rtemis::check_float01

check_floatpos1 <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x <= 0) || any(x > 1)) {
    cli::cli_abort(
      "{.var {xname}} must be greater than 0 and less or equal to 1."
    )
  }

  invisible()
} # /rtemis::check_floatpos1


#' Check positive integer
#'
#' @param x Integer vector.
#'
#' @return x, otherwise error.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' clean_posint(5)
clean_posint <- function(x, allow_na = FALSE, xname = deparse(substitute(x))) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  } else {
    x <- na.exclude(x)
  }

  if (any(x <= 0)) {
    cli::cli_abort("{.var {xname}} must contain only positive integers.")
  }

  clean_int(x, xname = xname)
} # /rtemis::clean_posint


#' Check float greater than or equal to 0
#'
#' Checks if an input is a numeric vector containing non-negative
#'   (>= 0) values and no `NA`s. It is designed to validate function arguments.
#'
#' @param x Numeric vector: The input object to check.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_float0pos <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x < 0)) {
    cli::cli_abort("{.var {xname}} must be zero or greater.")
  }

  invisible()
} # /rtemis::check_float0pos


#' Check float -1 <= x <= 1
#'
#' @param x Numeric vector: The input object to check.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_float_neg1_1 <- function(
  x,
  allow_null = TRUE,
  xname = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {xname}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {xname}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  }

  if (any(x < -1 | x > 1)) {
    cli::cli_abort("{.var {xname}} must be between -1 and 1, inclusive.")
  }

  invisible()
} # /rtemis::check_float_neg1_1


#' Abbreviate object class name
#'
#' @param x Object
#'
#' @return Character: Abbreviated class
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
abbreviate_class <- function(x, n = 4L) {
  paste0("<", abbreviate(class(x)[1], minlength = n), ">")
} # /rtemis::abbr_class


#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbosity Integer: Verbosity level.
#' Note: An error will always printed if dependencies are missing.
#' Setting this to FALSE stops it from printing
#' "Dependencies check passed".
#'
#' @return Called for side effects. Aborts and prints list of missing dependencies, if any.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_dependencies <- function(..., verbosity = 0L) {
  ns <- as.list(c(...))
  err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))
  if (any(err)) {
    cli::cli_abort(
      paste0(
        "Please install the following ",
        ngettext(sum(err), "dependency", "dependencies"),
        ":\n",
        pastels(ns[err], bullet = "    -")
      )
    )
  } else {
    if (verbosity > 0L) msg("Dependency check passed")
  }
  invisible()
} # /rtemis::check_dependencies


#' Check data.table
#'
#' @param x Object to check.
#'
#' @return Called for side effects. Throws an error if input is not a data.table, returns x
#' invisibly otherwise.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_data.table <- function(x, xname = deparse(substitute(x))) {
  if (!data.table::is.data.table(x)) {
    cli::cli_abort("{.var {xname}} must be a data.table.")
  }
  invisible(x)
} # /rtemis::check_data.table
