# ddSci.R
# ::rtemis::
# 2015- EDG rtemis.org

#' Format Numbers for Printing
#'
#' 2 Decimal places, otherwise scientific notation
#'
#' Numbers will be formatted to 2 decimal places, unless this results in 0.00 (e.g. if input was .0032),
#' in which case they will be converted to scientific notation with 2 significant figures.
#' `ddSci` will return `0.00` if the input is exactly zero.
#' This function can be used to format numbers in plots, on the console, in logs, etc.
#'
#' @param x Vector of numbers
#' @param decimal_places Integer: Return this many decimal places.
#' @param hi Float: Threshold at or above which scientific notation is used.
#' @param as_numeric Logical: If TRUE, convert to numeric before returning.
#' This will not force all numbers to print 2 decimal places. For example:
#' 1.2035 becomes "1.20" if `as_numeric = FALSE`, but 1.2 otherwise
#' This can be helpful if you want to be able to use the output as numbers / not just for printing.
#' @return Formatted number
#' @author EDG
#' @examples
#' \dontrun{
#' x <- .34876549
#' ddSci(x)
#' # "0.35"
#' x <- .00000000457823
#' ddSci(x)
#' # "4.6e-09"
#' }
#' @export

ddSci <- function(x, decimal_places = 2, hi = 1e06, as_numeric = FALSE) {
  if (is.null(x)) {
    if (as_numeric) {
      return(NULL)
    } else {
      return("NULL")
    }
  }
  # Do not format factors, characters, or integers.
  if (is.factor(x) || is.character(x)) {
    return(as.character(x))
  }
  if (is.integer(x)) {
    return(x)
  }

  x <- as.list(unlist(x))

  x <- lapply(x, as.numeric)

  xf <- list()

  # Check for non-zero decimals
  # decs <- sum(unlist(x) %% 1, na.rm = TRUE) > 0

  for (i in seq(x)) {
    if (is.na(x[[i]])) {
      xf[[i]] <- NA
    } else {
      # if (decs & x[[i]] == 0) { # x[[i]] is zero but others have decimals
      if (x[[i]] == 0) {
        # always give requested decimal places
        xf[[i]] <- format(0, nsmall = decimal_places)
      } else {
        if (abs(x[[i]]) >= hi) {
          xf[[i]] <- format(
            round(x[[i]], decimal_places),
            scientific = TRUE,
            digits = decimal_places,
            nsmall = decimal_places
          )
        } else {
          # if (decs) {
          #   xf[[i]] <- ifelse(round(x[[i]], 2) != 0,
          #     format(round(x[[i]], decimal_places), nsmall = decimal_places),
          #     format(x[[i]], scientific = TRUE, digits = 2)
          #   )
          # } else {
          #   xf[[i]] <- as.character(x[[i]])
          # }
          xf[[i]] <- ifelse(
            round(x[[i]], 2) != 0,
            format(round(x[[i]], decimal_places), nsmall = decimal_places),
            format(x[[i]], scientific = TRUE, digits = 2)
          )
        }
      }
    }
  }
  xf <- as.character(xf)
  if (as_numeric) {
    xf <- as.numeric(xf)
  }
  xf
} # /rtemis::ddSci
