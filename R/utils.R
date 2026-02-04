# utils.R
# ::rtemis::
# 2016- EDG rtemis.org

#' Print range of continuous variable
#'
#' @param x Numeric vector
#' @param ddSci Logical: If TRUE, use [ddSci] or range.
#' @param decimal_places Integer: Number of decimal place to use if `ddSci = TRUE`.
#' @param na.rm Logical: passed to `base::range`
#'
#' @return Called for its side effect of printing the range of `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
show_range <- function(x, ddSci = TRUE, decimal_places = 1, na.rm = TRUE) {
  if (ddSci) {
    paste(
      ddSci(range(x, na.rm = na.rm), decimal_places = decimal_places),
      collapse = " to "
    )
  } else {
    paste(range(x, na.rm = na.rm), collapse = " to ")
  }
} # /rtemis::catrange


#' Set Dynamic Range
#'
#' `rtemis preproc`: Adjusts the dynamic range of a vector or matrix input.
#'   By default normalizes to 0-1 range.
#'
#' @param x Numeric vector or matrix / data frame: Input
#' @param lo Target range minimum. Defaults to 0
#' @param hi Target range maximum. Defaults to 1
#' @param byCol Logical: If TRUE: if `x` is matrix, `drange` each
#' column separately
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- runif(20, -10, 10)
#' x <- drange(x)
#' }
drange <- function(x, lo = 0, hi = 1, byCol = TRUE) {
  dr <- function(x, lo, hi) {
    .min <- min(x, na.rm = TRUE)
    (x - .min) / max(x - .min, na.rm = TRUE) * (hi - lo) + lo
  }

  if (NCOL(x) > 1) {
    if (byCol) {
      apply(x, 2, function(x) dr(x, lo, hi))
    } else {
      dr(x, lo, hi)
    }
  } else {
    dr(x, lo, hi)
  }
} # /rtemis::drange


#' Factor NA to "missing" level
#'
#' Set NA values of a factor vector to a new level indicating missingness
#'
#' @param x Factor.
#' @param na_level_name Character: Name of new level to create that will be assigned to all current
#' NA values in `x`.
#'
#' @return factor.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- factor(sample(letters[1:3], 100, TRUE))
#' x[sample(1:100, 10)] <- NA
#' xm <- factor_NA2missing(x)
factor_NA2missing <- function(x, na_level_name = "missing") {
  check_inherits(x, "factor")
  if (anyNA(x)) {
    x <- factor(x, levels = c(levels(x), na_level_name))
    x[is.na(x)] <- na_level_name
    x
  } else {
    x
  }
} # /rtemis::factor_NA2missing


#' Filter order
#'
#' @param x Input vector
#' @param idl Logical vector: Index of elements to filter
#' @param decreasing Logical: If TRUE, sort in descending order
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' x <- rnorm(10)
#' x
#' x[filter_order(x, x < 0)]
#' }
filter_order <- function(x, idl, decreasing = FALSE) {
  idi <- which(idl)
  flt_ord <- order(x[idi], decreasing = decreasing)
  idi[flt_ord]
}


#' Get version of all loaded packages (namespaces)
#'
#' @author EDG
#'
#' @return Data frame with columns "Package_Name" and "Version".
#'
#' @export
#'
#' @examples
#' get_loaded_pkg_version()
get_loaded_pkg_version <- function() {
  loaded_ <- loadedNamespaces()
  data.frame(
    Package_Name = loaded_,
    Version = sapply(loaded_, function(i) as.character(packageVersion(i))),
    row.names = seq(loaded_)
  )
} # /rtemis::get_loaded_pkg_version


#' Get the mode of a factor or integer
#'
#' Returns the mode of a factor or integer
#'
#' @param x Vector, factor or integer: Input data.
#' @param na.rm Logical: If TRUE, exclude NAs (using `na.exclude(x)`).
#' @param getlast Logical: If TRUE, get the last value in case of ties.
#' @param retain_class Logical: If TRUE, output is always same class as input.
#'
#' @return The mode of `x`
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- c(9, 3, 4, 4, 0, 2, 2, NA)
#' get_mode(x)
#' x <- c(9, 3, 2, 2, 0, 4, 4, NA)
#' get_mode(x)
#' get_mode(x, getlast = FALSE)
get_mode <- function(
  x,
  na.rm = TRUE,
  getlast = TRUE,
  retain_class = TRUE
) {
  if (retain_class) {
    .class <- class(x)
  }
  if (na.rm) {
    x <- na.exclude(x)
  }
  freq <- table(x)
  if (sum(freq) > 0) {
    if (getlast) {
      .vals <- unique(x)
      out <- .vals[rev(which(.vals %in% names(freq)[which(freq == max(freq))]))[
        1
      ]]
    } else {
      out <- names(freq)[which.max(freq)]
    }
    if (length(out) == 0) out <- NA
  } else {
    out <- NA
  }

  if (retain_class) {
    if (is.factor(x)) {
      out <- factor(out, levels = levels(x))
    } else {
      class(out) <- .class
    }
  }
  out
} # /rtemis::get_mode


#' Check if vector is constant
#'
#' @param x Vector: Input
#' @param skip_missing Logical: If TRUE, skip NA values before test
#'
#' @return Logical.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rep(9, 1000000)
#' is_constant(x)
#' x[10] <- NA
#' is_constant(x)
#' is_constant(x, skip_missing = TRUE)
#' }
is_constant <- function(x, skip_missing = FALSE) {
  # all(duplicated(x)[-1L])
  if (skip_missing) {
    x <- na.exclude(x)
  }
  isTRUE(all(x == x[1]))
} # /rtemis::is_constant


#' Check if variable is discrete (factor or integer)
#'
#' @param x Input
#'
#' @return Logical.
#'
#' @author EDG
#' @export
is_discrete <- function(x) {
  is.factor(x) || is.integer(x) || is.logical(x) || is.character(x)
} # /rtemis::is_discrete


#' Return object if it has length > 0
#'
#' Returns the input object if it has length > 0, else NULL
#'
#' @param x Object
#'
#' @return `x` if `length(x) > 0`, else `NULL`
#'
#' @keywords internal
#' @noRd
iflengthy <- function(x) {
  if (length(x) > 0) x else NULL
} # /rtemis::iflengthy


#' @keywords internal
#' @noRd
pval_stars <- function(x) {
  cut(x, breaks = c(0, .001, .01, .05, 1), labels = c("***", "**", "*", ""))
}


#' Format singular/plural noun
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' singorplu(0, "cat")
#' singorplu(1, "cat")
#' singorplu(2, "cat")
#' }
singorplu <- function(n, x) {
  switch(
    as.character(n),
    `0` = paste0("no ", x, "s"),
    `1` = paste("1", x),
    paste0(n, " ", x, "s")
  )
}


#' Size of object
#'
#' Returns the size of an object
#'
#' @details
#' If `dim(x)` is NULL, returns `length(x)`.
#' @param x any object with `length()` or `dim()`.
#' @param verbosity Integer: Verbosity level. If > 0, print size to console
#'
#' @return Integer vector with length equal to the number of dimensions of `x`, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- rnorm(20)
#' size(x)
#' # 20
#' x <- matrix(rnorm(100), 20, 5)
#' size(x)
#' # 20  5

size <- function(x, verbosity = 1L) {
  z <- if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
  if (verbosity > 0L) {
    # Format to add "," for thousands
    z_formatted <- format(z, trim = TRUE, big.mark = ",", scientific = FALSE)
    cat(paste(bold(z_formatted), collapse = gray(" x ")), "\n")
  }
  invisible(z)
} # /rtemis::size


#' Recycle values of vector to match length of target
#'
#' @details
#' Used internally by many functions.
#'
#' @param x Vector to be recycled
#' @param target Object whose length defines target length
#'
#' @return Vector.
#'
#' @author EDG
#' @export
recycle <- function(x, target) {
  lenx <- length(x)
  lent <- length(target)

  if (lenx >= lent) {
    x
  } else {
    rep(x, ceiling(lent / lenx))[seq(lent)]
  }
} # /rtemis::recycle


#' Random Normal Matrix
#'
#' Create a matrix or data frame of defined dimensions, whose columns are random normal vectors
#'
#' @param nrow Integer: Number of rows.
#' @param ncol Integer: Number of columns.
#' @param mean Float: Mean.
#' @param sd Float: Standard deviation.
#' @param return_df Logical: If TRUE, return data.frame, otherwise matrix.
#' @param seed Integer: Set seed for `rnorm`.
#'
#' @return `matrix` or `data.frame`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- rnormmat(20, 5, mean = 12, sd = 6, return_df = TRUE, seed = 2026)
#' x
rnormmat <- function(
  nrow = 10,
  ncol = 10,
  mean = 0,
  sd = 1,
  return_df = FALSE,
  seed = NULL
) {
  if (length(mean) < ncol) {
    mean <- rep(mean, ncol / length(mean))
  }
  if (length(sd) < ncol) {
    sd <- rep(sd, ncol / length(sd))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  mat <- sapply(seq_len(ncol), function(j) rnorm(nrow, mean = mean, sd = sd))
  if (return_df) {
    mat <- as.data.frame(mat)
  }
  mat
} # /rtemis::rnormmat


#' Random Uniform Matrix
#'
#' Create a matrix or data frame of defined dimensions, whose columns are random uniform vectors
#'
#' @param nrow Integer: Number of rows.
#' @param ncol Integer: Number of columns.
#' @param min Float: Min.
#' @param max Float: Max.
#' @param return_df Logical: If TRUE, return data.frame, otherwise matrix.
#' @param seed Integer: Set seed for `rnorm`.
#'
#' @return `matrix` or `data.frame`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- runifmat(20, 5, min = 12, max = 18, return_df = TRUE, seed = 2026)
#' x
runifmat <- function(
  nrow = 10,
  ncol = 10,
  min = 0,
  max = 1,
  return_df = FALSE,
  seed = NULL
) {
  if (length(min) < ncol) {
    min <- rep(min, ncol / length(min))
  }
  if (length(max) < ncol) {
    max <- rep(max, ncol / length(max))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  mat <- sapply(seq_len(ncol), function(j) runif(nrow, min = min, max = max))
  if (return_df) {
    mat <- as.data.frame(mat)
  }
  mat
} # /rtemis::runifmat


#' Get rtemis version and system info
#'
#' @return List: rtemis version and system info, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' rtversion()
rtversion <- function() {
  out <- c(
    list(rtemis_version = as.character(packageVersion("rtemis"))),
    as.list(Sys.info())
  )
  printls(out, print_class = FALSE)
  invisible(out)
} # /rtemis::rtversion


#' Symmetric Set Difference
#'
#' @param x vector
#' @param y vector of same type as `x`
#'
#' @return Vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setdiff(1:10, 1:5)
#' setdiff(1:5, 1:10)
#' setdiffsym(1:10, 1:5)
#' setdiffsym(1:5, 1:10)
setdiffsym <- function(x, y) {
  union(setdiff(x, y), setdiff(y, x))
} # /rtemis::setdiffsym


#' Construct an n-length vector of letters
#'
#' Returns an n-length vector of the latin alphabet, replicating for every 26 characters
#'
#' @param n Length of vector to return
#' @param caps Logical: If TRUE, return all caps
#'
#' @keywords internal
#' @noRd
rt_letters <- function(n = 100, caps = FALSE) {
  reps <- ceiling(n / 26)
  prtlet <- function(x = NULL) paste0(x, if (caps) LETTERS else letters)
  out <- NULL
  for (i in 1:reps) {
    out_length <- length(out)
    out <- c(out, prtlet(out[(out_length - 25):out_length]))
  }
  out[1:n]
} # /rtemis::rt_letters


#' Initialize Project Directory
#'
#' Initializes Directory Structure: "R", "Data", "Results"
#'
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character: the working directory path, invisibly.
#'
#' @author EDG
#' @export

init_project_dir <- function(verbosity = 1L) {
  wd <- getwd()
  if (verbosity > 0L) {
    msg("Initializing project directory...")
  }
  if (verbosity > 0L) {
    cat("  Working in ", wd, "...\n", sep = "")
  }

  # rtInit.log ----
  # if (verbosity > 0L) cat(highlight("  Writing 'rtInit.log' file..."))
  sink("rtInit.log", append = TRUE)
  cat(".:rtemis Project Directory\n")
  cat(date(), "\n")
  cat("--------------------------\n")
  print(sessionInfo())
  sink()

  # ./R ./Data ./Results ----
  dirs <- c("R", "Data", "Results")
  for (i in dirs) {
    if (verbosity > 0L) {
      cat("  > Creating ", bold(i), " folder...", sep = "")
    }
    if (!dir.exists(i)) {
      dir.create(i)
      if (dir.exists(i)) {
        if (verbosity > 0L) cat(highlight(" Done\n"))
      } else {
        if (verbosity > 0L) cat(bold(red(" Failed")))
      }
    } else {
      if (verbosity > 0L) cat(bold(orange(" Already present\n")))
    }
  }

  if (verbosity > 0L) {
    cat(highlight("  All done\n"))
  }
  invisible(wd)
} # /rtemis::init_project_dir
