# utils.R
# ::rtemis::
# 2016- EDG rtemis.org

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
} # rtemis::recycle


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
      new.x <- apply(x, 2, function(x) dr(x, lo, hi))
    } else {
      new.x <- dr(x, lo, hi)
    }
  } else {
    new.x <- dr(x, lo, hi)
  }

  new.x
} # rtemis::drange


#' Get Variable Name from Arguments
#'
#' Get the name of the variable passed as argument, limit number of characters in case of failure
#'
#' @param x Variable whose name you want to extract
#' @param alt Character: If name derived from `deparse(substitute(x))` exceeds `max_nchar` characters, use this name instead
#' @param max_nchar Integer: Maximum N of characters to allow for name
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
getName <- function(x, alt = "x", max_nchar = 20) {
  name <- deparse(substitute(x))

  if (nchar(name) > max_nchar) {
    name <- alt
  }

  name
} # rtemis::getName


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
#' \dontrun{
#' x <- c(9, 3, 4, 4, 0, 2, 2, NA)
#' get_mode(x)
#' x <- c(9, 3, 2, 2, 0, 4, 4, NA)
#' get_mode(x)
#' get_mode(x, getlast = FALSE)
#' }
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


#' Check for constant columns
#'
#' Checks if any column of a data frame have zero variance
#'
#' @param x Input Data Frame
#'
#' @return Logical.
#'
#' @author EDG
#' @export

any_constant <- function(x) {
  # var0 <- which(apply(x, 2, var) == 0)
  # if (length(var0) > 0) TRUE else FALSE
  constant.index <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
  if (length(constant.index) > 0) TRUE else FALSE
} # rtemis::any_constant


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
} # rtemis::is_constant


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
} # rtemis::is_discrete


#' Logit transform
#'
#' @param x Float \[0, 1\] Input
#'
#' @keywords internal
#' @noRd
logit <- function(x) {
  log(x / (1 - x))
} # rtemis::logit


#' Inverse Logit
#'
#' @param x Float: Input data
#'
#' @return The inverse logit of the input
#' @author EDG
#'
#' @keywords internal
#' @noRd
invlogit <- function(x) {
  exp(x) / (1 + exp(x))
} # rtemis::invlogit


#' Logistic function
#'
#' @param x Float: Input
#' @param x0 x-value of the midpoint.
#' @param L maximum value.
#' @param k steepness of the curve.
#'
#' @keywords internal
#' @noRd
logistic <- function(x, x0 = 0, L = 1, k = 1) {
  L / (1 + exp(-k * (x - x0)))
} # rtemis::logistic


#' ReLU - Rectified Linear Unit
#'
#' @param x Numeric: Input
#'
#' @keywords internal
#' @noRd
relu <- function(x) {
  unlist(Map(function(i) max(0, i), x))
} # rtemis::relu


#' Softplus function
#'
#' Softplus function:
#' \deqn{log(1 + e^x)}
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd
softplus <- function(x) {
  log(1 + exp(x))
} # rtemis::softplus


#' Sigmoid function
#'
#' @param x Vector, float: Input
#'
#' @keywords internal
#' @noRd
sigmoid <- function(x) 1 / (1 + exp(-x))


#' Softmax function
#'
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd
softmax <- function(x) {
  logsumexp <- function(x) {
    y <- max(x)
    y + log(sum(exp(x - y)))
  }

  exp(x - logsumexp(x))
} # rtemis::softmax


#' Square
#'
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd
square <- function(x) x^2


#' Cube
#'
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd
cube <- function(x) x^3


#' Collapse data.frame to vector by getting column max
#'
#' @param x Matrix or Data frame input
#' @param na.rm Logical: passed to `max`, If TRUE, ignore NA values,
#' otherwise if NA is present in any column, NA will be returned.
#'
#' @author EDG
#' @keywords internal
#' @noRd
colMax <- function(x, na.rm = TRUE) {
  apply(x, 2, function(i) max(i, na.rm = na.rm))
} # rtemis::colMax


#' Collapse data.frame to vector by getting row max
#'
#' @param x Input vector
#' @param na.rm Logical. If TRUE, missing values are not considered.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rowMax <- function(x, na.rm = TRUE) {
  apply(x, 1, function(i) max(i, na.rm = na.rm))
} # rtrmis::rowMax


#' Combine rules
#'
#' @param ... Character: Rules
#'
#' @return Character.
#'
#' @author EDG
#' @export
crules <- function(...) {
  rules <- c(...)
  paste0(rules, collapse = " & ")
} # rtemis::crules


#' Say No to `NULL`
#'
#' Returns the input, unless it is NULL, in which case it returns an empty vector / list, etc
#' of defined type
#'
#' This can be useful when creating S4, RC, or R6 objects
#'
#' @param x Input of any type, may be NULL
#' @param defType If `x` is NULL, return empty vector of this type. Options: list, numeric,
#' character, integer
#'
#' @author EDG
#' @keywords internal
#' @noRd
ifNotNull <- function(x, defType) {
  if (!is.null(x)) {
    return(x)
  } else {
    return(defType())
  }
} # rtemis::ifNotNull


# psd.R
# ::rtemis::
# 2016 EDG rtemis.org

#' Population Standard Deviation
#'
#' Estimate the population standard deviation:
#' \deqn{sqrt(mean(x^2) - mean(x)^2)}
#'
#' This will be particularly useful when the machines finally collect data on all humans.
#' Caution is advised, however, as you never know how many may be hiding underground.
#'
#' @param x Numeric vector
#'
#' @return Numeric.
#'
#' @author EDG
#' @export
psd <- function(x) {
  return(sqrt(mean(x^2) - mean(x)^2))
} # rtemis::psd


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
} # rtemis::rnormmat


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
} # rtemis::runifmat


#' Construct an n-length vector of letters
#'
#' Returns an n-length vector of the latin alphabet, replicating for every 26 characters
#'
#' @param n Length of vector to return
#' @param caps Logical: If TRUE, return all caps
#'
#' @keywords internal
#' @noRd
rtLetters <- function(n = 100, caps = FALSE) {
  reps <- ceiling(n / 26)
  prtlet <- function(x = NULL) paste0(x, if (caps) LETTERS else letters)
  out <- NULL
  for (i in 1:reps) {
    out.length <- length(out)
    out <- c(out, prtlet(out[(out.length - 25):out.length]))
  }
  out[1:n]
} # rtemis::rtLetters


#' Format singular/plural noun
#'
#' @keywords internal
#' @noRd
singorplu <- function(n, x) {
  switch(
    as.character(n),
    `0` = paste0("no ", x, "s"),
    `1` = paste("1", x),
    paste0(n, " ", x, "s")
  )
}

#' Round to nearest .5
#'
#' @param x numeric vector
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
roundtohalf <- function(x) {
  round(x * 2) / 2
}


#' Round to nearest fraction
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
roundtofrac <- function(x, t = .5) {
  round(x / t) * t
}


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
#' @export
catrange <- function(x, ddSci = TRUE, decimal_places = 1, na.rm = TRUE) {
  if (ddSci) {
    paste(
      ddSci(range(x, na.rm = na.rm), decimal_places = decimal_places),
      collapse = " to "
    )
  } else {
    paste(range(x, na.rm = na.rm), collapse = " to ")
  }
  invisible()
} # rtemis::catrange


#' @keywords internal
#' @noRd
null2na <- function(x) {
  if (is.null(x)) NA else x
}

#' Get rtemis and OS version info
#'
#' @keywords internal
#' @noRd
rtversion <- function() {
  out <- c(
    list(rtemis_version = as.character(packageVersion("rtemis"))),
    as.list(Sys.info())
  )
  printls(out)
  invisible(out)
} # rtemis::rtversion


#' @keywords internal
#' @noRd
popvar <- function(x) {
  mean((x - mean(x))^2)
}


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

#' @keywords internal
#' @noRd
pval_stars <- function(x) {
  cut(x, breaks = c(0, .001, .01, .05, 1), labels = c("***", "**", "*", ""))
}


#' Return object if it has length > 0
#'
#' @keywords internal
#' @noRd
iflengthy <- function(x) {
  if (length(x) > 0) x else NULL
}


#' Size of matrix or vector
#'
#' Return the size of a matrix or vector as (Nrows, Ncolumns)
#' Are you tired of getting NULL when you run dim() on a vector?
#'
#' @param x Vector or matrix input
#' @param verbosity Integer: Verbosity level. If > 0, print size to console
#'
#' @return Integer vector of length 2: c(Nrow, Ncols), invisibly
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(20)
#' size(x)
#' # 20  1
#' x <- matrix(rnorm(100), 20, 5)
#' size(x)
#' # 20  5
#' }

size <- function(x, verbosity = 1L) {
  z <- c(NROW(x), NCOL(x))
  if (verbosity > 0L) {
    # Format to add "," for thousands
    z_formatted <- format(z, trim = TRUE, big.mark = ",", scientific = FALSE)
    cat(bold(z_formatted[1]), gray("x"), bold(z_formatted[2]), "\n")
  }
  invisible(z)
} # /rtemis::size


# winsorize.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Winsorize vector
#'
#' Replace extreme values by absolute or quantile threshold
#'
#' If both lo and prob_lo or both hi and prob_hi are NULL, cut-off is set to min(x) and max(x) respectively, i.e.
#' no values are changed
#'
#' @param x Numeric vector: Input data
#' @param lo Numeric: If not NULL, replace any values in `x` lower than
#' this with this.
#' @param hi Numeric: If not NULL, replace any values in `x` higher than
#' this with this.
#' @param prob_lo Numeric (0, 1): If not NULL and `lo = NULL`, find sample
#' quantile that corresponds to this probability and set as `lo`.
#' @param prob_hi Numeric (0, 1): If not NULL and `hi = NULL`, find sample
#' quantile that corresponds to this probability and set as `hi`.
#' @param quantile_type Integer: passed to `stats::quantile`
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # Winsorize a normally distributed variable
#' x <- rnorm(500)
#' xw <- winsorize(x)
#' # Winsorize an exponentially distributed variable only on
#' # the top 5% highest values
#' x <- rexp(500)
#' xw <- winsorize(x, prob_lo = NULL, prob_hi = .95)
#' }
winsorize <- function(
  x,
  lo = NULL,
  hi = NULL,
  prob_lo = .025,
  prob_hi = .975,
  quantile_type = 7,
  verbosity = 1L
) {
  lo.cut <- if (!is.null(lo)) {
    lo
  } else if (!is.null(prob_lo)) {
    as.numeric(quantile(x, prob_lo, type = quantile_type))
  } else {
    min(x)
  }
  if (verbosity > 0L) {
    msg("Lo cut set to", lo.cut)
  }

  hi.cut <- if (!is.null(hi)) {
    hi
  } else if (!is.null(prob_hi)) {
    as.numeric(quantile(x, prob_hi, type = quantile_type))
  } else {
    max(x)
  }
  if (verbosity > 0L) {
    msg("Hi cut set to", hi.cut)
  }

  xw <- ifelse(x < lo.cut, lo.cut, x)
  xw <- ifelse(xw > hi.cut, hi.cut, xw)

  xw
} # rtemis::winsorize


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
#' \dontrun{
#' setdiff(1:10, 1:5)
#' setdiff(1:5, 1:10)
#' setdiffsym(1:10, 1:5)
#' setdiffsym(1:5, 1:10)
#' }
setdiffsym <- function(x, y) {
  union(setdiff(x, y), setdiff(y, x))
} # rtemis::setdiffsym

#' n Choose r
#'
#' Calculate number of combinations
#'
#' In plain language:
#'   You have `n` items. How many different cobinations of `r` items can you make?
#'
#' @param n Integer: Total number of items
#' @param r Integer: Number of items in each combination
#'
#' @return Integer: Number of combinations
#' @author EDG
#' @export

nCr <- function(n, r) {
  if (n < r) {
    0
  } else {
    factorial(n) / (factorial(r) * factorial(n - r))
  }
} # rtemis::nCr

#' Create permutations
#'
#' Creates all possible permutations
#'
#' n higher than 10 will take a while, or may run out of memory in systems
#' with limited RAM
#'
#' @param n Integer: Length of elements to permute
#'
#' @export
#' @return Matrix where each row is a different permutation

permute <- function(n) {
  if (n == 1) {
    matrix(1)
  } else {
    mat0 <- permute(n - 1)
    p <- nrow(mat0)
    mat1 <- matrix(nrow = n * p, ncol = n)
    for (i in seq_len(n)) {
      mat1[(i - 1) * p + seq_len(p), ] <- cbind(i, mat0 + (mat0 >= i))
    }
    mat1
  }
} # rtemis::permute


#' Geometric mean
#'
#' @param x Numeric vector
#'
#' @return Numeric.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(1, 3, 5)
#' mean(x)
#' gmean(x)
#' # same as, but a little faster than:
#' exp(mean(log(x)))
#' }
gmean <- function(x) {
  prod(x)^(1 / length(x))
} # rtemis::gmean


#' Table 1
#'
#' Build Table 1. Subject characteristics
#'
#' The output will look like "summaryFn1 (summaryFn2)".
#' Using defaults this will be "mean (sd)"
#' @param x data.frame or matrix: Input data, cases by features
#' @param summaryFn1 Function: Summary function 1.
#' @param summaryFn2 Function: Summary function 2.
#' @param summaryFn1_extraArgs List: Extra arguments for `summaryFn1`.
#' @param summaryFn2_extraArgs List: Extra arguments for `summaryFn2`.
#' @param labelify Logical: If TRUE, apply [labelify] to column names of  `x`
#' @param verbosity Integer: Verbosity level.
#' @param filename Character: Path to output CSV file to save table.
#'
#' @return
#' A data.frame, invisibly, with two columns: "Feature", "Value mean (sd) | N"
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' table1(iris)
#' }
table1 <- function(
  x,
  summaryFn1 = mean,
  summaryFn2 = sd,
  summaryFn1_extraArgs = list(na.rm = TRUE),
  summaryFn2_extraArgs = list(na.rm = TRUE),
  labelify = TRUE,
  verbosity = 1L,
  filename = NULL
) {
  if (is.null(dim(x))) {
    cli::cli_abort("Please provide a matrix or data frame")
  }
  .dim <- dim(x)
  if (verbosity > 0L) {
    msg(
      "Input:",
      highlight(.dim[1]),
      "cases with",
      highlight(.dim[2]),
      "features"
    )
  }

  .names <- colnames(x)
  if (is.null(.names)) {
    warning(
      "No column names found, please check input. Generic names will be used."
    )
    .names <- paste("Feature", seq_len(NCOL(x)))
  } else {
    if (labelify) .names <- labelify(.names)
  }

  x <- as.data.frame(x)

  # Get index for continuous and discrete features
  index.cont <- which(sapply(x, is.numeric))
  index.disc <- which(sapply(x, is.factor) | sapply(x, is.character))

  # Get summary statistics ----

  ## '- Continuous Features ----
  if (length(index.cont) > 0) {
    # .summary1_cont <- apply(x[, index.cont, drop = FALSE], 2, summaryFn1)
    .summary1_cont <- apply(x[, index.cont, drop = FALSE], 2, function(i) {
      do.call(summaryFn1, c(list(i), summaryFn1_extraArgs))
    })
    # .summary2_cont <- apply(x[, index.cont, drop = FALSE], 2, summaryFn2)
    .summary2_cont <- apply(x[, index.cont, drop = FALSE], 2, function(i) {
      do.call(summaryFn2, c(list(i), summaryFn2_extraArgs))
    })
    .summary_cont <- paste0(
      ddSci(.summary1_cont),
      " (",
      ddSci(.summary2_cont),
      ")"
    )
  } else {
    .summary_cont <- NULL
  }

  ## '- Discrete Features ----
  if (length(index.disc) > 0) {
    .summary1_disc <- lapply(index.disc, function(i) table(x[, i]))
    .summary_disc <- sapply(
      .summary1_disc,
      function(i) paste0(names(i), ": ", i, collapse = "; ")
    )
  } else {
    .summary_disc <- NULL
  }

  # Table 1 ----
  .table1 <- data.frame(
    Feature = c(.names[index.cont], .names[index.disc]),
    Value = c(.summary_cont, .summary_disc)
  )
  colnames(.table1)[2] <- "Mean (sd) | Count per group"

  if (verbosity > 0L) {
    .table1.f <- .table1
    colnames(.table1.f) <- NULL
    cat(bold("Table 1."), "Subject Characteristics\n")
    print(.table1.f, row.names = FALSE)
    cat(
      "\nAll values are displayed as ",
      deparse(substitute(summaryFn1)),
      " (",
      deparse(substitute(summaryFn2)),
      ") or Count per group\n",
      sep = ""
    )
  }

  if (!is.null(filename)) {
    # Add .csv extension if not present
    filename <- ifelse(
      grepl("\\.csv$", filename),
      filename,
      paste0(filename, ".csv")
    )
    i <- 1
    while (file.exists(filename)) {
      filename <- gsub("\\.csv$", paste0("_", i, ".csv"), filename)
      i <- i + 1
    }
    write.csv(.table1, filename, row.names = FALSE)
  }

  invisible(.table1)
} # rtemis::table1


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
#' \dontrun{
#' x <- factor(sample(letters[1:3], 100, TRUE))
#' x[sample(1:100, 10)] <- NA
#' xm <- factor_NA2missing(x)
#' }
factor_NA2missing <- function(x, na_level_name = "missing") {
  check_inherits(x, "factor")
  if (anyNA(x)) {
    x <- factor(x, levels = c(levels(x), na_level_name))
    x[is.na(x)] <- na_level_name
    x
  } else {
    x
  }
} # rtemis::factor_NA2missing


#' Sparse rnorm
#'
#' A sparse version of `stats::rnorm`
#' Outputs a vector where a fraction of values are zeros (determined by `sparseness`)
#' and the rest are drawn from a random normal distribution using `stats::rnorm`
#'
#' @param n Integer: Length of output vector
#' @param sparseness Float (0, 1): Fraction of required nonzero elements, i.e. output will have
#' `round(sparseness * n) nonzero elements`.
#' If `sparseness = 0`, a vector of zeros length `n` is returned,
#' if `sparseness = 1`, `rnorm(n, mean, sd)` is returned.
#' @param mean Float: Target mean of nonzero elements, passed to `stats::rnorm`.
#' @param sd Float: Target sd of nonzero elements, passed to `stats::rnorm`.
#'
#' @return Numeric vector of length `n`.
#'
#' @author EDG
#' @export
sparsernorm <- function(n, sparseness = 0.1, mean = 0, sd = 1) {
  if (sparseness > 0 && sparseness < 1) {
    .n <- round(sparseness * n)
    .rnorm <- rnorm(.n, mean = mean, sd = sd)
    out <- rep(0, n)
    index <- sample(n, .n)
    out[index] <- .rnorm
    out
  } else if (sparseness == 0) {
    rep(0, n)
  } else {
    rnorm(n, mean = mean, sd = sd)
  }
} # rtemis::sparsernorm


#' Get version of all loaded packages (namespaces)
#'
#' @author EDG
#' @return Data frame with columns "Package_Name" and "Version"
#' @export

get_loaded_pkg_version <- function() {
  loaded_ <- loadedNamespaces()

  data.frame(
    Package_Name = loaded_,
    Version = sapply(loaded_, function(i) as.character(packageVersion(i))),
    row.names = seq(loaded_)
  )
} # rtemis::get_loaded_pkg_version


#' FWHM to Sigma
#'
#' Convert Full width at half maximum values to sigma
#'
#' @param fwhm FWHM value
#' @return sigma
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' fwhm2sigma(8)
#' # FWHM of 8 is equivalent to sigma = 3.397287
#' }

fwhm2sigma <- function(fwhm) {
  sigma <- fwhm / (2 * sqrt(2 * log(2)))
  cat("FWHM of", fwhm, "is equivalent to sigma =", sigma, "\n")
  # return only prints if function run without assignment
  sigma
} # rtemis::fwhm2sigma

# gt_table.R
# ::rtemis::
# 2018 EDG rtemis.org

#' Greater-than Table
#'
#' Compare vectors element-wise, and tabulate N times each vector is greater than the others
#'
#' @param x List of vectors of same length.
#' @param x_name Character: Name of measure being compared.
#' @param na.rm Passed to `sum` to handle missing values.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Data frame of N times each vector is greater than the others.
#'
#' @author EDG
#' @export
gt_table <- function(x = list(), x_name = NULL, na.rm = TRUE, verbosity = 1L) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }

  if (is.null(names(x))) {
    names(x) <- paste0("x", seq_along(x))
  }
  names <- names(x)

  grid <- expand.grid(names, names, stringsAsFactors = FALSE)
  grid[["Nwin"]] <- vector("numeric", NROW(grid))
  for (i in seq_len(NROW(grid))) {
    name1 <- grid[i, 1]
    name2 <- grid[i, 2]
    grid[["Nwin"]][i] <- sum(x[[name1]] > x[[name2]], na.rm = na.rm)
  }

  dat <- as.data.frame(matrix(grid[["Nwin"]], length(x)))
  colnames(dat) <- rownames(dat) <- names

  if (verbosity > 0L) {
    .nchar <- nchar(paste("Row > than Column :", x_name))
    cat("Row > than Column :", x_name, "\n")
    cat(rep("-", .nchar), "\n", sep = "")
    print(dat)
  }

  invisible(dat)
} # /rtemis::gt_table
