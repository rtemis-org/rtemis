# utils_data.table.R
# ::rtemis::
# 2022- EDG rtemis.org

#' Number of unique values per feature
#'
#' @param x data.table
#' @param excludeNA Logical: If TRUE, exclude NA values.
#' @param limit Integer: Print up to this many features. Set to -1L to print all.
#' @param verbosity Integer: If > 0, print output to console.
#'
#' @return Named integer vector of length `NCOL(x)` with number of unique values per column/feature, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' ir <- as.data.table(iris)
#' dt_nunique_perfeat(ir)
dt_nunique_perfeat <- function(
  x,
  excludeNA = FALSE,
  limit = 20L,
  verbosity = 1L
) {
  stopifnot(inherits(x, "data.table"))
  nupf <- sapply(x, \(i) data.table::uniqueN(i, na.rm = excludeNA))
  if (verbosity > 0L) {
    printls(nupf, item_format = thin, limit = limit, print_class = FALSE)
  }
  invisible(nupf)
} # /rtemis::dt_nunique_perfeat


#' Long to wide key-value reshaping
#'
#' Reshape a long format `data.table` using key-value pairs with
#' `data.table::dcast`
#'
#' @param x `data.table` object.
#' @param id_name Character: Name of column in `x` that defines the IDs
#' identifying individual rows.
#' @param key_name Character: Name of column in `x` that holds the key.
#' @param positive Numeric or Character: Used to fill id ~ key combination
#' present in the long format input `x`.
#' @param negative Numeric or Character: Used to fill id ~ key combination
#' NOT present in the long format input `x`.
#' @param xname Character: Name of `x` to be used in messages.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `data.table` in wide format.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(
#'   ID = rep(1:3, each = 2),
#'   Dx = c("A", "C", "B", "C", "D", "A")
#' )
#' dt_keybin_reshape(x, id_name = "ID", key_name = "Dx")
dt_keybin_reshape <- function(
  x,
  id_name,
  key_name,
  positive = 1,
  negative = 0,
  xname = NULL,
  verbosity = 1L
) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  stopifnot(inherits(x, "data.table"))
  x <- copy(x)

  # Assign positive value to all in long form
  value_name <- "Bin__"
  x[, (value_name) := positive]

  .formula <- as.formula(paste(
    paste(id_name, collapse = " + "),
    "~",
    key_name
  ))
  if (verbosity > 0L) {
    msg("Reshaping", highlight(xname), "to wide format...")
    catsize(x, "Input size")
  }
  # Reshape to wide, filling all absent with negative value
  x <- dcast(
    x,
    .formula,
    fun.aggregate = length,
    value.var = value_name,
    drop = FALSE,
    fill = negative
  )

  if (verbosity > 0L) {
    catsize(x, "Output size")
  }
  x
} # /rtemis::dt_keybin_reshape


#' Merge data.tables
#'
#' @param left data.table
#' @param right data.table
#' @param on Character: Name of column to join on.
#' @param left_on Character: Name of column on left table.
#' @param right_on Character: Name of column on right table.
#' @param how Character: Type of join: "inner", "left", "right", "outer".
#' @param left_name Character: Name of left table.
#' @param right_name Character: Name of right table.
#' @param left_suffix Character: If provided, add this suffix to all left column names,
#' excluding on/left_on.
#' @param right_suffix Character: If provided, add this suffix to all right column names,
#' excluding on/right_on.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments to be passed to `data.table::merge`.
#'
#' @return Merged data.table.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' xleft <- data.table(ID = 1:5, Alpha = letters[1:5])
#' xright <- data.table(ID = c(3, 4, 5, 6), Beta = LETTERS[3:6])
#' xlr_inner <- dt_merge(xleft, xright, on = "ID", how = "inner")
dt_merge <- function(
  left,
  right,
  on = NULL,
  left_on = NULL,
  right_on = NULL,
  how = "left",
  left_name = NULL,
  right_name = NULL,
  left_suffix = NULL,
  right_suffix = NULL,
  verbosity = 1L,
  ...
) {
  if (is.null(left_name)) {
    left_name <- deparse(substitute(left))
  }
  if (is.null(right_name)) {
    right_name <- deparse(substitute(right))
  }
  if (is.null(left_on)) {
    left_on <- on
  }
  if (is.null(right_on)) {
    right_on <- on
  }
  if (verbosity > 0L) {
    icon <- switch(
      how,
      inner = "\u2A1D",
      left = "\u27D5",
      right = "\u27D6",
      "\u27D7"
    )
    if (left_on == right_on) {
      msg0(
        bold(highlight(icon)),
        " Merging ",
        highlight(left_name),
        " & ",
        highlight(right_name),
        " on ",
        highlight(left_on),
        "..."
      )
    } else {
      msg0(
        bold(highlight(icon)),
        " Merging ",
        highlight(left_name),
        " & ",
        highlight(right_name),
        " on ",
        highlight(left_on),
        " & ",
        highlight(right_on),
        "..."
      )
    }

    catsize(left, left_name)
    catsize(right, right_name)
  }

  if (how == "left") {
    all.x <- TRUE
    all.y <- FALSE
  } else if (how == "right") {
    all.x <- FALSE
    all.y <- TRUE
  } else if (how == "inner") {
    all.x <- FALSE
    all.y <- FALSE
  } else {
    all.x <- all.y <- TRUE
  }
  if (!is.null(left_suffix)) {
    left_names <- setdiff(names(left), left_on)
    setnames(left, left_names, paste0(left_names, left_suffix))
  }
  if (!is.null(right_suffix)) {
    right_names <- setdiff(names(right), right_on)
    setnames(right, right_names, paste0(right_names, right_suffix))
  }
  dat <- merge(
    left,
    right,
    by.x = left_on,
    by.y = right_on,
    all.x = all.x,
    all.y = all.y,
    ...
  )
  if (verbosity > 0L) {
    catsize(dat, "Merged")
  }
  dat
} # /rtemis::dt_merge


#' Clean factor levels of data.table in-place
#'
#' Finds all factors in a data.table and cleans factor levels to include
#' only underscore symbols
#'
#' @param x data.table
#' @param prefix_digits Character: If not NA, add this prefix to all factor levels that
#' are numbers
#'
#' @return Nothing, modifies `x` in-place.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- as.data.table(iris)
#' levels(x[["Species"]]) <- c("setosa:iris", "versicolor$iris", "virginica iris")
#' levels(x[["Species"]])
#' dt_set_cleanfactorlevels(x)
#' levels(x[["Species"]])
dt_set_cleanfactorlevels <- function(x, prefix_digits = NA) {
  stopifnot(inherits(x, "data.table"))
  idi <- names(x)[sapply(x, is.factor)]
  for (i in idi) {
    x[,
      (i) := factor(
        x[[i]],
        labels = clean_names(levels(x[[i]]), prefix_digits = prefix_digits)
      )
    ]
  }
} # /rtemis::dt_set_cleanfactorlevels


#' Get N and percent match of values between two columns of two data.tables
#'
#' @param x data.table
#' @param y data.table
#' @param on Integer or character: column to read in `x` and `y`, if it is the
#' same
#' @param left_on Integer or character: column to read in `x`
#' @param right_on Integer or character: column to read in `y`
#' @param verbosity Integer: Verbosity level.
#'
#' @return list.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(ID = 1:5, Alpha = letters[1:5])
#' y <- data.table(ID = c(3, 4, 5, 6), Beta = LETTERS[3:6])
#' dt_pctmatch(x, y, on = "ID")
dt_pctmatch <- function(
  x,
  y,
  on = NULL,
  left_on = NULL,
  right_on = NULL,
  verbosity = 1L
) {
  if (is.null(left_on)) {
    left_on <- on
  }
  if (is.null(right_on)) {
    right_on <- on
  }
  xv <- unique(x[[left_on]])
  n <- length(xv)
  yv <- unique(y[[right_on]])
  nmatch <- sum(xv %in% yv)
  matchpct <- nmatch / n * 100
  if (verbosity > 0L) {
    by_final <- paste(unique(c(left_on, right_on)), collapse = ", ")
    msg0(
      "Matched ",
      highlight(nmatch),
      "/",
      highlight(n),
      " on ",
      bold(by_final),
      " (",
      highlight(ddSci(matchpct)),
      "%)"
    )
  }
  invisible(list(nmatch = nmatch, matchpct = matchpct))
} # /rtemis::dt_pctmatch


#' Get percent of missing values from every column
#'
#' @param x data.frame or data.table
#' @param verbosity Integer: Verbosity level.
#'
#' @return list
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4), c = c("A", "B", "C", NA))
#' dt_pctmissing(x)
dt_pctmissing <- function(x, verbosity = 1L) {
  nmissing <- sapply(x, \(i) sum(is.na(i)))
  pctmissing <- nmissing / NROW(x)
  if (verbosity > 0L) {
    cat("Percent missing per column:\n")
    printls(pctmissing, print_class = FALSE)
  }
  invisible(list(nmissing = nmissing, pctmissing = pctmissing))
} # /rtemis::dt_pctmissing


#' Convert data.table logical columns to factors
#'
#' Convert data.table logical columns to factors with custom labels in-place
#'
#' @param x data.table
#' @param cols Integer or character: columns to convert, if NULL, operates on all
#' logical columns
#' @param labels Character: labels for factor levels
#' @param maintain_attributes Logical: If TRUE, maintain column attributes
#' @param fillNA Character: If not NULL, fill NA values with this constant
#'
#' @return data.table, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(a = 1:5, b = c(T, F, F, F, T))
#' x
#' dt_set_logical2factor(x)
#' x
#' z <- data.table(alpha = 1:5, beta = c(T, F, T, NA, T), gamma = c(F, F, T, F, NA))
#' z
#' # You can usee fillNA to fill NA values with a constant
#' dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"), fillNA = "No")
#' z
#' w <- data.table(mango = 1:5, banana = c(F, F, T, T, F))
#' w
#' dt_set_logical2factor(w, cols = 2, labels = c("Ugh", "Huh"))
#' w
#' # Column attributes are maintained by default:
#' z <- data.table(alpha = 1:5, beta = c(T, F, T, NA, T), gamma = c(F, F, T, F, NA))
#' for (i in seq_along(z)) setattr(z[[i]], "source", "Guava")
#' str(z)
#' dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"))
#' str(z)
dt_set_logical2factor <- function(
  x,
  cols = NULL,
  labels = c("False", "True"),
  maintain_attributes = TRUE,
  fillNA = NULL
) {
  if (is.null(cols)) {
    cols <- names(x)[sapply(x, is.logical)]
  }
  for (i in cols) {
    if (maintain_attributes) {
      .attr <- attributes(x[[i]])
    }
    x[, (i) := factor(x[[i]], levels = c(FALSE, TRUE), labels = labels)]
    if (!is.null(fillNA)) {
      x[is.na(x[[i]]), (i) := fillNA]
    }
    if (maintain_attributes) {
      for (j in seq_along(.attr)) {
        setattr(x[[i]], names(.attr)[j], .attr[[j]])
      }
    }
  }
  invisible(x)
}


#' Inspect character and factor vector
#'
#' Checks character or factor vector to determine whether it might be best to convert to
#' numeric.
#'
#' @details
#' All data can be represented as a character string. A numeric variable may be read as
#' a character variable if there are non-numeric characters in the data.
#' It is important to be able to automatically detect such variables and convert them,
#' which would mean introducing NA values.
#'
#' @param x Character or factor vector.
#' @param xname Character: Name of input vector `x`.
#' @param verbosity Integer: Verbosity level.
#' @param thresh Numeric: Threshold for determining whether to convert to numeric.
#' @param na.omit Logical: If TRUE, remove NA values before checking.
#'
#' @return Character.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- c("3", "5", "undefined", "21", "4", NA)
#' inspect_type(x)
#' z <- c("mango", "banana", "tangerine", NA)
#' inspect_type(z)
inspect_type <- function(
  x,
  xname = NULL,
  verbosity = 1L,
  thresh = .5,
  na.omit = TRUE
) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  if (na.omit) {
    x <- na.omit(x)
  }
  xclass <- class(x)[1]
  xlen <- length(x)
  raw_na <- sum(is.na(x))
  n_non_na <- xlen - raw_na
  # char_na <- sum(is.na(as.character(x)))
  suppressWarnings({
    num_na <- if (xclass == "character") {
      sum(is.na(as.numeric(x)))
    } else {
      sum(is.na(as.numeric(as.character(x))))
    }
  })
  if (raw_na == xlen) {
    "NA"
  } else if (
    xclass %in% c("character", "factor") && (num_na / n_non_na) < thresh
  ) {
    if (verbosity > 0L) {
      msg0(
        "Possible type error: ",
        highlight(xname),
        " is a ",
        bold(xclass),
        ", but perhaps should be ",
        bold("numeric"),
        "."
      )
    }
    "numeric"
  } else {
    xclass
  }
} # /rtemis::inspect_type


#' Inspect column types
#'
#' Will attempt to identify columns that should be numeric but are either character or
#' factor by running [inspect_type] on each column.
#'
#' @param x data.table
#' @param cols Character vector: columns to inspect.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(
#'   id = 8001:8006,
#'   a = c("3", "5", "undefined", "21", "4", NA),
#'   b = c("mango", "banana", "tangerine", NA, "apple", "kiwi"),
#'   c = c(1, 2, 3, 4, 5, 6)
#' )
#' dt_inspect_types(x)
dt_inspect_types <- function(x, cols = NULL, verbosity = 1L) {
  if (is.null(cols)) {
    char_factor_idi <- which(sapply(x, is.character) | sapply(x, is.factor))
    cols <- names(x[, .SD, .SDcols = char_factor_idi])
  }
  current_types <- sapply(x[, .SD, .SDcols = cols], class)
  suggested_types <- sapply(
    cols,
    \(cn) inspect_type(x[[cn]], xname = cn, verbosity = verbosity)
  )
  to_convert <- suggested_types != current_types
  names(to_convert)[to_convert]
}


#' Set column types automatically
#'
#' This function inspects a data.table and attempts to identify columns that should be
#' numeric but have been read in as character, and fixes their type in-place.
#' This can happen when one or more fields contain non-numeric characters, for example.
#'
#' @param x data.table
#' @param cols Character vector: columns to work on. If not defined, will work on all
#' columns
#' @param verbosity Integer: Verbosity level.
#'
#' @return data.table, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- data.table(
#'   id = 8001:8006,
#'   a = c("3", "5", "undefined", "21", "4", NA),
#'   b = c("mango", "banana", "tangerine", NA, "apple", "kiwi"),
#'   c = c(1, 2, 3, 4, 5, 6)
#' )
#' str(x)
#' # in-place operation means no assignment is needed
#' dt_set_autotypes(x)
#' str(x)
dt_set_autotypes <- function(x, cols = NULL, verbosity = 1L) {
  if (is.null(cols)) {
    cols <- names(x)
  }
  character_idx <- which(sapply(x, is.character))
  for (i in character_idx) {
    if (inspect_type(x[[i]], i, verbosity = 0L) == "numeric") {
      if (verbosity > 0L) {
        msg("Converting", highlight(i), "to", bold("numeric"))
      }
      # This will generate warnings if there are non-numeric values
      suppressWarnings({
        x[, (i) := as.numeric(x[[i]])]
      })
    }
  }
  invisible(x)
} # /rtemis::dt_set_autotypes


#' List column names by attribute
#'
#' @param x data.table
#' @param attribute Character: name of attribute
#' @param exact Logical: If TRUE, use exact matching
#' @param sorted Logical: If TRUE, sort the output
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
dt_names_by_attr <- function(x, attribute, exact = TRUE, sorted = TRUE) {
  attrs <- unlist(lapply(x, \(i) attr(i, attribute)))
  attrs <- sapply(x, \(i) {
    .attr <- attr(i, attribute, exact = exact)
    if (is.null(.attr)) "NA" else .attr
  })
  vals <- unique(attrs)
  if (sorted) {
    sapply(vals, \(i) sort(names(x)[attrs == i]))
  } else {
    sapply(vals, \(i) names(x)[attrs == i])
  }
} # /rtemis::dt_names_by_attr


#' Clean column names and factor levels in-place
#'
#' @param x data.table
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip
#'
#' @return Nothing, modifies `x` in-place.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' x <- as.data.table(iris)
#' levels(x[["Species"]]) <- c("setosa:iris", "versicolor$iris", "virginica iris")
#' names(x)
#' levels(x[["Species"]])
#' # in-place operation means no assignment is needed
#' dt_set_clean_all(x)
#' names(x)
#' levels(x[["Species"]])
dt_set_clean_all <- function(x, prefix_digits = NA) {
  if (!is.data.table(x)) {
    cli::cli_abort("{.arg x} must be a data.table")
  }
  data.table::setnames(x, names(x), clean_colnames(x))
  idi <- names(x)[sapply(x, is.factor)]
  for (i in idi) {
    x[,
      (i) := factor(
        x[[i]],
        labels = clean_names(levels(x[[i]]), prefix_digits = prefix_digits)
      )
    ]
  }
} # /rtemis::dt_set_clean_all


#' Describe data.table
#'
#' @param x data.table input.
#' @param verbosity Integer: If > 0, print output to console.
#'
#' @return List with three data.tables: Numeric, Categorical, and Date.
#'
#' @author EDG
#' @export
#'
#' @examples
#' library(data.table)
#' origin <- as.POSIXct("2022-01-01 00:00:00", tz = "America/Los_Angeles")
#' x <- data.table(
#'   ID = paste0("ID", 1:10),
#'   V1 = rnorm(10),
#'   V2 = rnorm(10, 20, 3),
#'   V1_datetime = as.POSIXct(
#'     seq(
#'       1, 1e7,
#'       length.out = 10
#'     ),
#'     origin = origin
#'   ),
#'   V2_datetime = as.POSIXct(
#'     seq(
#'       1, 1e7,
#'       length.out = 10
#'     ),
#'     origin = origin
#'   ),
#'   C1 = sample(c("alpha", "beta", "gamma"), 10, TRUE),
#'   F1 = factor(sample(c("delta", "epsilon", "zeta"), 10, TRUE))
#' )
dt_describe <- function(x, verbosity = 1L) {
  if (!is.data.table(x)) {
    cli::cli_abort("{.arg x} must be a data.table")
  }
  nrows <- NROW(x)

  # appease R CMD check: do not use ..var in DT frame, use with = FALSE instead

  # Numeric
  index_nm <- which(sapply(x, is.numeric))

  nm_summary <- if (length(index_nm) > 0) {
    data.frame(
      Variable = x[, index_nm, with = FALSE] |> names(),
      Min = sapply(x[, index_nm, with = FALSE], min, na.rm = TRUE),
      Max = sapply(x[, index_nm, with = FALSE], max, na.rm = TRUE),
      Median = sapply(x[, index_nm, with = FALSE], median, na.rm = TRUE),
      Mean = sapply(x[, index_nm, with = FALSE], mean, na.rm = TRUE),
      SD = sapply(x[, index_nm, with = FALSE], sd, na.rm = TRUE),
      Pct_missing = sapply(
        x[, index_nm, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.frame(
      Variable = character(),
      Min = numeric(),
      Max = numeric(),
      Median = numeric(),
      Mean = numeric(),
      SD = numeric(),
      Pct_missing = numeric()
    )
  }

  # Characters & factors
  index_cf <- c(which(sapply(x, is.character)), which(sapply(x, is.factor)))

  cf_summary <- if (length(index_cf) > 0) {
    data.frame(
      Variable = x[, index_cf, with = FALSE] |> names(),
      N_unique = sapply(
        x[, index_cf, with = FALSE],
        \(col) length(unique(col))
      ),
      Mode = sapply(x[, index_cf, with = FALSE], get_mode),
      Counts = sapply(x[, index_cf, with = FALSE], fct_describe),
      Pct_missing = sapply(
        x[, index_cf, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.frame(
      Variable = numeric(),
      N_unique = integer(),
      Mode = character(),
      Counts = character(),
      Pct_missing = numeric()
    )
  }

  # Dates
  index_dt <- which(sapply(
    x,
    \(col) any(class(col) %in% c("Date", "IDate", "POSIXct", "POSIXt"))
  ))

  dt_summary <- if (length(index_dt) > 0) {
    data.frame(
      Variable = x[, index_dt, with = FALSE] |> names(),
      Min = do.call(c, lapply(x[, index_dt, with = FALSE], min, na.rm = TRUE)),
      Max = do.call(c, lapply(x[, index_dt, with = FALSE], max, na.rm = TRUE)),
      Median = do.call(
        c,
        lapply(x[, index_dt, with = FALSE], median, na.rm = TRUE)
      ),
      Mean = do.call(
        c,
        lapply(x[, index_dt, with = FALSE], mean, na.rm = TRUE)
      ),
      Pct_missing = sapply(
        x[, index_dt, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.frame(
      Variable = character(),
      Min = numeric(),
      Max = numeric(),
      Median = numeric(),
      Mean = numeric(),
      Pct_missing = numeric()
    )
  }

  out <- list(
    Numeric = nm_summary,
    Categorical = cf_summary,
    Date = dt_summary
  )
  if (verbosity > 0L) {
    printls(out, print_df = TRUE)
  }
  invisible(out)
} # /rtemis::dt_describe
