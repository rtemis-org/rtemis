# strng.R
# ::rtemis::
# 2022 EDG rtemis.org

# General hilite function output bold + any color.
hilite <- function(
  ...,
  col = highlight_col,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  if (output_type == "ansi") {
    paste0("\033[1;38;5;", col, "m", paste(...), "\033[0m")
  } else if (output_type == "html") {
    paste0(
      "<span style='color: #",
      col,
      "; font-weight: bold;'>",
      paste(...),
      "</span>"
    )
  } else {
    paste0(...)
  }
} # /rtemis::hilite


#' @param x Numeric: Input
#'
#' @keywords internal
#' @noRd
highlightbig <- function(x, output_type = c("ansi", "html", "plain")) {
  highlight(
    format(x, scientific = FALSE, big.mark = ","),
    output_type = output_type
  )
}


red <- function(..., bold = FALSE) {
  fmt(
    paste(...),
    col = rt_red,
    bold = bold
  )
}


# og green: "92m"
green <- function(..., bold = FALSE) {
  fmt(
    paste(...),
    col = rt_green,
    bold = bold
  )
}

blue <- function(..., bold = FALSE) {
  fmt(
    paste(...),
    col = rt_blue,
    bold = bold
  )
}

orange <- function(..., bold = FALSE) {
  fmt(
    paste(...),
    col = rt_orange,
    bold = bold
  )
}

reset <- function(...) {
  paste0("\033[0m", paste(...))
}

citation("rtemis")

# rtcitation <- paste0(
#   "> ",
#   col256("citation", col = "69"),
#   "(",
#   col256("rtemis", col = "177"),
#   ")"
# )

rtcitation <- paste0(
  "> ",
  fmt("citation", col = rt_blue),
  "(",
  fmt("rtemis", col = rt_magenta),
  ")"
)

yay <- function(..., sep = " ", end = "\n", pad = 0) {
  message(
    strrep(" ", pad),
    green("\u2714 "),
    paste(..., sep = sep),
    end,
    appendLF = FALSE
  )
}

nay <- function(..., sep = " ", end = "\n", pad = 0) {
  message(
    strrep(" ", pad),
    red("\u2715 "),
    paste(..., sep = sep),
    end,
    appendLF = FALSE
  )
}


# labelify.R
# ::rtemis::
# 2017 EDG rtemis.org

#' Format text for label printing
#'
#' @param x Character: Input
#' @param underscoresToSpaces Logical: If TRUE, convert underscores to spaces.
#' @param dotsToSpaces Logical: If TRUE, convert dots to spaces.
#' @param toLower Logical: If TRUE, convert to lowercase (precedes `toTitleCase`).
#' Default = FALSE (Good for getting all-caps words converted to title case, bad for abbreviations
#' you want to keep all-caps)
#' @param toTitleCase Logical: If TRUE, convert to Title Case. Default = TRUE (This does not change
#' all-caps words, set `toLower` to TRUE if desired)
#' @param capitalize_strings Character, vector: Always capitalize these strings, if present. Default = `"id"`
#' @param stringsToSpaces Character, vector: Replace these strings with spaces. Escape as needed for `gsub`.
#' Default = `"\\$"`, which formats common input of the type `data.frame$variable`
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
labelify <- function(
  x,
  underscoresToSpaces = TRUE,
  dotsToSpaces = TRUE,
  toLower = FALSE,
  toTitleCase = TRUE,
  capitalize_strings = c("id"),
  stringsToSpaces = c("\\$", "`")
) {
  if (is.null(x)) {
    return(NULL)
  }
  xf <- x
  for (i in stringsToSpaces) {
    xf <- gsub(i, " ", xf)
  }
  for (i in capitalize_strings) {
    xf <- gsub(paste0("^", i, "$"), toupper(i), xf, ignore.case = TRUE)
  }
  if (underscoresToSpaces) {
    xf <- gsub("_", " ", xf)
  }
  if (dotsToSpaces) {
    xf <- gsub("\\.", " ", xf)
  }
  if (toTitleCase) {
    xf <- tools::toTitleCase(xf)
  }
  if (toLower) {
    xf <- tolower(xf)
  }
  xf <- gsub(" {2,}", " ", xf)
  xf <- gsub(" $", "", xf)

  # Remove [[X]], where X is any length of characters or numbers
  xf <- gsub("\\[\\[.*\\]\\]", "", xf)

  return(xf)
} # rtemis::labelify


#' Clean names
#'
#' Clean character vector by replacing all symbols and sequences of symbols with single
#' underscores, ensuring no name begins or ends with a symbol
#'
#' @param x Character vector.
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
#' x
#' clean_names(x)
#' }
clean_names <- function(x, prefix_digits = "V_") {
  xc <- gsub("[^[:alnum:]]{1,}", "_", x)
  xc <- gsub("^_|_$", "", xc)
  if (!is.na(prefix_digits)) {
    sn_idi <- grep("^[0-9]", xc)
    xc[sn_idi] <- paste0(prefix_digits, xc[sn_idi])
  }
  xc
}

#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character vector or matrix with colnames or any object with `names()` method.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' clean_colnames(iris)
#' }
clean_colnames <- function(x) {
  if (!inherits(x, "character")) {
    x <- if (inherits(x, "matrix")) colnames(x) else names(x)
  }
  clean_names(x)
}


leftpad <- function(x, target_length, pad_char = " ") {
  lpad <- target_length - nchar(x)
  if (lpad > 0) {
    paste0(paste0(rep(pad_char, lpad), collapse = ""), x)
  } else {
    x
  }
}

#' Force plain text when using `message()`
#'
#' @param x Character: Text to be output to console.
#'
#' @return Character: Text with ANSI escape codes removed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
plain <- function(x) {
  paste0("\033[0m", x)
}

oxfordcomma <- function(..., format_fn = identity) {
  x <- unlist(list(...))
  if (length(x) > 2) {
    paste0(
      paste(sapply(x[-length(x)], format_fn), collapse = ", "),
      ", and ",
      format_fn(x[length(x)])
    )
  } else if (length(x) == 2) {
    paste(format_fn(x), collapse = " and ")
  } else {
    format_fn(x)
  }
} # rtemis::oxfordcomma

#' Padded cat
#'
#' @param x Character: Text to be output to console.
#' @param format_fn Function: Any function to be applied to `x`.
#' @param col Color: Any color fn.
#' @param newline_pre Logical: If TRUE, start with a new line.
#' @param newline Logical: If TRUE, end with a new (empty) line.
#' @param pad Integer: Pad message with this many spaces on the left.
#'
#' @author EDG
#' @keywords internal
#' @noRd
padcat <- function(
  x,
  format_fn = I,
  col = NULL,
  newline_pre = FALSE,
  newline = FALSE,
  pad = 2L
) {
  x <- as.character(x)
  if (!is.null(format_fn)) {
    x <- format_fn(x)
  }
  if (newline_pre) {
    cat("\n")
  }
  cat(strrep(" ", pad))
  if (!is.null(col)) {
    cat(col(x, TRUE))
  } else {
    cat(bold(x))
  }
  if (newline) {
    cat("\n")
  }
} # rtemis::padcat

pastebox <- function(x, pad = 0) {
  paste0(strrep(" ", pad), ".:", x)
}

#' Show S7 class name
#'
#' @param x Character: S7 class name.
#' @param col Color: Color code for the class name.
#' @param pad Integer: Number of spaces to pad the message with.
#' @param verbosity Integer: Verbosity level. If > 1, adds package name to the output.
#' @param output_type Character: Output type ("ansi", "html", "plain").
#'
#' @return Character: Formatted string that can be printed with cat().
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
show_S7name <- function(
  x,
  colors = c(rtemis_teal, rtemis_light_teal),
  pad = 0L,
  verbosity = 2L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  paste0(
    strrep(" ", pad),
    gray(if (verbosity > 1L) "<rt " else "<", output_type = output_type),
    # fmt(x, col = col, bold = TRUE, output_type = output_type),
    fmt_gradient(
      x,
      colors = colors,
      bold = TRUE,
      output_type = output_type
    ),
    gray(">", output_type = output_type),
    "\n"
  )
} # /rtemis::show_S7name

#' Cat object
#'
#' @param x Character: Object description
#' @param col Character: Color code for the object name
#' @param pad Integer: Number of spaces to pad the message with.
#' @param verbosity Integer: Verbosity level. If > 1, adds package name to the output.
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @return NULL: Prints the formatted object description to the console.
#'
#' @author EDG
#' @keywords internal
#' @noRd

objcat <- function(
  x,
  col = col_object,
  pad = 0L,
  verbosity = 2L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  out <- show_S7name(
    x,
    colors = col,
    pad = pad,
    verbosity = verbosity,
    output_type = output_type
  )
  cat(out)
} # rtemis::objcat

#' Function to label
#'
#' Create axis label from function definition and variable name
#'
#' @param fn Function.
#' @param varname Character: Variable name.
#'
#' @return Character: Label.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fn2label <- function(fn, varname) {
  # Get function body
  fn_body <- deparse(fn)[2]
  # Replace "x" with variable name
  sub("\\(x\\)", paste0("(", varname, ")"), fn_body)
} # /rtemis::fn2label
