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


#' Red
#'
#' @author EDG
#' @keywords internal
#' @noRd
red <- function(..., bold = FALSE, output_type = c("ansi", "html", "plain")) {
  fmt(
    paste(...),
    col = rt_red,
    bold = bold,
    output_type = output_type
  )
}


#' Green
#'
#' @author EDG
#' @keywords internal
#' @noRd
green <- function(..., bold = FALSE, output_type = c("ansi", "html", "plain")) {
  fmt(
    paste(...),
    col = rt_green,
    bold = bold,
    output_type = output_type
  )
}


#' Blue
#'
#' @author EDG
#' @keywords internal
#' @noRd
blue <- function(..., bold = FALSE, output_type = c("ansi", "html", "plain")) {
  fmt(
    paste(...),
    col = rt_blue,
    bold = bold,
    output_type = output_type
  )
}


#' Orange
#'
#' @author EDG
#' @keywords internal
#' @noRd
orange <- function(
  ...,
  bold = FALSE,
  output_type = c("ansi", "html", "plain")
) {
  fmt(
    paste(...),
    col = rt_orange,
    bold = bold,
    output_type = output_type
  )
}


#' Reset ANSI formatting
#'
#' @param ... Optional character: Text to be output to console.
#'
#' @return Character: Text with ANSI reset code prepended.
#'
#' @author EDG
#' @keywords internal
#' @noRd
reset <- function(...) {
  paste0("\033[0m", paste(...))
}


#' Get rtemis citation
#'
#' @return Character: Citation command.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rtcitation <- paste0(
  "> ",
  fmt("citation", col = rt_blue),
  "(",
  fmt("rtemis", col = rt_teal),
  ")"
)


checkmark <- function(
  col = rt_green,
  output_type = c("ansi", "html", "plain")
) {
  fmt("\u2713", col = col, bold = TRUE, output_type = output_type)
}

crossmark <- function(output_type = c("ansi", "html", "plain")) {
  fmt("\u2715", col = rt_red, bold = TRUE, output_type = output_type)
}


#' Success message
#'
#' @param ... Character: Message components.
#' @param sep Character: Separator between message components.
#' @param end Character: End character.
#' @param pad Integer: Number of spaces to pad the message with.
#'
#' @author EDG
#' @keywords internal
#' @noRd
yay <- function(..., sep = " ", end = "\n", pad = 0) {
  message(
    strrep(" ", pad),
    paste(checkmark(), ..., sep = sep),
    end,
    appendLF = FALSE
  )
} # /rtemis::yay


#' Failure message
#'
#' @param ... Character: Message components.
#' @param sep Character: Separator between message components.
#' @param end Character: End character.
#' @param pad Integer: Number of spaces to pad the message with.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nay <- function(..., sep = " ", end = "\n", pad = 0) {
  message(
    strrep(" ", pad),
    paste(crossmark(), ..., sep = sep),
    end,
    appendLF = FALSE
  )
} # /rtemis::nay


#' Format text for label printing
#'
#' @param x Character: Input
#' @param underscores_to_spaces Logical: If TRUE, convert underscores to spaces.
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
#'
#' @examples
#' x <- c("county_name", "total.cost$", "age", "weight.kg")
#' labelify(x)
labelify <- function(
  x,
  underscores_to_spaces = TRUE,
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
  if (underscores_to_spaces) {
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
  gsub("\\[\\[.*\\]\\]", "", xf)
} # /rtemis::labelify


#' Clean names
#'
#' Clean character vector by replacing all symbols and sequences of symbols with single
#' underscores, ensuring no name begins or ends with a symbol
#'
#' @param x Character vector.
#' @param sep Character: Separator to replace symbols with.
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
#' x
#' clean_names(x)
#' clean_names(x, sep = " ")
clean_names <- function(x, sep = "_", prefix_digits = "V_") {
  xc <- gsub("[^[:alnum:]]{1,}", sep, x)
  xc <- gsub(paste0("^", sep, "+|", sep, "+$"), "", xc)
  if (!is.na(prefix_digits)) {
    sn_idi <- grep("^[0-9]", xc)
    xc[sn_idi] <- paste0(prefix_digits, xc[sn_idi])
  }
  xc
} # /rtemis::clean_names


#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character vector OR any object with `colnames()` method, like matrix, data.frame,
#' data.table, tibble, etc.
#' @param lowercase Logical: If TRUE, convert to lowercase.
#' @param uppercase Logical: If TRUE, convert to uppercase.
#' @param titlecase Logical: If TRUE, convert to Title Case.
#'
#' @return Character vector with cleaned names.
#'
#' @author EDG
#' @export
#'
#' @examples
#' clean_colnames(iris, lowercase = FALSE, uppercase = FALSE, titlecase = FALSE)
clean_colnames <- function(
  x,
  lowercase = FALSE,
  uppercase = FALSE,
  titlecase = FALSE
) {
  # Check arguments: only one of lowercase, uppercase, or titlecase can be TRUE
  if (sum(c(lowercase, uppercase, titlecase)) > 1) {
    cli::cli_abort(
      "Only one of {.arg lowercase}, {.arg uppercase}, or {.arg titlecase} can be TRUE."
    )
  }
  if (!inherits(x, "character")) {
    x <- colnames(x)
  }
  if (lowercase) {
    clean_names(tolower(x))
  } else if (uppercase) {
    clean_names(toupper(x))
  } else if (titlecase) {
    gsub(" ", "_", tools::toTitleCase(clean_names(x, sep = " ")))
  } else {
    clean_names(x)
  }
} # /rtemis::clean_colnames


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


#' Oxford comma
#'
#' @param ... Character vector: Items to be combined.
#' @param format_fn Function: Any function to be applied to each item.
#'
#' @return Character: Formatted string with oxford comma.
#'
#' @author EDG
#' @keywords internal
#' @noRd
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
} # /rtemis::oxfordcomma


#' Paste with box
#'
#' @param x Character: Text to be output to console.
#' @param pad Integer: Number of spaces to pad to the left.
#'
#' @return Character: Padded string with box.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pastebox <- function(x, pad = 0) {
  paste0(strrep(" ", pad), ".:", x)
}


#' Show S7 class name
#'
#' @param x Character: S7 class name.
#' @param col Color: Color code for the object name.
#' @param pad Integer: Number of spaces to pad the message with.
#' @param prefix Character: Prefix to add to the object name.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted string that can be printed with cat().
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' repr_S7name("Supervised") |> cat()
repr_S7name <- function(
  x,
  col = col_object,
  bold = TRUE,
  underline = TRUE,
  pad = 0L,
  prefix = NULL,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)

  if (S7_inherits(x)) {
    x <- S7_class(x)@name
  }

  paste0(
    strrep(" ", pad),
    fmt("<", col = highlight_col, output_type = output_type),
    if (!is.null(prefix)) {
      fmt(
        prefix,
        col = col_object,
        bold = bold,
        underline = underline,
        output_type = output_type
      )
    },
    fmt(
      x,
      col = col,
      bold = bold,
      underline = underline,
      output_type = output_type
    ),
    fmt(">", col = highlight_col, output_type = output_type),
    "\n"
  )
} # /rtemis::repr_S7name


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
  prefix = NULL,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  out <- repr_S7name(
    x,
    col = col,
    pad = pad,
    prefix = prefix,
    output_type = output_type
  )
  cat(out)
} # /rtemis::objcat


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
} # /rtemis::padcat


#' Pad string to target length
#'
#' @param x Character: String to pad.
#' @param target Integer: Target length.
#' @param char Character: Padding character.
#'
#' @return Character: Padded string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pad_string <- function(x, target = 17L, char = " ") {
  lpad <- max(0, target - max(0, nchar(x)))
  paste0(
    strrep(char, lpad),
    x
  )
} # /rtemis::pad_string


#' Pad left string to target length and print with right string
#'
#' @return Called for side effect: prints padded left string and right string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' {
#'   msg("Hello")
#'   pcat("super", "wow")
#'   pcat(NULL, "oooo")
#' }
#' }
pcat <- function(left, right, target = 17, newline = TRUE) {
  cat(pad_string(left, target = target), right)
  if (newline) cat("\n")
}


#' Paste tables
#'
#' Collapses the contents of two tables element-wise with a separator
#' Table names are kept if same, otherwise also collapsed with separator
#'
#' @param left table: Left table.
#' @param right table: Right table.
#' @param sep Character: Separator between tables' values.
#'
#' @return table: Table with collapsed values and names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
paste_tables <- function(left, right, sep = "=>") {
  # Check inputs are tables
  if (!inherits(left, "table") || !inherits(right, "table")) {
    cli::cli_abort(c(
      "x" = "Both {.arg left} and {.arg right} must be {.cls table} objects.",
      "i" = "Got: {.cls {class(left)}} and {.cls {class(right)}}."
    ))
  }

  # Check dimensions match
  if (!identical(dim(left), dim(right))) {
    cli::cli_abort(c(
      "x" = "Tables must have matching dimensions.",
      "i" = "Got dimensions: {dim(left)} and {dim(right)}."
    ))
  }

  # Paste values element-wise
  values <- paste(as.vector(left), as.vector(right), sep = sep)

  # Handle dimnames
  left_names <- dimnames(left)
  right_names <- dimnames(right)

  if (identical(left_names, right_names)) {
    # Keep names if identical
    result_names <- left_names
  } else {
    # Paste names element-wise if different
    result_names <- mapply(
      function(l, r) {
        if (identical(l, r)) l else paste(l, r, sep = sep)
      },
      left_names,
      right_names,
      SIMPLIFY = FALSE
    )
  }

  # Create result table
  result <- array(values, dim = dim(left), dimnames = result_names)
  class(result) <- "table"
  result
} # /rtemis::paste_tables


#' Paste data frames
#'
#' Collapses the contents of two data frames element-wise with a separator
#' Column names and row names are kept if same, otherwise also collapsed with separator
#'
#' @param left data.frame: Left data frame.
#' @param right data.frame: Right data frame.
#' @param sep Character: Separator between data frames' values.
#'
#' @return data.frame: Data frame with collapsed values and names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
paste_dfs <- function(left, right, sep = "=>", decimal_places = 2L) {
  # Check inputs are data frames
  if (!inherits(left, "data.frame") || !inherits(right, "data.frame")) {
    cli::cli_abort(c(
      "x" = "Both {.arg left} and {.arg right} must be {.cls data.frame} objects.",
      "i" = "Got: {.cls {class(left)}} and {.cls {class(right)}}."
    ))
  }

  # Check dimensions match
  if (!identical(dim(left), dim(right))) {
    cli::cli_abort(c(
      "x" = "Data frames must have matching dimensions.",
      "i" = "Got dimensions: {dim(left)} and {dim(right)}."
    ))
  }

  # Paste values element-wise for each column
  result <- mapply(
    function(l, r) {
      paste(
        ddSci(l, decimal_places = decimal_places),
        ddSci(r, decimal_places = decimal_places),
        sep = sep
      )
    },
    left,
    right,
    SIMPLIFY = FALSE
  )

  # Handle column names
  left_colnames <- colnames(left)
  right_colnames <- colnames(right)

  if (identical(left_colnames, right_colnames)) {
    result_colnames <- left_colnames
  } else {
    result_colnames <- paste(left_colnames, right_colnames, sep = sep)
  }

  # Handle row names
  left_rownames <- rownames(left)
  right_rownames <- rownames(right)

  if (identical(left_rownames, right_rownames)) {
    result_rownames <- left_rownames
  } else {
    result_rownames <- paste(left_rownames, right_rownames, sep = sep)
  }

  # Create result data frame
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)
  colnames(result_df) <- result_colnames
  rownames(result_df) <- result_rownames
  result_df
} # /rtemis::paste_dfs
