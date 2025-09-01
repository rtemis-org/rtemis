# print_ops.R
# ::rtemis::
# 2016-23 EDG rtemis.org

is_common_struct <- function(x) {
  class(x)[1] %in%
    c(
      "numeric",
      "integer",
      "character",
      "logical",
      "factor",
      "Date",
      "POSIXct",
      "POSIXlt",
      "list",
      "data.frame",
      "matrix",
      "array",
      "table",
      "ts",
      "tbl_df",
      "data.table"
    )
}

#' Pretty print list
#'
#' Pretty print a list (or data frame) recursively
#'
#' Data frames in R began life as lists
#'
#' @param x list or object that will be converted to a list.
#' @param prefix Character: Optional prefix for names.
#' @param pad Integer: Pad output with this many spaces.
#' @param center_title Logical: If TRUE, autopad title for centering, if present.
#' @param format_fn Formatting function.
#' @param print_class Logical: If TRUE, print abbreviated class of object.
#' @param abbrev_class_n Integer: Number of characters to abbreviate class names to.
#' @param print_df Logical: If TRUE, print data frame contents, otherwise print n rows and columns.
#' @param print_S4 Logical: If TRUE, print S4 object contents, otherwise print class name.
#' @param output_type Character: One of "console", "ansi", "html" for output formatting.
#'
#' @author EDG
#' @keywords internal
#' @noRd

printls <- function(
    x,
    prefix = "",
    pad = 2L,
    item_format = bold,
    maxlength = 4L,
    center_title = TRUE,
    title = NULL,
    title_newline = TRUE,
    newline_pre = FALSE,
    format_fn_rhs = ddSci,
    print_class = TRUE,
    abbrev_class_n = 3L,
    print_df = FALSE,
    print_S4 = FALSE,
    limit = 12L) {
  # Arguments ----
  if (newline_pre) {
    cat("\n")
  }
  if (is.null(x)) {
    if (!is.null(title)) {
      padcat(title, pad = pad, newline = title_newline, newline_pre = FALSE)
    }
    cat(strrep(" ", pad), "NULL", sep = "")
  } else if (length(x) == 0) {
    cat(class(x), "of length 0.\n")
  } else if (is.data.frame(x) && !print_df) {
    cat(
      "data.frame with",
      NROW(x),
      "rows and",
      NCOL(x),
      "columns.\n"
    )
  } else if (!is_common_struct(x)) {
    cat("object of class:", class(x), "\n")
  } else {
    x <- as.list(x)
    # Get class of each element
    classes_ <- sapply(x, class)
    # Remove closures that will cause error
    is_fn <- which(sapply(x, is.function))
    if (length(is_fn) > 0) {
      for (i in is_fn) {
        x[[i]] <- paste0(as.character(head(deparse(x[[i]]), n = 1L)), "...")
      }
    }
    # Remove NULLs
    null_index <- sapply(x, is.null)
    x[null_index] <- "NULL"
    xnames <- names(x)
    lhs <- max(nchar(paste0(prefix, xnames))) + pad
    if (!is.null(title)) {
      title.pad <- if (center_title) {
        max(0, lhs - round((.5 * nchar(title))) - 3)
      } else {
        0
      }
      padcat(
        title,
        pad = title.pad,
        newline = title_newline,
        newline_pre = FALSE
      )
    } # /title
    counter <- 0L
    # Print each item up to limit items
    if (limit != -1L && length(x) > limit) {
      padcat(
        italic(thin(
          paste0(
            "Showing first",
            limit,
            "of",
            length(x),
            "items.\n"
          )
        )),
        pad = pad
      )
    }
    for (i in seq_along(x)) {
      counter <- counter + 1L
      if (limit != -1L && counter > limit) {
        padcat(
          italic(thin(
            paste0(
              "...",
              length(x) - limit,
              "more items not shown.\n"
            )
          )),
          pad = pad
        )
        break
      }
      # Print item
      if (is.list(x[[i]])) {
        if (length(x[[i]]) == 0) {
          cat(paste0(
            item_format(format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            )),
            ": ",
            format_fn_rhs("(empty list)"),
            "\n"
          ))
        } else {
          cat(
            paste0(
              item_format(format(
                paste0(prefix, xnames[i]),
                width = lhs,
                justify = "right"
              )),
              ": "
            )
          )
          if (is_common_struct(x[[i]])) {
            printls(x[[i]], pad = lhs + 2, newline_pre = TRUE)
          } else {
            cat(
              italic("object of class:", class(x[[i]])),
              "\n"
            )
          }
        }
      } else if (is.logical(x[[i]])) {
        cat(paste0(
          item_format(format(
            paste0(prefix, xnames[i]),
            width = lhs,
            justify = "right"
          )),
          ": ",
          if (print_class) {
            gray(paste0("<", abbreviate("logical", abbrev_class_n), "> "))
          },
          ifelse(isTRUE(x[[i]]), "TRUE", "FALSE"),
          "\n"
        ))
      } else if (S7_inherits(x[[i]])) {
        cat(
          paste0(
            item_format(format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            )),
            ": "
          ),
          "\n"
        )
        # Print S7 object
        print(x[[i]], pad = lhs + 2)
      } else if (is.data.frame(x[[i]])) {
        cat(paste0(
          item_format(format(
            paste0(prefix, xnames[i]),
            width = lhs,
            justify = "right"
          )),
          ": ",
          if (print_class) {
            gray(paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "))
          },
          headdot(x[[i]], maxlength = maxlength, format_fn = format_fn_rhs),
          "\n"
        ))
      } else if (isS4(x[[i]])) {
        cat(paste0(
          item_format(format(
            paste0(prefix, xnames[i]),
            width = lhs,
            justify = "right"
          )),
          ": "
        ))
        # Print S4 object
        if (print_S4) {
          cat("\n")
          print(x[[i]])
        } else {
          cat("(S4 object of class: '", class(x[[i]]), "')\n", sep = "")
        }
      } else if (!is_common_struct(x[[i]])) {
        cat(paste0(
          item_format(format(
            paste0(prefix, xnames[i]),
            width = lhs,
            justify = "right"
          )),
          ": ",
          if (print_class) {
            gray(paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "))
          },
          italic("object of class:", class(x[[i]])),
          "\n"
        ))
      } else {
        cat(paste0(
          item_format(format(
            paste0(prefix, xnames[i]),
            width = lhs,
            justify = "right"
          )),
          ": ",
          if (print_class) {
            gray(paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "))
          },
          headdot(x[[i]], maxlength = maxlength, format_fn = format_fn_rhs),
          "\n"
        ))
      }
    }
  }
} # /rtemis::printls

# printdf1
# ::rtemis::
# 2016 rtemis.org
#' Print 1 x N data frame
#'
#' Pretty print a data frame containing 1 row of data with named columns as a vertical list
#'   of "       name : value"
#'      " other.name : other.value"
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces.
#'
#' @author EDG
#' @keywords internal
#' @noRd

printdf1 <- function(x, pad = 2) {
  x <- as.data.frame(x)
  # df <- data.frame(Parameter = c(names(x)), Value = unlist(x), row.names = NULL)

  xnames <- colnames(x)
  lhs <- max(nchar(xnames)) + pad

  for (i in seq_len(ncol(x))) {
    cat(
      paste(format(xnames[i], width = lhs, justify = "right"), ":", x[1, i]),
      "\n"
    )
  }
} # /rtemis::printdf1


cpad <- function(x, length = NULL, adjust = c("right", "left")) {
  adjust <- match.arg(adjust)
  if (is.null(length)) {
    cat(x)
  } else {
    reps <- max(0, length - nchar(x))
    if (adjust == "right") {
      paste0(strrep(" ", reps), x)
    } else {
      paste0(x, strrep(" ", reps))
    }
  }
} # /rtemis::cpad


#' Print data frame
#'
#' Pretty print a data frame
#'
#' By design, numbers will not be justified, but using ddSci_dp will convert to characters,
#' which will be justified. This is intentional for internal use.
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces.
#' @param spacing Integer: Number of spaces between columns.
#' @param ddSci_dp Integer: Number of decimal places to print using [ddSci]. Default = NULL for no
#' formatting
#' @param transpose Logical: If TRUE, transpose `x` before printing.
#' @param justify Character: "right", "left".
#' @param colnames Logical: If TRUE, print column names.
#' @param rownames Logical: If TRUE, print row names.
#' @param column_fmt Color fn for printing column names.
#' @param row_col Color fn for printing row names.
#' @param newline_pre Logical: If TRUE, print a new line before printing data frame.
#' @param newline Logical: If TRUE, print a new line after printing data frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd

printdf <- function(
    x,
    pad = 0,
    spacing = 1,
    ddSci_dp = NULL,
    transpose = FALSE,
    justify = "right",
    colnames = TRUE,
    rownames = TRUE,
    column_fmt = highlight,
    row_col = gray,
    newline_pre = FALSE,
    newline = FALSE) {
  if (transpose) {
    x <- as.data.frame(t(x))
  }
  xnames <- colnames(x)
  xrownames <- gsub(pattern = "\\.", replacement = " ", rownames(x))
  if (!is.null(ddSci_dp)) {
    xf <- as.data.frame(matrix(ddSci(x, decimal_places = ddSci_dp), NROW(x)))
    colnames(xf) <- xnames
    rownames(xf) <- xrownames
    x <- xf
  }

  col_char <- sapply(seq_along(xnames), \(i) {
    max(nchar(as.character(x[, i])), nchar(xnames[i]))
  })

  xrownames_spacing <- if (rownames) max(nchar(xrownames)) + pad else pad
  spacer <- strrep(" ", spacing)
  if (newline_pre) {
    cat("\n")
  }
  if (colnames) {
    cat(strrep(" ", xrownames_spacing))
    if (justify == "left") {
      cat(spacer)
    }
    for (i in seq_len(NCOL(x))) {
      cat(column_fmt(format(
        xnames[i],
        width = col_char[i] + spacing,
        justify = justify
      )))
    }
    cat("\n")
  }

  # cat(gray(" ]]\n"))
  if (rownames) {
    for (i in seq_len(NROW(x))) {
      # cat(row_col(cpad(xrownames[i], xrownames_spacing)))
      cat(row_col(format(
        xrownames[i],
        width = xrownames_spacing,
        justify = "right"
      )))
      for (j in seq_len(NCOL(x))) {
        cat(
          spacer,
          paste(format(x[i, j], width = col_char[j], justify = justify)),
          sep = ""
        )
      }
      cat("\n")
    }
  } else {
    for (i in seq_len(NROW(x))) {
      for (j in seq_len(NCOL(x))) {
        cat(
          spacer,
          paste(format(x[i, j], width = col_char[j], justify = justify)),
          sep = ""
        )
      }
      cat("\n")
    }
  }
  if (newline) cat("\n")
} # /rtemis::printdf

#' Show data.frame
#'
#' Create a pretty text representation of a data.frame.
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces.
#' @param spacing Integer: Number of spaces between columns.
#' @param ddSci_dp Integer: Number of decimal places to print using [ddSci]. Default = NULL for no
#' formatting
#' @param transpose Logical: If TRUE, transpose `x` before printing.
#' @param justify Character: "right", "left".
#' @param colnames Logical: If TRUE, print column names.
#' @param rownames Logical: If TRUE, print row names.
#' @param colnames_formatter Format function for printing column names.
#' @param rownames_formatter Format function for printing row names.
#' @param newline_pre Logical: If TRUE, print a new line before printing data frame.
#' @param newline Logical: If TRUE, print a new line after printing data frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd
show_df <- function(
    x,
    pad = 0L,
    spacing = 1L,
    ddSci_dp = NULL,
    transpose = FALSE,
    justify = "right",
    incl_colnames = TRUE,
    incl_rownames = TRUE,
    colnames_formatter = highlight,
    rownames_formatter = gray,
    output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  if (transpose) {
    x <- as.data.frame(t(x))
  }
  xnames <- colnames(x)
  xrownames <- gsub(pattern = "\\.", replacement = " ", rownames(x))
  if (!is.null(ddSci_dp)) {
    xf <- as.data.frame(matrix(ddSci(x, decimal_places = ddSci_dp), NROW(x)))
    colnames(xf) <- xnames
    rownames(xf) <- xrownames
    x <- xf
  }

  col_char <- sapply(seq_along(xnames), \(i) {
    max(nchar(as.character(x[, i])), nchar(xnames[i]))
  })

  xrownames_spacing <- if (incl_rownames) {
    max(nchar(xrownames)) + pad
  } else {
    pad
  }
  spacer <- strrep(" ", spacing)

  out <- character()
  if (incl_colnames) {
    out <- paste0(
      out,
      strrep(" ", xrownames_spacing)
    )
    if (justify == "left") {
      out <- paste0(out, spacer)
    }
    for (i in seq_len(NCOL(x))) {
      out <- paste0(
        out,
        colnames_formatter(
          format(
            xnames[i],
            width = col_char[i] + spacing,
            justify = justify
          ),
          output_type = output_type
        )
      )
    }
    out <- paste0(out, "\n")
  }

  # Row names
  if (incl_rownames) {
    for (i in seq_len(NROW(x))) {
      # cat(row_col(cpad(xrownames[i], xrownames_spacing)))
      out <- paste0(
        out,
        rownames_formatter(
          format(
            xrownames[i],
            width = xrownames_spacing,
            justify = "right"
          ),
          output_type = output_type
        )
      )
      for (j in seq_len(NCOL(x))) {
        out <- paste0(
          out,
          spacer,
          paste(format(x[i, j], width = col_char[j], justify = justify))
        )
      }
      out <- paste0(out, "\n")
    }
  } else {
    for (i in seq_len(NROW(x))) {
      for (j in seq_len(NCOL(x))) {
        out <- paste0(
          out,
          spacer,
          paste(format(x[i, j], width = col_char[j], justify = justify))
        )
      }
      out <- paste0(out, "\n")
    }
  }
  out
} # /rtemis::show_df


#' Show table
#'
#' @param x table.
#' @param spacing Integer: Number of spaces between columns.
#' @param pad Integer: Pad output with this many spaces.
#'
#' @return Character: formatted string.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
show_table <- function(
    x,
    spacing = 2L,
    pad = 2L,
    formatter = highlight,
    output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  dim_names <- names(attr(x, "dimnames"))
  class_names <- attr(x, "dimnames")[["Reference"]]
  n_classes <- NCOL(x)
  mat <- matrix(c(x), NROW(x))
  colnames(mat) <- colnames(x)
  rownames(mat) <- rownames(x)
  # Column width without spacing
  col.width <- sapply(seq_along(class_names), \(i) {
    max(nchar(as.character(x[, i])), nchar(class_names[i]))
  })
  lhspad <- max(nchar(class_names), nchar(dim_names[1])) + spacing + pad
  # Top dimname
  formatted_dimname <- format(
    dim_names[2],
    width = lhspad + nchar(dim_names[2]),
    justify = "right"
  )
  out <- paste0(
    bold(formatted_dimname, output_type = output_type),
    "\n"
  )
  # Left dimname
  formatted_dimname1 <- format(
    dim_names[1],
    width = lhspad - spacing,
    justify = "right"
  )
  out <- paste0(
    out,
    bold(formatted_dimname1, output_type = output_type),
    strrep(" ", spacing)
  )

  # Column names
  # (Continue on same row as left dimname)
  for (i in seq_len(n_classes)) {
    formatted_classname <- format(
      class_names[i],
      width = col.width[i] + spacing,
      justify = "left"
    )
    out <- paste0(
      out,
      formatter(formatted_classname, output_type = output_type)
    )
  }
  # Add Confusion matrix excluding colnames that are already added
  out <- paste0(
    out,
    "\n",
    show_df(
      mat,
      pad = lhspad - max(nchar(class_names)) - spacing,
      incl_colnames = FALSE,
      spacing = spacing,
      colnames_formatter = formatter,
      rownames_formatter = formatter,
      output_type = output_type
    )
  )
  out
} # /rtemis::show_table

#' @keywords internal
#' @noRd
pastels <- function(x, bullet = "  -") {
  paste(paste(bullet, x, collapse = "\n"), "\n")
} # /rtemis::pastels


#' Get first few elements of a vector with ellipsis
#'
#' @details
#' Used, for example, by `show_ls`
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
headdot <- function(x, maxlength = 6L, format_fn = identity) {
  if (maxlength == -1L || length(x) < maxlength) {
    paste(format_fn(x), collapse = ", ")
  } else {
    paste0(
      paste(format_fn(head(as.vector(x), n = maxlength)), collapse = ", "),
      "..."
    )
  }
} # /rtemis::headdot

# twocol2html.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Create html table from a data.frame with 2 columns: Var name, and Coefs
#'
#' @examples
#' \dontrun{
#' x <- data.frame(
#'   ID = c("Int", paste0("V", 1:10)),
#'   Coef = rnorm(11)
#' )
#' twocol2html(x)
#' }
#' @keywords internal
#' @noRd
twocol2html <- function(
    x,
    font_family = "'Lato'",
    font_col = "#ffffff",
    font_size = "18px",
    header_bg = "#404040",
    table_bg = "#7F7F7F",
    dat_col = rep("#525252", NROW(x)), # get color grad using all tables
    dat_font_col = "#ffffff",
    height = "50px",
    # header
    head_padding = "5px",
    # table
    dat_padding = "5px") {
  # 1. table style ----
  tablestyle <- paste0(
    '<table style="font-family: ',
    font_family,
    ", sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color:",
    font_col,
    "; font-size: ",
    font_size,
    "; padding: 0px; text-align: right; background-color: ",
    table_bg,
    "; width: auto; border-top-style: none; border-bottom-style: none; overflow-y: scroll; height: ",
    height,
    '; display: box">'
  )

  # 2. header row ----
  header <- paste0(
    '<tr><th style="font-weight: bold; padding: ',
    head_padding,
    "; text-align: center;",
    "background-color: ",
    header_bg,
    '">',
    colnames(x)[1],
    '</th>
    <th style="font-weight: bold; padding:',
    head_padding,
    "; text-align: center;",
    "background-color: ",
    header_bg,
    '">',
    colnames(x)[2],
    "</th></tr>"
  )

  # 3. Data rows ----
  tab <- vector("character", NROW(x))
  for (i in seq_along(tab)) {
    # first column: variable name; second column: coefficient
    tab[i] <- paste0(
      "<tr><td>",
      x[i, 1],
      '</td><td style="color: ',
      dat_font_col,
      "; font-variant-numeric: tabular-nums; background-color: ",
      dat_col[i],
      "; padding: ",
      dat_padding,
      '">',
      ddSci(x[i, 2], 3),
      "</td></tr>"
    )
  }

  # '- convert minus to &minus;
  tab <- gsub(">-", ">&minus;", tab)
  tab <- paste(tab, collapse = "")

  # Combine
  paste(tablestyle, header, tab, "</table>", collapse = "")
} # /rtemis::twocol2html


#' Print Size
#'
#' Get `NCOL(x)` and \code{NROW{x}}
#'
#' @param x R object (usually that inherits from matrix or data.frame)
#' @param name Character: Name of input object
#' @param verbosity Integer: Verbosity level.
#' @param newline Logical: If TRUE, end with new line character.
#'
#' @return vector of NROW, NCOL invisibly
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' catsize(iris)
#' }
catsize <- function(x, name = NULL, verbosity = 1L, newline = TRUE) {
  if (inherits(x, c("matrix", "data.frame"))) {
    .nrow <- NROW(x)
    .ncol <- NCOL(x)
    nrows <- format(.nrow, big.mark = ",")
    ncols <- format(.ncol, big.mark = ",")
    if (verbosity > 0L) {
      pcat(
        name,
        paste(highlight(nrows), "x", highlight(ncols)),
        newline = newline
      )
    }
    invisible(c(.nrow, .ncol))
  } else {
    .nels <- length(x)
    nels <- format(.nels, big.mark = ",")
    if (verbosity > 0L) {
      cat(
        # "There",
        # ngettext(.nels, "is", "are"),
        name,
        highlight(nels),
        # ngettext(.nels, "element", "elements"),
        if (newline) "\n"
      )
    }
    invisible(.nels)
  }
} # /rtemis::catsize


#' Print single line of object info
#'
#' @param x object to print
#' @param ... Not used.
#'
#' @author EDG
#' @keywords internal
#' @noRd

print1 <- function(x, ...) {
  UseMethod("print1")
} # /rtemis::print1

#' @author EDG
#' @keywords internal
#' @noRd
print1.default <- function(x, ...) {
  head(x, 1)
} # /rtemis::print1.default


#' @author EDG
#' @keywords internal
#' @noRd
list2text <- function(x, sep = ": ", line = "\n") {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(.names[i], sep, x[[i]], line)
  }) |>
    paste0(collapse = "")
} # /rtemis::list2text


#' @author EDG
#' @keywords internal
#' @noRd
list2html <- function(
    x,
    sep = ": ",
    col = "#16A0AC",
    key_weight = 100,
    value_weight = 300,
    line = "<br>") {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(
      span(.names[i], style = paste0("font-weight:", key_weight, ";")),
      sep,
      span(
        x[[i]],
        style = paste0("color:", col, "; font-weight:", value_weight, ";")
      ),
      line
    )
  }) |>
    paste0(collapse = "") |>
    htmltools::HTML()
} # /rtemis::list2html


#' @author EDG
#' @keywords internal
#' @noRd
printchar <- function(x, left_pad = 2) {
  target_length <- left_pad + max(nchar(x))
  for (i in x) {
    cat(
      highlight(leftpad(i, target_length)),
      "\n"
    )
  }
} # /rtemis::printchar


# Helper function to build padded string equivalent of padcat
show_padded <- function(
    text,
    pad = 2L,
    newline_pre = FALSE,
    newline = FALSE) {
  result <- ""
  if (newline_pre) {
    result <- paste0(result, "\n")
  }
  result <- paste0(result, strrep(" ", pad))
  result <- paste0(result, text)
  if (newline) {
    result <- paste0(result, "\n")
  }
  result
}


#' Show list as formatted string
#'
#' Works exactly like printls, but instead of printing to console with cat,
#' it outputs a single string, formatted using mformat, so that cat(show_ls(x))
#' looks identical to printls(x) for any list x
#'
#' @param x list or object that will be converted to a list.
#' @param prefix Character: Optional prefix for names.
#' @param pad Integer: Pad output with this many spaces.
#' @param item_format Formatting function for items.
#' @param maxlength Integer: Maximum length of items to show using `headdot()` before truncating with ellipsis.
#' @param center_title Logical: If TRUE, autopad title for centering, if present.
#' @param title Character: Title to print before list.
#' @param title_newline Logical: If TRUE, print title on new line.
#' @param newline_pre Logical: If TRUE, print newline before list.
#' @param format_fn_rhs Formatting function for right-hand side of items.
#' @param print_class Logical: If TRUE, print abbreviated class of object.
#' @param abbrev_class_n Integer: Number of characters to abbreviate class names to.
#' @param print_df Logical: If TRUE, print data frame contents, otherwise print n rows and columns.
#' @param print_S4 Logical: If TRUE, print S4 object contents, otherwise print class name.
#' @param limit Integer: Maximum number of items to show.
#' @param output_type Character: Output type for mformat ("ansi", "html", "plain").
#'
#' @return Character: Formatted string that can be printed with cat()
#'
#' @details
#' Exported as internal function for use by other rtemis packages.
#'
#' @author EDG
#'
#' @keywords internal
#' @export

show_ls <- function(
    x,
    prefix = "",
    pad = 2L,
    item_format = bold,
    maxlength = 4L,
    center_title = TRUE,
    title = NULL,
    title_newline = TRUE,
    newline_pre = FALSE,
    format_fn_rhs = ddSci,
    print_class = TRUE,
    abbrev_class_n = 3L,
    print_df = FALSE,
    print_S4 = FALSE,
    limit = 12L,
    output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  # Initialize output string
  result <- ""

  # Arguments ----
  if (newline_pre) {
    result <- paste0(result, "\n")
  }

  if (is.null(x)) {
    if (!is.null(title)) {
      result <- paste0(
        result,
        show_padded(title, pad = pad, newline = title_newline)
      )
    }
    result <- paste0(result, strrep(" ", pad), "NULL")
  } else if (length(x) == 0) {
    result <- paste0(result, class(x)[1], " of length 0.\n")
  } else if (is.data.frame(x) && !print_df) {
    result <- paste0(
      result,
      show_padded("(data.frame with ", pad = pad),
      NROW(x),
      " rows and ",
      NCOL(x),
      " columns.)\n"
    )
  } else if (!is_common_struct(x)) {
    result <- paste0(
      result,
      "object of class: ",
      paste(class(x), collapse = ", "),
      "\n"
    )
  } else {
    x <- as.list(x)
    # Get class of each element
    classes_ <- sapply(x, function(el) class(el)[[1L]])
    # Deparse closures that would cause error
    is_fn <- which(sapply(x, is.function))
    if (length(is_fn) > 0) {
      for (i in is_fn) {
        x[[i]] <- paste0(as.character(head(deparse(x[[i]]), n = 1L)), "...")
      }
    }
    # Set NULLs to "NULL"
    null_index <- sapply(x, is.null)
    x[null_index] <- "NULL"
    xnames <- names(x)
    lhs <- max(nchar(paste0(prefix, xnames))) + pad

    if (!is.null(title)) {
      title.pad <- if (center_title) {
        max(0, lhs - round((.5 * nchar(title))) - 3)
      } else {
        0
      }
      result <- paste0(
        result,
        show_padded(title, pad = title.pad, newline = title_newline)
      )
    } # /title

    counter <- 0L
    # Print each item up to limit items
    if (limit != -1L && length(x) > limit) {
      limit_text <- paste0(
        italic(
          thin(
            paste0(
              "Showing first ",
              limit,
              " of ",
              length(x),
              " items.\n"
            ),
            output_type = output_type
          ),
          output_type = output_type
        )
      )
      result <- paste0(result, show_padded(limit_text, pad = pad))
    }

    for (i in seq_along(x)) {
      counter <- counter + 1L
      if (limit != -1L && counter > limit) {
        more_text <- paste0(
          italic(
            thin(
              paste0(
                "...",
                length(x) - limit,
                " more items not shown.\n"
              )
            ),
            output_type = output_type
          )
        )
        result <- paste0(result, show_padded(more_text, pad = pad))
        break
      }

      # Print item
      if (is.list(x[[i]])) {
        if (length(x[[i]]) == 0) {
          item_text <- paste0(
            item_format(
              format(
                paste0(prefix, xnames[i]),
                width = lhs,
                justify = "right"
              ),
              output_type = output_type
            ),
            ": ",
            format_fn_rhs("(empty list)"),
            "\n"
          )
          result <- paste0(result, item_text)
        } else {
          item_text <- paste0(
            item_format(
              format(
                paste0(prefix, xnames[i]),
                width = lhs,
                justify = "right"
              ),
              output_type = output_type
            ),
            ": "
          )
          result <- paste0(result, item_text)

          if (is_common_struct(x[[i]])) {
            sub_result <- show_ls(
              x[[i]],
              pad = lhs + 2,
              item_format = item_format,
              newline_pre = TRUE, # important
              format_fn_rhs = format_fn_rhs,
              print_class = print_class,
              limit = limit,
              output_type = output_type
            )
            result <- paste0(result, sub_result)
          } else {
            result <- paste0(
              result,
              italic(
                paste(
                  "object of class:",
                  paste(class(x[[i]]), collapse = ", ")
                ),
                output_type = output_type
              ),
              "\n"
            )
          }
        }
      } else if (is.logical(x[[i]])) {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ": ",
          if (print_class) {
            gray(
              paste0("<", abbreviate("logical", abbrev_class_n), "> "),
              output_type = output_type
            )
          } else {
            ""
          },
          ifelse(isTRUE(x[[i]]), "TRUE", "FALSE"),
          "\n"
        )
        result <- paste0(result, item_text)
      } else if (S7_inherits(x[[i]])) {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ":\n" # S7 show begin on next line, otherwise must have different pad for first line (S7name) and for rest
        )
        result <- paste0(result, item_text)
        # Show S7 object: repr() must return a character string of length 1
        s7_output <- tryCatch(
          {
            repr(x[[i]], pad = lhs + 2, output_type = output_type)
          },
          error = function(e) {
            paste0(
              "(S7 object of class: '",
              paste(class(x[[i]]), collapse = ", "),
              "')\n"
            )
          }
        )
        result <- paste0(result, s7_output)
      } else if (is.data.frame(x[[i]])) {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ": ",
          if (print_class) {
            gray(
              paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "),
              output_type = output_type
            )
          } else {
            ""
          },
          headdot(x[[i]], maxlength = maxlength, format_fn = format_fn_rhs),
          "\n"
        )
        result <- paste0(result, item_text)
      } else if (isS4(x[[i]])) {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ": "
        )
        result <- paste0(result, item_text)

        # Print S4 object
        if (print_S4) {
          result <- paste0(result, "\n")
          # For S4 objects, we would need to capture their print output
          # This is complex, so for now we'll just show the class
          result <- paste0(
            result,
            "(S4 object of class: '",
            paste(class(x[[i]]), collapse = ", "),
            "')\n"
          )
        } else {
          result <- paste0(
            result,
            "(S4 object of class: '",
            paste(class(x[[i]]), collapse = ", "),
            "')\n"
          )
        }
      } else if (!is_common_struct(x[[i]])) {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ": ",
          if (print_class) {
            gray(
              paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "),
              output_type = output_type
            )
          } else {
            ""
          },
          italic(
            paste(
              "object of class:",
              paste(class(x[[i]]), collapse = ", ")
            ),
            output_type = output_type
          ),
          "\n"
        )
        result <- paste0(result, item_text)
      } else {
        item_text <- paste0(
          item_format(
            format(
              paste0(prefix, xnames[i]),
              width = lhs,
              justify = "right"
            ),
            output_type = output_type
          ),
          ": ",
          if (print_class) {
            gray(
              paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> "),
              output_type = output_type
            )
          } else {
            ""
          },
          headdot(x[[i]], maxlength = maxlength, format_fn = format_fn_rhs),
          "\n"
        )
        result <- paste0(result, item_text)
      }
    }
  }

  result
} # /rtemis::show_ls
