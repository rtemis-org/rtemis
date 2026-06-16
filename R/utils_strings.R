# strng.R
# ::rtemis::
# 2022 EDG rtemis.org

# General hilite function output bold + any color.

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
    col = rtemis_colors[["orange"]],
    bold = bold,
    output_type = output_type
  )
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
  fmt("citation", col = rtemis_colors[["blue"]]),
  "(",
  fmt("rtemis", col = rtemis_colors[["teal"]]),
  ")"
)


checkmark <- function(
  col = rtemis_colors[["green"]],
  output_type = c("ansi", "html", "plain")
) {
  fmt("\u2713", col = col, bold = TRUE, output_type = output_type)
}

crossmark <- function(output_type = c("ansi", "html", "plain")) {
  fmt(
    "\u2715",
    col = rtemis_colors[["red"]],
    bold = TRUE,
    output_type = output_type
  )
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
    rtemis.core::abort(
      "Both `left` and `right` must be <table> objects.\n",
      "Got: <",
      paste(class(left), collapse = "/"),
      "> and <",
      paste(class(right), collapse = "/"),
      ">.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  # Check dimensions match
  if (!identical(dim(left), dim(right))) {
    rtemis.core::abort(
      "Tables must have matching dimensions.\n",
      "Got dimensions: ",
      paste(dim(left), collapse = " x "),
      " and ",
      paste(dim(right), collapse = " x "),
      ".",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
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
    rtemis.core::abort(
      "Both `left` and `right` must be <data.frame> objects.\n",
      "Got: <",
      paste(class(left), collapse = "/"),
      "> and <",
      paste(class(right), collapse = "/"),
      ">.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  # Check dimensions match
  if (!identical(dim(left), dim(right))) {
    rtemis.core::abort(
      "Data frames must have matching dimensions.\n",
      "Got dimensions: ",
      paste(dim(left), collapse = " x "),
      " and ",
      paste(dim(right), collapse = " x "),
      ".",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
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
