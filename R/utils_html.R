# html_ops.R
# ::rtemis::
# 2023- EDG rtemis.org

#' @keywords internal
#' @noRd
html_highlight <- function(..., bold = TRUE) {
  if (bold) {
    html_span(..., style = "color: #16A0AC; font-weight: 700;")
  } else {
    html_span(..., style = "color: #16A0AC;")
  }
}


#' @keywords internal
#' @noRd
html_orange <- function(..., bold = TRUE) {
  if (bold) {
    html_span(..., style = "color: #FA6E1E; font-weight: 700;")
  } else {
    html_span(..., style = "color: #FA6E1E;")
  }
}


#' @keywords internal
#' @noRd
html_red <- function(..., bold = TRUE) {
  if (bold) {
    html_span(..., style = "color: #E61048; font-weight: 700;")
  } else {
    html_span(..., style = "color: #E61048;")
  }
}


#' @keywords internal
#' @noRd
html_success <- function(..., bold = TRUE) {
  if (bold) {
    html_span(..., style = "color: #32A03E; font-weight: 700;")
  } else {
    html_span(..., style = "color: #32A03E;")
  }
}


# %% twocol2html ----
#' Render a two-column data.frame as an HTML table
#'
#' Built for tooltips that have to carry a table rather than a line of text.
#' `draw_linad()` puts one of these on every node so a reader can see how a
#' coefficient changes from node to node -- including when it changes sign,
#' which the per-cell background color is what makes visible at a glance.
#'
#' The second column is right-aligned with tabular figures, so the digits line
#' up down the column and a change in magnitude reads without being parsed.
#' Minus signs become `&minus;`, which is typographically the same width as a
#' plus and so does not shift the alignment.
#'
#' @param x data.frame: Two columns, a label and a numeric value.
#' @param value_col Character vector: Background color per data row, length
#' `NROW(x)`. Supply one gradient computed across every table being shown, or
#' the colors mean something different in each.
#' @param font_family Character: CSS font stack.
#' @param font_col Character: Header and label color.
#' @param value_font_col Character: Color of the value text.
#' @param font_size Character: CSS font size.
#' @param header_bg Character: Header background.
#' @param table_bg Character: Table background.
#' @param head_padding Character: CSS padding for header cells.
#' @param value_padding Character: CSS padding for value cells.
#' @param digits Integer: Significant digits for the values.
#'
#' @return Character: One HTML table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
twocol2html <- function(
  x,
  value_col = rep("#525252", NROW(x)),
  font_family = "'Helvetica Neue', sans-serif",
  font_col = "#ffffff",
  value_font_col = "#ffffff",
  font_size = "14px",
  header_bg = "#404040",
  table_bg = "#7F7F7F",
  head_padding = "4px",
  value_padding = "4px",
  digits = 3L
) {
  table_style <- paste0(
    '<table style="font-family: ',
    font_family,
    "; display: table; border-collapse: collapse; margin-left: auto; ",
    "margin-right: auto; color: ",
    font_col,
    "; font-size: ",
    font_size,
    "; padding: 0px; text-align: right; background-color: ",
    table_bg,
    '; width: auto; border-top-style: none; border-bottom-style: none">'
  )
  header_cell <- function(label) {
    paste0(
      '<th style="font-weight: bold; padding: ',
      head_padding,
      "; text-align: center; background-color: ",
      header_bg,
      '">',
      label,
      "</th>"
    )
  }
  header <- paste0(
    "<tr>",
    header_cell(colnames(x)[[1L]]),
    header_cell(colnames(x)[[2L]]),
    "</tr>"
  )
  rows <- vapply(
    seq_len(NROW(x)),
    function(i) {
      paste0(
        "<tr><td>",
        x[i, 1L],
        '</td><td style="color: ',
        value_font_col,
        "; font-variant-numeric: tabular-nums; background-color: ",
        value_col[[i]],
        "; padding: ",
        value_padding,
        '">',
        ddSci(x[i, 2L], digits),
        "</td></tr>"
      )
    },
    character(1L)
  )
  # A hyphen-minus is narrower than a plus and would break the alignment the
  # tabular figures are there to give.
  rows <- gsub(">-", ">&minus;", paste(rows, collapse = ""))
  paste0(table_style, header, rows, "</table>")
} # /rtemis::twocol2html
