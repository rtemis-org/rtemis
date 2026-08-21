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
#' `max_height` is not cosmetic. A model over a hundred features produces a
#' table taller than the screen, which renders as a broken-looking tooltip and
#' makes the browser lay out hundreds of rows on every hover.
#'
#' @param x data.frame: Two columns, a label and a numeric value.
#' @param value_col Character vector: Background color per data row, length
#' `NROW(x)`. Supply one gradient computed across every table being shown, or
#' the colors mean something different in each.
#' @param caption Optional Character: Rendered above the header, for a quantity
#' that belongs to the table as a whole rather than to either column. It sits
#' outside the scrolling element, so it stays visible while the rows scroll.
#' @param caption_value Optional Character: The caption's own value, set below
#' it as the table's headline number, with `caption` demoted to its label.
#' NULL renders `caption` alone as a single line.
#' @param max_height Character: CSS max height; the table scrolls beyond it.
#' @param font_family Character: CSS font stack.
#' @param font_col Character: Header and label color.
#' @param value_font_col Optional Character: Color of the value text, recycled
#' over rows. NULL picks black or white per cell from that cell's own
#' background, which is the only choice that stays legible across a full
#' diverging gradient.
#' @param font_size Character: CSS font size.
#' @param header_bg Character: Header background.
#' @param table_bg Character: Table background.
#' @param radius Character: CSS corner radius.
#' @param pad Character: CSS padding inside cells.
#' @param border_col Optional Character: Color of a 1px border around the whole
#' table. NULL draws none.
#' @param digits Integer: Significant digits for the values.
#'
#' @return Character: One HTML table, wrapped in a rounded scrolling container.
#'
#' @author EDG
#' @keywords internal
#' @noRd
twocol2html <- function(
  x,
  value_col = rep("#525252", NROW(x)),
  caption = NULL,
  caption_value = NULL,
  max_height = "420px",
  font_family = "'Helvetica Neue', sans-serif",
  font_col = "#ffffff",
  value_font_col = NULL,
  font_size = "14px",
  header_bg = "#404040",
  table_bg = "#7F7F7F",
  radius = "6px",
  pad = "5px 9px",
  border_col = NULL,
  digits = 3L
) {
  # Read the text color off each cell's own background rather than fixing one
  # for the table: cells span a full diverging gradient, so a single color is
  # unreadable at one end whichever end it is chosen for.
  if (is.null(value_font_col)) {
    value_font_col <- ifelse(
      col2grayscale(value_col, "decimal") > 0.5,
      "#000000",
      "#ffffff"
    )
  }
  value_font_col <- rep_len(value_font_col, NROW(x))
  value_col <- rep_len(value_col, NROW(x))
  # One label treatment for the whole panel. The caption's label and the two
  # column headers are the same kind of thing -- names for the values beneath
  # them -- so they are set from one string rather than described twice.
  label_css <- paste0(
    "font-size: 0.78em; font-weight: 600; letter-spacing: 0.08em; ",
    "text-transform: uppercase; opacity: 0.65"
  )
  cell <- function(content, style) {
    paste0("<td style=\"", style, "\">", content, "</td>")
  }
  # `opacity` applies to an element's background as well as its text, so it
  # goes on an inner span; on the `th` it would let the table show through the
  # header.
  header_cell <- function(label) {
    paste0(
      '<th style="padding: ',
      pad,
      "; text-align: center; position: sticky; top: 0; background-color: ",
      header_bg,
      '"><span style="',
      label_css,
      '">',
      label,
      "</span></th>"
    )
  }
  # The caption spans both columns. Sitting it in its own element above the
  # table would align it with the first column instead of with the table.
  # The caption sits outside the scrolling element rather than in the table, so
  # a quantity belonging to the whole table stays visible while its rows are
  # scrolled. `em` sizes keep it proportional to `font_size` without parsing it.
  caption_block <- if (is.null(caption)) {
    ""
  } else {
    headline <- if (is.null(caption_value)) {
      ""
    } else {
      paste0(
        '<div style="font-size: 1.5em; font-weight: 600; line-height: 1.2; ',
        'font-variant-numeric: tabular-nums">',
        gsub("^-", "&minus;", caption_value),
        "</div>"
      )
    }
    label_style <- if (is.null(caption_value)) {
      # Without a number under it the caption is the content, not a name for
      # something, so it is not set as a label.
      "font-weight: 400"
    } else {
      label_css
    }
    paste0(
      '<div style="font-family: ',
      font_family,
      "; color: ",
      font_col,
      "; font-size: ",
      font_size,
      "; background-color: ",
      header_bg,
      "; padding: ",
      pad,
      "; text-align: center; border-bottom: 1px solid ",
      table_bg,
      '">',
      '<div style="',
      label_style,
      '">',
      caption,
      "</div>",
      headline,
      "</div>"
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
        "<tr>",
        cell(x[i, 1L], paste0("padding: ", pad, "; text-align: right")),
        cell(
          ddSci(x[i, 2L], digits),
          paste0(
            "color: ",
            value_font_col[[i]],
            "; font-variant-numeric: tabular-nums; text-align: right; ",
            "background-color: ",
            value_col[[i]],
            "; padding: ",
            pad
          )
        ),
        "</tr>"
      )
    },
    character(1L)
  )
  # A hyphen-minus is narrower than a plus and would break the alignment the
  # tabular figures are there to give.
  rows <- gsub(">-", ">&minus;", paste(rows, collapse = ""))
  table_html <- paste0(
    '<table style="font-family: ',
    font_family,
    "; border-collapse: collapse; color: ",
    font_col,
    "; font-size: ",
    font_size,
    "; background-color: ",
    table_bg,
    '; width: 100%">',
    header,
    rows,
    "</table>"
  )
  # Border and radius go on the outer element and scrolling on the inner one:
  # a radius on the scroller leaves square corners wherever the table overflows
  # it, and `overflow: hidden` here is what clips the children to the curve.
  border <- if (is.null(border_col)) {
    ""
  } else {
    paste0("; box-sizing: border-box; border: 1px solid ", border_col)
  }
  paste0(
    '<div style="border-radius: ',
    radius,
    "; overflow: hidden",
    border,
    '">',
    caption_block,
    '<div style="max-height: ',
    max_height,
    '; overflow-y: auto; overflow-x: hidden">',
    table_html,
    "</div></div>"
  )
} # /rtemis::twocol2html
