# utils_markdown.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% md_heading ----
#' Markdown ATX heading
#'
#' @param text Character: Heading text.
#' @param level Integer: Heading level, 1 to 6.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
md_heading <- function(text, level) {
  paste0(strrep("#", level), " ", text)
} # /rtemis::md_heading


# %% md_table ----
#' Markdown pipe table
#'
#' Pipes inside cells are escaped, so a cell cannot split a row.
#'
#' @param rows Character matrix: The header row, then the body rows.
#' @param align Optional Character: Per column, "left" or "right". NULL leaves
#'   alignment to the renderer.
#'
#' @return Character vector, one line per row and one for the delimiter row.
#'
#' @author EDG
#' @keywords internal
#' @noRd
md_table <- function(rows, align = NULL) {
  md_row <- function(cells) {
    paste0(
      "| ",
      paste(gsub("|", "\\|", cells, fixed = TRUE), collapse = " | "),
      " |"
    )
  }
  delimiter <- if (is.null(align)) {
    rep("---", NCOL(rows))
  } else {
    ifelse(align == "right", "---:", "---")
  }
  c(
    md_row(rows[1L, ]),
    paste0("|", paste(delimiter, collapse = "|"), "|"),
    vapply(
      seq_len(NROW(rows))[-1L],
      function(i) md_row(rows[i, ]),
      character(1L)
    )
  )
} # /rtemis::md_table
