# html_ops.R
# ::rtemis::
# 2023 EDG rtemis.org

#' @keywords internal
#' @noRd
html_highlight <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #16A0AC; font-weight: 700;")
  } else {
    span(..., style = "color: #16A0AC;")
  }
}


#' @keywords internal
#' @noRd
html_orange <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #FA6E1E; font-weight: 700;")
  } else {
    span(..., style = "color: #FA6E1E;")
  }
}


#' @keywords internal
#' @noRd
html_red <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #E61048; font-weight: 700;")
  } else {
    span(..., style = "color: #E61048;")
  }
}


#' @keywords internal
#' @noRd
html_success <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #32A03E; font-weight: 700;")
  } else {
    span(..., style = "color: #32A03E;")
  }
}


#' @keywords internal
#' @noRd
rtreactable <- function(
  x,
  pagination = TRUE,
  searchable = TRUE,
  bordered = TRUE,
  lightsout = FALSE,
  ...
) {
  theme <- if (lightsout) {
    reactable::reactableTheme(
      color = "#fff",
      backgroundColor = "#000",
      borderWidth = 0,
      stripedColor = "hsl(0, 0%, 20%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(
        backgroundColor = "hsl(0, 0%, 0%)",
        borderColor = "hsl(0, 0%, 37%)"
      ),
      headerStyle = list(
        background = "hsl(0, 0%, 65%)",
        color = "#000",
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 20%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
          background = "hsl(0, 0%, 25%)"
        ),
        borderColor = "#00000000"
      )
    )
  } else {
    reactable::reactableTheme(
      headerStyle = list(
        borderWidth = 0,
        background = "hsl(0, 0%, 55%)",
        color = "#fff",
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 60%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
          background = "hsl(0, 0%, 65%)"
        ),
        borderColor = "#ffffff00"
      )
    )
  }
  reactable::reactable(
    x,
    searchable = searchable,
    pagination = pagination,
    bordered = bordered,
    resizable = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      cell = function(value) format(value, digits = 2, nsmall = 2),
      align = "right",
      headerStyle = list(background = "#707070", color = "#fff")
    ),
    theme = theme,
    ...
  )
} # /rtemis::rtreactable
