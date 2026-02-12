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


#' View table using reactable
#'
#' @param x data.frame, data.table or similar
#' @param datatypes Character vector: Data types of columns in x,
#' e.g. `c("numeric", "factor", "character")`
#' @param lightsout Logical: If TRUE, use dark theme.
#' @param bg Background color.
#' @param pagination Logical: If TRUE, paginate table.
#' @param searchable Logical: If TRUE, add search box.
#' @param bordered Logical: If TRUE, add border.
#' @param ... Additional arguments passed to `reactable::reactable`
#'
#' @return `reactable` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # needs html viewer
#' rt_reactable(iris, datatypes = sapply(iris, class))
#' }
rt_reactable <- function(
  x,
  datatypes = NULL,
  lightsout = TRUE,
  bg = "#121212",
  pagination = TRUE,
  searchable = TRUE,
  bordered = TRUE,
  ...
) {
  theme <- if (lightsout) {
    reactable::reactableTheme(
      color = "#fff",
      backgroundColor = bg,
      borderWidth = 0,
      stripedColor = "hsl(0, 0%, 12%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(
        backgroundColor = bg,
        borderColor = "hsl(0, 0%, 37%)"
      ),
      headerStyle = list(
        background = "hsl(0, 0%, 20%)",
        color = "#fff",
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 15%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
          background = "hsl(0, 0%, 10%)"
        ),
        borderColor = "#00000000"
      )
    )
  } else {
    reactable::reactableTheme(
      color = "#080808",
      backgroundColor = bg,
      borderWidth = 0,
      headerStyle = list(
        borderWidth = 0,
        background = "hsl(0, 0%, 85%)",
        color = "#000",
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 80%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
          background = "hsl(0, 0%, 75%)"
        ),
        borderColor = "#ffffff00"
      )
    )
  }

  header <- if (is.null(datatypes)) {
    function(value) {
      value <- gsub("_", " ", value, fixed = TRUE)
      div(title = value, value)
    }
  } else {
    function(value) {
      type <- datatypes[[value]]
      value <- gsub("_", " ", value, fixed = TRUE)
      div(
        title = value,
        value,
        div(type, style = "font-weight: 300; color: rgb(24, 163, 172);")
      )
    }
  }

  widget <- reactable::reactable(
    x,
    searchable = searchable,
    pagination = pagination,
    bordered = bordered,
    resizable = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    style = list(backgroundColor = bg),
    defaultColDef = reactable::colDef(
      header = header,
      cell = function(value) format(value, digits = 2, nsmall = 2),
      align = "right"
    ),
    theme = theme,
    ...
  )

  if (isTRUE(lightsout)) {
    widget <- htmlwidgets::prependContent(
      widget,
      htmltools::tags$style(sprintf(
        "body { background: %s; }",
        bg
      ))
    )
  }

  widget
} # /rtemis::rt_reactable
