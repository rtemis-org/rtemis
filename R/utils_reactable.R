# rt_reactable.R
# ::rtemis::
# 2022 EDG rtemis.org

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
#' @author E D Gennatas
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

  reactable::reactable(
    x,
    searchable = searchable,
    pagination = pagination,
    bordered = bordered,
    resizable = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    defaultColDef = reactable::colDef(
      header = header,
      cell = function(value) format(value, digits = 2, nsmall = 2),
      align = "right"
    ),
    theme = theme,
    ...
  )
} # /rtemis::rt_reactable
