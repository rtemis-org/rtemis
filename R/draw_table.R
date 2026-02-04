# draw_table.R
# ::rtemis::
# 2019 EDG rtemis.org

#' Simple HTML table
#'
#' Draw an html table using `plotly`
#'
#' @param x data.frame: Table to draw
#' @param .ddSci Logical: If TRUE, apply [ddSci] to numeric columns.
#' @param main Character: Table tile.
#' @param main_col Color: Title color.
#' @param main_x Float \[0, 1\]: Align title: 0: left, .5: center, 1: right.
#' @param main_xanchor Character: "auto", "left", "right": plotly's layout xanchor for
#' title.
#' @param fill_col Color: Used to fill header with column names and first column with
#' row names.
#' @param table_bg Color: Table background.
#' @param bg Color: Background.
#' @param line_col Color: Line color.
#' @param lwd Float: Line width.
#' @param header_font_col Color: Header font color.
#' @param table_font_col Color: Table font color.
#' @param font_size Integer: Font size.
#' @param font_family Character: Font family.
#' @param margin List: plotly's margins.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- data.frame(
#'    Name = c("Alice", "Bob", "Charlie"),
#'    Age = c(25, 30, 35),
#'    Score = c(90.5, 85.0, 88.0)
#' )
#' p <- draw_table(
#'   df,
#'   main = "Sample Table",
#'   main_col = "#00b2b2"
#' )
#' }
draw_table <- function(
  x,
  .ddSci = TRUE,
  main = NULL,
  main_col = "black",
  main_x = 0,
  main_xanchor = "auto",
  fill_col = "#18A3AC",
  table_bg = "white",
  bg = "white",
  line_col = "white",
  lwd = 1,
  header_font_col = "white",
  table_font_col = "gray20",
  font_size = 14,
  font_family = "Helvetica Neue",
  margin = list(
    l = 0,
    r = 5,
    t = 30,
    b = 0,
    pad = 0
  )
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Input ----
  x <- as.data.frame(x)
  if (.ddSci) {
    # x <- dplyr::mutate_if(x, is.numeric, ddSci)
    # Lose the dep:
    x <- data.frame(lapply(x, function(x) if (is.numeric(x)) ddSci(x) else x))
  }

  # Colnames ----
  if (!is.null(colnames(x))) {
    colnames(x) <- paste0("<b>", colnames(x), "</b>")
  }

  # Rownames ----
  if (!is.null(rownames(x))) {
    rownames(x) <- paste0("<b>", rownames(x), "</b>")
  }

  # plotly ----

  plt <- plotly::plot_ly(x)
  plt <- plotly::add_table(
    plt,
    header = list(
      line = list(
        width = lwd,
        color = c(
          "rgba(255,255,255,0)",
          plotly::toRGB(line_col)
        )
      ),
      fill = list(
        color = c(
          "rgba(255,255,255,0)",
          plotly::toRGB(fill_col)
        )
      ),
      align = c("right", "center"),
      font = list(
        color = plotly::toRGB(header_font_col),
        family = font_family,
        size = font_size
      )
    ),
    cells = list(
      line = list(
        width = lwd,
        color = c(
          plotly::toRGB(line_col),
          plotly::toRGB(fill_col)
        )
      ),
      fill = list(
        color = c(
          plotly::toRGB(fill_col),
          plotly::toRGB(table_bg)
        )
      ),
      align = c("right", "center"),
      font = list(
        color = c(
          plotly::toRGB(header_font_col),
          plotly::toRGB(table_font_col)
        ),
        family = font_family,
        size = font_size
      )
    )
  )

  # layout ----
  main <- paste0("<b>", main, "</b>")
  plt <- plotly::layout(
    plt,
    title = list(
      text = main,
      font = list(
        family = font_family,
        size = font_size,
        color = main_col
      ),
      x = main_x,
      xanchor = main_xanchor
    ),
    paper_bgcolor = plotly::toRGB(bg),
    margin = margin
  )

  plt
} # /rtemis::draw_table
