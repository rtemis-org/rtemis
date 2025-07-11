% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_table.R
\name{draw_table}
\alias{draw_table}
\title{Simple HTML table}
\usage{
draw_table(
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
  margin = list(l = 0, r = 5, t = 30, b = 0, pad = 0)
)
}
\arguments{
\item{x}{data.frame: Table to draw}

\item{.ddSci}{Logical: If TRUE, apply \link{ddSci} to numeric columns.}

\item{main}{Character: Table tile.}

\item{main_col}{Color: Title color.}

\item{main_x}{Float [0, 1]: Align title: 0: left, .5: center, 1: right.}

\item{main_xanchor}{Character: "auto", "left", "right": plotly's layout xanchor for
title.}

\item{fill_col}{Color: Used to fill header with column names and first column with
row names.}

\item{table_bg}{Color: Table background.}

\item{bg}{Color: Background.}

\item{line_col}{Color: Line color.}

\item{lwd}{Float: Line width.}

\item{header_font_col}{Color: Header font color.}

\item{table_font_col}{Color: Table font color.}

\item{font_size}{Integer: Font size.}

\item{font_family}{Character: Font family.}

\item{margin}{List: plotly's margins.}
}
\value{
\code{plotly} object.
}
\description{
Draw an html table using \code{plotly}
}
\examples{
\dontrun{
 df <- data.frame(
   Name = c("Alice", "Bob", "Charlie"),
   Age = c(25, 30, 35),
   Score = c(90.5, 85.0, 88.0)
)
p <- draw_table(
  df,
  main = "Sample Table",
  main_col = "#00b2b2"
)
}
}
\author{
EDG
}
