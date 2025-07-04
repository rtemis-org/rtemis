% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_color.R
\name{color_invertRGB}
\alias{color_invertRGB}
\title{Invert Color in RGB space}
\usage{
color_invertRGB(x)
}
\arguments{
\item{x}{Color, vector}
}
\value{
Inverted colors using hexadecimal notation #RRGGBBAA
}
\description{
Invert Color in RGB space
}
\examples{
\dontrun{
cols <- c("red", "green", "blue")
previewcolor(cols)
cols |>
  color_invertRGB() |>
  previewcolor()
}
}
\author{
EDG
}
