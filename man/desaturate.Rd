% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_color.R
\name{desaturate}
\alias{desaturate}
\title{Pastelify a color (make a color more pastel)}
\usage{
desaturate(color, s = 0.3)
}
\arguments{
\item{color}{Color, vector: Color(s) to operate on}

\item{s}{Float: Decrease saturation by this fraction. Default = .3, which means if saturation of given color is 1,
it will become .7}
}
\value{
List of adjusted colors
}
\description{
Lower a color's saturation by a given percent in the HSV color system
}
\examples{
\dontrun{
color <- c("red", "green", "blue")
color_p <- desaturate(color)
}
}
\author{
EDG
}
