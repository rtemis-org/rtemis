% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_strings.R
\name{clean_names}
\alias{clean_names}
\title{Clean names}
\usage{
clean_names(x, prefix_digits = "V_")
}
\arguments{
\item{x}{Character vector.}

\item{prefix_digits}{Character: prefix to add to names beginning with a
digit. Set to NA to skip.}
}
\value{
Character vector.
}
\description{
Clean character vector by replacing all symbols and sequences of symbols with single
underscores, ensuring no name begins or ends with a symbol
}
\examples{
\dontrun{
x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
x
clean_names(x)
}
}
\author{
EDG
}
