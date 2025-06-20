% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_data.table.R
\name{dt_pctmatch}
\alias{dt_pctmatch}
\title{Get N and percent match of values between two columns of two data.tables}
\usage{
dt_pctmatch(x, y, on = NULL, left_on = NULL, right_on = NULL, verbosity = 1L)
}
\arguments{
\item{x}{data.table}

\item{y}{data.table}

\item{on}{Integer or character: column to read in \code{x} and \code{y}, if it is the
same}

\item{left_on}{Integer or character: column to read in \code{x}}

\item{right_on}{Integer or character: column to read in \code{y}}

\item{verbosity}{Integer: Verbosity level.}
}
\value{
list.
}
\description{
Get N and percent match of values between two columns of two data.tables
}
\author{
EDG
}
