% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/10_S7_DecompositionParameters.R
\name{setup_NMF}
\alias{setup_NMF}
\title{Setup NMF parameters.}
\usage{
setup_NMF(k = 2L, method = "brunet", nrun = if (length(k) > 1L) 30L else 1L)
}
\arguments{
\item{k}{Integer: Number of components.}

\item{method}{Character: NMF method. See \code{NMF::nmf}.}

\item{nrun}{Integer: Number of runs to perform.}
}
\value{
NMFParameters object.
}
\description{
Setup NMF parameters.
}
\author{
EDG
}
