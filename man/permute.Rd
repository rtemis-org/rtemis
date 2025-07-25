% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{permute}
\alias{permute}
\title{Create permutations}
\usage{
permute(n)
}
\arguments{
\item{n}{Integer: Length of elements to permute}
}
\value{
Matrix where each row is a different permutation
}
\description{
Creates all possible permutations
}
\details{
n higher than 10 will take a while, or may run out of memory in systems
with limited RAM
}
