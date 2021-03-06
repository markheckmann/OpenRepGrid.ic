% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-calculate.R
\name{count_matches}
\alias{count_matches}
\title{Count number of matches / non-matches for two constructs}
\usage{
count_matches(ci, cj, inverse = FALSE)
}
\arguments{
\item{ci, cj}{Two ratings scores to be compared.}

\item{inverse}{Whether to count matches (`FALSE`) or inverse matches (`TRUE`).}
}
\description{
Missing values lead to neither a match nor non-match
}
\keyword{internal}
