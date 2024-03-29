% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OpenRepGrid.ic.R
\docType{package}
\name{OpenRepGrid.ic}
\alias{OpenRepGrid.ic-package}
\alias{OpenRepGrid.ic}
\title{\pkg{OpenRepGrid.ic} -  Interpretive Clustering for Repertory Grids}
\description{
The \pkg{OpenRepGrid.ic} package implements \emph{Interpretive Clustering (IC)}
as outlined in Burr, King, and Heckmann (2020). The authors describe
a variant of construct clustering which uses a procedure from graph theory
called \href{https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques}{maximal cliques enumeration}.
Given a similarity measure, in our case the number of matching scores between
two constructs, a network graph of relatedness between constructs is
construed. A clique is a group of constructs which are all mutually related,
given some cut-off criterion for relatedness (e.g. 6 matching scores in a
grid with 7 elements). While the paper also describes an offline approach to identify
the construct cliques, this software automates the process. Under the hood,
the package uses the \code{igraph} package for clique identification.

The package also contains a shiny based UI you can start via the function
\code{ic()}. Visit \url{http://ic.openrepgrid.org} for an online version.
An introduction to the software is also available on \href{https://youtu.be/f9oINYA22Rc}{YouTube}.
Below you find an example of how to process a repgrid in an Excel
file using code only.
}
\examples{
# The shiny package is just a small UI wrapper around the 
# the workhorse core functions. Here is how to call them.

library(tidyverse)
library(openxlsx)
library(igraph)
library(OpenRepGrid.ic)

file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") \%>\% basename

x <- read.xlsx(file)            # read grid
tests <- check_excel_input(x)   # check if input format is correct
l <- network_graph_images(x, min_clique_size = 3, 
                          show_edges = TRUE, 
                          min_matches = 6)    # produce images
file_tmp <- create_excel_output(file, l)      # create Excel file
# file.show(file_tmp)  # not run during tests

# open images saved as temp files (as shown in output Excel file)
file.show(l$img_all_constructs)
file.show(l$img_all_constructs_full_labels)
file.show(l$img_all_constructs_separate_poles)

file.show(l$img_cliques_only)
file.show(l$img_cliques_only_full_labels)
file.show(l$img_cliques_only_separate_poles)

# calculation results used in network_graph_images
# some of them are also contained in Excel file
s <- calculate_similarity(x)
s


}
\references{
Burr, V. King, N. & Heckmann, M. (2020) The qualitative analysis of repertory grid data: Interpretive Clustering,
    Qualitative Research in Psychology, \doi{10.1080/14780887.2020.1794088}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/markheckmann/OpenRepGrid.ic}
  \item Report bugs at \url{https://github.com/markheckmann/OpenRepGrid.ic/issues}
}

}
\author{
\strong{Maintainer}: Mark Heckmann \email{heckmann.mark@gmail.com} (\href{https://orcid.org/0000-0002-0736-7417}{ORCID}) [copyright holder]

Authors:
\itemize{
  \item Viv Burr
  \item Nigel King
}

}
\keyword{OpenRepGrid.ic}
\keyword{package}
