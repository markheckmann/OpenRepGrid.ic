

#' \pkg{OpenRepGrid.ic} -  Interpretive Clustering for Repertory Grids
#'  
#' @description The \pkg{OpenRepGrid.ic} package implements \emph{interpretive clustering}
#' as introduced in Burr, King, and Heckmann (forthcoming). The authors describe
#' a variant of construct clustering which uses a procedure from graph theory
#' called \href{https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques}{maxmimal cliques enumeration}.
#' Given a similarity measure, in our case the number of matching scores between
#' two constructs, a network graph of relatedness between constructs is
#' construed. A clique is a group of constructs which are all mutually related,
#' given some cut-off criterion for relatedness (e.g. 6 matching scores in a
#' grid with 7 elements). While the paper also describes an offline approach to identify
#' the construct cliques, this software automates the process. Under the hood, 
#' the package uses the \code{igraph} package for clique identification. 
#' 
#' The package also contains a shiny based UI you can start via the function 
#' \code{ic()}. Visit \url{http://ic.openrepgrid.org} for an online version. 
#' Below you find an example of how to process a repgrid in an Excel
#' file using code only.
#' 
#' @references 
#' Burr, King, and Heckmann (forthcoming). Interpretive Clustering. Manuscript submitted.
#' @example inst/examples/01-process-excel-file.R
#' @keywords package OpenRepGrid.ic
#' @name OpenRepGrid.ic-package
#' @docType package
#' @importFrom  magrittr "%>%"
#' @importFrom  dplyr filter select group_by
#' @rawNamespace import(graphics, except = box)
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace import(igraph, except = compare)
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import
#' utils
#' grDevices
#' shinyjs
#' shinyBS
#' shinythemes
#' shinyWidgets
#' shinydashboard
#' shinydashboardPlus
#' shinycssloaders
#' shinyFeedback
#' reactlog
#' rintrojs
#' openxlsx
#' DT
#' stringr
#' reshape2
#' testthat
#' scales
#' splines
#' formattable
NULL


#' Launch app in browser
#'
#' @param display.mode \code{auto} by default, can also be \code{showcase}. See
#'   \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} (default) to open the app in
#'   the browser. See \link[shiny]{runApp}.
#' @export
#' @import shiny
#' @import shinythemes
#' @import shinyBS
#' @examples
#' \dontrun{
#' ic()
#' }
ic <- function(display.mode = "auto",
                    launch.browser = TRUE) 
{
  appDir <- system.file("shiny", package = "OpenRepGrid.ic")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `OpenRepGrid.ic`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}








