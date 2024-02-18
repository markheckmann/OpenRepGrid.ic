

#' \pkg{OpenRepGrid.ic} -  Interpretive Clustering for Repertory Grids

#' @description The \pkg{OpenRepGrid.ic} package implements \emph{Interpretive Clustering (IC)}
#' as outlined in Burr, King, and Heckmann (2020). The authors describe
#' a variant of construct clustering which uses a procedure from graph theory
#' called \href{https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques}{maximal cliques enumeration}.
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
#' An introduction to the software is also available on \href{https://youtu.be/f9oINYA22Rc}{YouTube}.
#' Below you find an example of how to process a repgrid in an Excel
#' file using code only.
#'
#' @references
#' Burr, V. King, N. & Heckmann, M. (2020) The qualitative analysis of repertory grid data: Interpretive Clustering,
#'     Qualitative Research in Psychology, \doi{10.1080/14780887.2020.1794088}
#' @example inst/examples/01-process-excel-file.R
#' @keywords package OpenRepGrid.ic
#' @name OpenRepGrid.ic
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by case_when recode
#' @rawNamespace import(igraph, except = c(compare, normalize))
#' @rawNamespace import(graphics, except = box)
#' @importFrom withr with_par
#' @importFrom stats na.omit
#' @importFrom shinyBS tipify
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(shinyWidgets, except = c(progressBar, alert))
#' @import utils grDevices openxlsx stringr reshape2 formattable shinyjs rintrojs formattable shinyFeedback shinydashboardPlus shinythemes splines tidyverse
#' @rawNamespace import(shinydashboard, except = c(dashboardPage, dashboardSidebar, box, dashboardHeader, taskItem, notificationItem, messageItem))
"_PACKAGE"


#' Launch app in browser
#'
#' @param display.mode \code{auto} by default, can also be \code{showcase}. See
#'   \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} (default) to open the app in
#'   the browser. See \link[shiny]{runApp}.
#' @export
#' @examples
#' if (interactive()) {
#'   ic()
#' }
ic <- function(display.mode = "auto", launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "OpenRepGrid.ic")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `OpenRepGrid.ic`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
