

#' \pkg{OpenRepGrid.ic} - OpenRepGrid.ic
#'  
#' @keywords package OpenRepGrid.ic
#' @name OpenRepGrid.ic-package
#' @docType package
#' @import 
#' shiny 
#' shinyjs 
#' shinyBS 
#' shinythemes
#' shinyWidgets
#' shinydashboard
#' shinydashboardPlus
#' shinycssloaders
#' data.table
#' formattable
#' openxlsx
#' DT
#' stringr
#' magrittr
#' tidyverse
#' tidyr
#' reshape2
#' testthat
#' scales
#' stringr
#' ggplot2
#' dplyr
#' splines
#' crayon
#' plotly
#' readxl
#' magrittr
#' officer
NULL


#' launch app
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








