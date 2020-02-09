# The shiny package is just a small UI wrapper around the 
# the workhorse core functions. Here is how to call them.

\dontrun{
  
library(tidyverse)
library(openxlsx)
library(igraph)
library(OpenRepGrid.ic)

file <- system.file("extdata/sylvia.xlsx", package = "OpenRepGrid.ic")
file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") %>% basename

x <- read.xlsx(file)            # read grid
tests <- check_excel_input(x)   # check if input format is correct
l <- network_graph_images(x, min_clique_size = 3, 
                          show_edges = T, 
                          min_matches = 7)  # produce images
file_tmp <- create_excel_output(file, l)         # create Excel file
file.copy(file_tmp, file_out, overwrite = TRUE)  # copy Excel to working dir

# calculation results used in network_graph_images
# some of them are also contained in Excel file
s <- calculate_similarity(x)
s
}


