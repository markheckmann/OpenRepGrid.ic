#//////////////////////////////////////////////////
#
#                     DEV 
#
#//////////////////////////////////////////////////

# The shiny package is just a small UI wrapper around the 
# the workhorse core functions. Here is how to call them.

library(tidyverse)
library(openxlsx)
library(igraph)
library(OpenRepGrid.ic)

#file <- "inst/extdata/Sylvia grid.xlsx"  # alternative
file <- system.file("extdata/Sylvia grid.xlsx", package = "OpenRepGrid.ic")
file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") %>% basename

x <- read.xlsx(file)
tests <- check_excel_input(x)  # check if input format is correct
l <- network_graph_images(x, min_clique_size = 3, show_edges = T, min_matches = 6, 
                          label_abbreviate = T, label_wrap_width = 15, label_max_length = -1,
                          indicate_direction = T, colorize_direction = T)
file_tmp <- create_excel_output(file, l)
file.copy(file_tmp, file_out, overwrite = TRUE)


# file <- "inst/extdata/NK ptp1 grid v2.xlsx"
#file <- "inst/extdata/Sylvia grid - long elements.xlsx"

