# The shiny package is just a small UI wrapper around the 
# the workhorse core functions. Here is how to call them.

library(tidyverse)
library(openxlsx)
library(igraph)
library(OpenRepGrid.ic)

file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") %>% basename

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


