# dev script

# read grid

library(tidyverse)
library(openxlsx)
library(igraph)

# file <- "inst/extdata/sylvia.xlsx"
file <- "inst/extdata/Sylvia grid.xlsx"
# file <- "inst/extdata/NK ptp1 grid v2.xlsx"
#file <- "inst/extdata/Sylvia grid - long elements.xlsx"
file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") %>% basename

x <- read.xlsx(file)
tests <- check_excel_input(x)
l <- network_graph_images(x, min_clique_size = 3, show_edges = T, min_matches = 7)
file_tmp <- create_excel_output(file, l)
file.copy(file_tmp, file_out, overwrite = T)

