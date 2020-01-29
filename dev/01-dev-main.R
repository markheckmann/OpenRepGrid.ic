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
library(RColorBrewer)

# #file <- "inst/extdata/Sylvia grid.xlsx"  # alternative
# file <- system.file("extdata/Sylvia grid.xlsx", package = "OpenRepGrid.ic")
# file_out <- str_replace(file, ".xlsx$", " CLIQUES.xlsx") %>% basename
# 
# x <- read.xlsx(file)
# tests <- check_excel_input(x)  # check if input format is correct
# l <- network_graph_images(x, min_clique_size = 3, show_edges = T, min_matches = 6, 
#                           label_wrap_width = 15, label_max_length = -1,
#                           indicate_direction = T, colorize_direction = T)
# file_tmp <- create_excel_output(file, l)
# file.copy(file_tmp, file_out, overwrite = TRUE)


# Richard -----------------------------------------------------------------------------------

x <- read.xlsx("../data/richard.xlsx")
file <- "../images/richard.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 40
vertex.label.cex <- .8
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")
        
#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(0)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  igraph::plot.igraph(g2, 
      mark.groups = clique_lists, 
      mark.border = mark_border, 
      mark.col = NA, 
      mark.expand = 20, 
      edge.arrow.size = 0, 
      edge.lty = edge.lty,
      edge.width = 2,
      edge.label = edge_labels,
      edge.label.color = edge_colors,
      vertex.size = vertex.size,
      vertex.label = vertex.labels,
      vertex.size2 = vertex.size,
      vertex.label.color = "black",
      vertex.label.cex = vertex.label.cex,
      vertex.label.family = "sans",
      vertex.color = grey(.9),
      vertex.frame.color = grey(.5))
dev.off() 




# Hugh -----------------------------------------------------------------------------------

x <- read.xlsx("../data/hugh.xlsx")
file <- "../images/hugh.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 40
vertex.label.cex <- .8
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(1)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))
igraph::plot.igraph(g2, 
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 20, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 



# Sylvia -----------------------------------------------------------------------------------

x <- read.xlsx("../data/sylvia.xlsx")
file <- "../images/sylvia.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 40
vertex.label.cex <- .8
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(1)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))
igraph::plot.igraph(g2, 
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 20, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 



# Amina -----------------------------------------------------------------------------------

x <- read.xlsx("../data/amina.xlsx")
file <- "../images/amina.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 40
vertex.label.cex <- .8
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")[1:n_clique]

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(1)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))
igraph::plot.igraph(g2, 
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 20, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 



# Jane -----------------------------------------------------------------------------------

x <- read.xlsx("../data/jane.xlsx")
file <- "../images/jane.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 25
vertex.label.cex <- .7
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")[1:n_clique]

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(1)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))

igraph::plot.igraph(g2, 
                    # layout = layout_with_dh(g2),
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 23, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 




# Hannah -----------------------------------------------------------------------------------

x <- read.xlsx("../data/hannah.xlsx")
file <- "../images/hannah.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 40
vertex.label.cex <- .8
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")[1:n_clique]

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(1)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))
igraph::plot.igraph(g2, 
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 20, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 


# Diane -----------------------------------------------------------------------------------


x <- read.xlsx("../data/diane NO LABELS.xlsx")
file <- "../images/diane.pdf"

min_clique_size = 3
show_edges = T
min_matches = 6
label_wrap_width = 15
label_max_length = -1
indicate_direction = T
colorize_direction = T

l <- calculate_similarity(x, min_matches = min_matches) #, use_labels = F)
MM <- l$MM
D <- l$D
edge.lty <- ifelse(show_edges, 3, 0)
cnames <- l$constructs

g <- igraph::graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
mc <- igraph::max_cliques(g, min = min_clique_size)
n_clique <- length(mc)

clique_lists <- lapply(mc, attr, "names")
clique_lists_full_names <- lapply(clique_lists, function(x) {
  cnames[x]
})

nms_keep <- clique_lists %>% unlist %>% unique
MM2 <- MM[nms_keep, nms_keep]
g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")

# colorize edges by direction
edges <- ends(g2, E(g2))   # edge from to as rowwise matrix
edge_directions <- D[edges] 
edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
edge_labels <- NULL
if (indicate_direction)
  edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
if (colorize_direction)
  E(g2)$color <- edge_colors


# full labels
cns <- V(g2)$name
vertex.labels <-
  cnames[cns] %>%
  str_sub(start = 1, end = label_max_length) %>%
  str_wrap(width = label_wrap_width)
vertex.size <- 25
vertex.label.cex <- .7
edge.lty <- 0
edge_labels <- NULL

mark_border <- brewer.pal(n_clique, "Dark2")[1:n_clique]

#img_cliques_only_full_labels <- tempfile(fileext = ".png")
set.seed(0)
pdf(file, width = 20 / 2.54, height = 20 / 2.54)
par(oma = c(0,0,0,0), mar = c(0,0,0,0))

igraph::plot.igraph(g2, 
                    # layout = layout_with_dh(g2),
                    mark.groups = clique_lists, 
                    mark.border = mark_border, 
                    mark.col = NA, 
                    mark.expand = 23, 
                    edge.arrow.size = 0, 
                    edge.lty = edge.lty,
                    edge.width = 2,
                    edge.label = edge_labels,
                    edge.label.color = edge_colors,
                    vertex.size = vertex.size,
                    vertex.label = vertex.labels,
                    vertex.size2 = vertex.size,
                    vertex.label.color = "black",
                    vertex.label.cex = vertex.label.cex,
                    vertex.label.family = "sans",
                    vertex.color = grey(.9),
                    vertex.frame.color = grey(.5))
dev.off() 
