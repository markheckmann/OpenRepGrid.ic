# make calculations



# number of matches / non-matches for two constructs 
# missing values lead to neither a match nor non-match
#
count_matches <- function(ci, cj, inverse = F) 
{
  if (inverse)
    matched <- ci != cj   # non-matches / inverse match count
  else 
    matched <- ci == cj   # matches
  sum(matched, na.rm = T)
}


#' Calculate similarity matrix
#' 
#' @param x Grid data.
#' @param min_matches Minimal number of matches to considers constructs as related.
#' @export
#' 
calculate_similarity <- function(x, min_matches = 6) #, use_labels = FALSE)
{
  i_preferred <- which(names(x) == "preferred")
  i_left <- 1
  i_right <- i_preferred - 1
  i_ratings <- (i_left + 1):(i_right - 1)
  
  # align constructs so all preferred poles are on right side
  # hence high matches represent positive relatedness
  for (i in seq_along(x$preferred)) {
    p <- x$preferred[i]
    if (isTRUE(p == 0)) {
      x[i, i_ratings] <- (as.numeric(x[i, i_ratings]) * -1 + 1)
      tmp <- x[i, i_left]
      x[i, i_left] <- x[i, i_right]  # change pole labels
      x[i, i_right] <- tmp
      x$preferred[i] <- 1
    }
  }
  
  s <- x[, i_ratings]   # remove construct poles
  S <- as.matrix(s)
  pole_left <- x[, i_left]
  pole_right <- x[, i_right]
  constructs <- paste(pole_left, "-", pole_right)
  
  # all positive poles (or neutral) are right pole by default
  valence_right <- recode(x$preferred, `1` = 1, `0` = -1)
  valence_right <- tidyr::replace_na(valence_right, 0)
  valence_left <- valence_right * -1
  
  # measure for construct relatedness: e.g. identical responses in 6 out of 7 cases (= matches)
  criterion_ <- min_matches
  
  # number of same ratings for pairs of constructs
  nr  <- nrow(S)
  R_01 <- R <- D <- COL <- MM <- Mi <- M <- matrix(NA, nr, nr)
  for (i in 1L:nr) {
    for (j in 1L:nr) {
      c_i <- S[i, ]
      c_j <- S[j, ]
      M[i, j] <- count_matches(c_i, c_j)                # matches
      Mi[i, j] <- count_matches(c_i, c_j, inverse = T)  # mismatches
    }
  }
  
  # # for each construct geerate list of constructs constructs with >= 6 matches
  # mm <- apply(ii, 1, which)   # like in Table 4a 
  # # mx <- sapply(mm, length) %>% max
  
  # keep bigger relatedness score
  M_vec <- as.vector(M)
  Mi_vec <- as.vector(Mi)
  R[, ] <- ifelse(M_vec > Mi_vec, M_vec, Mi_vec)
  diag(R) <- NA

  diag(M) <- NA   # exlude match to self
  diag(Mi) <- NA  # exlude match to self
  
  # mark positive and negative relatedness
  ii_m <- M >= criterion_ 
  ii_mi <- Mi >= criterion_
  ii <- ii_m | ii_mi   # min of 6 identical responses (match or mismatch)
  D[ii_m] <- 1
  D[ii_mi] <- -1
  
  # experiments in visualization
  MM[,] <- 0
  MM[ii] <- 1
  R_01[,] <- as.numeric(R >= criterion_)
  MM == R_01
  
  diag(MM) <- 0
  # if (use_labels) {
  #   labels <- constructs 
  # } else {
  #   labels <- paste0("C", 1L:nrow(MM))
  # }
  
  labels <- paste0("C", 1L:nrow(MM))
  colnames(R) <- colnames(D) <- colnames(MM) <- labels
  rownames(R) <- rownames(D) <- rownames(MM) <- labels
  names(constructs) <- labels
  names(pole_left) <- labels
  names(pole_right) <- labels
  names(valence_left) <- labels
  names(valence_right) <- labels

  list(R = R,    # no of matches 
       MM = MM,  # relatedness 0/1
       D = D,    # direction of relation -1/1
       constructs = constructs,
       pole_left = pole_left,
       pole_right = pole_right,
       valence_left = valence_left,
       valence_right = valence_right)
}



# https://stackoverflow.com/questions/55910373/generating-distinct-groups-of-nodes-in-a-network

#' Build network graph plots
#'
#' Detects maximal cliques and saves images of network graphs into tempfile.
#' Tempfile paths and info on cliques are returned.
#'
#' @param x A dataframe with a grid.
#' @param min_clique_size miniaml size of cliquesto be considered.
#' @param show_edges Whether to show edges in plot.
#' @param min_matches Minimal number of matching scores between constructs to be
#'   marked as related.
#' @param label_wrap_width Width of wrapped element label text.
#' @param label_max_length Trim element label at max length characters.
#' @param indicate_direction,colorize_direction Indicate direction of
#'   relatedness by \code{+/-} sign and edge color repsectively.
#' @export
#' 
network_graph_images <- function(x, 
                                 min_clique_size = 3, 
                                 show_edges = TRUE, 
                                 min_matches = 6, 
                                # label_abbreviate = FALSE,
                                 label_wrap_width = 15, 
                                 label_max_length = -1,
                                 indicate_direction = TRUE, 
                                 colorize_direction = TRUE) 
{
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
  
  ## show all constructs
  nms_keep <- clique_lists %>% unlist %>% unique
  MM2 <- MM[nms_keep, nms_keep]
  mark_border <- ifelse(n_clique == 0, NA, 1L:n_clique)
  mark_col <- ifelse(n_clique == 0, NA, scales::alpha(1L:n_clique, .2))
  
  # colorize edges by direction
  edges <- ends(g, E(g))   # edge from to as rowwise matrix
  edge_directions <- D[edges] 
  edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red")
  edge_labels <- NULL
  if (indicate_direction)
    edge_labels <- recode(edge_directions, `1` = "+", `-1` = "-")
  if (colorize_direction)
    E(g)$color <- edge_colors
  
  
  # change labels
  
  # cns <- V(g)$name
  # vertex.labels <- 
  #   cnames[cns] %>% 
  #   str_sub(start = 1, end = label_max_length) %>%
  #   str_wrap(width = label_wrap_width)
  # if (label_abbreviate) {
  #   vertex.labels <- NULL
  #   vertex.size = 15
  #   vertex.label.cex <- 1
  # } else {
  #   vertex.size <- 22
  #   vertex.label.cex <- .6
  # }
  
  # abbreviated labels
  vertex.labels <- NULL
  vertex.size = 15
  vertex.label.cex <- 1
  img_all_constructs <- tempfile(fileext = ".png")
  set.seed(0)
  png(img_all_constructs, width = 20, height = 20, units = "cm", res = 300)
    par(oma = c(0,0,0,0), mar = c(0,0,0,0))
    set.seed(0)
    igraph::plot.igraph(g, 
                        mark.groups = clique_lists, 
                        mark.border = mark_border, 
                        mark.col = mark_col, 
                        mark.expand = 15, 
                        edge.arrow.size = 0, 
                        edge.lty = edge.lty, 
                        edge.width = 1,
                        edge.label = edge_labels,
                        edge.label.color = edge_colors,
                        vertex.size = vertex.size,
                        vertex.size2 = vertex.size,
                        vertex.label = vertex.labels,
                        vertex.label.color = "black",
                        vertex.label.cex = vertex.label.cex,
                        vertex.label.family = "sans",
                        vertex.color = grey(.9),
                        vertex.frame.color = grey(.5))
  dev.off()
  
  # full labels
  cns <- V(g)$name
  vertex.labels <-
    cnames[cns] %>%
    str_sub(start = 1, end = label_max_length) %>%
    str_wrap(width = label_wrap_width)
    vertex.size <- 22
    vertex.label.cex <- .6
  img_all_constructs_full_labels <- tempfile(fileext = ".png")
  set.seed(0)
  png(img_all_constructs_full_labels, width = 20, height = 20, units = "cm", res = 300)
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  igraph::plot.igraph(g, 
                      mark.groups = clique_lists, 
                      mark.border = mark_border, 
                      mark.col = mark_col, 
                      mark.expand = 15, 
                      edge.arrow.size = 0, 
                      edge.lty = edge.lty, 
                      edge.width = 1,
                      edge.label = edge_labels,
                      edge.label.color = edge_colors,
                      vertex.size = vertex.size,
                      vertex.size2 = vertex.size,
                      vertex.label = vertex.labels,
                      vertex.label.color = "black",
                      vertex.label.cex = vertex.label.cex,
                      vertex.label.family = "sans",
                      vertex.color = grey(.9),
                      vertex.frame.color = grey(.5))
  dev.off()
  
  
  ## clique constructs only 
  
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
  
  
  # cns <- V(g2)$name
  # vertex.labels <- 
  #   cnames[cns] %>% 
  #   str_sub(start = 1, end = label_max_length) %>%
  #   str_wrap(width = label_wrap_width)
  # if (label_abbreviate) {
  #   vertex.labels <- NULL
  #   vertex.size = 15
  #   vertex.label.cex <- 1
  # } else {
  #   vertex.size <- 22
  #   vertex.label.cex <- .6
  # }
  
  # abbreviated construct labels
  vertex.labels <- NULL
  vertex.size = 30
  vertex.label.cex <- 1
  img_cliques_only <- tempfile(fileext = ".png")
  set.seed(0)
  png(img_cliques_only, width = 20, height = 20, units = "cm", res = 300)
    par(oma = c(0,0,0,0), mar = c(0,0,0,0))
    if (n_clique > 0) {
      igraph::plot.igraph(g2, 
                          mark.groups = clique_lists, 
                          mark.border = mark_border, 
                          mark.col = mark_col, 
                          mark.expand = 15, 
                          edge.arrow.size = 0, 
                          edge.lty = edge.lty, 
                          edge.width = 1,
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
    } else {
      plot.new()
      text(.5, .5, "No cliques detected", cex = 1.5, adj = c(.5, .5))
    } 
  dev.off()    

  # full labels
  cns <- V(g2)$name
  vertex.labels <-
    cnames[cns] %>%
    str_sub(start = 1, end = label_max_length) %>%
    str_wrap(width = label_wrap_width)
    vertex.size <- 30
    vertex.label.cex <- .6
  img_cliques_only_full_labels <- tempfile(fileext = ".png")
  set.seed(0)
  png(img_cliques_only_full_labels, width = 20, height = 20, units = "cm", res = 300)
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  if (n_clique > 0) {
    igraph::plot.igraph(g2, 
                        mark.groups = clique_lists, 
                        mark.border = mark_border, 
                        mark.col = mark_col, 
                        mark.expand = 15, 
                        edge.arrow.size = 0, 
                        edge.lty = edge.lty, 
                        edge.width = 1,
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
  } else {
    plot.new()
    text(.5, .5, "No cliques detected", cex = 1.5, adj = c(.5, .5))
  } 
  dev.off()   
  
  l2 <- list(img_all_constructs = img_all_constructs, 
             img_all_constructs_full_labels = img_all_constructs_full_labels,
             img_cliques_only = img_cliques_only,
             img_cliques_only_full_labels = img_cliques_only_full_labels,
             min_clique_size = min_clique_size,
             cliques_list = clique_lists,
             clique_lists_full_names = clique_lists_full_names,
             min_matches = min_matches)
  append(l, l2)
}




