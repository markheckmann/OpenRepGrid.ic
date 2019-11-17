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
#' @export
#' 
calculate_similarity <- function(x, min_matches = 6)
{
  i_preferred <- which(names(x) == "preferred")
  i_left <- 1
  i_right <- i_preferred - 1
  i_ratings <- (i_left + 1):(i_right - 1)
  
  # align constructs so all preferred poles are on right sode
  for (i in seq_along(x$preferred)) {
    p <- x$preferred[i]
    if (isTRUE(p == 0)) {
      x[i, i_ratings] <- (as.numeric(x[i, 2:8]) * -1 + 1)
      tmp <- x[i, i_left]
      x[i, i_left] <- x[i, i_right]  # change poale labels
      x[i, i_right] <- tmp
      x$preferred[i] <- 1
    }
  }
  
  s <- x[, i_ratings]   # remove construct poles
  S <- as.matrix(s)
  constructs <- paste(x[, i_left], "-", x[, i_right])
  
  # measure for construct relatedness: identical responses in 6 out of 7 cases (= matches)
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
  colnames(R) <- colnames(D) <- colnames(MM) <- paste0("C", 1:nrow(MM))
  rownames(R) <- rownames(D) <- rownames(MM) <- paste0("C", 1:nrow(MM))
  
  list(R = R, D = D, MM = MM)
}



# https://stackoverflow.com/questions/55910373/generating-distinct-groups-of-nodes-in-a-network

#' Build network graph plots
#' 
#' @export
#' 
network_graph_images <- function(x, min_clique_size = 3, show_edges = T, min_matches = 6) 
{
  l <- calculate_similarity(x, min_matches = min_matches)
  MM <- l$MM
  edge.lty <- ifelse(show_edges, 3, 0)
    
  g <- graph_from_adjacency_matrix(MM, diag = F, mode  = "undirected")
  mc <- max_cliques(g, min = min_clique_size)
  n_clique <- length(mc)
  nm <- lapply(mc, attr, "names")
  
  # show all constructs
  nms_keep <- nm %>% unlist %>% unique
  MM2 <- MM[nms_keep, nms_keep]
  img_all_constructs <- tempfile(fileext = ".png")
  png(img_all_constructs, width = 15, height = 15, units = "cm", res = 300)
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  plot.igraph(g, mark.groups = nm, mark.border = 1:n_clique, mark.col = alpha(1:n_clique, .2), 
              mark.expand = 15, edge.arrow.size = 0, edge.lty = edge.lty, edge.width = 1)
  dev.off()
  
  # show cliques only
  g <- graph_from_adjacency_matrix(MM2, diag = F, mode  = "undirected")
  mc <- max_cliques(g, min = 4)
  n_clique <- length(mc)
  gs <- simplify(g)
  nm <- lapply(mc, attr, "names")
  img_cliques_only <- tempfile(fileext = ".png")
  png(img_cliques_only, width = 15, height = 15, units = "cm", res = 300)
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  plot.igraph(gs, mark.groups = nm, mark.border = 1:n_clique, mark.col = alpha(1:n_clique, .1), 
              mark.expand = 15, edge.arrow.size = 0, edge.lty = edge.lty, edge.width = 1)
  dev.off()
  
  l2 <- list(img_all_constructs = img_all_constructs, 
             img_cliques_only = img_cliques_only,
             min_clique_size = min_clique_size,
             cliques_list = nm,
             min_matches = min_matches)
  append(l, l2)
}


