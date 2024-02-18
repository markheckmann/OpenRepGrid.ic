# //////////////////////////////////////////////////////////////////////////////////////
#
#                                IC Calculations
#
# //////////////////////////////////////////////////////////////////////////////////////


#' Count number of matches / non-matches for two constructs
#'
#' Missing values lead to neither a match nor non-match
#' @param ci,cj Two ratings scores to be compared.
#' @param inverse Whether to count matches (`FALSE`) or inverse matches (`TRUE`).
#' @keywords internal
#'
count_matches <- function(ci, cj, inverse = FALSE) {
  if (inverse) {
    matched <- ci != cj
  } # non-matches / inverse match count
  else {
    matched <- ci == cj
  } # matches
  sum(matched, na.rm = TRUE)
}


#' Align all preferred poles on right side
#' @keywords internal
#'
align_positive_poles <- function(x) {
  i_preferred <- which(names(x) == "preferred")
  i_left <- 1
  i_right <- i_preferred - 1
  i_ratings <- (i_left + 1):(i_right - 1)

  # hence high matches represent positive relatedness
  for (i in seq_along(x$preferred)) {
    p <- x$preferred[i]
    if (isTRUE(p == 0)) {
      x[i, i_ratings] <- (as.numeric(x[i, i_ratings]) * -1 + 1)
      tmp <- x[i, i_left]
      x[i, i_left] <- x[i, i_right] # change pole labels
      x[i, i_right] <- tmp
      x$preferred[i] <- 1
    }
  }
  x
}


#' Calculate similarity matrix
#'
#' @param x Grid data.
#' @param min_matches Minimal number of matches to considers constructs as related.
#' @param align_poles Align positive poles on the right and negative poles on the left.
#' @export
#'
calculate_similarity <- function(x, min_matches = 6, align_poles = TRUE) # , use_labels = FALSE)
{
  i_preferred <- which(names(x) == "preferred")
  i_left <- 1
  i_right <- i_preferred - 1
  i_ratings <- (i_left + 1):(i_right - 1)

  # align constructs so all preferred poles are on right side
  # hence high matches represent positive relatedness
  if (align_poles) {
    x <- align_positive_poles(x)
  }

  s <- x[, i_ratings] # remove construct poles
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
  nr <- nrow(S)
  R_01 <- R <- D <- COL <- MM <- Mi <- M <- matrix(NA, nr, nr)
  for (i in 1L:nr) {
    for (j in 1L:nr) {
      c_i <- S[i, ]
      c_j <- S[j, ]
      M[i, j] <- count_matches(c_i, c_j) # matches
      Mi[i, j] <- count_matches(c_i, c_j, inverse = TRUE) # mismatches
    }
  }

  # keep bigger relatedness score
  M_vec <- as.vector(M)
  Mi_vec <- as.vector(Mi)
  R[, ] <- ifelse(M_vec > Mi_vec, M_vec, Mi_vec)
  diag(R) <- NA

  diag(M) <- NA # exlude match to self
  diag(Mi) <- NA # exlude match to self

  # mark positive and negative relatedness
  ii_m <- M >= criterion_
  ii_mi <- Mi >= criterion_
  ii <- ii_m | ii_mi # min of n identical responses (match or mismatch)?
  D[ii_m] <- 1
  D[ii_mi] <- -1

  # experiments in visualization
  MM[, ] <- 0
  MM[ii] <- 1
  R_01[, ] <- as.numeric(R >= criterion_)

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

  list(
    R = R, # no of matches (inclucing optional construct reversal, i.e. only high no. of matches relevant
    M = M, # matrix of matches without optional construct reversal, as described in paper, i.e. a very low and very high number of matches relevant
    MM = MM, # relatedness 0/1
    D = D, # direction of relation -1/1
    constructs = constructs,
    pole_left = pole_left,
    pole_right = pole_right,
    valence_left = valence_left,
    valence_right = valence_right
  )
}


#' Replace left or right pole of constructs with blanks
#'
#' To display the two poles of the constructs as vertex labels with different
#' colors, the poles have to be printed twice. The helper function replaces all letters
#' from one of the poles with blanks.
#' @param x Character vector of constructs with `@` sign used to separate construct poles.
#' @param first Replace first pole (`TRUE`) or second pole (`FALSE`)
#' @rdname replace-poles
#' @keywords internal
replace_one <- function(x, first = TRUE) {
  o <- str_split(x, "@")[[1]]
  r <- str_replace_all(o, ".", " ") # constructs poles are expected to be separated by @-sign
  if (first) {
    w <- paste0(o[1], "-", r[2])
  } else {
    w <- paste0(r[1], " ", o[2])
  }
  w
}

replace_all_ <- Vectorize(replace_one, USE.NAMES = FALSE)

#' @rdname replace-poles
#' @examples OpenRepGrid.ic:::replace_all("left pole @ right pole", first = TRUE)
replace_all <- function(x, first = TRUE) {
  if (length(x) == 0) { # catch length 0 error
    return(x)
  }
  replace_all_(x, first = first)
}


#' Generate colors for cliques
#'
#' @param n Number of colors.
#' @param name Name of RColorBrewer qualitative palette.
#' @param alpha Alpha color value for fill colors.
#' @return A list with a vector of border and fill colors.
#' @keywords internal
#' @md
clique_color_pals <- function(n, name = "Dark2", alpha = .1) {
  if (n == 0) {
    l <- list(
      border = NA,
      fill = NA
    )
    return(l)
  }

  pals <- RColorBrewer::brewer.pal.info
  n_max <- pals[name, ]$maxcolors # max umber of avaiabke colors in palette
  cols <- RColorBrewer::brewer.pal(n_max, name) # build palette
  cols <- rep(cols, n %/% n_max + 1) # repeat palette in case more colors needed than contained in palette

  border_colors <- cols[1L:n]
  list(
    border = border_colors,
    fill = scales::alpha(border_colors, alpha = alpha)
  )
}


#' Add a border around an image
#'
#' Creates border if a color is supplied.
#' @keywords internal
#'
add_image_border <- function(color = NA) {
  if (is.null(color) || is.na(color)) {
    return(NULL)
  }
  graphics::box(which = "outer", lty = "solid", col = color)
}


prep_label <- function(x, label_max_length = -1) {
  str_trim(x) %>% str_sub(start = 1, end = label_max_length)
}


#' Prefix the pole label with +/- for indicate pole valence
#' @keywords internal
#'
valence_pole_prefix <- function(x, valence, prefix = "(", postfix = ")") {
  postfix <- paste0(postfix, "\u00A0") # add blank at end of postfix
  valence_string <- recode(valence, `1` = "+", `-1` = "-", .default = "")
  # valence_string <- recode(valence, `1` = "\u2295", `-1` = "\u2296", .default = "")
  paste0(prefix, valence_string, postfix, x)
}


#' Build network graph plots
#'
#' Detects maximal cliques and saves images of network graphs into tempfile.
#' Tempfile paths and info on cliques are returned.
#'
#' @param x A dataframe with a grid.
#' @param min_clique_size Minimal size of cliques to be considered.
#' @param show_edges Whether to show edges in plot.
#' @param min_matches Minimal number of matching scores between constructs to be
#'   marked as related.
#' @param label_wrap_width Width of wrapped element label text.
#' @param label_max_length Trim element label at max length characters.
#' @param indicate_direction,colorize_direction Indicate direction of
#'   relatedness by edge label \code{+/-} and edge color (red, green). Only
#'   applied if `show_edges = TRUE`.
#' @param colorize_cliques Draw cliques in different colors? (default `TRUE`).
#' @param colorize_poles Colorize positive/negative/neutral poles as red, green,
#'   and gray respectively (default `TRUE`).
#' @param align_poles Align preferred poles on the same side.
#' @param valence_prefix Add (+/-) pole prefix to indicate preference. Empty
#'   means no preference.
#' @param alpha Alpha color value for cliques fillings (default `.1`).
#' @param border_default,fill_default Default border and fill color of polygon
#'   encircling clique constructs. Used when `colorize_cliques` is `FALSE`. Use
#'   \code{NA} for no color.
#' @param image_border_color Color of border around generated graph images. If
#'   `NULL` or `NA` no border is drawn.
#' @param seed Seed number passed to \code{set.seed}. Will determine the
#'   orientation of the graph.
#' @export
#' @md
#'
network_graph_images <- function(x,
                                 min_clique_size = 3,
                                 show_edges = TRUE,
                                 min_matches = 6,
                                 # label_abbreviate = FALSE,
                                 label_wrap_width = 15,
                                 label_max_length = -1,
                                 indicate_direction = show_edges,
                                 colorize_direction = TRUE,
                                 colorize_cliques = TRUE,
                                 colorize_poles = TRUE,
                                 align_poles = TRUE,
                                 alpha = .1,
                                 valence_prefix = FALSE,
                                 border_default = "#987824",
                                 fill_default = "#00000008",
                                 image_border_color = grey(.6),
                                 seed = 0) {
  img_par <- list(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) # par settings for images

  l <- calculate_similarity(x, min_matches = min_matches, align_poles = align_poles) # , use_labels = FALSE)
  MM <- l$MM
  D <- l$D
  edge.lty <- ifelse(show_edges, 2, 0)
  cnames <- l$constructs
  valence_left <- l$valence_left
  valence_right <- l$valence_right

  if (valence_prefix) {
    l$pole_left <- valence_pole_prefix(l$pole_left, l$valence_left)
    l$pole_right <- valence_pole_prefix(l$pole_right, l$valence_right)
  }

  g <- igraph::graph_from_adjacency_matrix(MM, diag = FALSE, mode = "undirected")
  mc <- igraph::max_cliques(g, min = min_clique_size)
  n_clique <- length(mc)

  clique_lists <- lapply(mc, attr, "names")
  clique_lists_full_names <- lapply(clique_lists, function(x) {
    cnames[x]
  })

  # clique colors
  if (colorize_cliques) {
    pals <- clique_color_pals(n_clique, "Dark2", alpha = alpha)
    mark_border <- pals$border
    mark_col <- pals$fill
  } else {
    mark_border <- border_default
    mark_col <- fill_default
  }


  ## show all constructs

  nms_keep <- clique_lists %>%
    unlist() %>%
    unique()
  MM2 <- MM[nms_keep, nms_keep]

  # colorize edges by direction
  edges <- ends(g, E(g)) # edge from to as rowwise matrix
  edge_directions <- D[edges]
  if (indicate_direction && show_edges) {
    edge_labels <- recode(edge_directions, `1` = "+", `-1` = "\u2013")
  } else {
    edge_labels <- NULL
  }

  if (colorize_direction) {
    edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red", .default = "grey")
    edge_label_colors <- edge_colors
  } else {
    edge_colors <- grey(.5)
    edge_label_colors <- grey(.2)
  }
  E(g)$color <- edge_colors


  ## __ all - abbreviated   ----------------------------------------------

  vertex.labels <- NULL
  vertex.size <- 15
  vertex.label.cex <- 1

  img_all_constructs <- tempfile(fileext = ".png")
  png(img_all_constructs, width = 20, height = 20, units = "cm", res = 300)
  # par(oma = c(0,0,0,0), mar = c(0,0,0,0))   # can be left out for now but kept as a reminder comment for an alternative approach
  with_par(img_par, {
    set.seed(seed)
    igraph::plot.igraph(g,
      mark.groups = clique_lists,
      mark.border = mark_border,
      mark.col = mark_col,
      mark.expand = 15,
      edge.arrow.size = 0,
      edge.lty = edge.lty,
      edge.width = 1,
      edge.label = edge_labels,
      edge.label.color = edge_label_colors,
      edge.label.font = 2,
      vertex.size = vertex.size,
      vertex.size2 = vertex.size,
      vertex.label = vertex.labels,
      vertex.label.color = "black",
      vertex.label.cex = vertex.label.cex,
      vertex.label.family = "sans",
      vertex.color = grey(.9),
      vertex.frame.color = grey(.5)
    )
  })
  add_image_border(image_border_color)
  dev.off()


  # __ all - full labels ----------------------------------------------

  cnames <- paste(
    prep_label(l$pole_left, label_max_length = label_max_length), "-",
    prep_label(l$pole_right, label_max_length = label_max_length)
  )
  names(cnames) <- names(l$constructs)

  cns <- V(g)$name
  vertex.labels <-
    cnames[cns] %>%
    # str_sub(start = 1, end = label_max_length) %>%
    str_wrap(width = label_wrap_width)
  vertex.size <- 22
  vertex.label.cex <- .6

  img_all_constructs_full_labels <- tempfile(fileext = ".png")
  png(img_all_constructs_full_labels, width = 20, height = 20, units = "cm", res = 300)
  # par(oma = c(0,0,0,0), mar = c(0,0,0,0))   # can be left out for now but kept as a reminder comment for an alternative approach
  with_par(img_par, {
    set.seed(seed)
    igraph::plot.igraph(g,
      mark.groups = clique_lists,
      mark.border = mark_border,
      mark.col = mark_col,
      mark.expand = 15,
      edge.arrow.size = 0,
      edge.lty = edge.lty,
      edge.width = 1,
      edge.label = edge_labels,
      edge.label.color = edge_label_colors,
      edge.label.font = 2,
      vertex.size = vertex.size,
      vertex.size2 = vertex.size,
      vertex.label = vertex.labels,
      vertex.label.color = "black",
      vertex.label.cex = vertex.label.cex,
      vertex.label.family = "sans",
      vertex.color = grey(.9),
      vertex.frame.color = grey(.5)
    )
  })
  add_image_border(image_border_color)
  dev.off()


  ## __ all - separate poles  ----------------------------------------------

  label_wrap_width <- 14
  cnames <- paste(
    prep_label(l$pole_left, label_max_length = label_max_length), "@",
    prep_label(l$pole_right, label_max_length = label_max_length)
  )
  names(cnames) <- names(l$constructs)

  cns <- V(g)$name
  ii_keep <- match(cns, names(cnames))
  vertex.labels <-
    cnames[cns] %>%
    str_wrap(width = label_wrap_width)
  vertex.size <- 22
  vertex.label.cex <- .5
  edge_labels <- NULL

  # find vertexes with negative relations only => we need to separate by direction
  D2 <- D[ii_keep, ii_keep]
  vertex_relations <- apply(D2, 2, function(x) {
    v <- x %>%
      na.omit() %>%
      unique()
    n <- length(v)
    case_when(
      n == 0 ~ "none",
      n == 2 ~ "mixed",
      n == 1 && v == 1 ~ "pos",
      n == 1 && v == -1 ~ "neg"
    )
  })
  names(vertex_relations) <- cns
  vertex_font_pole_1 <- 1 # recode(vertex_relations, "neg" = 2, .default = 1)
  vertex_font_pole_2 <- 1 # recode(vertex_relations, "neg" = 1, .default = 2)

  vertex.labels1 <- replace_all(vertex.labels, first = TRUE)
  vertex.labels2 <- replace_all(vertex.labels, first = FALSE)

  # colorize edges by direction
  edges <- ends(g, E(g)) # edge from to as rowwise matrix
  edge_directions <- D[edges]
  if (indicate_direction && show_edges) {
    edge_labels <- recode(edge_directions, `1` = "+", `-1` = "\u2013")
  } else {
    edge_labels <- NULL
  }

  if (colorize_direction) {
    edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red", .default = "grey")
    edge_label_colors <- edge_colors
  } else {
    edge_colors <- grey(.5)
    edge_label_colors <- grey(.2)
  }
  E(g)$color <- edge_colors

  if (colorize_poles) {
    colors_poles_left <- recode(valence_left, `1` = "darkgreen", `-1` = "red", .default = grey(.2))
    colors_poles_right <- recode(valence_right, `1` = "darkgreen", `-1` = "red", .default = grey(.2))
  } else {
    colors_poles_left <- grey(.2)
    colors_poles_right <- grey(.2)
  }

  img_all_constructs_separate_poles <- tempfile(fileext = ".png")
  png(img_all_constructs_separate_poles, width = 20, height = 20, units = "cm", res = 300)

  # we need two superimposed plots here (hence 2 x same seed) in order to achieve
  # a different font face for each of the pole
  with_par(img_par, {
    set.seed(seed)
    igraph::plot.igraph(g,
      mark.groups = clique_lists,
      mark.border = mark_border,
      mark.col = mark_col,
      mark.expand = 15,
      edge.arrow.size = 0,
      edge.lty = edge.lty,
      edge.width = 1,
      edge.label = edge_labels,
      edge.label.color = edge_label_colors,
      edge.label.font = 2,
      vertex.size = vertex.size,
      vertex.size2 = vertex.size,
      vertex.label = vertex.labels1,
      vertex.label.color = colors_poles_left, # "black",
      vertex.label.cex = vertex.label.cex,
      vertex.label.family = "mono",
      vertex.label.font = vertex_font_pole_1,
      vertex.color = grey(.9),
      vertex.frame.color = grey(.5)
    )

    set.seed(seed)
    igraph::plot.igraph(g,
      add = TRUE,
      mark.groups = NULL,
      mark.border = NA,
      mark.col = NA,
      mark.expand = 15,
      edge.arrow.size = 0,
      edge.lty = edge.lty,
      edge.width = NA,
      edge.label = edge_labels,
      edge.label.color = edge_label_colors,
      edge.label.font = 2,
      vertex.size = vertex.size,
      vertex.label = vertex.labels2,
      vertex.size2 = vertex.size,
      vertex.label.color = colors_poles_right,
      vertex.label.cex = vertex.label.cex,
      vertex.label.family = "mono",
      vertex.label.font = vertex_font_pole_2,
      vertex.color = "transparent",
      vertex.frame.color = grey(.5)
    )
  })
  add_image_border(image_border_color)
  dev.off()


  ## __ cliques - abbreviated  ----------------------------------------------

  g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = FALSE, mode = "undirected")

  # colorize edges by direction
  edges <- ends(g2, E(g2)) # edge from to as rowwise matrix
  edge_directions <- D[edges]
  if (indicate_direction && show_edges) {
    edge_labels <- recode(edge_directions, `1` = "+", `-1` = "\u2013")
  } else {
    edge_labels <- NULL
  }

  if (colorize_direction) {
    edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red", .default = "grey")
    edge_label_colors <- edge_colors
  } else {
    edge_colors <- grey(.5)
    edge_label_colors <- grey(.2)
  }
  E(g2)$color <- edge_colors

  cns <- V(g2)$name
  vertex.labels <-
    cnames[cns] %>%
    str_sub(start = 1, end = label_max_length) %>%
    str_wrap(width = label_wrap_width)

  # abbreviated construct labels
  vertex.labels <- NULL
  vertex.size <- 30
  vertex.label.cex <- 1

  img_cliques_only <- tempfile(fileext = ".png")
  png(img_cliques_only, width = 20, height = 20, units = "cm", res = 300)
  with_par(img_par, {
    # par(oma = c(0,0,0,0), mar = c(0,0,0,0))  # can be left out for now but kept as a reminder comment for an alternative approach
    set.seed(seed)
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
        edge.label.color = edge_label_colors,
        edge.label.font = 2,
        vertex.size = vertex.size,
        vertex.label = vertex.labels,
        vertex.size2 = vertex.size,
        vertex.label.color = "black",
        vertex.label.cex = vertex.label.cex,
        vertex.label.family = "sans",
        vertex.color = grey(.9),
        vertex.frame.color = grey(.5)
      )
    } else {
      plot.new()
      text(.5, .5, "No cliques detected", cex = 1.5, adj = c(.5, .5))
    }
  })
  add_image_border(image_border_color)
  dev.off()


  # __ cliques - full labels  ----------------------------------------------

  cnames <- paste(
    prep_label(l$pole_left, label_max_length = label_max_length), "-",
    prep_label(l$pole_right, label_max_length = label_max_length)
  )
  names(cnames) <- names(l$constructs)

  cns <- V(g2)$name
  vertex.labels <-
    cnames[cns] %>%
    str_wrap(width = label_wrap_width)
  vertex.size <- 30
  vertex.label.cex <- .6

  img_cliques_only_full_labels <- tempfile(fileext = ".png")

  png(img_cliques_only_full_labels, width = 20, height = 20, units = "cm", res = 300)
  with_par(img_par, {
    set.seed(seed)
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
        edge.label.color = edge_label_colors,
        edge.label.font = 2,
        vertex.size = vertex.size,
        vertex.label = vertex.labels,
        vertex.size2 = vertex.size,
        vertex.label.color = "black",
        vertex.label.cex = vertex.label.cex,
        vertex.label.family = "sans",
        vertex.color = grey(.9),
        vertex.frame.color = grey(.5)
      )
    } else {
      plot.new()
      text(.5, .5, "No cliques detected", cex = 1.5, adj = c(.5, .5))
    }
  })
  add_image_border(image_border_color)
  dev.off()


  ## __ cliques - separate poles  ----------------------------------------------

  g2 <- igraph::graph_from_adjacency_matrix(MM2, diag = FALSE, mode = "undirected")

  label_wrap_width <- 14
  cnames <- paste(
    prep_label(l$pole_left, label_max_length = label_max_length), "@",
    prep_label(l$pole_right, label_max_length = label_max_length)
  )
  names(cnames) <- names(l$constructs)

  # full labels
  cns <- V(g2)$name
  ii_keep <- match(cns, names(cnames))
  vertex.labels <-
    cnames[cns] %>%
    str_wrap(width = label_wrap_width - 1)
  vertex.size <- 30
  vertex.label.cex <- .5
  edge_labels <- NULL

  # find vertexes with negative relations only
  D2 <- D[ii_keep, ii_keep]
  vertex_relations <- apply(D2, 2, function(x) {
    v <- x %>%
      na.omit() %>%
      unique()
    n <- length(v)
    case_when(
      n == 0 ~ "none",
      n == 2 ~ "mixed",
      n == 1 && v == 1 ~ "pos",
      n == 1 && v == -1 ~ "neg"
    )
  })

  names(vertex_relations) <- cns
  vertex_font_pole_1 <- 1 # recode(vertex_relations, "neg"= 2, .default = 1)  # can be left out for now but kept as a reminder comment for an alternative approach
  vertex_font_pole_2 <- 2 # recode(vertex_relations, "neg"= 1, .default = 2)  # can be left out for now but kept as a reminder comment for an alternative approach

  vertex.labels1 <- replace_all(vertex.labels, first = TRUE)
  vertex.labels2 <- replace_all(vertex.labels, first = FALSE)

  # colorize edges by direction
  edges <- ends(g2, E(g2)) # edge from to as rowwise matrix
  edge_directions <- D[edges]
  if (indicate_direction && show_edges) {
    edge_labels <- recode(edge_directions, `1` = "+", `-1` = "\u2013")
  } else {
    edge_labels <- NULL
  }

  if (colorize_direction) {
    edge_colors <- recode(edge_directions, `1` = "darkgreen", `-1` = "red", .default = "grey")
    edge_label_colors <- edge_colors
  } else {
    edge_colors <- grey(.5)
    edge_label_colors <- grey(.2)
  }
  E(g2)$color <- edge_colors

  if (colorize_poles) {
    colors_poles_left <- recode(valence_left[ii_keep], `1` = "darkgreen", `-1` = "red", .default = grey(.2))
    colors_poles_right <- recode(valence_right[ii_keep], `1` = "darkgreen", `-1` = "red", .default = grey(.2))
  } else {
    colors_poles_left <- grey(.2)
    colors_poles_right <- grey(.2)
  }

  img_cliques_only_separate_poles <- tempfile(fileext = ".png")
  with_par(img_par, {
    png(img_cliques_only_separate_poles, width = 20, height = 20, units = "cm", res = 300)
    # we need two superimposed plots here (hence 2 x seed) in order to achieve
    # a different font face for the poles
    set.seed(seed)
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
        edge.label.color = edge_label_colors,
        edge.label.font = 2,
        vertex.size = vertex.size,
        vertex.size2 = vertex.size,
        vertex.label = vertex.labels1,
        vertex.label.color = colors_poles_left,
        vertex.label.cex = vertex.label.cex,
        vertex.label.family = "mono",
        vertex.label.font = vertex_font_pole_1,
        vertex.color = grey(.9),
        vertex.frame.color = grey(.5)
      )

      set.seed(seed)
      igraph::plot.igraph(g2,
        add = TRUE,
        mark.groups = NULL,
        mark.border = NA,
        mark.col = NA,
        mark.expand = 15,
        edge.arrow.size = 0,
        edge.lty = edge.lty,
        edge.width = NA,
        edge.label = edge_labels,
        edge.label.color = edge_label_colors,
        edge.label.font = 2,
        vertex.size = vertex.size,
        vertex.label = vertex.labels2,
        vertex.size2 = vertex.size,
        vertex.label.color = colors_poles_right,
        vertex.label.cex = vertex.label.cex,
        vertex.label.family = "mono",
        vertex.label.font = vertex_font_pole_2,
        vertex.color = "transparent",
        vertex.frame.color = grey(.5)
      )
    } else {
      plot.new()
      text(.5, .5, "No cliques detected", cex = 1.5, adj = c(.5, .5))
    }
    add_image_border(image_border_color)
    dev.off()
  })

  # return image paths and other info as list
  l2 <- list(
    img_all_constructs = img_all_constructs,
    img_all_constructs_full_labels = img_all_constructs_full_labels,
    img_all_constructs_separate_poles = img_all_constructs_separate_poles,
    img_cliques_only = img_cliques_only,
    img_cliques_only_full_labels = img_cliques_only_full_labels,
    img_cliques_only_separate_poles = img_cliques_only_separate_poles,
    min_clique_size = min_clique_size,
    cliques_list = clique_lists,
    clique_lists_full_names = clique_lists_full_names,
    min_matches = min_matches
  )
  append(l, l2)
}
