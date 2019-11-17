

#' Check if Excel input file contains valid data
#' 
#' @param x Data from Excel input file.
#' @export
#' 
check_excel_input <- function(x) 
{
  nms <- names(x)
  
  nc <- ncol(x)
  i_left <- 1L
  i_preferred <- nc - 0
  i_right <- nc - 1
  i_ratings <- 2L:(nc - 2)
  
  # preferred --
  
  c1_res <- isTRUE(tail(nms, 1) == "preferred")
  c1 <- list(
    assert = "rightmost column is named 'preferred'",
    passed = c1_res,
    error = ifelse(c1_res, "", "Rightmost column must be named 'preferred'")
  )

  c2_res <- all(x$preferred %in% c(0, 1, NA))
  c2 <- list(
    assert = "column 'preferred' contains values 0, 1, NA or empty cells",
    passed = c2_res,
    error = ifelse(c2_res, "", "Column 'preferred' must only contain the values: 0, 1, NA or empty cells")
  )
  
  # ratings --
  
  ratings <- x[, i_ratings] %>% unlist %>% unname
  c3_res <- all(ratings %in% c(0, 1, NA))
  c3 <- list(
    assert = "ratings contain values 0, 1, NA or empty cells",
    passed = c3_res,
    error = ifelse(c3_res, "", "Ratings must only contain the values: 0, 1, NA or empty cells")
  )
  
  # constructs --
  
  c_left <- x[[i_left]] %>% str_trim
  c_right <- x[[i_right]] %>% str_trim
  c4_res <- all(c_left != "") && all(c_right != "")
  c4 <- list(
    assert = "constructs are non-empty strings",
    passed = c4_res,
    error = ifelse(c4_res, "", "Some construct poles contain empty strings")
  )
  
  # elements --
  
  elements <- names(x)[i_ratings] %>% str_trim
  c_right <- x[[i_right]] %>% str_trim
  c5_res <- all(elements != "") || any(duplicated(elements))
  c5 <- list(
    assert = "all element names are unique and non-empty strings",
    passed = c5_res,
    error = ifelse(c5_res, "", "Some element names are not unique or contain non-empty string")
  )
  
  # meta --
  rating_lowest <- names(x)[i_left] %>% as.numeric
  rating_highest <- names(x)[i_right] %>% as.numeric
  c6_res <- (rating_lowest < rating_highest) || is.na(rating_lowest) || is.na(rating_highest)
  c6 <- list(
    assert = "meta data for highest and lowest ratuing values is given",
    passed = c6_res,
    error = ifelse(c6_res, "", "The highest lowest possible rating values must be given in the column name of the left and right pole.")
  )
  
  l <- 
    list(c1, c2, c3, c4, c5, c6) %>%
    lapply(as.data.frame)
  do.call(rbind, l)
  
  #lapply(l, `class<-`, c("test", "list"))
}


# print.test <- function(x)
# {
#   str(x)  
# }


#' Create output Excel file
#' 
#' Loads the supplied workbook and adds calculations
#' @export
#' @param file Path to workbook.
#' @param data Named list of data objects to add to Excel file.
#'  The following contents are expected: TODO
#' @return Path to saved file.
#' 
create_excel_output <- function(file, data = list()) 
{
  wb <- loadWorkbook(file)
  nms <- names(wb)
  
  R <- data$R
  D <- data$D
  cliques_list <- data$cliques_list
  img_all_constructs <- data$img_all_constructs
  img_cliques_only <- data$img_cliques_only
  min_clique_size <- data$min_clique_size
  min_matches <- data$min_matches
  
  # Styles ----

  style_h1 <- createStyle(fontColour = "#000000", fontSize = 14,
                          fgFill = "#cccccc",
                          halign = "left", valign = "center", 
                          textDecoration = "bold")
  style_italic <- createStyle(textDecoration = "italic")
  style_bold <- createStyle(textDecoration = "bold")
  
  # Calculations ----
  
  removeWorksheet 
  
  sheet <- "calculations"
  if (isTRUE(sheet %in% nms))
    removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet)
  
  writeData(wb, sheet, "Relatedness scores", startRow = 1, startCol = 1)
  writeData(wb, sheet, R, startRow = 3, startCol = 1, colNames = TRUE, rowNames = TRUE)
  
  i <- 3 + nrow(R) + 2
  txt <- paste0("Relatedness by criterion (score >= ", min_matches, ")")
  writeData(wb, sheet, txt, startRow = i, startCol = 1)
  writeData(wb, sheet, D, startRow = i + 2, startCol = 1, colNames = TRUE, rowNames = TRUE)
  setColWidths(wb, sheet, cols = 2L:(ncol(R) + 1), widths = "auto")
  setColWidths(wb, sheet, cols = 1, widths = 8)
  
  
  # Clique enumeration ----
  
  sheet <- "cliques"
  if (isTRUE(sheet %in% nms))
    removeWorksheet(wb, sheet)
  addWorksheet(wb, sheet)
  writeData(wb, sheet, "Clique identification", startRow = 1, startCol = 1)
  addStyle(wb, sheet, style_h1, rows = 1, cols = 1:100, gridExpand = T)
  
  intro <-   
    c("The results of the maximal clique identification are shown below.",
    "A clique is a subgraph in which all vertexes (here constructs) are connected to each other.",
    "A clique is maximal if it is not part of a bigger clique.",
    paste0("The minimal size of a cliques, ie. the minimal number of constructs that can form a clique, has been set to ", min_clique_size, "."),
    paste0("The minimal number of matching or inversely matching rating scores for two constructs to be considered 'related' has been set to ", min_matches, "."))
  
  for (i in seq_along(intro)) {
    writeData(wb, sheet, intro[i], startRow = i + 2, startCol = 1)  
    addStyle(wb, sheet, style_italic, rows = i + 2, cols = 1)  
  }
  
  writeData(wb, sheet, "Clique Nr.", startRow = 9, startCol = 1)
  addStyle(wb, sheet, style_bold, rows = 9, cols = 1)  
  for (i in seq_along(cliques_list)) {
    cc <- cliques_list[[i]]
    writeData(wb, sheet, i, startRow = 9 + i, startCol = 1)
    for (j in seq_along(cc)) {
      writeData(wb, sheet, cc[j], startRow = 9 + i, startCol = 1 + j)  
    }
  }
  
  # Network graphs ----
  
  i2 <- 11 + i
  start_col <- 1
  writeData(wb, sheet, "Figure 1: Network diagram for all constructs", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col) 
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col) 
  insertImage(wb, sheet, img_all_constructs, width = 15, height = 15, units = "cm", startRow = i2 + 4, startCol = start_col)
  
  i2 <- 11 + i
  start_col <- 11
  writeData(wb, sheet, "Figure 2: Network diagram for constructs inside cliques only", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col) 
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col) 
  insertImage(wb, sheet, img_cliques_only, width = 15, height = 15, units = "cm", startRow = i2 + 4, startCol = start_col)
  
  tmp_file <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp_file, overwrite = TRUE)
  tmp_file
  
}




