# /////////////////////////////////////////////////////////////////////////////////////
#
#                                 Excel output
#
# /////////////////////////////////////////////////////////////////////////////////////


#' Check if Excel input file contains valid data
#'
#' @param x Data from Excel input file.
#' @export
#'
check_excel_input_test <- function(x) {
  nms <- names(x)
  nc <- ncol(x)
  i_left <- 1L
  i_preferred <- nc - 0
  i_right <- nc - 1
  i_ratings <- 2L:(nc - 2)

  # preferred --

  col_right <- utils::tail(nms, 1)
  c1_res <- isTRUE(col_right == "preferred")
  c1 <- list(
    assert = "Rightmost column is named 'preferred' (must be lowercase letters)",
    passed = c1_res,
    error = ifelse(c1_res, "", paste0("Currently, the rightmost column is named '", col_right, "'"))
  )

  ii <- x$preferred %in% c(0, 1, NA)
  not_allowed <- x$preferred[!ii] %>%
    unique() %>%
    head(10)
  c2_res <- all(ii)
  c2 <- list(
    assert = "Column 'preferred' only contains one of the following values: 0, 1, NA, empty cell",
    passed = c2_res,
    error = ifelse(c2_res, "", paste("The following values are not allowed:", paste(not_allowed, collapse = ", ")))
  )

  # ratings --

  ratings <- x[, i_ratings] %>%
    unlist() %>%
    unname()
  ii <- ratings %in% c(0, 1, NA)
  not_allowed <- ratings[!ii] %>%
    unique() %>%
    head(10)
  c3_res <- all(ii)
  c3 <- list(
    assert = "All ratings have one of the following values: 0, 1, NA, empty cell",
    passed = c3_res,
    error = ifelse(c3_res, "", paste("The following values are not allowed:", paste(not_allowed, collapse = ", ")))
  )

  # constructs --

  c_left <- stringr::str_trim(x[[i_left]]) %>% tail(-1)
  c_right <- stringr::str_trim(x[[i_right]]) %>% tail(-1)
  c4_res <- all(c_left != "") && !any(is.na(c_left))
  c4 <- list(
    assert = "Left poles start in column 1, row 2 and must all be non-empty strings (right poles may contain empty strings, though this is not recommended)",
    passed = c4_res,
    error = ifelse(c4_res, "", "Some left poles contain empty strings")
  )

  # elements --

  elements <- stringr::str_trim(names(x)[i_ratings])
  ii <- duplicated(elements)
  c5_res <- all(!ii)
  dupes <- elements[ii]
  c5 <- list(
    assert = "All element names must be unique",
    passed = c5_res,
    error = ifelse(c5_res, "", paste("The following element names are not unique:", paste(dupes, collapse = ", ")))
  )

  elements <- stringr::str_trim(names(x)[i_ratings])
  ii <- elements != "" | is.na(elements)
  c6_res <- all(ii)
  c6 <- list(
    assert = "All element names are non-empty strings and not missing",
    passed = c6_res,
    error = ifelse(c6_res, "", paste("The element names in the following columns are not correct:", paste(int2col(which(ii) + 1), collapse = ", ")))
  )

  # meta --

  rating_lowest <- names(x)[i_left] %>% as.numeric()
  rating_highest <- names(x)[i_right] %>% as.numeric()
  c7_res <- !((rating_lowest > rating_highest) || is.na(rating_lowest) || is.na(rating_highest))
  c7 <- list(
    assert = "Meta data for highest and lowest rating values is given",
    passed = c7_res,
    error = ifelse(c7_res, "", "The highest lowest possible values are missing or not plausible.")
  )

  l <- list(c1, c2, c3, c4, c5, c6, c7) %>%
    lapply(as.data.frame)
  tests <- do.call(rbind, l)

  # if some test cannot be properly executed and yield NA e.g. because of some data oddity,
  # we throw a general fallback error
  use_fallback <- any(is.na(tests$passed))
  if (use_fallback) {
    fallback_error <- data.frame(
      assert = "All grid format tests work.",
      passed = FALSE,
      error = "One or more Excel format tests could not be executed. The reason is unknown. Please double check the Excel grid format for correctness."
    )
    return(fallback_error)
  }

  tests
}


#' Check if Excel input file contains valid data
#'
#' @param x Data from Excel input file.
#' @export
#'
check_excel_input <- function(x) {
  suppressWarnings({ # so that NA coercions do not raise unwanted warnings
    tests <- tryCatch(
      check_excel_input_test(x),
      error = function(e) {
        data.frame(
          assert = "Excel file tests can be executed without error.",
          passed = FALSE,
          error = "When testing your Excel file format for correctness, the program crashed. The reason is unknown. Most likely, there is a problem in the Excel file you uploaded. Please check the Excel grid format for correctness."
        )
      }
    )
  })
  tests
}


#' Create output Excel file
#'
#' Loads the supplied workbook and adds calculations
#' @export
#' @param file Path to workbook.
#' @param data Named list of data objects to add to Excel file.
#'  The following contents are expected: TODO
#' @return Path to saved file.
#'
create_excel_output <- function(file, data = list()) {
  wb <- loadWorkbook(file)
  nms <- names(wb)
  constructs_df <- data.frame("constructs" = data$constructs, stringsAsFactors = FALSE)
  R <- data$R
  D <- data$D # matrix with matches (with optional construct reversal)
  M <- data$M # matrix with matches (no construct reversal)
  cliques_list <- data$cliques_list
  clique_lists_full_names <- data$clique_lists_full_names

  img_all_constructs <- data$img_all_constructs
  img_all_constructs_full_labels <- data$img_all_constructs_full_labels
  img_all_constructs_separate_poles <- data$img_all_constructs_separate_poles
  img_cliques_only <- data$img_cliques_only
  img_cliques_only_full_labels <- data$img_cliques_only_full_labels
  img_cliques_only_separate_poles <- data$img_cliques_only_separate_poles

  min_clique_size <- data$min_clique_size
  min_matches <- data$min_matches

  # Styles -----------------------------------------------

  style_h1 <- createStyle(
    fontColour = "#000000", fontSize = 14,
    fgFill = "#cccccc",
    halign = "left", valign = "center",
    textDecoration = "bold"
  )
  style_italic <- createStyle(textDecoration = "italic")
  style_right <- createStyle(halign = "right")
  style_bold <- createStyle(textDecoration = "bold")
  neg_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  pos_style <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

  # Method description ------------------------------------------

  # add method explanation found on https://hhs.hud.ac.uk/InterpretiveClustering/
  # to have all in one place
  sheet <- "method"
  if (isTRUE(sheet %in% nms)) {
    removeWorksheet(wb, sheet)
  }
  addWorksheet(wb, sheet)
  file <- system.file("extdata", "interpretive_clustering_method.png", package = "OpenRepGrid.ic")
  insertImage(wb, sheet, file, width = 1763 * 2, height = 5199 * 2, units = "px")


  # Calculations ------------------------------------------

  sheet <- "calculations"
  if (isTRUE(sheet %in% nms)) {
    removeWorksheet(wb, sheet)
  }
  addWorksheet(wb, sheet)

  # legends
  row <- 1
  writeData(wb, sheet, "Legend", startRow = row, startCol = 1)
  addStyle(wb, sheet, style = style_bold, rows = row, cols = 1)
  lgnd <- data.frame(
    text =
      c(
        "* Positive construct poles (as indicated by column 'preferred') are aligned on the right",
        "** 1 = positive relatedness, -1 = negative relatedness"
      ), stringsAsFactors = FALSE
  )
  writeData(wb, sheet, lgnd, startRow = row + 2, startCol = 1, colNames = FALSE, rowNames = FALSE)

  # matches (no construct reversal)
  row <- 6
  writeData(wb, sheet, "Number of matches between constructs (no construct reversal)*", startRow = row, startCol = 1)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = row, cols = 1)
  writeData(wb, sheet, constructs_df, startRow = row + 2, startCol = 1, colNames = TRUE, rowNames = FALSE)
  addStyle(wb, sheet, style = style_right, rows = row + 2, cols = 1)
  addStyle(wb, sheet, style = style_italic, rows = row + 2, cols = 1, stack = TRUE)
  addStyle(wb, sheet, style = style_right, gridExpand = TRUE, rows = 1L:nrow(R) + row + 2, cols = 1)
  writeData(wb, sheet, M, startRow = row + 2, startCol = 2, colNames = TRUE, rowNames = TRUE)

  # matches (with optional construct reversal)
  row <- row + nrow(M) + 4
  writeData(wb, sheet, "Number of matches between constructs (after optional construct reversal)*", startRow = row, startCol = 1)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = row, cols = 1)
  writeData(wb, sheet, constructs_df, startRow = row + 2, startCol = 1, colNames = TRUE, rowNames = FALSE)
  addStyle(wb, sheet, style = style_right, rows = row + 2, cols = 1)
  addStyle(wb, sheet, style = style_italic, rows = row + 2, cols = 1, stack = TRUE)
  addStyle(wb, sheet, style = style_right, gridExpand = TRUE, rows = 1L:nrow(R) + row + 2, cols = 1)
  writeData(wb, sheet, R, startRow = row + 2, startCol = 2, colNames = TRUE, rowNames = TRUE)

  # direction
  i <- row + nrow(R) + 4
  txt <- paste0("Relatedness by criterion (matches >= ", min_matches, " after optional construct reversal)**")
  writeData(wb, sheet, txt, startRow = i, startCol = 1)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i, cols = 1)
  writeData(wb, sheet, constructs_df, startRow = i + 2, startCol = 1, colNames = TRUE, rowNames = FALSE)
  addStyle(wb, sheet, style = style_right, rows = i + 2, cols = 1)
  addStyle(wb, sheet, style = style_italic, rows = i + 2, cols = 1, stack = TRUE)
  addStyle(wb, sheet, style = style_right, gridExpand = TRUE, rows = 1L:nrow(R) + i + 2, cols = 1)
  writeData(wb, sheet, D, startRow = i + 2, startCol = 2, colNames = TRUE, rowNames = TRUE)
  setColWidths(wb, sheet, cols = 1, widths = "60")
  setColWidths(wb, sheet, cols = 2L:(ncol(R) + 2), widths = "auto")
  conditionalFormatting(wb, sheet, cols = 1L:nrow(R) + 2, rows = 1L:nrow(R) + i + 2, rule = "<0", style = neg_style)
  conditionalFormatting(wb, sheet, cols = 1L:nrow(R) + 2, rows = 1L:nrow(R) + i + 2, rule = ">0", style = pos_style)


  # Clique enumeration ----------------------------------

  sheet <- "cliques"
  if (isTRUE(sheet %in% nms)) {
    removeWorksheet(wb, sheet)
  }
  addWorksheet(wb, sheet)
  writeData(wb, sheet, "Clique identification", startRow = 1, startCol = 1)
  addStyle(wb, sheet, style_h1, rows = 1, cols = 1:100, gridExpand = TRUE)

  intro <-
    c(
      "The results of the maximal clique identification are shown below.",
      "A clique is a subgraph in which all vertexes (here constructs) are connected to each other.",
      "A clique is maximal if it is not part of a bigger clique.",
      paste0("The minimal size of a cliques, ie. the minimal number of constructs that can form a clique, has been set to ", min_clique_size, "."),
      paste0("The minimal number of matching or inversely matching rating scores for two constructs to be considered 'related' has been set to ", min_matches, "."),
      "Positive poles are right-aligned, i.e. the second pole of the construct is always the preferred pole."
    )

  for (i in seq_along(intro)) {
    writeData(wb, sheet, intro[i], startRow = i + 2, startCol = 1)
    addStyle(wb, sheet, style_italic, rows = i + 2, cols = 1)
  }

  # Abbreviated constructs
  row <- 10
  writeData(wb, sheet, "Clique Nr.", startRow = row, startCol = 1)
  addStyle(wb, sheet, style_bold, rows = row, cols = 1)
  n_cliques <- length(cliques_list)
  if (n_cliques == 0) {
    cliques_list[[1]] <- c("No cliques detected")
  } # show this hint if not cliques were found
  for (i in seq_along(cliques_list)) {
    cc <- cliques_list[[i]]
    writeData(wb, sheet, i, startRow = row + i, startCol = 1)
    for (j in seq_along(cc)) {
      writeData(wb, sheet, cc[j], startRow = row + i, startCol = 1 + j)
    }
  }

  # Full construct labels per clique
  row <- 12 + i
  writeData(wb, sheet, "Clique Nr.", startRow = row, startCol = 1)
  writeData(wb, sheet, "Construct", startRow = row, startCol = 2)
  writeData(wb, sheet, "Label", startRow = row, startCol = 3)
  addStyle(wb, sheet, style_bold, rows = row, cols = 1:3)
  n_cliques <- length(clique_lists_full_names)
  if (n_cliques == 0) {
    clique_lists_full_names[[1]] <- c(" " = "No cliques detected")
  } # show this hint if not cliques were found

  for (i in seq_along(clique_lists_full_names)) {
    row <- row + 1
    cc <- clique_lists_full_names[[i]]
    n <- length(cc)
    writeData(wb, sheet, i, startRow = row, startCol = 1)
    for (j in seq_along(cc)) {
      row <- row + 1
      writeData(wb, sheet, names(cc[j]), startRow = row, startCol = 2)
      writeData(wb, sheet, cc[j], startRow = row, startCol = 3)
    }
  }

  # Network graphs -------------------------------------

  # image interpretation help

  row <- row + 2
  i2 <- row
  writeData(wb, sheet, "Interpretation help for network graphs", startRow = i2, startCol = 1)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = 1)
  row <- row + 2
  writeData(wb, sheet, "The following image explains all graphical features which may appear in the network images below.", startRow = i2 + 2, startCol = 1)
  file <- system.file("extdata", "legend_images.png", package = "OpenRepGrid.ic")
  insertImage(wb, sheet, file, width = 817 * 2, height = 752 * 2, units = "px", startRow = i2 + 4, startCol = 1)

  # row 1: full labels

  row <- row + 29

  i2 <- row
  start_col <- 1
  writeData(wb, sheet, "Figure 1a: Network diagram for all constructs (full labels)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_all_constructs_full_labels, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)

  start_col <- 13
  writeData(wb, sheet, "Figure 2a: Network diagram for constructs inside cliques only (full labels)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_cliques_only_full_labels, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)

  # row 2: construct numbers as labels

  row <- row + 45
  i2 <- row
  start_col <- 1
  writeData(wb, sheet, "Figure 1b: Network diagram for all constructs (construct no. as labels)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_all_constructs, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)

  start_col <- 13
  writeData(wb, sheet, "Figure 2b: Network diagram for constructs inside cliques only (construct no. as labels)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_cliques_only, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)

  # row 3: separate poles

  row <- row + 45
  i2 <- row

  start_col <- 1
  writeData(wb, sheet, "Figure 3a: Network diagram for all constructs (full labels + related poles in bold)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs - Bold poles are related", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_all_constructs_separate_poles, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)

  start_col <- 13
  writeData(wb, sheet, "Figure 3b: Network diagram for constructs inside cliques only (full labels + related poles in bold)", startRow = i2, startCol = start_col)
  addStyle(wb, sheet, style = style_bold, gridExpand = TRUE, rows = i2, cols = start_col)
  writeData(wb, sheet, "Lines represent relatedness of constructs - Bold poles are related", startRow = i2 + 1, startCol = start_col)
  writeData(wb, sheet, "Colored hull indicates a clique", startRow = i2 + 2, startCol = start_col)
  addStyle(wb, sheet, style = style_italic, gridExpand = TRUE, rows = i2 + 1:2, cols = start_col)
  insertImage(wb, sheet, img_cliques_only_separate_poles, width = 20, height = 20, units = "cm", startRow = i2 + 4, startCol = start_col)


  tmp_file <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp_file, overwrite = TRUE)
  tmp_file
}
