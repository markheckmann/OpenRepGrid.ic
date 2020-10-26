

library(tidyverse)
library(openxlsx)

# sample grid should be read in correctly 

test_that("Sylvia's grid is read and processed correctly", {
  
  file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
  x <- read.xlsx(file)
  tests <- check_excel_input(x)   # check if input format is correct
  
  expect_true(all(tests$passed))  # all format tests have been passed
  
  l <- network_graph_images(x, min_clique_size = 3, show_edges = TRUE, min_matches = 6)
  cl <- l$cliques_list %>% lapply(sort)
  
  expect_equal(length(cl), 3)                              # three cliques identified
  expect_identical(cl[[1]], c("C10", "C11", "C3", "C8"))   # constructs in first clique
  expect_identical(cl[[2]], c("C10", "C11", "C7", "C8"))  
  expect_identical(cl[[3]], c("C16", "C3", "C5", "C9"))   
})


# errors are thrown if Excel input format is incorrect

test_that("input file format is checked correctly", {
  
  file <- system.file("extdata", "incorrect_formats.xlsx", package = "OpenRepGrid.ic")
  x <- read.xlsx(file)
  tests <- check_excel_input(x)  # check if input format is correct
  
  expect_false(all(tests$passed))  # all format tests have been passe
})
