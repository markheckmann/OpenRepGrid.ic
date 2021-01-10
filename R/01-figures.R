#///////////////////////////////////////////////////////
#
#           01 Create figures used in article 
#
#///////////////////////////////////////////////////////


library(OpenRepGrid)
library(OpenRepGrid.ic)
library(openxlsx)
library(dplyr)

#file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
#x <- importExcel("data/")

x <- boeker   
  
# 01 Bertin  -----------

png("img/01-bertin.png", width = 20, height = 15, res = 300, units = "cm")
  bertin(x, colors = c("white", "darkred"))
dev.off()


# 02 Boeker binarized  -----------

# binarize Boeker grid
r <- getRatingLayer(x)
mp <- getScaleMidpoint(x)
r[r < mp] <- 0
r[r > mp] <- 1
x[,] <- r
x <- setScale(x, 0, 1)
png("img/01-bertin-binary.png", width = 20, height = 15, res = 300, units = "cm")
bertin(x, colors = c(grey(.9), "darkred"))
dev.off()


file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
x <- read.xlsx(file)            # read grid
tests <- check_excel_input(x)   # check if input format is correct
l <- network_graph_images(x, min_clique_size = 3, 
                          align_poles = T, 
                          valence_prefix = T,
                          show_edges = TRUE, 
                          min_matches = 6,
                          colorize_cliques = T,
                          )    # produce images
#file_tmp <- create_excel_output(file, l)      # create Excel file
# l$img_all_constructs_full_labels %>% file.show()
# l$img_all_constructs_separate_poles %>% file.show()
file.copy(from = l$img_all_constructs_full_labels, to = "img/03-analysis-result.png")
file.copy(from = l$img_all_constructs_separate_poles, to = "img/03-analysis-result-poles.png")
