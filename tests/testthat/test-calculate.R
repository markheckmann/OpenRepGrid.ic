


test_that("calculation functions work as expected", {

  # count_matches --------------------

  ci <- matrix(c(1, 1, 0, 0), 2, 2, byrow = TRUE)
  cj <- matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
  expect_equal(count_matches(ci, ci), 4)
  expect_equal(count_matches(ci, ci, inverse = TRUE), 0)

  expect_equal(count_matches(cj, cj), 4)
  expect_equal(count_matches(cj, cj, inverse = TRUE), 0)

  expect_equal(count_matches(cj, ci), 2)
  expect_equal(count_matches(cj, ci, inverse = TRUE), 2)


  # align_positive_poles --------------------

  df <- data.frame(
    left = c("l1", "l2", "l3"),
    e1 = c(0, 1, NA),
    e2 = c(0, 1, 1),
    right = c("r1", "r2", "r3"),
    preferred = c(1, 1, 1)
  )
  expect_equal(align_positive_poles(df), df)

  df2 <- data.frame(
    left = c("r1", "r2", "r3"),
    e1 = c(1, 0, NA),
    e2 = c(1, 0, 0),
    right = c("l1", "l2", "l3"),
    preferred = c(0, 0, 0)
  )
  expect_equal(align_positive_poles(df2), df)


  # calculate_similarity --------------------

  # (these results are the input for the graph algortihm)
  file <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
  x <- read.xlsx(file)
  x_subset <- x[c(3, 7, 10), ] # only use three constructs for testing
  l <- calculate_similarity(x_subset)

  # => no of matches (inclucing optional construct reversal)
  R <- matrix(c(
    NA, 5, 6,
    5, NA, 6,
    6, 6, NA
  ), nrow = 3, byrow = TRUE)
  expect_equal(l$R, R, check.attributes = FALSE)

  # => # matrix of matches without optional construct reversal)
  M <- matrix(c(
    NA, 2, 6,
    2, NA, 1,
    6, 1, NA
  ), nrow = 3, byrow = TRUE)
  expect_equal(l$M, M, check.attributes = FALSE)

  # relatedness 0/1
  MM <- matrix(c(
    0, 0, 1,
    0, 0, 1,
    1, 1, 0
  ), nrow = 3, byrow = TRUE)
  expect_equal(l$MM, MM, check.attributes = FALSE)

  # direction of relation -1/1
  D <- matrix(c(
    NA, NA, 1,
    NA, NA, -1,
    1, -1, NA
  ), nrow = 3, byrow = TRUE)
  expect_equal(l$D, D, check.attributes = FALSE)
})
