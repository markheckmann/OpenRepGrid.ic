


test_that("utility functions work as expected", {

  # emptify_object
  expect_equal(emptify_object(mtcars), mtcars[integer(0), ])
  expect_equal(
    emptify_object(list(a = 1, b = 1:10)),
    list(a = list(), b = list())
  )

  # dt_default
  dt <- dt_default()
  expect_true(inherits(dt, "datatables"))

  # cell_text_split
  res <- list(c("10", "20", "30"))
  expect_equal(cell_text_split("10, 20,30"), res)
  expect_equal(cell_text_split("; ,  10  ,,,  20;30,,"), res)

  # make_names_vec
  a <- c("ä", "ü", "ö", "Ä", "Ü", "Ö", "ß")
  b <- c("ae", "ue", "oe", "ae", "ue", "oe", "ss")
  expect_equal(make_names_vec(a), b)

  a <- c("___", "_a_", "a___b", "a  b  c", " _ _ _A")
  b <- c("", "_a", "a_b", "a_b_c", "_a")
  expect_equal(make_names_vec(a), b)

  # make_names
  df <- as.data.frame(matrix(NA, 0, 5))
  names(df) <- a
  expect_equal(names(make_names(df)), b)

  # fnum
  expect_equal(fnum(0.1), "0.10")
  expect_equal(fnum(11), "11.00")
  expect_equal(fnum(1, 3), "1.000")
})
