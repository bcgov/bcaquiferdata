test_that("fix_range()", {

  x <- c("1-2", "3 to 4", "5 - 8", "8 - 10", "10-15")
  y <- c(1.5, 3.5, 6.5, 9, 12.5)

  expect_equal(fix_range(x), y)
})

test_that("fix_fraction()", {

  x <- c("1 3/4", "1/2", "1.5", "6 1/2", "3.5", "1 /2", "1 /2", "1 / 2")
  y <- c(1.75, 0.5, 1.5, 6.5, 3.5, 0.5, 0.5, 0.5) %>% as.character()
  expect_equal(fix_fraction(x), y)

  x <- list(c("1 3/4", "1/2"), c("1.5", "6 1/2", "3.5"), c("1 /2"))
  y <- list(c("1.75", "0.5"), c("1.5", "6.5", "3.5"), c("0.5"))
  expect_equal(purrr::map(x, fix_fraction), y)
})
