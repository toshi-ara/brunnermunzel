context("C subroutines")
source("function.R")


test_that("rank", {
  test_C_rank(1:10)
  test_C_rank(10:1)
  test_C_rank(c(1, 3, 5, 7, 9))
  test_C_rank(c(1:5, 1:5))
  test_C_rank(c(1:5, 3:7))
})

