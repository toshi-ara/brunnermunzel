context("FORTRAN subroutines")
source("f_function.R")


test_that("rank", {
  test_f_rank(1:10)
  test_f_rank(10:1)
  test_f_rank(c(1, 3, 5, 7, 9))
  test_f_rank(c(1:5, 1:5))
  test_f_rank(c(1:5, 3:7))
})

test_that("divide_group", {
  test_f_group(dat = 1:10, idx = 1:5)
  test_f_group(dat = 1:20, idx = 1:5)
  test_f_group(dat = 1:20, idx = 1:10)
  test_f_group(dat = 1:30, idx = 1:15)
  test_f_group(dat = 1:20, idx = c(1, 6, 8, 10, 11, 15, 17, 19))
})

test_that("combination", {
  test_f_combination(6, c(1,2,3,4), c(1,2,3,5))
  test_f_combination(6, c(1,3,5,6), c(1,4,5,6))
  test_f_combination(6, c(2,4,5,6), c(3,4,5,6))
  test_f_combination(10, c(1,2,3,5,10), c(1,2,3,6,7))
  test_f_combination(10, c(4,7,8,9,10), c(5,6,7,8,9))
})

test_that("calc_statistics", {
    test_f_statistics(c(1:5, 1:5), 5, 0)
    test_f_statistics(c(1:5, 2:6), 5, 0.3975535)
    test_f_statistics(c(1:5, 3:7), 5, 0.9335359)
    test_f_statistics(c(1:10, 4:13), 10, 0.4667542)

    ## Pain score on the third day after surgery for 14 patients under
    ## the treatment Y and 11 patients under the treatment N
    ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
    test_f_statistics(c(1,2,1,1,1,1,1,1,1,1,2,4,1,1,3,3,4,3,1,2,3,1,1,5,4),
                      14, 0.5093291)
})

