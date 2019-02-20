context("FORTRAN subroutine: combination")

test_f_combination <- function(n, input, output) {
    res <- .Fortran("combination",
                n = as.integer(n),
                r = length(input),
                ini = 1:n,
                arr = as.integer(input))
    return(expect_equal(res$arr, output))
}

test_that("", {
  test_f_combination(6, c(1,2,3,4), c(1,2,3,5))
  test_f_combination(6, c(1,3,5,6), c(1,4,5,6))
  test_f_combination(6, c(2,4,5,6), c(3,4,5,6))
  test_f_combination(10, c(1,2,3,5,10), c(1,2,3,6,7))
  test_f_combination(10, c(4,7,8,9,10), c(5,6,7,8,9))
})

