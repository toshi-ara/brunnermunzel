context("FORTRAN subroutine: rank")

test_f_rank <- function(x) {
    n <- length(x)
    res <- .Fortran("rank",
                n = as.integer(n),
                x = as.numeric(x),
                rk = numeric(n))
    return(expect_equal(res$rk, rank(x)))
}

test_that("", {
  test_f_rank(1:10)
  test_f_rank(10:1)
  test_f_rank(c(1, 3, 5, 7, 9))
  test_f_rank(c(1:5, 1:5))
  test_f_rank(c(1:5, 3:7))
})

