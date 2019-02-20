context("FORTRAN subroutine: divide_group")

test_f_group <- function(dat, idx) {
    n <- length(dat)
    nx <- length(idx)
    ny <- n - nx
    x <- dat[idx]
    y <- dat[-idx]

    res <- .Fortran("divide_groups",
                nx = as.integer(nx),
                ny = as.integer(ny),
                dat = as.numeric(dat),
                idx = as.integer(idx),
                x = numeric(nx),
                y = numeric(ny),
                xy = numeric(n))

    match <- ((res$x == x) && (res$y == y) && (res$xy == c(x, y)))
    expect_true(match)
}

test_that("", {
  test_f_group(dat = 1:10, idx = 1:5)
  test_f_group(dat = 1:20, idx = 1:5)
  test_f_group(dat = 1:20, idx = 1:10)
  test_f_group(dat = 1:30, idx = 1:15)
  test_f_group(dat = 1:20, idx = c(1, 6, 8, 10, 11, 15, 17, 19))
})

