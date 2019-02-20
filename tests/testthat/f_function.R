test_f_rank <- function(x) {
    n <- length(x)
    res <- .Fortran("rank",
                n = as.integer(n),
                x = as.numeric(x),
                rk = numeric(n))
    return(expect_equal(res$rk, rank(x)))
}


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


test_f_combination <- function(n, input, output) {
    res <- .Fortran("combination",
                n = as.integer(n),
                r = length(input),
                ini = 1:n,
                arr = as.integer(input))
    return(expect_equal(res$arr, output))
}


test_f_statistics <- function(dat, nx, stat) {
    n <- length(dat)
    ny <- n - nx

    const <- numeric(4)
    const[1] = (nx + 1) * 0.5
    const[2] = (ny + 1) * 0.5
    const[3] = nx * 1.0 / (nx - 1)
    const[4] = ny * 1.0 / (ny - 1)

    res <- .Fortran("calc_statistics",
                nx = as.integer(nx),
                ny = as.integer(ny),
                dat = as.numeric(dat),
                const = as.numeric(const),
                idx = 1:nx,
                stat = numeric(1))
    return(expect_equal(res$stat, stat, tolerance = 1e-6))
}

