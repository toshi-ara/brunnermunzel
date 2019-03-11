#' @useDynLib brunnermunzel
NULL

#' permuted Brunner-Munzel test
#'
#' This function performs the permuted Brunner-Munzel test.
#'
#' @return A list containing the following components:
#'  \item{method}{the characters ``permuted Brunner-Munzel Test''}
#'  \item{p.value}{the \eqn{p}-value of the test.}
#'  \item{data.name}{a character string giving the name of the data.}
#'
#' @references Karin Neubert and Edgar Brunner,
#' ``A studentized permutation test for the non-parametric
#' Behrens-Fisher problem'',
#' Computational Statistics and Data Analysis, Vol. 51, pp. 5192-5204 (2007).
#'
#' @seealso This function is made in reference to following cite (in Japanese):
#' Prof. Haruhiko Okumura
#' (\url{https://oku.edu.mie-u.ac.jp/~okumura/stat/brunner-munzel.html}).
#'
#' @note FORTRAN subroutine `combination` in combination.f is derived from
#' the program by shikino (\url{http://slpr.sakura.ne.jp/qp/combination})
#' (CC-BY-4.0).
#' Thanks to shikono for your useful subroutine.
#'
#' @examples
#' ## Hollander & Wolfe (1973), 29f.
#' ## Hamilton depression scale factor measurements in 9 patients with
#' ##  mixed anxiety and depression, taken at the first (x) and second
#' ##  (y) visit after initiation of a therapy (administration of a
#' ##  tranquilizer).
#' x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#'
#' brunnermunzel.permutation.test(x, y)
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  x and y
#' ## p-value = 0.158
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(x, y),
#'     group = factor(rep(c("x", "y"), c(length(x), length(y))),
#'                    levels = c("x", "y"))
#' )
#'
#' brunnermunzel.permutation.test(value ~ group, data = dat)
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## p-value = 0.158
#'
#'
#' ## Pain score on the third day after surgery for 14 patients under
#' ## the treatment Y and 11 patients under the treatment N
#' ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
#'
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#'
#' \donttest{
#' brunnermunzel.permutation.test(Y, N)
#' }
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  Y and N
#' ## p-value = 0.008038
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' \donttest{
#' brunnermunzel.permutation.test(value ~ group, data = dat)
#' }
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## p-value = 0.008038
#'
#'
#' ## Matrix or Table interface.
#' ##
#' dat1 <- matrix(c(4, 4, 2, 1, 5, 4), nr = 2, byrow = TRUE)
#' dat2 <- as.table(dat1)
#'
#' brunnermunzel.permutation.test(dat1)  # matrix
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  Group1 and Group2
#' ## p-value = 0.1593
#'
#' brunnermunzel.permutation.test(dat2)  # table
#'
#' ##       Brunner-Munzel Test
#' ##
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  A and B
#' ## p-value = 0.1593
#'
#' @export
#'
brunnermunzel.permutation.test <-  function(x, ...)
    UseMethod("brunnermunzel.permutation.test")


#' @rdname brunnermunzel.permutation.test
#' @method brunnermunzel.permutation.test default
#'
#' @importFrom stats setNames terms
#'
#' @param x the numeric vector of data values from the sample 1,
#'  or 2 x n matrix of table
#' (number of row must be 2 and column is ordinal variables).
#' @param y the numeric vector of data values from the sample 2.
#'  If x is matrix or table, y must be missing.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#' @param force
#'   \describe{
#'    \item{FALSE}{(default): If sample size is too large
#'      [number of combinations > 40116600 = choose(28, 14)],
#'      use \code{brunnermunzel.test}.}
#'    \item{TRUE}{: perform permuted Brunner-Munzel test
#'      regardless sample size.}
#'   }
#'
#' @export
#'
brunnermunzel.permutation.test.default <-
    function(x, y,
             alternative = c("two.sided", "greater", "less"),
             force = FALSE,
             ...) {
        alternative <- match.arg(alternative)
        DNAME <-  paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)))

        nx <- length(x); ny <- length(y)
        if (nx == 1L || ny == 1L) stop("not enough observations")
        n_nCr <- choose(nx + ny, nx)

        # switch brunnermunzel.test
        #  when sample number is too large
        if (!force && (n_nCr > 40116600L)) { # choose(28, 14)
            warning(c("Sample number is too large. ",
                      "Using 'brunnermunzel.test'\n"))
            res <- brunnermunzel.test(x, y, alternative = alternative)
            return(res)
        }

        alter <- switch(alternative,
                        "two.sided" = 1L,
                        "greater" = 2L,
                        "less" = 3L)

        res <- .Fortran("bm_permutation_test",
                        n = as.integer(nx + ny),
                        r = as.integer(nx),
                        n_nCr = as.integer(n_nCr),
                        dat = as.double(c(x, y)),
                        alter = as.integer(alter),
                        pval = numeric(1),
                        PACKAGE = "brunnermunzel")

        structure(
            list(
                method = "permuted Brunner-Munzel Test",
                p.value = res$pval,
                data.name = DNAME),
            class = "htest")
    }


#' @rdname brunnermunzel.permutation.test
#' @method brunnermunzel.permutation.test formula
#'
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs}
#' is a numeric variable giving the data values and \code{rhs} a factor
#' with two levels giving the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the
#' formula \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations
#' to be used.
#' @param na.action a function which indicates what should happen when
#' the data contain \code{NA}s.  Defaults to
#' \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods
#' (This argument is for only formula).
#'
#' @export
#'
brunnermunzel.permutation.test.formula <-
    function(formula, data, subset, na.action, ...)
{
    if (missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL

    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")

    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")

    DATA <- setNames(split(mf[[response]], g), c("x", "y"))
    y <- do.call("brunnermunzel.permutation.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}


#' @rdname brunnermunzel.permutation.test
#' @method brunnermunzel.permutation.test matrix
#'
#' @export
#'
brunnermunzel.permutation.test.matrix <- function(x, ...) {
    if (!is.matrix(x)) {
        stop("Class of data must be 'matrix' or 'table'")
    }

    if (nrow(x) != 2L) {
        stop("Number of rows must be 2")
    }

    rname <- rownames(x)
    if (is.null(rname)) {
        rname <- c("Group1", "Group2")
    }
    DNAME <- paste(rname, collapse = " and ")

    lv <- seq_len(ncol(x))
    g1 <- rep(lv, x[1L,])
    g2 <- rep(lv, x[2L,])

    z <- do.call("brunnermunzel.permutation.test",
                 c(list(g1, g2, ...)))
    z$data.name <- DNAME
    z
}


#' @rdname brunnermunzel.permutation.test
#' @method brunnermunzel.permutation.test table
#'
#' @export
#'
brunnermunzel.permutation.test.table <- function(x, ...) {
    brunnermunzel.permutation.test.matrix(x, ...)
}

