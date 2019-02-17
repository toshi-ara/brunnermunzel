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
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#'
#' \dontrun{
#' brunnermunzel.permutation.test(Y, N)
#' }
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  Y and N
#' ## p-value = 0.008038
#'
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' \dontrun{
#' brunnermunzel.permutation.test(value ~ group, data = dat)
#' }
#'
#' ##       permuted Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## p-value = 0.008038
#'
#' @export
#'
brunnermunzel.permutation.test <-  function(x, ...)
    UseMethod("brunnermunzel.permutation.test")


#' @rdname brunnermunzel.permutation.test
#' @method brunnermunzel.permutation.test default
#'
#' @importFrom utils combn
#' @importFrom stats setNames terms
#'
#' @param x the numeric vector of data values from the sample 1.
#' @param y the numeric vector of data values from the sample 2.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#'
#' @export
#'
brunnermunzel.permutation.test.default <-
    function(x, y,
             alternative = c("two.sided", "greater", "less"),
             ...) {
        alternative <- match.arg(alternative)
        DNAME <-  paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)))

        nx <- length(x); ny <- length(y)
        if (nx == 1 || ny == 1) stop("not enough observations")
        n_nCr <- choose(nx + ny, nx)

        res <- .Fortran("bm_permutation_stat",
                        n = as.integer(nx + ny),
                        r = as.integer(nx),
                        n_nCr = as.integer(n_nCr),
                        dat = as.double(c(x, y)),
                        statistics = numeric(n_nCr),
                        PACKAGE = "brunnermunzel")
        z1 <- res$statistics
        z0 <- z1[1]

        p.value <- switch(alternative,
                          "two.sided" = mean(abs(z1) >= abs(z0)),
                          "greater" = mean(z1 <= z0),
                          "less" = mean(z1 >= z0))

        structure(
            list(
                method = "permuted Brunner-Munzel Test",
                p.value = p.value,
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
