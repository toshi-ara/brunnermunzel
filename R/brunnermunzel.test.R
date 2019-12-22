#' brunnermunzel.test
#'
#' This function performs the Brunner--Munzel test for stochastic
#' equality of two samples, which is also known as the Generalized Wilcoxon
#' Test. \code{NA}s from the data are omitted.
#' This function enables to use formula as argument.
#'
#' @return A list containing the following components:
#'  \item{data.name}{a character string giving the name of the data.}
#'  \item{statistic}{the Brunner--Munzel test statistic.}
#'  \item{parameter}{the degrees of freedom.}
#'  \item{p.value}{the \eqn{p}-value of the test.}
#'  \item{conf.int}{the confidence interval.}
#'  \item{estimate}{an estimate of the effect size}
#'
#' @note There exist discrepancies with Brunner and Munzel (2000)
#'  because there is a typo in the paper. The corrected version is
#'  in Neubert and Brunner (2007) (e.g., compare the estimates for
#'  the case study on pain scores).
#'  The current R function follows Neubert and Brunner (2007).
#'
#' @seealso The R script of brunnermunzel.test.default is
#'  derived from that of \code{brunner.munzel.test}
#'   in \code{lawstat} package,
#'   and is rewritten with FORTRAN.
#'  Thanks to authors of \code{lawstat} package.
#'
#'@examples
#' ## Hollander & Wolfe (1973), 29f.
#' ## Hamilton depression scale factor measurements in 9 patients with
#' ##  mixed anxiety and depression, taken at the first (x) and second
#' ##  (y) visit after initiation of a therapy (administration of a
#' ##  tranquilizer).
#' x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#'
#' brunnermunzel.test(x, y)
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  x and y
#' ## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
#' ## 95 percent confidence interval:
#' ##  -0.02962941  0.59753064
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##        0.2839506
#'
#' ## 'est' option
#' ## if 'est = "difference"' return P(X<Y) - P(X>Y)
#' brunnermunzel.test(x, y, est = "difference")
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  x and y
#' ## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
#' ## 95 percent confidence interval:
#' ##  -1.0592588  0.1950613
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##    -0.4320988
#'
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(x, y),
#'     group = factor(rep(c("x", "y"), c(length(x), length(y))),
#'                    levels = c("x", "y"))
#' )
#'
#' brunnermunzel.test(value ~ group, data = dat)
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
#' ## 95 percent confidence interval:
#' ##  -0.02962941  0.59753064
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y) 
#' ##        0.2839506 
#'
#'
#' ## Pain score on the third day after surgery for 14 patients under
#' ## the treatment Y and 11 patients under the treatment N
#' ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
#'
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#'
#' brunnermunzel.test(Y, N)
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data: Y and N
#' ## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
#' ## 95 percent confidence interval:
#' ##  0.5952169 0.9827052
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##        0.788961
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' brunnermunzel.test(value ~ group, data = dat)
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
#' ## 0.005786
#' ## 95 percent confidence interval:
#' ##  0.5952169 0.9827052
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##         0.788961
#'
#'
#' ## Matrix or Table interface.
#' ##
#' dat1 <- matrix(c(4, 4, 2, 1, 5, 4), nr = 2, byrow = TRUE)
#' dat2 <- as.table(dat1)
#'
#' brunnermunzel.test(dat1)  # matrix
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  Group1 and Group2
#' ## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
#' ## 0.1393
#' ## 95 percent confidence interval:
#' ##  0.4351213 0.9248787
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##             0.68
#'
#' brunnermunzel.test(dat2)  # table
#' ##
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  A and B
#' ## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
#' ## 0.1393
#' ## 95 percent confidence interval:
#' ##  0.4351213 0.9248787
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##             0.68
#'
#' @export
#'
brunnermunzel.test <- function(x, ...) UseMethod("brunnermunzel.test")

#' @rdname brunnermunzel.test
#' @method brunnermunzel.test default
#'
#' @importFrom stats na.omit
#'
#' @param x the numeric vector of data values from the sample 1,
#'  or 2 x n matrix of table
#' (number of row must be 2 and column is ordinal variables).
#' @param y the numeric vector of data values from the sample 2.
#'  If x is matrix or table, y must be missing.
#' @param alpha significance level, default is 0.05 for 95\% confidence
#' interval.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{two.sided} (default), \code{greater} or
#' \code{less}. User can specify just the initial letter.
#' @param perm logical
#'   \describe{
#'    \item{FALSE}{(default): perform Brunner-Munzel test.}
#'    \item{TRUE}{: perform permuted Brunner-Munzel test.}
#'   }
#' @param est a method to calculate estimate and confidence interval,
#' must be either \code{original} (default) or \code{difference}.
#'    \describe{
#'      \item{original}{(default): return \eqn{p = P(X < Y) + 0.5 * P(X = Y)}}
#'      \item{difference}{: return mean difference.
#'            i.e. \eqn{P(X < Y) - P(X > Y) = 2 * p - 1}}
#'    }
#' This change is proposed by Dr. Julian D. Karch.
#'
#'
#' @export
#'
brunnermunzel.test.default <-
    function (x, y,
              alternative = c("two.sided", "greater", "less"),
              alpha = 0.05, perm = FALSE,
              est = c("original", "difference"),
              ...)
{
    if (perm) {
        res <- brunnermunzel.permutation.test(x, y,
                                              alternative = alternative,
                                              alpha = alpha,
                                              est = est,
                                              ...)
        return(res)
    }

    alternative <- match.arg(alternative)
    est <- match.arg(est)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- na.omit(x); y <- na.omit(y)

    n1 <- length(x); n2 <- length(y)
    if (n1 == 1L || n2 == 1L) stop("not enough observations")

    alt <- switch(alternative,
                  "two.sided" = 1L,
                  "greater" = 2L,
                  "less" = 3L)

    res <- .Fortran("bm_test",
                    nx = as.integer(n1), ny = as.integer(n2),
                    x = as.double(x), y = as.double(y),
                    alpha = as.double(alpha),
                    alter = as.integer(alt),
                    pst = numeric(1), ci = numeric(2),
                    stat = numeric(1), df = numeric(1),
                    pval = numeric(1),
                    PACKAGE = "brunnermunzel")

    if (est == "original") {
        ESTIMATE <- res$pst
        names(ESTIMATE) <- "P(X<Y)+.5*P(X=Y)"
        CONF.INT <- res$ci
    } else {
        ESTIMATE <- res$pst * 2 - 1
        names(ESTIMATE) <- "P(X<Y)-P(X>Y)"
        CONF.INT = res$ci * 2 - 1
    }

    STATISTIC <- res$stat
    names(STATISTIC) <- "Brunner-Munzel Test Statistic"
    PARAMETER <- res$df
    names(PARAMETER) <- "df"
    names(CONF.INT) <- c("lower", "upper")
    attr(CONF.INT, "conf.level") <- (1 - alpha)
    METHOD <- "Brunner-Munzel Test"

    structure(
        list(method = METHOD,
             statistic = STATISTIC,
             data.name = DNAME,
             parameter = PARAMETER,
             estimate = ESTIMATE,
             p.value = res$pval,
             conf.int = CONF.INT),
        class = "htest"
    )
}


#' @rdname brunnermunzel.test
#' @method brunnermunzel.test formula
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
brunnermunzel.test.formula <-
    function(formula, data, subset = NULL, na.action, ...)
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
    y <- do.call("brunnermunzel.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}


#' @rdname brunnermunzel.test
#' @method brunnermunzel.test matrix
#'
#' @export
#'
brunnermunzel.test.matrix <- function(x, ...) {
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

    level <- seq_len(ncol(x))
    g1 <- rep(level, x[1L,])
    g2 <- rep(level, x[2L,])

    z <- do.call("brunnermunzel.test", c(list(g1, g2, ...)))
    z$data.name <- DNAME
    z
}

#' @rdname brunnermunzel.test
#' @method brunnermunzel.test table
#'
#' @export
#'
brunnermunzel.test.table <- function(x, ...) {
    brunnermunzel.test.matrix(x, ...)
}

