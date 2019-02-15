#' brunnermunzel.test
#'
#' This function performs the Brunner--Munzel test for stochastic
#' equality of two samples, which is also known as the Generalized Wilcoxon
#' Test. \code{NA}s from the data are omitted.
#' This function enables to use formula as argument.
#'
#' @return A list containing the following components:
#'  \item{statistic}{the Brunner--Munzel test statistic.}
#'  \item{parameter}{the degrees of freedom.}
#'  \item{conf.int}{the confidence interval.}
#'  \item{p.value}{the \eqn{p}-value of the test.}
#'  \item{data.name}{a character string giving the name of the data.}
#'  \item{estimate}{an estimate of the effect size,
#'        i.e., \eqn{P(X < Y) + 0.5 * P(X = Y)}}
#'
#' @note There exist discrepancies with Brunner and Munzel (2000)
#'  because there is a typo in the paper. The corrected version is
#'  in Neubert and Brunner (2007) (e.g., compare the estimates for
#'  the case study on pain scores).
#'  The current R function follows Neubert and Brunner (2007).
#'
#' @seealso The R script of brunnermunzel.test.default is
#'  derived from that of \code{brunner.munzel.test}
#'   in \code{lawstat} package.
#'  Thanks to authors of \code{lawstat} package.
#'
#'@examples
#' ## Pain score on the third day after surgery for 14 patients under
#' ## the treatment Y and 11 patients under the treatment N
#' ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
#'
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#'
#' brunnermunzel.test(Y, N)
#'
#' ##       Brunner-Munzel Test
#' ## data: Y and N
#' ## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
#' ## 95 percent confidence interval:
#' ##  0.5952169 0.9827052
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##        0.788961
#'
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' brunnermunzel.test(value ~ group, data = dat)
#'
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
#' @export
brunnermunzel.test <- function(x, ...) UseMethod("brunnermunzel.test")

#' @rdname brunnermunzel.test
#' @method brunnermunzel.test default
#'
#' @importFrom stats na.omit pt qt
#'
#' @param x the numeric vector of data values from the sample 1.
#' @param y the numeric vector of data values from the sample 2.
#' @param alpha significance level, default is 0.05 for 95\% confidence
#' interval.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#'
#' @export
#'
brunnermunzel.test.default <-
    function (x, y,
              alternative = c("two.sided", "greater", "less"),
              alpha = 0.05, ...)
{
    alternative <- match.arg(alternative)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- na.omit(x); y <- na.omit(y)

    n1 <- length(x); n2 <- length(y)
    if (n1 == 1 || n2 == 1) stop("not enough observations")

    r1 <- rank(x); r2 <- rank(y); r <- rank(c(x, y))
    m1 <- mean(r[1:n1]); m2 <- mean(r[n1 + 1:n2])

    pst <- (m2 - (n2 + 1)/2) / n1
    v1 <- sum((r[1:n1] - r1 - m1 + (n1 + 1)/2)^2) / (n1 - 1)
    v2 <- sum((r[n1 + 1:n2] - r2 - m2 + (n2 + 1)/2)^2) / (n2 - 1)

    statistic <- n1 * n2 * (m2 - m1) / (n1 + n2) / sqrt(n1 * v1 + n2 * v2)
    dfbm <- (n1 * v1 + n2 * v2)^2 /
        (((n1 * v1)^2)/(n1 - 1) + ((n2 * v2)^2)/(n2 - 1))

    p.value <-
        switch(alternative,
               "two.sided" =
                   2 * min(pt(abs(statistic), dfbm),
                           pt(abs(statistic), dfbm, lower.tail = FALSE)),
               "greater" =
                   pt(statistic, dfbm),
               "less" =
                   pt(statistic, dfbm, lower.tail = FALSE)
               )

    conf.int <- pst + c(-1, 1) * qt(1 - alpha/2, dfbm) *
        sqrt(v1/(n1 * n2^2) + v2/(n2 * n1^2))

    ESTIMATE <- pst
    names(ESTIMATE) <- "P(X<Y)+.5*P(X=Y)"
    STATISTIC <- statistic
    names(STATISTIC) <- "Brunner-Munzel Test Statistic"
    PARAMETER <- dfbm
    names(PARAMETER) <- "df"
    CONF.INT <- conf.int
    names(CONF.INT) <- c("lower", "upper")
    attr(CONF.INT, "conf.level") <- (1 - alpha)
    METHOD <- "Brunner-Munzel Test"

    structure(list(estimate = ESTIMATE, conf.int = CONF.INT,
                   statistic = STATISTIC, parameter = PARAMETER,
                   p.value = p.value, method = METHOD, data.name = DNAME),
              class = "htest")
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
    y <- do.call("brunnermunzel.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
