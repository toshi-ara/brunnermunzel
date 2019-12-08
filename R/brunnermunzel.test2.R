#' brunnermunzel.test2
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
#'  \item{estimate}{an estimate of the effect size,
#'        i.e., \eqn{P(X < Y) - P(X > Y)}}
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
#' brunnermunzel.test2(x, y)
#'
#' ##       Brunner-Munzel Test
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
#' brunnermunzel.test2(value ~ group, data = dat)
#'
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
#' ## 95 percent confidence interval:
#' ##  -1.0592588  0.1950613
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##    -0.4320988
#'
#'
#' ## Pain score on the third day after surgery for 14 patients under
#' ## the treatment Y and 11 patients under the treatment N
#' ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
#'
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#'
#' brunnermunzel.test2(Y, N)
#'
#' ##       Brunner-Munzel Test
#' ## data: Y and N
#' ## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
#' ## 95 percent confidence interval:
#' ##  0.1904337 0.9654104
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##     0.5779221
#'
#'
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' brunnermunzel.test2(value ~ group, data = dat)
#'
#' ##       Brunner-Munzel Test
#' ##
#' ## data:  value by group
#' ## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
#' ## 0.005786
#' ## 95 percent confidence interval:
#' ##  0.1904337 0.9654104
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##     0.5779221
#'
#'
#' ## Matrix or Table interface.
#' ##
#' dat1 <- matrix(c(4, 4, 2, 1, 5, 4), nr = 2, byrow = TRUE)
#' dat2 <- as.table(dat1)
#'
#' brunnermunzel.test2(dat1)  # matrix
#'
#' ##       Brunner-Munzel Test
#' ## data:  Group1 and Group2
#' ## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
#' ## 0.1393
#' ## 95 percent confidence interval:
#' ##  -0.1297573  0.8497573
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##          0.36
#'
#' brunnermunzel.test2(dat2)  # table
#'
#' ##       Brunner-Munzel Test
#' ## data:  A and B
#' ## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
#' ## 0.1393
#' ## 95 percent confidence interval:
#' ##  -0.1297573  0.8497573
#' ## sample estimates:
#' ## P(X<Y)-P(X>Y)
#' ##          0.36
#'
#' @export
#'
brunnermunzel.test2 <- function(x, ...) UseMethod("brunnermunzel.test2")

#' @rdname brunnermunzel.test2
#' @method brunnermunzel.test2 default
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
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#' @param perm logical
#'   \describe{
#'    \item{FALSE}{(default): perform Brunner-Munzel test.}
#'    \item{TRUE}{: perform permuted Brunner-Munzel test.}
#'   }
#'
#' @export
#'
brunnermunzel.test2.default <-
    function (x, y,
              alternative = c("two.sided", "greater", "less"),
              alpha = 0.05, perm = FALSE, ...)
{
    if (perm) {
        res <- brunnermunzel.permutation.test2(x, y,
                                               alternative = alternative, ...)
        return(res)
    }

    res <- brunnermunzel.test(x, y,
                              alternative = alternative,
                              alpha = alpha, perm = perm, ...)

    ESTIMATE <- res$estimate * 2 - 1
    names(ESTIMATE) <- "P(X<Y)-P(X>Y)"

    structure(list(estimate = ESTIMATE,
                   conf.int = res$conf.int * 2 - 1,
                   statistic = res$statistic,
                   parameter = res$parameter,
                   p.value = res$p.value,
                   method = res$method,
                   data.name = res$data.name),
              class = "htest")
}


#' @rdname brunnermunzel.test2
#' @method brunnermunzel.test2 formula
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
brunnermunzel.test2.formula <-
    function(formula, data, subset = NULL, na.action, ...)
{
    res <- brunnermunzel.test(formula = formula,
                              data = data, subset = NULL,
                              na.action = na.action, ...)

    ESTIMATE <- res$estimate * 2 - 1
    names(ESTIMATE) <- "P(X<Y)-P(X>Y)"

    structure(
        list(estimate = ESTIMATE,
             conf.int = res$conf.int * 2 - 1,
             statistic = res$statistic,
             parameter = res$parameter,
             p.value = res$p.value,
             method = res$method,
             data.name = res$data.name),
        class = "htest"
    )
}


#' @rdname brunnermunzel.test2
#' @method brunnermunzel.test2 matrix
#'
#' @export
#'
brunnermunzel.test2.matrix <- function(x, ...) {
    res <- brunnermunzel.test(x, ...)

    ESTIMATE <- res$estimate * 2 - 1
    names(ESTIMATE) <- "P(X<Y)-P(X>Y)"

    structure(list(estimate = ESTIMATE,
                   conf.int = res$conf.int * 2 - 1,
                   statistic = res$statistic,
                   parameter = res$parameter,
                   p.value = res$p.value,
                   method = res$method,
                   data.name = res$data.name),
              class = "htest")
}

#' @rdname brunnermunzel.test2
#' @method brunnermunzel.test2 table
#'
#' @export
#'
brunnermunzel.test2.table <- function(x, ...) {
    brunnermunzel.test2.matrix(x, ...)
}

