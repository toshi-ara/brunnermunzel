% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brunnermunzel.test.R
\name{brunnermunzel.test}
\alias{brunnermunzel.test}
\alias{brunnermunzel.test.default}
\alias{brunnermunzel.test.formula}
\alias{brunnermunzel.test.matrix}
\alias{brunnermunzel.test.table}
\title{brunnermunzel.test}
\usage{
brunnermunzel.test(x, ...)

\method{brunnermunzel.test}{default}(
  x,
  y,
  alternative = c("two.sided", "greater", "less"),
  alpha = 0.05,
  perm = FALSE,
  est = c("original", "difference"),
  ...
)

\method{brunnermunzel.test}{formula}(formula, data, subset = NULL, na.action, ...)

\method{brunnermunzel.test}{matrix}(x, ...)

\method{brunnermunzel.test}{table}(x, ...)
}
\arguments{
\item{x}{the numeric vector of data values from the sample 1,
 or 2 x n matrix of table
(number of row must be 2 and column is ordinal variables).}

\item{\dots}{further arguments to be passed to or from methods
(This argument is for only formula).}

\item{y}{the numeric vector of data values from the sample 2.
If x is matrix or table, y must be missing.}

\item{alternative}{a character string specifying the alternative
hypothesis, must be one of \code{two.sided} (default), \code{greater} or
\code{less}. User can specify just the initial letter.}

\item{alpha}{significance level, default is 0.05 for 95\% confidence
interval.}

\item{perm}{logical
\describe{
 \item{FALSE}{(default): perform Brunner-Munzel test.}
 \item{TRUE}{: perform permuted Brunner-Munzel test.}
}}

\item{est}{a method to calculate estimate and confidence interval,
must be either \code{original} (default) or \code{difference}.
   \describe{
     \item{original}{(default): return \eqn{p = P(X < Y) + 0.5 * P(X = Y)}}
     \item{difference}{: return mean difference.
           i.e. \eqn{P(X < Y) - P(X > Y) = 2 * p - 1}}
   }
This change is proposed by Dr. Julian D. Karch.}

\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
is a numeric variable giving the data values and \code{rhs} a factor
with two levels giving the corresponding groups.}

\item{data}{an optional matrix or data frame (or similar: see
\code{\link{model.frame}}) containing the variables in the
formula \code{formula}.  By default the variables are taken from
\code{environment(formula)}.}

\item{subset}{an optional vector specifying a subset of observations
to be used.}

\item{na.action}{a function which indicates what should happen when
the data contain \code{NA}s.  Defaults to
\code{getOption("na.action")}.}
}
\value{
A list containing the following components:
 \item{data.name}{a character string giving the name of the data.}
 \item{statistic}{the Brunner--Munzel test statistic.}
 \item{parameter}{the degrees of freedom.}
 \item{p.value}{the \eqn{p}-value of the test.}
 \item{conf.int}{the confidence interval.}
 \item{estimate}{an estimate of the effect size}
}
\description{
This function performs the Brunner--Munzel test for stochastic
equality of two samples, which is also known as the Generalized Wilcoxon
Test. \code{NA}s from the data are omitted.
This function enables to use formula as argument.
}
\note{
There exist discrepancies with Brunner and Munzel (2000)
 because there is a typo in the paper. The corrected version is
 in Neubert and Brunner (2007) (e.g., compare the estimates for
 the case study on pain scores).
 The current R function follows Neubert and Brunner (2007).
}
\examples{
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

brunnermunzel.test(x, y)
##
##       Brunner-Munzel Test
##
## data:  x and y
## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
## 95 percent confidence interval:
##  -0.02962941  0.59753064
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##        0.2839506

## 'est' option
## if 'est = "difference"' return P(X<Y) - P(X>Y)
brunnermunzel.test(x, y, est = "difference")
##
##       Brunner-Munzel Test
##
## data:  x and y
## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
## 95 percent confidence interval:
##  -1.0592588  0.1950613
## sample estimates:
## P(X<Y)-P(X>Y)
##    -0.4320988


## Formula interface.
dat <- data.frame(
    value = c(x, y),
    group = factor(rep(c("x", "y"), c(length(x), length(y))),
                   levels = c("x", "y"))
)

brunnermunzel.test(value ~ group, data = dat)
##
##       Brunner-Munzel Test
##
## data:  value by group
## Brunner-Munzel Test Statistic = -1.4673, df = 15.147, p-value = 0.1628
## 95 percent confidence interval:
##  -0.02962941  0.59753064
## sample estimates:
## P(X<Y)+.5*P(X=Y) 
##        0.2839506 


## Pain score on the third day after surgery for 14 patients under
## the treatment Y and 11 patients under the treatment N
## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).

Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)

brunnermunzel.test(Y, N)
##
##       Brunner-Munzel Test
##
## data: Y and N
## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##        0.788961

## Formula interface.
dat <- data.frame(
    value = c(Y, N),
    group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
                   levels = c("Y", "N"))
)

brunnermunzel.test(value ~ group, data = dat)
##
##       Brunner-Munzel Test
##
## data:  value by group
## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
## 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##         0.788961


## Matrix or Table interface.
##
dat1 <- matrix(c(4, 4, 2, 1, 5, 4), nr = 2, byrow = TRUE)
dat2 <- as.table(dat1)

brunnermunzel.test(dat1)  # matrix
##
##       Brunner-Munzel Test
##
## data:  Group1 and Group2
## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
## 0.1393
## 95 percent confidence interval:
##  0.4351213 0.9248787
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##             0.68

brunnermunzel.test(dat2)  # table
##
##       Brunner-Munzel Test
##
## data:  A and B
## Brunner-Munzel Test Statistic = 1.5511, df = 16.961, p-value =
## 0.1393
## 95 percent confidence interval:
##  0.4351213 0.9248787
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##             0.68

}
\seealso{
The R script of brunnermunzel.test.default is
 derived from that of \code{brunner.munzel.test}
  in \code{lawstat} package,
  and is rewritten with FORTRAN.
 Thanks to authors of \code{lawstat} package.
}
