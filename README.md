---
title: brunnermunzel
author: Toshiaki Ara
date: "2019-02-18"
---

# Introduction

For Brunner-Munzel test
`brunner.munzel.test` function in `lawstat` package is well-known.
`brunnermunzel.test` in this package also accepts **formula**.

Moreover,
 this package provides
 `brunnermunzel.permutation.test` function
  for a permuted Brunner-Munzel test
  used in the case of small sample size.

The script of `brunnermunzel.test` is derived from
 that of `lawstat` package and slightly modified.
Thanks to Vyacheslav Lyubchich
 (maintainer of `lawstat` package).

# Installation

```r
library(devtools)
install_github("toshi-ara/brunnermunzel")
```

# Example
## Brunner-Munzel test

```r
## Pain score on the third day after surgery for 14 patients under
## the treatment Y and 11 patients under the treatment N
## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).

Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)

dat <- data.frame(
    value = c(Y, N),
    group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
                   levels = c("Y", "N"))
)

library(brunnermunzel)

## Default
brunnermunzel.test(Y, N)
```

```
## 
## 	Brunner-Munzel Test
## 
## data:  Y and N
## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
## 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y) 
##         0.788961
```

```r
## Formula interface
brunnermunzel.test(value ~ group, data = dat)
```

```
## 
## 	Brunner-Munzel Test
## 
## data:  value by group
## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
## 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y) 
##         0.788961
```

## permuted Brunner-Munzel test

`brunnermunzel.permutation.test` takes time to obtain results.


```r
library(brunnermunzel)

Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)

dat <- data.frame(
    value = c(Y, N),
    group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
                   levels = c("Y", "N"))
)

## Default
brunnermunzel.permutation.test(Y, N)
```

```
## 
## 	permuted Brunner-Munzel Test
## 
## data:  Y and N
## p-value = 0.008038
```

```r
## Formula interface
brunnermunzel.permutation.test(value ~ group, data = dat)
```

```
## 
## 	permuted Brunner-Munzel Test
## 
## data:  value by group
## p-value = 0.008038
```

### Comparison with other methods

```r
# Wilcoxon sum-rank test
wilcox.test(value ~ group, data = dat)
```

```
## Warning in wilcox.test.default(x = c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, :
## cannot compute exact p-value with ties
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  value by group
## W = 32.5, p-value = 0.007741
## alternative hypothesis: true location shift is not equal to 0
```

```r
# exact Wilcoxon sum-rank test
library(coin)
wilcox_test(value ~ group, data = dat, distribution = "exact")
```

```
## 
## 	Exact Wilcoxon-Mann-Whitney Test
## 
## data:  value by group (Y, N)
## Z = -2.6934, p-value = 0.00669
## alternative hypothesis: true mu is not equal to 0
```

