# ChangeLog
## version 1.3.1.9000
* fix typo

## version 1.3.1 (2019-03-11)
* rewrote completely with FORTRAN (PR#12)
  * calculation of P value

## version 1.3.0
* bugfix (PR#9)
  * return appropriate values using non-overlapping data
    * estimate, confidence interval and P value
  * fix to return NA when using too large sample size
* rewrote brunnermunzel.test with FORTRAN (PR#10)

## version 1.2.0 (2019-02-28)
* initial release
* extensive function from `brunner.munzel.test` in `lawstat` package
  * Functions accept formula
  * Functions accept matrix and table (PR#3)
  * add option to `brunnermunzel.test` (PR#8)
    * `perm`: perform permuted Brunner-Munzel test
  * add option to `brunnermunzel.permutaion.test` (PR#8)
    * `force`: perform permuted Brunner-Munzel test
               whenever sample size is large
  * add vignette (PR#7)

