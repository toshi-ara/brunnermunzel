# ChangeLog
## version 1.2.0.9000- 
* bugfix (PR#9)
  * return appropriate values using non-overlapping data
    * estimate, confidence interval and P value
  * fix that confidence interval exceed 0 and 1
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

