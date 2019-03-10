#include <R_ext/RS.h>
#include <R_ext/Utils.h> /* for qsort4 */
#include <Rmath.h>

double F77_SUB(rf_pt)(double *q, double *df, int *lower_tail) {
  return pt(*q, *df, *lower_tail, 0);
}

double F77_SUB(rf_qt)(double *p, double *df, int *lower_tail) {
  return qt(*p, *df, *lower_tail, 0);
}
