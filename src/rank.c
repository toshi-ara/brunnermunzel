#include <R_ext/Utils.h>
#include <R_ext/RS.h>

void rank(int *n, double *v, double *rk) {

  int i, j, k;
  int idx[*n];
  double ave;

  for (i = 0; i < *n; i++) {
    idx[i] = i + 1;
  }

  /* quicksort in R */
  R_qsort_I(v, idx, 1, *n);

  /* do_rank in R */
  for (i = 0; i < *n; i = j + 1) {
    j = i;
    while ((j < *n - 1) && (v[j] == v[j + 1])) {
      j++;
    }

    ave = (i + j + 2.0) * 0.5;
    for (k = i; k <= j; k++) {
      rk[idx[k] - 1] = ave;
    }
  }
}


#ifdef R_RS_H
void F77_NAME(rank)(int *n, double *v, double *rk) {
  rank(n, v, rk);
}
#endif

