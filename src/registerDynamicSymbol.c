// RegisteringDynamic Symbols

#include <Rinternals.h>

void R_init_brunnermunzel(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
