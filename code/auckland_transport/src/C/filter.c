#include <R.h>
#include <Rinternals.h>

SEXP wtMean(SEXP x, SEXP w) {
  int n = length(x);
  SEXP mu = PROTECT(allocVector(REALSXP, 1));
  double wt = 0;

  REAL(mu)[0] = 0;
  for(int i=0; i<n; i++) {
    REAL(mu)[0] += REAL(x)[i] * REAL(w)[i];
    wt = wt + REAL(w)[i];
  }
  REAL(mu)[0] = REAL(mu)[0] / wt;

  UNPROTECT(1);

  return mu;  
}
