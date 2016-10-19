#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <string.h>

#include "truncated_normal.h"

double runif()
{
  return ((double) rand() / RAND_MAX);
}

double rexp(double mean)
{  
  double u = runif();
  return - log(u) * mean;
}


void transition(double *d, double *v, int *s, double *A, double *D, double *ts, int *N,
		double *delta, double *gamma, double *pi, double *tau, double *rho, double *upsilon,
		int *M, double *nu, double *xi, double *Sd,
		double *sMAX, double *sMIN,
		int *seed)
{
  srand((unsigned) seed[0]);
  
  for (int i=0;i<*N;i++) {
    if (s[i] >= *M) continue;
    double tr = *delta;
    
    // first off, the bus might be stuck at lights or at a stop
    double wait = 0;
    if (isnan(D[i]) && !isnan(A[i])) {
      // The bus is at a stop.
      if (*ts - A[i] < *gamma) {
	// must wait AT LEAST gamma seconds:
	wait += *gamma - *ts + A[i];
      }

      // sample random dwell time
      wait += rexp(*tau);
      tr -= wait;

      if (tr <= 0) {
	continue;
      }

      // bus has left: update departure time:
      D[i] = A[i] + wait;
    } else {
      // The bus is not at a stop ...
      wait += (runif() < *rho ? 1 : 0) * rexp(*upsilon);
      tr -= wait;
    
      if (tr <= 0) {
	continue;
      }
    }
    
    // Add noise
    v[i] = truncated_normal_ab_sample(v[i], 2, *sMIN, *sMAX, &seed[i]);

    while (tr > 0) {
      double ds = Sd[s[i]];

      double eta = (ds - d[i]) / v[i];

      if (tr > eta) {
	tr -= eta;
	// Bus will reach the next stop; dwell time!
	d[i] = ds;
	s[i] = s[i] + 1;
	if (isnan(D[i])) {
	  A[i] = *ts + eta;
	} else {
	  A[i] = (*ts > D[i] ? *ts : D[i]) + eta;
	}
	D[i] = 0.0 / 0.0;

	if (s[i] >= *M) {
	  break;
	}
      
	// DWELL time
	double tbar = (runif() < *pi ? 1 : 0) * (*gamma + rexp(*tau));
	tr -= tbar;
      
	if (tr > 0 ) {
	  // still time left --- drive off!
	  D[i] = A[i] + tbar;
	  // adjust speed for the new segment
	  double speedProposal = truncated_normal_ab_sample(v[i], 6, *sMIN, *sMAX, &seed[i]);
	  double alpha = truncated_normal_ab_pdf(speedProposal, nu[s[i]], xi[s[i]], *sMIN, *sMAX) /
	    truncated_normal_ab_pdf(v[i], nu[s[i]-1], xi[s[i]-1], *sMIN, *sMAX);
	  if (runif() < alpha) {
	    v[i] = speedProposal;
	  }
	}
            
      } else {
	// Bus isn't going to reach it ... stop wherever it gets to
	d[i] += tr * v[i];
	tr = 0.0;
      }
    }
  }
}
