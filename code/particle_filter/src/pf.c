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

/**
 * Transition particles forward.
 * @param d       distance into trip (m)
 * @param v       velocity (m/s)
 * @param s       stop index (1, 2, ..., L)
 * @param A       arrival time (at current/previous stop)
 * @param D       departure time from previous stop
 * @param r       segment index (1, 2, ..., M)
 * @param ts      timestamp (UNIX)
 * @param N       number of particles
 * @param delta   time since last observation
 * @param gamma   minimum dwell time
 * @param pi      pr(stop) at a bus stop
 * @param tau     average dwell time
 * @param rho     pr(stop) at intersection
 * @param upsilon average intersection dwell time
 * @param M       number of segments
 * @param L       number of stops
 * @param nu      mean speed in each segment
 * @param xi      variance of speed in each segment
 * @param Sd      stop distances into trip
 * @param Rd      segment distances into trip
 * @param sMAX    max speed
 * @param sMIN    min speed
 * @param seed    some random seed(s)
 */
void transition(double *d, double *v, int *s, double *A, double *D, int *r, double *ts, int *N,
		double *delta, double *gamma, double *pi, double *tau, double *rho, double *upsilon,
		int *M, int *L, double *nu, double *xi, double *Sd, double *Rd,
		double *sMAX, double *sMIN,
		int *seed)
{
  srand((unsigned) seed[0]);

  for (int i=0;i<*N;i++) {
    if (r[i] >= *M | s[i] >= *L - 1) {
      continue;
    }
    double tr = *delta;

    // BEGIN: are we currently waiting at a bus stop/traffic lights?
    double wait = 0;
    // are we at a bus stop?
    if (isnan(D[i]) && !isnan(A[i])) {
      // Yes, we are at a bus stop!
      if (*ts - A[i] < *gamma) {
      	// must wait AT LEAST gamma seconds:
      	wait += *gamma - *ts + A[i];
      }
      // sample random dwell time - memoryless, so start waiting from 0
      wait += rexp(*tau);
      tr -= wait;

      if (tr <= 0) {
        continue;
      }
      // bus has time left: update departure time:
      D[i] = A[i] + wait;

    } else if (d[i] > Rd[r[i]+1] - 50) {
      // within 50m of the next intersection
      wait += (runif() < *rho ? 1 : 0) * rexp(*upsilon);
      tr -= wait;

      if (tr <= 0) {
        continue;
      }
    } // end check bus-stop/intersection

    // Add system noise to speed ("within segment" variation)
    v[i] = truncated_normal_ab_sample(v[i], 2, *sMIN, *sMAX, &seed[i]);

    // Now we just keep moving forward, piece by piece, until time runs out
    while (tr > 0) {
      // distance until the next STOP, and SEGMENT:
      double ds = Sd[s[i]+1];
      double dr = Rd[r[i]+1];

      // look for approaching stops first:
      if (ds <= dr) { // -------------------------------- APPROACHING STOP
        double eta = (ds - d[i]) / v[i]; // ETA to stop
        if (tr > eta) {
          // enough time to reach the stop
          tr -= eta;
          d[i] = ds; // bus is AT the stop ...
          s[i]++; // move on to next STOP

          /**
           * Set arrival time:
           * if departure time set, then ETA is from then; otherwise from *now*
           */
          if (isnan(D[i])) {
        	  A[i] = *ts + eta;
        	} else {
        	  A[i] = (*ts > D[i] ? *ts : D[i]) + eta;
        	}
        	D[i] = 0.0 / 0.0; // set departute time to NaN

          if (s[i] >= *L - 1) {
            // we're at the end of the line!
        	  break;
        	}

          // DWELL time
        	double tbar = (runif() < *pi ? 1 : 0) * (*gamma + rexp(*tau));
        	tr -= tbar;
          if (tr > 0) {
        	  // still time left --- drive off (setting departure time!)
        	  D[i] = A[i] + tbar;
          }
        } else {
          d[i] += tr * v[i];
        	tr = 0.0;
        }
      } else { // --------------------------------------- APPROACHING INTERSECTION
        double eta = (dr - d[i]) / v[i]; // ETA to next intersection
        if (tr > eta) {
          // enough time to reach intersection
          tr -= eta;
          if (runif() < *rho) {
            // yep, the bus gets caught at the intersection!
            d[i] = fmax(d[i], dr - runif() * 50); // within 50m of intersection
            tr -= rexp(*upsilon);
          } else {
            d[i] = dr;
            r[i]++; // move on to next segment

            if (r[i] >= *M) {
              break;
            }

            // need to adjust speed for the new segment!
            v[i] = truncated_normal_ab_sample(v[i], 6, *sMIN, *sMAX, &seed[i]);

          }
        } else {
          d[i] += tr * v[i];
        	tr = 0.0;
        }
      } // end of if/else (stop/intersection)
    } // end of while[tr > 0]
  } // end of for[i]
} // end of function

      	  // adjust speed for the new segment
      	  /*double speedProposal = truncated_normal_ab_sample(v[i], 6, *sMIN, *sMAX, &seed[i]);
      	  double alpha = truncated_normal_ab_pdf(speedProposal, nu[s[i]], xi[s[i]], *sMIN, *sMAX) /
      	    truncated_normal_ab_pdf(v[i], nu[s[i]-1], xi[s[i]-1], *sMIN, *sMAX);
      	  if (runif() < alpha) {
      	    v[i] = speedProposal;
      	    }*/
