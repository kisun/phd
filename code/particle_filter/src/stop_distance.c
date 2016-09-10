#include <stdio.h>
#include <stdlib.h>
#include <libpq-fe.h>
#include <math.h>
#include <string.h>

#include "distance.h"

void do_exit(PGconn *conn) {

  PQfinish(conn);
  exit(1);

}

/**
  * Calculate distance into trip of stops along a route
  * @param  conn a db connection
  * @param  id   trip id
  * @return      0
  */
int CalcStopDist(PGconn *conn, char *id, char *sid) {
  const char *paramValues[1];
  paramValues[0] = id;

  printf("\nSHAPEID = %s\n", sid);

  // Get the stops for the trip:
  char *stm = "SELECT st.stop_id, s.stop_lat, s.stop_lon "
              "FROM stop_times AS st, stops AS s "
              "WHERE trip_id=$1 AND st.stop_id=s.stop_id ORDER BY stop_sequence";
  PGresult *res = PQexecParams(conn, stm, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    printf("No data retrieved (stops)\n");
    PQclear(res);
    return 1;
  }

  // Get the shape for the trip:
  paramValues[0] = sid;
  char *stm2 = "SELECT shape_pt_lat, shape_pt_lon, shape_dist_traveled "
               "FROM shapes AS sh WHERE sh.shape_id=$1 ORDER BY shape_pt_sequence";
  PGresult *res2 = PQexecParams(conn, stm2, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res2) != PGRES_TUPLES_OK) {
    printf("No data retrieved (shape)\n");
    PQclear(res2);
    return 1;
  }

  // compute segment lenths, bearings, and cumulative lengths:
  int slen = PQntuples(res2);
  double lengths[slen];
  double bearings[slen];
  double cumdist[slen];

  for (int j=0; j<slen; j++) {
    if (j+1 < slen) {
      lengths[j] = strtod(PQgetvalue(res2, j+1, 2), NULL) - strtod(PQgetvalue(res2, j, 2), NULL);
      bearings[j] = bearing(strtod(PQgetvalue(res2, j, 0), NULL), strtod(PQgetvalue(res2, j, 1), NULL),
                            strtod(PQgetvalue(res2, j+1, 0), NULL), strtod(PQgetvalue(res2, j+1, 1), NULL));
      cumdist[j] = strtod(PQgetvalue(res2, j, 2), NULL);
    }
  }

  // For each stop, find the minimum distance to every segment ...
  int len = 1; //PQntuples(res);
  // int SEG = 1;
  // double shapeDist = 0;

  for (int j=0; j<len; j++) {
    double lat, lon;
    lat = strtod(PQgetvalue(res, j, 1), NULL);
    lon = strtod(PQgetvalue(res, j, 2), NULL);

    // int seg_N;
    // double d_squared, r_squared;

    for (int k=1; k<slen; k++) {
      // Center the shape on the stop, points as [x, y]:
      double q1[2] = {(strtod(PQgetvalue(res2, k-1, 1), NULL) - lon) * cos(deg2rad(lat)),
                      (strtod(PQgetvalue(res2, k-1, 0), NULL) - lat)};
      double q2[2] = {(strtod(PQgetvalue(res2, k, 1), NULL) - lon) * cos(deg2rad(lat)),
                      (strtod(PQgetvalue(res2, k, 0), NULL) - lat)};

      // Reference point becomes (0, 0):
      double v[2] = {(q2[0] - q1[0]), (q2[1] - q1[1])};  // q2 - q1
      double w[2] = {(q1[0]), (q1[1])};          // p - q1
      // |w| and |v| - i.e., their lengths (in meters using Equirectangular approximation)
      // double wlen = 6371000 * sqrt(pow(deg2rad(w[0]), 2) + pow(deg2rad(w[1]), 2));
      // double vlen = 6371000 * sqrt(pow(deg2rad(v[0]), 2) + pow(deg2rad(v[1]), 2));

      double wv = w[0] * v[0] + w[1] * v[1];
      double vv = pow(v[0], 2) + pow(v[1], 2);
      double ww = pow(w[0], 2) + pow(w[1], 2);
      double d, d2, r, r2;
      if (wv < 0) {
        // stop lies BEFORE this segment ... skip
        d = 0;
        r = sqrt(ww);
        // j++;
        // continue;
      } else if (wv <= vv) {
        printf("\n    segment %d: ", k);
        printf("[2] | wv = %lf, vv = %lf, ww = %lf", wv, vv, ww);
        // stop projects onto the segment ...
        d2 = pow(wv, 2) / vv;
        r2 = ww - d2;
        d = 6371000 * sqrt(d2);
        r = 6371000 * sqrt(r2);
        printf(", d = %lf, r = %lf", d, r);
      } else {
        r2 = pow(w[0] - v[0], 2) + pow(w[1] - v[1], 2);
        d = sqrt(vv);
        r = sqrt(r2);
      }


    }

    // SEG = 0; // wherever we end up ...
  }

  return 0;
}


/**
  * compute distance into trip of STOPS (in stop_times table)
  */
int main() {

  PGconn *conn = PQconnectdb("user=homestead password=secret host=localhost port=5432 dbname=homestead");

  if (PQstatus(conn) == CONNECTION_BAD) {

    fprintf(stderr, "Connection to database failed: %s\n",
            PQerrorMessage(conn));
    do_exit(conn);

  }

  PGresult *res = PQexec(conn, "SELECT trip_id, shape_id FROM trips WHERE trip_id IN (SELECT DISTINCT trip_id FROM stop_times LIMIT 1)");

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {

    printf("No data retrieved\n");
    PQclear(res);
    do_exit(conn);

  }

  int rows = PQntuples(res);
  char *tripid, *shapeid;
  for (int i=0; i<rows; i++) {
    printf("%03d of %d\n", i+1, rows);
    tripid = PQgetvalue(res, i, 0);
    shapeid = PQgetvalue(res, i, 1);
    CalcStopDist(conn, tripid, shapeid);
    fflush(stdout);
  }
  printf("\n\nDone\n");

  PQclear(res);
  PQfinish(conn);

  return 0;
}
