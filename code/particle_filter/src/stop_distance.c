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
  char *stm = "SELECT st.stop_id, s.lat, s.lon "
              "FROM stop_times AS st, stops AS s "
              "WHERE trip_id=$1 AND st.stop_id=s.id ORDER BY stop_sequence";
  PGresult *res = PQexecParams(conn, stm, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    printf("No data retrieved (stops)\n");
    PQclear(res);
    return 1;
  }

  // Get the shape for the trip:
  paramValues[0] = sid;
  char *stm2 = "SELECT lat, lon, dist_traveled "
               "FROM shapes AS sh WHERE sh.id=$1 ORDER BY pt_sequence";
  PGresult *res2 = PQexecParams(conn, stm2, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res2) != PGRES_TUPLES_OK) {
    printf("No data retrieved (shape)\n");
    PQclear(res2);
    return 1;
  }

  // compute segment lenths, bearings, and cumulative lengths:
  int slen = PQntuples(res2);
  double cumdist[slen];

  for (int j=0; j<slen; j++) {
    if (j+1 < slen) {
      cumdist[j] = strtod(PQgetvalue(res2, j, 2), NULL);
    }
  }

  // For each stop, find the minimum distance to every segment ...
  int len = PQntuples(res);
  int SEG = 1;

  for (int j=0; j<len; j++) {
    double lat, lon;
    lat = strtod(PQgetvalue(res, j, 1), NULL);
    lon = strtod(PQgetvalue(res, j, 2), NULL);

    printf("\n*** Stop %d:\n", j+1);

    for (int k=SEG; k<slen; k++) {
      double d = distance(lat, lon, strtod(PQgetvalue(res2, k, 0), NULL), strtod(PQgetvalue(res2, k, 1), NULL));
      if (d < 0.1) {
        printf("seg %d, %lf, %lf\n", k, d, cumdist[k-1]);
        SEG = k;

        char *str = "UPDATE stop_times SET shape_dist_traveled='%05.03lf' WHERE trip_id='%s' AND stop_sequence='%d'";
        char qry[strlen(str) + 9 + strlen(id) + 4];
        sprintf(qry, str, cumdist[k-1], id, j+1);

        printf("%s\n", qry);
        PGresult *ures = PQexec(conn, "SELECT id, shape_id FROM trips LIMIT 1");
        PQclear(ures);

        break;
      }
    }
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

  PGresult *res = PQexec(conn, "SELECT id, shape_id FROM trips LIMIT 1");

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {

    printf("No data retrieved\n");
    PQclear(res);
    do_exit(conn);

  }

  int rows = PQntuples(res);
  char *tripid, *shapeid;
  for (int i=0; i<rows; i++) {
    // printf("%03d of %d\n", i+1, rows);
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
