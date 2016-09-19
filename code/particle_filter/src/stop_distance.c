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
  // printf("%s\n", id);

  // Get the shape for the trip:
  paramValues[0] = sid;
  char *stm2 = "SELECT lat, lon, dist_traveled "
               "FROM shapes AS sh WHERE sh.id=$1 AND dist_traveled is null ORDER BY pt_sequence";
  PGresult *res2 = PQexecParams(conn, stm2, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res2) != PGRES_TUPLES_OK) {
    printf("No data retrieved (shape)\n");
    PQclear(res2);
    return 1;
  }

  // compute segment lenths, bearings, and cumulative lengths:
  int slen = PQntuples(res2);
  // char *cumdist[slen];
  //
  // for (int j=0; j<slen; j++) {
  //   if (j+1 < slen) {
  //     cumdist[j] = PQgetvalue(res2, j, 2);
  //   }
  // }

  // For each stop, find the minimum distance to every segment ...
  int len = PQntuples(res);
  int valLen = 2 + strlen(id) + 1 + 4 + 1 + 9 + 1 + 2;
  int valuesLen = valLen * len;
  char values[valuesLen];

  int SEG = 0;
  for (int j=0; j<len; j++) {
    double lat, lon;
    lat = strtod(PQgetvalue(res, j, 1), NULL);
    lon = strtod(PQgetvalue(res, j, 2), NULL);

    for (int k=SEG; k<slen; k++) {
      double d = distance(lat, lon, strtod(PQgetvalue(res2, k, 0), NULL), strtod(PQgetvalue(res2, k, 1), NULL));
      if (d < 0.1) {
        SEG = k;

        if (j > 0) {
          sprintf(values, "%s, ('%s',%d,%s)", values, id, j+1, PQgetvalue(res2, k, 2));
        } else {
          sprintf(values, "('%s',%d,%s)", id, j+1, PQgetvalue(res2, k, 2));
        }

        break;
      }
    }
  }

  char *upd = "UPDATE stop_times SET shape_dist_traveled = c.dist " // 51
              "FROM (values %s) as c(id, seq, dist) " // 35
              "WHERE c.id = stop_times.trip_id AND c.seq = stop_times.stop_sequence"; // 59
  char qry[valuesLen + strlen(upd)];

  sprintf(qry, upd, values);

  // printf("%s", qry);

  PGresult *ures = PQexec(conn, qry);

  PQclear(ures);

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

  PGresult *res = PQexec(conn, "SELECT id, shape_id FROM trips");

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {

    printf("No data retrieved\n");
    PQclear(res);
    do_exit(conn);

  }

  int rows = PQntuples(res);
  char *tripid, *shapeid;
  for (int i=0; i<rows; i++) {
    printf("%05d of %d\r", i+1, rows);
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
