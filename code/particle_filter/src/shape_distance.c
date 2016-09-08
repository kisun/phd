#include <stdio.h>
#include <stdlib.h>
#include <libpq-fe.h>
#include <math.h>

void do_exit(PGconn *conn) {

  PQfinish(conn);
  exit(1);

}

double deg2rad(double);
double rad2deg(double);

/**
 * Compute teh distance between two coordinates, in meters
 * @param  lat1 latitude value of the first point
 * @param  lon1 longitude value of the first point
 * @param  lat2 latitude value of the second point
 * @param  lon2 longitude value of the second point
 * @return      double, the distance, in meters, between the points
 */
double distance(double lat1, double lon1, double lat2, double lon2) {
  double theta, dist;
  double R = 6371e3;

  theta = lon1 - lon2;
  dist = sin(deg2rad(lat1)) * sin(deg2rad(lat2)) +
         cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * cos(deg2rad(theta));
  dist = acos(dist);
  // dist = rad2deg(dist);
  dist = dist * R;

  return dist;
}

/**
 * Convert degrees to radians
 * @param  deg value in degrees
 * @return     double, value in radians
 */
double deg2rad(double deg) {
  return (deg * M_PI / 180);
}

/**
 * Convert radians to degrees
 * @param  deg value in radians
 * @return     double, value in degrees
 */
double rad2deg(double rad) {
  return (rad * 180 / M_PI);
}

/**
 * Get a single shape file from the database.
 * @param  conn a db connection
 * @param  id   shape id
 * @return      0
 */
int GetShape(PGconn *conn, char *id) {
  const char *paramValues[1];
  paramValues[0] = id;

  char *stm = "SELECT * FROM shapes WHERE shape_id=$1";
  PGresult *res = PQexecParams(conn, stm, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    printf("No data retrieved\n");
    PQclear(res);
    return 1;
  }

  int len = PQntuples(res);
  double shapeDist = 0;
  // double cumDist[len];

  for (int j=0; j<len; j++) {
    if (j > 0) {
      shapeDist += distance(strtod(PQgetvalue(res, j-1, 0), NULL), strtod(PQgetvalue(res, j-1, 1), NULL),
                            strtod(PQgetvalue(res, j, 0), NULL), strtod(PQgetvalue(res, j, 1), NULL));
    }
    // cumDist[j] = shapeDist;

    // Update the value in the database:
    // char cumdist[50];
    // char seq[5];
    // sprintf(cumdist, "%lf", shapeDist);
    // sprintf(seq, "%d", j - 1);
    // char *upd = "UPDATE shapes SET shape_dist_traveled=$1 WHERE shape_id=$2 AND shape_pt_sequence=$3";
    // const char *params[3] = {
    //   cumdist, id, seq
    // };
    //
    // PGresult *ures = PQexecParams(conn, upd, 3, NULL, params, NULL, NULL, 0);
    // PQclear(ures);
  }

  // char values = "('1', '1', '0'), ('1', '2', '100')";
  // char upd = sprintf("UPDATE shapes set shape_dist_traveled = c.dist
  //                     FROM (values %s) as c(id, seq, dist)
  //                     WHERE c.id = shapes.shape_id AND c.seq = shapes.shape_pt_sequence",
  //                    values);
  // printf("%s", upd);
  PQclear(res);

  return 0;
}

int main() {
  PGconn *conn = PQconnectdb("user=homestead password=secret host=localhost port=5432 dbname=homestead");

  if (PQstatus(conn) == CONNECTION_BAD) {

    fprintf(stderr, "Connection to database failed: %s\n",
            PQerrorMessage(conn));
    do_exit(conn);

  }

  PGresult *res = PQexec(conn, "SELECT DISTINCT shape_id FROM shapes");

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {

    printf("No data retrieved\n");
    PQclear(res);
    do_exit(conn);

  }

  int rows = PQntuples(res);
  char *shapeid;
  for (int i=0; i<rows; i++) {
    printf("Shapes %03d of %d ...\r", i+1, rows);
    fflush(stdout);
    shapeid = PQgetvalue(res, i, 0);
    GetShape(conn, shapeid);
  }
  printf("\nDone\n");

  PQclear(res);
  PQfinish(conn);

  return 0;
}
