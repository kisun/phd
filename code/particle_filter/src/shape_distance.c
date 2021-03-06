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
 * Calculate cumulative distance into trip for a shapfile.
 * @param  conn a db connection
 * @param  id   shape id
 * @return      0
 */
int GetShape(PGconn *conn, char *id) {
  const char *paramValues[1];
  paramValues[0] = id;

  char *stm = "SELECT lat, lon, pt_sequence FROM shapes WHERE id=$1 ORDER BY pt_sequence";
  PGresult *res = PQexecParams(conn, stm, 1, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    printf("No data retrieved\n");
    PQclear(res);
    return 1;
  }

  int len = PQntuples(res);
  double shapeDist = 0;

  int chunks = 1 + ((len - 1) / 1000); // len mod 1000
  // printf("=====================================\nShape %s: N = %d\n", id, len+1);

  for (int k=0; k<chunks; k++) {
    int start = 1000 * k;
    // int stop = 1000 * (k + 1) - 1;
    int Len = 1000;

    if (k + 1 == chunks) {
      // stop = len;
      Len = len - k * 1000;
    }

    // printf("  Chunk %d of %d: %d-%d\n", k + 1, chunks, start+1, stop+1);

    char cumDist[Len][10];

    for (int j=0; j<Len; j++) {
      if (j > 0 || k > 0) {
        // printf("(%lf,%lf) -> (%lf, %lf)  | ",
              //  strtod(PQgetvalue(res, start+j-1, 0), NULL), strtod(PQgetvalue(res, start+j-1, 1), NULL),
              //  strtod(PQgetvalue(res, start+j, 0), NULL), strtod(PQgetvalue(res, start+j, 1), NULL));
        shapeDist += distance(strtod(PQgetvalue(res, start+j-1, 0), NULL), strtod(PQgetvalue(res, start+j-1, 1), NULL),
                              strtod(PQgetvalue(res, start+j, 0), NULL), strtod(PQgetvalue(res, start+j, 1), NULL));
        // printf("%05.03lf", distance(strtod(PQgetvalue(res, start+j-1, 0), NULL), strtod(PQgetvalue(res, start+j-1, 1), NULL),
                              // strtod(PQgetvalue(res, start+j, 0), NULL), strtod(PQgetvalue(res, start+j, 1), NULL)));
      }
      sprintf(cumDist[j], "%05.3lf", shapeDist);
      // printf(" | %05.3lf\n", shapeDist);
    }

    // length of the value items wil be 1 + 1 + len(id) + 1 + 1 + 4 + 1 + 9 +  1 + 1
    //                                  (      'SHAPE_ID`     ,  SSEQ ,xxxxx.xx)   ,
    int valLen = 2 + strlen(id) + 1 + 4 + 1 + 9 + 1 + 2;
    int valuesLen = valLen * Len;
    char values[valuesLen];

    for (int j=0; j<Len; j++) {
      if (j > 0) {
        sprintf(values, "%s, ('%s',%d,%s)", values, id, start + j + 1, cumDist[j]);
      } else {
        sprintf(values, "('%s',%d,%s)", id, start + j + 1, cumDist[j]);
      }
    }

    char *upd = "UPDATE shapes SET dist_traveled = c.dist " // 47
                "FROM (values %s) as c(id, seq, dist) " // 35
                "WHERE c.id = shapes.id AND c.seq = shapes.pt_sequence"; // 65
    char qry[valuesLen + strlen(upd)];

    sprintf(qry, upd, values);

    // printf("shape %s: %lf\n", id, shapeDist);

    PGresult *ures = PQexec(conn, qry);

    PQclear(ures);
  }

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

  PGresult *res = PQexec(conn, "SELECT DISTINCT id FROM shapes WHERE dist_traveled IS NULL");

  if (PQresultStatus(res) != PGRES_TUPLES_OK) {

    printf("No data retrieved\n");
    PQclear(res);
    do_exit(conn);

  }

  int rows = PQntuples(res);
  char *shapeid;
  for (int i=0; i<rows; i++) {
    printf("%03d of %d\r", i+1, rows);
    shapeid = PQgetvalue(res, i, 0);
    GetShape(conn, shapeid);
    fflush(stdout);
  }
  printf("\n\nDone\n");

  PQclear(res);
  PQfinish(conn);

  return 0;
}
