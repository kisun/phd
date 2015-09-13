### Functions for working with GTFS data
### Assumed to be in an SQLite database with GTFS and GTFS-Realtime data

if (!"package:RSQLite" %in% search()) require(RSQLite)
con <- dbConnect(SQLite(), "gtfs-historical.db")

gtfsQuery <- function(table, cols, rows, ..., order, debug = FALSE) {
    qry <- paste0("SELECT ", cols,
                  paste(" FROM", table),
                  if (!missing(rows)) {
                      paste0(" WHERE ", rows)
                  },
                  if (!missing(order)) {
                      paste0(" ORDER BY ", order)
                  })
    
    query(con, qry, ..., debug = debug)
}



drawRoute <- function(id, ..., new = TRUE) {
    r <- gtfsQuery("shapes", "shape_pt_lat AS y, shape_pt_lon AS x",
                   "shape_id=%s", id,
                   order = "shape_pt_sequence")
    if (new)
        plot(r$x, r$y, type = "n", asp = 1)
    
    lines(r$x, r$y, ...)

    points(r$x[1], r$y[1], pch = 19, cex = 0.5)
    points(r$x[nrow(r)], r$y[nrow(r)], pch = 19, cex = 0.5, col = "red")
}
