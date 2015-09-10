### Functions for working with GTFS data
### Assumed to be in an SQLite database with GTFS and GTFS-Realtime data

if (!"package:RSQLite" %in% search()) require(RSQLite)
con <- dbConnect(SQLite(), "gtfs-historical.db")

gtfsDefault <- function(table, cols, rows, ..., order) {
    qry <- paste0("SELECT ", cols,
                  paste(" FROM", table),
                  if (!missing(rows)) {
                      paste0(" WHERE ", rows)
                  },
                  if (!missing(order)) {
                      paste0(" ORDER BY ", order)
                  })

    query(con, qry, ...)
}

gtfsRoutes <- function(cols = "*", rows, ..., order)
    gtfsDefault("routes", cols, rows, ..., order)

gtfsTrips <- function(cols = "*", rows, ..., order)
    gtfsDefault("trips", cols, rows, ..., order)
