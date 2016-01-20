## functions for reading GTFS database

getPositions <- function(con, route.id, vehicle.id, date,
                         order = "timestamp", verbose = TRUE, ...) {
    ## SQL preparation ...
    where <- character()
    sql <- "SELECT * FROM vehicle_positions"

    if (!missing(route.id))
        where <- c(where, sprintf("route_id LIKE '%s%s'", route.id, "%"))
    if (!missing(vehicle.id))
        where <- c(where, sprintf("vehicle_id = '%s'", vehicle.id))
    if (!missing(date)) {
        ## convert it to MIN and MAX timestamp
        datets <- as.numeric(format(as.POSIXct(date), format = "%s")) + c(0, 86400)
        where <- c(where, sprintf("timestamp >= %s AND timestamp < %s", datets[1], datets[2]))
    }

    if (length(where))
        sql <- paste0(sql, " WHERE ", paste0(where, collapse = " AND "))

    if (verbose)
        cat(sql, "\n")

    ## SQL call 
    pos <- dbGetQuery(con, sql)

    ## Value fixes
    pos$trip_start_date <- ifelse(nchar(pos$trip_start_date) == 0,
                                  tsDate(pos$timestamp),
                                  pos$trip_start_date)
    strip.cols <- c("trip_id", "route_id")
    #versions <- lapply(pos[, strip.cols], function(x) gsub(".+_v", "", x))
    #print(do.call(cbind, versions))
    pos[, strip.cols] <- lapply(pos[, strip.cols], function(x) gsub("-.+", "", x))

    pos
}



getTrips <- function(ids, con = "db/gtfs-static.db", verbose = TRUE,
                     ...,
                     .con = dbConnect(SQLite(), con)) {
    ## Get all of the information for a bunch of trips:
    sql <- sprintf("SELECT DISTINCT t.trip_id, route_id, shape_id, trip_headsign, service_id, departure_time
FROM trips as t, stop_times as s
WHERE t.trip_id=s.trip_id AND t.trip_id IN ('%s') AND s.stop_sequence=1
ORDER BY departure_time",
                   paste(ids, collapse = "','"))
    
    if (verbose) cat(sql, '\n')

    dbGetQuery(.con, sql)
}


getBlocksA <- function(date, con = "db/gtfs-history.db", verbose = TRUE,
                       ...,
                       .con = dbConnect(SQLite(), con)) {
    ## Grab all vehicle-blocks on a given date:

    datets <- as.numeric(format(as.POSIXct(date), format = "%s")) + c(0, 86400)
    
    sql <- sprintf("SELECT DISTINCT vehicle_id, trip_id, min(trip_start_time) AS start FROM vehicle_positions
WHERE timestamp >= %s AND timestamp < %s
GROUP BY vehicle_id, trip_id
ORDER BY vehicle_id, start", datets[1], datets[2])

    if (verbose) cat(sql, '\n')

    blocks <- dbGetQuery(.con, sql)
    blocks$trip_id <- gsub("-.+", "", blocks$trip_id)

    ## also get route numbers ...
    trps <- paste(blocks$trip_id, collapse = "','")
    sql <- sprintf("SELECT trip_id, route_id, trip_headsign, shape_id, service_id
FROM trips WHERE trip_id IN ('%s') GROUP BY trip_id", trps)

    trips <- dbGetQuery(dbConnect(SQLite(), "db/gtfs-static.db"), sql)

    merge(blocks, trips, by = "trip_id", all.x = TRUE, all.y = FALSE, sort = FALSE)
}
