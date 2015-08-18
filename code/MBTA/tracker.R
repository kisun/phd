## function(s) for tracking a vehicle:

setupDatabase <- function() {
    system('sqlite3 trackers.db < tracker_setup.sql')
}

query <- function(sql, ..., alt.con = NULL) {
    dots <- list(...)
    dots <- lapply(dots, function(x) {
        if (length(x) > 1)
            paste0("('", paste(x, collapse = "', '"), "')")
        else
            x
    })
    dbGetQuery(if (is.null(alt.con)) con else alt.con, XX <- do.call(sprintf, c(sql, dots)))
}

pathDistance <- function(lat, lon) {
    ## Return the distance of a path defined by lat and lon.
    
    require(geosphere)

    coords <- cbind(lon, lat)
    from <- coords[-nrow(coords), ]
    to <- coords[-1, ]
    
    distGeo(from, to)
}

## AVL OBJECT + METHODS
getAVL <- function(vehicle.id, timestamp = NULL) {
    ## if timestamp is set, obtain AVL from historical data; otherwise from latest
    if (is.null(timestamp)) {
        AVL <- query("SELECT vehicle_id, trip_id, timestamp, position_latitude AS lat, position_longitude AS lon FROM vehicle_positions WHERE vehicle_id = '%s'", alt.con = dbConnect(SQLite(), "gtfs.db"), vehicle.id)
        
        if (nrow(AVL) == 0)
            stop("That vehicle isn't running. Maybe you want to specify a timestamp and use historical data?")
    } else {
        AVL <- query("SELECT vehicle_id, trip_id, timestamp, position_latitude AS lat, position_longitude AS lon FROM vehicle_positions WHERE vehicle_id='%s' AND timestamp='%s'", vehicle.id, timestamp)

        if (nrow(AVL) == 0)
            stop("No results found. Check the vehicle.id and timestamp are valid.")
    }

    createAVL(AVL$vehicle_id, AVL$trip_id, c(AVL$lat, AVL$lon), AVL$timestamp)
}
createAVL <- function(vehicle.id, trip.id, position, time) {
    out <- list(vehicle.id = vehicle.id,
                trip.id = trip.id,
                position = position,
                time = time)
    class(out) <- "gtfs.avl"
    out
}
print.gtfs.avl <- function(x, ...) {
    cat("Latest GTFS AVL report for vehicle", x$vehicle.id, "\n")
}

## TRACK OBJECT + METHODS
create <- function(x, ...) UseMethod("create")

create.gtfs.avl<- function(x) {
    AVL <- x
    shape_id <- query("SELECT shape_id FROM trips WHERE trip_id='%s'", AVL$trip.id,
                      alt.con = dbConnect(SQLite(), "gtfs.db"))

    if (nrow(query("SELECT shape_id FROM shapes WHERE shape_id='%s'", shape_id)) == 0) {
        shape <- query("SELECT shape_id, shape_pt_lat AS lat, shape_pt_lon AS lon, shape_pt_sequence AS seq FROM shapes WHERE shape_id='%s'", shape_id, alt.con = dbConnect(SQLite(), "gtfs.db"))

        shape$dist <- cumsum(c(0, pathDistance(shape$lat, shape$lon)))

        res <- dbWriteTable(con, "shapes", shape, append = TRUE)
        if (!res)
            stop("Unable to save shape to the database.")
    }
        
    
    dbSendQuery(
        con,
        sprintf("INSERT INTO tracks (vehicle_id, timestamp, pos_lat, pos_lon, trip_id, shape_id) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
                AVL$vehicle.id, AVL$time, AVL$position[1], AVL$position[2], AVL$trip.id, shape_id)
    )
    
    invisible(NULL)
}
getTrack <- function(vehicle_id) {
    track <- query("SELECT * FROM tracks WHERE vehicle_id='%s'", vehicle.id)

    out <- list(vehicle.id = track$vehicle_id,
                AVL = createAVL(track$vehicle_id, track$trip_id, c(track$pos_lat, track$pos_lon), track$timestamp),
                trip = track$trip_id,
                shape = track$shape_id,
                distance.into.trip = track$distance_into_trip,
                TPI = track$TPI,
                distance.into.TPI = track$distance_into_TPI,
                deviation = track$deviation,
                validity = track$validity,
                n.rejects = track$n.rejects)
    class(out) <- "gtfs.track"
    out
}

## Proposals
propose <- function(track, AVL) {
    ## propose DISTANCE INTO TRIP, TPI, DISTANCE INTO TPI (TRIP is known)

    ## 1. max distance bus could have moved since previous report?
    if (is.na(track$distance.into.trip)) {
        delta.time <- NA
        max.dist <- NA
    } else {
        delta.time <- AVL$time - track$AVL$time
        max.dist <- track$distance.into.trip + delta.time * (100 * 1000 / 60 / 60) - 2 * sigma  ## time (in minutes) * (speed in meters/s)
    }


    ## 2. starting location for checking:
    if (is.na(track$TPI)) {
        k0 <- 0
    } else {
        k0 <- track$TPI - (track$distance.into.TPI < sigma)
    }
    
}

track <- function(vehicle.id, timestamp = NULL, ) {
    ## vehicle.id: unique vehicle identifier
    ## timestamp: if not NULL, will be used to obtain historical data; otherwise, will use the latest GTFS report

    ## ERRORS
    sigma <- 100  # meters
    

    require(RSQLite)
    if (!"trackers.db" %in% list.files())
        setupDatabase()
    con <- dbConnect(SQLite(), "trackers.db")

    ## obtain the latest report:
    AVL <- getAVL(vehicle.id, timestamp)
    
    ## Does the vehicle exist in the trackers.db database?
    check <- query("SELECT vehicle_id FROM tracks WHERE vehicle_id='%s'", vehicle.id)
    if (nrow(check) == 0) create(AVL)
    
    track <- getTrack(vehicle.id)
    
    candidates <- propose(track, AVL)
}


vehicle.id <- "v2046"
timestamp <- NULL
