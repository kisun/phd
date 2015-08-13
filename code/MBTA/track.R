require(lubridate)

list.vehicles <- function() {
    dbGetQuery(dbConnect(SQLite(), "gtfs.db"),
               "SELECT vehicle_id FROM vehicle_positions")
}
AVLreport <- function(N) {
    res <- dbGetQuery(dbConnect(SQLite(), "gtfs.db"),
                      sprintf("SELECT timestamp AS t, position_longitude AS lon, position_latitude AS lat, vehicle_id AS N, trip_id AS B FROM vehicle_positions WHERE vehicle_id='%s'", N))
    if (nrow(res) < 1)
        stop("That vehicle doesn't exit in the latest GTFS report.")

    res
}


track <- function(x) {
    if (class(x) == "track") {
        ## Vehicle identified, N
        N <- track$N
    } else if (missing(x)) {
        stop("Provide either a track, or vehicle ID.")
    }
    
    ## Last associated AVL report
    AVL <- AVLreport(N)
    
    ## Trip, T
    T <- AVL$B
    
    ## Distance into trip, z
    z <- NA
    
    ## TPI
    TPI <- NA
    
    ## distance-into-TPI, d
    d <- NA
    
    ## Deviation, delta.t
    delta.t <- NA
    
    ## Validity
    validity <- FALSE
    
    ## number of rejected updates
    R <- 0

    out <- list(N = N,
                AVL = AVL,
                T = structure(c(T), class = "trip"),
                z = z,
                TPI = TPI,
                d = d,
                delta.t = delta.t,
                validity = validity)
    class(out) <- "track"
    out
}

shape <- function(x, ...)  UseMethod("shape")
shape.track <- function(x, ...)
    dbGetQuery(dbConnect(SQLite(), "gtfs.db"),
               sprintf("SELECT shape_pt_lon AS lon, shape_pt_lat AS lat FROM shapes WHERE shape_id=(SELECT shape_id FROM trips WHERE trip_id='%s')", x$T))

schedule <- function(x, ...) UseMethod("schedule")
schedule.track <- function(x, ...) {
    schedule(x$T)
}
schedule.trip <- function(x, ...) {
    dbGetQuery(dbConnect(SQLite(), "gtfs.db"),
               sprintf("SELECT stop_times.stop_id, arrival_time AS arrive, departure_time AS depart, stop_sequence, timepoint, stops.stop_lat AS lat, stops.stop_lon AS lon FROM stop_times, stops WHERE stop_times.stop_id=stops.stop_id AND stop_times.trip_id='%s' ORDER BY stop_sequence", x))
}

plot.track <- function(x, ...) {
    shape <- shape(x)
    schedule <- schedule(x)
    plot(shape, type = "l", asp = 1)
    points(schedule$lon, schedule$lat, pch = 19, cex = 0.5)
    points(shape[1, , drop = FALSE], col = "black", pch = 19)
    points(x$AVL["lon"], x$AVL["lat"], pch = 19, col = "red")
}

faesible <- function(T, t) {
    ## Using TRIP (T) and time (t), select pairs that are faesible
    ## - 20 mins AHEAD of t, or 90 min behind

    sched <- schedule(T)
    time <- hms(format(as.POSIXct(t), "%H:%M:%S"))

    arrivals <- hms(sched$arrive)

    arrivals - time
}

cur <- track(N)
plot(cur)

schedule(cur)
