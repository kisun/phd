require(RSQLite)
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
        AVL <- AVLreport(N)
        z <- NA
    } else if (missing(x)) {
        stop("Provide either a track, or vehicle ID.")
    } else {
        N <- x
    
    
        ## Last associated AVL report
        AVL <- AVLreport(N)
        
        ## Trip, T
        T <- AVL$B

        ## Distance into trip, z
        z <- NA
    }
    
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
    points(x$AVL["lon"], x$AVL["lat"], pch = 19, col = rgb(1,0, 0, 0.4))
}

faesible <- function(T, t) {
    ## Using TRIP (T) and time (t), select pairs that are faesible
    ## - 20 mins AHEAD of t, or 90 min behind

    sched <- schedule(T)
    time <- hms(format(as.POSIXct(t, origin = "1970-01-01", tz = "EDT5EDT"), "%H:%M:%S"))
    arrivals <- hms(sched$arrive)

    diff <- arrivals - time
    which(diff < minutes(20) & diff > minutes(-90))
}


whereIsTheBus <- function(x, bus = TRUE) {
    source("functions.R")
    ## Where on the trip/route is the bus??

    if (bus) {
        rll <- x$AVL[c("lon", "lat")]
    } else {
        rll <- schedule(x)[, c("lon", "lat")]
    }


    apply(rll, 1, function(rl) {
        r <- as.numeric(convertGPS(rl["lat"], rl["lon"]))
        sh <- shape(x)
        
        p <- convertGPS(sh$lat, sh$lon)
        d <- c(0, sapply(2:nrow(p), function(i) pathDistance(sh$lat[1:i], sh$lon[1:i])))
        
        D <- numeric(nrow(p) - 1)
        R <- D
        for (k in 1:(nrow(p) - 1)) {
            q1 <- as.numeric(p[k, ])
            q2 <- as.numeric(p[k + 1, ])
            
            if (all(q1 == q2)) D[k] <- R[k] <- NA
            
            ##        plot(c(r[1], q1[1], q2[1]), c(r[2], q1[2], q2[2]), pch = 19)
            ##        lines(rbind(q1, q2))
            
            v <- q2 - q1
            w <- r - q1
            ww <- w %*% w
            wv <- w %*% v
            vv <- v %*% v
            if (dp < 0) {
                r2 <- ww
                D[k] <- 0
                R[k] <- sqrt(r2)
            } else if (0 <= wv && wv <= vv) {
                d2 <- wv^2 / vv
                r2 <- ww - d2
                R[k] <- sqrt(r2)
                D[k] <- sqrt(d2)
            } else {
                r2 <- (w - v) %*% (w - v)
                d2 <- vv
                R[k] <- sqrt(r2)
                D[k] <- sqrt(d2)
            }
        }
        kk <- which.min(R)
        
        dist <- d[kk] + D[kk]
        
        dist
    })
}


list.vehicles()
cur <- track("v2194")
plot(cur)

schedule(cur)


T <- cur$T; t <- cur$AVL$t
schedule(cur)[faesible(T, t), ]


layout(matrix(c(1, 1, 1, 2)))
plot(cur)
busPos <- whereIsTheBus(cur)
stopPos <- whereIsTheBus(cur, bus = FALSE)

plot(stopPos, rep(1, length(stopPos)), pch = 19, cex = 0.5)
lines(stopPos[c(1, length(stopPos))], c(1, 1))
points(busPos, 1, pch = 19, col = rgb(1, 0, 0, 0.4))

