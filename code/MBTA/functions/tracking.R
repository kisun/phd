## function(s) for tracking a vehicle:

setupDatabase <- function() {
    system('cp gtfs-STATIC.db trackers.db && sqlite3 trackers.db < tracker_setup.sql')
}

convertGPS <- function(lat, lon) {
    require("rgdal")
    
    LatLong <- data.frame(X = lat, Y = lon)
    names(LatLong) <- c("X", "Y")

    coordinates(LatLong) <- ~ Y + X
    proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

    Utm <- spTransform(LatLong, CRS("+proj=utm +zone=11 ellps=WGS84"))

    data.frame(lon = Utm$X, lat = Utm$Y)
}

pathDistance <- function(lat, lon) {
    ## Return the distance of a path defined by lat and lon.
    
    require(geosphere)

    coords <- cbind(lon, lat)
    from <- coords[-nrow(coords), , drop = FALSE]
    to <- coords[-1, , drop = FALSE]
    
    distGeo(from, to)
}


## AVL OBJECT + METHODS
getAVL <- function(vehicle.id, timestamp = NULL) {
    ## if timestamp is set, obtain AVL from historical data; otherwise from latest
    if (is.null(timestamp)) {
        AVL <- query(dbConnect(SQLite(), "gtfs.db"), "SELECT vehicle_id, trip_id, timestamp, position_latitude AS lat, position_longitude AS lon FROM vehicle_positions WHERE vehicle_id = %s LIMIT 1", vehicle.id)
        
        if (nrow(AVL) == 0)
            stop("That vehicle isn't running. Maybe you want to specify a timestamp and use historical data?")
    } else {
        AVL <- query(dbConnect(SQLite(), HISTDB),
                     "SELECT vehicle_id, trip_id, timestamp, position_latitude AS lat, position_longitude AS lon FROM vehicle_positions WHERE vehicle_id=%s AND timestamp=%s LIMIT 1", vehicle.id, timestamp)

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


## Closest point on track
closestPoint <- function(pos, shape, ...) {
    ## Position should be a vector c(LAT, LON)
    ## shape should be a data.frame containing at least `shape_pt_sequence`, `shape_pt_lat`, `shape_pt_lon`, `shape_dist_traveled`

    r <- as.numeric(convertGPS(pos[1], pos[2]))
    p <- convertGPS(as.numeric(shape$shape_pt_lat), as.numeric(shape$shape_pt_lon))
    d <- shape$shape_dist_traveled

    D <- numeric(nrow(p) - 1)
    R <- D
    for (k in 1:(nrow(p) - 1)) {
        q1 <- as.numeric(p[k, ])
        q2 <- as.numeric(p[k + 1, ])

        
        if (all(q1 == q2)) D[k] <- R[k] <- NA
        
        ## Ths following algorithm comes from Cathey & Dailey, 2003
        
        v <- q2 - q1
        w <- r - q1
        ww <- w %*% w
        wv <- w %*% v
        vv <- v %*% v
        if (wv < 0) {
            r2 <- ww
            D[k] <- 0
            R[k] <- sqrt(r2)
        } else if (wv <= vv) {
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
    ## R = minimum distance from point to line
    ## D = distance alone line to closest point on line (i.e., distance into trip)

    kk <- which.min(R)
    
    dist <- d[kk] + D[kk]
    
    list(dist = dist, k = kk)
}


## TRACK OBJECT + METHODS
create <- function(x, ...) UseMethod("create")

create.gtfs.avl<- function(x, ...) {
    AVL <- x
    shape_id <- query(con, "SELECT shape_id FROM trips WHERE trip_id=%s", AVL$trip.id)

    shape <- query(
        con,
        "SELECT shape_id, shape_pt_sequence, shape_pt_lat, shape_pt_lon, shape_dist_traveled FROM shapes WHERE shape_id=%s",
        shape_id)
    
    if (all(is.na(shape$shape_dist_traveled))) {

        message("Calculating `shape_distance_traveled` for shape with `shape_id = ", shape_id, "`")
        
        shape$shape_dist_traveled <- cumsum(c(0, pathDistance(shape$shape_pt_lat, shape$shape_pt_lon)))

        insert.query <- 
            sprintf("UPDATE shapes SET shape_dist_traveled='%s' WHERE shape_id='%s' AND shape_pt_sequence='%s'",
                    shape$shape_dist_traveled, shape$shape_id, shape$shape_pt_sequence)
        res <- sapply(insert.query, function(q) query(con, q))
    }

    ## Does this shape/route have distances applied to stops?
    stops <- query(con, "SELECT trip_id, stop_sequence, stop_times.stop_id, stops.stop_lat, stops.stop_lon, shape_dist_traveled FROM stop_times, stops WHERE stop_times.stop_id=stops.stop_id AND trip_id=%s ORDER BY stop_sequence", AVL$trip.id)
    if (all(is.na(stops$shape_dist_traveled))) {

        message("Calculating `shape_distance_traveled` for trip with `trip_id = ", AVL$trip.id, "`")
        
        ## Yes? Alright, lets add distances!
        ## Remember, each stop will be further along the route than the previous (for efficiency)
        
        inorder <- numeric(nrow(stops))
        for (s in stops$stop_sequence) {
            ## Find shape points that are within a reasonable range of the stop
            ## within 100m?
            w <- which(stops$stop_sequence == s)
            stop.pos <- stops[w, c("stop_lat", "stop_lon")]
            ## distances uses GPS coordinates:
            distToStop <- apply(shape, 1, function(sh) {
                pathDistance(c(stop.pos[1, 1], as.numeric(sh["shape_pt_lat"])),
                             c(stop.pos[1, 2], as.numeric(sh["shape_pt_lon"])))
            })

            goForward <- (1:nrow(shape)) >= ifelse(w == 1, 1, inorder[w - 1])
            if (all(distToStop > sigma)) {
                valid <- which(goForward)
                warning("All stops are a fair way off ...")
            } else
                valid <- which(distToStop < sigma & goForward)

            i <- 1
            while (length(valid) == 1 |
                       (length(unique(shape[valid, "shape_pt_lat"])) == 1 &
                            length(unique(shape[valid, "shape_pt_lon"])) == 1)) {
                valid <- which(distToStop < i * sigma & goForward)
                i <- i + 1
            }

            closest <- closestPoint(stop.pos, shape[valid, ])

            if (length(closest$dist) == 0) {
                cat("stop.pos:\n")
                print(stop.pos)

                cat("\nvalid:\n")
                print(valid)

                cat("\nshape[valid, ]:\n")
                print(shape[valid, ])

                cat("\nclosest:\n")
                print(closest)
            }
        
            stops$shape_dist_traveled[w] <- closest$dist

            if (w == 1)
                inorder[w] <- valid[closest$k]
            else if (inorder[w - 1] <= valid[closest$k])
                inorder[w] <- valid[closest$k]
            else
                stop("Invalid at stop ", s)
        }

        insert.query <- 
            sprintf("UPDATE stop_times SET shape_dist_traveled='%s' WHERE trip_id='%s' AND stop_sequence='%s'",
                    stops$shape_dist_traveled, stops$trip_id, stops$stop_sequence)
        res <- sapply(insert.query, function(q) query(con, q))
    }
    
    ## Update all trips with the same SHAPE_ID:
    ##all.trips <- query("SELECT trip_id, route_id, direction_id, shape_id FROM trips WHERE shape_id=%s", shape_id)

    message("Inserting new track record into database.")

    dbSendQuery(
        con,
        sprintf("INSERT INTO tracks (vehicle_id, timestamp, pos_lat, pos_lon, trip_id, shape_id) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
                AVL$vehicle.id, AVL$time, AVL$position[1], AVL$position[2], AVL$trip.id, shape_id)
    )   
    
    invisible(NULL)
}
getTrack <- function(vehicle.id) {
    track <- query(con, "SELECT * FROM tracks WHERE vehicle_id=%s", vehicle.id)

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
updateTrack <- function(track, DIT, TPI, dTPI) {
    if (!is.na(track$distance.into.trip))
        if (DIT < track$distance.into.trip)
            stop("Bus went backwards!!!!")
    
    track$distance.into.trip <- DIT
    track$TPI <- TPI
    track$distance.into.TPI <- dTPI

    track
}


## Proposals
propose <- function(track, AVL) {
    ## propose DISTANCE INTO TRIP, TPI, DISTANCE INTO TPI (TRIP is known)

    ## 1. max distance bus could have moved since previous report?
    if (length(track$distance.into.trip) == 0) {
        delta.time <- NA
        max.dist <- 1e10
    } else {
        delta.time <- AVL$time - track$AVL$time
        max.dist <- track$distance.into.trip + delta.time * (100 * 1000 / 60 / 60) - 2 * sigma  ## time (in minutes) * (speed in meters/s)
    }


    ## 2. starting location for checking:
    if (length(track$TPI) == 0) {
        k0 <- 0
    } else {
        k0 <- track$TPI - (track$distance.into.TPI < sigma)
    }

    SHAPE <- query(con, "SELECT shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled FROM shapes WHERE shape_id=%s",
                   track$shape)
    STOPS <- query(con, "SELECT trip_id, stop_sequence, stop_times.stop_id, stops.stop_lat, stops.stop_lon, shape_dist_traveled FROM stop_times, stops WHERE stop_times.stop_id=stops.stop_id AND trip_id=%s ORDER BY stop_sequence", AVL$trip.id)


    ## Find all TPIs with segments within some radius
    pos <- AVL$pos
    ## distances uses GPS coordinates:
    valid <- which(1:nrow(SHAPE) >= k0   &   SHAPE$shape_dist_traveled < max.dist)
    distToBus <- apply(SHAPE[valid, ], 1, function(sh) {
        pathDistance(c(pos[1], as.numeric(sh["shape_pt_lat"])),
                     c(pos[2], as.numeric(sh["shape_pt_lon"])))
    })

    w <- valid[which(distToBus < sigma)]
    if (all(distToBus > sigma)){
        warning("Bus is a fair way off from the route!")
        w <- 1:nrow(SHAPE)
    }
    closest <- closestPoint(pos, SHAPE[w, ])
    
    
    TPI <- which.max(which(STOPS$shape_dist_traveled <= closest$dist))
    


    ## layout(matrix(c(1, 1, 1, 2, 3, 3, 3), ncol = 1))
    ## par(mar = c(3.1, 4.1, 1.1, 1.1))

    ## plot(SHAPE$shape_pt_lon, SHAPE$shape_pt_lat, type = "l", asp = 1,
    ##      xlim = range(c(SHAPE$shape_pt_lon, AVL$pos[2])),
    ##      ylim = range(c(SHAPE$shape_pt_lat, AVL$pos[1])))
    ## points(STOPS$stop_lon, STOPS$stop_lat, pch = 19, cex = 0.5)
    ## points(tail(STOPS$stop_lon, 1), tail(STOPS$stop_lat, 1), pch = 15, col = "blue")
    ## points(AVL$pos[2], AVL$pos[1], pch = 19, cex = 0.5, col = "red")
    
    ## plot(SHAPE$shape_dist_traveled, rep(1, nrow(SHAPE)), type = "l")
    ## points(STOPS$shape_dist_traveled, rep(1, nrow(STOPS)), pch = 19, cex = 0.5)
    ## points(closest$dist, 1, col = "red", pch = 19, cex = 0.5)

    list(distance.into.trip = closest$dist,
         TPI = TPI,
         distance.into.TPI = closest$dist - STOPS$shape_dist_traveled[TPI])
    
}


kalmanFilter <- function(new, old) {
    ## Matrices
    H <- t(c(1, 0, 0))
    R <- 500^2

    F <- rbind(c(0, 1, 0),
               c(0, 0, 1),
               c(0, 0, 0))
    G <- cbind(c(0, 0, 1))

    q2 <- 264

    Phi <- function(dt) rbind(c(1, dt, dt^2 / 2),
                                 c(0, 1, dt),
                                 c(0, 0, 1))
    Q <- function(dt, q2) rbind(c(dt^5 / 20, dt^4 / 8, dt^3 / 6),
                                   c(dt^4 / 8, dt^3 / 3, dt^2 / 2),
                                   c(dt^3 / 6, dt^2 / 2, dt)) * q2

    if (is.null(attributes(old)$P))
        P <- rbind(c(R, 0, 0),
                   c(0, 30^2, 0),
                   c(0, 0, 16^2))
    else
        P <- attr(old, "P")
    
    X. <- matrix(old, ncol = 1)

    dt <- (new$AVL$time - attr(old, "time")) / 60
    if (dt == 0) {
        attr(X., "time") <- new$AVL$time
        attr(X., "history") <- attr(old, "history")
        attr(X., "P") <- P
        return(X.)
    }

    Phi.k <- Phi(dt)
    Xk.hat. <- Phi.k %*% X.
    Pk. <- Phi.k %*% P %*% t(Phi.k) + Q(dt, q2)


    K.k <- Pk. %*% t(H) %*% solve(H %*% Pk. %*% t(H) + R)

    z <- new$distance.into.trip * 3.28084
    Xk <- Xk.hat. + K.k %*% (z - H %*% Xk.hat.)
    Pk <- (diag(3) - K.k %*% H) %*% Pk.
    

    X <- Xk
    attr(X, "time") <- new$AVL$time
    attr(X, "P") <- Pk
    attr(X, "history") <- rbind(tail(attr(old, "history"), 50), c(drop(X), new$AVL$time, new$distance.into.trip))
    rownames(attr(X, "history")) <- NULL
    X
}

predict <- function(old, minutes.ahead = 1) {
    H <- t(c(1, 0, 0))
    R <- 500^2
    
    F <- rbind(c(0, 1, 0),
               c(0, 0, 1),
               c(0, 0, 0))
    G <- cbind(c(0, 0, 1))
    
    q2 <- 264
    
    Phi <- function(dt) rbind(c(1, dt, dt^2 / 2),
                              c(0, 1, dt),
                              c(0, 0, 1))
    Q <- function(dt, q2) rbind(c(dt^5 / 20, dt^4 / 8, dt^3 / 6),
                                c(dt^4 / 8, dt^3 / 3, dt^2 / 2),
                                c(dt^3 / 6, dt^2 / 2, dt)) * q2
    
    P <- attr(old, "P")
    
    X. <- matrix(old, ncol = 1)
    
    ## time in minutes
    dt <- minutes.ahead
    
    Phi.k <- Phi(dt)
    Xk.hat. <- Phi.k %*% X.
    Pk. <- Phi.k %*% P %*% t(Phi.k) + Q(dt, q2)
    
    
    est <- Xk.hat.[1]
    est.sd <- sqrt(Pk.[1,1])

    structure(c(est, est.sd), .Names = c("mean", "sd"))
}

trackMyBus <- function(vehicle.id, timestamp = NULL, prev = NULL,
                       origin = format(Sys.time(), "%Y-%m-%d")) {
    ## vehicle.id: unique vehicle identifier
    ## timestamp: if not NULL, will be used to obtain historical data; otherwise, will use the latest GTFS report

    ## ERRORS
    assign("sigma", 100 * 3.28084, envir = .GlobalEnv)  # meters -> feet

#    dev.hold()
    
    require(RSQLite)
    if (!"trackers.db" %in% list.files())
        setupDatabase()
    
    assign("con", dbConnect(SQLite(), "trackers.db"), envir = .GlobalEnv)

    ## obtain the latest report:
    AVL <- getAVL(vehicle.id, timestamp)
    
    ## Does the vehicle exist in the trackers.db database?
    check <- query(con, "SELECT vehicle_id FROM tracks WHERE vehicle_id=%s AND trip_id=%s", vehicle.id, AVL$trip.id)
    if (nrow(check) == 0) {
        reset <- TRUE
        query(con, "DELETE FROM tracks WHERE vehicle_id=%s", vehicle.id)
        create(AVL)
    } else {
        reset <- FALSE
    }

    track <- getTrack(vehicle.id)

    candidates <- propose(track, AVL)

    newtrack <- updateTrack(track, candidates$distance.into.trip, candidates$TPI, candidates$distance.into.TPI)
    newtrack$AVL <- AVL
    
    if (is.null(prev)) {
        KF <- cbind(c(x = newtrack$distance.into.trip * 3.28084, v = 1000, a = 100))
        attr(KF, "time") <- AVL$time
    } else if (reset) {
        KF <- prev
        KF[1, ] <- 0
        attr(KF, "time") <- AVL$time
        attr(KF, "history") <- if (is.null(attr(KF, "history"))) NULL else matrix(NA, ncol = ncol(attr(KF, "history")))#rbind(attr(KF, "history"), NA)
    } else {
        KF <- kalmanFilter(newtrack, prev)
    }

    if (!is.null(attr(KF, "history"))) {
        HH <- as.data.frame(attr(KF, "history"))
        colnames(HH) <- c("x", "v", "a", "time", "dit")
        HH$x <- HH$x * 0.3048
        

        qry <- query(con, "SELECT stop_id, arrival_time, departure_time, shape_dist_traveled FROM stop_times WHERE trip_id=%s ORDER BY stop_sequence", track$trip)
        qry$timepoint <- ifelse(is.na(qry$arrival_time), qry$departure_time, qry$arrival_time)
        qry$time <- as.numeric(as.POSIXct(paste(origin, qry$timepoint), format="%Y-%m-%d %H:%M:%S", tz="EST5EDT"))

        
        ## plot(HH$time, HH$dit,
        ##      xlim = range(HH$time, qry$time, na.rm = TRUE),
        ##      ylim = range(HH$x, HH$dit, qry$shape_dist_traveled, na.rm = TRUE),
        ##      pch = 19, cex = 0.5)
        ## lines(HH$time, HH$x)
        ## points(qry$time, qry$shape_dist_traveled, col = "#00000040", pch = 19, cex = 0.5)

        ## abline(h = qry$shape_dist_traveled, col = "#00000010")
    }
#    dev.flush(dev.flush())
    
    list(track = newtrack,
         kalman.filter = KF)
}
