newBus <- function(row, db, n.particles = 10) {
    obj <- list(mat = rbind(lat   = as.numeric(row$position_latitude),
                             lon   = as.numeric(row$position_longitude),
                             t     = as.numeric(row$timestamp),
                             delta = 0),
                trip_id = row$trip_id, 
                route_id = row$route_id,
                shape_id = dbGetQuery(dbConnect(SQLite(), db),
                                       sprintf("SELECT shape_id FROM trips WHERE trip_id='%s'",
                                               row$trip_id))$shape_id,
                trip_start_time = row$trip_start_time,
                trip_start_date = tsDate(row$timestamp))
    obj$shape <- dbGetQuery(dbConnect(SQLite(), db),
                            sprintf("SELECT shape_id, sh.segment_id, segment_sequence, direction,
                                            MAX( CAST(distance_into_segment AS real) ) AS segment_length
                                       FROM shapes_seg AS sh, segments AS seg
                                      WHERE shape_id = '%s' AND sh.segment_id=seg.segment_id
                                   GROUP BY seg.segment_id
                                   ORDER BY segment_sequence",
                                    obj$shape_id))
    obj$shape$segment_length <- round(as.numeric(obj$shape$segment_length), 3)
    tmp <- dbGetQuery(dbConnect(SQLite(), db),
                      sprintf("SELECT shape_id, sh.segment_id, segment_sequence, direction,
                                      CAST(shape_pt_lat AS REAL) as shape_pt_lat,
                                      CAST(shape_pt_lon AS REAL) as shape_pt_lon, shape_pt_sequence,
                                      CAST(distance_into_segment AS REAL) as distance_into_segment
                                 FROM shapes_seg AS sh, segments AS seg
                                WHERE shape_id = '%s' AND sh.segment_id=seg.segment_id
                             ORDER BY segment_sequence, shape_pt_sequence",
                              obj$shape_id))
    p1 <- t(tmp[-nrow(tmp), c("shape_pt_lon", "shape_pt_lat")])
    p2 <- t(tmp[-1, c("shape_pt_lon", "shape_pt_lat")])
    tmp$length <- c(distanceFlat(p1, p2), NA)
    tmp$distance_into_shape <- c(0, cumsum(tmp$length[-nrow(tmp)]))
    tmp$bearing <- c(bearing(p1, p2), NA)
    obj$shapefull <- tmp

    ## stops:
    obj$stops <- getSchedule(obj$trip_id, db, verbose = FALSE)
    obj$stops$distance_into_trip <- getShapeDist(obj$stops, obj$shapefull)
    
    obj$particles <- array(NA, dim = c(3, n.particles, 1))
    obj
}
moveBus <- function(obj, row) {
    n <- ncol(obj$mat)
    dt <- row$timestamp - obj$mat["t", n]
    if (dt == 0) return(obj)
    obj$mat <- cbind(obj$mat,
                     rbind(as.numeric(row$position_latitude),
                           as.numeric(row$position_longitude),
                           as.numeric(row$timestamp), NA))

    obj$mat["delta", n + 1] <- dt
    obj
}
resetBus <- function(obj, row, db) {
    ## new trip ... so reset things
    new <- newBus(row, db, dim(obj$particles)[2])
    new$mat <- cbind(obj$mat, new$mat)
    new$particles <- abind::abind(obj$particles, obj$particles[,,dim(obj$particles)[3]])
    new$particles[1,,dim(new$particles)[3]] <- 0
    new
}
plotBus <- function(obj, db) {
    n <- ncol(obj$mat)
    plotSegments(id = obj$shape_id, db = db)
    with(obj$stops, addPoints(stop_lon, stop_lat, pch = 2, gpar = list(cex = 0.4, col = "#000099")))
    if (!all(is.na(obj$particles))) {
        pos <- sapply(obj$particles[1,,dim(obj$particles)[3]], h, shape = obj$shapefull)
        addPoints(pos[2,], pos[1,], gpar = list(col = "#00009930", cex = 0.3), pch = 4)
    }    
    addPoints(obj$mat["lon", n], obj$mat["lat", n], pch = 3,
              gpar = list(col = "red", cex = 0.5, alpha = 1, lwd = 2))
    
    invisible(NULL)
}
h <- function(x, shape) {
    ## Calculate Lat/Lon position of point(s) a given distance (x)
    ## into a pattern/shape

    if (is.na(x[1])) print(x)
    if (x[1] <= 0) return(c(0, 0))
    if (x[1] > max(shape$distance_into_shape))
        return(as.numeric(shape[nrow(shape), c("shape_pt_lat", "shape_pt_lon")]))
    
    j <- which.min(x[1] > shape$distance_into_shape) - 1
    sj <- shape[j, c("shape_pt_lat", "shape_pt_lon", "bearing", "distance_into_shape")]
    Psi <- sj$bearing
    d <- x[1] - sj$distance_into_shape
    
    ## only do the calculations if we need to!
    if (d == 0) return(as.numeric(sj[1:2]))
    partialSegment(as.numeric(sj[1:2]), Psi, d)
}
update <- function(obj) {
    mat <- obj$mat
    n <- ncol(mat)
    M <- ncol(obj$particles)
    if (all(is.na(obj$particles))) {
        ## need to find where the bus is on the route:
        D <- runif(1e3, 0, max(obj$shapefull$distance_into_shape))
        d <- distanceFlat(mat[1:2, 1, drop = FALSE], sapply(D, h, shape = obj$shapefull))
        pr <- dnorm(d, 0, 20)
        wt <- pr / sum(pr)
        wt[is.na(wt)] <- 0
        if (any(wt > 0)) {
            D <- sample(D, size = M, replace = TRUE, prob = wt)
            obj$particles[1,,1] <- D
        }
    } else {
        k <- dim(obj$particles)[3]
        X <- obj$particles[,,k]
        dt <- mat["delta", n]
        if (dt  == 0) return(obj)
        dmax <- max(obj$shapefull$distance_into_shape)
        if (all(X[1,] == dmax)) {
            obj$particles <- abind::abind(obj$particles, X)
            return(obj)
        }
        ## use whatever to update the particles:
        if (all(is.na(X[2,]))) {
            ## no speed set - guess it?
            new <- array(NA, dim = dim(X))
            new[3,] <- rnorm(M, 0, 3)
            new[2,] <- runif(M, 0, 30)  ## m/s
            new[1,] <- ## X[1,] + msm::rtnorm(M, dt * new[2,], 3 * dt, lower = 0)
                pmin(X[1,] + dt * new[2, ], dmax)
        } else {
            ## speed already there, adjust it:
            new <- array(NA, dim = dim(X))
            new[3,] <- rnorm(M, 0, 1)
            new[2,] <- pmax(0, X[2,] + dt * new[3,])
            new[1,] <- pmin(X[1,] + dt * new[2, ], dmax)
        }
        ## apply selection:
        if (any(new[1,] <=  dmax)) {
            d <- distanceFlat(mat[1:2, n, drop = FALSE], sapply(new[1, ], h, shape = obj$shapefull))
            var <- 20
            pr <- dnorm(d, 0, var)
            ## bus might have stopped!
            if (all(distanceFlat(mat[1:2, n, drop = FALSE],
                                 sapply(X[1, ], h, shape = obj$shapefull)) < d)) {
                new[2,] <- runif(M, 0, 30)
                new[1,] <- pmax(X[1,] + dt * new[2, ], dmax)
                d <- distanceFlat(mat[1:2, n, drop = FALSE],
                                  sapply(new[1, ], h, shape = obj$shapefull))
                pr <- dnorm(d, 0, var)
            }
            while (all(pr < 1e-5)) {
                var <- 2 * var
                pr <- dnorm(d, 0, var)
            }
            wt <- pr / sum(pr)
            wt[is.na(wt)] <- 0
            
            wi <- sample(M, replace = TRUE, prob = wt)
            new <- new[, wi]
        }
        obj$particles <- abind::abind(obj$particles, new)
    }
    
    obj
}



createHistoricalDb <- function(db = "db/gtfs-static2.db", yes = FALSE,
                               .con = dbConnect(SQLite(), db)) {
    add <- TRUE
    if ("history" %in% dbListTables(.con)) {
        if (!yes)
            yes <- readline("Table `history` exists ... overwrite? (y/n) ") == "y"
        if (yes) dbGetQuery(.con, "DROP TABLE history")
        else add <- FALSE
    }
    if (add) {
        dbGetQuery(.con, "CREATE TABLE history (
    oid INTEGER PRIMARY KEY,
    trip_id VARCHAR(10),
    route_id VARCHAR(10),
    shape_id VARCHAR(10),
    segment_id INTEGER,
    segment_direction INTEGER,
    segment_min_distance FLOAT,
    trip_start_time VARCHAR(8),
    trip_start_date VARCHAR(10),
    vehicle_id VARCHAR(10),
    position_latitude FLOAT,
    position_longitude FLOAT,
    timestamp BIGINT,
    distance_mean FLOAT,
    distance_median FLOAT,
    distance_var FLOAT,
    distance_min FLOAT,
    distance_max FLOAT,
    speed_mean FLOAT,
    speed_median FLOAT,
    speed_var FLOAT,
    speed_min FLOAT,
    speed_max FLOAT,
    nparticles INT
)")
        cat("New table `history` created.\n")
    }

    dbDisconnect(.con)
    invisible(NULL)
}
writeHistory <- function(v, vid, db) {
    n <- ncol(v$mat)
    m <- dim(v$particles)[3]
    if (any(is.na(v$particles[,,m]))) return(invisible(NULL))
    
    d <- v$particles[1,,m]
    s <- v$particles[2,,m]
    shape <- v$shapefull
    ## This could cause issues ... but hopefully not once the shape algorithms
    ## have been fixed up so shapes actually join together at nodes.
    si <- tail(which((median(d) - shape$distance_into_shape) >= 0), 1)        

    con <- dbConnect(SQLite(), db)
    dbGetQuery(con,
               sprintf("INSERT INTO history ('trip_id', 'route_id', 'shape_id', 'segment_id',
                            'segment_direction', 'segment_min_distance',
                            'trip_start_time', 'trip_start_date', 'vehicle_id',
                            'position_latitude', 'position_longitude', 'timestamp',
                            'distance_mean', 'distance_median', 'distance_var',
                            'distance_min', 'distance_max', 'speed_mean', 'speed_median',
                            'speed_var', 'speed_min', 'speed_max', 'nparticles')
                             VALUES ('%s')",
                       paste(v$trip_id, v$route_id, v$shape_id, shape$segment_id[si],
                             shape$direction[si],
                             min(shape$distance_into_shape[shape$segment_id == shape$segment_id[si]]),
                             v$trip_start_time, v$trip_start_date, vid, v$mat["lat", n],
                             v$mat["lon", n], v$mat["t", n], mean(d), median(d), var(d),
                             min(d), max(d), mean(s), median(s), var(s), min(s), max(s),
                             dim(v$particles)[2], sep = "','")))
    dbDisconnect(con)
    invisible(NULL)
}
