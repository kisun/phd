newBus <- function(row, db, n.particles = 10) {
    obj <- list(mat = rbind(lat   = as.numeric(row$position_latitude),
                             lon   = as.numeric(row$position_longitude),
                             t     = as.numeric(row$timestamp),
                             delta = 0),
                trip_id = row$trip_id, 
                route_id = row$route_id,
                shape_id = dbGetQuery(dbConnect(SQLite(), db),
                                       sprintf("SELECT shape_id FROM trips WHERE trip_id='%s'",
                                               row$trip_id))$shape_id)
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
    obj$particles <- array(NA, dim = c(3, n.particles, 1))
    obj
}
moveBus <- function(obj, row) {
    obj$mat <- cbind(obj$mat,
                     rbind(as.numeric(row$position_latitude),
                           as.numeric(row$position_longitude),
                           as.numeric(row$timestamp), NA))
    n <- ncol(obj$mat)
    obj$mat["delta", n] <- diff(obj$mat["t", (n-1):n])

    obj
}

plotBus <- function(obj, db) {
    n <- ncol(obj$mat)
    plotSegments(id = obj$shape_id, db = db)
    with(obj$shapefull[1, ], addPoints(shape_pt_lon, shape_pt_lat, pch = 18, gp = list(cex = 0.6)))
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
        ## use whatever to update the particles:
        if (all(is.na(X[2,]))) {
            ## no speed set - guess it?
            new <- array(NA, dim = dim(X))
            new[3,] <- rnorm(M, 0, 3)
            new[2,] <- runif(M, 0, 30)  ## m/s
            new[1,] <- ## X[1,] + msm::rtnorm(M, dt * new[2,], 3 * dt, lower = 0)
                X[1,] + dt * new[2, ]
        } else {
            ## speed already there, adjust it:
            new <- array(NA, dim = dim(X))
            new[3,] <- rnorm(M, 0, 1)
            new[2,] <- pmax(0, X[2,] + dt * new[3,])
            new[1,] <- X[1,] + dt * new[2, ]
        }

        ## apply selection:
        d <- distanceFlat(mat[1:2, n, drop = FALSE], sapply(new[1, ], h, shape = obj$shapefull))
        pr <- dnorm(d, 0, 20)
        wt <- pr / sum(pr)
        wt[is.na(wt)] <- 0
        wi <- sample(M, replace = TRUE, prob = wt)
        new <- new[, wi]
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
    speed_max FLOAT
)")
        cat("New table `history` created.\n")
    }

    dbDisconnect(.con)
    invisible(NULL)
}
