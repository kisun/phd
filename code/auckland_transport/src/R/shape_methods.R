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
    obj$shapefull <- dbGetQuery(dbConnect(SQLite(), db),
                                sprintf("SELECT shape_id, sh.segment_id, segment_sequence, direction,
                                                CAST(shape_pt_lat AS REAL) as shape_pt_lat,
                                                CAST(shape_pt_lon AS REAL) as shape_pt_lon,
                                                shape_pt_sequence,
                                                CAST(distance_into_segment AS REAL) as distance_into_segment
                                       FROM shapes_seg AS sh, segments AS seg
                                      WHERE shape_id = '%s' AND sh.segment_id=seg.segment_id
                                   ORDER BY segment_sequence, shape_pt_sequence",
                                        obj$shape_id))
    z <- rbind(obj$shapefull$shape_pt_lat,
               obj$shapefull$shape_pt_lon)
    obj$shapefull$distance_into_shape <- cumsum(c(0, distanceFlat(z[, -1], z[, -nrow(obj$shapefull)])))
    obj$particles <- array(NA, dim = c(3, n.particles, 1))
    obj
}
moveBus <- function(obj, row) {
    obj$mat <- cbind(mat,
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
    addPoints(obj$mat["lon", n], obj$mat["lat", n], pch = 19, gpar = list(col = "red", cex = 0.5))

    invisible(NULL)
}
h <- function(x, shape) {
    ## Calculate Lat/Lon position of point(s) a given distance (x)
    ## into a pattern/shape

    if (is.na(x[1])) print(x)
    if (x[1] <= 0) return(c(0, 0))
    if (x[1] > max(shape$distance_into_shape))
        return(as.numeric(shape[nrow(shape), c("shape_pt_lat", "shape_pt_lon")]))
    
    j <- which.min(x[1] > shape$distance_into_pattern) - 1
    sj <- shape[j, c("shape_pt_lat", "shape_pt_lon", "bearing", "distance_into_shape")]
    Psi <- sj$bearing
    d <- x[1] - sj$distance_into_pattern
    
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
        D <- runif(M, 0, max(obj$shapefull$distance_into_shape))
        sapply(D, h, shape = obj$shapefull)
    } else {
        
    }
    obj
}
