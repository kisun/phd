getShape <- function(id, db = "db/gtfs-static.db") {
    con <- dbConnect(SQLite(), db)
    shape <- dbGetQuery(con, sprintf("SELECT CAST(shape_pt_lat AS REAL) AS lat,
                                             CAST(shape_pt_lon AS REAL) AS lon, shape_pt_sequence FROM shapes
                                      WHERE shape_id=(SELECT shape_id FROM trips WHERE trip_id='%s')
                                      ORDER BY shape_pt_sequence",
                                     id))
    dbDisconnect(con)
    ## work out stuff - this should be written in C at some point (and the above, I guess ...)
    shape <- shape[c(TRUE, abs(diff(shape$lat)) > 1e-15 & abs(diff(shape$lon)) > 1e-15), ]
    shape$length <- c(distance(t(shape[-nrow(shape), c("lon", "lat")]),
                               t(shape[-1, c("lon", "lat")])), NA)
    shape$bearing <- c(bearing(t(shape[-nrow(shape), c("lon", "lat")]),
                               t(shape[-1, c("lon", "lat")])), NA)
    shape$distance_into_shape <- c(0, cumsum(shape$length[-nrow(shape)]))
    shape
}
getSchedule <- function(id, shape = getShape(id, db = db), db = "db/gtfs-static.db") {
    con <- dbConnect(SQLite(), db)
    sched <- dbGetQuery(con, sprintf("SELECT arrival_time, departure_time, st.stop_id,
                                             CAST(s.stop_lon AS REAL) as stop_lon,
                                             CAST(s.stop_lat AS REAL) as stop_lat, st.stop_sequence
                                      FROM stop_times AS st, stops AS s
                                      WHERE st.stop_id = s.stop_id AND st.trip_id = '%s'
                                      ORDER BY stop_sequence", id))
    dbDisconnect(con)
    sched$distance_into_shape <- getShapeDist(sched, shape)
    sched
}
distance <- function(y, z, R = 6371000) {
    ## computes the distance between {y} and {z}
    ## different R (such as in km) give results in those units    
    if (length(dim(y)) < 2)
        y <- cbind(y)
    if (length(dim(z)) < 2)
        z <- cbind(z)
    if (ncol(y) != ncol(z) & ncol(y) > 1 & ncol(z) > 1)
        stop("Incorrent dimensions")
    ## need to scale from degrees to radians:
    lam.y <- y[1, ] * pi / 180
    lam.z <- z[1, ] * pi / 180
    phi.y <- y[2, ] * pi / 180
    phi.z <- z[2, ] * pi / 180
    ## use great circle distance:
    res <- suppressWarnings({
        R * acos(sin(phi.y) * sin(phi.z) + cos(phi.y) * cos(phi.z) * cos(abs(lam.y - lam.z)))
    })
    res[is.infinite(res)] <- 0
    res
}
bearing <- function(a, b) {
    ## compute the bearing between two points
    if (length(dim(a)) < 2) y <- cbind(a)
    if (length(dim(b)) < 2) z <- cbind(b)
    if (ncol(a) != ncol(b) & ncol(a) > 1 & ncol(b) > 1)
        stop("Incorrent dimensions")
    mode(a) <- mode(b) <- "numeric"    
    ## convert to radians!!
    lam.a <- a[1, ] * pi / 180
    lam.b <- b[1, ] * pi / 180
    phi.a <- a[2, ] * pi / 180
    phi.b <- b[2, ] * pi / 180
    th.rad <- atan2(sin(lam.b - lam.a) * cos(phi.b),
                    cos(phi.a) * sin(phi.b) - sin(phi.a) * cos(phi.b) * cos(lam.b - lam.a))
    (th.rad * 180 / pi) %% 360
}
getShapeDist <- function(sched, shape) {
    sched <- sched[, c("stop_lon", "stop_lat")]
    shape <- shape[shape$length > 0 | is.na(shape$length), ]
    ## Convert to FLAT XY
    shape$lam <- as.numeric(shape$lon) * pi / 180
    shape$phi <- as.numeric(shape$lat) * pi / 180
    sched$lam <- as.numeric(sched$stop_lon) * pi / 180
    sched$phi <- as.numeric(sched$stop_lat) * pi / 180
    phi1 <- mean(shape$phi)
    shape$x <- shape$lam * cos(phi1)
    shape$y <- shape$phi
    sched$x <- sched$lam * cos(phi1)
    sched$y <- sched$phi
    d <- numeric(nrow(sched))
    J <- 1
    nr <- nrow(shape)
    nr1 <- nr - 1
    for (i in 1:nrow(sched)) {
        p <- as.numeric(sched[i, c("x", "y")])
        di <- ri <- numeric(nr - J)
        pxy <- matrix(NA, nrow = nr - J, ncol = 2)
        for (j in J:nr1) {
            ji <- j - J + 1
            q1 <- as.numeric(shape[j, c("x", "y")])
            q2 <- as.numeric(shape[j + 1, c("x", "y")])
            v <- q2 - q1
            w <- p - q1
            wv <- w %*% v
            vv <- v %*% v
            ww <- w %*% w
            suppressWarnings({
                if (wv < 0) {
                    r2 <- ww
                    di[ji] <- 0
                    ri[ji] <- sqrt(r2)
                } else if (wv <= vv) {
                    d2 <- wv^2 / vv
                    r2 <- ww - d2
                    ri[ji] <- sqrt(r2)
                    di[ji] <- sqrt(d2)
                } else {
                    r2 <- (w - v) %*% (w - v)
                    d2 <- vv
                    ri[ji] <- sqrt(r2)
                    di[ji] <- sqrt(d2)
                }
            })   
            pxy[ji, ] <- q1 + di[ji] / sqrt(vv) * v
        }
        wi <- which.min(ri)
        J <- J + wi - 1
        ## convert it:
        pll <- c(pxy[wi, 1] / cos(phi1), pxy[wi, 2]) * 180 / pi
        d[i] <- shape[J, "distance_into_shape"] + distance(pll, as.numeric(shape[J, c("lon", "lat")]))
    }
    d
}



## time functions:
ts2dt <- function(ts, to = c("datetime", "date", "time")) {
    to <- match.arg(to)
    t <- as.POSIXct(ts, origin = "1970-01-01")
    switch(to,
           "datetime" = t,
           "date" = format(t, "%Y-%m-%d"),
           "time" = format(t, "%H:%M:%S"))
}
timeDiff <- function(t1, t2) {
    diff(as.numeric(as.POSIXct(paste("2016-01-01", c(t1, t2)))))
}
