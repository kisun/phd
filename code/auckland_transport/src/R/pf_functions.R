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
partialSegment <- function(x, theta, d, R = 6371000) {
    ## Compute the Lat/Lon after traveling distance D
    ## from point X at a bearing of THETA
    
    delta <- d / R ## for single calculation
    theta <- theta * pi / 180  ## convert to radians
    phi.s <- x[1] * pi / 180
    lam.s <- x[2] * pi / 180
    
    phi.z <- asin(sin(phi.s) * cos(delta) + cos(phi.s) * sin(delta) * cos(theta))
    lam.z <- lam.s + atan2(sin(theta) * sin(delta) * cos(phi.s),
                           cos(delta) - sin(phi.s) * sin(phi.z))
    c(phi.z, lam.z) * 180 / pi
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
pfilter <- function(X, row, shape, sched, gamma = 10, rerun = FALSE, gps = 5) {
    s <- sched$distance_into_shape
    if (all(X[1,] == max(s))) return(X)
    Xold <- X
    if (rerun) X <- cbind(X, X)
    R <- ncol(X)
    NEW <- matrix(NA, nrow(X), ncol(X))
    tx <- attr(state, "ts")
    tn <- row$timestamp
    dt <- tn - tx
    ## print(dt)
    ## probabiltiy of staying where we are:
    at.stop <- abs(X[1,] - s[X[3,]]) < 20  ## within 20m of a stop
    at.stop[!is.finite(at.stop)] <- FALSE
    stay <- rbinom(R, 1, ifelse(at.stop, 0.5,  ifelse(rerun, 0.5, 0.1)))
    tau <- ifelse(stay, rexp(R, 1 / dt), 0)
    ## sample new speed, and progress particles forward:
    NEW[2,] <- msm::rtnorm(R, X[2,], ifelse(rerun, 4, 2), lower = 0, upper = 25)
    NEW[1,] <- X[1,] + NEW[2,] * pmax(0, (dt - tau))
    NEW[3,] <- X[3,]
    NEW[4,] <- X[4,]   # ifelse(is.na(X[5,]), X[4,], NA)
    NEW[5,] <- ifelse(dt - tau > 0 & at.stop, tx + tau, X[5,])
    ## work out stop-passing stuff ...
    w <- apply(NEW, 2, function(x) x[1] > s[x[3]+1])
    if (any(is.na(w))) NEW[1,] <- pmin(s[NEW[3,]], NEW[1,])
    w[is.na(w)] <- FALSE
    TIMES <- array(NA, dim = c(3, ncol(NEW), 1))
    while (any(w, na.rm=TRUE)) {
        rtau <- rexp(sum(w, na.rm=TRUE), 1/20)
        rtau <- ifelse(rtau < gamma, 0, rtau)
        rt <- dt - rtau - (s[NEW[3,w]+1] - X[1,w]) / NEW[2,w]
        NEW[1,w] <- pmin(s[NEW[3,w]+1] + (rt > 0) * NEW[2,w] * rt, max(shape$distance_into_shape))
        NEW[3,w] <- pmin(NEW[3,w] + 1, max(s))
        NEW[4,w] <- tx + (s[NEW[3,w]] - X[1,w]) / NEW[2,w]
        NEW[5,w] <- ifelse(NEW[3,w] == max(s), tn, ifelse(rt > 0, tn - rt, NA))
        TIMES <- abind::abind(TIMES, NEW[3:5, ])
        w <- apply(NEW, 2, function(x) x[1] > s[x[3]+1])
        if (any(is.na(w))) NEW[1,] <- pmin(s[NEW[3,]], NEW[1,])
        w[is.na(w)] <- FALSE
    }
    ## compute weights:
    dist <- distance(t(row[, c("position_latitude", "position_longitude")]),
                     sapply(NEW[1,], h, shape = shape))
    if (all(dist > 3 * gps)) {
        xhat <- sapply(NEW[1,], h, shape = shape)
        #addPoints(xhat[2, ], xhat[1, ], pch = 4,
        #          gp = list(col = "#99990030", cex = 1))
        ## essentially an error if none of the particles are close to the bus
        ## which error code?
        attr(NEW, "code") <- 1
        if (!rerun) ## return(structure(NEW[,1:ncol(Xold)], code = 2))
            return(NEW)
    }
    pr <- dnorm(dist, 0, gps)
    if (sum(pr > 0) < 5)  ## less than 5 valid particles
        return(structure(NEW[,1:ncol(Xold)], code = 3))
    wt <- pr / sum(pr)
    wi <- sample(R, size = ncol(Xold), replace = TRUE, prob = wt)
    X <- NEW[, wi]
    attr(X, "xhat") <- NEW
    attr(X, "code") <- 0
    attr(X, "wi") <- unique(wi)
    attr(X, "times") <- TIMES[, wi, -1, drop = FALSE]
    X
}
h <- function(x, shape) {
    ## Calculate Lat/Lon position of point(s) a given distance (x)
    ## into a pattern/shape

    if (is.na(x[1])) print(x)
    if (x[1] <= 0) return(as.numeric(shape[1, c("lat", "lon")]))
    if (x[1] > max(shape$distance_into_shape))
        return(as.numeric(shape[nrow(shape), c("lat", "lon")]))
    
    j <- which.min(x[1] > shape$distance_into_shape) - 1
    sj <- shape[j, c("lat", "lon", "bearing", "distance_into_shape")]
    Psi <- sj$bearing
    d <- x[1] - sj$distance_into_shape
    
    ## only do the calculations if we need to!
    if (d == 0) return(as.numeric(sj[1:2]))
    partialSegment(as.numeric(sj[1:2]), Psi, d)
}
