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
    from <- coords[-nrow(coords), ]
    to <- coords[-1, ]
    
    sum(distGeo(from, to))
}


distanceIntoTPI <- function(x, shape) {
    if (is.data.frame(x))
        rll <- x[, c("lat", "lon")]
    else {
        rll <- matrix(as.numeric(x[c("lat", "lon")]), ncol = 2)
        colnames(rll) <- c("lat", "lon")
    }
    
    apply(rll, 1, function(rl) {
        r <- as.numeric(convertGPS(rl["lat"], rl["lon"]))
        sh <- shape[, c("lat", "lon")]
        
        p <- convertGPS(sh$lat, sh$lon)
        d <- c(0, sapply(2:nrow(p), function(i) pathDistance(sh$lat[1:i], sh$lon[1:i])))
        
        D <- numeric(nrow(p) - 1)
        R <- D
        for (k in 1:(nrow(p) - 1)) {
            q1 <- as.numeric(p[k, ])
            q2 <- as.numeric(p[k + 1, ])
            
            if (all(q1 == q2)) D[k] <- R[k] <- NA
            
            v <- q2 - q1
            w <- r - q1
            ww <- w %*% w
            wv <- w %*% v
            vv <- v %*% v
            if (wv < 0) {
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
