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
