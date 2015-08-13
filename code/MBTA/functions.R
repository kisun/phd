convertGPS <- function(lat, lon) {
    require("rgdal")
    
    LatLong <- data.frame(X = lat, Y = lon)
    names(LatLong) <- c("X", "Y")

    coordinates(LatLong) <- ~ Y + X
    proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

    Utm <- spTransform(LatLong, CRS("+proj=utm +zone=11 ellps=WGS84"))

    data.frame(lon = Utm$X, lat = Utm$Y)
}
