library("jsonlite")
library(iNZightMaps)

getLatest <- function() {
    
    system("./atscript.sh")
    vp <- fromJSON("vehiclepositions.json", simplifyVector = FALSE)
    
    has.position <- sapply(vp$response$entity, function(x) "vehicle" %in% names(x))
    
    vps <- sapply(vp$response$entity[has.position], function(x) x$vehicle$position[c("latitude", "longitude")])
    
    vpdf <- data.frame(latitude = as.numeric(vps["latitude", ]),
                       longitude = as.numeric(vps["longitude", ]))

    attr(vpdf, "time") <-
        as.POSIXct(as.numeric(vp$response$header$timestamp), origin = "1970-01-01", tz = "GMT")
    vpdf
}

ts2time <- function(ts) {
    as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "GMT")
}

drawLatest <- function() {
    latest <- getLatest()
    obj <- iNZightMap(lat=~latitude, lon=~longitude, data = latest,
                      name = paste("Auckland Bus Locations\n", attr(latest, "time")))
    call.list <- list(x = obj, pch = 19, col.pt = "black", cex.pt = 0.1)
    do.call("plot", call.list)
}

drawLatest()
