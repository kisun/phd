setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))


loadall()
con <- dbConnect(SQLite(), "db/gtfs-history.db")
positions <- getPositions(con, route.id = "09001")
latest.map <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
                         name = "Auckland Busses")
plot(latest.map, pch = 19, cex.pt = 0.1, col.pt = "#00000040")


tab <- table(positions$vehicle_id)
vehicleid <- names(tab)[which.max(tab)]

tab2 <- table(positions$trip_start_date[positions$vehicle_id == vehicleid])
date <- names(tab2)[which.max(tab2)]

loadall()
positions2 <- getPositions(con, vehicle.id = vehicleid, date = date)
vehicle.map <- iNZightMap(~position_latitude, ~position_longitude, data = positions2,
                          name = "Auckland Busses")
dateFmt <- format(as.POSIXct(date), format = "%A %e %b, %Y")
plot(vehicle.map, pch = 19, cex.pt = 0.5, col.pt = "black",
     main = sprintf("Journey of Bus %s on %s", vehicleid, dateFmt))

plot(vehicle.map, pch = 19, cex.pt = 0.5, col.pt = "black", g1 = trip_start_time,
     main = sprintf("Journey of Bus %s on %s", vehicleid, dateFmt))



loadall()
trips <- getTrips(unique(positions2$trip_id))
trips



## let's watch a movie ...
dat <- positions2
mp <- iNZightMap(~position_latitude, ~position_longitude, data = dat, name = "Auckland Busses")
xyT <- latlon.xy(data.frame(x=dat$position_latitude, y=dat$position_longitude), map = global.objects$maps$map)


## install.packages("animation")
## library(animation)

## saveHTML({
##     for (i in 1:nrow(dat)) {
##         plot(mp, pch = 19, cex.pt = 0.2, col.pt = "gray50",
##              main = paste0("trip_id=", dat$trip_id[i], ",  route_id=", dat$route_id[i],
##                  ",  Start Time: ", dat$trip_start_time[i],
##                  ",  Current Time: ", tsTime(dat$timestamp[i])))
##         tmp = iNZightMaps:::map.xylim(current.viewport()$yscale, current.viewport()$xscale, SCALE = 2)$window.lim
##         pushViewport(viewport(xscale = tmp[1:2], yscale = tmp[3:4]))
##         grid.points(xyT$newX[i], xyT$newY[i], pch = 19,
##                     gp = gpar(col = "#ff000040"))
##     }
## }, img.name = "history090_20150118", htmlfile = "bushistory_090_2015-01-13.html",
##          ani.height = 600, ani.width = 800)





## grab "blocks"
loadall()
blocks1801 <- getBlocksA("2016-01-18")

blocks1801[blocks1801$vehicle_id == vehicleid, ]
