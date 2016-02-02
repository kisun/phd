setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))


loadall()
con <- dbConnect(SQLite(), "db/gtfs-history.db")
positions <- getPositions(con, route.id = "090")
latest.map <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
                         name = "Auckland Busses")
plot(latest.map, pch = 19, cex.pt = 0.1, col.pt = "#00000040")


tab <- table(positions$vehicle_id)
vehicleid <- names(tab)[which.max(tab) +1 ]

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
## dat <- positions2[-(1:14), ]
## mp <- iNZightMap(~position_latitude, ~position_longitude, data = dat, name = "Auckland Busses")
## xyT <- latlon.xy(data.frame(x=dat$position_latitude, y=dat$position_longitude), map = global.objects$maps$map)
## plot(mp, pch = 4, cex.pt = 0.4)

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


dat <- positions2[-c(1:15), ]

## Get stop info:
loadall()
scheds <- lapply(unique(dat$trip_id), getSchedule, verbose = FALSE)
names(scheds) <- unique(dat$trip_id)

## "started" = logical, has the trip started? (according to the schedule)
dat$started <- dat$timestamp -
    as.numeric(format(as.POSIXct(paste(dat$trip_start_date, dat$trip_start_time)), format = "%s")) >= 0

vehicle.map <- iNZightMap(~position_latitude, ~position_longitude, data = dat,
                          name = "Bus 090")
plot(vehicle.map, pch = 4, cex.pt = 0.5, g1 = trip_start_time, colby = started, col.pt = c("red", "gray50"),
     main = sprintf("Journey of Bus %s on %s", vehicleid, dateFmt))

plot(vehicle.map,  cex.pt = 0.5, g1 = trip_start_time, colby = started, col.pt = c("red", "gray50"),
     g1.level = 10, sizeby = timestamp,
     main = sprintf("Journey of Bus %s on %s", vehicleid, dateFmt))



## --- now the data is filtered and basically ready to analyse!

loadall()
block <- getBlock(unique(dat$trip_id))
pattern <- getPattern(unique(dat$trip_id))

head(pattern)


## devAskNewPage(TRUE)
## for (id in unique(pattern$trip_id)) {
##     plot(dat$position_longitude, dat$position_latitude)
##     with(pattern[pattern$trip_id == id, ], lines(shape_pt_lon, shape_pt_lat, pch = 4, col = "red", cex = 0.4))
## }
## devAskNewPage(FALSE)



### Implement some model ... :D
loadall()
v001 = vehicle$new(dat$vehicle_id[1],
                   dat[1, c("position_latitude", "position_longitude", "timestamp")],
                   dat$trip_id[1], pattern = pattern)
v001$plot()
v001$update()$plot()
v001$update(dat[2, c("position_latitude", "position_longitude", "timestamp")])$plot()
v001$update(dat[3, c("position_latitude", "position_longitude", "timestamp")])$plot()


pb <- txtProgressBar(2, nrow(dat), style = 3)
for (i in 2:nrow(dat)) {
    v001$update(dat[i, c("position_latitude", "position_longitude", "timestamp")],
                dat[i, "trip_id"])$plot()
    setTxtProgressBar(pb, i)
    #v001$info()
    grid::grid.locator()
}
close(pb)


hist <- v001$getParticles()

histX <- hist$x
dX <- histX[1,,]

plot(NA, xlim = range(dat$timestamp), ylim = range(dX, na.rm = TRUE),
     xlab = "Time", ylab = "Distance into Block (m)")
for (i in 1:ncol(dX)) points(rep(dat$timestamp[i], nrow(dX)), dX[, i], pch = 4)
for (i in 1:ncol(dX)) points(rep(dat$timestamp[i], nrow(dX)), hist$xhat[1,,i],
                             pch = 4, col="red", cex=0.5)
