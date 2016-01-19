setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))


loadall()
con <- dbConnect(SQLite(), "db/gtfs-history.db")
positions <- getPositions(con, route.id = "09001")
latest.map <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
                         name = "Auckland Busses")
plot(latest.map, pch = 19, cex.pt = 0.5, col.pt = "black")


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
