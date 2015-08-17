library(RSQLite)

## Mess around with historical data ... (like half a day of it >.>)
con <- dbConnect(SQLite(), "gtfs-historical.db")


## which routes were buses?
bus.routes <- dbGetQuery(con, "SELECT route_id FROM routes WHERE route_type=3")

routes <- dbGetQuery(
    con,
    sprintf("SELECT route_id, COUNT(*) AS n FROM vehicle_positions WHERE route_id IN %s GROUP BY route_id",
            paste0("('", paste(bus.routes$route_id, collapse = "', '"), "')"))
)

## trips <- dbGetQuery(
##    con,
##    sprintf("SELECT trip_id, COUNT(*) AS n FROM vehicle_positions WHERE route_id IN %s GROUP BY trip_id",
##            paste0("('", paste(bus.routes$route_id, collapse = "', '"), "')"))
## )

## trip.id <- "27952017"

route.id <- "1"

route.history <- dbGetQuery(
    con,
    sprintf("SELECT DISTINCT trip_id, route_id, vehicle_id, position_latitude AS lat, position_longitude AS lon, timestamp FROM vehicle_positions WHERE route_id='%s' ORDER BY vehicle_id, timestamp", route.id)
)

## timestamps wrong - hopefully won't cause too much difficulty ...

## Draw the route ...
shape.ids <- dbGetQuery(
    con,
    sprintf("SELECT trip_id, shape_id, direction FROM trips WHERE trip_id IN %s",
             paste0("('", paste(route.history$trip_id, collapse = "', '"), "')"))
)

shapes <- dbGetQuery(
    con,
    sprintf("SELECT shape_id, shape_pt_lat AS lat, shape_pt_lon AS lon, shape_pt_sequence AS seq FROM shapes WHERE shape_id IN %s",
            paste0("('", paste(unique(shape.ids$shape_id), collapse = "', '"), "')"))
)
shapes



schedule <- dbGetQuery(
    con,
    sprintf("SELECT trip_id, arrival_time, departure_time, stop_times.stop_id, stop_sequence, stop_lat AS lat, stop_lon AS lon FROM stop_times, stops WHERE trip_id IN %s AND stop_times.stop_id=stops.stop_id",
            paste0("('", paste(unique(route.history$trip_id), collapse = "', '"), "')"))
)
#schedule


## Route type 1:
sid <- "010038"
wi <- which(shapes$shape_id == sid)
shape <- shapes[wi, ]
plot(shape$lon, shape$lat, type = "l", asp = 1)
points(shapes$lon[wi[1]], shapes$lat[wi[1]], col = "red", pch = 5)

schedi <- which(schedule$trip_id %in% shape.ids$trip_id[wi])
trip.ids <- unique(schedule$trip_id[schedi])
#trip1 <- trips.north[1]
#ti <- which(schedule$trip_id == trip1)




stops <- schedule[ti, ]
points(stops$lon, stops$lat, pch = 4, cex = 1)
#points(route.history$lon, route.history$lat, col = rgb(0, 0, 1, 0.2 + (1:nrow(route.history) / nrow(route.history) * 0.8)),
#       pch = 19)


stopPos <- distanceIntoTPI(stops, shape)
stops <- cbind(stops, stopPos)

layout(matrix(c(1, 1, 1, 2)))
plot(shape$lon, shape$lat, type = "l", asp = 1)
points(shapes$lon[wi[1]], shapes$lat[wi[1]], col = "red", pch = 5)
schedi <- which(schedule$trip_id %in% shape.ids$trip_id[wi])
trip1 <- schedule$trip_id[schedi[1]]
ti <- which(schedule$trip_id == trip1)
stops <- schedule[ti, ]
points(stops$lon, stops$lat, pch = 4, cex = 1)



plot(stopPos, rep(1, length(stopPos)), pch = 19, cex = 0.5)
lines(c(0, stopPos[length(stopPos)]), c(1, 1))


## Distance over time:
route1a.north <- dbGetQuery(
    con,
    sprintf("SELECT DISTINCT vehicle_positions.trip_id, vehicle_positions.route_id, vehicle_id, position_latitude AS lat, position_longitude AS lon, timestamp FROM vehicle_positions, trips WHERE vehicle_positions.trip_id=trips.trip_id AND trips.trip_id IN %s AND trips.direction_id=0",
            paste0("('", paste(trip.ids[1:8], collapse = "', '"), "')"))
)

nrow(route1a.north)

##HIST <- route.history[route.history$trip_id %in% trips.north[1:4], ]
HIST <- route1a.north
DITs <- apply(HIST, 1, function(r) distanceIntoTPI(r, shape))
time <- format(as.POSIXct(HIST$timestamp, tz = "EST5EDT", origin = "1970-01-01"), "%H:%M:%S")
time2 <- as.POSIXct(strptime(time, "%H:%M:%S"))

timetable <- schedule[schedule$trip_id %in% unique(route1a.north$trip_id), ]
timetable$time <- ifelse(is.na(timetable$arrival_time), timetable$departure_time, timetable$arrival_time)
timetable <- merge(timetable, stops[, c("stop_id", "stopPos"), drop = FALSE], by = "stop_id", all.x = TRUE)
timetable <- timetable[order(timetable$trip_id, timetable$stop_sequence), ]

arr <- as.POSIXct(strptime(timetable$time, "%H:%M:%S"))
plot(time2, DITs, ylim = range(timetable$stopPos))
tapply(1:nrow(timetable), timetable$trip_id, function(i)
    lines(as.POSIXct(arr[i], tz = "EST5EDT"), timetable$stopPos[i]))
