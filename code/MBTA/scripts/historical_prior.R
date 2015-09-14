### HISTORICAL PRIOR DISTRIBUTION FOR ARRIVAL TIME AT A STOP
setwd("~/Documents/uni/phd/code/MBTA")
set.seed(2)

## requires functions/database.R
source("functions/database.R")

## requires functions/gtfs.R
source("functions/gtfs.R")

## requires functions/datetime.R
source("functions/datetime.R")

## requires functions/tracking.R
source("functions/tracking.R")


### We would like to obtain historical data for bus arrival at a stop,
### for a given trip,
### for a given route.

## List of all (bus) routes:
routes <- gtfsQuery("routes", "route_id", rows = "route_type=3", order = "route_id")
(route.id <- sample(routes$route_id, 1))


## List all trips along that route and their shapes:
trips <- gtfsQuery("trips", "trip_id, shape_id",
                   rows = "route_id=%s", route.id,
                   order = "shape_id, trip_id")
(shape.ids <- unique(trips$shape_id))

shapes <- gtfsQuery("shapes", "shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence",
                    rows = "shape_id IN %s",
                    shape.ids, order = "shape_id, shape_pt_sequence")
head(shapes)

plot(shapes$shape_pt_lon, shapes$shape_pt_lat, type = "n", asp = 1)
sapply(shape.ids, function(id) drawRoute(id, new = FALSE, lwd = 4))

trip.ids <- trips$trip_id[trips$shape_id == shape.ids[1]]

## Select historical data:
data <- gtfsQuery("vehicle_positions",
                  "trip_id, route_id, vehicle_id, position_latitude, position_longitude, timestamp",
                  rows = "trip_id IN %s", trip.ids,
                  order = "vehicle_id, trip_id, timestamp")
data$date <- convertTimestamp(data$timestamp, "date")
data$time <- convertTimestamp(data$timestamp, "time")

points(data$position_longitude, data$position_latitude, col = "green4", pch = 19, cex = 0.2)

## Get trip schedules
stops <- gtfsQuery("stop_times AS st, stops AS s",
                   "st.trip_id, st.arrival_time, st.departure_time, st.stop_id, s.stop_lat, s.stop_lon, st.stop_sequence",
                   rows = "st.stop_id=s.stop_id AND st.trip_id IN %s", trip.ids,
                   order = "st.trip_id, st.stop_sequence")

points(stops$stop_lon, stops$stop_lat, col = "red", pch = 19, cex = 0.5)


## Filter out observations that occur before route has started: 
trip.start <- tapply(stops$departure_time, stops$trip_id, min, na.rm = TRUE)
trip.end <- tapply(stops$arrival_time, stops$trip_id, max, na.rm = TRUE)
del <- hms(data$time) < hms(trip.start[data$trip_id]) | hms(data$time) > hms(trip.end[data$trip_id])
data <- data[!del, ]

plot(shapes$shape_pt_lon, shapes$shape_pt_lat, type = "n", asp = 1)
sapply(shape.ids, function(id) drawRoute(id, new = FALSE, lwd = 4))
points(data$position_longitude, data$position_latitude, col = "green4", pch = 19, cex = 0.2)
points(stops$stop_lon, stops$stop_lat, col = "red", pch = 19, cex = 0.5)


## Now turn these into distance-into-trips:
HISTDB <- "gtfs-historical.db"
v1 <- data
tracks <- vector("list", nrow(v1))
tracks[[1]] <- trackMyBus(v1$vehicle_id[1], v1$timestamp[1], origin = as.character(v1$date[1]))
for (i in 2:nrow(v1)) {
    tracks[[i]] <- trackMyBus(v1$vehicle_id[i], v1$timestamp[i], tracks[[i-1]]$kalman.filter, origin = "2015-08-24")
}

data$DIT <- sapply(tracks, function(x) x$track$distance.into.trip)
data$timeIntoTrip <- time2seconds(data$time) - time2seconds(trip.start[data$trip_id])

stopInfo <- query(con, "SELECT trip_id, arrival_time, departure_time, shape_dist_traveled FROM stop_times WHERE trip_id IN %s", trip.ids)
stopInfo$time <- ifelse(is.na(stopInfo$arrival_time), stopInfo$departure_time, stopInfo$arrival_time)

devAskNewPage(TRUE)
for (tid in unique(data$trip_id)) {
    plot(data$timeIntoTrip, data$DIT, type = "n")
    di <- data[data$trip_id == tid, ]
    si <- stopInfo[stopInfo$trip_id == tid, ]
    abline(h = si$shape_dist_traveled, lty = 3)
    points(time2seconds(si$time) - time2seconds(trip.start[tid]),
           si$shape_dist_traveled, pch = 19, col = "red", cex = 0.4)
    tapply(1:nrow(di), paste(di$date, di$trip_id, sep = "_"),
           function(i) lines(di$timeIntoTrip[i], di$DIT[i]))    
}
devAskNewPage(FALSE)

head(stopInfo)
