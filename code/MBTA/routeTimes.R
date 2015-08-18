## setup -
library(RSQLite)
con <- dbConnect(SQLite(), "gtfs-historical.db")
polylines <- function(x, y, id, ...) 
    invisible(tapply(1:length(x), id, function(i) lines(x[i], y[i], ...)))


## estimate the time to get from STOP A to STOP B, independent of the route.

### 1. find all trips that go from STOP A -> STOP B

## first, get a list of potential STOP A's
pot <- query("SELECT stop_id, COUNT(stop_id) AS n FROM stop_times WHERE trip_id IN (SELECT DISTINCT trip_id FROM vehicle_positions) GROUP BY stop_id ORDER BY n DESC")


## pick a stop to use:
STOP.A <- "64"

## Look at it ...
all.stops <- query("SELECT stop_id, stop_lat, stop_lon FROM stops")
with(all.stops, plot(stop_lon, stop_lat, pch = 19, cex = 0.3))
with(all.stops[all.stops$stop_id == STOP.A, ], points(stop_lon, stop_lat, pch = 19, col = "red"))

## all trips that use this stop:
all.trips.A <- query("SELECT trip_id, stop_id, stop_sequence FROM stop_times WHERE trip_id IN (SELECT trip_id FROM stop_times WHERE stop_id='%s' AND trip_id IN (SELECT DISTINCT trip_id FROM vehicle_positions)) ORDER BY trip_id, stop_sequence", STOP.A)
stop.A.rows <- which(all.trips.A$stop_id == STOP.A)
next.stop <- all.trips.A$stop_id[stop.A.rows + 1]
next.stop.tab <- table(next.stop)

STOP.B <- names(next.stop.tab)[which.max(next.stop.tab)]

## all trips that go from STOP A to STOP B:
trips.to.B <- unique(all.trips.A[which(all.trips.A$stop_id[stop.A.rows + 1] == STOP.B), "trip_id"])

## grab these trips from the database
trip.shapes <- query("SELECT shape_id, shape_pt_lat AS lat, shape_pt_lon AS lon, shape_pt_sequence AS seq FROM shapes WHERE shape_id IN (SELECT shape_id FROM trips WHERE trip_id IN %s)", trips.to.B)

unique(trip.shapes$shape_id)

## plot them?
with(trip.shapes, plot(lon, lat, type = "n", asp = 1))
polylines(trip.shapes$lon, trip.shapes$lat, trip.shapes$shape_id)
with(all.stops[all.stops$stop_id %in% c(STOP.A, STOP.B), ], points(stop_lon, stop_lat, pch = 19))

### 2. for these trips, estimate (interpolate) the departure time from STOP A and arrival time at STOP B

trips.history <- query("SELECT trip_id, route_id, vehicle_id, position_latitude AS lat, position_longitude AS lon, timestamp FROM vehicle_positions WHERE trip_id IN %s", trips.to.B)

### 3. compute duration of trip STOP A -> STOP B as a function of TIME and DAY

### 4. use the historical data (including the most recent!) to predict the time it will take a bus to reach STOP B if it leaves STOP A *now*


