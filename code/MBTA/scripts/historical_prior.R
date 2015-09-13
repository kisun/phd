### HISTORICAL PRIOR DISTRIBUTION FOR ARRIVAL TIME AT A STOP
setwd("~/Documents/uni/phd/code/MBTA")
set.seed(5)

## requires functions/database.R
source("functions/database.R")

## requires functions/gtfs.R
source("functions/gtfs.R")


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
sapply(shape.ids, function(id) drawRoute(id, new = FALSE))

trip.ids <- trips$trip_id[trips$shape_id == shape.ids[1]]

## Select historical data:
data <- gtfsQuery("vehicle_positions",
                  "trip_id, route_id, vehicle_id, position_latitude, position_longitude, timestamp",
                  rows = "trip_id IN %s", trip.ids,
                  order = "vehicle_id, trip_id, timestamp")

plot(data$position_longitude, data$position_latitude)
