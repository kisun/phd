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
routes <- gtfsRoutes("route_id", rows = "route_type=3", order = "route_id")
route.id <- sample(routes$route_id, 1)


## List all trips along that route:
trips <- gtfsTrips("trip_id", rows = "route_id=%s", route.id, order = "trip_id")
trip.id <- sample(trips$trip_id, 1)
