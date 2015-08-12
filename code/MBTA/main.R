## Dependencies and settings:
require("RProtoBuf")
readProtoFiles("gtfs-realtime.proto")
url <- "http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb"

trips <- read.csv("gtfs/trips.txt", header = TRUE, stringsAsFactors = FALSE)
shapes <- read.csv("gtfs/shapes.txt", header = TRUE, stringsAsFactors = FALSE)
routes <- read.csv("gtfs/routes.txt", header = TRUE, stringsAsFactors = FALSE)
stops <- read.csv("gtfs/stops.txt", header = TRUE, stringsAsFactors = FALSE)
stop.times <- read.csv("gtfs/stop_times.txt", header = TRUE, stringsAsFactors = FALSE)


getVehicles <- function() {
  system(sprintf("curl -s -o vehiclepositions.pb %s", url))
  positions <- read(transit_realtime.FeedMessage, "vehiclepositions.pb")
  activeIDs <- sapply(positions$entity, function(vh) vh$vehicle$vehicle$id)

  list(positions = positions, activeIDs = activeIDs)
}

trackVehicle <- function(id, proto = getVehicles()) {
  activeIDs <- proto$activeIDs
  positions <- proto$positions

  if (id %in% activeIDs) {
    track <- positions$entity[[which(activeIDs == id)]]$vehicle
    tripID <- track$trip$trip_id

    routeID <- trips$route_id[trips$trip_id == tripID]
    route <- routes[routes$route_id == routeID, , drop = FALSE]

    shapeID <- trips$shape_id[trips$trip_id == tripID]
    shape <- shapes[shapes$shape_id == shapeID, ]

    stopIDs <- stop.times$stop_id[stop.times$trip_id == tripID]
    stop <- stops[stops$stop_id %in% stopIDs, ]

    pos <- c(track$position$longitude, track$position$latitude)

    with(shape, plot(shape_pt_lon, shape_pt_lat, type = "l",
                     xlim = range(shape_pt_lon, pos[1]),
                     ylim = range(shape_pt_lat, pos[2]),
                     main = paste0(route$route_short_name, "  ",
                                   route$route_long_name, "(",
                                   route$route_desc, ")"),
                     asp = 1))
    with(stop, points(stop_lon, stop_lat, pch = 19))
    with(shape, points(shape_pt_lon[1], shape_pt_lat[1],
                       pch = 15, col = "green4"))
    with(shape, points(shape_pt_lon[nrow(shape)], shape_pt_lat[nrow(shape)],
                       pch = 15, col = "red"))
    points(pos[1], pos[2], col = "blue", pch = 19)

    return(track)
  } else {
    return("That route is no longer available.")
  }
}
printGTFS <- function(x) {
  writeLines(as.character(x))
}

followVehicle <- function(id) {
  proto = getVehicles()

  while (id %in% proto$activeIDs) {
    printGTFS(trackVehicle(id, proto = proto))
    Sys.sleep(20)
    proto <- getVehicles()
  }

  cat("That route is no longer available ...\n")
  return(proto$activeIDs)
}

## Have a look at a single vehicle
trackVehicle("v2196", proto = latest)

## All vehicles sequentially
for (ID in activeIDs) {
 trackVehicle(ID)
 Sys.sleep(2)
}


## Follow a vehicle
getVehicles()$activeIDs
res <- followVehicle("v2196")






AVL <- trackVehicle("v2196", proto = latest)
printGTFS(AVL)

tripID <- AVL$trip$trip_id
routeID <- trips$route_id[trips$trip_id == AVL$trip$trip_id]
route.stops <- stop.times[stop.times$trip_id == AVL$trip$trip_id, ]
k <- AVL$current_stop_sequence

route.between <- route.stops[route.stops$stop_sequence %in% c(k-1, k), ]
route.between.pos <- stops[stops$stop_id %in% route.between$stop_id, ]

shapeID <- trips$shape_id[trips$trip_id == tripID]
shape <- shapes[shapes$shape_id == shapeID, ]


## 42.35393, -71.13636
## 42.35525, -71.13318
