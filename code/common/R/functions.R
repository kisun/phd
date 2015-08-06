require(ggmap)

getMap <- function(lat, lon, ...) {
  ## Dimensions for Google Maps:
  wst <- min(lon)
  est <- max(lon)
  nth <- max(lat)
  sth <- min(lat)
  latDiff <- abs(nth - sth)
  lonDiff <- abs(wst - est)
  zoomLon <- ceiling(log2(360*2 / lonDiff))
  zoomLat <- ceiling(log2(180*2 / latDiff))
  zoom <- min(zoomLon, zoomLat)

  ## Request map and plot it:
  map <- get_map(c(wst, sth, est, nth), zoom = zoom)
  map
}

plotLocations <- function(lat, lon, ...) {
  map <- getMap(lat, lon)
  coords <- data.frame(lon = lon, lat = lat)
  plot <- ggmap(map) +
            geom_point(aes(x = lon, y = lat), data = coords)

  plot
}


getTrip <- function(trip.id, gtfs.location = "./gtfs") {
  trips <- read.csv(file.path(gtfs.location, "trips.txt"), header = TRUE)
  shapes <- read.csv(file.path(gtfs.location, "shapes.txt"), header = TRUE)

  shape.id <- trips$shape_id[trips$trip_id %in% trip.id]
  out <- list(trips = trips[trips$trip_id %in% trip.id, , drop = FALSE],
              shapes = shapes[shapes$shape_id %in% shape.id, , drop = FALSE])

  class(out) <- c(class(out), "gtfs.trip")

  out
}

plot.gtfs.trip <- function(x, feed = NULL, line.col = "black", pt.col = "red", ...) {
  shapes <- x$shapes
  lon = shapes$shape_pt_lon
  lat = shapes$shape_pt_lat

  if (!is.null(feed)) {
    vehicle <- feed[feed$trip_id %in% x$trips$trip_id, , drop = FALSE]
    lon <- c(lon, vehicle$lon)
    lat <- c(lat, vehicle$lat)
  }

  map <- getMap(lat, lon)

  plot <- ggmap(map) +
    geom_path(aes(x = shape_pt_lon, y = shape_pt_lat, group = shape_id),
              color = line.col,
              data = shapes) +
    geom_point(aes(x = shape_pt_lon, y = shape_pt_lat),
              color = "blue", data = shapes[shapes$shape_pt_sequence == 1, , drop = FALSE])

  if (!is.null(feed)) {
    plot <- plot +
              geom_point(aes(x = lon, y = lat),
                         color = pt.col, data = vehicle)
  }

  plot
}

getRoute <- function(route.id, gtfs.location = "./gtfs") {
  routes <- read.csv(file.path(gtfs.location, 'routes.txt'), header = TRUE)

  out <- routes[routes$route_id == route.id, ]
  out
}

updateFeed <- function() {
  system("python read_feed.py")
}
getLatestFeed <- function() {
  updateFeed()
  read.csv("latest_feed.csv", header = TRUE)
}

trackVehicle <- function(vehicle_id) {
  feed <- getLatestFeed()
  trip.id <- as.character(feed$trip_id[feed$vehicle_id == vehicle_id])

  if (length(trip.id) != 1) {
    return("That vehicle is no longer in service.")
  }

  while (length(trip.id) == 1) {
    trip <- getTrip(trip.id)
    ggmap(plot(trip, feed = feed))


    Sys.sleep(30)
    feed <- getLatestFeed()
    trip.id <- as.character(feed$trip_id[feed$vehicle_id == vehicle_id])
    print(feed[feed$vehicle_id == vehicle_id, , drop = FALSE])
  }

}
