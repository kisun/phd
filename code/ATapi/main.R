dev.new()
library(jsonlite)
library(ggmap)
library(lubridate)

routes <- read.csv("docs/routes.txt", header = TRUE)
shapes <- read.csv("docs/shapes.txt", header = TRUE)
trips <- read.csv("docs/trips.txt", header = TRUE)
stops <- read.csv("docs/stops.txt", header = TRUE)

api <- "?api_key=b15e1f5d-19d5-4795-8550-07c9022985f7"
westgate.search <- fromJSON(paste0("http://api.at.govt.nz/v1/gtfs/routes/search/Westgate", api))
westgate.search
route.id <- "0901RT6710"
route.trips <- fromJSON(paste0("http://api.at.govt.nz/v1/gtfs/trips/routeId/", route.id, api))

for (i in 1:nrow(route.trips$response)) {
    o <- fromJSON(paste0("http://api.at.govt.nz/v1/gtfs/stopTimes/tripId/", route.trips$response$trip_id[i], api))
    arrive <- hms(o$response$arrival_time)
    if (arrive[1] > hms("12:00:00")) {
        trip.id <- route.trips$response$trip_id[i]
        break
    }
}
trip.id

trip.stops <- fromJSON(paste0("http://api.at.govt.nz/v1/gtfs/stops/tripId/", trip.id, api))
trip.shape <- fromJSON(paste0("http://api.at.govt.nz/v1/gtfs/shapes/tripId/", trip.id, api))

bus.loc <- fromJSON(paste0("http://api.at.govt.nz/v1/public/realtime/vehiclelocations/", api, "&tripid=", trip.id))


shape <- trip.shape$response
stops <- trip.stops$response

lon <- shape$shape_pt_lon
lat <- shape$shape_pt_lat
wst <- min(lon)
est <- max(lon)
nth <- max(lat)
sth <- min(lat)
lonDiff = abs(nth - sth)
latDiff = abs(wst - est)

## using the formula from the source code
zoomLon = ceiling(log2(360*2 / lonDiff))
zoomLat = ceiling(log2(180*2 / latDiff))
zoom = min(zoomLon, zoomLat)

shape$lon <- shape$shape_pt_lon
shape$lat <- shape$shape_pt_lat

stops$lon <- stops$stop_lon
stops$lat <- stops$stop_lat

map <- get_map(c(wst, sth, est, nth), zoom = zoom)
ggmap(map) +
    geom_path(aes(x = lon, y = lat), data = shape) +
    geom_point(aes(x = lon, y = lat), data = stops)

