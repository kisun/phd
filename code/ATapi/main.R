dev.new()
library(jsonlite)
library(iNZightMaps)
library(ggmap)

call <- "http://api.at.govt.nz/v1/gtfs/shapes/shapeId/6735?api_key=b15e1f5d-19d5-4795-8550-07c9022985f7"
result <- fromJSON(call)

lon <- result$response$shape_pt_lon
lat <- result$response$shape_pt_lat
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
location = c(nth, est, sth, wst)


data <- result$response
data$lon <- data$shape_pt_lon
data$lat <- data$shape_pt_lat

p <- drawMap(data.frame(north = location[1], east = location[2],
                        south = location[3], west = location[4]), zoom = 12)
p <- p + geom_path(aes(x = lon, y = lat), data = data)
p

#draw(result$response, lon = shape_pt_lon, lat = shape_pt_lat, location = location, zoom = zoom, type = "point")
