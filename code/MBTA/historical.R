library(RSQLite)

## Mess around with historical data ... (like half a day of it >.>)
con <- dbConnect(SQLite(), "gtfs-historical.db")


## which routes were buses?
bus.routes <- dbGetQuery(con, "SELECT route_id FROM routes WHERE route_type=3")

trips <- dbGetQuery(
    con,
    sprintf("SELECT trip_id, COUNT(*) AS n FROM vehicle_positions WHERE route_id IN %s GROUP BY trip_id",
            paste0("('", paste(bus.routes$route_id, collapse = "', '"), "')"))
)

trip.id <- "27952017"

route.history <- dbGetQuery(
    con,
    sprintf("SELECT DISTINCT trip_id, route_id, vehicle_id, position_latitude AS lat, position_longitude AS lon, timestamp FROM vehicle_positions WHERE trip_id='%s' ORDER BY vehicle_id, timestamp", trip.id)
)
route.history

## timestamps wrong - hopefully won't cause too much difficulty ...

## Draw the route ...
route.shapes <- dbGetQuery(
    con,
    sprintf("SELECT shapes.shape_id, shape_pt_lat AS lat, shape_pt_lon AS lon, shape_pt_sequence AS seq FROM shapes WHERE shape_id IN (SELECT shape_id FROM trips WHERE trip_id='%s' GROUP BY shape_id)", trip.id)
)
route.shapes


plot(route.shapes$lon, route.shapes$lat, type = "l", asp = 1)
points(route.shapes$lon[1], route.shapes$lat[1], col = "red", pch = 19)

schedule <- dbGetQuery(
    con,
    sprintf("SELECT trip_id, arrival_time, departure_time, stop_times.stop_id, stop_sequence, stop_lat AS lat, stop_lon AS lon FROM stop_times, stops WHERE trip_id='%s' AND stop_times.stop_id=stops.stop_id",
            trip.id)
)
schedule


plot(route.shapes$lon, route.shapes$lat, type = "l", asp = 1)
points(route.shapes$lon[1], route.shapes$lat[1], col = "red", pch = 5)
points(schedule$lon, schedule$lat, pch = 4, cex = 1)
points(route.history$lon, route.history$lat, col = rgb(0, 0, 1, 0.2 +(1:nrow(route.history) / nrow(route.history) * 0.8)),
       pch = 19)
