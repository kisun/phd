library(RSQLite)

## Connect to the database:
db <- dbConnect(SQLite(), "gtfs.db")


head(latest.positions)



DRAW <- function(db = dbConnect(SQLite(), "gtfs.db")) {
    result <- dbGetQuery(db,
                         paste0("SELECT vehicle_id, vehicle_positions.trip_id, trips.shape_id, position_latitude, position_longitude, timestamp, shape_id ",
                                "FROM vehicle_positions, trips WHERE vehicle_positions.trip_id=trips.trip_id"))
    
    
    shapes <- dbGetQuery(db,
                         sprintf("SELECT * FROM shapes WHERE shape_id IN %s",
                                 paste0("('", paste(unique(result$shape_id), collapse = "', '"), "')", sep = "")))

    dev.hold()
    plot(NA, NA, xlim = range(shapes$shape_pt_lon), ylim = range(shapes$shape_pt_lat),
         xlab = "Longitude", ylab = "Latitude")
    invisible(tapply(1:nrow(shapes), shapes$shape_id, function(i) lines(shapes[i, "shape_pt_lon"], shapes[i, "shape_pt_lat"])))
    points(result$position_longitude, result$position_latitude, col = "red", pch = 19, cex = 0.4)
    dev.flush()
}
    
DRAW()

latest.positions[1, c("position_longitude", "position_latitude")]

cc <- convertGPS(lat = latest.positions$position_latitude, lon = latest.positions$position_longitude)

par(mfrow = c(2, 1))
plot(latest.positions$position_latitude, latest.positions$position_longitude, asp = 1)
plot(cc, asp = 1)
par(mfrow = c(1, 1))


plot(t(apply(latest.positions, 1, function(r) convertGPS(lat = r["position_latitude"], lon = r["position_longitude"]))))
