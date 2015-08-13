library(RSQLite)

## Connect to the database:
db <- dbConnect(SQLite(), "gtfs.db")

latest.positions <- dbGetQuery(
    db,
    "SELECT vehicle_id, trip_id, position_latitude, position_longitude, timestamp FROM vehicle_positions")
with(latest.positions, plot(position_longitude, position_latitude, pch = 3))
