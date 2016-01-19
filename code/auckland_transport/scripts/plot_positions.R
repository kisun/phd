suppressMessages({
  source("src/R/.load.R")
  source("src/R/time_fns.R")
  source("src/R/database.R")
})

jpeg("images/latest_positions_tmp.jpg", width = 1920, height = 1080)

con <- dbConnect(SQLite(), "db/gtfs-realtime.db")
positions <- getPositions(con, route.id = "090")
latest.map <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
                         name = "Auckland Busses")
plot(latest.map, pch = 19, cex.pt = 0.5, col.pt = "black")

dev.off()
system("mv images/latest_positions_tmp.jpg images/latest_positions.jpg")
