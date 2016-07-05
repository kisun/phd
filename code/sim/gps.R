## multivariate for GPS coordinates:

library(RSQLite)
library(iNZightMaps)

con <- dbConnect(SQLite(), "../auckland_transport/db/gtfs-static.db")
r <- dbGetQuery(con, "SELECT * FROM shapes LIMIT 50")
mode(r$shape_pt_lat) <- mode(r$shape_pt_lon) <- "numeric"

x <- r$shape_pt_lon * cos(0)
y <- r$shape_pt_lat
plot(x, y, type = "l", asp = 1)




dev.new()
mm <- iNZightMap(~shape_pt_lat, ~shape_pt_lon, data = r)
plot(mm, join = TRUE, pch = NA)
