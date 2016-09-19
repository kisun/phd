library(RPostgreSQL)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")

trip = dbGetQuery(con, "SELECT * from trips where id='3100034043-20160907091452_v45.21'")
shape = dbGetQuery(con,
  sprintf("SELECT lat, lon, dist_traveled FROM shapes WHERE id='%s' ORDER BY pt_sequence",
          trip$shape_id))
stops = dbGetQuery(con,
  sprintf("SELECT shape_dist_traveled, lat, lon FROM stop_times AS st, stops AS s WHERE st.stop_id=id AND trip_id='%s' ORDER BY stop_sequence", trip$id))


plot(shape$lon, shape$lat, type = "l")
points(stops$lon, stops$lat)

points(t(h(stops$shape_dist_traveled, shape))[, 2:1], pch = 19, col = "red", cex = 0.5)
