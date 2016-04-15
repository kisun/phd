setwd("~/Documents/uni/phd/code/auckland_transport")
options(width = 130)
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$",
                                all.files = TRUE, full.names = TRUE),
                     source))
loadall()

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

### Analysis using segments instead of shapefiles

## Get a bunch of routes that travel around the same area (along Symonds Street)
con <- dbConnect(SQLite(), "db/gtfs-static.db")
trips <- dbGetQuery(con, "SELECT route_id, shape_id, trip_id FROM trips WHERE trip_id IN
                          (SELECT trip_id FROM stop_times WHERE stop_id IN
                           (SELECT stop_id FROM stops WHERE stop_name LIKE 'Symonds%'))")
shapes <- unique(trips$shape_id)

## create their shape_segment files
db <- "db/gtfs-static-symonds.db"
createSegmentTable(db = db)

for (i in seq_along(shapes))
    shape2seg(id = shapes[i], db = db, plot = TRUE)


## get historical data for them:
histcon <- dbConnect(SQLite(), "db/gtfs-history.db")
dates <- dbGetQuery(histcon, sprintf("SELECT timestamp FROM vehicle_positions
                                      WHERE trip_id IN ('%s')",
                                     paste(trips$trip_id, collapse = "','")))
dates <- unique(tsDate(dates$timestamp))

## one day at a time
ts1 <- as.numeric(as.POSIXct(dates[1])) + c(0, 24 * 60 * 60)
day1 <- dbGetQuery(histcon,
                   sprintf("SELECT trip_id, route_id, vehicle_id, trip_start_time,
                                   position_latitude, position_longitude, timestamp
                              FROM vehicle_positions WHERE timestamp BETWEEN %s AND %s
                               AND trip_id IN ('%s') ORDER BY timestamp",
                           ts1[1], ts1[2], paste(trips$trip_id, collapse = "','")))

plotSegments(db = db)
with(day1, addPoints(position_longitude, position_latitude, pch = 19,
                     gp = list(cex = 0.3, col = "#990000")))

createHistoricalDb(db)

vehicles <- list()
i <- 1

pb <- txtProgressBar(1, nrow(day1), style = 3)
for (i in i:length(day1[[1]])) {
    row <- day1[i, ]
    ## grab vehicle:
    vid <- row$vehicle_id
    ## does it exists?
    if (vid %in% names(vehicles)) {
        V <- vehicles[[vid]]     ## "read from database"
        ## has the trip changed?
        if (row$trip_id != V$trip_id) {
            V <- resetBus(V, row, db)
        } else {
            V <- moveBus(V, row)
        }
    } else {
        ## create a new one
        V <- newBus(row, db, n.particles = 100)
    }
    ## plotBus(V, db)
    V <- update(V)
    ## plotBus(V, db)
    setTxtProgressBar(pb, i)
    vehicles[[vid]] <- V         ## "write to database"
    ## writeHistory(V, db)
}; close(pb)

lapply(vehicles, function(V) {
    dev.flush()
    if (dim(V$particles)[3] > 1){
        t <- V$mat["t",]
        t <- t - min(t)
        Dhat <- apply(V$particles[1,,], 2, mean)
        Dvar <- apply(V$particles[1,,], 2, range)
        plot(t, Dhat, xlab = "Time (s)", ylab = "Distance (m)", type = "n")
        arrows(t, Dvar[1,], y1=Dvar[2,], length = 0.05, angle = 90, code = 0)
        points(t, Dhat, pch = 21, bg = "white", cex = 0.7)
        locator(1)
    }
})
