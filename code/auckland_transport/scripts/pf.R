## IDEA: a functioning particle filter applied to 'real time' data
## Will it work? Who knows!

## Features: standalone! stop using all those functions written ages ago,
## and write them again (in C if I'm clever enough ...)

setwd("../")
.libPaths("../../.Rlibrary")
library(RSQLite)
source("src/R/pf_functions.R")
source("src/R/graphics.R")

rtdb <- "db/backups/gtfs-history_201602230919.db"  # use an old database because it's smaller!
db <- "db/backups/gtfs-static_2016-02-18.db"
con <- dbConnect(SQLite(), rtdb)

## pick a day:
## ts <- dbGetQuery(con, sprintf("SELECT DISTINCT timestamp FROM vehicle_positions"))$timestamp
## table(format(as.POSIXct(ts, origin = "1970-1-1"), "%Y-%m-%d"))
date <- "2016-02-15"
ts <- as.numeric(as.POSIXct(date))

## pick a vehicle to stalk:
## vehicles <- dbGetQuery(con, sprintf("SELECT DISTINCT vehicle_id, trip_id FROM vehicle_positions
##                                      WHERE timestamp >= %s AND timestamp < %s",
##                                     ts, ts + 60 * 60 * 24))
## sort(table(vehicles$vehicle_id))
vid <- "3787"

## get the vehicles row numbers:
rowids <- dbGetQuery(con, sprintf("SELECT oid FROM vehicle_positions
                                   WHERE vehicle_id='%s' AND timestamp>=%s AND timestamp<%s",
                                  vid, ts + 7.5 * 60 * 60 + 4*60, ts + 60 * 60 * 24))$oid

## For each row, run something
i <- 0
M <- 500
info <- list(cur.trip = "", trip_id = character(), shapes = list(), schedules = list(),
             status = "")
## status: ['waiting', 'delayed', 'inprogress', 'finished']
state = matrix(NA, 5, M)
refresh <- TRUE

for (i in seq_along(rowids)) {
    ##i <- i + 1
    row <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE oid='%s'", rowids[i]))
    print(ts2dt(row$timestamp))
    t <- timeDiff(row$trip_start_time, ts2dt(row$timestamp, "time"))
    ## get shape and schedule
    if (!row$trip_id %in% info$trip_id) {
        shape <- getShape(row$trip_id, db = db)
        schedule <- getSchedule(row$trip_id, db = db)
        info$trip_id <- c(info$trip_id, row$trip_id)
        info$shapes[[row$trip_id]] <- shape
        info$schedule[[row$trip_id]] <- schedule
        info$status <- "init"
    }
    ## draw it!
    if (row$trip_id != info$cur.trip | refresh) {
        shape <- info$shapes[[row$trip_id]]
        schedule <- info$schedule[[row$trip_id]]
        info$cur.trip <- row$trip_id
        mobj <- iNZightMaps::iNZightMap(~lat, ~lon, data = shape)
        grid::grid.locator()
        plot(mobj, pch = NA)
        addLines(shape$lon, shape$lat, gp = list(lwd = 2, col = "#550000"))
        addPoints(schedule$stop_lon, schedule$stop_lat, pch = 21,
                  gp = list(cex = 0.4, col = "#550000", fill = "white", lwd = 2))
    }
    ## first check: has the trip stated?
    if (t < 0) {
        ## bus is early - zero everything
        state[1, ] <- 0
        state[2, ] <- NA
        state[3, ] <- NA
        state[4, ] <- ifelse(distance(t(row[, c("position_latitude", "position_longitude")]),
                                      t(schedule[1, c("stop_lat", "stop_lon")])) < 20,
                             0, NA)
        state[5, ] <- NA
        info$status <- "waiting"
    } else {
        ## bus should have left ...
        if (info$status %in% c("init", "waiting", "delayed")) {
            ## if it's within ~20m of first stop, consider it at the beginning of the trip
            if (distance(t(row[, c("position_latitude", "position_longitude")]),
                         t(schedule[1, c("stop_lat", "stop_lon")])) < 20) {
                info$status <- "inprogress"
                state[1, ] <- 0
                state[2, ] <- runif(M, 0, 30)
                state[3, ] <- 1
                state[4, ] <- ifelse(state[4, ] == 0, 0, t)  ## should probably interpolate this ...???
                state[5, ] <- NA  ## considered still at the stop
            } else {
                ## bus might be in progress, OR late:
                if (min(distance(t(row[, c("position_latitude", "position_longitude")]),
                                 t(shape[, c("lat", "lon")]))) > 50) {
                    info$status <- "delayed"
                } else {
                    info$status <- "inprogress"
                }
            }
        }
    }
    if (info$status == "inprogress") {
        ## run particle filter
        ## state <-
        state <- pfilter(state, row, shape, schedule)
        ## while (!is.null(attr(state, "code"))) {
        ##     state <- switch(attr(state, "code"),
        ##                     pfilter(state, row, shape, schedule, delayed = FALSE)
        ##                     )
        ## }
        attr(state, "ts") <- row$timestamp
        if (refresh) {
            xhat <- sapply(attr(state, "xhat")[1,], h, shape = shape)
            addPoints(xhat[2, ], xhat[1, ], pch = 19,
                      gp = list(col = "#00009930", cex = 0.5))
            xhat <- sapply(state[1,], h, shape = shape)
            addPoints(xhat[2, ], xhat[1, ], pch = 19,
                      gp = list(col = "#99000030", cex = 0.4))
        }
    } else {
        attr(state, "ts") <- row$timestamp
    }
    addPoints(row$position_longitude, row$position_latitude,
              gp =
                  list(col =
                           switch(info$status, "waiting" = "orange", "delayed" = "red",
                                  "inprogress" = "green3", "finished" = "blue"),
                       lwd = 2, cex = 0.6), pch = 4)
}


