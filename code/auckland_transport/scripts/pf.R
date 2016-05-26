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
## dates <- format(as.POSIXct(ts, origin = "1970-1-1"), "%Y-%m-%d")
## data.frame(table(dates),
##            dow = lubridate::wday(unique(dates), TRUE, FALSE))
date <- "2016-01-25"
ts <- as.numeric(as.POSIXct(date))

## pick a vehicle to stalk:
## vehicles <- dbGetQuery(con, sprintf("SELECT DISTINCT vehicle_id, trip_id FROM vehicle_positions
##                                      WHERE timestamp >= %s AND timestamp < %s",
##                                     ts, ts + 60 * 60 * 24))
## sort(table(vehicles$vehicle_id))
## vid <- "3787"
## vid <- "2933"
## vid <- "3704"

## get the vehicles row numbers:
## rowids <- dbGetQuery(con, sprintf("SELECT oid FROM vehicle_positions
##                                    WHERE vehicle_id='%s' AND timestamp>=%s AND timestamp<%s
##                                    ORDER BY timestamp",
##                                   vid, ts, ts + 60 * 60 * 24))$oid

## get all of the timestamps
## tss <- dbGetQuery(con, sprintf("SELECT trip_id, timestamp FROM vehicle_positions
##                                 WHERE vehicle_id='%s' AND timestamp>=%s AND timestamp<%s
##                                 ORDER BY timestamp",
##                                vid, ts, ts + 60 * 60 * 24))

## get route ID:
## route <- dbGetQuery(dbConnect(SQLite(), db),
##                     sprintf("SELECT route_id FROM trips WHERE trip_id LIKE '%s'",
##                             paste0(gsub("-.+", "", unique(tss$trip_id)[2]), "%")))
## routeN <- gsub("-.+", "", route[[1]][1])
routeN <- "04901"
date <- "2016-01-23"

## tripids
tids <- dbGetQuery(dbConnect(SQLite(), db),
                   sprintf("SELECT trip_id FROM trips WHERE route_id LIKE '%s'",
                           paste0(routeN, "%")))
tripN <- unique(gsub("-.+", "", tids[[1]]))


## and the row IDs for them .......
rowids <- dbGetQuery(con,
                     sprintf("SELECT oid FROM vehicle_positions
                              WHERE (%s) AND timestamp>=%s AND timestamp<%s
                              ORDER BY vehicle_id, timestamp",
                             paste0("trip_id LIKE '", tripN, "%'", collapse = " OR "),
                             ts, ts + 60 * 60 * 24 * 5))$oid; length(rowids)

## For each row, run something
i <- 0
M <- 500
info <- list(cur.trip = "", trip_id = character(), shapes = list(), schedules = list(),
             status = "")
## status: ['waiting', 'delayed', 'inprogress', 'finished']
state = matrix(NA, 5, M)
refresh <- TRUE
mean.dist <- numeric(length(rowids))
times <- numeric(length(rowids))

pb <- txtProgressBar(1, length(rowids), style = 3)
jpeg(paste0("figs/pf_singlebus/route_", routeN, "/particle_map%03d.jpg"), width = 1920, height = 1080)
dir.create(paste0("figs/pf_singlebus/route_", routeN))
for (i in seq_along(rowids)) {
    setTxtProgressBar(pb, i)
#    i <- i + 1
    row <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE oid='%s'", rowids[i]))
    t <- timeDiff(row$trip_start_time, ts2dt(row$timestamp, "time"))
    ## get shape and schedule
    if (!row$trip_id %in% info$trip_id) {
        db.tripid <- dbGetQuery(dbConnect(SQLite(), db),
                                sprintf("SELECT trip_id FROM trips WHERE trip_id LIKE '%s'",
                                        paste0(gsub("-.+", "", row$trip_id), "%")))
        tid <- db.tripid[[1]][1]
        shape <- getShape(tid, db = db)
        schedule <- getSchedule(tid, db = db)
        info$trip_id <- c(info$trip_id, row$trip_id)
        info$shapes[[row$trip_id]] <- shape
        info$schedule[[row$trip_id]] <- schedule
        info$status <- "init"
    }
    ## draw it!
    reset <- FALSE
    if (!is.null(attr(state, "ts")))
        if (attr(state, "ts") > row$timestamp) {
            reset <- TRUE
        }
    if (row$trip_id != info$cur.trip | reset) {
        shape <- info$shapes[[row$trip_id]]
        schedule <- info$schedule[[row$trip_id]]
        info$cur.trip <- row$trip_id
        state[1, ] <- 0
        state[3:5, ] <- NA
        attr(state, "ts") <- NULL
        info$status <- "init"
    }  
    if (refresh) {
        mobj <- iNZightMaps::iNZightMap(~lat, ~lon, data = shape)
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
                    state[2, ] <- ifelse(is.na(state[2, ]), runif(M, 0, 30), state[2, ])
                    state[3, ] <- 1
                }
            }
        }
    }
    if (info$status == "inprogress" & !is.null(attr(state, "ts"))) {
        ## run particle filter
        ## state <-
        STATE <- state
        new.state <- pfilter(STATE, row, shape, schedule)
        if (attr(new.state, "code") != 0) {
            cat("\nParticle Filter returned error", attr(new.state, "code"), " ... re-running\n")
            cat("Observation #:", i, "\n")
            new.state <- pfilter(STATE, row, shape, schedule, rerun = TRUE)
            if (attr(new.state, "code") != 0) {
                cat("Re-run didn't help ... increasing GPS error ...\n")
                new.state <- pfilter(STATE, row, shape, schedule, rerun = TRUE, gps = 20)
                if (attr(new.state, "code") != 0) {
                    cat("That didn't help either ... giving up.\n")
                }
            }
        } else {
            mean.dist[i] <- mean(new.state[1, ], na.rm = TRUE)
        }
        STATE <- state
        state <- new.state
        attr(state, "ts") <- row$timestamp
        if (refresh & attr(new.state, "code") == 0) {
            xhat <- sapply(attr(state, "xhat")[1,], h, shape = shape)
            addPoints(xhat[2, ], xhat[1, ], pch = 4,
                      gp = list(col = "#00009930", cex = 0.5))
            xhat <- sapply(state[1,], h, shape = shape)
            addPoints(xhat[2, ], xhat[1, ], pch = 4,
                      gp = list(col = "#99000030", cex = 0.8))
        }
    } else {
        attr(state, "ts") <- row$timestamp
    }
    times[i] <- row$timestamp
    addPoints(row$position_longitude, row$position_latitude,
              gp =
                  list(col =
                           switch(info$status, "waiting" = "orange", "delayed" = "red",
                                  "inprogress" = "green2", "finished" = "blue"),
                       lwd = 3, cex = 0.4), pch = 3)
}; dev.off(); close(pb)

is.zero <- mean.dist == 0
timeTS <- as.POSIXct(times[!is.zero], origin = "1970-01-01")
hour <- as.numeric(format(timeTS, "%H")) + as.numeric(format(timeTS, "%M")) / 60 +
    as.numeric(format(timeTS, "%S")) / 60 / 60
dow <- lubridate::wday(lubridate::ymd(format(timeTS, "%Y-%m-%d")), TRUE, FALSE)
jpeg(paste0("figs/pf_singlebus/route_", routeN, "/distance_time.jpg"),
     width = 1920, height = 1080)
iNZightPlots::iNZightPlot(hour, mean.dist[!is.zero] / 1e3, colby = dow,
                          main = "", xlab = "Time", ylab = "Distance Into Trip (km)",
                          pch = 19, cex.pt = 0.2, plottype = "scatter")
dev.off()



dists <- mean.dist[!is.zero]
secs <- times[!is.zero] - ts

jpeg(paste0("figs/pf_singlebus/route_", routeN, "/delta_distance_time.jpg"),
     width = 1920, height = 1080)
pos <- diff(dists) > 0
plot(diff(secs)[pos] / 60, diff(dists)[pos],
     xlab = expression(paste(Delta[t], " (min)")),
     ylab = expression(paste(Delta[d], " (m)")))
dev.off()

cor(diff(secs)[pos] / 60, diff(dists)[pos])


## save
hist <- data.frame(
    dbGetQuery(con,
               sprintf("SELECT trip_id, vehicle_id FROM vehicle_positions WHERE oid IN ('%s')",
                       paste0(rowids, collapse = "','"))),
    timestamp = times,
    distance_into_trip = mean.dist
)
hist$trip_id <- gsub("-.+", "", hist$trip_id)

iNZightPlots::iNZightPlot(timestamp, distance_into_trip, data = hist, plottype = "scatter",
                          cex.pt = 0.5)

# dput(hist, "_data/route_history.rda")

## predicting arrival times:
