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
## datesdates <- format(as.POSIXct(ts, origin = "1970-1-1"), "%Y-%m-%d")
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
date <- "2016-01-25"

tripTimes <- dbGetQuery(dbConnect(SQLite(), db),
                        sprintf("SELECT trips.trip_id, departure_time
                                 FROM trips, stop_times
                                 WHERE route_id LIKE '%s' AND trips.trip_id=stop_times.trip_id
                                    AND stop_sequence=1",
                                paste0(routeN, "%_v37.28")))
#tripTimes                                     

## tripids
#tids <- dbGetQuery(dbConnect(SQLite(), db),
#                   sprintf("SELECT trip_id FROM trips WHERE route_id LIKE '%s'",
#                           paste0(routeN, "%")))
#tripN <- unique(gsub("-.+", "", tids[[1]]))[7]
tripN <- gsub("-.+", "", tripTimes$trip_id)[11]


## and the row IDs for them .......
rowids <- dbGetQuery(con,
                     sprintf("SELECT oid FROM vehicle_positions
                              WHERE (%s) AND timestamp>=%s AND timestamp<%s
                              ORDER BY vehicle_id, timestamp",
                             paste0("trip_id LIKE '", tripN, "%'", collapse = " OR "),
                             ts, ts + 60 * 60 * 24 * 1))$oid; length(rowids)

## For each row, run something
i <- 0
M <- 500
info <- list(cur.trip = "", trip_id = character(), shapes = list(), schedules = list(),
             status = "")
## status: ['waiting', 'delayed', 'inprogress', 'finished']
state = matrix(NA, 6, M)
refresh <- TRUE
mean.dist <- numeric(length(rowids))
times <- numeric(length(rowids))
state.hist <- array(NA, dim = c(6, M, length(rowids)))
time.hist <- #vector("list", length(rowids))
    list(arrival = matrix(NA, length(rowids), M), departure = matrix(NA, length(rowids), M))
pb <- txtProgressBar(1, length(rowids), style = 3)
jpeg(paste0("figs/pf_singlebus/route_", routeN, "/particle_map%03d.jpg"), width = 1920, height = 1080, pointsize = 12*2)
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
        plot(mobj, pch = NA, main = "")
        addLines(shape$lon, shape$lat, gp = list(lwd = 3, col = "#550000"))
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
        }
        mean.dist[i] <- mean(new.state[1, ], na.rm = TRUE)
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
    state.hist[,,i] <- state
    if (!is.null(attr(state, "times"))) {
        for (j in 1:M) {
            A <- attr(state, "times")[,j,,drop=FALSE]
            Ta <- tapply(A[2,,], A[1,,], max)
            Td <- tapply(A[3,,], A[1,,], max)
            time.hist$arrival[as.numeric(names(Ta)), j] <- Ta
            time.hist$departure[as.numeric(names(Ta)), j] <- Td
        }
    }
    if (i > 1) state.hist[6, , i-1] <- as.numeric(1:M %in% attr(state, "wi"))
    times[i] <- row$timestamp
    addPoints(row$position_longitude, row$position_latitude,
              gp =
                  list(col =
                           switch(info$status, "waiting" = "orange", "delayed" = "red",
                                  "inprogress" = "green2", "finished" = "blue"),
                       lwd = 3, cex = 0.4), pch = 3)
}; close(pb); dev.off()

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

## dput(hist, "_data/route_history2.rda")
## hist <- dget("_data/route_history.rda")
hist <- dget("_data/route_history2.rda")







## Schedule prediction:







apply(state.hist[6,,], 2, sum)

lapply(time.hist, dim)

dev.hold()
plot(NA, xlim = range(time.hist$arrival, na.rm = TRUE),
     ylim = c(0, max(schedule$distance_into_shape)), yaxs = "i")
abline(h = schedule$distance_into_shape, lty = 3, col = "#999999")
for (i in 2:nrow(schedule)) {
    points(time.hist$arrival[i,], rep(schedule$distance_into_shape[i], M),
           col = "#99000020", pch = 19, cex = 0.8)
    points(time.hist$departure[i,], rep(schedule$distance_into_shape[i], M),
           col = "#00009920", pch = 19, cex = 0.5)
}
dev.flush()





plot(state.hist[1,,], state.hist[2,,], pch = 19, col = "#00000040",
     xlab = "Distance (m)", ylab = "Speed (m/s)")





## predicting arrival times:

### super basic state projection:
time2sec <- function(x) sapply(strsplit(x, ":"), function(y) sum(as.numeric(y) * 60^(2:0)))
arrivalTime <- function(state, schedule, t = 0, stop = nrow(schedule), draw = TRUE) {
    ## if t = 0, then result is ETA; otherwise an actual time
    Sj <- schedule$distance_into_shape[stop]
    Ta <- (Sj - state[1, ]) / state[2, ]
    Nrem <- nrow(schedule) - ifelse(is.na(state[3,]), 0, state[3,])
    p <- rbinom(sum(Nrem), 1, 0.5)
    gamma <- 10
    tau <- rexp(sum(Nrem), 1/20)
    Ta <- t + Ta + tapply(p * (gamma + tau), rep(1:length(Ta), Nrem), sum)
    if (draw) {
        abline(h = Sj, lwd = 2, col = "#666666", lty = 2)
        points(Ta, rep(Sj, length(Ta)), pch = 4, cex = 0.5, col = "#cc0000")
    }
    if (all(Nrem == 0)) return(invisible(rep(t, ncol(state))))
    invisible(pmin(10000, Ta))
}

tx <- times - min(times)
## #jpeg(paste0("figs/pf_singlebus/route_", routeN, "/arrival_last%03d.jpg"),
## #     width = 1920/2, height = 1080/2)
## for (j in 5:(length(tx - 1))) {    
##     plot(NA, xlim = c(0, 6000), ylim = range(state.hist[1,,], na.rm = TRUE),
##          xlab = "Time (s)", ylab = "Distance into Trip (m)")
##     Nt <- j
##     for (i in 1:Nt) {
##         points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#00009920", cex = 0.5)
##     }
##     for (i in (Nt + 1):length(tx)) {
##         points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#99999920", cex = 0.5)
##     }
##     arrivalTime(state.hist[,,Nt], schedule, t = tx[j])
## }
#dev.off()

## "all in one"
jpeg(paste0("figs/pf_singlebus/route_", routeN, "/arrival_last.jpg"),
     width = 1920/2, height = 1080/2)
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    arrival.last[j, ] <- arrivalTime(state.hist[,,j], schedule, t = tx[j], draw = FALSE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, max(arrival.last, na.rm = TRUE)),
     xlim = c(0, max(state.hist[1,,], na.rm = TRUE)),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(c(0, max(arrival.last, na.rm = TRUE)) / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(max(schedule$distance_into_shape) - state.hist[1,,j], arrival.last[j, ],
           pch = 19, col = "#44444430")
}
abline(h = state.hist[4,,length(tx)] - min(times), col = "red", lty = 2)
dev.off()

## "delay" to last stop
arrivalTimeSched <- function(state, schedule, stop = nrow(schedule), draw = TRUE) {
    Sj <- schedule$distance_into_shape[stop]
    Sa <- time2sec(schedule$arrival_time)  ## scheduled arrival times ...
    delay <- time2sec(format(as.POSIXct(state[4, ], origin = "1970-01-01"), "%H:%M:%S")) -
        Sa[state[3, ]]
    Ta <- Sa[stop] - min(Sa) + delay
    if (draw) {
        abline(h = Sj, lwd = 2, col = "#666666", lty = 2)
        points(Ta, rep(Sj, length(Ta)), pch = 4, cex = 0.5, col = "#cc0000")
    }
    invisible(Ta)
}



## Loop is causing issues >_< but shouldn't matter!
jpeg(paste0("figs/pf_singlebus/route_", routeN, "/arrival2_last%03d.jpg"), width = 1920, height = 1080)
for (j in 5:(length(tx) - 1)) {
##    dev.hold()
    tx <- times - min(times)
    plot(NA, xlim = c(0, 6000), ylim = range(state.hist[1,,], na.rm = TRUE),
         xlab = "Time (s)", ylab = "Distance into Trip (m)")
    Sa <- time2sec(schedule$arrival_time)
    ## points(Sa - min(Sa), schedule$distance_into_shape, lwd = 2)
    Nt <- j
    for (i in 1:Nt) {
        points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#00009920", cex = 0.5)
    }
    for (i in (Nt + 1):length(tx)) {
        points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#99999920", cex = 0.5)
    }
    arrivalTimeSched(state.hist[,,Nt], schedule)
##    dev.flush()
}
dev.off()



## use historical .... somehow
do.call(cbind, apply(state.hist, 2, function(X) {
    res <- tapply(X[5, ] - X[4, ], X[3, ], max, na.rm = TRUE)
    ifelse(is.finite(res), res, NA)
})) -> dwell

apply(state.hist, 2, function(X) {
          X <- state.hist[,1,]
          tapply(X[4,], factor(X[3,], levels = 1:nrow(schedule)), max, na.rm = TRUE) - min(times)
      }) -> Ta
apply(state.hist, 2, function(X) {
          X <- state.hist[,1,]
          tapply(X[5,], factor(X[3,], levels = 1:nrow(schedule)), max, na.rm = TRUE) - min(times)
      }) -> Td

p <- apply(dwell, 1, function(x) mean(x != 0, na.rm = TRUE))
tau <- apply(dwell, 1, function(x) mean(x[x > 0], na.rm = TRUE))

plot(NA, xlim = c(0, max(tau, na.rm = TRUE) * 1.04), ylim = c(0, length(tau) + 1),
     xlab = "Mean Dwell Time (s)",
     ylab = "Stop No.", type = "n", xaxs = "i", yaxs = "i")
abline(h = 1:length(tau), lty = 3)
points(tau, 1:length(tau), pch = 21, lwd = 2, bg = "white", cex = 2 * sqrt(p))


plot(NA, xlim = c(0, 80*60), ylim = c(0, max(schedule$distance_into_shape)),
     xlab = "Time (min)", ylab = "Distance into Trip (m)", xaxs = "i", yaxs = "i", xaxt = "n")
axis(1, at = pretty(c(0, 80)) * 60, labels = pretty(c(0, 80)))
abline(h = schedule$distance_into_shape, lty = 3, col = "#99999980")
apply(Ta, 2, function(y) points(y, schedule$distance_into_shape, pch = 19, col = "#99000030", cex = 0.5))
apply(Td, 2, function(y) points(y, schedule$distance_into_shape, pch = 19, col = "#00009930", cex = 0.5))
apply(state.hist, 2, function(X) points(tx, X[1,], pch = 19, col = "#99999930", cex = 0.2))
