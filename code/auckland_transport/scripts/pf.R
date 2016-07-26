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
## routeN <- "04901"
routeN <- "27401"
date <- "2016-01-25"

tripTimes <- dbGetQuery(dbConnect(SQLite(), db),
                        sprintf("SELECT trips.trip_id, departure_time
                                 FROM trips, stop_times
                                 WHERE route_id LIKE '%s' AND trips.trip_id=stop_times.trip_id
                                    AND stop_sequence=1 ORDER BY departure_time",
                                paste0(routeN, "%_v37.28")))
tripTimes                                     

## tripids
#tids <- dbGetQuery(dbConnect(SQLite(), db),
#                   sprintf("SELECT trip_id FROM trips WHERE route_id LIKE '%s'",
#                           paste0(routeN, "%")))
#tripN <- unique(gsub("-.+", "", tids[[1]]))[7]
## tripN <- gsub("-.+", "", tripTimes$trip_id)[11]
tripN <- gsub("-.+", "", tripTimes$trip_id)[5]

#GOGOGO(tripN)

mypng <- function(file, ...) png(gsub(".jpg", ".png", file), width = 1280, height = 720, pointsize = 18, bg = "transparent")

## and the row IDs for them .......
#GOGOGO <- function(tripN) {
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
time.hist <- vector("list", length(rowids))
    ## list(arrival = matrix(NA, length(rowids), M), departure = matrix(NA, length(rowids), M))
pb <- txtProgressBar(1, length(rowids), style = 3)
mypng(paste0("figs/pf_singlebus/route_", routeN, "/particle_map%03d.jpg"),
     width = 1920, height = 1080, pointsize = 12*2)
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
                    state[4, ] <- ifelse(is.na(state[4, ]), row$timestamp, state[4, ])
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
    time.hist[[i]] <- attr(state, "times")
    ## if (!is.null(attr(state, "times"))) {
        ## for (j in 1:M) {
        ##     A <- attr(state, "times")[,j,,drop=FALSE]
        ##     Ta <- tapply(A[2,,], A[1,,], max)
        ##     Td <- tapply(A[3,,], A[1,,], max)
        ##     time.hist$arrival[as.numeric(names(Ta)), j] <- Ta
        ##     time.hist$departure[as.numeric(names(Ta)), j] <- Td
        ## }
    ## }
    if (i > 1) state.hist[6, , i-1] <- as.numeric(1:M %in% attr(state, "wi"))
    times[i] <- row$timestamp
    addPoints(row$position_longitude, row$position_latitude,
              gp =
                  list(col =
                           switch(info$status, "waiting" = "orange", "delayed" = "red",
                                  "inprogress" = "green2", "finished" = "blue"),
                       lwd = 3, cex = 0.4), pch = 3)
}; close(pb); dev.off()
#
is.zero <- mean.dist == 0
timeTS <- as.POSIXct(times[!is.zero], origin = "1970-01-01")
hour <- as.numeric(format(timeTS, "%H")) + as.numeric(format(timeTS, "%M")) / 60 +
    as.numeric(format(timeTS, "%S")) / 60 / 60
dow <- lubridate::wday(lubridate::ymd(format(timeTS, "%Y-%m-%d")), TRUE, FALSE)
t0 <- time2sec(schedule$departure_time[1]) + dayStartSec(times[1])
## jpeg(paste0("figs/pf_singlebus/route_", routeN, "/distance_time.jpg"),
##      width = 1920, height = 1080)
## iNZightPlots::iNZightPlot(hour, mean.dist[!is.zero] / 1e3, colby = dow,
##                           main = "", xlab = "Time", ylab = "Distance Into Trip (km)",
##                           pch = 19, cex.pt = 0.2, plottype = "scatter")
## dev.off()
#
dists <- mean.dist[!is.zero]
secs <- times[!is.zero] - ts
mypng(paste0("figs/pf_singlebus/route_", routeN, "/delta_distance_time.jpg"),
     width = 1280, height = 720, pointsize = 18, bg="transparent")
pos <- diff(dists) > 0
plot(diff(secs)[pos] / 60, diff(dists)[pos],
     xlab = expression(paste(Delta[t], " (min)")),
     ylab = expression(paste(Delta[d], " (m)")))
dev.off()
#
## cor(diff(secs)[pos] / 60, diff(dists)[pos])
## save
## hist <- data.frame(
##     dbGetQuery(con,
##                sprintf("SELECT trip_id, vehicle_id FROM vehicle_positions WHERE oid IN ('%s')",
##                        paste0(rowids, collapse = "','"))),
##     timestamp = times,
##     distance_into_trip = mean.dist
## )
## hist$trip_id <- gsub("-.+", "", hist$trip_id)
#
## iNZightPlots::iNZightPlot(timestamp, distance_into_trip, data = hist, plottype = "scatter",
##                           cex.pt = 0.5)
## ## dput(hist, "_data/route_history2.rda")
## ## hist <- dget("_data/route_history.rda")
## hist <- dget("_data/route_history2.rda")
## Schedule prediction:
## apply(state.hist[6,,], 2, sum)
## lapply(time.hist, dim)
## time.hist[[4]][,,2]
mypng(paste0("figs/pf_singlebus/route_", routeN, "/distance_traveled.jpg"))
dev.hold()
plot(NA, xlim = range(times, na.rm = TRUE), xaxt = "n",
     ylim = c(0, max(schedule$distance_into_shape)), yaxs = "i",
     xlab = "Time", ylab = "Distance into Trip (m)")
Xlab <- pretty(as.POSIXct(times, origin = "1970-01-01"))
axis(1, at = Xlab, label = format(Xlab, "%H:%M"))
abline(h = schedule$distance_into_shape, lty = 3, col = "#999999")
invisible(apply(state.hist[1,,], 1, function(x)
    points(times, x, pch = 19, col = "#00009930")))
ds <- schedule$distance_into_shape
points(dayStartSec(t0) + time2sec(schedule$arrival_time), ds,
       pch = 21, bg = "white")
for (i in 2:length(rowids)) {
    for (j in 1:M) {
        Xij <- state.hist[,j,i]
        kept <- state.hist[6,j,i-1]
        if (TRUE & !is.null(time.hist[[i]])){#kept) {
            Tm <- time.hist[[i]][,j,,drop=FALSE]
            if (dim(Tm)[3] == 0) {
                Ta <- NULL
                Td <- NULL
            } else {
                Ta <- tapply(Tm[2, ,1], Tm[1, ,1], max)
                Td <- tapply(Tm[3, ,1], Tm[1, ,1], max)
            }
            xx <- c(times[i-1], Ta, Td, times[i])
            yy <- c(state.hist[1,j,i-1], ds[as.numeric(names(Ta))],
                    ds[as.numeric(names(Td))], Xij[1])
            o <- order(xx)
            xx <- xx[o]
            yy <- yy[o]
            w <- xx >= times[i-1] & xx <= times[i]
            lines(xx[w], yy[w], col = "#99000040")
        }
    }
}
dev.flush()
dev.off()
#
## remove dwell times for each particle ...
as.numfact <- function(x, N = nrow(schedule)) factor(x, levels = 1:N)
Ta <- Td <- 
    TT <- matrix(NA, nrow(schedule), M)
for (i in 2:nrow(schedule)) {
    Tai <- sapply(time.hist, function(X) {
        if (is.null(X)) return (rep(NA, M))
        apply(X, 2, function(Y) {
            yy <- Y[2,Y[1,] == i]
            if (all(is.na(yy))) return(NA)
            max(yy, na.rm = TRUE)
        })
    })
    Ta[i,] <- apply(Tai, 1, function(x)
        if (all(is.na(x))) return (NA) else return(max(x, na.rm = TRUE)))
    Tdi1 <- sapply(time.hist, function(X) {
        if (is.null(X)) return (rep(NA, M))
        apply(X, 2, function(Y) {
            yy <- Y[3,Y[1,] == i-1]
            if (all(is.na(yy))) return(NA)
            max(yy, na.rm = TRUE)
        })
    })
    Td[i-1,] <- apply(Tdi1, 1, function(x)
        if (all(is.na(x))) return (NA) else return(max(x, na.rm = TRUE)))
    TT[i,] <- pmax(0, Ta[i,] - Td[i-1,])
}
#Ta <- apply(state.hist, 2, function(P)
#    tapply(P[4,], as.numfact(P[3,]), function(z)
#         if (all(is.na(z))) return(NA) else max(z, na.rm = TRUE)))
#Ta[1, ] <- t0
#Td <- apply(state.hist, 2, function(P)
#    tapply(P[5,], as.numfact(P[3,]), function(z)
#         if (all(is.na(z))) return(NA) else max(z, na.rm = TRUE)))
#travel.t <- Ta[-1, ] - Td[-nrow(Td), ]
tt.hat <- apply(TT, 1, median, na.rm = TRUE)[-1]
#tt.quant <- apply(TT, 1, quantile, c(0.05, 0.95), na.rm = TRUE)[,-1]
dwell <- Td - Ta
dwell[dwell <= 10] <- NA
Ta.hat <- apply(Ta, 1, median, na.rm = TRUE)
if (is.na(Ta.hat[1]))
    Ta.hat[1] <- t0
## dwell <- apply(dwell, 2, function(x) pmax(0, x))
## dwell[is.na(dwell)] <- 0
## cumdwell <- apply(dwell, 2, cumsum)
## cumdwell
#
mypng(paste0("figs/pf_singlebus/route_", routeN, "/distance_average.jpg"),
     width = 1920, height = 1080, pointsize = 12*2, bg="transparent")
dev.hold()
plot(NA, xlim = c(0, max(times) - t0)/60, xaxs = "i",
     ylim = c(0, max(schedule$distance_into_shape)), yaxs = "i",
     xlab = "Travel Time (min)", ylab = "Distance into Trip (m)")
#Xlab <- pretty(as.POSIXct(times, origin = "1970-01-01"))
#axis(1, at = Xlab, label = format(Xlab, "%H:%M"))
abline(h = schedule$distance_into_shape, lty = 3, col = "#999999")
apply(TT, 2, function(x) {
    lines(c(0, cumsum(ifelse(is.na(x), 0, x)[-1])) / 60, ds, col = "#cc000040")
})
lines(txx <- c(0, cumsum(tt.hat))/60, ds, lwd = 2)
#lines(c(0, cumsum(tt.quant[1,]))/60, ds, lty = 2, lwd = 2)
#lines(c(0, cumsum(tt.quant[2,]))/60, ds, lty = 2, lwd = 2)
points(txx, ds, cex = apply(!is.na(dwell), 1, mean), col = cc <- "#222222", pch = 19)
dwell.start <- txx
dwell.mean <- apply(dwell, 1, median, na.rm = TRUE)/60
arrows(dwell.start, ds, dwell.start + dwell.mean, code = 0, lwd = 2, col = cc)
dev.flush()
dev.off()
#
mypng(paste0("figs/pf_singlebus/route_", routeN, "/distance_speed.jpg"),
     width = 1920, height = 1080, pointsize = 12*2, bg="transparent")
plot(state.hist[1,,], state.hist[2,,], pch = 19, col = "#00000040",
     xlab = "Distance (m)", ylab = "Speed (m/s)")
dev.off()
## dev.hold()
## plot(NA, xlim = range(time.hist$arrival, na.rm = TRUE),
##      ylim = c(0, max(schedule$distance_into_shape)), yaxs = "i")
## abline(h = schedule$distance_into_shape, lty = 3, col = "#999999")
## for (i in 2:nrow(schedule)) {
##     points(time.hist$arrival[i,], rep(schedule$distance_into_shape[i], M),
##            col = "#99000020", pch = 19, cex = 0.8)
##     points(time.hist$departure[i,], rep(schedule$distance_into_shape[i], M),
##            col = "#00009920", pch = 19, cex = 0.5)
## }
## dev.flush()
## draw a picture!!!!! only the ones that survive
## dev.hold()
## plot(NA, xlim = range(time.hist$arrival, na.rm = TRUE),
##      ylim = c(0, max(schedule$distance_into_shape)), yaxs = "i")
## abline(h = schedule$distance_into_shape, lty = 3, col = "#999999")
## for (i in 2:nrow(schedule)) {
##     points(time.hist$arrival[i,], rep(schedule$distance_into_shape[i], M),
##            col = "#99000020", pch = 19, cex = 0.8)
##     points(time.hist$departure[i,], rep(schedule$distance_into_shape[i], M),
##            col = "#00009920", pch = 19, cex = 0.5)
## }
## for (j in 1:M) {
##     xx <- c(times, time.hist$arrival[,j],
##                                         #ifelse(is.na(time.hist$departure[,j]), time.hist$arrival[,j],
##             time.hist$departure[,j])
##     yy <- c(state.hist[1,j,], schedule$distance_into_shape, schedule$distance_into_shape)
##     o <- order(xx)
##     lines(xx[o], yy[o], col = "#000000")
## }
## dev.flush()
## predicting arrival times:
#
### super basic state projection:
arrivalTime <- function(state, schedule, t = 0, stop = nrow(schedule), draw = TRUE) {
    ## if t = 0, then result is ETA; otherwise an actual time
    Sj <- schedule$distance_into_shape[stop]
    Ta <- t + (Sj - state[1, ]) / state[2, ]
    Nrem <- nrow(schedule) - ifelse(is.na(state[3,]), 0, state[3,])
    p <- rbinom(sum(Nrem), 1, 0.5)
    gamma <- 6
    tau <- rexp(sum(Nrem), 1/5)
    tt <- tapply(p * (gamma + tau), rep(1:length(Ta), Nrem), sum)
    Ta[Nrem > 0] <- Ta[Nrem > 0] + tt
    if (draw) {
        abline(h = Sj, lwd = 2, col = "#666666", lty = 2)
        points(Ta, rep(Sj, length(Ta)), pch = 4, cex = 0.5, col = "#cc0000")
    }
    ## if (all(Nrem == 0)) return(invisible(rep(t, ncol(state))))
    invisible(pmin(10000, ifelse(Nrem == 0, t, Ta)))
}
#
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
#
## "all in one"
mypng(paste0("figs/pf_singlebus/route_", routeN, "/arrival_last_state.jpg"),
     width = 1920/2, height = 1080/2)
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    arrival.last[j, ] <- arrivalTime(state.hist[,,j], schedule, t = tx[j], draw = FALSE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, min(max(arrival.last, na.rm = TRUE),
                         diff(range(time2sec(schedule$arrival_time))) + 90*60)),
     xlim = c(0, max(state.hist[1,,], na.rm = TRUE)),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(par()$usr[1:2] / 60, n = 15)
## ETAmin <- pretty(c(0, max(arrival.last, na.rm = TRUE)) / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(max(schedule$distance_into_shape) - state.hist[1,,j], arrival.last[j, ],
           pch = 19, col = "#44444430")
}
abline(h = (state.hist[4,,length(tx)]) - min(times), col = "#aa000040", lty = 2)
dev.off()
#
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
#
mypng(paste0("figs/pf_singlebus/route_", routeN, "/arrival_last_delay.jpg"),
     width = 1920/2, height = 1080/2)
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    arrival.last[j, ] <- arrivalTimeSched(state.hist[,,j], schedule, draw = FALSE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, min(max(arrival.last, na.rm = TRUE),
                         diff(range(time2sec(schedule$arrival_time))) + 30*60)),
     xlim = c(0, max(state.hist[1,,], na.rm = TRUE)),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(par()$usr[1:2] / 60, n = 15)
## ETAmin <- pretty(c(0, max(arrival.last, na.rm = TRUE)) / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(max(schedule$distance_into_shape) - state.hist[1,,j], arrival.last[j, ],
           pch = 19, col = "#44444430")
}
abline(h = state.hist[4,,length(tx)] - min(times), col = "#aa000040", lty = 2)
dev.off()
#
## ## Loop is causing issues >_< but shouldn't matter!
## jpeg(paste0("figs/pf_singlebus/route_", routeN, "/arrival2_last%03d.jpg"), width = 1920, height = 1080)
## for (j in 5:(length(tx) - 1)) {
## ##    dev.hold()
##     tx <- times - min(times)
##     plot(NA, xlim = c(0, 6000), ylim = range(state.hist[1,,], na.rm = TRUE),
##          xlab = "Time (s)", ylab = "Distance into Trip (m)")
##     Sa <- time2sec(schedule$arrival_time)
##     ## points(Sa - min(Sa), schedule$distance_into_shape, lwd = 2)
##     Nt <- j
##     for (i in 1:Nt) {
##         points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#00009920", cex = 0.5)
##     }
##     for (i in (Nt + 1):length(tx)) {
##         points(rep(tx[i], M), state.hist[1,,i], pch = 3, col = "#99999920", cex = 0.5)
##     }
##     arrivalTimeSched(state.hist[,,Nt], schedule)
## ##    dev.flush()
## }
## dev.off()
#
## ## use historical .... somehow
## do.call(cbind, apply(state.hist, 2, function(X) {
##     res <- tapply(X[5, ] - X[4, ], X[3, ], max, na.rm = TRUE)
##     ifelse(is.finite(res), res, NA)
## })) -> dwell

## apply(state.hist, 2, function(X) {
##           X <- state.hist[,1,]
##           tapply(X[4,], factor(X[3,], levels = 1:nrow(schedule)), max, na.rm = TRUE) - min(times)
##       }) -> Ta
## apply(state.hist, 2, function(X) {
##           X <- state.hist[,1,]
##           tapply(X[5,], factor(X[3,], levels = 1:nrow(schedule)), max, na.rm = TRUE) - min(times)
##       }) -> Td
#
## p <- apply(dwell, 1, function(x) mean(x != 0, na.rm = TRUE))
## tau <- apply(dwell, 1, function(x) mean(x[x > 0], na.rm = TRUE))

## plot(NA, xlim = c(0, max(tau, na.rm = TRUE) * 1.04), ylim = c(0, length(tau) + 1),
##      xlab = "Mean Dwell Time (s)",
##      ylab = "Stop No.", type = "n", xaxs = "i", yaxs = "i")
## abline(h = 1:length(tau), lty = 3)
## points(tau, 1:length(tau), pch = 21, lwd = 2, bg = "white", cex = 2 * sqrt(p))
#
## plot(NA, xlim = c(0, 80*60), ylim = c(0, max(schedule$distance_into_shape)),
##      xlab = "Time (min)", ylab = "Distance into Trip (m)", xaxs = "i", yaxs = "i", xaxt = "n")
## axis(1, at = pretty(c(0, 80)) * 60, labels = pretty(c(0, 80)))s
## abline(h = schedule$distance_into_shape, lty = 3, col = "#99999980")
## apply(Ta, 2, function(y) points(y, schedule$distance_into_shape, pch = 19, col = "#99000030", cex = 0.5))
## apply(Td, 2, function(y) points(y, schedule$distance_into_shape, pch = 19, col = "#00009930", cex = 0.5))
## apply(state.hist, 2, function(X) points(tx, X[1,], pch = 19, col = "#99999930", cex = 0.2))
#
#save.image()
#
#setwd("../")
#.libPaths("../../.Rlibrary")
#library(RSQLite)
#load(".RData")
                                        #










## Historical!
arrivalTimeHist <- function(state, schedule, t = 0, route.id,
                            stop = nrow(schedule), draw = FALSE,
                            hdb = "db/hist.db", returnNA = FALSE) {
    hist <- dbGetQuery(dbConnect(SQLite(), hdb),
                       sprintf("SELECT * FROM travel_history
                                WHERE route_id = '%s'",
                               route.id, min(state[3,], na.rm = TRUE), stop))
    ds <- schedule$distance_into_shape
    ts <- tapply(hist$travel_time, hist$stop_no, mean)
    pr <- tapply(hist$pr_stop, hist$stop_no, mean)
    gamma <- 6
    tau <- pmax(0, tapply(hist$dwell_time, hist$stop_no, mean) - gamma)
    stopl <- stop - 1
    TT <- apply(state, 2, function(x) {
        if (x[3] >= stop) return(0)
        ## travel time remaining
        tr <- (1 - (x[1] - ds[x[3]]) / (ds[x[3]+1] - ds[x[3]])) * ts[x[3]]
        if (x[3]+1 == stop) return(tr)
        tr <- tr + sum(ts[-(1:x[3])])
        ## dwell times
        Nr <- stop - x[3] - 1
        #if (Nr == 0) return(tr)
        p <- rbinom(Nr, 1, pr[x[3]:stopl])
        tau <- rexp(Nr, 1 / pmax(1, tau[x[3]:stopl]))
        tr <- tr + sum(p * (gamma + tau))
    })
    if (returnNA)
        return(ifelse(TT == 0, NA, TT + t))
    TT + t
}
##
mypng(paste0("figs/pf_singlebus/route_", routeN, "/arrival_last_hist.jpg"),
     width = 1920/2, height = 1080/2)
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    if (any(is.na(state.hist[3,,j]))) next
    arrival.last[j, ] <- arrivalTimeHist(state.hist[,,j], schedule, t = tx[j],
                                         route.id = row$route_id, draw = FALSE, returnNA = TRUE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, min(max(arrival.last, na.rm = TRUE),
                         diff(range(time2sec(schedule$arrival_time))) + 30*60)),
     xlim = c(0, max(state.hist[1,,], na.rm = TRUE)),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(par()$usr[1:2] / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(max(schedule$distance_into_shape) - state.hist[1,,j], arrival.last[j, ],
           pch = 19, col = "#44444430")
}
abline(h = Ta[nrow(schedule),] - min(times), col = "#aa000040", lty = 2)
dev.off()

mypng(paste0("figs/pf_singlebus/route_", routeN, "/arrival_20_hist.jpg"))
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    if (any(is.na(state.hist[3,,j]))) next
    arrival.last[j, ] <- arrivalTimeHist(state.hist[,,j], schedule, t = tx[j],
                                         route.id = row$route_id, stop=20, draw = FALSE, returnNA = TRUE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, min(max(arrival.last, na.rm = TRUE),
                         diff(range(time2sec(schedule$arrival_time))) + 30*60)),
     xlim = c(0, schedule$distance_into_shape[20]),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(par()$usr[1:2] / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(schedule$distance_into_shape[20] - state.hist[1,,j],
           ifelse(arrival.last[j, ] == 0, NA, arrival.last[j,]),
           pch = 19, col = "#44444430")
}
abline(h = Ta[20, ] - min(times), col = "#aa000040", lty = 2)
dev.off()

mypng(paste0("figs/pf_singlebus/route_", routeN, "/arrival_15_hist.jpg"))
otx <- times - min(times)
arrival.last <- matrix(NA, length(tx), M)
for (j in 1:length(tx)) {
    if (any(is.na(state.hist[3,,j]))) next
    arrival.last[j, ] <- arrivalTimeHist(state.hist[,,j], schedule, t = tx[j],
                                         route.id = row$route_id, stop=15, draw = FALSE, returnNA = TRUE)
}
par(mar = c(5.1, 6.1, 4.1, 2.1))
plot(NA, ylim = c(0, min(max(arrival.last, na.rm = TRUE),
                         diff(range(time2sec(schedule$arrival_time))) + 30*60)),
     xlim = c(0, schedule$distance_into_shape[15]),
     xlab = "Distance from Stop (m)", yaxt = "n", ylab = "")
ETAmin <- pretty(par()$usr[1:2] / 60, n = 15)
ETA <- as.POSIXct(time2sec(schedule$arrival_time[1]) + ETAmin * 60,
                  origin = "1970-01-01", tz = "NZDT")
axis(2, at = ETAmin * 60, labels = format(ETA, "%H:%M:%S"), las = 2)
title(ylab = "ETA", line = 5)
for (j in 1:length(tx)) {
    points(schedule$distance_into_shape[15] - state.hist[1,,j],
           ifelse(arrival.last[j, ] == 0, NA, arrival.last[j,]),
           pch = 19, col = "#44444430")
}
abline(h = Ta[15, ] - min(times), col = "#aa000040", lty = 2)
dev.off()





hist.df <- data.frame(trip_id = tid, route_id = row$route_id,
                      date = format(ts2dt(t0), "%Y-%m-%d"),
                      vehicle_id = row$vehicle_id,
                      stop_no = 1:nrow(schedule),
                      arrival_time = Ta.hat,
                      dwell_time = ifelse(is.na(dwell.mean), 0, dwell.mean * 60),
                      pr_stop = apply(!is.na(dwell), 1, mean, na.rm = TRUE),
                      pr_stop_se = apply(!is.na(dwell), 1, sd) / sqrt(M),
                      travel_time = c(tt.hat, 0))
ENT <- paste(apply(hist.df, 1, paste, collapse = "','"), collapse = "'), ('")
dbGetQuery(dbConnect(SQLite(), "db/hist.db"),
           sprintf("INSERT INTO travel_history (%s)
                         VALUES ('%s')", paste(colnames(hist.df), collapse = ", "), ENT))

