setwd("~/Documents/uni/phd/code/auckland_transport")
options(width = 100)
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$",
                                all.files = TRUE, full.names = TRUE),
                     source))
loadall()

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

### Analysis using segments instead of shapefilesw3-

## Get a bunch of routes that travel around the same area (along Symonds Street)
con <- dbConnect(SQLite(), "db/gtfs-static.db")
trips <- dbGetQuery(con, "SELECT route_id, shape_id, trip_id FROM trips WHERE trip_id IN
                          (SELECT trip_id FROM stop_times WHERE stop_id IN
                           (SELECT stop_id FROM stops WHERE stop_name LIKE 'Symonds%'))")
shapes <- unique(trips$shape_id)

## create their shape_segment files
db <- "db/gtfs-static-symonds.db"
#createSegmentTable(db = db)

#for (i in seq_along(shapes))
#    shape2seg(id = shapes[i], db = db, plot = TRUE)


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

loadall()
#createHistoricalDb(db)

vehicles <- list()
i <- 1

pb <- txtProgressBar(1, nrow(day1), style = 3)
for (i in i:length(day1[[1]])) {
    row <- day1[i, ]
    ## grab vehicle:
    vid <- row$vehicle_id
    ## does it exists?
    if (vid %in% names(vehicles)) {
        V <- vehicles[[vid]]     ## "read from database" - maybe not ... 
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
    writeHistory(V, vid, db)
}; close(pb)

loadall()

trip.data <- dbGetQuery(dbConnect(SQLite(), db), "SELECT * FROM history")

## invisible(tapply(1:nrow(trip.data), trip.data$trip_id, function(i) {
##     dat <- trip.data[i, ]
##     if (all(diff(dat$distance_mean) == 0)) return()
##     with(dat, plot(timestamp, distance_mean, type = "n", xlab = "Timestamp", ylab = "Distance (m)"))
##     with(dat, arrows(timestamp, distance_min, y1=distance_max, length = 0.05, angle = 90, code = 0))
##     with(dat, points(timestamp, distance_mean, pch = 21, bg = "white", cex = 0.7))
##     locator(1)
## }))



## segments!
con <- dbConnect(SQLite(), db)
most <- names(sort(table(trip.data$segment_id)))
segs <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT segment_id FROM segments WHERE segment_id IN ('%s')",
                           paste(most, collapse = "','")))$segment_id

segs

for (seg.most in segs) {
    seg.shapes <- dbGetQuery(con,
                             sprintf("SELECT DISTINCT shape_id FROM shapes_seg WHERE segment_id='%s'",
                                     seg.most))$shape_id
    #plotSegments(id = seg.shapes, db = db)
    seg.hist <- trip.data[trip.data$segment_id == seg.most, ]
    seg.hist$distance_into_segment <- seg.hist$distance_mean - seg.hist$segment_min_distance
    if (nrow(seg.hist) <= 1) next
    if (all(diff(seg.hist$distance_into_segment) == 0)) next
    ## print(seg.hist)
    iNZightPlots::iNZightPlot(distance_into_segment, speed_median, data = seg.hist,
                              ylim = range(trip.data$speed_median))    
    ## mode(SEG$shape_pt_lat) <- mode(SEG$shape_pt_lon) <- "numeric"
    ## mp <- iNZightMap(~shape_pt_lat, ~shape_pt_lon, data = SEG)
    ## iNZightMaps:::plot.inzightmap(mp, pch = NA, main = paste("Segment", seg.most))
    ## addLines(SEG$shape_pt_lon, SEG$shape_pt_lat, gpar = list(lwd = 2))
    ## with(SEG[1, ], addPoints(shape_pt_lon, shape_pt_lat, pch = 19))
    grid::grid.locator()
}


dev.new()

iNZightPlots::iNZightPlot(timestamp, distance_into_segment, data = seg.hist, colby = route_id,
                          pch = 19, cex.pt = 0.5)
























############# complete new approach ... again :(
setwd("~/Documents/uni/phd/code/auckland_transport")
options(width = 100)
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$",
                                all.files = TRUE, full.names = TRUE),
                     source))
loadall()
library(iNZightMaps)

## get a trip:
con <- dbConnect(SQLite(), db <- "db/gtfs-static-symonds.db")
trips <- dbGetQuery(con, "SELECT trip_id, count(trip_id) AS count
                          FROM history GROUP BY trip_id ORDER BY count DESC")

trip1 <- dbGetQuery(con, sprintf("SELECT * FROM history
                                  WHERE trip_id='%s' ORDER BY timestamp",
                                 trips$trip_id[4]))
trip1 <- trip1[- (which(diff(trip1$timestamp) == 0) + 1), ]

plotSegments(unique(trip1$shape_id), db = db, zoom = 0.2)
ClickOnZoom(0.2)
with(trip1, addPoints(position_longitude, position_latitude, pch = 4,
                      gpar = list(col = "#990000", alpha = 0.5, cex = 0.3)))

trip1$timeSeconds <-
    with(trip1, timestamp - as.numeric(as.POSIXct(paste(trip_start_date, trip_start_time))))

## get stop distances:
loadall()
bus <- newBus(trip1[i <- 1, ], db, n = M <- 100)
##plotBus(bus, db)
with(bus$stops, addPoints(stop_lon, stop_lat, gpar = list(cex = 0.2, col = "#000099"), pch = 19))


bus <- update(bus)
plotBus(bus, db)

plotDistance <- function(obj, t) {
    n <- ncol(obj$mat)
    M <- dim(obj$particles)[2]
    start <- as.numeric(as.POSIXct(paste(obj$trip_start_date[1], obj$trip_start_time[1])))
    at <- as.numeric(as.POSIXct(paste(obj$trip_start_date[1], obj$stops$arrival_time))) - start
    xlim <- if (!missing(t)) c(0, max(at, ifelse(t >= start, t - start, t))) else c(0, max(at))
    plot(NA, xlim = xlim,
         ylim = c(0, max(obj$shapefull$distance_into_shape)),
         xlab = "Time (seconds)", ylab = "Distance (m)")
    st <- obj$stops$distance_into_trip
    abline(h = st, lty = 2, col = "#cccccc")
    points(at, st, pch = 17, cex = 0.6)
    if (!missing(t)) {
        if (all(t >= start)) t <- t - start
        abline(v = t, lty = 3, col = "#444444")
    }
    points(rep(obj$mat["t", ], each = M) - start, c(obj$particles[1, , ]),
           col = "#00009940", pch = 4, cex = 0.5)
}
plotDistance(bus, trip1$timeSeconds)

## second observation:
for (i in 3:nrow(trip1)) {
    bus <- moveBus(bus, trip1[i, ])
    bus <- update(bus)
    ##plotBus(bus, db)
    ##plotDistance(bus, trip1$timeSeconds)
}


## now using the 'best estimate' compute other stuff ...
plotDistance(bus)
dhat <- apply(bus$particles[1,,], 2, median)

Y <- dhat
t <- bus$mat["t", ] - as.numeric(as.POSIXct(paste(bus$trip_start_date[1], bus$trip_start_time[1])))

ds <- bus$stops$distance_into_trip

N <- nrow(bus$stops) - 1
T0 <- 0
T <- rep(NA, N)
pi0 <- 1
pi <- rep(0.5, N)
gamma <- log(30)
sig.gamma <- log(100)
D <- numeric(N)
s <- runif(N, 0, 20)

plotDistance(bus)

## Segment 1:
Y1 <- Y[Y >= ds[1] & Y < ds[2]]
t1 <- t[Y >= ds[1] & Y < ds[2]]
seg1 <- lm(Y1 ~ t1)
abline(seg1)



## model ...
library(rstan)

bus.dat <- list(N = length(Y),
                M = nrow(bus$stops),
                t = t, d = round(Y),
                s = round(bus$stops$distance_into_trip))
bus.dat$smin <- sapply(bus.dat$s, function(si) {
    r <- max(bus.dat$t[bus.dat$d <= si - 20])
    if (is.infinite(r)) return(0) else return(r)
})
bus.dat$smax <- sapply(bus.dat$s, function(si) {
    r <- min(bus.dat$t[bus.dat$d >= si + 20])
    if (is.infinite(r)) return(max(bus.dat$t)) else return(r)
})


fit <- stan(file = "src/stan/model1.stan", data = bus.dat, iter = 1000, chains = 4)
fit

ests <- extract(fit, permuted = TRUE)
plotDistance(bus)
points(colMeans(ests$T), bus.dat$s, col = "red", pch = 19)
arrows(colMeans(ests$T), bus.dat$s, colMeans(ests$T) + mean(ests$gamma), code = 0)

hist(ests$tau)
