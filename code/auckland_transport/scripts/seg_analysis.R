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

#plotSegments(unique(trip1$shape_id), db = db, zoom = 0.2)
#ClickOnZoom(0.2)
#with(trip1, addPoints(position_longitude, position_latitude, pch = 4,
#                      gpar = list(col = "#990000", alpha = 0.5, cex = 0.3)))

trip1$timeSeconds <-
    with(trip1, timestamp - as.numeric(as.POSIXct(paste(trip_start_date, trip_start_time))))

## get stop distances:
loadall()
bus <- newBus(trip1[i <- 1, ], db, n = M <- 100)
plotBus(bus, db)
with(bus$stops, addPoints(stop_lon, stop_lat, gpar = list(cex = 0.2, col = "#000099"), pch = 19))


bus <- update(bus)
plotBus(bus, db)

## i <- i + 1
## bus <- update(moveBus(bus, trip1[i, ]))
## plotBus(bus, db)
## bus$parameters[,,4]
## bus$particles[,,i]


## obj <- bus






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
           col = "#00009920", pch = 4, cex = 0.3)
}
plotDistance(bus, trip1$timeSeconds)

## second observation:
I <- i+1
for (i in I:nrow(trip1)) {
    bus <- moveBus(bus, trip1[i, ])
    bus <- update(bus)
    ## plotBus(bus, db)
    ## plotDistance(bus, trip1$timeSeconds)
}


## now using the 'best estimate' compute other stuff ...
plotDistance(bus)
dhat <- apply(bus$particles[1,,], 2, median)

Y <- dhat
t <- bus$mat["t", ] - as.numeric(as.POSIXct(paste(bus$trip_start_date[1], bus$trip_start_time[1])))

ds <- bus$stops$distance_into_trip

## model ...
library(rstan)
options(width = 100)

bus.dat <- list(N = length(Y),
                M = nrow(bus$stops),
                t = t, d = round(Y),
                s = round(bus$stops$distance_into_trip))
bus.dat$seg <-
    sapply(bus.dat$d, function(x) sum(x >= bus.dat$s + 10) *
                                  all(x <= bus.dat$s - 10 | x >= bus.dat$s + 10))
bus.dat$stop <-
    sapply(bus.dat$d, function(x) sum(x >= bus.dat$s + 10) *
                                  any(x > bus.dat$s - 10 & x < bus.dat$s + 10))

inits <- function() {
    o <- list(v = with(bus.dat, (s[-1] - s[-M]) / (smin[-1] - smin[-M])))
    o$v[is.infinite(o$v)] <- 30
    o
}

fit <- stan("src/stan/model1.stan", data = bus.dat, iter = 2000,
            warmup = 1000, thin = 1, cores = 4)

ests <- extract(fit, permuted = TRUE)
plotDistance(bus, max(bus.dat$t))

points(colMeans(ests$T), bus.dat$s, col = "#99000040", pch = 19, cex = 0.8)
points(colMeans(ests$D), bus.dat$s, col = "#00990040", pch = 19, cex = 0.8)

# apply(ests$T, 1, function(x) points(x, bus.dat$s, col = "#99000030", pch = 3, cex = 0.3))
#arrows(colMeans(ests$T), bus.dat$s, colMeans(ests$T) + mean(ests$gamma), code = 0)

drawLine <- function(T, D, s) {
    ## plotDistance(bus, max(bus.dat$t))
    xx <- c(rbind(D, T))
    ## xx[length(xx)] <- max(bus.dat$t)
    yy <- rep(s, each = 2)[-(c(1, 2 * length(s)))]
    lines(xx, yy, lwd = 1, lty = 3, col = "#00990040")
}

plotDistance(bus, max(bus.dat$t))
for (i in nrow(ests$T) - (50:0)) {
    drawLine(ests$T[i, -1], ests$D[i,-bus.dat$M], bus.dat$s)
    points(apply(ests$T, 2, min), bus.dat$s, cex = apply(ests$pi, 2, median)^2,
           col = "#aa000080", pch = 19)
}

pairs(fit, pars = c("sigsq_obs", "gamma", "mu_tau", "lp__"))











############# A silly example:

## constant speed:
v <- 10
s <- 0:5 * 500
gamma <- 10
tau <- gamma + c(0, 10, 30, 0, 20, 60)
tau <- ifelse(tau <= gamma, 0, tau)
T <- c(0, s[-1] / v + cumsum(tau[-length(tau)]))
D <- T + tau
plot(c(rbind(T, D)), rep(s, each = 2), type = "l")

F <- function(x) {
    i <- max(which(D <= x))
    min(s[i] + (v * (x - D[i])), s[i+1])
}
f <- Vectorize(F)

curve(f(x), 0, max(D), n = 1001, add=TRUE, lty = 3)

t <- seq(0, max(D), by = 30)
d <- f(t)

t <- trip1$timeSeconds
s <- round(bus$stops$distance_into_trip)


## particle filter:
N <- length(d)
M <- length(s)
Y <- rbind(lat=trip1$position_latitude, lon=trip1$position_longitude, time = t)
R <- 10
## Step 1: generate initial values
## particles
X <- array(NA, dim = c(4, R, N))
X[1,,1] <- 0
X[2,,1] <- runif(R, 0, 30)
X[3,,1] <- 1
gamma <- 10
mu_tau <- c(60, rep(20, length(s) - 1))
X[4,,1] <- 0
i <- 1
tau.list <- vector("list", length = length(mu_tau))

i <- i+1
for (i in 2:N) {
    ## Step 3: sample x[1]: this is, i = 2
    delta <- Y[3,i] - Y[3,i-1]
    if (any(X[4,,i-1] > 0)) {
        ## tau has been set - add to it ... 
        rtau <- mu_tau[X[3,,i-1]] - X[4,,i-1]
        rtau[rtau > 0] <- rexp(sum(rtau > 0), 1 / rtau[rtau > 0])
        rtau[rtau <= 0] <- rexp(sum(rtau <= 0), 2 / gamma)
        rtau <- pmin(rtau, delta)
        X[4,,i] <- X[4,,i-1] + rtau
    } else {
        rtau <- apply(X[,,i-1], 2, function(x) if (x[1] == s[x[3]]) rexp(1, 2 / gamma) else 0)
    }
    X[2,,i] <- msm::rtnorm(R, X[2,,i-1], 0.1 * (delta - rtau), lower = -0.01, upper = 30)
    X[1,,i] <- xhat <- X[1,,i-1] + X[2,,i] * (delta - rtau)
    X[3,,i] <- X[3,,i-1]
    X[4,,i] <- 0
    names(rtau) <- X[3,,i]
    ## draw...
    # plot(t, d, pch = 4, xlab = "Time (S)", ylab = "Distance (m)")
    w <- apply(X[,,i], 2, function(x) x[1] > s[x[3]+1])
    if (any(is.na(w)))
        X[1,,i] <- pmin(s[X[3,,i]], X[1,,i])
    w[is.na(w)] <- FALSE
    # points(rep(t[i], R), xhat, col = ifelse(w, "red", "black"))
    ## sample tau[1]
    while (any(w, na.rm=TRUE)) {
        tau <- rexp(sum(w, na.rm=TRUE), 1/20)
        tau <- ifelse(tau < gamma, 0, tau)
        rt <- delta - tau - (s[X[3,w,i]+1] - X[1,w,i-1]) / X[2,w,i]  ## remaining time after dwell
        xhat[w] <- s[X[3,w,i]+1] + (rt > 0) * X[2,w,i] * rt
        X[1,w,i] <- xhat[w]
        X[3,w,i] <- X[3,w,i] + 1
        X[4,w,i] <- ifelse(rt < 0, delta-rt, 0)
        w <- apply(X[,,i], 2, function(x) x[1] > s[x[3]+1])
        if (any(is.na(w)))
            X[1,,i] <- pmin(s[X[3,,i]], X[1,,i])
        w[is.na(w)] <- FALSE
    }
    ## compute weights
    dist <- distanceFlat(Y[1:2,i], sapply(X[1,,i], h, shape = bus$shapefull))
    pr <- dnorm(dist, 0, 10)
    wt <- pr / sum(pr)
    plot(rep(t[i], R), X[1,,i], pch = 4, xlab = "Time (S)", ylab = "Distance (m)",
         xlim = c(0, max(t)), ylim = c(0, max(bus$shapefull$distance_into_shape)))
    abline(h = s, lty = 3)
    ## resample particles:
    X[,,i] <- X[,ii <- sample(R, replace = TRUE, prob = wt),i]
    points(rep(t[i], R), X[1,,i], pch = 4, xlab = "Time (S)", ylab = "Distance (m)", col="red",
           xlim = c(0, max(t)), ylim = c(0, max(bus$shapefull$distance_into_shape)))
    rtau <- rtau[ii]
    if (any(rtau > 0)) {
        rtau <- rtau[rtau > 0]
        for (k in as.numeric(unique(names(rtau))))
            tau.list[[k]] <- c(tau.list[[k]], rtau[names(rtau) == k])
    }
}

plot(NA, pch = 4, xlab = "Time (S)", ylab = "Distance (m)",
     xlim = c(0, max(t)), ylim = c(0, max(bus$shapefull$distance_into_shape,
                                          X[1,,])))
for (i in 1:N) {
    points(rep(t[i], R), X[1,,i], col = "#99000050", pch = 19)
}
lines(t, colMeans(X[1,,]))
abline(h = s, lty = 3)

plot(NA, xlim = range(t), ylim = range(X[2,,]), xlab = "Time (s)", ylab = "Speed (m/s)")
for (i in 1:N) {
    points(rep(t[i], R), X[2,,i], col = "#99000050", pch = 19)
}
lines(t, colMeans(X[2,,]))
