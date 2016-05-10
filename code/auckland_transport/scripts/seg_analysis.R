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
##trips <- dbGetQuery(con, "SELECT route_id, shape_id, trip_id FROM trips WHERE trip_id IN
##                          (SELECT trip_id FROM stop_times WHERE stop_id IN
##                           (SELECT stop_id FROM stops WHERE stop_name LIKE 'Symonds%'))")
trips <- dbGetQuery(con, "SELECT route_id, shape_id, trip_id FROM trips WHERE route_id LIKE '27701%'")
shapes <- unique(trips$shape_id)
routes <- unique(trips$route_id)

## create their shape_segment files
db <- "db/gtfs-static-277.db"
createSegmentTable(db = db)

for (i in seq_along(shapes))
    shape2seg(id = shapes[i], db = db, plot = TRUE)


## get historical data for them:
histcon <- dbConnect(SQLite(), "db/gtfs-history.db")
dates <- dbGetQuery(histcon, sprintf("SELECT timestamp FROM vehicle_positions
                                      WHERE trip_id IN ('%s')",
                                     paste(unique(trips$trip_id), collapse = "','")))
dates <- unique(tsDate(dates$timestamp))

## one day at a time
ts1 <- as.numeric(as.POSIXct(dates[8])) + c(0, 24 * 60 * 60)
day1 <- dbGetQuery(histcon,
                   sprintf("SELECT trip_id, route_id, vehicle_id, trip_start_time,
                                   position_latitude, position_longitude, timestamp
                              FROM vehicle_positions WHERE timestamp BETWEEN %s AND %s
                               AND trip_id IN ('%s') ORDER BY timestamp",
                           ts1[1], ts1[2], paste(unique(trips$trip_id), collapse = "','")))

plotSegments(db = db)
with(day1, addPoints(position_longitude, position_latitude, pch = 19,
                     gp = list(cex = 0.3, col = "#990000")))

loadall()
#createHistoricalDb(db, TRUE)

vehicles <- list()
i <- 1

t <- unique(day1$trip_id)[5]
tript <- day1[day1$trip_id == t, ]; nrow(tript)

i <- 1
pb <- txtProgressBar(1, nrow(day1), style = 3)
for (i in i:length(tript[[1]])) {
    row <- tript[i, ]
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
                                 trips$trip_id[1]))
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










############# complete new approach ... again :(
setwd("~/Documents/uni/phd/code/auckland_transport")
options(width = 100)
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$",
                                all.files = TRUE, full.names = TRUE),
                     source))
loadall()
library(iNZightMaps)
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

curve(f(x), 0, max(D), n = 1001, lty = 3)

t <- seq(0, max(D), by = 30)
d <- f(t)

## get a trip:
routes <- tapply(day1$trip_id, day1$route_id, unique)
sapply(routes, length)

route1 <- names(routes)[1]
trips <- routes[[route1]]


KEEP <- numeric()

for (T in seq_along(trips)) {
    trip1 <- day1[day1$trip_id == trips[T], ]
    trip1 <- trip1[- (which(diff(trip1$timestamp) == 0) + 1), ]
    trip1$timeSeconds <- with(trip1, timestamp - min(timestamp))
    ## get stop distances:
    bus <- newBus(trip1[1, ], db, n = 10)
    t <- trip1$timeSeconds  #trip1$timeSeconds
    s <- round(bus$stops$distance_into_trip)
    d <- s
    mp <- iNZightMap(~position_latitude, ~position_longitude, data = trip1)
    plot(mp)
    for(i in 1:nrow(trip1)) {
        with(trip1[i,], addPoints(position_longitude, position_latitude, pch = 3, gp=list(col = "red",lwd=2)))
                                        #grid::grid.locator()
        #Sys.sleep(0.1)
    }
    if (readline("Keep? (y/n)") == "y")
        KEEP <- c(KEEP, trips[T])
}

SAVE <- vector("list", length = length(KEEP))
names(SAVE) <- KEEP
for (Ki in seq_along(KEEP)) {
    id <- KEEP[Ki]
    trip1 <- day1[day1$trip_id == id, ]
    trip1 <- trip1[- (which(diff(trip1$timestamp) == 0) + 1), ]
    trip1$timeSeconds <- with(trip1, timestamp - min(timestamp))
    ## get stop distances:
    bus <- newBus(trip1[i <- 1, ], db, n = M <- 100)
    t <- trip1$timeSeconds  #trip1$timeSeconds
    s <- round(bus$stops$distance_into_trip)
    d <- s
    ## particle filter:
    N <- length(t)
    M <- length(s)
    Y <- rbind(lat=trip1$position_latitude, lon=trip1$position_longitude, time = t)
    ##Y <- rbind(d, NA, t)
    R <- 500
    ## Step 1: generate initial values
    ## particles
    X <- array(NA, dim = c(5, R, N))
    X[1,,1] <- 0
    X[2,,1] <- runif(R, 0, 30)
    X[3,,1] <- 1
    X[4:5,,1] <- NA
    pb <- txtProgressBar(1, N, style = 3)
    draw <- FALSE
    if (draw) {
        plot(NA, pch = 4, xlab = "Time (S)", ylab = "Distance (m)",
             xlim = c(0, max(t)), ylim = c(0, max(d)))
        abline(h = s, lty = 3)
    }
    for (i in 2:N) {
        setTxtProgressBar(pb, i)
        ## Step 3: sample x[1]: this is, i = 2
        if (all(X[1,,i-1] == max(s))) break
        delta <- Y[3,i] - Y[3,i-1]
        ## probability of staying where we are:
        at.stop <- X[1,,i-1] - s[X[3,,i-1]] < 10
        stay <- rbinom(R, 1, ifelse(at.stop,
                                    if (Ki > 1) ifelse(X[3,,i-1] %in% names(mu.tau),
                                                       Pi[as.character(X[3,,i-1])],
                                                       0.1)
                                    else 0.5,
                                    0.05))
        tau <- ifelse(stay,
                      rexp(R, ifelse(at.stop,
                                     if (delta > 60) 1/delta
                                     else if (Ki > 1) ifelse(X[3,,i-1] %in% names(mu.tau),
                                                             1/mu.tau[as.character(X[3,,i-1])],
                                                             1/mean(mu.tau))
                                     else 1/20,
                                     1/10)),
                      0)
        X[2,,i] <- msm::rtnorm(R, X[2,,i-1], 0.05 * pmax(0, (delta - tau)), lower = 0, upper = 30)
        X[1,,i] <- xhat <- X[1,,i-1] + X[2,,i] * pmax(0, (delta - tau))
        X[3,,i] <- X[3,,i-1]
        X[4,,i] <- ifelse(is.na(X[5,,i-1]), X[4,,i-1], NA)
        X[5,,i] <- ifelse(delta - tau > 0 & !is.na(X[4,,i]), t[i-1] + tau, NA)
        ## draw...
        w <- apply(X[,,i], 2, function(x) x[1] > s[x[3]+1])
        if (any(is.na(w)))
            X[1,,i] <- pmin(s[X[3,,i]], X[1,,i])
        w[is.na(w)] <- FALSE
        ## sample tau[1]
        while (any(w, na.rm=TRUE)) {
            rtau <- rexp(sum(w, na.rm=TRUE), 1/20)
            rtau <- ifelse(rtau < gamma, 0, rtau)
            rt <- delta - rtau - (s[X[3,w,i]+1] - X[1,w,i-1]) / X[2,w,i]  ## remaining time after dwell
            xhat[w] <- s[X[3,w,i]+1] + (rt > 0) * X[2,w,i] * rt
            X[1,w,i] <- xhat[w]
            X[3,w,i] <- X[3,w,i] + 1
            X[4,w,i] <- Y[3,i-1] + (s[X[3,w,i]] - X[1,w,i-1]) / X[2,w,i]
            X[5,w,i] <- ifelse(rt > 0, Y[3,i] - rt, NA)
            w <- apply(X[,,i], 2, function(x) x[1] > s[x[3]+1])
            if (any(is.na(w)))
                X[1,,i] <- pmin(s[X[3,,i]], X[1,,i])
            w[is.na(w)] <- FALSE
        }
        ## compute weights
        dist <- distanceFlat(Y[1:2,i], sapply(X[1,,i], h, shape = bus$shapefull))
        ##dist <- Y[1,i] - X[1,,i]
        z <- 1
        pr <- dnorm(dist, 0, 10^z)
        while (all(pr == 0)) {
            z <- z + 1
            pr <- dnorm(dist, 0, 10^z)
        }
        if (z > 1) warning("Bad fit ... z = ", z)
        wt <- pr / sum(pr)
        ## points(rep(t[i], R), X[1,,i], pch = 19, col = "#cccccc80", cex = 0.5)
        t0 <- t[i-1]
        t1 <- t[i]
        ## resample particles:
        ii <- sample(R, replace = TRUE, prob = wt)
        ## points(rep(t[i], length(unique(ii))), X[1,unique(ii),i], col = "#cc000060", pch = 19, cex = 1)
        ji <- 1:R
        if (draw) {
            for (j in ji[order(ji %in% ii)]) {
                if (diff(X[1,j,(i-1):i]) < 0) next
                colj <- ifelse(j %in% ii, "#cc000060", "#cccccc60")
                lwdj <- ifelse(j %in% ii, 2, 1)
                ltyj <- ifelse(j %in% ii, 1, 3)
                ## draw path of each particle:
                if (is.na(X[4,j,i])) {
                    if (tau[j] > 0) {
                        lines(c(t0, t0 + min(tau, delta), t1),X[1,j,c(i-1,i-1,i)],lty=ltyj,col=colj,lwd=lwdj)
                    } else {
                        lines(c(t0, t1), X[1,j,(i-1):i], lty = ltyj, col = colj, lwd = lwdj)
                    }
                } else {
                    if (X[4,j,i] > t0) {  ## case that particle arrives at stop
                        lines(c(t0, X[4,j,i]), c(X[1,j,i-1], s[X[3,j,i]]), lty = ltyj, col = colj, lwd = lwdj)
                        if (is.na(X[5,j,i])) {  ## still at stop
                            lines(c(X[4,j,i], t1), rep(s[X[3,j,i]],2), lty=ltyj, col=colj, lwd=lwdj)
                        } else { ## gets past stop
                            lines(c(X[4:5,j,i],t1),c(rep(s[X[3,j,i]],2),X[1,j,i]),lty=ltyj,col=colj,lwd=lwdj)
                        }
                    } else {  ## particle was already at stop:
                        if (is.na(X[5,j,i])) { ## particle STILL at stop ...
                            lines(c(t0, t1), rep(s[X[3,j,i]], 2), lty = ltyj, col = colj, lwd = lwdj)
                        } else {
                            lines(c(t0,X[5,j,i],t1),c(rep(s[X[3,j,i]],2),X[1,j,i]),lty=ltyj,col=colj,lwd=lwdj)
                        }
                    }
                }
            }
        }
        X[,,i] <- X[,ii,i]
    }; close(pb)
    Tau <- tapply(c(X[5,,] - X[4,,]), c(X[3,,]), function(x) x[!is.na(x) & x > 0])
    mu.tau <- sapply(Tau[sapply(Tau, length) > 0], mean)
    Pi <- tapply(c(X[5,,] - X[4,,]), c(X[3,,]), function(x) mean(x > 0 | is.na(x)) * 0.8 + 0.1)
    SAVE[[id]] <- list(X = X, t = t, s = s, Y = Y, N = N, M = M, R = R,
                       start = bus$mat["t", 1], mu.tau = mu.tau, pi = Pi)
}

## plot(NA, pch = 4, xlab = "Time (S)", ylab = "Distance (m)",
##      xlim = c(0, max(t)), ylim = c(0, max(d)))
## abline(h = s, lty = 3)
## lines(xx, yy, col="#cccccc40", lty = 3)

layout(1)
cols <- colorspace::rainbow_hcl(length(SAVE))
for (i in 1:length(SAVE)) SAVE[[i]]$colour <- cols[i]

plot(NA, pch = 4, xlab = "Time (S)", ylab = "Distance (m)",
     xlim = range(sapply(SAVE, function(T) c(T$start, max(T$t + T$start)))),
     ylim = c(0, max(SAVE[[1]]$s)))
abline(h = SAVE[[1]]$s, lty = 3)
lapply(SAVE, function(T) {
    try({
        for (i in 1:T$N) {
            points(T$start + rep(T$t[i], R),
                   T$X[1,,i], col = "#99000050", pch = 19)
        }
        Ta <- T$X[4,,]
        for (k in 1:T$M) {
            Tk <- Ta[T$X[3,,] == k]
            Tk <- Tk[Tk > 0 & !is.na(Tk)]
            points(T$start + Tk[Tk > 0], rep(T$s[k], length(Tk)), col = "#00009940", pch = 4)
        }
        lines(T$start + T$t, colMeans(T$X[1,,]))
    })
})

plot(NA,
     xlim = range(sapply(SAVE, function(T) range(T$X[1,,], na.rm = TRUE))),
     ylim = range(sapply(SAVE, function(T) range(T$X[2,,], na.rm = TRUE))),
     xlab = "Distance (m)", ylab = "Speed (m/s)")
abline(v = SAVE[[1]]$s, lty = 3)
lapply(SAVE, function(T) {
    for (i in 1:T$N) {
        points(T$X[1,,i], T$X[2,,i], col = "#99000050", pch = 19)
    }
    lines(colMeans(T$X[1,,], na.rm = TRUE), colMeans(T$X[2,,], na.rm = TRUE), lwd = 2)
})


layout(matrix(1:2, nrow = 2), heights = c(3, 1))
o <- par(mar = c(0, 4.1, 4.1, 2.1))
plot(NA, xlim = c(0, M) + 0.5,
     ylim = c(0, max(sapply(SAVE, function(T) max(T$mu.tau))) * 1.04),
     ylab = "Dwell Time (s)", xaxt = "n", xaxs = "i", yaxs = "i")
rect(0, 0, 1e4, 1e4, col = "#333333")
lapply(SAVE, function(T) {
    lines(as.numeric(names(T$mu.tau)), T$mu.tau, pch = 19,
          col = T$colour, lwd = 2)
    points(as.numeric(names(T$mu.tau)), T$mu.tau, pch = 19,
           col = T$colour)
})
par(mar = c(5.1, 4.1, 0, 2.1))
plot(NA, xlim = c(0, M) + 0.5, ylim = c(0,1), xaxs = "i", yaxs = "i",
     xlab = "Stop No.", ylab = expression(pi[j]))
rect(0, 0, 1e4, 1, col = "#333333")
lapply(SAVE, function(T) {
    xx <- as.numeric(names(T$pi))
    arrows(xx - 0.4, T$pi, xx + 0.4, code = 0, col = T$colour)
})
par(o)

tau.mat <- matrix(NA, length(SAVE), M)
for (i in 1:length(SAVE))
    tau.mat[i, as.numeric(names(SAVE[[i]]$mu.tau))] <- SAVE[[i]]$mu.tau
tau.mat2 <- apply(tau.mat, 2, function(x) pmax(-2, diff(x) / x[-1])) * 100
layout(matrix(1:2, nrow = 2), heights = c(3, 1))
o <- par(mar = c(0, 4.1, 4.1, 2.1))
plot(NA, xlim = c(0, M) + 0.5,
     ylim = range(tau.mat2, na.rm = TRUE),
     ylab = "Dwell Time (s)", xaxt = "n", xaxs = "i")
rect(-1e6, -1e6, 1e6, 1e6, col = "#333333")
for (i in 2:nrow(tau.mat2)) {
    x <- tau.mat2[i, ]
    ii <- which(!is.na(x))
    lines(ii, x[ii], col = SAVE[[i]]$colour, lwd = 2)    
}
par(mar = c(5.1, 4.1, 0, 2.1))
plot(NA, xlim = c(0, M) + 0.5, ylim = c(0,1), xaxs = "i", yaxs = "i",
     xlab = "Stop No.", ylab = expression(pi[j]))
rect(0, 0, 1e4, 1, col = "#333333")
lapply(SAVE, function(T) {
    xx <- as.numeric(names(T$pi))
    arrows(xx - 0.4, T$pi, xx + 0.4, code = 0, col = T$colour)
})
par(o)




## correlation between stop K and K-1:
cbind(as.numeric(tau.mat[-1, -1]),
      as.numeric(tau.mat[-nrow(tau.mat), -1]),
      rep(2:ncol(tau.mat), each = nrow(tau.mat) - 1)) -> tau.comp

tapply(1:nrow(tau.comp), tau.comp[,3],
       function(i) cor(tau.comp[i,1], tau.comp[i,2], use='complete.obs'))


par(mfrow = c(5, 8), mar = c(0,0,0,0))
for (i in 1:ncol(tau.mat)) {
    plot(tau.mat[-nrow(tau.mat), i], tau.mat[-1, i], xaxt = "n", yaxt = "n",
         xlim = c(0, max(tau.mat, na.rm = TRUE)), ylim = c(0, max(tau.mat, na.rm = TRUE)))
}
par(o)


pi.mat <- matrix(NA, length(SAVE), M)
for (i in 1:length(SAVE))
    pi.mat[i, as.numeric(names(SAVE[[i]]$pi))] <- SAVE[[i]]$pi


par(mfrow = c(5, 8), mar = c(0,0,0,0))
for (i in 1:ncol(pi.mat)) {
    plot(pi.mat[-nrow(pi.mat), i], pi.mat[-1, i], xaxt = "n", yaxt = "n",
         xlim = 0:1, ylim = 0:1)
}
par(o)



MM <- 10
LIM <- c(0, max(tau.mat, na.rm = TRUE))
par(mfrow = rep(MM - 1, 2), mar = rep(0, 4))
for (i in 2:MM) {
    for (j in 2:MM) {
        if (i == j) hist(tau.mat[, i], xlim = LIM, xaxt = "n", yaxt = "n")
        else if (i < j)
            plot(tau.mat[-nrow(tau.mat), i], tau.mat[-1, j],
                 xlim = LIM, ylim = LIM, xaxt = "n", yaxt = "n")
        else
            plot(NA, xlim = LIM, ylim = LIM, xaxt = "n", yaxt = "n")
    }
}
par(o)




dput(SAVE, "tmp/route277.rdat")
