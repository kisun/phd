## A simulation

setwd("../")
.libPaths("../../.Rlibrary")

library(RPostgreSQL)
library(jsonlite)
library(iNZightPlots)
library(iNZightMaps)
library(mvtnorm)
source("src/pf.R")
source("src/mapping.R")
source("src/h.R")
source("src/figures.R")

## db connection/s
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")

rid <- "27402-20160920093629_v46.5"
tid <- dbGetQuery(con, sprintf("SELECT id FROM trips WHERE route_id = '%s' LIMIT 1", rid))$id

INFO <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", tid), flatten = TRUE)
shape <- INFO$shape
schedule <- INFO$schedule
shape$segment <- sapply(shape$dist_traveled, function(x) which(schedule$pivot.shape_dist_traveled >= x)[1])

Times <- seq(0, 2 * 60, by = 5)

## functions:
drawSegments <- function(shape, schedule, speeds, times) {
    var <- NULL
    if (class(speeds) == "list") {
        speeds <- BHist$mean
        times <- BHist$t / 60
        var <- BHist$var
    } else {
        speeds <- cbind(speeds)
        if (missing(times)) times <- seq(0, nrow(speeds), by = 1)
    }
    Sd <- schedule$pivot.shape_dist_traveled
    o <- par(mfrow = c(1, 1), bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", col.main = "#cccccc")
    plot(NA, type = "n", xlab = "Time (minutes)", ylab = "Segment", 
         xlim = range(times), xaxs = "i",
         ylim = c(max(Sd), 0), yaxt = "n", yaxs = "i")
    axis(2, at = Sd[-1] - diff(Sd) / 2, labels = 1:(length(Sd) - 1), las = 1, tick = FALSE, cex.axis = 0.8)
    spd <- round(speeds / MAX.speed * 10) + 1
    cols <- apply(spd, 1, function(x) RColorBrewer::brewer.pal(11, "RdYlGn")[x])
    cols <- if (is.null(dim(cols))) cbind(cols) else t(cols)
    for (i in 1:nrow(speeds)) {
        rect(times[-length(times)], rep(Sd[i], length(times) - 1),
             times[-1], rep(Sd[i + 1], length(times) - 1),
             border = cols[i, ], col = cols[i, ])
        if (!is.null(var)) {
            polygon(c(times, rev(times)),
                    c(Sd[i] + (Sd[i + 1] - Sd[i]) * pmin((speeds[i, ] + sqrt(var[i, ])) / MAX.speed, 1),
                      rev(Sd[i] + (Sd[i + 1] - Sd[i]) * pmax(0, (speeds[i, ] - sqrt(var[i, ])) / MAX.speed))),
                    border = NULL, col = "#33333320")
            lines(times, Sd[i] + (Sd[i + 1] - Sd[i]) * speeds[i, ] / MAX.speed, lwd = 1, col = "#333333")
        }
    }
    abline(h = Sd[2:(length(Sd) - 1)], col = "#33333330")
    par(o)
}

### SIMULATION
## generate fake data
MAX.speed <- 50 * (1000 / 60^2)
MIN.speed <- 10 * (1000 / 60^2)

## each segment's speed changes slowly over time...
set.seed(25101990)
Speed <- matrix(NA, nrow = nrow(schedule) - 1, ncol = length(Times))
Speed[, 1] <- msm::rtnorm(nrow(Speed), 10, 2, lower = MIN.speed, upper = MAX.speed)
drawSegments(shape, schedule, Speed[,  1])
for (i in 2:ncol(Speed)) {
    Speed[, i] <- msm::rtnorm(nrow(Speed),
                              ifelse(rbinom(nrow(Speed), 1, 0.09) == 1,
                                     MAX.speed - Speed[, i - 1],
                                     Speed[, i - 1]),
                              0.05, lower = MIN.speed, upper = MAX.speed)
}
drawSegments(shape, schedule, Speed, c(Times, max(Times) + 5))
## simulated trips: every 10 minutes
start <- seq(0, 90, by = 10) * 60
delta <- 30
gamma <- 10
pi <- 0.7
tau <- 6
Sd <- schedule$pivot.shape_dist_traveled
Hist <- list()
for (i in 1:length(start)) {
    pstop <- rbinom(nrow(schedule) - 1, 1, pi)
    dwell <- pstop * (gamma + rexp(nrow(schedule) - 1, 1 / tau))
    Ta <- start[i]
    Td <- Ta + dwell[1]
    t <- Td
    tt <- which(Times * 60 > t)[1] - 1
    for (j in 2:nrow(schedule)) {
        seg.spd <- msm::rtnorm(1, Speed[j - 1, tt], 0, lower = MIN.speed, upper = MAX.speed)
        t <- t + (Sd[j] - Sd[j - 1]) / seg.spd
        Ta <- c(Ta, t)
        t <- t + dwell[j]
        Td <- c(Td, t)
    }
    lines(c(rbind(Td[-length(Td)], Ta[-1])) / 60, c(0, rep(Sd[2:(length(Sd) - 1)], each = 2), max(Sd)),
          col = "#222222")
    Hist <- c(Hist, list(rbind(Ta, Td)))
}
abline(v = seq(0, max(Times), by = 30 / 60), col = "#ffffff20")
## observations every 30 seconds:
ts <- seq(0, max(Times) * 60, by = 30)
hist.db <- lapply(Hist, function(trip) {
    tk <- ts[ts >= min(trip, na.rm = TRUE) & ts <= max(trip, na.rm = TRUE)]
    ## which segment are we in?
    Sk <- sapply(tk, function(x) which(trip[1, ] > x)[1] - 1)
    ## and have we left yet?
    dep <- ifelse(Sk == ncol(trip), FALSE, trip[2, Sk] < tk)
    x <- Sd[Sk] + ifelse(dep, (Sd[Sk + 1] - Sd[Sk]) * (tk - trip[2, Sk]) / (trip[1, Sk + 1] - trip[2, Sk]), 0)
    y <- h(x, shape) ## "true" y
    ## add ~5m GPS error
    Y <- rbind(rnorm(length(x), 0, 5 / R) + y[1, ] * pi / 180,
               y[2, ] * pi / 180 + rnorm(length(x), 0, 5 / R) / cos(y[1, ] * pi / 180)) * 180 / pi
    data.frame(position_latitude= Y[1, ], position_longitude = Y[2, ], timestamp = tk, trueX = x,
               trueV = (Sd[Sk + 1] - Sd[Sk]) / (trip[1, Sk + 1] - trip[2, Sk]))
})
for (i in 1:length(hist.db)) {
    hist.db[[i]]$vehicle_id <- paste(sample(LETTERS, 4, TRUE), collapse = "")
    hist.db[[i]]$trip_id <- sprintf("t%02d", i)
}
hist.db <- do.call(rbind, hist.db)
hist.db <- hist.db[order(hist.db$timestamp), ]
hist.db$trip_id <- tid
hist.db$route_id <- rid
with(hist.db, points(timestamp / 60, trueX, pch = 19, col = "#333333", cex = 0.4))


### And now to see if we get the same results back ......
vps <- hist.db
ind <- 1:nrow(vps)
kf.t <- vps[ind[1], "timestamp"]
N <- 500
shape <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", vps[ind[1], "trip_id"]), flatten = TRUE)
SHAPE <- shape$shape
SHAPE$segment <- sapply(SHAPE$dist_traveled, function(x) which(shape$schedule$pivot.shape_dist_traveled >= x)[1])
M <- nrow(shape$schedule)
ds <- shape$schedule$pivot.shape_dist_traveled
B0 <- matrix(rep(10, M), ncol = 1)
P0 <- 10 * diag(M)
A <- diag(M)
H <- diag(M)
delta <- 5 * 60
speed <- list(B = B0, P = P0, N = N, M = M, A = A, H = H, t = kf.t, delta = delta)
i <- 1
BHist <- list(mean = speed$B, var = cbind(diag(speed$P)), t = speed$t)
infoList <- lapply(unique(vps$trip_id), function(ID) {
    fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", ID), flatten = TRUE)
})
names(infoList) <- unique(vps$trip_id)

pb <- txtProgressBar(0, length(ind), style = 3)
Rprof("pf_profile.out")
for (i in i:length(ind)) {
    setTxtProgressBar(pb, i)
    ## update the speed KF:
#    i <- i + 1
    ## if (vps[ind[i], "timestamp"] > speed$t + speed$delta) {
    ##     #jpeg(sprintf("~/Desktop/figs/speeds_%s.jpg", speed$t), width = 500, height = 1000)
    ##     speed <- update(speed, q = 2)
    ##     if (any(diag(speed$P) < 0.000001)) diag(speed$P) <- pmax(0.000001, diag(speed$P))
    ##     BHist$mean <- cbind(BHist$mean, speed$B)
    ##     BHist$var <- cbind(BHist$var, diag(speed$P))
    ##     BHist$t <- c(BHist$t, speed$t)
    ##     #plotSpeeds(speed, shape = SHAPE)
    ##     #dev.off()
    ## }
    pf(con, vps[ind[i], "vehicle_id"], 500, sig.gps = 5, vp = vps[ind[i], ], speed = speed,
       gamma = gamma, pi = pi, tau = tau, rho = 0, info = infoList[[vps[ind[i], "trip_id"]]])
    ## vel <- dbGetQuery(con, sprintf("SELECT velocity, segment FROM particles WHERE active AND timestamp > %s",
    ##                                speed$t - delta))
    ## useg <- 1:23
    ## nseg <- length(useg)
    ## N <- round(sqrt(nseg))
    ## M <- ceiling(sqrt(nseg))
    ## dev.hold()
    ## par(mfrow = c(N, M))
    ## for (j in seq_along(useg)) {
    ##     hist(vel$velocity[vel$segment == useg[j]], breaks = seq(0, 16, by = 0.5), freq = FALSE,
    ##          main = paste0("Segment ", useg[j]), xlab = "Velocity (m/s)", col = "lightblue",
    ##          ylim = c(0, 2.5))
    ##     curve(dnorm(x, speed$B[useg[j]], sqrt(diag(speed$P)[useg[j]])), 0, 16, 1001, add = TRUE,
    ##           lty = 2, col = "#990000", lwd = 2)
    ## }
    ## dev.flush()
}; close(pb); Rprof(NULL)

summaryRprof("pf_profile.out")

dev.new()
shape <- INFO$shape
schedule <- INFO$schedule
shape$segment <- sapply(shape$dist_traveled, function(x) which(schedule$pivot.shape_dist_traveled >= x)[1])
drawSegments(shape, schedule, BHist)



plotTrip <- function(vid, trip, true, dwell = FALSE, ...) {
    speed <- !dwell
    dev.hold()
    res <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE vehicle_id='%s' ORDER BY id", vid))
    shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trip), flatten = TRUE)
    o <- par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc")
    layout(matrix(c(1,1,1,2), nrow = 1))
    sh <- shape$schedule$pivot.shape_dist_traveled
    par(mar = c(5.1, 4.1, 4.1, 0))
    plot(res$timestamp, res$distance_into_trip, pch = 19, cex = 0.1, xaxt = "n", yaxt = "n", yaxs = "i",
         xlab = "Time", ylab = "Distance into Trip (km)", col = "#cccccc", bty = "n", ylim = c(0, max(sh)*1.04), ...)
    abline(h =  sh, col = "#393939")
    res$parentid <- sapply(res$parent ,function (x) {
        ret <- which(res$id == x)
        if (length(ret) == 1) return(ret) else return(NA)
    })
    #res$parentid <- res$parent - min(res$id) + 1
    #res$parentid[res$parentid < 1] <- NA
    resl <- res[!is.na(res$parent), ]
    arrows(x0 = resl$timestamp,
           x1 = ifelse(resl$segment == res$segment[resl$parentid] & !is.na(res$departure_time[resl$parentid]),
                       res$timestamp[resl$parentid],
                       resl$departure_time),
           y0 = resl$distance_into_trip,
           y1 = ifelse(resl$segment == res$segment[resl$parentid] & !is.na(res$departure_time[resl$parentid]),
                       res$distance_into_trip[resl$parentid],
                       shape$schedule$pivot.shape_dist_traveled[resl$segment]),
           code = 0, col = "#cc666640")
    arrows(x0 = ifelse(resl$departure_time <= resl$timestamp,
                       resl$departure_time, NA),
           x1 = resl$arrival_time,
           y0 = shape$schedule$pivot.shape_dist_traveled[resl$segment], code = 0, col = "#cc666640")
    arrows(x0 = ifelse(resl$segment != res$segment[resl$parentid],
                       resl$arrival_time, NA),
           x1 = res$timestamp[resl$parentid],
           y0 = shape$schedule$pivot.shape_dist_traveled[resl$segment],
           y1 = res$distance_into_trip[resl$parentid], code = 0, col = "#cc666640")
    ts <- as.POSIXct(res$timestamp, origin = "1970-01-01")
    axis(1, pretty(ts), labels = format(pretty(ts), "%H:%M"), lwd = 0)
    axis(2, at = pretty(shape$schedule$pivot.shape_dist_traveled/1000*1000, high.u.bias = 1),
         labels = pretty(shape$schedule$pivot.shape_dist_traveled/1000, high.u.bias = 1),
         lwd = 0, las = 2, cex.axis = 0.8)
    if (!missing(true)) {
        points(true$timestamp, true$trueX, col = "white", pch = 19)
    }
    if (dwell) {
        res2 <- res[res$id %in% res$parent, ]
        res2$dwell <- res2$departure_time - res2$arrival_time
        mu.dwell <- with(res2[res2$dwell > 0, ], tapply(dwell, segment, mean))
        pi.stop <- with(res2[!is.na(res2$dwell), ], tapply(dwell, segment, function(x) mean(x > 0)))
        if (length(mu.dwell) > 0) {
            par(mar = c(5.1, 0, 4.1, 2.1))
            plot(mu.dwell, sh[as.numeric(names(mu.dwell))], type = "n", bty = "n", yaxt = "n", yaxs = "i",
                 ylab = "", xlab = "Dwell time (s)", xaxt = "n", xaxs = "i",
                 xlim = c(0, max(mu.dwell)*1.04), ylim = c(0, max(sh)*1.04))
            abline(h = sh, col = "#393939")
            abline(v = 0, col = "#888888")
            points(mu.dwell, sh[as.numeric(names(mu.dwell))], pch = 19, cex = 2 * sqrt(pi.stop))
        }
    } else {
        par(mar = c(5.1, 0, 4.1, 2.1))
        plot(res$velocity, res$distance_into_trip, col = "#cc666670", type = "n", bty = "n", yaxt = "n", xaxt = "n",
             xlab = "Velocity (m/s)", ylab = "", xlim = c(0, 16), ylim = c(0, max(sh)*1.04), yaxs = "i", xaxs = "i")
        abline(h = sh, col = "#393939")
        abline(v = 0, col = "#555555")
        points(res$velocity, res$distance_into_trip, col = "#cc666670", pch = 19)
        axis(4, at = pretty(shape$schedule$pivot.shape_dist_traveled/1000*1000, high.u.bias = 1),
             labels = pretty(shape$schedule$pivot.shape_dist_traveled/1000, high.u.bias = 1),
             lwd = 0, las = 2, cex.axis = 0.8)
        axis(1, lwd = 0)
        if (!missing(true)) {
            points(true$trueV, true$trueX, col = "white", pch = 19)
        }
    }
    dev.flush()
    par(o)
}
vs <- dbGetQuery(con, "SELECT vehicle_id, min(timestamp) as start FROM particles GROUP BY vehicle_id ORDER BY start")$vehicle_id

for (vi in vs) {
    plotTrip(vi, tid, true = hist.db[hist.db$vehicle_id == vi, ])
    locator(1)
}


## testing
delta <- 30
gamma <- 6
pi <- 0.5
tau <- 5
rho <- 0.1
upsilon <- 20
p <- data.frame(distance_into_trip = 0, velocity = 10, segment = 0, arrival_time = 0, departure_time = 0, timestamp = 30)
p <- rbind(p, p, p)
speed <- c(10, 5, 9, 10)
speed.var <- c(2, 0.4, 1, 2)
Sd <- c(0, 1000, 1700, 2000, 2500)


transitionC(p)

(transitionC(p))

(p <- transitionC(p)); p$timestamp <- p$timestamp + delta



#  MAKEFLAGS="PKG_CPPFLAGS=-Iinclude"  R CMD SHLIB -o bin/pf.so src/truncated_normal.c src/pf.c
