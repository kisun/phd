setwd(file.path(gsub(".phd.+", "", getwd()),
                "phd", "code", "particle_filter")); getwd()
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

host <- ifelse(system("whoami", TRUE) == "tell029", "localhost", "130.216.50.187")
user <- "homestead"
port <- "54320"
pass <- "secret"
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = host,
                user = user, port = port, password = pass)
con2 = dbConnect(drv, dbname = "historical", host = host,
                 user = user, port = port, password = pass)


hist <- dbGetQuery(con2, "SELECT route_id, count(route_id) as n FROM vehicle_positions WHERE route_id LIKE '%v46.25' group by route_id order by n")
rid <- "27402-20161011151756_v46.25"
#vid <- "3A9A"
vps <- dbGetQuery(
    con2,
    sprintf("SELECT * FROM vehicle_positions WHERE route_id='%s' ORDER BY timestamp", rid))
vps$trip_start_date <- format(as.POSIXct(vps$timestamp, origin = "1970-01-01"), "%Y-%m-%d")

ind <- which(vps$trip_start_date == "2016-10-27")
infoList <- lapply(unique(vps$trip_id[ind]), function(ID) {
    res <- fromJSON(sprintf("http://130.216.50.187:8000/api/shape_schedule/%s", ID), flatten = TRUE)
    shape <- res$shape
    if ("segment_info.id" %in% names(shape)) {
        Shape <- shape$segment_info.shape_points
        dmax <- cumsum(c(0, sapply(Shape, function(x) max(x$dist_traveled))))
        invisible(lapply(1:length(Shape), function(i) {
            Shape[[i]]$dist_traveled <<- Shape[[i]]$dist_traveled + dmax[i]
            Shape[[i]]$leg <<- i
        }))
        shape <- do.call(rbind, Shape)
        rm(Shape)
    } else {
        shape$segment <-
            sapply(shape$dist_traveled, function(x) which(shape$schedule$pivot.shape_dist_traveled >= x)[1])
    }
    res$shape <- shape
    res
})
names(infoList) <- unique(vps$trip_id[ind])

source("src/pf.R"); source('src/figures.R'); system("make pf.so")
N <- 500
shape <- infoList[[1]]$shape
schedule <- infoList[[1]]$schedule
M <- max(shape$leg)
L <- nrow(schedule) ## number of STOPS
## kf.t <- vps[ind[1], "timestamp"]
## ds <- schedule$pivot.shape_dist_traveled # stop distances
## dr <- c(0, tapply(shape$dist_traveled, shape$leg, max))
## speeds <- dbGetQuery(con, "SELECT * FROM segment_speeds WHERE current")
## B0 <- matrix(speeds$speed_mean, ncol = 1)
## P0 <- diag(speeds$speed_var)
## A <- diag(nrow(speeds))
## H <- diag(nrow(speeds))
## delta <- 5 * 60
## speed <- list(B = B0, P = P0, N = N, M = M, A = A, H = H, t = kf.t, delta = delta)
## ## PRED <- function(m, t) sprintf("predictions/method_%d/%d.csv", m, t)
## BHist <- list(mean = speed$B, var = cbind(diag(speed$P)), t = speed$t)
speed <- list(t = vps[ind[1], "timestamp"], delta = 5 * 60)
MAX.speed <- 80 * 1000 / 60^2
MIN.speed <- 10 * 1000 / 60^2


############################################################################
fileP <- function(x) file.path("../../presentation/nzsa/figure", x)
### FIGURE ONE
library(iNZightMaps)

pdf(fileP("vehicle-state-1.pdf"), width = 1.5, height = 2.5, bg = "transparent", pointsize = 6)
mobj <- iNZightMap(~lat, ~lon, data = infoList[[1]]$shape)
plot(mobj, pch = NA, join = TRUE, main = "", col.line = "purple")
pt <- rbind(rev(h(2000, infoList[[1]]$shape)))
addPoints(pt[2], pt[1], pch = 8, gpar = list(col = "#990000", cex = 0.6))
dev.off()

pdf(fileP("vehicle-state-2.pdf"), width = 3, height = 1.5, bg = "transparent", pointsize = 6)
plot(range(infoList[[1]]$shape$dist_traveled), rep(0, 2), type = "l",
     yaxt = "n", bty = "n", xlab = "Distance into trip (m)", ylab = "")
points(2000, 0, pch = 8, col = "#990000")
dev.off()



### FIGURE TWO
dbGetQuery(con, sprintf("DELETE FROM particles WHERE timestamp >= %s", vps[ind[1], "timestamp"]))
#dbGetQuery(con, "UPDATE particles SET active = FALSE")
set.seed(2005) ############################################### SEEEEED: 1010, 1015
## run a few iterations to get going ...
for (k in 1:8) {
    pf(con, vps[ind[k], "vehicle_id"], 10, sig.gps = 5,
       vp = vps[ind[k], ],  info = infoList[[vps[ind[k], "trip_id"]]],
       SPEED.range = c(MIN.speed, MAX.speed), draw = TRUE,
       rho = 0.5)
}
## a: particles with speed
p1 <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE timestamp=%s", vps[ind[8], "timestamp"]))
Sd <- infoList[[1]]$schedule$pivot.shape_dist_traveled
p2prop <- pf(con, vps[ind[9], "vehicle_id"], 10, sig.gps = 5,
             vp = vps[ind[9], ],  info = infoList[[vps[ind[9], "trip_id"]]],
             SPEED.range = c(MIN.speed, MAX.speed), draw = TRUE,
             rho = 0.5, keep.proposal = TRUE)
##p2 <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE timestamp=%s", vps[ind[9], "timestamp"]))
p2 <- p1
p2$timestamp <- vps[ind[9], "timestamp"]
p2$velocity <- p2prop$velocity
p2$distance_into_trip <- p1$distance_into_trip + (p2$timestamp - p1$timestamp) * p2$velocity
p2$reach <- p2$distance_into_trip >= Sd[p2$stop_index+2]
p2$arrival_time <- ifelse(p2$reach,
                          p1$timestamp + (Sd[p2$stop_index+2] - p1$distance_into_trip) / p2$velocity,
                          p2$arrival_time)
p2$dwell <- ifelse(p2$reach,
                   rbinom(nrow(p2), 1, 0.5) * (6 + rexp(nrow(p2), 1/5)), NA)
p2$departure_time <- ifelse(!p2$reach, p2$departure_time,
                     ifelse(p2$arrival_time + p2$dwell > p2$timestamp, NA,
                            p2$arrival_time + p2$dwell))
p2$distance_into_trip <- ifelse(!p2$reach,
                                p2$distance_into_trip,
                                ifelse(is.na(p2$departure_time), Sd[p2$stop_index+2],
                                       Sd[p2$stop_index+2] + (p2$timestamp - p2$departure_time) * p2$velocity))
p2$stop_index <- ifelse(p2$reach, p2$stop_index + 1, p2$stop_index)


YLIM <- c(min(p1$distance_into_trip) - 10,
          max(p2$distance_into_trip) + 100)
xaxis <- function(t0) {
    lbls <- pretty(par()$usr[1:2] - t0) + t0
    axis(1, at = lbls, labels = lbls - t0)
}

pdf(fileP("predict-state-1.pdf"), width = 4, height = 3, bg = "transparent", pointsize = 6)
with(p1, {
    plot(timestamp, distance_into_trip,
         xlim = vps[ind[8:9], "timestamp"],
         ylim = YLIM,
         pch = 19, cex = 0.6, col = "#990000",
         xaxt = "n", ylab = "Distance into trip (m)",
         xlab = "Trip Time (sec)")
    xaxis(vps[ind[1], "timestamp"])
    abline(h = Sd, lty = 2, col = "#333333")
    arrows(timestamp, distance_into_trip, timestamp + 4,
           distance_into_trip + 4 * velocity, code = 2, length = 0.05)
})
dev.off()

## b: randomise speeds
pdf(fileP("predict-state-2.pdf"), width = 4, height = 3, bg = "transparent", pointsize = 6)
with(p1, {
    plot(timestamp, distance_into_trip,
         xlim = vps[ind[8:9], "timestamp"],
         ylim = YLIM,
         pch = 19, cex = 0.6, col = "#990000",
         xaxt = "n", ylab = "Distance into trip (m)",
         xlab = "Trip Time (sec)")
        xaxis(vps[ind[1], "timestamp"])
    abline(h = Sd, lty = 2, col = "#333333")
})
with(p1, {
    arrows(timestamp, distance_into_trip, timestamp + 4,
           distance_into_trip + 4 * p2$velocity, code = 2, length = 0.05, col = "#00000070")    
})
dev.off()

## c: predicted position
pdf(fileP("predict-state-3.pdf"), width = 4, height = 3, bg = "transparent", pointsize = 6)
with(p1, {
    plot(timestamp, distance_into_trip,
         xlim = vps[ind[8:9], "timestamp"],
         ylim = YLIM,
         pch = 19, cex = 0.6, col = "666666",
         xaxt = "n", ylab = "Distance into trip (m)",
         xlab = "Trip Time (sec)")
    xaxis(vps[ind[1], "timestamp"])
    abline(h = Sd, lty = 2, col = "#333333")
})
with(p2, {
    points(timestamp, distance_into_trip, pch = 19, cex = 0.6)
    before <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index == stop_index
    after <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index < stop_index
    at <- !is.na(arrival_time) & is.na(departure_time)
    arrows(p1$timestamp,
           p1$distance_into_trip,
           ifelse(before, timestamp, arrival_time),
           ifelse(before, distance_into_trip, Sd[stop_index+1]),
           code = 0, length = 0.05, col = "#00000070")
    arrows(ifelse(after, departure_time, NA),
           ifelse(after, Sd[stop_index+1], NA),
           timestamp, distance_into_trip,
           code = 0, length = 0.05, col = "#00000070")
    arrows(arrival_time, Sd[stop_index+1],
           departure_time, Sd[stop_index+1],
           code = 0, length = 0.05, col = "#00000070")
})
dev.off()


### FIGURE THREE

pdf(fileP("update-state-1.pdf"), width = 3, height = 3, bg = "transparent", pointsize = 6)
with(p2, {
    plot(distance_into_trip, rep(0, nrow(p2)),
         xlim = range(shape$dist_traveled), yaxt = "n", bty = "n", xlab = "Distance into trip (m)", ylab = "",
         pch = 19, cex = 0.5)
    lines(range(shape$dist_traveled), rep(0, 2))
    abline(v = Sd[4], lty = 2, col = "#333333")
})
dev.off()

pdf(fileP("update-state-2.pdf"), width = 1.5, height = 3, bg = "transparent", pointsize = 6)
with(infoList[[1]]$shape,
     plot(lon, lat, type = "l", xlab = expression(lambda), ylab = expression(phi),
          asp = 1.4)
     )
points(t(sapply(p2$distance_into_trip, h, shape = infoList[[1]]$shape))[,2:1],
       pch = 19, col = "#333333", cex = 0.5)
with(infoList[[1]]$shape, {
    text(lon[1], lat[1], "Start", pos = 2, cex = 0.6)
    text(lon[length(lon)], lat[length(lat)], "End", pos = 4, cex = 0.6)
})
dev.off()


pdf(fileP("update-state-3.pdf"), width = 2.5, height = 3, bg = "transparent", pointsize = 6)
with(infoList[[1]]$shape[shi <- 220:380,],
     plot(lon, lat, type = "l", xlab = expression(lambda), ylab = expression(phi),
          asp = 1.4)
     )
points(t(sapply(p2$distance_into_trip, h, shape = infoList[[1]]$shape))[,2:1],
       pch = 19, col = "#333333", cex = 0.5)
with(vps[ind[9],], points(position_longitude, position_latitude, col = "#990000", pch = 8, lwd = 2))
dev.off()


## d: centered:
lam1 <- vps[ind[9], "position_longitude"] * pi/180
phi0 <- vps[ind[9], "position_latitude"] * pi/180
pLL <- t(sapply(p2$distance_into_trip, h, shape = infoList[[1]]$shape)) * pi/180
px <- R * (pLL[,2] - lam1) * cos(phi0)
py <- R * (pLL[,1] - phi0)
sLL <- infoList[[1]]$shape[shi, c("lon", "lat")] * pi/180
sx <- R * (sLL[,1] - lam1) * cos(phi0)
sy <- R * (sLL[,2] - phi0)
pdf(fileP("update-state-4.pdf"), width = 2.5, height = 3, bg = "transparent", pointsize = 6)
plot(px, py, xlab = "West-East axis (m)", ylab = "North-South axis (m)", asp = 1, pch = 19,
     xlim = range(sx), ylim = range(sy), cex = 0.5)
lines(sx, sy)
points(0, 0, col = "#990000", pch = 8, lwd = 2)
dev.off()



## e: after culling D:
z <- rbind(px, py)
lh <- exp(- 1 / (2 * 40^2) * diag(t(z) %*% z))
wt <- lh / sum(lh)

wi <- sample(nrow(p2), replace = TRUE, prob = wt)
pdf(fileP("update-state-5.pdf"), width = 2.5, height = 3, bg = "transparent", pointsize = 6)
plot(px[wi], py[wi], xlab = "West-East axis (m)", ylab = "North-South axis (m)", asp = 1, pch = 19,
     xlim = range(sx), ylim = range(sy), cex = 0.5)
lines(sx, sy)
points(0, 0, col = "#990000", pch = 8, lwd = 2)
dev.off()


## f: back transform
pdf(fileP("update-state-6.pdf"), width = 4, height = 3, bg = "transparent", pointsize = 6)
with(p1, {
    plot(timestamp, distance_into_trip,
         xlim = vps[ind[8:9], "timestamp"],
         ylim = YLIM,
         pch = 19, cex = 0.6, col = "666666",
         xaxt = "n", ylab = "Distance into trip (m)",
         xlab = "Trip Time (sec)")
    xaxis(vps[ind[1], "timestamp"])
    abline(h = Sd, lty = 2, col = "#333333")
})
with(p2, {
    points(timestamp, distance_into_trip, pch = 19, cex = 0.6)
    before <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index == stop_index
    after <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index < stop_index
    at <- !is.na(arrival_time) & is.na(departure_time)
    arrows(p1$timestamp,
           p1$distance_into_trip,
           ifelse(before, timestamp, arrival_time),
           ifelse(before, distance_into_trip, Sd[stop_index+1]),
           code = 0, length = 0.05, col = "#00000070", lty = 3)
    arrows(ifelse(after, departure_time, NA),
           ifelse(after, Sd[stop_index+1], NA),
           timestamp, distance_into_trip,
           code = 0, length = 0.05, col = "#00000070", lty = 3)
    arrows(arrival_time, Sd[stop_index+1],
           departure_time, Sd[stop_index+1],
           code = 0, length = 0.05, col = "#00000070", lty = 3)
})
with(p2[wi,], {
    points(timestamp, distance_into_trip, pch = 19, cex = 0.6, col = "#990000")
    before <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index == stop_index
    after <- !is.na(arrival_time) & !is.na(departure_time) & p1$stop_index < stop_index
    at <- !is.na(arrival_time) & is.na(departure_time)
    arrows(p1$timestamp,
           p1$distance_into_trip,
           ifelse(before, timestamp, arrival_time),
           ifelse(before, distance_into_trip, Sd[stop_index+1]),
           code = 0, length = 0.05, col = "#99000070")
    arrows(ifelse(after, departure_time, NA),
           ifelse(after, Sd[stop_index+1], NA),
           timestamp, distance_into_trip,
           code = 0, length = 0.05, col = "#99000070")
    arrows(arrival_time, Sd[stop_index+1],
           departure_time, Sd[stop_index+1],
           code = 0, length = 0.05, col = "#99000070")
})
dev.off()




### FIGURE FOUR

library(iNZightMaps)
mobj <- iNZightMap(~lat, ~lon, data = infoList[[1]]$shape)

pdf(fileP("road-state-1.pdf"), width = 1.5, height = 2.5, bg = "transparent", pointsize = 6)
plot(mobj, join = TRUE, pch = NA, lwd = 2, col.line = "black", main = "")
dev.off()

pdf(fileP("road-state-2.pdf"), width = 1.5, height = 2.5, bg = "transparent", pointsize = 6)
plot(mobj, pch = NA, main = "")
with(mobj,
     addLines(.latitude, .longitude, id = leg,
              gpar = list(
                  lwd = 2,
                  col = ifelse(1:max(leg) %% 2 == 1, "black", "purple")
              ))
     )
dev.off()



