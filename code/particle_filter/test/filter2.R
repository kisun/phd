setwd(file.path(gsub(".code.particle_filter.+", "", getwd()),
                "code", "particle_filter")); getwd()
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

ind <- which(vps$trip_start_date == "2016-10-26")
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
MAX.speed <- 60 * 1000 / 60^2
MIN.speed <- 10 * 1000 / 60^2

## initialise speed on ALL segments ...
if (FALSE){
    segs <- dbGetQuery(con, "SELECT id FROM segment_infos")$id
    dbGetQuery(con, "DELETE FROM segment_speeds")
    invisible(sapply(segs, function(s) {
        dbGetQuery(con, sprintf("INSERT INTO segment_speeds (segment_id, speed_mean, speed_var, timestamp, current) VALUES ('%d', 10, 10, %d, TRUE)",
                                s, vps[ind[1], "timestamp"]))
    }))
}


## DELETE PARTICLES!!!!!
del <- dbGetQuery(con, "DELETE FROM particles")
k <- 0
pb <- txtProgressBar(0, length(ind), style = 3)
##pdf("figures_1/trial1.pdf", width = 6, height = 10)
for (k in (k+1):length(ind)) {
    setTxtProgressBar(pb, k)
    ## update the speed KF:
    if (vps[ind[k], "timestamp"] > speed$t + speed$delta) {
        updateSpeeds(con, q = 1, t2 = speed$t + speed$delta)
        speed$t <- vps[ind[k], "timestamp"]
    }
    jpeg(sprintf("figures_2/r274_2016-10-26_v%s_t%d.jpg",
                 vps[ind[k], "vehicle_id"], vps[ind[k], "timestamp"]),
         width = 600, height = 1000)
    res <- pf(con, vps[ind[k], "vehicle_id"], 500, sig.gps = 5,
              vp = vps[ind[k], ],  info = infoList[[vps[ind[k], "trip_id"]]],
              SPEED.range = c(MIN.speed, MAX.speed), draw = TRUE,
              rho = 0.5)
    dev.off()
    ## if (res <= 0) {
    ##     dat <- dbGetQuery(con,
    ##                       sprintf("SELECT distance_into_trip, velocity, arrival_time, departure_time, segment FROM particles WHERE vehicle_id = '%s' AND active",
    ##                               vps[ind[k], "vehicle_id"]))
    ##     sk <- dat$segment
    ##     St <- as.numeric(as.POSIXct(paste(vps$trip_start_date[1],
    ##                                       infoList[[vps[ind[k], "trip_id"]]]$schedule$pivot.arrival_time)))
    ##     tstart <- min(St)
    ##     St <- St - tstart
    ##     invisible(sapply(1:length(sk), function(i) {
    ##         if (sk[i] < M) {
    ##             PRED[-(1:sk[i]), i, k, 1] <<- St[-(1:sk[i])]
    ##             PRED[-(1:sk[i]), i,  k, 2] <<-
    ##                 St[-(1:sk[i])] + (ifelse(is.na(dat$departure_time[i]),
    ##                                          dat$arrival_time[i], dat$departure_time[i]) - St[sk[i]]) - tstart
    ##             PRED[-(1:sk[i]), i, k, 3] <<-
    ##                 vps[ind[k], "timestamp"] + (ds[-(1:sk[i])] - dat$distance_into_trip[i]) / dat$velocity[i] +
    ##                 cumsum(rbinom(M - sk[i], 1, 0.5) * (6 + rexp(M - sk[i], 1 / 5))) - tstart
    ##             PRED[-(1:sk[i]), i, k, 4] <<-
    ##                 vps[ind[k], "timestamp"] +
    ##                 (ds[sk[i] + 1] - dat$distance_into_trip[i]) / msm::rtnorm(1, speed$B[sk[i]],
    ##                                                                           diag(speed$P)[sk[i]],
    ##                                                                           MIN.speed, MAX.speed) +
    ##                 cumsum((ds[(sk[i]+1):M] - ds[sk[i]:(M-1)]) /
    ##                        msm::rtnorm(M - sk[i], speed$B[sk[i]:M],
    ##                                    diag(speed$P)[sk[i]:M], MIN.speed, MAX.speed)) +
    ##                 cumsum(rbinom(M - sk[i], 1, 0.5) * (6 + rexp(M - sk[i], 1 / 5))) - tstart
    ##         }
    ##         NULL
    ##     }))
    ## }
}; close(pb);




sp.hist <- dbGetQuery(con, "SELECT * FROM segment_speeds ORDER BY segment_id")

ns <- length(segs <- unique(sp.hist$segment_id))
nt <- length(ts <- sort(unique(sp.hist$timestamp)))
BHist <- list(mean = matrix(NA, ns, nt, dimnames = list(segs, ts)),
              var  = matrix(NA, ns, nt, dimnames = list(segs, ts)),
              t    = ts)
invisible(apply(sp.hist, 1, function(x) {
    BHist$mean[as.character(x["segment_id"]), as.character(x["timestamp"])] <<- x["speed_mean"]
    BHist$var[as.character(x["segment_id"]), as.character(x["timestamp"])] <<- x["speed_var"]
}))


source("src/figures.R")
drawSegments(shape, schedule, BHist, MAX.speed = MAX.speed)
