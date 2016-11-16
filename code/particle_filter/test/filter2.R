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
savePred <- function(pred, m, t)
    write.table(pred, file = sprintf("predictions/method_%s/%d.csv", m, t),
                quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
sapply(c("schedule", "schedule_deviation", "vehicle_state", "road_state"),
       function(x) system(sprintf("mkdir -p predictions/method_%s", x)))
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
    if (res <= 0) {
        dat <- dbGetQuery(con,
                          sprintf("SELECT distance_into_trip, velocity, stop_index, arrival_time, departure_time, segment_index FROM particles WHERE vehicle_id = '%s' AND active",
                                  vps[ind[k], "vehicle_id"]))
        Speed <- dbGetQuery(con,
                            sprintf("SELECT speed_mean, speed_var FROM segment_speeds WHERE current AND segment_id IN ('%s')", paste(with(infoList[[vps[ind[k], "trip_id"]]]$shape, tapply(segment_id, leg, min)), collapse = "','")))
        sk <- dat$stop_index + 1 ## switching to R indexing
        rk <- dat$segment_index + 1
        St <- as.numeric(as.POSIXct(paste(vps$trip_start_date[1],
                                          infoList[[vps[ind[k], "trip_id"]]]$schedule$pivot.arrival_time)))
        Sd <- infoList[[vps[ind[k], "trip_id"]]]$schedule$pivot.shape_dist_traveled
        Rd <- with(infoList[[vps[ind[k], "trip_id"]]]$shape,
                   tapply(dist_traveled, leg, max))
        tstart <- min(St)
        St <- St - tstart
        tk <- vps[ind[k], "timestamp"]
        ## 1. Schedule prediction:
        p1 <- matrix(St[-(1:min(sk))], nrow = 1)
        colnames(p1) <- min(sk) + 1:ncol(p1)
        savePred(p1, "schedule", tk)
        ## 2. Schedule-deviation prediction:
        delays <- ifelse(is.na(dat$departure_time), dat$arrival_time, dat$departure_time) - tstart - St[sk]
        p2 <- sapply((min(sk)+1):length(St), function(j) ifelse(j > sk, St[j] + delays, NA))
        colnames(p2) <- min(sk) + 1:ncol(p2)
        savePred(p2, "schedule_deviation", tk)
        ## 3. Vehicle-state prediction:
        p3 <- (tk - tstart) +
            sapply((min(sk)+1):length(St), function(j)
                ifelse(j > sk, (Sd[j] - dat$distance_into_trip) / dat$velocity, NA))
        dwell <- matrix(rbinom(prod(dim(p3)), 1, 0.5) * (6 + rexp(prod(dim(p3)), 1/5)),
                        nrow = nrow(p3), ncol = ncol(p3))
        if (ncol(p3) > 1)
            p3 <- p3 + cbind(0, t(apply(dwell, 1, cumsum))[, -ncol(dwell)])
        colnames(p3) <- min(sk) + 1:ncol(p3)
        savePred(p3, "vehicle_state", tk)
        ## 4. Road-state prediction:
        skj <- (min(sk)+1):length(St)
        rkj <- (min(rk)+1):length(Rd)
        p4 <- t(sapply(1:length(sk), function(i) {
            spds <- msm::rtnorm(nrow(Speed), Speed[, 1], Speed[, 2], MIN.speed, MAX.speed)
            if (sk[i] == length(Sd)) return(rep(NA, length(skj)))
            sapply(skj, function(j) {
                if (j <= sk[i]) return(NA) ## stop behind bus
                wseg <- Rd > Sd[sk[i]] & Rd < Sd[j]
                ## next stop in same segment
                if (sum(wseg) == 0) return((Sd[j] - dat$distance_into_trip[i]) / dat$velocity[i])
                ## pass through intersections
                tt1 <- (Rd[rk[i]] - dat$distance_into_trip[i]) / dat$velocity[i]
                tt2 <- 0 ## intermediate segments
                tt3 <- 0
                if (Sd[j] < max(Rd))
                    tt3 <- (Sd[j] - max(Rd[Rd < Sd[j]])) / spds[which.max(Rd[Rd < Sd[j]]) + 1]
                wseg <- Rd > Sd[sk[i]] & Rd < max(Rd[Rd <= Sd[j]])
                if (sum(wseg) > 0) ## sum travel time along intermediate segments
                    tt2 <- tt2 + sum(diff(c(0, Rd)) / spds * wseg)
                return(tt1 + tt2 + tt3)
            })
        }))
        if (nrow(p4) == 1) p4 <- t(p4)
        p4 <- (tk - tstart) + p4
        if (ncol(p4) > 1)
            p4 <- p4 + cbind(0, t(apply(dwell, 1, cumsum))[, -ncol(dwell)])
        colnames(p4) <- min(sk) + 1:ncol(p4)
        savePred(p4, "road_state", tk)
    }
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


arrivaltimes <- dbGetQuery(con2, sprintf("select distinct trip_id, stop_sequence, stop_id, arrival_delay, departure_delay from trip_updates as tu, stop_time_updates as stu where tu.oid=stu.trip_update_id and timestamp between %s and %s order by trip_id, stop_sequence", min(vps[ind, "timestamp"]), max(vps[ind, "timestamp"])))

o <- with(vps[ind, ], tapply(trip_start_time, trip_id, unique))
ORD <- order(factor(vps$trip_id[ind], levels = names(o)[order(o)]), vps$timestamp[ind])

install.packages("animation")

animation::saveHTML({
    pb <- txtProgressBar(0, length(ind), style = 3)
    dev.flush(dev.flush())
    for (k in ORD) {
        dev.hold()
        tk <- vps[ind[k], "timestamp"]
        plot(NA, xlim = c(0, 90 * 60), ylim = c(1, M+1),
             xlab = "Arrival Time (min after trip start)", xaxs = "i", xaxt = "n",
             ylab = "Stop #", yaxs = "i", yaxt = "n",
             main = sprintf("2am trip"))
        abline(h = 2:M, lty = 3, col = "#cccccc")
        axis(2, at = 1:M + 0.5, labels = 1:M + 1, las = 1, tick = FALSE)
        axis(1, at = pretty(c(0, 90)) * 60, labels = pretty(c(0, 90)))
        Ta <- arrivaltimes[arrivaltimes$trip_id == vps[ind[k], "trip_id"], ]
        St <- infoList[[vps[ind[k], "trip_id"]]]$schedule[, c("pivot.arrival_time", "pivot.departure_time")]
        St[, 1] <- as.numeric(as.POSIXct(paste(vps[ind[k], "trip_start_date"], St[, 1]), origin = "1970-01-01"))
        St[, 2] <- as.numeric(as.POSIXct(paste(vps[ind[k], "trip_start_date"], St[, 2]), origin = "1970-01-01"))
        St <- as.matrix(St)
        abline(v = tk - St[1,1], lty = 3, col = "#33cccc")
        if (nrow(Ta) > 0) {
            sapply(unique(Ta$stop_sequence), function(s) {
                arr <- Ta[Ta$stop_sequence == s & Ta$arrival_delay != 0, "arrival_delay"]
                dep <- Ta[Ta$stop_sequence == s & Ta$departure_delay != 0, "departure_delay"]
                if (length(arr) == 0 & length(dep) == 0) (c(NA, NA))
                if (length(arr) == 0) return(rep(max(dep), 2))
                if (length(dep) == 0) return(rep(max(arr), 2))
                return(c(max(arr), max(dep)))
            }) -> delays
            dimnames(delays) <- list(c("arrival", "departure"), unique(Ta$stop_sequence))

            
            for (s in as.character(unique(Ta$stop_sequence))) {
                delays[, s] <- St[s, ] + delays[, s] - min(St)
                ##lines(delays[, s], rep(as.numeric(s), 2) - 0.9, col = "orangered", lwd = 2)
                rect(delays[1, s], as.numeric(s) - 1, delays[2, s], as.numeric(s), col = "#cccccc",
                     border = "#999999")
            }
        }
        for (mth in c("schedule", "schedule_deviation", "vehicle_state", "road_state")) {
            file <- sprintf("predictions/method_%s/%d.csv", mth, tk)
            if (file.exists(file)) {
                pred <- read.csv(file, header = TRUE)
                for (Sj in 1:ncol(pred)) {
                    points(pred[, Sj],
                           rep(as.numeric(gsub("X", "", colnames(pred)[Sj])), nrow(pred)) +
                           switch(mth, "schedule" = -0.8, "schedule_deviation" = -0.6,
                                  "vehicle_state" = -0.4, "road_state" = -0.2),
                           pch = 19, cex = 0.4,
                           col = switch(mth, "schedule" = "black", "schedule_deviation" = "orangered",
                                        "vehicle_state" = "#009900", "road_state" = "#000099"))
                }
            } else {
                next
            }
        }
        legend("bottomright", legend = rev(c("Schedule", "Schedule Deviation", "Vehicle State", "Road State")),
               col = rev(c("black", "orangered", "#009900", "#000099")), pch = 19, cex = 0.8, bty = "n")
        dev.flush()
        setTxtProgressBar(pb, which(ORD == k))
        #locator(1)
    }
    close(pb)
}, "arrival_time_predictions", ani.width = 900, ani.height = 600)
