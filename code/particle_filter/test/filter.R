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

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")
con2 = dbConnect(drv, dbname = "historical", host = "localhost",
                 user = "homestead", port = "54320", password = "secret")

hist <- dbGetQuery(con2, "SELECT vehicle_id, count(vehicle_id) as n FROM vehicle_positions group by vehicle_id")
vid <- "3A9A"
vps <- dbGetQuery(
    con2,
    sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s' AND trip_id LIKE '%s' ORDER BY timestamp",
            vid, '%v46.5'))

#for (i in 1:nrow(vps))
#    pf(con, vid, 500, sig.gps = 5, draw = TRUE, vp = vps[i, ])

trips <- dbGetQuery(con, "SELECT distinct trip_id FROM particles")$trip_id
par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(4, 3))
plotTrip <- function(trip, dwell = FALSE, ...) {
    dev.hold()
    res <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE trip_id='%s' ORDER BY id", trip))
    shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trip), flatten = TRUE)
    par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc")
    if (dwell) layout(matrix(c(1,1,1,2), nrow = 1))
    sh <- shape$schedule$pivot.shape_dist_traveled
    par(mar = c(5.1, 4.1, 4.1, 0))
    plot(res$timestamp, res$distance_into_trip, pch = 19, cex = 0.1, xaxt = "n", yaxt = "n", yaxs = "i",
         xlab = "Time", ylab = "Distance into Trip (km)", col = "#cccccc", bty = "n", ylim = c(0, max(sh)*1.04), ...)
    abline(h =  sh, col = "#393939")
    res$parentid <- res$parent - min(res$id) + 1
    res$parentid[res$parentid < 1] <- NA
    resl <- res[!is.na(res$parent), ]
    arrows(x0 = resl$timestamp,
           x1 = res$timestamp[resl$parentid],
           y0 = resl$distance_into_trip,
           y1 = res$distance_into_trip[resl$parentid], code = 0, col = "#cc666640")
    ts <- as.POSIXct(res$timestamp, origin = "1970-01-01")
    axis(1, pretty(ts), labels = format(pretty(ts), "%H:%M"), lwd = 0)
    axis(2, at = pretty(shape$schedule$pivot.shape_dist_traveled/1000*1000, high.u.bias = 1),
         labels = pretty(shape$schedule$pivot.shape_dist_traveled/1000, high.u.bias = 1),
         lwd = 0, las = 2, cex.axis = 0.6)
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
    }
    dev.flush()
}

par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(3, 3))
for (trip in trips[-(1:3)]) plotTrip(trip)

par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(1,1))
plotTrip(trips[1], xlim = c(1474497914, 1474498270), ylim = c(4800, 5300))


for (trip in trips) { plotTrip(trip, dwell = TRUE); locator(1) }



dev.new()






###
print(pf(con, vid, sig.gps = 50, draw = TRUE,
         vp = ))

ret <- 0
while(ret <= 0) {
    cat(".")
    ret <- pf(con, vid, 1000, sig.gps = 5)
    Sys.sleep(0.5)
}


##
devtools::load_all("~/iNZight/iNZightPlots")
devtools::load_all("~/iNZight/iNZightMaps")

##
vp <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s'", vs[5]))



mu <- with(vp, c(position_longitude, position_latitude))
sigma <- diag(2) * 5
z <- outer(x <- seq(mu[1] - 1, mu[1] + 1, length.out = 500),
           y <- seq(mu[2] - 1, mu[2] + 1, length.out = 500),
           function(x, y) dmvnorm(cbind(x, y), mean = mu, sigma = sigma))
filled.contour(x, y, z, color.palette=viridis::magma)
