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

hist <- dbGetQuery(con2, "SELECT route_id, count(route_id) as n FROM vehicle_positions WHERE route_id LIKE '%v46.5' group by route_id order by n")
rid <- "27402-20160920093629_v46.5"
#vid <- "3A9A"
vps <- dbGetQuery(
    con2,
    sprintf("SELECT * FROM vehicle_positions WHERE route_id='%s' ORDER BY timestamp", rid))
vps$trip_start_date <- format(as.POSIXct(vps$timestamp, origin = "1970-01-01"), "%Y-%m-%d")

# table(vps$trip_start_date)

ind <- which(vps$trip_start_date == "2016-09-22")
pb <- txtProgressBar(0, length(ind), style = 3)
for (i in 1:length(ind)) {
    setTxtProgressBar(pb, i)
    pf(con, vps[ind[i], "vehicle_id"], 500, sig.gps = 5, vp = vps[ind[i], ])
}; close(pb)


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
         xlab = "Time", ylab = "Distance into Trip (km)", col = "#cccccc", bty = "n", ylim = c(0, max(sh)*1.04))#, ...)
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
plotTrip(trips[1], dwell = TRUE)
#par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(3, 3))
#for (trip in trips[-(1:3)]) plotTrip(trip)

#par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(1,1))
#plotTrip(trips[1], xlim = c(1474497914, 1474498270), ylim = c(4800, 5300))

pb <- txtProgressBar(0, length(trips), style = 3)
for (trip in trips) {
    pdf(sprintf('~/Desktop/figs/trip-%s.pdf', trip), width = 9, height = 5)
    plotTrip(trip, dwell = TRUE)
    dev.off()
    setTxtProgressBar(pb, which(trips == trip))
}
close(pb)



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



#### Speed filter:
N <- 500
shape <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trips[1]), flatten = TRUE)
M <- nrow(shape$schedule)
ds <- shape$schedule$pivot.shape_dist_traveled
B0 <- matrix(rep(10, M), ncol = 1)
P0 <- 10 * diag(M)
A <- diag(M)
H <- diag(M)

update <- function(res, q = 1) {
    if (missing(res)) stop("Please specify prior")
    N <- res$N
    M <- res$M
    t <- res$t
    delta <- res$delta
    
    obs <- dbGetQuery(
        con,
        sprintf("SELECT segment, AVG(velocity) AS mean, VAR_SAMP(velocity) AS sd FROM particles WHERE timestamp BETWEEN %s AND %s GROUP BY segment",
                t - delta, t))
    Obs <- data.frame(segment = 1:M, mean = res$B, sd = rep(1e6, M))
    Obs[Obs$segment %in% obs$segment, ] <- obs
    Obs[is.na(Obs$sd), "sd"] <- 10
    
    B <- res$B
    P <- res$P
    Q <- q * diag(M)
    A <- res$A
    H <- res$H
    
    ## Predict
    Bk <- A %*% B
    Pk <- A %*% P %*% t(A) + Q
    
    ## Update
    yk <- Obs$mean - H %*% Bk
    Rk <- diag(Obs$sd)
    Sk <- H %*% Pk %*% t(H) + Rk
    Kk <- Pk %*% t(H) %*% solve(Sk)
    res$B <- Bk + Kk %*% yk
    res$P <- (diag(M) - Kk %*% H) %*% Pk

    res$t <- t + delta
    return(res)
}
plotSpeeds <- function(res, shape = NULL) {
    B <- res$B
    fr <- round(pmin(11, B[,1]/15 * 11))
    if (is.null(shape)) {
        plot(ds, rep(0, M), type = "n", yaxt = "n", ylab = "n", bty = "n",
             main = format(as.POSIXct(res$t, origin = "1970-01-01")))
        abline(v = ds, lty = 3)
        arrows(ds[-M], 0, ds[-1], code = 0, col = RColorBrewer::brewer.pal(11, "RdYlGn")[fr], lwd = 10, lend = 1)
        text(0.5 * (ds[-1] - ds[-M]) + ds[-M], 0.2, labels = round(B[-M]))
    } else {
        
        library(iNZightMaps)
        mobj <- iNZightMap(~lat, ~lon, data = SHAPE, name = "Route 274: Britomart to Three Kings")
        plot(mobj, pch = NA, join = TRUE, lwd = 6, col.line = "black",
             varnames = list(colby = "Speed (m/s)"),
             subtitle = format(as.POSIXct(res$t, origin = "1970-01-01")),
             col.fun = colorRampPalette(c("green4", "yellow", "red")))
        spd <- round(res$B / 16 * 11)
        cols <- RColorBrewer::brewer.pal(11, "RdYlGn")[spd]
        addLines(SHAPE$lat, SHAPE$lon, id = SHAPE$segment,
                 gpar = list(col = cols, lwd = 4))
    }
}

tt <- dbGetQuery(con, "SELECT min(timestamp) AS min, max(timestamp) AS max FROM particles")
t0 <- tt$min
delta <- 5 * 60
shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trips[1]), flatten = TRUE)
SHAPE <- shape$shape
SHAPE$segment <- sapply(SHAPE$dist_traveled, function(x) which(shape$schedule$pivot.shape_dist_traveled >= x)[1])

res <- list(B = B0, P = P0, N = N, M = M, A = A, H = H, t = t0 + delta, delta = delta)
Bhist <- list(mean = res$B, var = cbind(diag(res$P)), t = res$t)
q <- 0.1
#plotSpeeds(res, shape = SHAPE)
while(res$t < tt$max) {
    jpeg(sprintf("~/Desktop/figs/speeds_%s.jpg", res$t), width = 500, height = 1000)
    res <- update(res, q = q)
    Bhist$mean <- cbind(Bhist$mean, res$B)
    Bhist$var <- cbind(Bhist$var, diag(res$P))
    Bhist$t <- c(Bhist$t, res$t)
    plotSpeeds(res, shape = SHAPE)
    dev.off()
    Sys.sleep(0.1)
}



unlink("speed_history.gif")
system("convert -delay 30 -loop 0 ~/Desktop/figs/speeds_*.jpg ~/Desktop/figs/speed_history.gif")



library(grid)
hist <- Bhist
plotHistory <- function(hist) {
    xlim <- range(hist$t)
    ylim <- c(0, 16)
    grid.newpage()
    grid.rect(gp = gpar(fill = "#333333"))
    pushViewport(viewport(width = 0.9, height = 0.9, layout = grid.layout(nrow = nrow(hist$mean)-1)))
    xx <- rep(hist$t, each = 4)
    xx <- xx[-(1:2)]
    xx <- xx[1:(length(xx) - 2)]
    yy <- rep(c(0, 1, 1, 0), length.out = length(xx))
    for (i in 2:M-1) {
        pushViewport(viewport(layout.pos.row = i, xscale = xlim, yscale = ylim, clip = TRUE))
        if (i %% 2 == 0) grid.rect(gp = gpar(fill = "#444444", lwd = 0))
        spd <- round(hist$mean[i, ] / 16 * 11)
        cols <- RColorBrewer::brewer.pal(11, "RdYlGn")[spd]
        grid.polygon(unit(xx, units = "native"),
                     unit(yy, units = "npc"),
                     id.lengths = rep(4, length(xx) / 4),
                     gp = gpar(lwd = NA, col = cols, fill = cols))
        ##grid.polygon(c(hist$t, rev(hist$t)),
        ##             c(hist$mean[i, ] + hist$var[i, ], rev(hist$mean[i, ] - hist$var[i, ])),
        ##             default.units = "native", gp = gpar(lwd = 0, fill = "#999999"))
        y2 <- rbind(hist$mean[i, ] - hist$var[i, ], hist$mean[i, ] + hist$var[i, ])
        grid.polyline(rep(hist$t, each = 2), c(y2), default.units = "native", id = rep(1:ncol(y2), each = 2),
                      gp = gpar(col = "#33333380"))
        grid.lines(hist$t + 30 * 5, hist$mean[i, ], default.units = "native",
                   gp = gpar(lwd = 2))
        popViewport()
    }
    
    pushViewport(viewport(xscale = xlim, yscale = c(M-1, 0)))
    tt <- as.POSIXct(hist$t, origin = "1970-01-01")
    tta <- pretty(tt, min.n = 6)
    grid.xaxis(at = tta, label = gsub("^0", "", format(tta, "%I%P")), gp = gpar(col = "#cccccc"))
    grid.yaxis(at = 1:(M-1) - 0.5, label = 1:(M-1), gp = gpar(lwd = 0, col = "#cccccc"))
    grid.text("Segment", x = unit(-3, "line"), y = 0.5, rot = 90, gp = gpar(col = "#cccccc"))
}

jpeg(paste0("~/Desktop/figs/speed_map_q-", q, ".jpg"), width = 1920, height = 1080)
plotHistory(Bhist)
grid.text(paste("Using q =", q), 0.5, unit(1, "npc") + unit(1, "line"), gp = gpar(col = "#cccccc"))
dev.off()


addTrips <- function() {
    trips <- dbGetQuery(con, "SELECT distinct trip_id FROM particles")$trip_id
    shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trips[1]), flatten = TRUE)
    sh <- shape$schedule$pivot.shape_dist_traveled
    for (trip in trips) {
        res <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE trip_id='%s' ORDER BY id", trip))
        res$parentid <- sapply(res$parent ,function (x) {
            ret <- which(res$id == x)
            if (length(ret) == 1) return(ret) else return(NA)
        })
        resl <- res[!is.na(res$parent), ]
        Ta <- tapply(resl$arrival_time, resl$segment, mean, na.rm = TRUE)
        Td <- tapply(resl$departure_time, resl$segment, mean, na.rm = TRUE)
        Ta <- Ta[-1]
        Td <- Td[-length(Td)]
        xx <- c(rbind(Td, Ta))
        yy <- c(1, rep(2:(M-1), each = 2), M) - 1
        grid.lines(xx, yy, default.units = "native")
    }
}
