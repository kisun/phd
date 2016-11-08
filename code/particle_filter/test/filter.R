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


## Above is common ...
## Below is different things ...

# table(vps$trip_start_date)

ind <- which(vps$trip_start_date == "2016-10-26")
infoList <- lapply(unique(vps$trip_id[ind]), function(ID) {
    fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", ID), flatten = TRUE)
})
names(infoList) <- unique(vps$trip_id[ind])

pb <- txtProgressBar(0, length(ind), style = 3)
for (i in 1:length(ind)) {
    setTxtProgressBar(pb, i)
    pf(con, vps[ind[i], "vehicle_id"], 500, sig.gps = 5, vp = vps[ind[i], ],
       info = infoList[[vps[ind[i], "trip_id"]]], SPEED.range = c(1, 60) * 1000 / 60^2)
    ## update the 
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
q <- 1
#plotSpeeds(res, shape = SHAPE)
while(res$t < tt$max) {
    #jpeg(sprintf("~/Desktop/figs/speeds_%s.jpg", res$t), width = 500, height = 1000)
    res <- update(res, q = q)
    Bhist$mean <- cbind(Bhist$mean, res$B)
    Bhist$var <- cbind(Bhist$var, diag(res$P))
    Bhist$t <- c(Bhist$t, res$t)
    #plotSpeeds(res, shape = SHAPE)
    #dev.off()
    #Sys.sleep(0.1)
}



unlink("speed_history.gif")
system("convert -delay 30 -loop 0 ~/Desktop/figs/speeds_*.jpg ~/Desktop/figs/speed_history.gif")




library(grid)
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
        grid.polygon(c(hist$t, rev(hist$t)),
                     c(hist$mean[i, ] + hist$var[i, ], rev(hist$mean[i, ] - hist$var[i, ])),
                     default.units = "native", gp = gpar(lwd = 0, fill = "#33333330"))
        y2 <- rbind(hist$mean[i, ] - hist$var[i, ], hist$mean[i, ] + hist$var[i, ])
        #grid.polyline(rep(hist$t, each = 2), c(y2), default.units = "native", id = rep(1:ncol(y2), each = 2),
        #              gp = gpar(col = "#33333380"))
        grid.lines(hist$t + 30 * 5, hist$mean[i, ], default.units = "native",
                   gp = gpar(lwd = 1, col = "#333333"))
        popViewport()
    }
    
    pushViewport(viewport(xscale = xlim, yscale = c(M-1, 0)))
    tt <- as.POSIXct(hist$t, origin = "1970-01-01")
    tta <- pretty(tt, min.n = 6)
    grid.xaxis(at = tta, label = gsub("^0", "", format(tta, "%I%P")), gp = gpar(col = "#cccccc"))
    grid.yaxis(at = 1:(M-1) - 0.5, label = 1:(M-1), gp = gpar(lwd = 0, col = "#cccccc"))
    grid.text("Segment", x = unit(-3, "line"), y = 0.5, rot = 90, gp = gpar(col = "#cccccc"))
}

#jpeg(paste0("~/Desktop/figs/speed_map_q-", q, ".jpg"), width = 1920, height = 1080)
plotHistory(Bhist)
#grid.text(paste("Using q =", q), 0.5, unit(1, "npc") + unit(1, "line"), gp = gpar(col = "#cccccc"))
#dev.off()
apply(Bhist$mean, 1, function(x) sd(diff(x)))

## addTrips <- function() {
##     trips <- dbGetQuery(con, "SELECT distinct trip_id FROM particles")$trip_id
##     shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trips[1]), flatten = TRUE)
##     sh <- shape$schedule$pivot.shape_dist_traveled
##     for (trip in trips) {
##         res <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE trip_id='%s' ORDER BY id", trip))
##         res$parentid <- sapply(res$parent ,function (x) {
##             ret <- which(res$id == x)
##             if (length(ret) == 1) return(ret) else return(NA)
##         })
##         resl <- res[!is.na(res$parent), ]
##         Ta <- tapply(resl$arrival_time, resl$segment, mean, na.rm = TRUE)
##         Td <- tapply(resl$departure_time, resl$segment, mean, na.rm = TRUE)
##         Ta <- Ta[-1]
##         Td <- Td[-length(Td)]
##         xx <- c(rbind(Td, Ta))
##         yy <- c(1, rep(2:(M-1), each = 2), M) - 1
##         grid.lines(xx, yy, default.units = "native")
##     }
## }


for (i in 1:ncol(Bhist$mean)) {
    plot(diff(Bhist$mean[, i]), type = "l", ylim = c(-16, 16), xlim = c(1, M))
    locator(1)
}


zx <- log(apply(Bhist$mean, 2, function(x) x[-1] / x[-length(x)]))
plot(1:ncol(zx), type="n", ylim = range(zx))
apply(zx, 1, lines, col = "#33333330")

barplot(rowMeans(zx), width = 1, space = 0)
sd <- apply(zx, 1, sd) / sqrt(ncol(zx))
arrows(1:nrow(zx) - 0.5, rowMeans(zx) - sd, y1 = rowMeans(zx + sd), length = 0.1, code = 0, angle = 90, lwd = 2)






#####################################################################
#####################################################################
#####################################################################
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

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")
con2 = dbConnect(drv, dbname = "historical", host = "localhost",
                 user = "homestead", port = "54320", password = "secret")

hist <- dbGetQuery(con2, "SELECT route_id, count(route_id) as n FROM vehicle_positions WHERE route_id LIKE '%v46.25' group by route_id order by n")
rid <- "27402-20161011151756_v46.25"
#vid <- "3A9A"
vps <- dbGetQuery(
    con2,
    sprintf("SELECT * FROM vehicle_positions WHERE route_id='%s' ORDER BY timestamp", rid))
vps$trip_start_date <- format(as.POSIXct(vps$timestamp, origin = "1970-01-01"), "%Y-%m-%d")

# table(vps$trip_start_date)

ind <- which(vps$trip_start_date == "2016-10-26")
infoList <- lapply(unique(vps$trip_id[ind]), function(ID) {
    fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", ID), flatten = TRUE)
})
names(infoList) <- unique(vps$trip_id[ind])


kf.t <- vps[ind[1], "timestamp"]
N <- 500
shape <- infoList[[1]]$shape
shape$segment <- sapply(shape$dist_traveled, function(x) which(shape$schedule$pivot.shape_dist_traveled >= x)[1])
schedule <- infoList[[1]]$schedule
M <- nrow(schedule)
ds <- schedule$pivot.shape_dist_traveled
B0 <- matrix(rep(10, M), ncol = 1)
P0 <- 10 * diag(M)
A <- diag(M)
H <- diag(M)
delta <- 5 * 60
speed <- list(B = B0, P = P0, N = N, M = M, A = A, H = H, t = kf.t, delta = delta)

i <- 0
BHist <- list(mean = speed$B, var = cbind(diag(speed$P)), t = speed$t)
pb <- txtProgressBar(0, length(ind), style = 3)
for (i in max(i, 1):length(ind)) {
    setTxtProgressBar(pb, i)
    ## update the speed KF:
    #i <- i + 1
    if (vps[ind[i], "timestamp"] > speed$t + speed$delta) {
        ## cat("Kalman filter update ...\n")
        ## jpeg(sprintf("~/Desktop/figs/speeds_%s.jpg", speed$t), width = 500, height = 1000)
        speed <- update(speed, q = 1)
        if (any(diag(speed$P) < 0.000001)) diag(speed$P) <- pmax(0.000001, diag(speed$P))
        BHist$mean <- cbind(BHist$mean, speed$B)
        BHist$var <- cbind(BHist$var, diag(speed$P))
        BHist$t <- c(BHist$t, speed$t)
        ## plotSpeeds(speed, shape = shape)
        ## dev.off()
    }
    pf(con, vps[ind[i], "vehicle_id"], 500, sig.gps = 5, vp = vps[ind[i], ], speed = speed,
       info = infoList[[vps[ind[i], "trip_id"]]], SPEED.range = c(2, 60) * 1000 / 60^2)
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
}; close(pb)

plotHistory(BHist)


drawSegments <- function(shape, schedule, speeds, times, true, MAX.speed = 100 * 1000 / 60^2) {
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
    o <- par(mfrow = c(1, 1), bg = "#333333", fg = "#cccccc", col.axis = "#cccccc",
             col.lab = "#cccccc", col.main = "#cccccc")
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
                    c(Sd[i] + (Sd[i + 1] - Sd[i]) * (1 - pmin((speeds[i, ] + sqrt(var[i, ])) / MAX.speed, 1)),
                      rev(Sd[i] + (Sd[i + 1] - Sd[i]) * (1 - pmax(0, (speeds[i, ] - sqrt(var[i, ])) / MAX.speed)))),
                    border = NULL, col = "#33333320")
            lines(times, Sd[i] + (Sd[i + 1] - Sd[i]) * (1 - speeds[i, ] / MAX.speed), lwd = 1, col = "#333333")
        }
        if (!missing(true)) {
            lines(times, Sd[i] + (Sd[i + 1] - Sd[i]) * (1 - true[i, ] / MAX.speed), col = "#222222", lty = 2, type = "s")
        }
    }
    abline(h = Sd[2:(length(Sd) - 1)], col = "#33333330")
    par(o)
}
drawSegments(shape, schedule, BHist, MAX.speed = 60 * 1000 / 60^2)





par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", mfrow = c(4, 3))
plotTrip <- function(trip, dwell = FALSE, ...) {
    speed <- !dwell
    dev.hold()
    res <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE trip_id='%s' ORDER BY id", trip))
    shape <-fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", trip), flatten = TRUE)
    par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc")
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
    }
    dev.flush()
}
trips <- dbGetQuery(con, "SELECT trip_id, min(timestamp) as start FROM particles GROUP BY trip_id ORDER BY start")$trip_id

for ( i in 1:length(trips)) {
    plotTrip(trips[i])
    locator(1)
}













### Latest
### BIG UPDATE: using SEGMENTS instead ...
ind <- which(vps$trip_start_date == "2016-10-26" & vps$vehicle_id == "3A99")
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


## library(iNZightMaps)

## mobj <- iNZightMap(~lat, ~lon, data = infoList[[1]]$shape)
## plot(mobj, join = TRUE, pch = NA, lwd = 4)
## with(infoList[[1]]$schedule, addPoints(lat, lon, pch = 19, gpar = list(col = "#990000", cex = 0.5)))
## res <- invisible({
##     sapply(infoList[[1]]$schedule$pivot.shape_dist_traveled, function(x) {
##         y <- h(x, infoList[[1]]$shape)
##         addPoints(y[1], y[2], pch = 19, gpar = list(cex = 0.3))
##     })
## })


source("src/pf.R"); system("make pf.so")
N <- 500
shape <- infoList[[1]]$shape
schedule <- infoList[[1]]$schedule
M <- max(shape$leg)
L <- nrow(schedule) ## number of STOPS
kf.t <- vps[ind[1], "timestamp"]
ds <- schedule$pivot.shape_dist_traveled # stop distances
dr <- c(0, tapply(shape$dist_traveled, shape$leg, max))
B0 <- matrix(rep(10, M), ncol = 1)
P0 <- 10 * diag(M)
A <- diag(M)
H <- diag(M)
delta <- 5 * 60
speed <- list(B = B0, P = P0, N = N, M = M, A = A, H = H, t = kf.t, delta = delta)
PRED <- function(m, t) sprintf("predictions/method_%d/%d.csv", m, t)
BHist <- list(mean = speed$B, var = cbind(diag(speed$P)), t = speed$t)
MAX.speed <- 60 * 1000 / 60^2
MIN.speed <- 10 * 1000 / 60^2



## DELETE PARTICLES!!!!!
del <- dbGetQuery(con, "DELETE FROM particles")
k <- 0
pb <- txtProgressBar(0, length(ind), style = 3)
pdf("figures_1/trial1.pdf", width = 6, height = 10)
for (k in (k+1):length(ind)) {
    setTxtProgressBar(pb, k)
    ## update the speed KF:
    ## if (vps[ind[k], "timestamp"] > speed$t + speed$delta) {
    ##     speed <- update(speed, q = 1)
    ##     if (any(diag(speed$P) < 0.000001)) diag(speed$P) <- pmax(0.000001, diag(speed$P))
    ##     BHist$mean <- cbind(BHist$mean, speed$B)
    ##     BHist$var <- cbind(BHist$var, diag(speed$P))
    ##     BHist$t <- c(BHist$t, speed$t)
    ## }
    res <- pf(con, vps[ind[k], "vehicle_id"], 500, sig.gps = 5, vp = vps[ind[k], ], speed = speed,
              info = infoList[[vps[ind[k], "trip_id"]]], SPEED.range = c(MIN.speed, MAX.speed), draw = TRUE,
              rho = 0.5)
    grid.locator()
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
}; close(pb); dev.off()

## save(PRED, file = "predictions.rda")
## save(BHist, file = "speed_history.rda")

## load("predictions.rda")
## load("speed_history.rda")


drawSegments(shape, schedule, BHist, MAX.speed = MAX.speed)

arrivaltimes <- dbGetQuery(con2, sprintf("select distinct trip_id, stop_sequence, stop_id, arrival_delay, departure_delay from trip_updates as tu, stop_time_updates as stu where tu.oid=stu.trip_update_id and timestamp between %s and %s order by trip_id, stop_sequence", min(vps[ind, "timestamp"]), max(vps[ind, "timestamp"])))

o <- with(vps[ind, ], tapply(trip_start_time, trip_id, unique))
ORD <- order(factor(vps$trip_id[ind], levels = names(o)[order(o)]), vps$timestamp[ind])


animation::saveHTML({
    pb <- txtProgressBar(0, length(ind), style = 3)
    for (k in ORD) {
        dev.hold()
        plot(NA, xlim = c(0, 90 * 60), ylim = c(0, M) + 0.5,
             xlab = "Arrival Time (min after trip start)", xaxs = "i", xaxt = "n",
             ylab = "Stop #", yaxs = "i", yaxt = "n")
        abline(h = 2:M - 0.5, lty = 3, col = "#cccccc")
        axis(2, at = 1:M, las = 1, tick = FALSE)
        axis(1, at = pretty(c(0, 90)) * 60, labels = pretty(c(0, 90)))
        for (j in 2:M) {
            ## "Actual"
            Ta <- arrivaltimes[arrivaltimes$trip_id == vps[ind[k], "trip_id"], ]
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

                St <- infoList[[vps[ind[k], "trip_id"]]]$schedule[, c("pivot.arrival_time", "pivot.departure_time")]
                St[, 1] <- as.numeric(as.POSIXct(paste(vps[ind[k], "trip_start_date"], St[, 1]), origin = "1970-01-01"))
                St[, 2] <- as.numeric(as.POSIXct(paste(vps[ind[k], "trip_start_date"], St[, 2]), origin = "1970-01-01"))
                St <- as.matrix(St)
                for (s in as.character(unique(Ta$stop_sequence))) {
                    delays[, s] <- St[s, ] + delays[, s] - min(St)
                    lines(delays[, s], rep(as.numeric(s), 2) + 0.3, col = "orangered", lwd = 2)
                }
            }
            ## Scheduled
            points(PRED[j,,k,1], rep(j - 0.5, N), pch = 19, cex = 0.2)
            points(PRED[j,,k,2], rep(j - 0.3, N), pch = 19, cex = 0.2, col = "#990000")
            points(PRED[j,,k,3], rep(j - 0.1, N), pch = 19, cex = 0.2, col = "#009900")
            points(PRED[j,,k,4], rep(j + 0.1, N), pch = 19, cex = 0.2, col = "#000099")
        }
        legend("bottomright", legend = rev(c("Schedule", "Schedule Deviation", "Vehicle State", "Traffic State")),
               col = rev(c("black", "#990000", "#009900", "#000099")), pch = 19, cex = 0.8, bty = "n")
        dev.flush()
        setTxtProgressBar(pb, which(ORD == k))
    }
    close(pb)
}, "arrival_time_predictions", ani.width = 900, ani.height = 600)
