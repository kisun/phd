## particle filter simple:
N = 5
X1 = c(170, 175, 182, 182, 190)

X2 = X1 + 15 * 8
X2e = X2 + c(-2, 1, -1, 3, 2)
Y2 = 290

newPl = function(file = NULL) {
if (!is.null(file))
	pdf(gsub("png", "pdf", file), width = 6, height = 2, bg = "transparent")
par(mar = c(5.1, 4.1, 0, 2.1))
plot.new()
plot.window(xlim = c(150, 350), ylim = c(0, 0.5))
axis(1)
title(xlab = "Distance (m)")
}
Yv = c(0.2, 0.2, 0.2, 0.24, 0.2)
## Frame 1:
## setwd("~/Documents/University/PhD/presentation/provisional/figs")
setwd("../provisional/figs")

newPl('pf1-frame1.png')
points(X1, Yv, pch = 19, cex = 1.5)
dev.off()

## Frame 2:
newPl('pf1-frame2.png')
points(X1, Yv, pch = 19, cex = 1)
points(X2, Yv, pch = 19, cex = 1.5, col = "#990000")
dev.off()

## Frame 3:
newPl('pf1-frame3.png')
points(X1, Yv, pch = 19, cex = 1)
points(X2e, rep(0.2, N), pch = 19, cex = 1.5, col = "#990000")
dev.off()

## Frame 4:
newPl('pf1-frame4.png')
points(X1, Yv, pch = 19, cex = 1)
points(X2e, rep(0.2, N), pch = 19, cex = 1, col = "#990000")
points(Y2, 0.1, pch = 4, cex = 2, col = "#000099", lwd = 2)
dev.off()

## Frame 5:
newPl('pf1-frame5.png')
points(X1, Yv, pch = 19, cex = 1)
points(Y2, 0.1, pch = 4, cex = 2, col = "#000099", lwd = 2)
dist = abs(Y2 - X2e)
pr = dnorm(dist, 0, 5)
wt = pr / sum(pr)
points(X2e, rep(0.2, N), pch = 19, cex = pmax(3*sqrt(wt), 0.1), col = "#990000")
dev.off()

## Frame 6:
X2new = X2e[c(1, 1, 2, 3, 2)]
newPl('pf1-frame6.png')
points(X1, Yv, pch = 19, cex = 1)
points(Y2, 0.1, pch = 4, cex = 2, lwd = 2, col = "#000099")
points(X2e, rep(0.2, N), pch = 19, cex = 1, col = "#990000")
points(X2new, c(0.2, 0.24, 0.2, 0.2, 0.24), pch = 19, cex = 1.5, col = "#000000")
dev.off()





################
## particle filter with bus stop

## t = 30 * 1:5
## Y = c(300, 600, 900, 1100, 1400)
## s = c(500, 1000, 1500)

## newPl = function(file = NULL) {
##     if (!is.null(file))
## 	png(file, width = 600, height = 300, bg = "transparent")
##     par(mar = c(2.1, 2.1, 0.1, 2.1))
##     plot.new()
##     plot.window(xlim = c(0, max(t) + 20), ylim = c(0, max(s) + 100), xaxs = "i", yaxs = "i")
##     #axis(1)
##     #axis(2)
##     box()
##     title(xlab = "Time (s)", ylab = "Distance into Trip (m)", line = 1)
##     abline(h = s, lty = 3)
## }

## ## True path:
## true <- function() {
##     lines(c(0, t[3]), c(0, Y[3]))
##     lines(c(t[3], 100), c(Y[3], 1000))
##     lines(c(100, 110), c(1000, 1000))
##     lines(c(110, t[5]), c(1000, Y[5]))
##     points(t, Y, pch = 21, cex = 1, lwd = 2, bg = "white")
## }
## newPl("figs/pf2-frame1.png")
## true()
## dev.off()


## y = Y + c(-10, 2, -5, 20, 10)
## ## start pf:
## newPl("figs/pf2-frame2.png")
## M <- 5
## points(0, 0, pch = 19, col = "#000099")
## dev.off()

## ## assign speeds:
## newPl("figs/pf2-frame3.png")
## points(0, 0, pch = 19, col = "#000099")
## X <- rbind(rep(0, M), c(5, 7, 9, 11, 13))
## for (i in 1:M)
##     arrows(0, 0, 20, X[2, i] * 20, lty = 2, col = "#999999", length = 0.05)
## dev.off()

## ## move particles:
## newPl("figs/pf2-frame4.png")
## abline(v = t[1], col = "#0000cc90", lty = 3)
## points(0, 0, pch = 19, col = "#000099")
## dev.off()

## ## animate!
## for (j in 1:50) {
##     newPl(sprintf("figs/pf2-frame5-%02d.png", j))
##     abline(v = t[1], col = "#0000cc90", lty = 3)
##     arrows(0, 0, t[1] * j/50, X[2, ] * t[1] * j/50, lty = 2, col = "#999999", length = 0.05, code = 0)
##     points(rep(t[1] * j/50, M), X[2, ] * t[1] * j/50, pch = 19, col = "#000099", cex = 0.8)
##     dev.off()
## }

## ## First observation:
## newPl("figs/pf2-frame6.png")
## points(rep(t[1], M), X[2, ] * t[1], pch = 19, col = "#000099", cex = 0.5)
## points(t[1], y[1], pch = 4, col = "#990000", lwd = 2, cex = 1.2)
## dev.off()

## ## Resample
## newPl("figs/pf2-frame7.png")
## X2 <- rbind(X[2, ] * t[1], X[2, ])[, c(2, 3, 3, 3, 4)]
## points(rep(t[1], M), X2[2, ] * t[1], pch = 19, col = "#000099", cex = 0.5)
## points(t[1], y[1], pch = 4, col = "#990000", lwd = 2, cex = 1.2)
## dev.off()


## ## Do it again ...
## newPl("figs/pf2-frame8.png")
## points(rep(t[1], M), X2[1, ], pch = 19, col = "#000099", cex = 0.5)
## X2[2, ] <- c(8, 8, 10, 11, 10)
## arrows(0, 0, t[1], unique(X2[1, ]), lty = 2, col = "#99999970", length = 0.05, code = 0)
## arrows(t[1], X2[1, ], t[1] + 20, X2[1, ] + X2[2, ] * 20, lty = 2, col = "#999999", length = 0.05)
## dev.off()

## ## start ... 
## newPl("figs/pf2-frame9.png")
## arrows(0, 0, t[1], unique(X2[1, ]), lty = 2, col = "#99999970", length = 0.05, code = 0)
## abline(v = t[2], col = "#0000cc90", lty = 3)
## points(rep(t[1], M), X2[1, ], pch = 19, col = "#000099", cex = 0.5)
## dev.off()


## ## animate!
## for (j in 1:50) {
##     newPl()#sprintf("figs/pf2-frame10-%02d.png", j))
##     abline(v = t[2], col = "#0000cc90", lty = 3)
##     arrows(t[1], X2[1, -5], t[1]  + 30 * j/50, X2[1, -5] + X2[2, -5] * (30 * j/50),
##            lty = 2, col = "#999999", length = 0.05, code = 0)
##     points(rep(t[1] + 30 * j/50, M-1), X2[1, -5] + X2[2, -5] * (30 * j/50), pch = 19, col = "#000099", cex = 0.8)
    
##     #dev.off()
## }




setwd("..")

## schedule times
sx <- c(0, 10, 20, 40, 50, 55, 80, 100)
st <- c(0, 1, 2, 5, 6, 6, 9, 10)

pl <- function(file=NULL) {
    if (!is.null(file))
	pdf(gsub("png", "pdf", file), width = 8, height = 4)
    par(mar = c(2.1, 2.1, 2.1, 2.1))
    plot(st, sx, xaxt = "n", yaxt = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i",
         pch = 19, xlim = c(0, 12), ylim = c(0, 110))
    title(xlab = "Scheduled Arrival Time", ylab = "Distance into Trip", line = 1)
    lines(c(st, 12), c(sx, 120), lty = 3)
    abline(h = max(sx), lty = 2, col = "#666666")
}

## fig 1:
pl("figs/pred-sched-frame1.png")
dev.off()


## fig 2:
X <- 30
pl("figs/pred-sched-frame2.png")
lines(c(0, 0.1), c(X, X), lty = 2, col = "#990000")
points(0.1, X, col = "#990000", pch = 4, lwd = 2)
dev.off()

## fig 3:
Xt <- 3.5
pl("figs/pred-sched-frame3.png")
lines(c(0, Xt), c(X, X), lty = 2, col = "#990000")
points(Xt, X, col = "#990000", pch = 4, lwd = 2)
dev.off()

## fig 4:
pl("figs/pred-sched-frame4.png")
lines(c(0, Xt), c(X, X), lty = 2, col = "#990000")
lines(c(Xt, Xt), c(0, X), lty = 3, col = "#444444")
lines(c(max(st), max(st)), c(0, max(sx)), lty = 3, col = "#444444")
arrows(Xt + 0.05, X - 10, x1 = max(st) - 0.05, length = 0.1, code = 3, lwd = 2, col = "#000099")
text(Xt + (max(st) - Xt) / 2, X - 10, "Scheduled Travel Time", cex = 0.8, pos = 3)
dev.off()



## dwell time:
png("figs/dwell_time.png", width = 600, height = 300, bg = "transparent")
par(mar = c(2.1, 2.1, 2.1, 2.1))
plot(NULL, xaxt = "n", yaxt = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     pch = 19, xlim = c(0, 10), ylim = c(0, 100))
title(xlab = "Time", ylab = "Distance into Trip", line = 1)
abline(h = 40, lty = 2, lwd = 2)
points(1, 10, pch = 19, col = "#000099")
#lines(c(1, 2), c(10, 30), col = "#000099", lwd = 2, lty = 3)
lines(c(1, 6), c(10, 110), col = "#000099", lwd = 2, lty = 3)
lines(c(2, 2.5, 2.7, 2.8, 4, 6, 6.2, 6.3, 6.5, 7, 10),
      c(30, 37, 38, 38.5, 40, 40, 41.5, 42, 43, 50, 110), col = "#990000", lwd = 2, lty = 3)
arrows(4.1, 70, 7.9, length = 0.15, code = 3, lwd = 2)
text(6, 70, expression(tau[j]), pos = 3)
dev.off()




### ALL BUSES
key <- scan("../../code/auckland_transport/apikey.txt", what=character())
system(sprintf('curl -v -X GET "https://api.at.govt.nz/v2/public/realtime" -H "Ocp-Apim-Subscription-Key: %s" > realtime.json', key))

library(jsonlite)
rt <- fromJSON(readLines("realtime.json"))

pos <- rt$response$entity$vehicle
pos <- pos[!is.na(pos$position$latitude), ]
library(iNZightMaps)
mobj <- iNZightMap(~latitude, ~longitude, data = pos$position, name = "Auckland Bus Locations")

pdf("figs/allbuses.pdf", width = 8, height = 5)
plot(mobj, pch = 19, cex.pt = 0.2, col.pt = "#333333",
     main = paste0(as.POSIXct(rt$response$header$timestamp, origin = "1970-01-01"),
                  ", N = ", nrow(pos)))
dev.off()

lat <- "-36.857595"
lon <- "174.764315"
dist <- 20
system(sprintf('curl -v -X GET "https://api.at.govt.nz/v2/gtfs/routes/geosearch?lat=%s&lng=%s&distance=%s" -H "Ocp-Apim-Subscription-Key: %s" > realtime-symonds.json', lat, lon, dist, key))

symonds <- fromJSON(readLines("realtime-symonds.json"))
routeids <- unique(symonds$response$route_id)

pos.sym <- pos[pos$trip$route_id %in% routeids, ]

mobj2 <- iNZightMap(~latitude, ~longitude, data = pos.sym$position, name = "Symonds Street Buses")

pdf("figs/symondsbuses.pdf", width = 8, height = 5)
plot(mobj2, pch = 4, cex.pt = 0.7, lwd.pt = 2, col.pt = "#4444cc", #fill.pt = "#cccccc",
     main = paste0(as.POSIXct(rt$response$header$timestamp, origin = "1970-01-01"),
                   ", N = ", nrow(pos.sym)))
dev.off()









##### GPS DISTANCE

## route shape
xy <- matrix(c(1.0, 0.0,
               1.5, 1.0,
               3.0, 2.0,
               3.0, 3.0,
               3.5, 4.0,
               4.0, 3.9,
               5.0, 3.5,
               6.0, 3.0,
               6.5, 3.5), nrow = 2)


set.seed(15)
d <- apply(rbind(xy[, -ncol(xy)], xy[, -1]), 2,
           function(x) sqrt(sum(c(x[3] - x[1], x[4] - x[2]))))
dd <- c(0, cumsum(d))
##dx <- sum(d[1:2]) + d[3]/2
dx <- dd[2] + rnorm(5, sd = 0.5)

pl <- function(file = NULL) {
    if (!is.null(file))
        pdf(file, width = 6, height = 6, bg = "transparent")
    par(mar = c(3.1, 3.1, 2.1, 2.1))
    plot(c(0, sum(d)), c(0, 0), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         bty = "n")
    title(xlab = "Distance into Trip", line = 1)
}


pl("figs/pf2-frame1.pdf")
points(dx, rep(0, 5), pch = 19)
dev.off()


dx2 <- dx + rnorm(5, 3.5, 0.1)
pl("figs/pf2-frame2.pdf")
points(dx, rep(0, 5), col = "#00000050", pch = 19, cex = 0.8)
points(dx2, rep(0, 5), pch = 19)
dev.off()

pl2 <- function(file = NULL) {
    if (!is.null(file))
        pdf(file, width = 6, height = 6, bg = "transparent")
    par(mar = c(3.1, 3.1, 2.1, 2.1))
    plot(t(xy), type = 'l', asp = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         bty = "n")
    title(xlab = expression(paste("Longitude (", lambda, ")")), line = 1)
    title(ylab = expression(paste("Latitude (", phi, ")")), line = 1)
}


w <- sapply(dx2, function(x) max(which(x > dd)))
dr <- dx2 - dd[w]
fr <- dr / d[w]
dxy <- t(xy[, w]) + sweep(apply(xy, 1, diff)[w, ], 1, fr, "*")

pl2("figs/pf2-frame3.pdf")
points(dxy, pch = 19)
dev.off()

yy <- c(3.55, 3.9)
pl2("figs/pf2-frame4.pdf")
points(dxy, pch = 19, cex = 0.8)
points(yy[1], yy[2], pch = 4, col = "#990000", lwd = 2, cex = 1.6)
dev.off()


dwt <- apply(dxy, 1, function(x) sqrt(sum((x - yy)^2)))
wt <- max(dwt) - dwt / sum(dwt)
pl2("figs/pf2-frame5.pdf")
points(dxy, pch = 19, cex = 2.5 * sqrt(wt))
points(yy[1], yy[2], pch = 4, col = "#990000", lwd = 2, cex = 1.6)
dev.off()


set.seed(10)
dxy2 <- dxy[wii <- sample(5, 5, replace = TRUE, prob = wt), ]
pl2("figs/pf2-frame6.pdf")
points(dxy2, pch = 19)
points(yy[1], yy[2], pch = 4, col = "#990000", lwd = 2, cex = 1.6)
dev.off()

pl("figs/pf2-frame7.pdf")
points(dx2, rep(0, 5), pch = 19, col = "#00000050", cex = 0.8)
points(dx2[wii], c(0.02, 0, 0.04, 0, 0), pch = 19)
dev.off()








par(mar = c(3.1, 3.1, 2.1, 2.1))
plot(t(xy), type = 'l', asp = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
title(xlab = expression(paste("Longitude (", lambda, ")")), line = 1)
title(ylab = expression(paste("Latitude (", phi, ")")), line = 1)
points(3, 2.5, pch = 19)
text(3, 2.5, expression(paste("(", lambda[k]^(i), ", ", phi[k]^(i), ")")), pos=4)
#dev.off()
