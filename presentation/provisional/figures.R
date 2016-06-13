## particle filter simple:
N = 5
X1 = c(170, 175, 182, 182, 190)

X2 = X1 + 15 * 8
X2e = X2 + c(-2, 1, -1, 3, 2)
Y2 = 290

newPl = function(file = NULL) {
if (!is.null(file))
	png(file, width = 600, height = 200, bg = "transparent")
par(mar = c(5.1, 4.1, 0, 2.1))
plot.new()
plot.window(xlim = c(150, 350), ylim = c(0, 0.5))
axis(1)
title(xlab = "Distance into Trip (m)")
}
Yv = c(0.2, 0.2, 0.2, 0.24, 0.2)
## Frame 1:
setwd("~/Documents/University/PhD/presentation/provisional/figs")

newPl('pf1-frame1.png')
points(X1, Yv, pch = 19, cex = 0.8)
dev.off()

## Frame 2:
newPl('pf1-frame2.png')
points(X1, Yv, pch = 19, cex = 0.5)
points(X2, Yv, pch = 19, cex = 0.8, col = "#990000")
dev.off()

## Frame 3:
newPl('pf1-frame3.png')
points(X1, Yv, pch = 19, cex = 0.5)
points(X2e, rep(0.2, N), pch = 19, cex = 0.8, col = "#990000")
dev.off()

## Frame 4:
newPl('pf1-frame4.png')
points(X1, Yv, pch = 19, cex = 0.5)
points(X2e, rep(0.2, N), pch = 19, cex = 0.5, col = "#990000")
points(Y2, 0.1, pch = 4, cex = 1, col = "#000099")
dev.off()

## Frame 5:
newPl('pf1-frame5.png')
points(X1, Yv, pch = 19, cex = 0.5)
points(Y2, 0.1, pch = 4, cex = 1, col = "#000099")
dist = abs(Y2 - X2e)
pr = dnorm(dist, 0, 5)
wt = pr / sum(pr)
points(X2e, rep(0.2, N), pch = 19, cex = pmax(2*sqrt(wt), 0.1), col = "#990000")
dev.off()

## Frame 6:
X2new = X2e[c(1, 1, 2, 3, 2)]
newPl('pf1-frame6.png')
points(X1, Yv, pch = 19, cex = 0.5)
points(Y2, 0.1, pch = 4, cex = 1, col = "#000099")
points(X2e, rep(0.2, N), pch = 19, cex = 0.5, col = "#990000")
points(X2new, c(0.2, 0.24, 0.2, 0.2, 0.24), pch = 19, cex = 0.8, col = "#000000")
dev.off()





################
## particle filter with bus stop

t = 30 * 1:5
Y = c(300, 600, 900, 1100, 1400)
s = c(500, 1000, 1500)

newPl = function(file = NULL) {
    if (!is.null(file))
	png(file, width = 600, height = 300, bg = "transparent")
    par(mar = c(2.1, 2.1, 0.1, 2.1))
    plot.new()
    plot.window(xlim = c(0, max(t) + 20), ylim = c(0, max(s) + 100), xaxs = "i", yaxs = "i")
    #axis(1)
    #axis(2)
    box()
    title(xlab = "Time (s)", ylab = "Distance into Trip (m)", line = 1)
    abline(h = s, lty = 3)
}

## True path:
true <- function() {
    lines(c(0, t[3]), c(0, Y[3]))
    lines(c(t[3], 100), c(Y[3], 1000))
    lines(c(100, 110), c(1000, 1000))
    lines(c(110, t[5]), c(1000, Y[5]))
    points(t, Y, pch = 21, cex = 1, lwd = 2, bg = "white")
}
newPl("figs/pf2-frame1.png")
true()
dev.off()


y = Y + c(-10, 2, -5, 20, 10)
## start pf:
newPl("figs/pf2-frame2.png")
M <- 5
points(0, 0, pch = 19, col = "#000099")
dev.off()

## assign speeds:
newPl("figs/pf2-frame3.png")
points(0, 0, pch = 19, col = "#000099")
X <- rbind(rep(0, M), c(5, 7, 9, 11, 13))
for (i in 1:M)
    arrows(0, 0, 20, X[2, i] * 20, lty = 2, col = "#999999", length = 0.05)
dev.off()

## move particles:
newPl("figs/pf2-frame4.png")
abline(v = t[1], col = "#0000cc90", lty = 3)
points(0, 0, pch = 19, col = "#000099")
dev.off()

## animate!
for (j in 1:50) {
    newPl(sprintf("figs/pf2-frame5-%02d.png", j))
    abline(v = t[1], col = "#0000cc90", lty = 3)
    arrows(0, 0, t[1] * j/50, X[2, ] * t[1] * j/50, lty = 2, col = "#999999", length = 0.05, code = 0)
    points(rep(t[1] * j/50, M), X[2, ] * t[1] * j/50, pch = 19, col = "#000099", cex = 0.8)
    dev.off()
}

## First observation:
newPl("figs/pf2-frame6.png")
points(rep(t[1], M), X[2, ] * t[1], pch = 19, col = "#000099", cex = 0.5)
points(t[1], y[1], pch = 4, col = "#990000", lwd = 2, cex = 1.2)
dev.off()

## Resample
newPl("figs/pf2-frame7.png")
X2 <- rbind(X[2, ] * t[1], X[2, ])[, c(2, 3, 3, 3, 4)]
points(rep(t[1], M), X2[2, ] * t[1], pch = 19, col = "#000099", cex = 0.5)
points(t[1], y[1], pch = 4, col = "#990000", lwd = 2, cex = 1.2)
dev.off()


## Do it again ...
newPl("figs/pf2-frame8.png")
points(rep(t[1], M), X2[1, ], pch = 19, col = "#000099", cex = 0.5)
X2[2, ] <- c(8, 8, 10, 11, 10)
arrows(0, 0, t[1], unique(X2[1, ]), lty = 2, col = "#99999970", length = 0.05, code = 0)
arrows(t[1], X2[1, ], t[1] + 20, X2[1, ] + X2[2, ] * 20, lty = 2, col = "#999999", length = 0.05)
dev.off()

## start ... 
newPl("figs/pf2-frame9.png")
arrows(0, 0, t[1], unique(X2[1, ]), lty = 2, col = "#99999970", length = 0.05, code = 0)
abline(v = t[2], col = "#0000cc90", lty = 3)
points(rep(t[1], M), X2[1, ], pch = 19, col = "#000099", cex = 0.5)
dev.off()


## animate!
for (j in 1:50) {
    newPl()#sprintf("figs/pf2-frame10-%02d.png", j))
    abline(v = t[2], col = "#0000cc90", lty = 3)
    arrows(t[1], X2[1, -5], t[1]  + 30 * j/50, X2[1, -5] + X2[2, -5] * (30 * j/50),
           lty = 2, col = "#999999", length = 0.05, code = 0)
    points(rep(t[1] + 30 * j/50, M-1), X2[1, -5] + X2[2, -5] * (30 * j/50), pch = 19, col = "#000099", cex = 0.8)
    
    #dev.off()
}
