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