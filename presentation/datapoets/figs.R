## normal dist
png("normal_dist.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
curve(dnorm(x), from = -5, to = 5, n = 1001,
      xaxt = "n", yaxt = "n", bty = "n",
      xlab = "", ylab = "", lwd = 2)
axis(1, at = c(-5, 0, 5), labels = c(NA, expression(mu), NA))
dev.off()


png("particle_dist.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
set.seed(200)  ## 200
x <- rnorm(20)
curve(dnorm(x), from = -5, to = 5, n = 1001,
      xaxt = "n", yaxt = "n", bty = "n",
      xlab = "", ylab = "", lwd = 1, lty = 2)
hx <- hist(x, plot = FALSE)
xx <- rep(hx$mids, hx$counts)
yy <- do.call(c, sapply(hx$counts, function(n) 1:n * 0.05))
points(xx, yy - 0.03, cex = 4, pch = 21, bg = "#cccccc", lwd = 2)
axis(1, at = c(-5, 0, 5), labels = c(NA, expression(mu), NA))
dev.off()



## arrival time prediction
set.seed(24)
t <- 0:6 * 30
d <- c(0, 100, 250, 320, 460, 590, 650)
s <- 1000
t0 <- 5 * 60
D <- matrix(rep(d, each = 10), nrow = 10)
for (i in 1:length(t))
    D[, i] <- d[i] + rnorm(10, sd = 5)
v <- (D[, 7] - D[, 6]) / 30
Vnew <- rnorm(10, v + 1, 1)
Ta <- t[7] + (s - D[, 7]) / Vnew



plot(t, d, xlim = c(0, 500), ylim = c(0, s), type = "n", xaxt = "n", yaxt = "n",
     xlab = "ETA (minutes)", ylab = "")
title(ylab = "Distance along route", line = 1)
axis(1, at = c(t[7]), labels = c(0))
abline(h = s, lty = 3, col = "#990000", lwd = 2)
abline(v = t[7], col = "gray50")
for (i in 1:length(t)) {
    points(rep(t[i], 10), D[, i], pch = 19, col = "#00009960")
}
for (i in 1:10) {
    lines(c(t[7], Ta[i]), c(D[i, length(t)], s), col = "#00009980", lty = 2, lwd = 2)
}
abline(v = median(Ta), lty = 2)
axis(1, at = median(Ta), labels = 5)



png("arrival/particle_arrival1.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot(t, d, xlim = c(0, 500), ylim = c(0, s), type = "n", xaxt = "n", yaxt = "n",
     xlab = "ETA (minutes)", ylab = "")
title(ylab = "Distance along route", line = 1)
axis(1, at = c(t[7]), labels = c(0))
abline(h = s, lty = 3, col = "#990000", lwd = 2)
abline(v = t[7], col = "gray50")
for (i in 1:length(t)) {
    points(rep(t[i], 10), D[, i], pch = 19, col = "#00009960")
}
dev.off()

png("arrival/particle_arrival2.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot(t, d, xlim = c(0, 500), ylim = c(0, s), type = "n", xaxt = "n", yaxt = "n",
     xlab = "ETA (minutes)", ylab = "")
title(ylab = "Distance along route", line = 1)
axis(1, at = c(t[7]), labels = c(0))
abline(h = s, lty = 3, col = "#990000", lwd = 2)
abline(v = t[7], col = "gray50")
for (i in 1:length(t)) {
    points(rep(t[i], 10), D[, i], pch = 19, col = "#00009960")
}
for (i in 1:10) {
    lines(c(t[7], Ta[i]), c(D[i, length(t)], s), col = "#00009980", lty = 2, lwd = 2)
}
dev.off()

png("arrival/particle_arrival3.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot(t, d, xlim = c(0, 500), ylim = c(0, s), type = "n", xaxt = "n", yaxt = "n",
     xlab = "ETA (minutes)", ylab = "")
title(ylab = "Distance along route", line = 1)
axis(1, at = c(t[7]), labels = c(0))
abline(h = s, lty = 3, col = "#990000", lwd = 2)
abline(v = t[7], col = "gray50")
for (i in 1:length(t)) {
    points(rep(t[i], 10), D[, i], pch = 19, col = "#00009960")
}
for (i in 1:10) {
    lines(c(t[7], Ta[i]), c(D[i, length(t)], s), col = "#00009980", lty = 2, lwd = 2)
}
abline(v = median(Ta), lty = 2)
axis(1, at = median(Ta), labels = 5)
dev.off()

png("arrival/particle_arrival4.png", width = 600, height = 300, bg = "transparent")
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot(t, d, xlim = c(0, 500), ylim = c(0, s), type = "n", xaxt = "n", yaxt = "n",
     xlab = "ETA (minutes)", ylab = "")
title(ylab = "Distance along route", line = 1)
axis(1, at = c(t[7]), labels = c(0))
abline(h = s, lty = 3, col = "#990000", lwd = 2)
abline(v = t[7], col = "gray50")
for (i in 1:length(t)) {
    points(rep(t[i], 10), D[, i], pch = 19, col = "#00009960")
}
for (i in 1:10) {
    lines(c(t[7], Ta[i]), c(D[i, length(t)], s), col = "#00009980", lty = 2, lwd = 2)
}
abline(v = qq <- quantile(Ta, c(0.025, 0.9)), lty = 2)
axis(1, at = qq, labels = c(4, 8))
dev.off()

