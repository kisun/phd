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
