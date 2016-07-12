## figs

xy <- matrix(c(1.0, 0.0,
               1.5, 1.0,
               3.0, 2.0,
               3.0, 3.0,
               3.5, 4.0,
               4.0, 3.9,
               5.0, 3.5,
               6.0, 3.0,
               6.5, 3.5), nrow = 2)
pdf("gps-dist1.jpg", width = 5, height = 4)
par(mar = c(3.1, 3.1, 2.1, 2.1))
plot(t(xy), type = 'l', asp = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
title(xlab = expression(paste("Longitude (", lambda, ")")), line = 1)
title(ylab = expression(paste("Latitude (", phi, ")")), line = 1)
points(3, 2.5, pch = 19)
text(3, 2.5, expression(paste("(", lambda[k]^(i), ", ", phi[k]^(i), ")")), pos=4)
dev.off()


d <- apply(rbind(xy[, -ncol(xy)], xy[, -1]), 2,
           function(x) sqrt(sum(c(x[3] - x[1], x[4] - x[2]))))
dx <- sum(d[1:2]) + d[3]/2
pdf("gps-dist2.jpg", width = 5, height = 4)
par(mar = c(3.1, 3.1, 2.1, 2.1))
plot(c(0, sum(d)), c(0, 0), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
title(xlab = "Distance into Trip", line = 1)
points(dx, 0, pch = 19)
text(dx, 0, expression(paste(d[k]^(i))), pos = 1)
dev.off()




set.seed(34245)
t1 <- 0
t2 <- 30
M <- 10
x1 <- rnorm(M, 10, 1)
x2 <- x1 + rnorm(M, 20, 1)
pdf("particle-stop.pdf", width = 5, height = 4)
par(mar = c(3.1, 3.1, 2.1, 2.1))
plot(c(rep(t1, M), rep(t2, M)), c(x1, x2), xlim = c(0, 60), ylim = c(0, 70),
     yaxt = "n", xaxt = "n", xlab = "", ylab = "", pch = 19, cex = 0.5)
title(xlab = "Time", ylab = "Distance into Trip", line = 1)
arrows(t1, x1, t2, x2, code = 0, lty = 3)
abline(h = 35, lty = 2)
t3 <- 60
x3 <- x2 + rnorm(M, 20, 1)
Ta <- t2 + (35 - x2) / (x3 - x2) * 30
arrows(t2, x2, Ta, 35, code = 0, lty = 3, col = "red")
x3[1:6] <- x3[1:6] - 10
Td <- ifelse(1:M <= 6, t3 - (x3 - 35) / (x3 - x2 + 10) * 30, Ta)
arrows(Ta, 35, Td, code = 0, lty = 3, col = "red")
arrows(Td, 35, t3, x3, code = 0, lty = 3, col = "red")
points(rep(t3, M), x3, pch = 19, cex = 0.5)
dev.off()
