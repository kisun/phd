## Simple as possible, a Kalman Filter model that uses the schedule times as a speed function.

set.seed(359); par(mfrow = c(2, 1))
stops <- c(0, 100, 150, 200, 400, 450, 900, 1000) * 30 ## m
times <- c(0, 2, 8, 10, 25, 30, 50, 60) * 60        ## min->s
plot(times, stops, xlab = "Time (s)", ylab = "Distance Into Trip (m)", pch = 19, col = "red", cex = 0.5, type = "b",
     xlim = c(0, max(times)), ylim = c(0, max(stops)))
dx <- diff(stops)
dt <- diff(times)
plot(times[-length(times)] + 0.5 * dt, dx / dt, type = "b", pch = 19, cex = 0.5, xlab = "Time (s)", ylab = "Speed (m/s)",
     xlim = c(0, max(times)), ylim = c(0, max(dx/dt)))
par(mfrow = c(1, 1))




t <- 1:25 * 120
z <- c(90, 110, 130, 140, 170, 200, 210, 230, 250, 270, 290, 320,
       350, 390, 410, 430, 460, 490, 520, 550, 580, 620, 650, 700, 750) * 30 + rnorm(length(t), 0, 6)
plot(times, stops, xlab = "Time (s)", ylab = "Distance Into Trip (m)", pch = 19, col = "red", cex = 0.5, type = "b",
     xlim = c(0, max(times)), ylim = c(0, max(stops)))
points(t, z)


H <- rbind(c(1, 0, 0))
R <- 150     # m
q2 <- 1  # m/s
F <- rbind(c(0, 1, 0),
           c(0, 0, 1),
           c(0, 0, 0))
G <- cbind(c(0, 0, 1))
Phi <- function(dt) rbind(c(1, dt, dt^2 / 2),
                          c(0, 1, dt),
                          c(0, 0, 1))
Q <- function(dt) rbind(c(dt^5 / 20, dt^4 / 8, dt^3 / 6),
                        c(dt^4 / 8, dt^3 / 3, dt^2 / 2),
                        c(dt^3 / 6, dt^2 / 2, dt)) * q2
Xo <- cbind(c(z[1], 0, 0)) 
Po <- rbind(c(R, 0, 0),
            c(0, 90, 0),
            c(0, 0, 6))

Nk <- length(z)
K <- P <- P. <- Xhat <- Xhat. <- vector("list", Nk)
dt <- diff(t) / 60
## Predict
Xhat.[[1]] <- Phi(0) %*% Xo
P.[[1]] <- Phi(0) %*% Po %*% t(Phi(0)) + Q(0)
## Update
K[[1]] <- P.[[1]] %*% t(H) %*% solve(H %*% P.[[1]] %*% t(H) + R)
Xhat[[1]] <- Xhat.[[1]] + K[[1]] %*% (z[1] - H %*% Xhat.[[1]])
P[[1]] <- (diag(3) - K[[1]] %*% H) %*% P.[[1]]

for (i in 2:Nk) {
    ## Predict
    Dt <- (t[i] - t[i - 1]) / 60
    Xhat.[[i]] <- Phi(Dt) %*% Xhat[[i - 1]]
    P.[[i]] <- Phi(Dt) %*% P[[i - 1]] %*% t(Phi(Dt)) + Q(Dt)
    ## Update
    K[[i]] <- P.[[i]] %*% t(H) %*% solve(H %*% P.[[i]] %*% t(H) + R)
    Xhat[[i]] <- Xhat.[[i]] + K[[i]] %*% (z[i] - H %*% Xhat.[[i]])
    P[[i]] <- (diag(3) - K[[i]] %*% H) %*% P.[[i]]
}

tt <- 26:29 * 120
for (i in 1:length(tt)) {
    Dt <- (tt[i] - tail(t, 1)) / 60
    k <- i + Nk
    Xhat. <- c(Xhat., list(Phi(Dt) %*% Xhat[[Nk]]))
    P. <- c(P., list(Phi(Dt) %*% P[[Nk]] %*% t(Phi(Dt)) + Q(Dt)))
}

XX <- t(do.call(cbind, Xhat))
XX. <- t(do.call(cbind, Xhat.))
SD <- sapply(P, function(p) sqrt(p[1,1]))
SD. <- sapply(P., function(p) sqrt(p[1,1]))
##lines(t, XX[, 1], type = "l")
plot(times, stops, xlab = "Time (s)", ylab = "Distance Into Trip (m)", pch = 19, col = "red", cex = 0.5, type = "b",
     xlim = c(0, max(times)), ylim = c(0, max(stops)))
polygon(c(t, rev(t)), c(qnorm(0.025, XX.[1:Nk, 1], SD.[1:Nk]), rev(qnorm(0.975, XX.[1:Nk, 1], SD.[1:Nk]))),
        lty = 0, col = "#00770070")
polygon(c(t, rev(t)), c(qnorm(0.025, XX[, 1], SD), rev(qnorm(0.975, XX[, 1], SD))), lty = 0, col = "#99999990")
lines(t, XX[, 1])
points(t, z, pch = 19, cex = 0.5)

lines(c(t, tt), XX.[, 1], col = "darkblue", lty = 3)

XX <- t(do.call(cbind, Xhat.[-(1:Nk)]))
SD <- sapply(P.[-(1:Nk)], function(p) sqrt(p[1,1]))
polygon(c(tt, rev(tt)), c(qnorm(0.025, XX[, 1], SD), rev(qnorm(0.975, XX[, 1], SD))), lty = 0, col = "#00660090")
#lines(tt, XX[, 1], col = "darkblue", lty = 3)
points(t, z, pch = 19, cex = 0.5)

