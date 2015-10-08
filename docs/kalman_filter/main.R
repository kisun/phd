stops <- dget("stopinfo.dat")
hist <- dget("triphistory.dat")

with(stops, plot(time, shape_dist_traveled, pch = 19, cex = 0.5, col = "#00000040",
                 xlab = "Time (s)", ylab = "Distance (m)",
                 xaxs = "i", yaxs = "i"))
sx <- unique(stops$time)
sy <- tapply(stops$shape_dist_traveled, stops$time, min)
points(sx, sy, pch = 19)

t <- hist$time
x <- hist$dist
points(t, x, pch = 19, cex = 0.4, col = "#FF000040")

## mathematically:
## x_k = f(f'(x_{k-x}) + dt)

## graphically:

f <- splinefun(sx, sy)
fi <- splinefun(sy, sx)
curve(f(x), min(sx), max(sx), 1001, add = TRUE)

k <- 4
tk1 <- t[k - 1]
xk1 <- x[k - 1]
points(tk1, xk1, cex = 0.5, pch = 19, col = "red")

t1. <- fi(xk1)
lines(c(t1., tk1), c(xk1, xk1), lty = 3, col = "red")

tk <- t[k]
dt <- tk - tk1
t. <- t1. + dt
xk. <- f(t.)

points(tk, xk., cex = 0.5, pch = 19, col = "green4")


lines(c(t1., t1.), c(xk1, xk.), col = "blue")
arrows(t1., xk., fi(xk.), length = 0.05, col = "blue")

lines(c(tk1, tk1), c(xk1, xk.), col = "blue")
arrows(tk1, xk., tk, length = 0.05, col = "blue")


### ATTEMPT to computer Q:
stops <- dget("data/STOPS.dat")
hist <- dget("data/HISTORY.dat")

## WE WILL IGNORE THE STOPS!!!
ID <- 27951337
Hist <- hist[hist$trip_id == ID, ]
with(Hist, plot(timeIntoTrip, DIT))

tapply(1:nrow(Hist), Hist$date, function(i) {
    xi <- Hist$timeIntoTrip[i]
    yi <- Hist$DIT[i]
    splinefun(xi, yi)
}) -> sl

#### THIS WOULD FUTURISTICALLY BY BASED OFF OF KALMAN FILTER DATA
Q <- function(K) {
    lapply(K, function(k) {
        D <- sapply(sl, function(F) rbind(F(k), F(k, 1)))
        D[1, ] <- t(scale(D[1, ], T, F))
        D
    }) -> m

    cov(t(do.call(cbind, m)))
    
    
    #names(m) <- K
    #m
}

Qi <- Q(seq(200, 1000, by = 100))
sqrt(Qi)
