## test running C from R:

n <- 10
x <- rnorm(n, 20, 5)
wt <- runif(n)

load <- function() {
    system("R CMD SHLIB ../src/C/filter.c")
    dyn.load("../src/C/filter.so")
}

wtMean <- function(x, wt) 
    .Call("wtMean", x, wt)
Rmean <- function(x, w) {
    mu <- 0
    wt <- 0
    for (i in 1:length(x)) {
        mu <- mu + x[i] * w[i]
        wt <- wt + w[i]
    }
    mu <- mu / wt

    return(mu)
}

load()

test <- function(N = 10) {
    x <- rnorm(N, 20, 5)
    wt <- runif(N)
    
    R <- system.time(Rmean(x, wt))
    C <- system.time(wtMean(x, wt))

    rbind(R, C)
}

T <- matrix(nrow = 2, ncol = 8)
for (i in 1:8) {
    T[, i] <- test(10^i)[, "elapsed"]
}
plot(NA, xlim = c(0, 10), ylim = c(0, log10(max(T)+1)), xaxt = "n",
     ylab = "Time (s)", xlab = "N")
lines(1:8, log10(T[1, ]+1), col = "red")
lines(1:8, log10(T[2, ]+1), col = "blue")
