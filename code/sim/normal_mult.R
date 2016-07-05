## compare likelihood of points in bivariate normal to likelihood

library(mnormt)

X <- rmnorm(1000, c(10, 10), diag(c(5, 5)))
plot(X)
points(10, 10, pch = 19, col = "red")

## lhood mvt
l1 <- sum(dmnorm(X, c(10, 10), diag(c(5,5))))

## distances
d <- apply(X - 10, 1, function(x) sqrt(sum(x^2)))
hist(d, freq = FALSE)
#curve(dchisq(x, 2, 1), 0, 10, add = TRUE)

