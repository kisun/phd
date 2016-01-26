vehicle = R6Class("vehicle",
                  public = list(
                      vehicle.id = NA,
                      position = NA,
                      particles = NA,

                      initialize = function(id, position) {
                          self$vehicle.id <- id
                          self$position <- position

                          
                          
                          cat("New vehicle instantiated\n")
                          self$report()
                      },

                      report = function() {
                          cat("Vehicle ", self$vehicle.id,
                              ", last reported position: (",
                              self$position[1], ", ", self$position[2], ")\n",
                              sep = "")
                      }
                  )
                  )


## --- Accessory functions:
## Some functions I have already defined:
distanceFlat <- function(y, z, R = 6371000) {
    ## computes the distance between {y} and {z}
    ## different R (such as in km) give results in those units
    
    if (length(dim(y)) < 2)
        y <- cbind(y)
    if (length(dim(z)) < 2)
        z <- cbind(z)
    if (ncol(y) != ncol(z) & ncol(y) > 1 & ncol(z) > 1)
        stop("Incorrent dimensions")
    
    ## need to scale from degrees to radians:
    lam.y <- y[1, ] * pi / 180
    lam.z <- z[1, ] * pi / 180
    phi.y <- y[2, ] * pi / 180
    phi.z <- z[2, ] * pi / 180
    R * sqrt(((lam.y - lam.z) * cos(0.5 * (phi.y + phi.z)))^2 + (phi.y - phi.z)^2)
}
bearing <- function(a, b) {
    ## compute the bearing between two points
    
    if (length(dim(a)) < 2) y <- cbind(a)
    if (length(dim(b)) < 2) z <- cbind(b)
    if (ncol(a) != ncol(b) & ncol(a) > 1 & ncol(b) > 1)
        stop("Incorrent dimensions")
    
    ## convert to radians!!
    lam.a <- a[1, ] * pi / 180
    lam.b <- b[1, ] * pi / 180
    phi.a <- a[2, ] * pi / 180
    phi.b <- b[2, ] * pi / 180
    
    th.rad <- atan2(sin(lam.b - lam.a) * cos(phi.b),
                    cos(phi.a) * sin(phi.b) - sin(phi.a) * cos(phi.b) * cos(lam.b - lam.a))
    (th.rad * 180 / pi) %% 360
}
partialSegment <- function(x, theta, d, R = 6371000) {
    ## Compute the Lat/Lon after traveling distance D
    ## from point X at a bearing of THETA
    
    delta <- d / R ## for single calculation
    theta <- theta * pi / 180  ## convert to radians
    phi.s <- x[2] * pi / 180
    lam.s <- x[1] * pi / 180
    
    phi.z <- asin(sin(phi.s) * cos(delta) + cos(phi.s) * sin(delta) * cos(theta))
    lam.z <- lam.s + atan2(sin(theta) * sin(delta) * cos(phi.s),
                           cos(delta) - sin(phi.s) * sin(phi.z))
    c(lam.z, phi.z) * 180 / pi
}
h <- function(x, shape) {
    ## Calculate Lat/Lon position of point(s) a given distance (x)
    ## into a pattern/shape
    
    if (x[1] <= 0) return(c(0, 0))
    if (x[1] > max(shape$distance)) return(as.numeric(shape[nrow(shape), 1:2]))
    
    j <- which.min(x[1] > shape$distance) - 1
    sj <- shape[j, ]
    Psi <- sj$bearing
    d <- x[1] - sj$distance
    
    ## only do the calculations if we need to!
    if (d == 0) return(as.numeric(sj[1:2]))
    partialSegment(as.numeric(sj[1:2]), Psi, d)
}
