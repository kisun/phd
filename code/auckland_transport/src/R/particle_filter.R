require(R6)
vehicle = R6Class("vehicle",
                  public = list(
                      sig.a = 1.5,
                      sig.gps = 50,
                      initialize = function(id, position, trip, pattern, N.particles = 200) {
                          private$vehicle.id <- id
                          private$position <- as.numeric(position)
                          private$current.trip <- trip

                          private$N.particles <- N.particles
                          
                          if (!missing(pattern)) {
                              self$setPattern(pattern)
                              private$initParticles()
                          }
                          
                          cat("New vehicle instantiated.\n")
                          self$report()

                          invisible(self)
                      },

                      report = function() {
                          cat("Vehicle ", private$vehicle.id,
                              ", last reported position: (",
                              private$position[1], ", ", private$position[2], ")\n",
                              sep = "")
                      },

                      setPattern = function(pattern) {
                          if (class(pattern) != "data.frame")
                              stop("Please supply a data frame with at least the columns `trip_id`, `shape_pt_lat`, and `shape_pt_lon`.")

                          cols <- c("trip_id", "shape_pt_lat", "shape_pt_lon", "departure_time")
                          if (!all(cols[1:3] %in% colnames(pattern)))
                              stop("Please supply a data frame with at least the columns `trip_id`, `shape_pt_lat`, and `shape_pt_lon`.")

                          tmp <- pattern[, cols]

                          ## compute distances and bearings
                          p1 <- t(tmp[-nrow(tmp), c("shape_pt_lon", "shape_pt_lat")])
                          p2 <- t(tmp[-1, c("shape_pt_lon", "shape_pt_lat")])
                          tmp$length <- c(distanceFlat(p1, p2), NA)
                          tmp$distance_into_pattern <- c(0, cumsum(tmp$length[-nrow(tmp)]))

                          tmp$bearing <- c(bearing(p1, p2), NA)
                          
                          private$pattern <- tmp
                          ## set private$pattern.length
                          private$pattern.length <- tapply(tmp$distance_into_pattern,
                                                           tmp$trip_id, max, na.rm = TRUE)
                          
                          invisible(self)
                      },

                      setInits = function() {
                          if (!is.null(private$pattern)) private$initParticles()

                          invisible(self)
                      },
                      
                      update = function(pos) { 
                          ## updates the POSITION of the vehicle, and runs another iteration
                          #private$position <- as.numeric(pos[1], pos[2], pos[3])

                          update <- TRUE
                          
                          if (!missing(pos)) {
                              r <- private$position
                              delta <- pos[3] - r[3]
                              if (delta > 0) {
                                  ## new data! run filter
                                  private$position <- as.numeric(pos)
                                  private$moveParticles(delta)
                              } else update <- FALSE
                          }

                          if (update) {
                              wt <- private$particleLikelihood()
                              wi <- sample(length(wt), length(wt), replace = TRUE, prob = wt)

                              private$history$x <- abind::abind(private$history$x,
                                                                private$particles, along = 3)
                              
                              private$particles <- private$particles[, wi]
                              private$history$xhat <- abind::abind(private$history$xhat,
                                                                   private$particles, along = 3)
                          }
                          
                          
                          invisible(self)
                      },

                      plotParticles = function(...) {
                          o <- iNZightMap(~lat, ~lon, data = private$particlePositions(),
                                          name = "Current Particle Distribution")
                          
                          do.call(plot, c(list(x = o,
                                               pch = 19, col.pt = "#0000FF40", cex.pt = 0.4),
                                          list(...)))
                          
                          invisible(self)
                      },

                      plot = function(...) {
                          if (is.null(private$particles)) {
                              stop("Particles not initialised.")
                          } else {
                              self$plotParticles(xlim = range(private$pattern$shape_pt_lon),
                                                 ylim = range(private$pattern$shape_pt_lat),
                                                 ...)
                              if (length(dim(private$history$x)) == 3) {
                                  d2 <- apply(private$history$x[,,dim(private$history$x)[3]], 2, h, shape = private$pattern)
                                  addPoints(d2[2, ], d2[1, ], pch = 3,
                                            gp = list(cex = 0.4, col = "#009900"))
                              }
                              
                              addPoints(private$position[2], private$position[1],
                                        pch = 4, gp = list(cex = 0.7, col = "#FF0000"))
                          }
                          
                          invisible(self)
                      },

                      getParticles = function() {
                          return(private$history)
                      }
                  ),

                  private = list(
                      vehicle.id = NA,
                      position = NA,
                      current.trip = NA,
                      pattern = NULL,
                      pattern.length = NA,
                      
                      N.particles = NA,
                      particles = NULL,
                      history =
                          list(x = NULL, xhat = NULL),

                      initParticles = function() {
                          if (is.null(private$pattern)) stop("Please add a pattern.")
                          ## Set the initial values (initial positions of the particles)
                          tid <- private$current.trip                          
                          max.dist <- private$pattern.length[[tid]]
                          min.dist <- min(private$pattern$distance_into_pattern[
                              private$pattern$trip_id == tid])

                          tmp <- rbind(runif(private$N.particles, min.dist, max.dist),
                                       runif(private$N.particles, 0, 20), 0)
                          rownames(tmp) <- c("distance", "velocity", "acceleration")

                          private$particles <- tmp

                          private$history$x <- tmp
                          private$history$xhat <- matrix(NA, nrow = nrow(tmp), ncol = ncol(tmp))

                          invisible(self)
                      },

                      particlePositions = function() {
                          dat <- apply(private$particles, 2, h, shape = private$pattern)
                          data.frame(lat = dat[1, ], lon = dat[2, ])
                      },

                      particleLikelihood = function() {
                          ## Returns the likelihood of every particle,
                          ## given the current vehicle location, trip_id AND time.

                          ## Important: if the timestamp < trip_start_time, then
                          ##            distance_into_trip = 0.

                          ## get location and time
                          r <- private$position[1:2]
                          ## t <- private$position[3]

                          d <- distanceFlat(r, t(private$particlePositions()))
                          lh <- dnorm(d, 0, self$sig.gps)
                          rl <- if (sum(lh) == 0) rep(1, length(d)) else lh / sum(lh)

                          rl
                      },

                      moveParticles = function(delta) {
                          delta <- as.numeric(delta)
                          ## this is the model!
                          x <- private$particles
                          a <- rnorm(private$N.particles, 0, self$sig.a)
                          ## hard code the fact that the bus isn't going to to backwards ... 
                          v <- pmin(30, pmax(0, x[2, ] + delta * a))
                          d <- x[1, ] + pmax(0, delta * x[2, ] + delta^2 / 2 * a)
                          private$particles <- rbind(distance = d,
                                                     velocity = v,
                                                     acceleration = a)

                          invisible(self)
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
    phi.s <- x[1] * pi / 180
    lam.s <- x[2] * pi / 180
    
    phi.z <- asin(sin(phi.s) * cos(delta) + cos(phi.s) * sin(delta) * cos(theta))
    lam.z <- lam.s + atan2(sin(theta) * sin(delta) * cos(phi.s),
                           cos(delta) - sin(phi.s) * sin(phi.z))
    c(phi.z, lam.z) * 180 / pi
}
h <- function(x, shape) {
    ## Calculate Lat/Lon position of point(s) a given distance (x)
    ## into a pattern/shape
   
    if (x[1] <= 0) return(c(0, 0))
    if (x[1] > max(shape$distance_into_pattern))
        return(as.numeric(shape[nrow(shape), c("shape_pt_lat", "shape_pt_lon")]))
    
    j <- which.min(x[1] > shape$distance_into_pattern) - 1
    sj <- shape[j, c("shape_pt_lat", "shape_pt_lon", "bearing", "distance_into_pattern")]
    Psi <- sj$bearing
    d <- x[1] - sj$distance_into_pattern
    
    ## only do the calculations if we need to!
    if (d == 0) return(as.numeric(sj[1:2]))
    partialSegment(as.numeric(sj[1:2]), Psi, d)
}
