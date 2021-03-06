require(R6)
vehicle = R6Class("vehicle",
                  public = list(
                      sig.a = 3,
                      sig.gps = 30,
                      initialize = function(id, position, trip, N.particles = 400) {
                          private$vehicle.id <- id
                          private$position <- as.numeric(position)
                          private$N.particles <- N.particles
                          
                          if (!missing(trip)) {
                              private$current.trip <- trip
                              self$setPattern(getPattern(trip, verbose = FALSE))
                              private$setSchedule()
                              private$initParticles()
                              ## self$loadHistory()
                          }
                          
                          ## cat("New vehicle instantiated.\n")
                          ## self$report()

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
                          if (is.null(private$pattern))
                              self$setPattern(getPattern(private$current.trip, verbose = FALSE))
                          
                          private$initParticles()

                          invisible(self)
                      },
                      
                      update = function(position, con) { 
                          ## updates the POSITION of the vehicle, and runs another iteration
                          #private$position <- as.numeric(pos[1], pos[2], pos[3])

                          if (!missing(con)) {
                              pos <- getPositions(con, vehicle.id = private$vehicle.id)
                              if (nrow(pos) == 0)
                                  warning("Looks like the vehicle is done for the day ...")
                              trip <- pos$trip_id
                          } else if (!missing(position)) {
                              pos <- position
                              trip <- position$trip_id
                          } else {
                              pos <- NULL
                          }

                          pos <- pos[, c("position_latitude", "position_longitude", "timestamp")]
                          #private$position <- as.numeric(pos[1], pos[2], pos[3])
                          update <- TRUE

                          if (!is.null(pos)) {
                              r <- private$position
                              delta <- pos[3] - r[3]
                              
                              if (trip != private$current.trip ||
                                  is.null(private$schedule$distance_into_trip)) {
                                  
                                  if (trip != private$current.trip) {
                                      private$resetParticles()
                                      self$setPattern(getPattern(trip, verbose = FALSE))
                                  }
                                  
                                  private$current.trip <- trip
                                  
                                  private$setSchedule()
                                  
                              }
                              
                              
                              if (delta > 0) {
                                  ## new data! run filter
                                  private$position <- as.numeric(pos)[1:3]
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

#                              print(private$particles["distance", ])
                              ## abline(h = private$particles["distance", ], col = "#00009930")

                              #self$info()
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
                                            gp = list(cex = 0.4, col = "#00990030"))
                              }
                              
                              addPoints(private$position[2], private$position[1],
                                        pch = 4, gp = list(cex = 0.7, col = "#FF0000"))
                          }
                          
                          invisible(self)
                      },

                      getParticles = function() {
                          return(private$history)
                      },

                      getCurrentState = function() {
                          return(private$particles)
                      },

                      getSchedule = function() {
                          return(private$schedule)
                      },

                      plotSchedule = function(which = c("distance", "speed", "acceleration")) {
                          which <- match.arg(which)
                          switch(which,
                                 "distance" = {
                                     curve(private$schedule.fn(x, deriv = 0),
                                           from = min(private$schedule$time),
                                           to = max(private$schedule$time),
                                           n = 101,
                                           xlab = "Time (s)",
                                           ylab = "Distance (m)")
                                 },
                                 "speed" = {
                                     curve(private$schedule.fn(x, deriv = 1),
                                           from = min(private$schedule$time),
                                           to = max(private$schedule$time),
                                           n = 101,
                                           xlab = "Time (s)",
                                           ylab = "Speed (m/s)")
                                 },
                                 "acceleration" = {
                                     curve(private$schedule.fn(x, deriv = 2),
                                           from = min(private$schedule$time),
                                           to = max(private$schedule$time),
                                           n = 101,
                                           xlab = "Time (s)",
                                           ylab = "Speed (m/s/s)")
                                 })
                      },

                      info = function() {
                          cat("\n\nVehicle ID:", private$vehicle.id,
                              "\n   Trip ID:", private$current.trip,
                              "\n Particles: mean =", rowMeans(private$particles),
                              "\n            sd   =", apply(private$particles, 1, sd))

                          invisible(self)
                      },

                      loadHistory = function() {
                          ## load history from the database:
                          hist <- dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
                                             sprintf("SELECT * FROM history WHERE trip_id='%s'",
                                                     private$current.trip))

                          if (nrow(hist) <= 1) {
                              warning("No history ...")
                              return(invisible(self))
                          }
                          
                          hist$time.day <-
                              hist$timestamp -
                              as.numeric(format(as.POSIXct(paste(hist$trip_start_date, "00:00:00")),
                                                format = "%s"))
                          hist$time.hour <- hist$time.day / 60 / 60
                          
                          hist$dvt <- as.factor(paste(hist$trip_start_date,
                                                      hist$trip_id, hist
                                                      $vehicle_id, sep = ":"))
                          which.keep <- 
                              do.call(c,
                                      invisible(tapply(1:nrow(hist), hist$dvt, function(i) {
                                          d <- diff(range(hist$time.day[i]))
                                          ## max speed
                                          dt <- diff(hist$time.day[i])
                                          dx <- diff(hist$distance[i])
                                          if (max(dx/dt) < 50) return(i) else numeric()
                                      })))
                          hist <- hist[which.keep, ]

                          if (nrow(hist) > 1) {
                              private$trip.history <- hist

                              history.fn <- try({
                                  tapply(1:nrow(hist), hist$dvt, function(i) {
                                      splinefun(hist$time.day[i],
                                                hist$distance[i],
                                                method = "hyman", ties = min)
                                  })
                              }, silent = TRUE)

                              if (inherits(history.fn, "try-error")) {
                                  warning("Unable to fit spline through history.")
                              } else {
                                  private$history.fn <- history.fn
                              }
                              
                              history.fnD <- function(d, delta = 0, deriv = 0) {
                                  sapply(private$history.fn, function(f) {
                                      ## convert distance to time, time to ["speed", "acceleration"]
                                      if (class(f) == "function") {
                                          t <- optimize(
                                              function(x) (f(x) - d)^2,
                                              interval = range(private$trip.history$time.day)
                                          )$minimum
                                          f(t + delta, deriv = deriv)
                                      } else 0
                                  })
                              }
                              private$history.fnD <- Vectorize(history.fnD, "d")
                          }
                          
                          invisible(self)
                      },

                      plotHistory = function() {
                          if (is.null(private$trip.history)) return(invisible(self))
                          hist <- private$trip.history
                          with(hist,
                               plot(time.hour, distance, type = "n",
                                    main = paste("History of trip", private$current.trip),
                                    ## xlim = c(6.2, 8), ylim = c(0, 10000),
                                    xlab = "Time (h)", ylab = "Distance into Trip (m)"))
                          invisible(tapply(1:nrow(hist), hist$dvt, function(i) {
                              lines(hist$time.hour[i], hist$distance[i], col = "#00000040")
                              points(hist$time.hour[i], hist$distance[i], cex = 0.3, pch = 10)
                          }))

                          invisible(self)
                      },

                      historyFns = function() {
                          list(private$history.fn,
                               private$history.fnD)
                      }
                      
                  ),

                  private = list(
                      vehicle.id = NA,
                      position = NA,
                      current.trip = NA,
                      pattern = NULL,
                      pattern.length = NA,
                      schedule = NULL,
                      schedule.fn = NULL,
                      schedule.fnD = NULL,
                      acc.sd = NA,
                      trip.history = NULL,
                      history.fn = NULL,
                      history.fnD = NULL,
                      
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

                      resetParticles = function() {
                          tmp <- private$particles
                          tmp[1, ] <- 0
                          private$particles <- tmp
                      },
                      
                      setSchedule = function() {
                          private$schedule <- getSchedule(private$current.trip, verbose = FALSE)

                          ## Times, in seconds:
                          time <- ifelse(is.na(private$schedule$arrival_time),
                                         private$schedule$departure_time,
                                         private$schedule$arrival_time)
                          time.sec <- t(sapply(time, function(x)
                              as.numeric(unlist(strsplit(x, ":"))))) %*% c(60 * 60, 60, 1)
                          private$schedule$time <- time.sec - min(time.sec)

                          ## Distance:
                          ## if (!is.na(private$current.trip)) {
                          ##     if (is.null(private$pattern)) {
                          ##         suppressWarnings({
                          ##             private$schedule$distance_into_trip <-
                          ##                 getShapeDist(private$schedule,
                          ##                              getPattern(private$current.trip,
                          ##                                         verbose = FALSE))
                          ##         })
                          ##     } else {
                          private$schedule$distance_into_trip <-
                              suppressWarnings({
                                  getShapeDist(private$schedule,
                                               private$pattern)
                              })
                          ##   }
                          ## }

                          schedule.fn <- try({
                              splinefun(private$schedule$time,
                                        private$schedule$distance_into_trip,
                                        method = "hyman", ties = min)
                          }, silent = TRUE)
                          if (inherits(schedule.fn, "try-error")) {
                              warning("Error fitting speed function.")
                              private$schedule.fn <- NULL
                          } else {
                              private$schedule.fn <- schedule.fn
                              private$acc.sd <- sd(schedule.fn(seq(min(private$schedule$time),
                                                                   max(private$schedule$time),
                                                                   length = 1001), deriv = 2))
                          }

                          private$schedule.fnD <- function(d, deriv = 1) {
                              ## convert distance to time, time to ["speed", "acceleration"]
                              t <- optimize(function(x) (private$schedule.fn(x) - d)^2,
                                            interval = range(private$schedule$time))$minimum
                              private$schedule.fn(t, deriv = deriv)
                          }
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
                          ## if (is.null(private$schedule.fn)) {
                              a <- rnorm(private$N.particles, x[3, ], self$sig.a)
                          ## } else {
                          ##     a <- rnorm(private$N.particles,
                          ##                sapply(x[1, ], private$schedule.fnD, deriv = 2),
                          ##                private$acc.sd * 1.5)
                          ## }
                          
                          ## hard code the fact that the bus isn't going to to backwards ...
                          ## ... actually, let it go backwards UNTIL it's been 0:
                          p.back <- ifelse(runif(ncol(x)) < 0.05, -Inf, 0)
                          v <- pmax(p.back, x[2, ] + delta * a)
                          ## d <- pmax(0, x[1, ] + pmax(p.back, delta * x[2, ] + delta^2 / 2 * a))
                          ## v <- x[2, ] + delta * a
                          ## d <- x[1, ] + delta * x[2, ] + delta^2 / 2 * a

                          ## Forget velocity, acceleration:
                          if (is.null(private$history.fnD)) {
                              d <- pmax(0, x[1, ] + pmax(p.back, delta * x[2, ] + delta^2 / 2 * a))
                          } else {
                              Dhat <- private$history.fnD(x[1, ], delta)
                              d <- colMeans(Dhat) + rnorm(private$N.particles, 0,
                                                          apply(Dhat, 2, sd) / sqrt(nrow(Dhat)))
                              d <- pmax(x[1, ], d)
                          }

                          ## self$plotHistory()
                          curX <- private$particles["distance", ]
                          ## abline(h = d, col = "#00990060")
                          ## abline(h = curX, col = "#99000020", lty = 3)

                          ## need to limit the distance!
                          #w <- which(private$pattern$trip_id == private$current.trip)
                          #if (max(w) < nrow(private$pattern))
                          #    w <- w + 1
                          ##dist.max <- max(private$pattern$distance_into_pattern[w], na.rm = TRUE)
                          dist.max <- max(private$pattern$distance_into_pattern, na.rm = TRUE)
                          
                          private$particles <- rbind(distance = pmin(dist.max, d),
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

    mode(a) <- mode(b) <- "numeric"
    
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

    if (is.na(x[1])) print(x)
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
