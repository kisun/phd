##' Particle filter for GTFS Realtime Vehicle Locations
##'
##' Use a database connection to progress particles.
##' @title GTFS-realtime Particle Filter
##' @param con a database connection which contains static GTFS tables and a realtime vehicle_positions tale
##' @param vid a vehicle id
##' @param N the number of particles
##' @param draw logical, if \code{TRUE} a map will be drawn
##' @return integer; 0 = success; -1 = nothing to do (no new observations;
##'         1 = no such vehicle; 2 = error in particle distribution ... run again?
##'         3 = bus not near route - not yet started?
##'
##' @author tell029
pf <- function(con, vid, N = 500,
               sig.gps = 20,
               pi = 0.5,        ## probability particle stops due to bus stop
               gamma = 6,       ## deceleration/open-doors/close-doors/acceleration time
               mu.tau = 5,     ## average time a bus is stopped at a stop for
               rho = 0.1,       ## probability particle stops !due to bus stop,
               mu.nu = 20,      ## average time a bus is stopped at "lights"
               draw = FALSE,
               vp = dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s'", vid))) {

    ## Get vehicle's latest realtime state:
    if (nrow(vp) == 0) return(invisible(1))
    if (nrow(vp) > 1) warning("Multiple instances for vehicle found. Using the first one.")
    vp <- vp[1, ]

    ## Get vehicle's shape and schedule:
    qry <- sprintf("http://mybus.app/api/shape_schedule/%s", vp$trip_id)
    info <- fromJSON(qry, flatten = TRUE)
    schedule <- flatten(info$schedule)

    colnames(schedule) <- gsub("pivot.", "", colnames(schedule))
    shape <- info$shape

    sx <- (deg2rad(shape$lon) - deg2rad(vp$position_longitude)) * cos(deg2rad(vp$position_latitude))
    sy <- deg2rad(shape$lat) - deg2rad(vp$position_latitude)
    dist <- distance(cbind(sx, sy))
    particles <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE vehicle_id='%s' AND active=TRUE", vid))

    

    NEW <- FALSE
    if (nrow(particles) == 0L) {
        #cat("Vehicle not yet instantiated ... doing that now ...\n")
        NEW <- TRUE

        ## initial proposal
        sh.near <- shape[which(dist < 200), ]
        if (nrow(sh.near) == 0) {
            return(3)
        }
        particles <- data.frame(vehicle_id = rep(vid, N),
                                distance_into_trip = runif(N, min(sh.near$dist_traveled),
                                                           max(sh.near$dist_traveled)),
                                velocity = runif(N, 0, 16))
        ## determine which segment of the route each particle is on
        particles$segment <- sapply(particles$distance_into_trip,
                                    function(x) which(schedule$shape_dist_traveled > x)[1L] - 1)
        particles$arrival_time <- particles$departure_time <- NA
    } else if (particles$trip_id[1] != vp$trip_id) {
        #cat("Trip has changed ... moving forward! \n")
        delta <- vp$timestamp - particles$timestamp[1L]

        if (delta <= 0) return(invisible(-1))
        
        ## "new trip"
        ## initial proposal
        sh.near <- shape[which(dist < 200), ]
        if (nrow(sh.near) == 0) {
            return(3)
        }
        
        particles$distance_into_trip <- runif(nrow(particles), min(sh.near$dist_traveled), max(sh.near$dist_traveled))
        particles$velocity <- msm::rtnorm(nrow(particles), particles$velocity, sd = 3, lower = 0, upper = 16)
        particles$segment <- sapply(particles$distance_into_trip,
                                    function(x) which(schedule$shape_dist_traveled > x)[1L] - 1)
        particles$arrival_time <- particles$departure_time <- NA
    } else {
        delta <- vp$timestamp - particles$timestamp[1L]

        if (delta <= 0) return(invisible(-1))
        ## movement step
        #cat("The bus has moved ...\n")

        ## add process noise to speeds:
        particles$velocity <- msm::rtnorm(nrow(particles), particles$velocity, sd = 3, lower = 0, upper = 16)
           # pmin(30, pmax(0, particles$velocity + rnorm(nrow(particles),0, sd = sqrt(delta))))
        particles$segment <- sapply(particles$distance_into_trip,
                                    function(x) which(schedule$shape_dist_traveled >= x)[1L] - 1L)
        ## move each particle
        for (i in 1L:nrow(particles)) {
            particles[i, ] <- transition(particles[i, ])
        }
    }


    if (draw) {
        wi <- which(dist < 1000)
        if (length(wi) == 0) {
            warning("No points close to the bus ...")
            wi <- 1:length(dist)
        }
        sh.near <- shape[min(wi):max(wi), ]
        mobj <- iNZightMap(~lat, ~lon, data = shape)
        e <- environment()
        plot(mobj, join = TRUE, pch = NA, lwd = 2, col.line = "#333333",
             xlim = range(sh.near$lon), ylim = range(sh.near$lat), env = e)
        with(schedule, addPoints(lat, lon, gpar = list(col = "#333333", cex = 0.5), pch = 19))
        with(vp, addPoints(position_latitude, position_longitude, pch = 19,
                           gpar = list(col = "#cc3333", cex = 0.3)))
    }

    ## resampling step
    sig.xy <- sig.gps / R

    pts <- h(particles$distance_into_trip, shape)
    px <- (deg2rad(pts[2L, ]) - deg2rad(vp$position_longitude)) * cos(deg2rad(vp$position_latitude))
    py <- deg2rad(pts[1L, ]) - deg2rad(vp$position_latitude)

    if (draw) {
        addPoints(pts[1L,], pts[2L,], pch = 19,
                  gpar = list(col = "lightblue", alpha = 0.9, cex = 0.1))
    }

    theta <- seq(0, 2 * pi, length.out = 101L)
    xx <- sig.xy * cos(theta)
    yy <- sig.xy * sin(theta)
    lhood <- dmvnorm(cbind(px, py), c(0, 0), diag(2L) * sig.xy^2)
    wt <- lhood / sum(lhood)
    wt[is.na(wt)] <- 0
    if (sum(wt) == 0) return(2)

    wi <- sample(nrow(particles), N, replace = TRUE, prob = wt)
    parents <- particles$id[wi]
    particles <- particles[wi, ]

    if (draw) {
        addPoints(pts[1L, wi], pts[2L, wi], pch = 19,
                  gpar = list(col = "orangered", alpha = 0.8, cex = 0.5))
        with(vp, addPoints(position_latitude, position_longitude, pch = 19,
                           gpar = list(col = "green", cex = 0.3)))
    }

    ## set old particles to inactive
    dbGetQuery(con, sprintf("UPDATE particles SET active=FALSE WHERE vehicle_id='%s' AND active=TRUE", vid))

    qry <- paste0(
        "INSERT INTO particles (vehicle_id, distance_into_trip, velocity, segment, arrival_time, ",
        "departure_time, parent, lat, lon, trip_id, timestamp, active) VALUES ",
        with(particles, paste0("('", vehicle_id, "',", round(distance_into_trip, 2L),
                               ",", round(velocity, 3L), ",", segment, ",",
                               ifelse(is.na(arrival_time), 'NULL', round(arrival_time)), ",",
                               ifelse(is.na(departure_time), 'NULL', round(departure_time)),
                               ",", if (NEW) 'NULL' else parents, ",", pts[1L, wi], ",", pts[2L, wi], ",'",
                               vp$trip_id, "',", vp$timestamp, ",TRUE)",
                               collapse = ", ")))
    dbGetQuery(con, qry)

    return(invisible(0))
}



deg2rad <- function(deg) deg * pi / 180
rad2deg <- function(rad) rad * 180 / pi
R <- 6371 * 1000
distance <- function(x) sqrt(x[, 1L]^2 + x[, 2L]^2) * R

##' Transition a particle ("imaginary bus") to the future
##'
##' @title Transtion function
##' @param p a particle state
##' @param delta time since the last observation
##' @param schedule the schedule (with stop locations and stuff)
##' @return a moved particle
##' @author Tom Elliott
transition <- function(p, e = parent.frame()) {
    ## the amount of time we have to play with:
    tr <- e$delta

    ## first off, the bus might be stuck at lights or at a stop
    if (is.na(p$departure_time) && !is.na(p$arrival_time)) {
        ## there is a MINIMUM wait time of `gamma`
        wait <- 0
        if (p$timestamp - p$arrival_time < e$gamma) {
            wait <- wait + e$gamma - p$timestamp + p$arrival_time
        }
        wait <- wait + rexp(1L, 1 / e$mu.tau)
        tr <- tr - wait
        if (tr <= 0) return(p)
        p$departure_time <- p$arrival_time + wait
    } else {
        wait <- rbinom(1L, 1L, e$rho) * rexp(1L, 1 / e$mu.nu)
        tr <- tr - wait
        if (tr <= 0) return(p)
    }

    d <- p$distance_into_trip[1L]
    v <- p$velocity[1L]

    ## ## OK so that's done --- now lets move!
    while (tr > 0) {
        d <- p$distance_into_trip[1L]
        v <- p$velocity[1L]  ## this will later depend on the segment we are in
        if (v <= 0) return(p)

        ## distance of the next stop, and how long it'll take to get there:
        ds <- e$schedule[p$segment[1L] + 1L, "shape_dist_traveled"]
        eta <- (ds - d) / v
        tr <- tr - eta

        if (tr > 0) {
            ## bus reaches stop: compute dwell time
            p$segment <- p$segment + 1
            p$arrival_time <- p$timestamp + eta
            p$departure_time <- NA
            
            tau <- rbinom(1L, 1L, e$pi) * (e$gamma + rexp(1L, 1 / e$mu.tau))
            tr <- tr - tau
            p$distance_into_trip <- ds

            if (p$segment + 1 >= nrow(e$schedule)) tr <- 0

            if (tr > 0) {
                ## bus leaves!
                p$departure_time <- p$arrival_time + tau
            }
        } else {
            ## bus doesn't reach stop: compute distance it'll travel
            p$distance_into_trip <- d + (tr + eta) * v
        }
    }
    
    p
}
