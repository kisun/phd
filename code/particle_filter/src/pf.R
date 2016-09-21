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
##'
##' @author tell029
pf <- function(con, vid, N = 500,
               sig.gps = 20,
               pi = 0.5,        ## probability particle stops due to bus stop
               gamma = 6,       ## deceleration/open-doors/close-doors/acceleration time
               mu.tau = 10,     ## average time a bus is stopped at a stop for
               rho = 0.1,       ## probability particle stops !due to bus stop,
               mu.nu = 20,      ## average time a bus is stopped at "lights"
               draw = FALSE) {
    ## Get vehicle's latest realtime state:
    vp <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s'", vid))
    if (nrow(vp) == 0) return(invisible(1))
    if (nrow(vp) > 1) warning("Multiple instances for vehicle found. Using the first one.")
    vp <- vp[1, ]

    print(vp$trip_id)

    ## Get vehicle's shape and schedule:
    info <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", vp$trip_id), flatten = TRUE)
    schedule <- flatten(info$schedule)
    colnames(schedule) <- gsub("pivot.", "", colnames(schedule))
    shape <- info$shape

    sx <- (deg2rad(shape$lon) - deg2rad(vp$position_longitude)) * cos(deg2rad(vp$position_latitude))
    sy <- deg2rad(shape$lat) - deg2rad(vp$position_latitude)
    dist <- distance(cbind(sx, sy))
    particles <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE vehicle_id='%s'", vid))
    


    NEW <- FALSE
    if (nrow(particles) == 0L) {
        cat("Vehicle not yet instantiated ... doing that now ...\n")
        NEW <- TRUE
        
        ## initial proposal
        sh.near <- shape[which(dist < 200), ]
        particles <- data.frame(vehicle_id = rep(vid, N),
                                distance_into_trip = runif(N, min(sh.near$dist_traveled),
                                                           max(sh.near$dist_traveled)),
                                velocity = runif(N, 0, 30))
        ## determine which segment of the route each particle is on
        particles$segment <- sapply(particles$distance_into_trip,
                                    function(x) which(schedule$shape_dist_traveled > x)[1L] - 1)
    } else {  ## else if ( trip_is_the_same ) { ... } else {
        delta <- vp$timestamp - particles$timestamp[1L]
        
        if (delta <= 0) return(invisible(-1))
        ## movement step
        cat("The bus has moved ...\n")

        ## add process noise to speeds:
        particles$velocity <-
            pmin(30, pmax(0, particles$velocity + rnorm(nrow(particles),0, sd = sqrt(delta))))
        particles$segment <- sapply(particles$distance_into_trip,
                                    function(x) which(schedule$shape_dist_traveled > x)[1L] - 1L)
        ## move each particle
        for (i in 1L:nrow(particles)) {
            particles[i, ] <- transition(particles[i, ])
        }
    }

    print(head(schedule))

    if (draw) {
        wi <- which(dist < 1000)
        sh.near <- shape[min(wi):max(wi), ]
        mobj <- iNZightMap(~lat, ~lon, data = shape)
        e <- environment()
        plot(mobj, join = TRUE, pch = NA, lwd = 2, col.line = "#333333",
             xlim = range(sh.near$lon), ylim = range(sh.near$lat), env = e)
        with(vp, addPoints(position_latitude, position_longitude, pch = 19,
                           gpar = list(col = "#cc3333", cex = 0.3)))
    }

    ## resampling step
    sig.xy <- sig.gps / R

    ## if (draw) {
    ##     ## draw the gps error "circle":
    ##     theta <- seq(0, 2 * pi, length.out = 101)
    ##     xx <- sig.xy * cos(theta)
    ##     yy <- sig.xy * sin(theta)
    ##     #addLines(rad2deg(yy) + vp$position_latitude,
    ##     #         rad2deg(xx) / cos(deg2rad(vp$position_latitude)) + vp$position_longitude)
    ## }
    
    pts <- h(particles$distance_into_trip, shape)
    px <- (deg2rad(pts[2L, ]) - deg2rad(vp$position_longitude)) * cos(deg2rad(vp$position_latitude))
    py <- deg2rad(pts[1L, ]) - deg2rad(vp$position_latitude)

    if (draw) {
        addPoints(pts[1L,], pts[2L,], pch = 19,
                  gpar = list(col = "lightblue", alpha = 0.8, cex = 0.9))
    }
    
    theta <- seq(0, 2 * pi, length.out = 101L)
    xx <- sig.xy * cos(theta)
    yy <- sig.xy * sin(theta)
    lhood <- dmvnorm(cbind(px, py), c(0, 0), diag(2L) * sig.xy^2)
    wt <- lhood / sum(lhood)
    wt[is.na(wt)] <- 0
    if (sum(wt) == 0) return(2)

    wi <- sample(nrow(particles), replace = TRUE, prob = wt)
    particles <- particles[wi, ]
    
    if (draw) {
        addPoints(pts[1L, wi], pts[2L, wi], pch = 19,
                  gpar = list(col = "#333333", alpha = 0.8, cex = 0.5))
        with(vp, addPoints(position_latitude, position_longitude, pch = 19,
                           gpar = list(col = "#cc3333", cex = 0.3)))
    }

    if (!NEW) {
        ## delete the old particles ...
        dbGetQuery(con, sprintf("DELETE FROM particles WHERE vehicle_id='%s'", vid))
    }

    qry <- paste0(
        "INSERT INTO particles (vehicle_id, distance_into_trip, velocity, segment, lat, lon, timestamp) VALUES ",
        with(particles, paste0("('", vehicle_id, "',", round(distance_into_trip, 2L),
                               ",", round(velocity, 3L), ",", segment,
                               ",", pts[1L, wi], ",", pts[2L, wi], ",", vp$timestamp, ")",
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
    cat("=====================================================\n")
    print(p)
    ## the amount of time we have to play with:
    tr <- e$delta
    
    ## first off, the bus might be stuck at lights ... 
    wait <- rbinom(1L, 1L, e$rho) * rexp(1L, 1 / e$mu.nu)
    tr <- tr - wait
    if (tr <= 0) return(p)

    d <- p$distance_into_trip[1L]
    v <- p$velocity[1L]
#    p$distance_into_trip <- d + tr * v
    print(p)

    ## ## OK so that's done --- now lets move!
    while (tr > 0) {
        print(tr)

        d <- p$distance_into_trip[1L]
        v <- p$velocity[1L]  ## this will later depend on the segment we are in
        if (v <= 0) return(p)

        ## distance of the next stop, and how long it'll take to get there:
        ds <- e$schedule[p$segment[1L] + 1, "shape_dist_traveled"]
        eta <- (ds - d) / v
        tr <- tr - eta
        print(c(ds, d, v, eta))
        print(tr)
        
        if (tr > 0) {
            ## bus reaches stop: compute dwell time
            Ta <- p$timestamp + eta
            tau <- rbinom(1L, 1L, e$pi) * (e$gamma + rexp(1L, 1 / e$mu.tau))

            tr <- tr - tau
            p$distance_into_trip <- ds

            
            print(tr)
            tr <- 0
    ##         if (tr > 0) {
    ##             Td <- Ta - tau
    ##             p$segment <- p$segment + 1
    ##             if (p$segment >= nrow(e$schedule)) tr <- 0
    ##         }
    ##     } else {
    ##         ## bus doesn't reach stop: compute distance it'll travel
    ##         p$distance_into_trip <- d + tr * v
    ##     }
    }

    p
}