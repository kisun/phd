##' Particle filter for GTFS Realtime Vehicle Locations
##'
##' Use a database connection to progress particles.
##' @title GTFS-realtime Particle Filter
##' @param con a database connection which contains static GTFS tables and a realtime vehicle_positions tale
##' @param vid a vehicle id
##' @return NULL; results entered into database
##'
##' @author tell029
pf <- function(con, vid, N = 500) {
    ## Get vehicle's latest realtime state:
    vp <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s'", vid))
    if (nrow(vp) == 0) stop("That vehicle does not exist right now.")
    if (nrow(vp) > 1) warning("Multiple instances for vehicle found. Using the first one.")
    vp <- vp[1, ]

    ## Get vehicle's shape and schedule:
    info <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", vp$trip_id))
    shape <- flatten(info$shape)
    schedule <- flatten(info$schedule)
    colnames(schedule) <- gsub("pivot.", "", colnames(schedule))


    particles <- dbGetQuery(con, sprintf("SELECT * FROM particles WHERE vehicle_id='%s'", vid))

    if (nrow(particles) == 0) {
        cat("Vehicle not yet instantiated ... doing that now ...\n")
        
        particles <- data.frame(vehicle_id = rep(vid, N),
                                distance_into_trip = runif(N, 0, max(shape$dist_traveled)),
                                velocity = rep(NA, N),
                                segment = rep(NA, N))
    } else {
        ## do the things
    }

    particles
}
