collectHistory <- function(route, day, data.clean = list(),
                           hist.db = dbConnect(SQLite(), "db/historical-data.db"),
                           verbose = FALSE) {

    con <- dbConnect(SQLite(), "db/backups/gtfs-history_latest.db")

    if (!missing(day))
        if (missing(route))
            positions <- getPositions(con, date = day)
        else
            positions <- getPositions(con, route.id = route, date = day)
    else
        positions <- getPositions(con, route.id = route)
    ## table(positions$route_id)  ## version number has updated! 
    ## table(positions$trip_id)
    
    ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
    ##                      name = "Auckland Busses")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    
    
    ## "started" = logical, has the trip started? (according to the schedule)
    positions$started <- positions$timestamp -
        as.numeric(format(as.POSIXct(paste(positions$trip_start_date, positions$trip_start_time)),
                          format = "%s")) >= 0
    
    ERRORS <- character()
    
    dat <- positions[positions$started, ]
    
    ## for (day in unique(dat$trip_start_date)) {
    if (length(unique(dat$route_id)) == 0) {
        warning("No routes ...")
        return()
    }
    pb <- txtProgressBar(0, length(unique(dat$route_id)), style = 4)
    for (I in seq_along(unique(dat$route_id))) {
        route <- unique(dat$route_id)[I]
        
        ## day <- unique(dat$trip_start_date)[1]
        if (verbose)
            cat("\n\n========================== Processing route:", route, "\n") 
        
        ## tmp <- dat[dat$trip_start_date == day, ]
        tmp <- dat[dat$route_id == route, ]
        
        trips <- unique(tmp$trip_id)
        ## if (day %in% names(data.clean))
        ##     trips <- trips[!trips %in% data.clean[[day]]]
        trips.schedule <- lapply(trips, getSchedule, verbose = FALSE)
        names(trips.schedule) <- trips
        trips.start <- sapply(trips.schedule, function(x) {
                                  t <- x$departure_time[1]
                                  as.numeric(format(as.POSIXct(paste(day, t)), format = "%s"))
                              })
        trips.order <- order(trips.start)
        
        for (trip in trips[trips.order]) {
            
            ## trip <- trips[trips.order][2]
            if (verbose) 
                cat("============ Processing trip:", trip, "\n")
            tmp2 <- tmp[tmp$trip_id == trip, ]

            ## trip.map <- iNZightMap(~position_latitude, ~position_longitude, data = tmp2,
            ##                        name = "Historical Route 090")
            ## do.call(plot, list(x = trip.map, pch = 4, cex.pt = 0.6, col.pt = "#cc0000"))
            
            shape <- getPattern(trip, verbose = FALSE)
            ## addLines(shape$shape_pt_lon, shape$shape_pt_lat)

            voffset <- sapply(unique(tmp2$vehicle_id), function(vid) {
                                  tt <- tmp2[tmp2$vehicle_id == vid, ]
                                  trip.start <- min(tt$timestamp)
                                  trip.offset <- trip.start -
                                      as.numeric(format(as.POSIXct(paste(tt$trip_start_date[1],
                                                                         trips.schedule[[trip]]$departure_time[1])),
                                                        format = "%s"))
                                  trip.offset
                              })

            ## Limit of 90 minutes
            if (all(voffset > 90 * 60)) next

            vs <- unique(tmp2$vehicle_id)
            ## if (length(vs) > 1)
               ## cat("Multiple matches ... using the vehicle starting closest to the schedule.\n")

            valid.vs <- which(voffset < 90 * 60)
            if (length(valid.vs) == 1) {
                vid <- vs[valid.vs]
            } else {
                min <- min(abs(voffset))
                vid <- vs[which(abs(voffset) == min)]
            }
            
            ## check to see if this day/trip is already in the database
            if ("history" %in% dbListTables(hist.db)) {
                if (dbGetQuery(hist.db,
                               sprintf("SELECT count(*) AS count FROM history WHERE trip_start_date='%s' AND trip_id='%s' AND vehicle_id='%s'",
                                       day, trip, vid))$count > 0) {
                    next
                }
            }
            
            ## do computations on the shape ...
            p1 <- t(shape[-nrow(shape), c("shape_pt_lon", "shape_pt_lat")])
            p2 <- t(shape[-1, c("shape_pt_lon", "shape_pt_lat")])
            mode(p1) <- mode(p2) <- "numeric"
            shape$length <- c(distanceFlat(p1, p2), 0)
            shape$distance_into_pattern <- c(0, cumsum(shape$length[-nrow(shape)]))
            shape$bearing <- c(bearing(p1, p2), 0)
                        
            tmp3 <- tmp2[tmp2$vehicle_id == vid, ]
            
            v <- vehicle$new(vid, tmp3[1, c("position_latitude", "position_longitude", "timestamp")], trip)
            v$update()## $plot()

            if (nrow(tmp3) < 2) next
            
            for (i in 2:nrow(tmp3)) {
                v$update(tmp3[i, ])
                ## if (i %% 5 == 0) v$plot()
                ## v$plot();grid::grid.locator()
            }
            
            
            ##try({
                
            out <- try(data.frame(cbind(
                tmp3[, c("trip_id", "vehicle_id", "route_id", "timestamp", "trip_start_date", "trip_start_time")],
                t(apply(v$getParticles()$xhat, 3, function(x) apply(x, 1, median, na.rm = TRUE))[, -1])
                )))

            if (inherits(out, "try-error")) {
                print("Unable to extract info ... rows don't match???")
                print(tail(v$getParticles()$xhat))
                next
            }
            

            ## only grab the positive distances:
            out <- out[out$distance >= 0, ]
            
            ## pick out the top points
            SCHED <- v$getSchedule()
            MAX <- which(out$distance == max(out$distance))
            MAX <- MAX[c(1, which(diff(MAX) > 1) + 1)]

            ## with(out, plot(timestamp, distance, type = "l",
            ## ylim = c(0, max(distance, SCHED$distance_into_trip)),
            ## main = "Unfiltered output"))
            
            ## pick out the bottom points
            segs <- lapply(seq_along(MAX), function(i) {
                               ii <- (c(1, MAX + 1)[i]):(c(1, MAX)[i + 1])
                               if (any(diff(out$distance[ii]) < 0)) {
                                   i2 <- which(diff(out$distance[ii]) < 0)
                                   lapply(i2, function(j) {
                                              if (j > 1 && j < (length(ii) - 1)) {
                                                  ## just smooth it out ...
                                                  out$distance[ii[j]] <<- NA
                                              }
                                          })
                                   ii <- ii[!ii %in% which(is.na(out$distance))]
                                        #out <- out[!is.na(out$distance), ]
                                   ## then clear up any NAs
                                   if (any(diff(out$distance[ii]) < 0))
                                       ii <- ii[-(1:max(which(diff(out$distance[ii]) < 0)))]
                               }
                               ii
                           })
           
            toffset <- sapply(segs, function(i) out[i[1], "timestamp"]) - 
                as.numeric(format(as.POSIXct(paste(out$trip_start_date[1],
                                                   SCHED$departure_time[1])),
                                  format = "%s"))
            segs[toffset > 90 * 60] <- NULL
            out <- try(out[segs[[which.max(sapply(segs, function(i) diff(range(out$distance[i]))))]], ])
            
            if (inherits(out, "try-error")) {
                ## cat("\nError with this route: ", route)
                ERRORS <- c(ERRORS, route)
                next
            }
            
            out$trip.timestamp <-
                out$timestamp - as.numeric(format(as.POSIXct(paste(tmp3$trip_start_date[1],
                                                                   tmp3$trip_start_time[1])),
                                                  format = "%s"))
            
            ## Provide a 90 minute window ...
            ## ... and only keep history that spans > 90% of the route ...
            #plot(distance_into_trip ~ time, data = SCHED,
            #     main = paste("Route:", route, "   Date:", day),
            #     xlim = range(SCHED$time, out$trip.timestamp),
            #     ylim = c(0, max(out$distance, SCHED$distance_into_trip)))
            #with(out, lines(trip.timestamp, distance))

            #cat("STARTS:", min(SCHED$time), ";   first observation:", out$trip.timestamp[1], "\n")

            msg <- ""
            if (out$trip.timestamp[1] < min(SCHED$time) + 90 * 60) {
                if (diff(range(out$distance)) > 0.9 * diff(range(SCHED$distance_into_trip))) {
                    
                    
                    
                    ## Some way of deciding whether to keep it or not ...
                    keep <- TRUE  ## readline("Keep this record? (Y/n) ")
                    ## if (keep %in% c("", "Y", "y")) {
                    out$message <- ""
                    
                    res <- dbWriteTable(dbConnect(SQLite(), "db/historical-data.db"),
                                        "history", out, append = TRUE)
                    
                    if (!res) stop("Unable to write to database ...")
                    else if (verbose) cat("Trip written to database.\n")
                    
                    
                } else {
                    if (verbose ) cat("\nRoute ", route, " - ", "range less than 90%; ignoring trip.")
                    msg <- "short"
                }
            } else {
                if (verbose) cat("\nRoute ", route, " - ", "started too late; assuming driver error.")
                msg <- "late"
            }

            if (msg != "") {
                dbWriteTable(dbConnect(SQLite(), "db/historical-data.db"),
                             "history",
                             data.frame(trip_id = out$trip_id[1],
                                        vehicle_id = out$vehicle_id[1],
                                        route_id = out$route_id[1],
                                        timestamp = out$timestamp[1],
                                        trip_start_date = out$trip_start_date[1],
                                        trip_start_time = out$trip_start_time[1],
                                        distance = 0,
                                        velocity = 0,
                                        acceleration = 0,
                                        trip.timestamp = 0,
                                        message = msg), append = TRUE)
            }
            
            ##}, silent = TRUE) -> tryy
            ##if (inherits(try, "try-error")) print(try)
        }
        if (verbose) cat("\n")
        setTxtProgressBar(pb, I)
    }
    close(pb)
}
